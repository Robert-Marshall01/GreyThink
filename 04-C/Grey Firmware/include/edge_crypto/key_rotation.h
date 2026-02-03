/**
 * @file key_rotation.h
 * @brief Key Rotation Manager for Edge Cryptography
 * 
 * INDUSTRY RELEVANCE:
 * Key lifecycle management is critical for:
 * - Limiting exposure from key compromise
 * - Compliance (PCI-DSS, HIPAA)
 * - Forward secrecy
 * - Device fleet management
 * - Zero-trust architectures
 * 
 * Standards: NIST SP 800-57, PKCS#11
 * Companies: HashiCorp Vault, AWS KMS, Azure Key Vault
 */

#ifndef GF_KEY_ROTATION_H
#define GF_KEY_ROTATION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_KR_MAX_KEYS              32
#define GF_KR_KEY_ID_SIZE           16
#define GF_KR_MAX_KEY_SIZE          64
#define GF_KR_MAX_KEY_VERSIONS      4

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_KR_OK = 0,
    GF_KR_ERROR_NULL_PTR,
    GF_KR_ERROR_NOT_INITIALIZED,
    GF_KR_ERROR_KEY_NOT_FOUND,
    GF_KR_ERROR_KEY_EXPIRED,
    GF_KR_ERROR_KEY_REVOKED,
    GF_KR_ERROR_GENERATION_FAILED,
    GF_KR_ERROR_STORAGE_FAILED,
    GF_KR_ERROR_SYNC_FAILED,
    GF_KR_ERROR_NO_ACTIVE_KEY,
    GF_KR_ERROR_ROTATION_IN_PROGRESS,
    GF_KR_WARN_KEY_EXPIRING,
    GF_KR_WARN_ROTATION_PENDING
} gf_kr_status_t;

typedef enum {
    GF_KEY_STATE_PENDING,       /* Generated, not yet active */
    GF_KEY_STATE_ACTIVE,        /* Current encryption key */
    GF_KEY_STATE_DECRYPT_ONLY,  /* Can decrypt, not encrypt */
    GF_KEY_STATE_DEACTIVATED,   /* Retired, kept for decryption */
    GF_KEY_STATE_COMPROMISED,   /* Known compromise */
    GF_KEY_STATE_DESTROYED      /* Securely erased */
} gf_key_state_t;

typedef enum {
    GF_KEY_TYPE_AES_128,
    GF_KEY_TYPE_AES_256,
    GF_KEY_TYPE_HMAC,
    GF_KEY_TYPE_ED25519,
    GF_KEY_TYPE_X25519,
    GF_KEY_TYPE_RSA_2048,
    GF_KEY_TYPE_RSA_4096,
    GF_KEY_TYPE_DEVICE_ID,      /* Device identity key */
    GF_KEY_TYPE_SESSION         /* Ephemeral session key */
} gf_key_type_t;

typedef enum {
    GF_KEY_USAGE_ENCRYPT,
    GF_KEY_USAGE_DECRYPT,
    GF_KEY_USAGE_SIGN,
    GF_KEY_USAGE_VERIFY,
    GF_KEY_USAGE_KEY_WRAP,
    GF_KEY_USAGE_DERIVE
} gf_key_usage_t;

typedef enum {
    GF_ROTATION_MANUAL,         /* Operator initiated */
    GF_ROTATION_SCHEDULED,      /* Time-based */
    GF_ROTATION_USAGE_LIMIT,    /* After N operations */
    GF_ROTATION_ON_COMPROMISE,  /* Emergency rotation */
    GF_ROTATION_ON_UPDATE       /* Firmware update trigger */
} gf_rotation_trigger_t;

/**
 * @brief Key metadata
 */
typedef struct {
    uint8_t key_id[GF_KR_KEY_ID_SIZE];
    gf_key_type_t type;
    gf_key_state_t state;
    uint32_t version;
    uint64_t created_ms;
    uint64_t activated_ms;
    uint64_t expires_ms;
    uint64_t last_used_ms;
    uint64_t usage_count;
    uint32_t allowed_usages;    /* Bitmask of gf_key_usage_t */
    bool exportable;
    bool hardware_backed;
} gf_kr_key_info_t;

/**
 * @brief Key generation parameters
 */
typedef struct {
    gf_key_type_t type;
    char key_name[32];
    uint32_t validity_days;
    uint64_t max_usage_count;
    uint32_t allowed_usages;
    bool exportable;
    bool auto_rotate;
    gf_rotation_trigger_t rotation_trigger;
    uint32_t rotation_period_days;
} gf_kr_gen_params_t;

/**
 * @brief Rotation policy
 */
typedef struct {
    gf_rotation_trigger_t trigger;
    uint32_t rotation_period_days;
    uint64_t max_usage_count;
    uint32_t overlap_days;      /* Active + decrypt-only overlap */
    uint32_t retention_days;    /* Keep deactivated keys */
    bool notify_before_rotation;
    uint32_t notify_days_before;
} gf_kr_rotation_policy_t;

/**
 * @brief Key version
 */
typedef struct {
    uint32_t version;
    gf_key_state_t state;
    uint64_t created_ms;
    uint64_t activated_ms;
    uint64_t deactivated_ms;
    uint64_t usage_count;
} gf_kr_key_version_t;

/**
 * @brief Rotation status
 */
typedef struct {
    bool rotation_in_progress;
    uint32_t old_version;
    uint32_t new_version;
    uint64_t rotation_started_ms;
    float progress_pct;
    uint32_t pending_rewrap_count;
} gf_kr_rotation_status_t;

/**
 * @brief Key ring statistics
 */
typedef struct {
    uint32_t total_keys;
    uint32_t active_keys;
    uint32_t expiring_soon;
    uint32_t rotations_completed;
    uint32_t emergency_rotations;
    uint64_t total_operations;
} gf_kr_stats_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_kr_rotation_cb_t)(const uint8_t* key_id,
                                     uint32_t old_version,
                                     uint32_t new_version,
                                     void* user_data);

typedef void (*gf_kr_expiry_cb_t)(const uint8_t* key_id,
                                   uint32_t days_remaining,
                                   void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_kr_status_t gf_kr_init(void);
void gf_kr_shutdown(void);

/* Key generation */
gf_kr_status_t gf_kr_generate(const gf_kr_gen_params_t* params,
                               uint8_t* key_id);
gf_kr_status_t gf_kr_import(const gf_kr_gen_params_t* params,
                             const uint8_t* key_material,
                             uint32_t key_len,
                             uint8_t* key_id);

/* Key retrieval (for operations) */
gf_kr_status_t gf_kr_get_active(const uint8_t* key_id,
                                 const uint8_t** key_material,
                                 uint32_t* key_len);
gf_kr_status_t gf_kr_get_version(const uint8_t* key_id,
                                  uint32_t version,
                                  const uint8_t** key_material,
                                  uint32_t* key_len);

/* Key information */
gf_kr_status_t gf_kr_get_info(const uint8_t* key_id, gf_kr_key_info_t* info);
gf_kr_status_t gf_kr_list_keys(gf_kr_key_info_t* keys,
                                uint32_t max_keys,
                                uint32_t* actual_count);
gf_kr_status_t gf_kr_get_versions(const uint8_t* key_id,
                                   gf_kr_key_version_t* versions,
                                   uint8_t max_versions,
                                   uint8_t* actual_count);

/* Key lifecycle */
gf_kr_status_t gf_kr_activate(const uint8_t* key_id);
gf_kr_status_t gf_kr_deactivate(const uint8_t* key_id);
gf_kr_status_t gf_kr_revoke(const uint8_t* key_id);
gf_kr_status_t gf_kr_destroy(const uint8_t* key_id);

/* Rotation */
gf_kr_status_t gf_kr_set_rotation_policy(const uint8_t* key_id,
                                          const gf_kr_rotation_policy_t* policy);
gf_kr_status_t gf_kr_rotate(const uint8_t* key_id);
gf_kr_status_t gf_kr_get_rotation_status(const uint8_t* key_id,
                                          gf_kr_rotation_status_t* status);
gf_kr_status_t gf_kr_emergency_rotate_all(void);

/* Usage tracking */
gf_kr_status_t gf_kr_record_usage(const uint8_t* key_id, gf_key_usage_t usage);

/* Callbacks */
gf_kr_status_t gf_kr_register_rotation_callback(gf_kr_rotation_cb_t cb, void* user_data);
gf_kr_status_t gf_kr_register_expiry_callback(gf_kr_expiry_cb_t cb, void* user_data);

/* Statistics */
gf_kr_status_t gf_kr_get_stats(gf_kr_stats_t* stats);

/* Periodic processing */
gf_kr_status_t gf_kr_process(void);

#endif /* GF_KEY_ROTATION_H */
