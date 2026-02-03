/**
 * @file key_vault.h
 * @brief Secure Key Storage
 * 
 * WHAT: Protected storage for cryptographic keys and secrets.
 *       Abstracts hardware security modules (HSM), secure enclaves,
 *       and software-based protection.
 * 
 * WHY: Proper key management is critical for security. Understanding
 *      key protection, access control, and secure storage demonstrates
 *      security expertise required for connected devices.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Device identity credentials
 *   - TLS certificates and keys
 *   - API secrets and tokens
 *   - Firmware signing keys
 *   - User authentication data
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Key derivation (KDF)
 *   - Access control policies
 *   - Key usage restrictions
 *   - Secure provisioning
 *   - Key rotation
 */

#ifndef GF_KEY_VAULT_H
#define GF_KEY_VAULT_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_VAULT_MAX_KEYS       16
#define GF_VAULT_KEY_ID_LEN     32
#define GF_VAULT_MAX_KEY_SIZE   256

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_KEY_TYPE_RAW = 0,        /* Raw symmetric key */
    GF_KEY_TYPE_AES,            /* AES key */
    GF_KEY_TYPE_HMAC,           /* HMAC key */
    GF_KEY_TYPE_EC_PRIVATE,     /* EC private key */
    GF_KEY_TYPE_EC_PUBLIC,      /* EC public key */
    GF_KEY_TYPE_RSA_PRIVATE,    /* RSA private key */
    GF_KEY_TYPE_RSA_PUBLIC,     /* RSA public key */
    GF_KEY_TYPE_CERTIFICATE     /* X.509 certificate */
} gf_key_type_t;

typedef enum {
    GF_KEY_USAGE_ENCRYPT    = (1 << 0),
    GF_KEY_USAGE_DECRYPT    = (1 << 1),
    GF_KEY_USAGE_SIGN       = (1 << 2),
    GF_KEY_USAGE_VERIFY     = (1 << 3),
    GF_KEY_USAGE_DERIVE     = (1 << 4),
    GF_KEY_USAGE_WRAP       = (1 << 5),
    GF_KEY_USAGE_UNWRAP     = (1 << 6)
} gf_key_usage_t;

typedef enum {
    GF_VAULT_OK = 0,
    GF_VAULT_NOT_FOUND,
    GF_VAULT_FULL,
    GF_VAULT_ACCESS_DENIED,
    GF_VAULT_LOCKED,
    GF_VAULT_INVALID,
    GF_VAULT_ERROR
} gf_vault_error_t;

typedef struct {
    char            id[GF_VAULT_KEY_ID_LEN];
    gf_key_type_t   type;
    uint16_t        size_bits;
    uint8_t         usage;      /* gf_key_usage_t flags */
    bool            exportable;
    bool            hardware_protected;
    uint32_t        created_time;
    uint32_t        expire_time;    /* 0 = never */
} gf_key_info_t;

typedef struct {
    gf_key_type_t   type;
    uint16_t        size_bits;
    uint8_t         usage;          /* gf_key_usage_t flags */
    bool            exportable;
    uint32_t        lifetime_days;  /* 0 = permanent */
} gf_key_policy_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize key vault
 */
int gf_vault_init(void);

/**
 * @brief Import key into vault
 */
gf_vault_error_t gf_vault_import(const char *key_id,
                                  const gf_key_policy_t *policy,
                                  const uint8_t *key_data, size_t key_len);

/**
 * @brief Generate key in vault
 */
gf_vault_error_t gf_vault_generate(const char *key_id,
                                    const gf_key_policy_t *policy);

/**
 * @brief Export key (if exportable)
 */
gf_vault_error_t gf_vault_export(const char *key_id,
                                  uint8_t *key_data, size_t *key_len);

/**
 * @brief Delete key
 */
gf_vault_error_t gf_vault_delete(const char *key_id);

/**
 * @brief Get key information
 */
gf_vault_error_t gf_vault_get_info(const char *key_id, gf_key_info_t *info);

/**
 * @brief Check if key exists
 */
bool gf_vault_exists(const char *key_id);

/**
 * @brief Use key for encryption (key never leaves vault)
 */
gf_vault_error_t gf_vault_encrypt(const char *key_id,
                                   const uint8_t *input, size_t input_len,
                                   uint8_t *output, size_t *output_len);

/**
 * @brief Use key for decryption
 */
gf_vault_error_t gf_vault_decrypt(const char *key_id,
                                   const uint8_t *input, size_t input_len,
                                   uint8_t *output, size_t *output_len);

/**
 * @brief Use key for signing
 */
gf_vault_error_t gf_vault_sign(const char *key_id,
                                const uint8_t *hash, size_t hash_len,
                                uint8_t *signature, size_t *sig_len);

/**
 * @brief Derive new key from existing key
 */
gf_vault_error_t gf_vault_derive(const char *base_key_id,
                                  const char *new_key_id,
                                  const uint8_t *context, size_t context_len,
                                  const gf_key_policy_t *policy);

/**
 * @brief Lock vault (require authentication to unlock)
 */
int gf_vault_lock(void);

/**
 * @brief Unlock vault
 */
int gf_vault_unlock(const uint8_t *auth_data, size_t auth_len);

/**
 * @brief Enumerate keys
 */
int gf_vault_list(gf_key_info_t *keys, size_t max_keys, size_t *count);

const void* gf_vault_get_driver(void);

#endif /* GF_KEY_VAULT_H */
