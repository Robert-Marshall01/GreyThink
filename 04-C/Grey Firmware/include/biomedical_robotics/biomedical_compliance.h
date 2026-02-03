/**
 * @file biomedical_compliance.h
 * @brief Regulatory Compliance Logging for Medical Devices
 * 
 * INDUSTRY RELEVANCE:
 * Medical devices must comply with:
 * - IEC 62304: Software lifecycle for medical devices
 * - FDA 21 CFR Part 11: Electronic records
 * - MDR (EU Medical Device Regulation)
 * - ISO 13485: Quality management systems
 * - HIPAA: Patient data protection
 * 
 * Requirements: Audit trails, data integrity, access control
 * Companies: Medtronic, Boston Scientific, Abbott
 */

#ifndef GF_BIOMEDICAL_COMPLIANCE_H
#define GF_BIOMEDICAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_COMPLIANCE_MAX_LOG_ENTRIES   10000
#define GF_COMPLIANCE_MAX_USERS         32
#define GF_COMPLIANCE_CHECKSUM_SIZE     32
#define GF_COMPLIANCE_SIGNATURE_SIZE    64

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_COMPLIANCE_OK = 0,
    GF_COMPLIANCE_ERROR_NULL_PTR,
    GF_COMPLIANCE_ERROR_NOT_INITIALIZED,
    GF_COMPLIANCE_ERROR_ACCESS_DENIED,
    GF_COMPLIANCE_ERROR_LOG_FULL,
    GF_COMPLIANCE_ERROR_INTEGRITY_FAIL,
    GF_COMPLIANCE_ERROR_SIGNATURE_FAIL,
    GF_COMPLIANCE_ERROR_USER_NOT_FOUND,
    GF_COMPLIANCE_ERROR_STORAGE_FAIL,
    GF_COMPLIANCE_WARN_LOG_NEARING_FULL
} gf_compliance_status_t;

typedef enum {
    GF_LOG_LEVEL_DEBUG,
    GF_LOG_LEVEL_INFO,
    GF_LOG_LEVEL_WARNING,
    GF_LOG_LEVEL_ERROR,
    GF_LOG_LEVEL_CRITICAL,
    GF_LOG_LEVEL_AUDIT
} gf_log_level_t;

typedef enum {
    GF_LOG_CAT_SYSTEM,          /* System events */
    GF_LOG_CAT_OPERATION,       /* Device operation */
    GF_LOG_CAT_SAFETY,          /* Safety events */
    GF_LOG_CAT_MAINTENANCE,     /* Maintenance actions */
    GF_LOG_CAT_USER_ACTION,     /* User interactions */
    GF_LOG_CAT_DATA_ACCESS,     /* PHI access */
    GF_LOG_CAT_CALIBRATION,     /* Calibration events */
    GF_LOG_CAT_ALARM            /* Alarms and alerts */
} gf_log_category_t;

typedef enum {
    GF_USER_ROLE_PATIENT,
    GF_USER_ROLE_CLINICIAN,
    GF_USER_ROLE_TECHNICIAN,
    GF_USER_ROLE_ADMIN,
    GF_USER_ROLE_SERVICE,
    GF_USER_ROLE_AUDITOR
} gf_user_role_t;

typedef enum {
    GF_ACCESS_READ,
    GF_ACCESS_WRITE,
    GF_ACCESS_EXECUTE,
    GF_ACCESS_ADMIN
} gf_access_type_t;

/**
 * @brief Compliance configuration
 */
typedef struct {
    bool audit_trail_enabled;
    bool electronic_signature_required;
    bool encryption_enabled;
    uint32_t log_retention_days;
    uint32_t session_timeout_sec;
    bool auto_logout_enabled;
    char device_identifier[32];
    char software_version[16];
} gf_compliance_config_t;

/**
 * @brief User identity
 */
typedef struct {
    uint16_t user_id;
    char username[32];
    gf_user_role_t role;
    uint64_t session_start_ms;
    bool authenticated;
    uint8_t access_level;
} gf_compliance_user_t;

/**
 * @brief Audit log entry
 */
typedef struct {
    uint64_t sequence_number;
    uint64_t timestamp_ms;
    uint16_t user_id;
    gf_log_level_t level;
    gf_log_category_t category;
    char message[128];
    char old_value[64];
    char new_value[64];
    uint8_t checksum[GF_COMPLIANCE_CHECKSUM_SIZE];
} gf_audit_entry_t;

/**
 * @brief Electronic signature
 */
typedef struct {
    uint16_t user_id;
    uint64_t timestamp_ms;
    char meaning[64];           /* Reason for signature */
    uint8_t signature[GF_COMPLIANCE_SIGNATURE_SIZE];
    bool verified;
} gf_electronic_signature_t;

/**
 * @brief Data integrity report
 */
typedef struct {
    uint64_t entries_checked;
    uint64_t entries_valid;
    uint64_t entries_corrupted;
    uint64_t first_corruption_seq;
    bool chain_intact;
    uint8_t overall_hash[GF_COMPLIANCE_CHECKSUM_SIZE];
} gf_integrity_report_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_audit_cb_t)(const gf_audit_entry_t* entry, void* user_data);

typedef void (*gf_access_cb_t)(uint16_t user_id,
                                gf_access_type_t access,
                                bool granted,
                                void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_compliance_status_t gf_compliance_init(const gf_compliance_config_t* config);
void gf_compliance_shutdown(void);

/* User management */
gf_compliance_status_t gf_compliance_register_user(const gf_compliance_user_t* user);
gf_compliance_status_t gf_compliance_authenticate(uint16_t user_id, const char* credential);
gf_compliance_status_t gf_compliance_logout(uint16_t user_id);
gf_compliance_status_t gf_compliance_check_access(uint16_t user_id, gf_access_type_t access);

/* Audit logging */
gf_compliance_status_t gf_compliance_log(gf_log_level_t level,
                                          gf_log_category_t category,
                                          const char* message);
gf_compliance_status_t gf_compliance_log_change(gf_log_category_t category,
                                                  const char* field,
                                                  const char* old_value,
                                                  const char* new_value);
gf_compliance_status_t gf_compliance_get_log(uint64_t start_seq,
                                              uint32_t count,
                                              gf_audit_entry_t* entries,
                                              uint32_t* actual_count);

/* Electronic signatures */
gf_compliance_status_t gf_compliance_sign(uint16_t user_id,
                                           const char* meaning,
                                           const uint8_t* data,
                                           uint32_t data_len,
                                           gf_electronic_signature_t* sig);
gf_compliance_status_t gf_compliance_verify_signature(const gf_electronic_signature_t* sig,
                                                        const uint8_t* data,
                                                        uint32_t data_len);

/* Integrity verification */
gf_compliance_status_t gf_compliance_verify_integrity(gf_integrity_report_t* report);
gf_compliance_status_t gf_compliance_export_logs(const char* filename);

/* Callbacks */
gf_compliance_status_t gf_compliance_register_audit_callback(gf_audit_cb_t cb, void* user_data);
gf_compliance_status_t gf_compliance_register_access_callback(gf_access_cb_t cb, void* user_data);

#endif /* GF_BIOMEDICAL_COMPLIANCE_H */
