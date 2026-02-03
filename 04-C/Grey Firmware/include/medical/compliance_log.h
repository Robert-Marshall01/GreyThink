/**
 * @file compliance_log.h
 * @brief HIPAA-Ready Compliance Logging for Medical Devices
 *
 * INDUSTRY RELEVANCE:
 * Medical devices handling Protected Health Information (PHI) must comply
 * with HIPAA, GDPR, and FDA 21 CFR Part 11 regulations. This module shows:
 * - Tamper-evident audit logging with integrity verification
 * - Access control and authentication event tracking
 * - Data retention policy enforcement
 * - Secure timestamp and sequence numbering
 * - Encrypted log storage for PHI protection
 *
 * Used in: Clinical devices, EHR integrations, telemedicine platforms
 *
 * @note This is a stub demonstrating compliance-aware firmware design.
 */

#ifndef GF_COMPLIANCE_LOG_H
#define GF_COMPLIANCE_LOG_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Compliance Event Types                                                    */
/*===========================================================================*/

/**
 * @brief Compliance event categories per 21 CFR Part 11
 */
typedef enum {
    /* Authentication Events */
    GF_COMP_AUTH_LOGIN_SUCCESS,     /**< Successful authentication */
    GF_COMP_AUTH_LOGIN_FAILURE,     /**< Failed authentication */
    GF_COMP_AUTH_LOGOUT,            /**< User logout */
    GF_COMP_AUTH_SESSION_TIMEOUT,   /**< Session expired */
    GF_COMP_AUTH_LOCKOUT,           /**< Account locked */
    
    /* Access Control Events */
    GF_COMP_ACCESS_PHI_READ,        /**< PHI data accessed */
    GF_COMP_ACCESS_PHI_WRITE,       /**< PHI data modified */
    GF_COMP_ACCESS_PHI_DELETE,      /**< PHI data deleted */
    GF_COMP_ACCESS_DENIED,          /**< Access attempt denied */
    GF_COMP_ACCESS_EXPORT,          /**< Data exported */
    
    /* System Events */
    GF_COMP_SYS_STARTUP,            /**< Device startup */
    GF_COMP_SYS_SHUTDOWN,           /**< Controlled shutdown */
    GF_COMP_SYS_CONFIG_CHANGE,      /**< Configuration modified */
    GF_COMP_SYS_FIRMWARE_UPDATE,    /**< Firmware updated */
    GF_COMP_SYS_CLOCK_SYNC,         /**< Time synchronized */
    GF_COMP_SYS_CLOCK_ADJUST,       /**< Manual time change */
    
    /* Data Integrity Events */
    GF_COMP_DATA_BACKUP,            /**< Data backup performed */
    GF_COMP_DATA_RESTORE,           /**< Data restored */
    GF_COMP_DATA_CORRUPTION,        /**< Data corruption detected */
    GF_COMP_DATA_RETENTION_PURGE,   /**< Retention policy purge */
    
    /* Electronic Signature Events */
    GF_COMP_ESIG_APPLIED,           /**< E-signature applied */
    GF_COMP_ESIG_REVOKED,           /**< E-signature revoked */
    
    /* Device Events */
    GF_COMP_DEVICE_CALIBRATION,     /**< Device calibrated */
    GF_COMP_DEVICE_MAINTENANCE,     /**< Maintenance performed */
    GF_COMP_DEVICE_ERROR,           /**< Device error occurred */
    GF_COMP_DEVICE_TAMPER,          /**< Tamper detected */
    
    GF_COMP_EVENT_COUNT
} gf_compliance_event_t;

/**
 * @brief Event severity levels
 */
typedef enum {
    GF_COMP_SEV_INFO,               /**< Informational */
    GF_COMP_SEV_WARNING,            /**< Warning condition */
    GF_COMP_SEV_ALERT,              /**< Requires attention */
    GF_COMP_SEV_CRITICAL            /**< Immediate action required */
} gf_compliance_severity_t;

/**
 * @brief User roles for access control
 */
typedef enum {
    GF_ROLE_PATIENT,                /**< Patient/subject */
    GF_ROLE_NURSE,                  /**< Nursing staff */
    GF_ROLE_PHYSICIAN,              /**< Physician/clinician */
    GF_ROLE_TECHNICIAN,             /**< Technical staff */
    GF_ROLE_ADMIN,                  /**< Administrator */
    GF_ROLE_AUDITOR,                /**< Compliance auditor */
    GF_ROLE_SYSTEM                  /**< System/automated */
} gf_user_role_t;

/*===========================================================================*/
/* Compliance Log Structures                                                 */
/*===========================================================================*/

/**
 * @brief User identity for audit trail
 */
typedef struct {
    uint32_t user_id;               /**< Unique user identifier */
    gf_user_role_t role;            /**< User role */
    char username[32];              /**< Username (not PHI) */
    uint8_t auth_method;            /**< Authentication method used */
} gf_comp_user_t;

/**
 * @brief Compliance log entry (21 CFR Part 11 compliant)
 */
typedef struct {
    /* Entry identification */
    uint64_t sequence_number;       /**< Monotonic sequence (tamper evident) */
    uint64_t timestamp_utc;         /**< UTC timestamp (NTP synchronized) */
    
    /* Event information */
    gf_compliance_event_t event;    /**< Event type */
    gf_compliance_severity_t severity;
    
    /* Actor information */
    gf_comp_user_t user;            /**< User who performed action */
    uint32_t session_id;            /**< Session identifier */
    
    /* Resource information */
    char resource_type[16];         /**< Type of resource accessed */
    char resource_id[32];           /**< Resource identifier */
    
    /* Result */
    bool success;                   /**< Action succeeded */
    uint32_t result_code;           /**< Detailed result code */
    
    /* Context */
    char description[128];          /**< Human-readable description */
    uint8_t ip_address[16];         /**< Client IP (v4 or v6) */
    
    /* Integrity protection */
    uint8_t entry_hash[32];         /**< SHA-256 of entry */
    uint8_t chain_hash[32];         /**< Hash chain link */
} gf_compliance_entry_t;

/**
 * @brief Compliance log configuration
 */
typedef struct {
    uint32_t max_entries;           /**< Maximum log entries */
    uint32_t retention_days;        /**< Retention period (HIPAA: 6 years) */
    bool encrypt_entries;           /**< Encrypt log entries */
    bool require_esig;              /**< Require e-signatures */
    bool allow_remote_access;       /**< Allow remote log access */
    void (*tamper_callback)(void);  /**< Called on tamper detection */
    const uint8_t* encryption_key;  /**< AES-256 key (if encrypted) */
} gf_compliance_config_t;

/**
 * @brief Compliance audit report
 */
typedef struct {
    uint64_t report_timestamp;
    uint64_t start_timestamp;
    uint64_t end_timestamp;
    uint32_t total_entries;
    uint32_t entries_by_event[GF_COMP_EVENT_COUNT];
    uint32_t entries_by_severity[4];
    uint32_t failed_access_attempts;
    uint32_t phi_access_count;
    bool integrity_verified;
    uint32_t integrity_failures;
} gf_compliance_report_t;

/**
 * @brief Compliance log handle
 */
typedef struct gf_compliance_log* gf_compliance_log_t;

/*===========================================================================*/
/* Compliance Logging API                                                    */
/*===========================================================================*/

/**
 * @brief Initialize compliance logging system
 * @param config Configuration parameters
 * @param log Output handle
 * @return 0 on success
 */
int gf_compliance_init(const gf_compliance_config_t* config,
                       gf_compliance_log_t* log);

/**
 * @brief Log a compliance event
 * @param log Log handle
 * @param event Event type
 * @param severity Severity level
 * @param user User performing action
 * @param resource_type Type of resource
 * @param resource_id Resource identifier
 * @param success Action result
 * @param description Human-readable description
 * @return 0 on success
 */
int gf_compliance_log_event(gf_compliance_log_t log,
                            gf_compliance_event_t event,
                            gf_compliance_severity_t severity,
                            const gf_comp_user_t* user,
                            const char* resource_type,
                            const char* resource_id,
                            bool success,
                            const char* description);

/**
 * @brief Log PHI access (convenience wrapper)
 * @param log Log handle
 * @param user User accessing PHI
 * @param patient_id Patient identifier (de-identified)
 * @param access_type Read/Write/Delete
 * @param reason Access reason
 * @return 0 on success
 */
int gf_compliance_log_phi_access(gf_compliance_log_t log,
                                 const gf_comp_user_t* user,
                                 const char* patient_id,
                                 gf_compliance_event_t access_type,
                                 const char* reason);

/**
 * @brief Log authentication event
 * @param log Log handle
 * @param user User attempting auth
 * @param event Login success/failure/logout
 * @param ip_address Client IP address
 * @return 0 on success
 */
int gf_compliance_log_auth(gf_compliance_log_t log,
                           const gf_comp_user_t* user,
                           gf_compliance_event_t event,
                           const uint8_t* ip_address);

/**
 * @brief Verify log integrity (hash chain)
 * @param log Log handle
 * @param start_seq Starting sequence number
 * @param end_seq Ending sequence number
 * @param failures Output: number of failures
 * @return 0 if integrity verified, negative on error
 */
int gf_compliance_verify_integrity(gf_compliance_log_t log,
                                   uint64_t start_seq,
                                   uint64_t end_seq,
                                   uint32_t* failures);

/**
 * @brief Generate audit report
 * @param log Log handle
 * @param start_time Start of reporting period
 * @param end_time End of reporting period
 * @param report Output report
 * @return 0 on success
 */
int gf_compliance_generate_report(gf_compliance_log_t log,
                                  uint64_t start_time,
                                  uint64_t end_time,
                                  gf_compliance_report_t* report);

/**
 * @brief Export log entries (for audit)
 * @param log Log handle
 * @param start_seq Starting sequence
 * @param entries Output buffer
 * @param max_entries Buffer capacity
 * @param out_count Number of entries exported
 * @return 0 on success
 */
int gf_compliance_export(gf_compliance_log_t log,
                         uint64_t start_seq,
                         gf_compliance_entry_t* entries,
                         size_t max_entries,
                         size_t* out_count);

/**
 * @brief Apply retention policy (purge old entries)
 * @param log Log handle
 * @param purged_count Output: number of entries purged
 * @return 0 on success
 */
int gf_compliance_apply_retention(gf_compliance_log_t log,
                                  uint32_t* purged_count);

/**
 * @brief Deinitialize compliance logging
 * @param log Log handle
 */
void gf_compliance_deinit(gf_compliance_log_t log);

/*===========================================================================*/
/* Electronic Signature Support (21 CFR Part 11)                             */
/*===========================================================================*/

/**
 * @brief Electronic signature data
 */
typedef struct {
    gf_comp_user_t signer;          /**< Signing user */
    uint64_t timestamp;             /**< Signature timestamp */
    char meaning[64];               /**< Signature meaning */
    uint8_t signature[64];          /**< ECDSA signature */
} gf_electronic_sig_t;

/**
 * @brief Apply electronic signature to log entry
 * @param log Log handle
 * @param sequence Entry sequence number
 * @param signer Signing user
 * @param pin User PIN for verification
 * @param meaning Signature meaning
 * @return 0 on success
 */
int gf_compliance_apply_esig(gf_compliance_log_t log,
                             uint64_t sequence,
                             const gf_comp_user_t* signer,
                             const char* pin,
                             const char* meaning);

#ifdef __cplusplus
}
#endif

#endif /* GF_COMPLIANCE_LOG_H */
