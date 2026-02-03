/**
 * @file medical_compliance.h
 * @brief Medical Device Compliance Logging (HIPAA/FDA)
 * 
 * @details
 * This module provides comprehensive compliance logging for precision
 * medicine devices. It ensures all operations are auditable and meet
 * regulatory requirements for electronic records and signatures.
 * 
 * INDUSTRY RELEVANCE:
 * - FDA-regulated Devices: All IVD and medical device manufacturers
 * - Clinical Laboratories: CLIA/CAP compliance for diagnostic testing
 * - Hospitals: EHR integration and HIPAA compliance
 * - Pharmaceutical: GxP compliance for drug manufacturing
 * - Research Institutions: IRB and data integrity requirements
 * 
 * COMPLIANCE FRAMEWORKS:
 * - FDA 21 CFR Part 11: Electronic Records and Signatures
 *   * Audit trail requirements
 *   * Electronic signature controls
 *   * System access controls
 * - HIPAA: Health Insurance Portability and Accountability Act
 *   * PHI protection
 *   * Access logging
 *   * Breach notification
 * - FDA 21 CFR Part 820: Quality System Regulation (QSR)
 * - ISO 13485: Medical Device Quality Management
 * - GDPR: EU data protection (for EU operations)
 * 
 * AUDIT REQUIREMENTS:
 * - Computer-generated, time-stamped audit trails
 * - Operator identification for all actions
 * - Reason for change documentation
 * - Secure, immutable record storage
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_MEDICAL_COMPLIANCE_H
#define GF_MEDICAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum audit trail entries */
#define GF_AUDIT_MAX_ENTRIES            10000

/** Maximum users */
#define GF_AUDIT_MAX_USERS              100

/** Signature timeout (seconds) */
#define GF_ESIG_TIMEOUT_S               300

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Audit event types (21 CFR Part 11)
 */
typedef enum {
    GF_AUDIT_LOGIN,               /**< User login */
    GF_AUDIT_LOGOUT,              /**< User logout */
    GF_AUDIT_LOGIN_FAIL,          /**< Failed login attempt */
    GF_AUDIT_CREATE,              /**< Record created */
    GF_AUDIT_MODIFY,              /**< Record modified */
    GF_AUDIT_DELETE,              /**< Record deleted */
    GF_AUDIT_VIEW,                /**< Record viewed */
    GF_AUDIT_PRINT,               /**< Record printed */
    GF_AUDIT_EXPORT,              /**< Data exported */
    GF_AUDIT_SIGN,                /**< Electronic signature */
    GF_AUDIT_VERIFY,              /**< Signature verified */
    GF_AUDIT_REJECT,              /**< Signature rejected */
    GF_AUDIT_CONFIG_CHANGE,       /**< Configuration change */
    GF_AUDIT_CALIBRATION,         /**< Calibration performed */
    GF_AUDIT_MAINTENANCE          /**< Maintenance action */
} gf_audit_event_t;

/**
 * @brief PHI access types (HIPAA)
 */
typedef enum {
    GF_PHI_ACCESS_VIEW,           /**< View PHI */
    GF_PHI_ACCESS_CREATE,         /**< Create PHI record */
    GF_PHI_ACCESS_MODIFY,         /**< Modify PHI */
    GF_PHI_ACCESS_TRANSMIT,       /**< Transmit PHI */
    GF_PHI_ACCESS_DISCLOSE        /**< Disclose PHI */
} gf_phi_access_t;

/**
 * @brief Electronic signature status
 */
typedef enum {
    GF_ESIG_NONE,                 /**< No signature */
    GF_ESIG_PENDING,              /**< Signature pending */
    GF_ESIG_SIGNED,               /**< Signed */
    GF_ESIG_REJECTED,             /**< Signature rejected */
    GF_ESIG_EXPIRED               /**< Signature expired */
} gf_esig_status_t;

/**
 * @brief Audit trail entry
 */
typedef struct {
    uint32_t sequence;            /**< Sequence number */
    gf_audit_event_t event;       /**< Event type */
    uint32_t timestamp;           /**< UTC timestamp */
    char user_id[32];             /**< User identifier */
    char record_id[64];           /**< Affected record ID */
    char old_value[128];          /**< Previous value */
    char new_value[128];          /**< New value */
    char reason[128];             /**< Reason for change */
    char workstation[32];         /**< Workstation ID */
    uint32_t checksum;            /**< Entry checksum */
} gf_audit_entry_t;

/**
 * @brief Electronic signature record
 */
typedef struct {
    char user_id[32];             /**< Signer user ID */
    char full_name[64];           /**< Signer full name */
    char title[32];               /**< Signer title */
    uint32_t timestamp;           /**< Signature timestamp */
    char meaning[64];             /**< Signature meaning */
    gf_esig_status_t status;      /**< Signature status */
    uint8_t signature_hash[32];   /**< Signature hash (SHA-256) */
} gf_esig_record_t;

/**
 * @brief PHI access log entry
 */
typedef struct {
    uint32_t timestamp;           /**< Access timestamp */
    char user_id[32];             /**< User accessing PHI */
    char patient_id[32];          /**< Patient identifier */
    gf_phi_access_t access_type;  /**< Type of access */
    char purpose[64];             /**< Purpose of access */
    bool authorized;              /**< Access was authorized */
} gf_phi_access_entry_t;

/**
 * @brief Compliance configuration
 */
typedef struct {
    bool enable_part11;           /**< Enable 21 CFR Part 11 */
    bool enable_hipaa;            /**< Enable HIPAA logging */
    bool require_esig;            /**< Require e-signatures */
    uint16_t session_timeout_min; /**< Session timeout (minutes) */
    uint8_t login_attempts_max;   /**< Max failed logins */
    bool encrypt_audit_trail;     /**< Encrypt audit entries */
} gf_compliance_config_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize compliance system
 * @param config Compliance configuration
 * @return 0 on success
 */
int gf_compliance_init(const gf_compliance_config_t* config);

/**
 * @brief Shutdown compliance system
 */
void gf_compliance_shutdown(void);

/**
 * @brief Log audit event
 * @param entry Audit entry
 * @return 0 on success
 */
int gf_audit_log(const gf_audit_entry_t* entry);

/**
 * @brief Log PHI access
 * @param entry PHI access entry
 * @return 0 on success
 */
int gf_phi_log_access(const gf_phi_access_entry_t* entry);

/**
 * @brief Request electronic signature
 * @param record_id Record to sign
 * @param meaning Signature meaning
 * @param sig Output signature record
 * @return 0 on success
 */
int gf_esig_request(const char* record_id, 
                     const char* meaning,
                     gf_esig_record_t* sig);

/**
 * @brief Verify electronic signature
 * @param sig Signature to verify
 * @return true if valid
 */
bool gf_esig_verify(const gf_esig_record_t* sig);

/**
 * @brief Get audit trail entries
 * @param start_time Start timestamp
 * @param end_time End timestamp
 * @param entries Output array
 * @param max_entries Maximum entries
 * @return Number of entries retrieved
 */
int gf_audit_get_entries(uint32_t start_time, uint32_t end_time,
                          gf_audit_entry_t* entries, uint16_t max_entries);

/**
 * @brief Generate compliance report
 * @param start_time Report start
 * @param end_time Report end
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Bytes written
 */
int gf_compliance_generate_report(uint32_t start_time, uint32_t end_time,
                                   char* buffer, uint32_t buffer_size);

#ifdef __cplusplus
}
#endif

#endif /* GF_MEDICAL_COMPLIANCE_H */
