/**
 * @file neuro_compliance.h
 * @brief Medical Compliance Logging for Neurotech Devices
 * 
 * @details
 * Regulatory compliance framework for neurotech medical devices.
 * Implements audit logging, data integrity verification, and
 * documentation requirements for FDA, CE, and IEC standards.
 * 
 * INDUSTRY RELEVANCE:
 * - FDA 21 CFR Part 11 compliance
 * - IEC 62304 medical device software
 * - IEC 60601-1 medical electrical equipment
 * - MDR/IVDR European regulations
 * - HIPAA data protection
 * 
 * KEY FEATURES:
 * - Tamper-evident audit logging
 * - Electronic signature support
 * - Data integrity verification (CRC/hash)
 * - Session recording and playback
 * - Calibration tracking
 * - Adverse event reporting
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_NEURO_COMPLIANCE_H
#define GF_NEURO_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum audit log entries */
#define GF_COMPLIANCE_MAX_LOG_ENTRIES   1000

/** Maximum user ID length */
#define GF_COMPLIANCE_USER_ID_LEN       32

/** Electronic signature length */
#define GF_COMPLIANCE_SIGNATURE_LEN     64

/** Hash length (SHA-256) */
#define GF_COMPLIANCE_HASH_LEN          32

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Compliance status codes
 */
typedef enum {
    GF_COMPLIANCE_OK = 0,
    GF_COMPLIANCE_ERROR_NOT_INIT,
    GF_COMPLIANCE_ERROR_NULL_PTR,
    GF_COMPLIANCE_ERROR_INVALID_STATE,
    GF_COMPLIANCE_ERROR_LOG_FULL,
    GF_COMPLIANCE_ERROR_SIGNATURE_FAIL,
    GF_COMPLIANCE_ERROR_INTEGRITY_FAIL,
    GF_COMPLIANCE_WARN_CALIBRATION_DUE
} gf_compliance_status_t;

/**
 * @brief Regulatory standards
 */
typedef enum {
    GF_COMPLIANCE_FDA_21CFR11 = 0x01,
    GF_COMPLIANCE_IEC_62304 = 0x02,
    GF_COMPLIANCE_IEC_60601 = 0x04,
    GF_COMPLIANCE_MDR = 0x08,
    GF_COMPLIANCE_HIPAA = 0x10
} gf_compliance_standard_t;

/**
 * @brief Audit event types
 */
typedef enum {
    GF_AUDIT_SESSION_START,
    GF_AUDIT_SESSION_END,
    GF_AUDIT_USER_LOGIN,
    GF_AUDIT_USER_LOGOUT,
    GF_AUDIT_CONFIG_CHANGE,
    GF_AUDIT_DATA_ACCESS,
    GF_AUDIT_DATA_EXPORT,
    GF_AUDIT_CALIBRATION,
    GF_AUDIT_ERROR_EVENT,
    GF_AUDIT_ADVERSE_EVENT
} gf_audit_event_t;

/**
 * @brief Risk classification (IEC 62304)
 */
typedef enum {
    GF_RISK_CLASS_A,              /**< No injury possible */
    GF_RISK_CLASS_B,              /**< Non-serious injury possible */
    GF_RISK_CLASS_C               /**< Death or serious injury possible */
} gf_risk_class_t;

/**
 * @brief Audit log entry
 */
typedef struct {
    uint32_t entry_id;            /**< Unique entry ID */
    uint32_t timestamp;           /**< Unix timestamp */
    gf_audit_event_t event;       /**< Event type */
    char user_id[GF_COMPLIANCE_USER_ID_LEN]; /**< User identifier */
    char description[128];        /**< Event description */
    uint8_t hash[GF_COMPLIANCE_HASH_LEN]; /**< Entry hash */
    uint8_t prev_hash[GF_COMPLIANCE_HASH_LEN]; /**< Previous hash (chain) */
} gf_audit_entry_t;

/**
 * @brief Electronic signature
 */
typedef struct {
    char user_id[GF_COMPLIANCE_USER_ID_LEN]; /**< Signer ID */
    uint32_t timestamp;           /**< Signature time */
    char meaning[64];             /**< Signature meaning */
    uint8_t signature[GF_COMPLIANCE_SIGNATURE_LEN]; /**< Digital signature */
    bool verified;                /**< Signature verified */
} gf_esignature_t;

/**
 * @brief Calibration record
 */
typedef struct {
    uint32_t calibration_id;      /**< Calibration ID */
    uint32_t date;                /**< Calibration date */
    uint32_t next_due;            /**< Next calibration due */
    char technician_id[GF_COMPLIANCE_USER_ID_LEN]; /**< Technician */
    bool passed;                  /**< Calibration passed */
    char notes[128];              /**< Calibration notes */
} gf_calibration_record_t;

/**
 * @brief Compliance configuration
 */
typedef struct {
    uint8_t standards;            /**< Enabled standards (bitmask) */
    gf_risk_class_t risk_class;   /**< Device risk class */
    bool require_esig;            /**< Require electronic signatures */
    uint32_t calibration_interval_days; /**< Calibration interval */
    bool encrypt_audit_log;       /**< Encrypt audit log */
} gf_compliance_config_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize compliance module
 * @param config Compliance configuration
 * @return Status code
 */
gf_compliance_status_t gf_compliance_init(const gf_compliance_config_t* config);

/**
 * @brief Shutdown compliance module
 * @return Status code
 */
gf_compliance_status_t gf_compliance_shutdown(void);

/**
 * @brief Log audit event
 * @param event Event type
 * @param user_id User identifier
 * @param description Event description
 * @return Status code
 */
gf_compliance_status_t gf_compliance_log_event(gf_audit_event_t event,
                                                const char* user_id,
                                                const char* description);

/**
 * @brief Create electronic signature
 * @param user_id Signer user ID
 * @param meaning Signature meaning
 * @param data Data to sign
 * @param data_len Data length
 * @param esig Output signature
 * @return Status code
 */
gf_compliance_status_t gf_compliance_sign(const char* user_id,
                                           const char* meaning,
                                           const void* data,
                                           uint32_t data_len,
                                           gf_esignature_t* esig);

/**
 * @brief Verify audit log integrity
 * @return Status code (OK if valid)
 */
gf_compliance_status_t gf_compliance_verify_log(void);

/**
 * @brief Record calibration
 * @param record Calibration record
 * @return Status code
 */
gf_compliance_status_t gf_compliance_record_calibration(
    const gf_calibration_record_t* record);

/**
 * @brief Check calibration status
 * @param days_until_due Output days until calibration due
 * @return Status code
 */
gf_compliance_status_t gf_compliance_check_calibration(int32_t* days_until_due);

/**
 * @brief Export audit log
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @param bytes_written Actual bytes written
 * @return Status code
 */
gf_compliance_status_t gf_compliance_export_log(void* buffer,
                                                 uint32_t buffer_size,
                                                 uint32_t* bytes_written);

#ifdef __cplusplus
}
#endif

#endif /* GF_NEURO_COMPLIANCE_H */
