/**
 * @file medical_compliance.h
 * @brief Medical Robotics Compliance Logging
 * 
 * INDUSTRY RELEVANCE:
 * Medical robots are Class II/III devices requiring FDA 510(k) or PMA approval.
 * This module provides audit logging, electronic signatures, and traceability
 * per FDA 21 CFR Part 11, IEC 62304, and ISO 13482. Essential for regulatory
 * approval and post-market surveillance.
 * 
 * Key applications:
 * - FDA 21 CFR Part 11 electronic records
 * - IEC 62304 software lifecycle compliance
 * - ISO 13482 personal care robot safety
 * - Post-market clinical data collection
 * - Adverse event reporting (MDR/MAUDE)
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_MEDICAL_COMPLIANCE_H
#define GF_MEDICAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define MED_LOG_MAX_ENTRY_SIZE      512     /**< Max log entry size */
#define MED_LOG_QUEUE_DEPTH         256     /**< Log queue depth */
#define MED_SIGNATURE_SIZE          64      /**< Digital signature size */
#define MED_AUDIT_RETENTION_DAYS    730     /**< 2-year retention */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Log entry type
 */
typedef enum {
    MED_LOG_PROCEDURE_START = 0,
    MED_LOG_PROCEDURE_END,
    MED_LOG_OPERATOR_ACTION,
    MED_LOG_SYSTEM_EVENT,
    MED_LOG_SAFETY_EVENT,
    MED_LOG_CALIBRATION,
    MED_LOG_CONFIGURATION,
    MED_LOG_ERROR,
    MED_LOG_MAINTENANCE
} med_log_type_t;

/**
 * @brief Event severity
 */
typedef enum {
    MED_SEVERITY_INFO = 0,
    MED_SEVERITY_WARNING,
    MED_SEVERITY_ERROR,
    MED_SEVERITY_CRITICAL,
    MED_SEVERITY_SAFETY
} med_severity_t;

/**
 * @brief Electronic signature
 */
typedef struct {
    uint32_t user_id;
    uint32_t timestamp;
    char meaning[32];               /**< Signature meaning */
    uint8_t signature[MED_SIGNATURE_SIZE];
    bool verified;
} med_signature_t;

/**
 * @brief Audit log entry
 */
typedef struct {
    uint32_t sequence_number;
    uint32_t timestamp;
    med_log_type_t type;
    med_severity_t severity;
    uint32_t user_id;
    char action[64];
    char details[256];
    bool requires_signature;
    med_signature_t signature;
    uint32_t checksum;
} med_audit_entry_t;

/**
 * @brief Procedure record
 */
typedef struct {
    uint32_t procedure_id;
    uint32_t patient_id;            /**< Anonymized ID */
    uint32_t start_time;
    uint32_t end_time;
    char procedure_type[32];
    uint32_t operator_id;
    uint32_t log_start;             /**< First log entry */
    uint32_t log_end;               /**< Last log entry */
    bool completed_normally;
    char notes[256];
} med_procedure_t;

/**
 * @brief Compliance configuration
 */
typedef struct {
    bool require_operator_login;
    bool require_procedure_signature;
    bool enable_real_time_sync;
    bool enable_backup;
    uint16_t sync_interval_seconds;
    char server_url[128];
} med_compliance_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize compliance logging
 * @param config Compliance configuration
 * @return 0 on success, negative on error
 */
int med_compliance_init(const med_compliance_config_t* config);

/**
 * @brief Shutdown compliance system
 * @return 0 on success, negative on error
 */
int med_compliance_shutdown(void);

/**
 * @brief Log audit entry
 * @param type Entry type
 * @param severity Severity level
 * @param action Action description
 * @param details Additional details
 * @return Entry ID on success, negative on error
 */
int med_log_entry(med_log_type_t type, med_severity_t severity,
                  const char* action, const char* details);

/**
 * @brief Start procedure record
 * @param patient_id Anonymized patient ID
 * @param procedure_type Procedure type string
 * @return Procedure ID on success, negative on error
 */
int med_procedure_start(uint32_t patient_id, const char* procedure_type);

/**
 * @brief End procedure record
 * @param procedure_id Procedure ID
 * @param notes Procedure notes
 * @return 0 on success, negative on error
 */
int med_procedure_end(uint32_t procedure_id, const char* notes);

/**
 * @brief Apply electronic signature
 * @param entry_id Entry to sign
 * @param user_id User applying signature
 * @param meaning Signature meaning
 * @param password User password for verification
 * @return 0 on success, negative on error
 */
int med_sign_entry(uint32_t entry_id, uint32_t user_id,
                   const char* meaning, const char* password);

/**
 * @brief Operator login
 * @param user_id User ID
 * @param password Password
 * @return 0 on success, negative on error
 */
int med_operator_login(uint32_t user_id, const char* password);

/**
 * @brief Operator logout
 * @return 0 on success, negative on error
 */
int med_operator_logout(void);

/**
 * @brief Sync logs to server
 * @return Entries synced, negative on error
 */
int med_sync_logs(void);

/**
 * @brief Get procedure record
 * @param procedure_id Procedure ID
 * @param record Output record
 * @return 0 on success, negative on error
 */
int med_get_procedure(uint32_t procedure_id, med_procedure_t* record);

#ifdef __cplusplus
}
#endif

#endif /* GF_MEDICAL_COMPLIANCE_H */
