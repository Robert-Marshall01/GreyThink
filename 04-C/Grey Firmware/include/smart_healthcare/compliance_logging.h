/**
 * @file compliance_logging.h
 * @brief Compliance Logging Module - Edge Healthcare AI
 * 
 * @details Industry Relevance:
 * Medical devices require comprehensive logging for regulatory compliance
 * and post-market surveillance:
 * - Audit trails for all clinical data and decisions
 * - AI/ML model predictions with inputs for explainability
 * - Device errors and anomalies
 * - Security events (access, authentication, data exports)
 * - Patient privacy protections (de-identification, consent)
 * 
 * FDA 21 CFR Part 11 requires electronic records to be trustworthy
 * with audit trails, access controls, and electronic signatures.
 * 
 * HIPAA mandates access logging for protected health information (PHI).
 * GDPR requires processing records and data subject access support.
 * 
 * Standards: 21 CFR Part 11, HIPAA, GDPR, ISO 27001, IEC 62443
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_HEALTHCARE_COMPLIANCE_LOGGING_H
#define GF_HEALTHCARE_COMPLIANCE_LOGGING_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum log entry size */
#define GF_COMP_LOG_MAX_ENTRY_SIZE      512

/** Maximum log entries in memory */
#define GF_COMP_LOG_MAX_ENTRIES         1000

/** Log retention period (days) */
#define GF_COMP_LOG_RETENTION_DAYS      365

/** Signature key size */
#define GF_COMP_LOG_SIG_SIZE            64

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Log entry types (21 CFR Part 11 categories)
 */
typedef enum {
    GF_COMP_LOG_CLINICAL,           /**< Clinical data recorded */
    GF_COMP_LOG_AI_INFERENCE,       /**< AI model inference */
    GF_COMP_LOG_ALERT,              /**< Alert generated */
    GF_COMP_LOG_USER_ACTION,        /**< User/operator action */
    GF_COMP_LOG_CONFIGURATION,      /**< Configuration change */
    GF_COMP_LOG_ACCESS,             /**< Data access */
    GF_COMP_LOG_SECURITY,           /**< Security event */
    GF_COMP_LOG_ERROR,              /**< Device error */
    GF_COMP_LOG_CALIBRATION,        /**< Calibration event */
    GF_COMP_LOG_EXPORT              /**< Data export */
} gf_comp_log_type_t;

/**
 * @brief Log entry severity
 */
typedef enum {
    GF_COMP_LOG_SEV_DEBUG,          /**< Debug (not retained) */
    GF_COMP_LOG_SEV_INFO,           /**< Informational */
    GF_COMP_LOG_SEV_NOTICE,         /**< Normal but significant */
    GF_COMP_LOG_SEV_WARNING,        /**< Warning condition */
    GF_COMP_LOG_SEV_ERROR,          /**< Error condition */
    GF_COMP_LOG_SEV_CRITICAL        /**< Critical condition */
} gf_comp_log_severity_t;

/**
 * @brief User identity for audit trail
 */
typedef struct {
    char user_id[32];               /**< User identifier */
    char role[16];                  /**< User role */
    uint8_t auth_method;            /**< Authentication method */
    uint32_t session_id;            /**< Session identifier */
    bool elevated;                  /**< Elevated privileges */
} gf_comp_log_user_t;

/**
 * @brief AI inference audit record
 */
typedef struct {
    char model_id[32];              /**< Model used */
    char model_version[16];         /**< Model version */
    uint8_t input_hash[32];         /**< SHA-256 of input */
    uint8_t output_hash[32];        /**< SHA-256 of output */
    float confidence;               /**< Output confidence */
    uint32_t inference_time_us;     /**< Inference duration */
    uint8_t result_code;            /**< Result classification */
} gf_comp_log_ai_t;

/**
 * @brief Log entry structure
 */
typedef struct {
    uint64_t sequence_num;          /**< Monotonic sequence */
    uint32_t timestamp;             /**< Unix timestamp */
    uint16_t milliseconds;          /**< Sub-second precision */
    gf_comp_log_type_t type;        /**< Entry type */
    gf_comp_log_severity_t severity;/**< Severity level */
    char patient_id[16];            /**< Patient ID (if applicable) */
    gf_comp_log_user_t user;        /**< Acting user */
    char action[64];                /**< Action description */
    char details[256];              /**< Additional details */
    uint8_t signature[GF_COMP_LOG_SIG_SIZE]; /**< Entry signature */
    bool signed_valid;              /**< Signature verified */
} gf_comp_log_entry_t;

/**
 * @brief Compliance status
 */
typedef struct {
    uint64_t last_sequence;         /**< Last sequence number */
    uint64_t entries_total;         /**< Total entries logged */
    uint64_t entries_pending_sync;  /**< Awaiting cloud sync */
    uint32_t oldest_entry_age_days; /**< Age of oldest entry */
    bool chain_valid;               /**< Hash chain integrity */
    bool storage_healthy;           /**< Storage available */
    float storage_used_pct;         /**< Storage utilization */
    uint32_t last_sync_time;        /**< Last successful sync */
} gf_comp_log_status_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize compliance logging
 * @return 0 on success, negative error code on failure
 */
int gf_comp_log_init(void);

/**
 * @brief Log clinical data event
 * @param patient_id Patient identifier
 * @param action Action description
 * @param details Additional details
 * @return 0 on success
 */
int gf_comp_log_clinical(const char* patient_id,
                         const char* action,
                         const char* details);

/**
 * @brief Log AI inference event
 * @param patient_id Patient identifier
 * @param ai_info AI inference details
 * @return 0 on success
 */
int gf_comp_log_ai_inference(const char* patient_id,
                             const gf_comp_log_ai_t* ai_info);

/**
 * @brief Log user action
 * @param user User information
 * @param action Action taken
 * @param patient_id Patient ID (NULL if N/A)
 * @return 0 on success
 */
int gf_comp_log_user_action(const gf_comp_log_user_t* user,
                            const char* action,
                            const char* patient_id);

/**
 * @brief Log security event
 * @param severity Event severity
 * @param event Event description
 * @param details Additional details
 * @return 0 on success
 */
int gf_comp_log_security(gf_comp_log_severity_t severity,
                         const char* event,
                         const char* details);

/**
 * @brief Log error event
 * @param error_code Error code
 * @param message Error message
 * @param component Component name
 * @return 0 on success
 */
int gf_comp_log_error(int32_t error_code,
                      const char* message,
                      const char* component);

/**
 * @brief Query logs for patient
 * @param patient_id Patient to query
 * @param since Start timestamp
 * @param entries Output entry array
 * @param max_entries Maximum entries to return
 * @return Number of entries found
 */
int gf_comp_log_query(const char* patient_id,
                      uint32_t since,
                      gf_comp_log_entry_t* entries,
                      uint16_t max_entries);

/**
 * @brief Verify log chain integrity
 * @param start_sequence Starting sequence to verify
 * @param end_sequence Ending sequence
 * @return 0 if valid, -1 if tampered
 */
int gf_comp_log_verify_chain(uint64_t start_sequence,
                             uint64_t end_sequence);

/**
 * @brief Export logs for regulatory submission
 * @param patient_id Patient (NULL for all)
 * @param since Start timestamp
 * @param until End timestamp
 * @param format Export format (0=JSON, 1=CSV, 2=PDF)
 * @param output Output buffer
 * @param output_len Output length
 * @return 0 on success
 */
int gf_comp_log_export(const char* patient_id,
                       uint32_t since,
                       uint32_t until,
                       uint8_t format,
                       uint8_t* output,
                       uint32_t* output_len);

/**
 * @brief Get compliance logging status
 * @param status Output status
 * @return 0 on success
 */
int gf_comp_log_get_status(gf_comp_log_status_t* status);

/**
 * @brief Shutdown compliance logging
 * @return 0 on success
 */
int gf_comp_log_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_HEALTHCARE_COMPLIANCE_LOGGING_H */
