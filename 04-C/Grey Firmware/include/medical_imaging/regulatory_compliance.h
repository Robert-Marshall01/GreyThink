/**
 * @file regulatory_compliance.h
 * @brief Medical Device Regulatory Compliance Logging
 *
 * INDUSTRY RELEVANCE:
 * Medical devices require rigorous documentation for regulatory approval.
 * This module demonstrates expertise in:
 * - FDA 21 CFR Part 11 (electronic records and signatures)
 * - IEC 62304 (medical device software lifecycle)
 * - ISO 14971 (risk management)
 * - HIPAA (patient data protection)
 * - Audit trail generation for regulatory inspections
 *
 * These skills are essential for medical device companies seeking FDA 510(k)
 * clearance, CE marking, or ISO 13485 certification. Applicable to roles at
 * Medtronic, Abbott, Boston Scientific, and medical device startups.
 *
 * @note This is a stub module demonstrating compliance framework design.
 *       Production implementation requires legal/regulatory review.
 */

#ifndef GF_REGULATORY_COMPLIANCE_H
#define GF_REGULATORY_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_REG_MAX_LOG_ENTRY_SIZE   512     /**< Maximum log entry size */
#define GF_REG_MAX_USER_ID_LEN      64      /**< Maximum user ID length */
#define GF_REG_MAX_SIGNATURE_LEN    256     /**< Digital signature length */
#define GF_REG_AUDIT_RETENTION_DAYS 730     /**< 2-year audit trail retention */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Regulatory framework types
 */
typedef enum {
    GF_REG_FDA_21CFR11,         /**< FDA electronic records */
    GF_REG_FDA_510K,            /**< FDA 510(k) premarket notification */
    GF_REG_FDA_PMA,             /**< FDA premarket approval */
    GF_REG_CE_MDR,              /**< EU Medical Device Regulation */
    GF_REG_ISO_13485,           /**< Quality management standard */
    GF_REG_IEC_62304,           /**< Software lifecycle standard */
    GF_REG_ISO_14971,           /**< Risk management */
    GF_REG_HIPAA               /**< Health data privacy */
} gf_reg_framework_t;

/**
 * @brief Audit event types (FDA 21 CFR Part 11 categories)
 */
typedef enum {
    GF_AUDIT_LOGIN,             /**< User authentication */
    GF_AUDIT_LOGOUT,            /**< User session end */
    GF_AUDIT_CONFIG_CHANGE,     /**< Configuration modification */
    GF_AUDIT_DATA_CREATE,       /**< Record creation */
    GF_AUDIT_DATA_MODIFY,       /**< Record modification */
    GF_AUDIT_DATA_DELETE,       /**< Record deletion */
    GF_AUDIT_DATA_ACCESS,       /**< Record access */
    GF_AUDIT_SIGNATURE,         /**< Electronic signature applied */
    GF_AUDIT_CALIBRATION,       /**< Device calibration event */
    GF_AUDIT_MAINTENANCE,       /**< Maintenance activity */
    GF_AUDIT_ALARM,             /**< Safety alarm triggered */
    GF_AUDIT_ERROR,             /**< System error */
    GF_AUDIT_EXPORT             /**< Data export event */
} gf_audit_event_t;

/**
 * @brief Risk level classification (ISO 14971)
 */
typedef enum {
    GF_RISK_NEGLIGIBLE,         /**< No significant harm */
    GF_RISK_MINOR,              /**< Temporary discomfort */
    GF_RISK_SERIOUS,            /**< Injury requiring treatment */
    GF_RISK_CRITICAL,           /**< Life-threatening */
    GF_RISK_CATASTROPHIC        /**< Death or permanent injury */
} gf_risk_level_t;

/**
 * @brief Software safety classification (IEC 62304)
 */
typedef enum {
    GF_SW_CLASS_A,              /**< No injury possible */
    GF_SW_CLASS_B,              /**< Non-serious injury possible */
    GF_SW_CLASS_C               /**< Serious injury or death possible */
} gf_sw_safety_class_t;

/**
 * @brief Audit log entry
 */
typedef struct {
    uint64_t timestamp_utc;         /**< UTC timestamp (seconds since epoch) */
    uint32_t sequence_number;       /**< Monotonic sequence number */
    gf_audit_event_t event_type;    /**< Event classification */
    char user_id[GF_REG_MAX_USER_ID_LEN];   /**< Authenticated user */
    char workstation_id[32];        /**< Device/workstation identifier */
    char description[256];          /**< Event description */
    char old_value[128];            /**< Previous value (for changes) */
    char new_value[128];            /**< New value (for changes) */
    uint8_t checksum[32];           /**< SHA-256 integrity checksum */
} gf_audit_entry_t;

/**
 * @brief Electronic signature (21 CFR Part 11 compliant)
 */
typedef struct {
    char signer_id[GF_REG_MAX_USER_ID_LEN]; /**< Signer identification */
    char signer_name[64];           /**< Printed name */
    uint64_t timestamp_utc;         /**< Signature timestamp */
    char meaning[64];               /**< Signature meaning (e.g., "Authored", "Reviewed") */
    uint8_t signature[GF_REG_MAX_SIGNATURE_LEN]; /**< Cryptographic signature */
    uint16_t signature_len;         /**< Signature length */
} gf_electronic_signature_t;

/**
 * @brief Compliance status
 */
typedef struct {
    gf_reg_framework_t framework;   /**< Regulatory framework */
    bool compliant;                 /**< Current compliance status */
    uint32_t violations_count;      /**< Number of violations detected */
    uint32_t last_audit_date;       /**< Last audit timestamp */
    char last_violation[256];       /**< Most recent violation description */
} gf_compliance_status_t;

/**
 * @brief Risk assessment entry
 */
typedef struct {
    uint32_t hazard_id;             /**< Unique hazard identifier */
    char hazard_description[256];   /**< Hazard description */
    gf_risk_level_t severity;       /**< Severity classification */
    uint8_t probability;            /**< Occurrence probability (1-5) */
    uint8_t detectability;          /**< Detectability rating (1-5) */
    uint16_t risk_priority;         /**< Calculated RPN */
    char mitigation[256];           /**< Mitigation measures */
    bool mitigation_verified;       /**< Verification status */
} gf_risk_entry_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize regulatory compliance subsystem
 * @param safety_class IEC 62304 software safety classification
 * @return 0 on success, negative error code on failure
 */
int gf_reg_init(gf_sw_safety_class_t safety_class);

/**
 * @brief Shutdown compliance subsystem
 */
void gf_reg_deinit(void);

/**
 * @brief Log audit event
 * @param event Event type
 * @param user_id Authenticated user (NULL for system events)
 * @param description Event description
 * @return 0 on success, negative error code on failure
 */
int gf_reg_log_audit(gf_audit_event_t event, const char* user_id, const char* description);

/**
 * @brief Log configuration change with before/after values
 * @param user_id Authenticated user
 * @param param_name Parameter name
 * @param old_value Previous value
 * @param new_value New value
 * @return 0 on success
 */
int gf_reg_log_config_change(const char* user_id, const char* param_name,
                              const char* old_value, const char* new_value);

/**
 * @brief Apply electronic signature to record
 * @param record_id Record identifier
 * @param signer User applying signature
 * @param meaning Signature meaning
 * @param signature Output signature structure
 * @return 0 on success, negative error code on failure
 */
int gf_reg_sign_record(const char* record_id, const char* signer,
                        const char* meaning, gf_electronic_signature_t* signature);

/**
 * @brief Verify electronic signature
 * @param record_id Record identifier
 * @param signature Signature to verify
 * @return 0 if valid, negative error code if invalid
 */
int gf_reg_verify_signature(const char* record_id, const gf_electronic_signature_t* signature);

/**
 * @brief Get compliance status for framework
 * @param framework Regulatory framework to check
 * @param[out] status Output status structure
 * @return 0 on success
 */
int gf_reg_get_compliance(gf_reg_framework_t framework, gf_compliance_status_t* status);

/**
 * @brief Export audit trail for regulatory inspection
 * @param start_time Start of time range (UTC)
 * @param end_time End of time range (UTC)
 * @param output_path Path for export file
 * @return Number of entries exported, or negative error code
 */
int gf_reg_export_audit_trail(uint64_t start_time, uint64_t end_time, const char* output_path);

/**
 * @brief Register risk entry
 * @param entry Risk assessment entry
 * @return Assigned hazard ID, or negative error code
 */
int gf_reg_register_risk(const gf_risk_entry_t* entry);

/**
 * @brief Verify audit trail integrity
 * @return 0 if intact, number of corrupted entries otherwise
 */
int gf_reg_verify_audit_integrity(void);

/**
 * @brief Get device unique identifier (UDI) for traceability
 * @param[out] udi Buffer for UDI string
 * @param udi_len Buffer length
 * @return 0 on success
 */
int gf_reg_get_udi(char* udi, size_t udi_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_REGULATORY_COMPLIANCE_H */
