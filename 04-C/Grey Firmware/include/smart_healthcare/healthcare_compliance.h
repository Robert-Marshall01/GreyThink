/**
 * @file healthcare_compliance.h
 * @brief Healthcare Regulatory Compliance Logging Module
 * 
 * @details
 * This module provides audit logging and compliance tracking for healthcare
 * facilities, ensuring adherence to HIPAA, FDA 21 CFR Part 11, and other
 * regulatory requirements for medical device software.
 * 
 * INDUSTRY RELEVANCE:
 * - HIPAA Privacy Rule compliance
 * - FDA 21 CFR Part 11 electronic records
 * - IEC 62304 software lifecycle
 * - ISO 13485 quality management
 * - Joint Commission accreditation
 * - GDPR health data protection
 * 
 * KEY FEATURES:
 * - Tamper-evident audit trails
 * - Electronic signature support
 * - PHI access logging
 * - User authentication integration
 * - Retention policy enforcement
 * - Export for regulatory audits
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_HEALTHCARE_COMPLIANCE_H
#define GF_HEALTHCARE_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_HC_OK = 0,
    GF_HC_ERROR_NOT_INITIALIZED,
    GF_HC_ERROR_NULL_PTR,
    GF_HC_ERROR_STORAGE_FULL,
    GF_HC_ERROR_INVALID_USER,
    GF_HC_ERROR_SIGNATURE_FAILED,
    GF_HC_ERROR_TAMPER_DETECTED,
    GF_HC_WARN_RETENTION_EXPIRED
} gf_hc_status_t;

typedef enum {
    GF_HC_EVENT_LOGIN,            /**< User login attempt */
    GF_HC_EVENT_LOGOUT,           /**< User logout */
    GF_HC_EVENT_PHI_ACCESS,       /**< PHI record access */
    GF_HC_EVENT_PHI_MODIFY,       /**< PHI record modification */
    GF_HC_EVENT_PHI_CREATE,       /**< PHI record creation */
    GF_HC_EVENT_PHI_DELETE,       /**< PHI record deletion */
    GF_HC_EVENT_CONFIG_CHANGE,    /**< System configuration change */
    GF_HC_EVENT_ALARM,            /**< Clinical alarm event */
    GF_HC_EVENT_SIGNATURE,        /**< Electronic signature */
    GF_HC_EVENT_EXPORT,           /**< Data export event */
    GF_HC_EVENT_SECURITY          /**< Security event */
} gf_hc_event_type_t;

typedef enum {
    GF_HC_SEVERITY_INFO,
    GF_HC_SEVERITY_WARNING,
    GF_HC_SEVERITY_ERROR,
    GF_HC_SEVERITY_CRITICAL
} gf_hc_severity_t;

typedef struct {
    uint32_t user_id;             /**< User identifier */
    char username[32];            /**< Username */
    uint8_t role_level;           /**< Access role level */
    uint32_t session_id;          /**< Session identifier */
} gf_hc_user_context_t;

typedef struct {
    uint64_t event_id;            /**< Unique event ID */
    uint64_t timestamp_ms;        /**< Event timestamp */
    gf_hc_event_type_t type;      /**< Event type */
    gf_hc_severity_t severity;    /**< Event severity */
    gf_hc_user_context_t user;    /**< User context */
    uint32_t patient_id;          /**< Patient ID if PHI event */
    char description[128];        /**< Event description */
    uint8_t hash[32];             /**< SHA-256 integrity hash */
} gf_hc_audit_record_t;

typedef struct {
    uint32_t signature_id;        /**< Signature ID */
    uint32_t user_id;             /**< Signer user ID */
    uint64_t timestamp_ms;        /**< Signature time */
    char meaning[64];             /**< Signature meaning */
    uint8_t signature[64];        /**< Digital signature */
} gf_hc_signature_t;

typedef struct {
    uint32_t total_records;       /**< Total audit records */
    uint32_t phi_access_count;    /**< PHI access events */
    uint32_t security_events;     /**< Security events */
    uint64_t oldest_record_ms;    /**< Oldest record timestamp */
    uint64_t storage_used_bytes;  /**< Storage used */
} gf_hc_stats_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_hc_status_t gf_hc_init(void);
void gf_hc_shutdown(void);
gf_hc_status_t gf_hc_set_user_context(const gf_hc_user_context_t* user);
gf_hc_status_t gf_hc_log_event(gf_hc_event_type_t type, gf_hc_severity_t severity,
                                uint32_t patient_id, const char* description);
gf_hc_status_t gf_hc_add_signature(const char* meaning, const uint8_t* auth_data,
                                    uint16_t auth_len);
gf_hc_status_t gf_hc_query_records(uint64_t start_time, uint64_t end_time,
                                    gf_hc_audit_record_t* records, uint16_t max_records,
                                    uint16_t* count);
gf_hc_status_t gf_hc_export_for_audit(const char* filename, uint64_t start_time,
                                       uint64_t end_time);
gf_hc_status_t gf_hc_verify_integrity(uint64_t start_id, uint64_t end_id, bool* valid);
gf_hc_status_t gf_hc_get_stats(gf_hc_stats_t* stats);
gf_hc_status_t gf_hc_apply_retention_policy(uint32_t retention_days);

#ifdef __cplusplus
}
#endif

#endif /* GF_HEALTHCARE_COMPLIANCE_H */
