/**
 * @file custody_compliance.h
 * @brief Chain-of-Custody Compliance Logging for Law Enforcement
 * 
 * INDUSTRY RELEVANCE:
 * Legal admissibility of digital evidence requires unbroken chain-of-custody
 * documentation. This module provides tamper-evident audit trails that meet
 * court standards for criminal and civil proceedings. Compliance logging
 * is essential for body cameras, in-car video, and mobile devices.
 * 
 * KEY CAPABILITIES:
 * - Immutable audit trail with cryptographic signing
 * - User authentication and access control logging
 * - Timestamp verification with NTP synchronization
 * - Export for court disclosure requirements
 * - Retention policy enforcement and destruction logging
 * - Multi-jurisdiction compliance (federal, state, local)
 * 
 * STANDARDS COMPLIANCE:
 * - CJIS Security Policy (FBI)
 * - NIST SP 800-86 (Digital Evidence)
 * - Federal Rules of Evidence (FRE 901, 902)
 * - State-specific body camera legislation
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_CUSTODY_COMPLIANCE_H
#define GF_CUSTODY_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Compliance status */
typedef enum {
    COMPL_STATUS_COMPLIANT,
    COMPL_STATUS_WARNING,
    COMPL_STATUS_VIOLATION,
    COMPL_STATUS_UNKNOWN
} compl_status_t;

/** Audit log entry type */
typedef enum {
    AUDIT_LOGIN,
    AUDIT_LOGOUT,
    AUDIT_ACCESS,
    AUDIT_MODIFY,
    AUDIT_DELETE,
    AUDIT_EXPORT,
    AUDIT_REDACT,
    AUDIT_TRANSFER
} audit_type_t;

/** Retention policy */
typedef struct {
    uint32_t default_days;     /**< Default retention period */
    uint32_t felony_days;      /**< Retention for felony cases */
    uint32_t civil_days;       /**< Retention for civil matters */
    bool auto_purge;           /**< Auto-delete after retention */
} retention_policy_t;

/** Audit entry */
typedef struct {
    uint32_t entry_id;
    audit_type_t type;
    uint32_t user_id;
    uint32_t timestamp;
    uint64_t resource_id;
    char action_detail[64];
    uint8_t signature[64];     /**< Digital signature */
} audit_entry_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize compliance logging system
 * @param jurisdiction_code Jurisdiction identifier
 * @return 0 on success, negative on error
 */
int compl_init(const char* jurisdiction_code);

/**
 * @brief Log an audit event
 * @param entry Audit entry to log
 * @return 0 on success, negative on error
 */
int compl_log_audit(const audit_entry_t* entry);

/**
 * @brief Check compliance status
 * @param resource_id Resource to check
 * @param status Output status
 * @return 0 on success, negative on error
 */
int compl_check_status(uint64_t resource_id, compl_status_t* status);

/**
 * @brief Set retention policy
 * @param policy Retention policy configuration
 * @return 0 on success, negative on error
 */
int compl_set_retention(const retention_policy_t* policy);

/**
 * @brief Export audit trail for disclosure
 * @param case_id Case number
 * @param buffer Output buffer
 * @param max_len Maximum buffer size
 * @param len Actual output length
 * @return 0 on success, negative on error
 */
int compl_export_audit(uint32_t case_id, uint8_t* buffer, 
                       uint32_t max_len, uint32_t* len);

/**
 * @brief Shutdown compliance logging
 * @return 0 on success, negative on error
 */
int compl_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CUSTODY_COMPLIANCE_H */
