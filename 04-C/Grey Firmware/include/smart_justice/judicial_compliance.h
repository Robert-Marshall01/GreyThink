/**
 * @file judicial_compliance.h
 * @brief Judicial Standards Compliance Logging
 * 
 * INDUSTRY RELEVANCE:
 * Legal compliance logging ensures adherence to:
 * - Professional responsibility rules (ABA Model Rules)
 * - Court procedural requirements
 * - Discovery obligations and preservation orders
 * - Client confidentiality and privilege
 * - Trust account regulations
 * 
 * Non-compliance can result in sanctions, malpractice, or disbarment.
 * Essential for law firm risk management and insurance.
 * 
 * STANDARDS: ABA Model Rules, State Bar Regulations, FRCP/FRCE
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_JUDICIAL_COMPLIANCE_H
#define GF_JUDICIAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Compliance domains */
typedef enum {
    COMPLIANCE_CONFIDENTIALITY,
    COMPLIANCE_CONFLICT_CHECK,
    COMPLIANCE_TRUST_ACCOUNT,
    COMPLIANCE_RETENTION,
    COMPLIANCE_DISCOVERY,
    COMPLIANCE_FILING,
    COMPLIANCE_ADVERTISING,
    COMPLIANCE_CLE            /**< Continuing Legal Education */
} compliance_domain_t;

/* Violation severity */
typedef enum {
    VIOLATION_MINOR,
    VIOLATION_MODERATE,
    VIOLATION_SERIOUS,
    VIOLATION_CRITICAL
} violation_severity_t;

/* Compliance status */
typedef enum {
    COMPLIANCE_OK,
    COMPLIANCE_WARNING,
    COMPLIANCE_VIOLATION,
    COMPLIANCE_REMEDIATED
} compliance_status_t;

/* Compliance check result */
typedef struct {
    compliance_domain_t domain;
    compliance_status_t status;
    uint32_t check_date;
    char description[256];
    char remediation[256];
    uint32_t due_date;
} compliance_check_t;

/* Audit entry */
typedef struct {
    uint32_t timestamp;
    compliance_domain_t domain;
    char action[64];
    char actor[64];
    char case_id[24];
    bool success;
    char details[256];
} audit_entry_t;

/**
 * @brief Initialize compliance logging
 * @return 0 on success
 */
int judicial_compliance_init(void);

/**
 * @brief Run compliance check
 * @param domain Domain to check
 * @param result Output check result
 * @return 0 on success
 */
int judicial_compliance_check(compliance_domain_t domain, compliance_check_t *result);

/**
 * @brief Log compliance action
 * @param domain Compliance domain
 * @param action Action description
 * @param actor Person/system performing action
 * @param case_id Related case (NULL if not case-specific)
 * @return 0 on success
 */
int judicial_compliance_log(compliance_domain_t domain, const char *action,
                            const char *actor, const char *case_id);

/**
 * @brief Report violation
 * @param domain Compliance domain
 * @param severity Violation severity
 * @param description Violation description
 * @param remediation Recommended remediation
 * @return 0 on success
 */
int judicial_compliance_violation(compliance_domain_t domain,
                                  violation_severity_t severity,
                                  const char *description,
                                  const char *remediation);

/**
 * @brief Get audit trail
 * @param start_time Start of time range
 * @param end_time End of time range
 * @param entries Output array for entries
 * @param max_entries Maximum entries to return
 * @return Number of entries
 */
int judicial_compliance_get_audit(uint32_t start_time, uint32_t end_time,
                                  audit_entry_t *entries, uint16_t max_entries);

/**
 * @brief Check overall compliance status
 * @return true if all domains compliant
 */
bool judicial_compliance_is_compliant(void);

/**
 * @brief Generate compliance report
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written
 */
int judicial_compliance_report(uint8_t *buffer, size_t max_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_JUDICIAL_COMPLIANCE_H */
