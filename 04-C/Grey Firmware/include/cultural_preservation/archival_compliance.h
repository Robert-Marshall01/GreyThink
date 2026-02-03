/**
 * @file archival_compliance.h
 * @brief Cultural Archival Compliance Module
 *
 * INDUSTRY RELEVANCE:
 * Cultural institutions must comply with international standards for
 * preservation, access, and handling. Compliance tracking ensures
 * proper stewardship and prevents damage to irreplaceable items.
 *
 * MARKET CONTEXT:
 * - AAM (American Alliance of Museums) standards
 * - ICOM Code of Ethics compliance
 * - NAGPRA repatriation requirements
 * - UNESCO Convention compliance
 * - Insurance and loan agreement requirements
 *
 * TECHNICAL APPROACH:
 * - Environmental monitoring compliance
 * - Handling protocol verification
 * - Access control and logging
 * - Storage condition tracking
 * - Audit trail generation
 *
 * @author Grey Firmware Project
 */

#ifndef GF_ARCHIVAL_COMPLIANCE_H
#define GF_ARCHIVAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Compliance domain
 */
typedef enum {
    COMPLY_ENVIRONMENTAL,     /**< Temperature, humidity, light */
    COMPLY_HANDLING,          /**< Physical handling protocols */
    COMPLY_ACCESS,            /**< Visitor/researcher access */
    COMPLY_STORAGE,           /**< Storage conditions */
    COMPLY_DOCUMENTATION,     /**< Record keeping */
    COMPLY_SECURITY           /**< Security protocols */
} gf_comply_domain_t;

/**
 * @brief Compliance status
 */
typedef enum {
    COMPLY_STATUS_OK,
    COMPLY_STATUS_WARNING,
    COMPLY_STATUS_VIOLATION,
    COMPLY_STATUS_CRITICAL
} gf_comply_status_t;

/**
 * @brief Environmental compliance record
 */
typedef struct {
    float temp_c;
    float temp_min_c;
    float temp_max_c;
    float humidity_pct;
    float humidity_min_pct;
    float humidity_max_pct;
    float light_lux;
    float light_max_lux;
    gf_comply_status_t status;
} gf_env_compliance_t;

/**
 * @brief Access log entry
 */
typedef struct {
    char artifact_id[32];
    char accessor_id[32];
    char purpose[64];
    uint32_t access_time;
    uint32_t duration_sec;
    bool supervised;
    bool gloves_worn;
} gf_access_log_t;

/**
 * @brief Compliance audit result
 */
typedef struct {
    gf_comply_domain_t domain;
    gf_comply_status_t status;
    uint8_t checks_passed;
    uint8_t checks_total;
    char findings[256];
    uint32_t audit_time;
    char auditor_id[16];
} gf_audit_result_t;

/* Function prototypes */
int gf_archival_compliance_init(void);
int gf_archival_check_environment(const char *location, gf_env_compliance_t *env);
int gf_archival_log_access(const gf_access_log_t *log);
int gf_archival_get_access_history(const char *artifact_id, gf_access_log_t *logs,
                                    uint8_t max_count, uint8_t *found);
int gf_archival_run_audit(gf_comply_domain_t domain, gf_audit_result_t *result);
int gf_archival_get_compliance_report(uint8_t *buffer, size_t max_len);
int gf_archival_set_alert_threshold(gf_comply_domain_t domain, gf_comply_status_t threshold);
void gf_archival_compliance_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ARCHIVAL_COMPLIANCE_H */
