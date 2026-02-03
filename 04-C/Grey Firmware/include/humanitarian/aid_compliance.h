/**
 * @file aid_compliance.h
 * @brief Humanitarian Aid Distribution Compliance
 * 
 * INDUSTRY RELEVANCE:
 * Humanitarian accountability requires tracking that aid reaches
 * intended beneficiaries. Donors, governments, and standards bodies
 * require transparent reporting on distribution effectiveness.
 * 
 * STANDARDS:
 * - Core Humanitarian Standard (CHS)
 * - Sphere Humanitarian Charter
 * - USAID/ECHO reporting requirements
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_HUMANITARIAN_COMPLIANCE_H
#define GF_HUMANITARIAN_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    AID_STANDARD_CHS,
    AID_STANDARD_SPHERE,
    AID_STANDARD_USAID,
    AID_STANDARD_ECHO,
    AID_STANDARD_LOCAL
} aid_standard_t;

typedef struct {
    uint32_t distribution_id;
    aid_standard_t standard;
    uint32_t beneficiaries_reached;
    uint32_t beneficiaries_planned;
    float coverage_percent;
    bool verification_complete;
    char notes[128];
} compliance_report_t;

int aid_compliance_init(void);
int aid_compliance_log_distribution(uint32_t dist_id, uint32_t beneficiaries);
int aid_compliance_verify(uint32_t dist_id, bool verified);
int aid_compliance_generate_report(aid_standard_t std, compliance_report_t *report);

#ifdef __cplusplus
}
#endif

#endif /* GF_HUMANITARIAN_COMPLIANCE_H */
