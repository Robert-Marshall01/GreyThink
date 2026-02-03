/**
 * @file heritage_compliance.h
 * @brief Conservation Standards Compliance Logging
 * 
 * INDUSTRY RELEVANCE:
 * Museums and heritage institutions must demonstrate compliance with
 * conservation standards for accreditation, insurance, and loan agreements.
 * This module provides audit trail generation, compliance scoring, and
 * reporting for regulatory and institutional requirements.
 * 
 * KEY CAPABILITIES:
 * - Accreditation compliance tracking (AAM, AIC)
 * - Insurance requirements documentation
 * - Loan agreement condition reporting
 * - Environmental deviation logging
 * - Incident recording and response
 * - Export for audits and assessments
 * 
 * COMPLIANCE FRAMEWORKS:
 * - American Alliance of Museums (AAM)
 * - American Institute for Conservation (AIC)
 * - ICOM Code of Ethics
 * - UK Museum Accreditation
 * - Spectrum Standard (Collections Management)
 * 
 * AUDIT CAPABILITIES:
 * - Environmental compliance history
 * - Conservation treatment records
 * - Loan condition reports
 * - Security incident logs
 * - Staff certification tracking
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_HERITAGE_COMPLIANCE_H
#define GF_HERITAGE_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Compliance framework */
typedef enum {
    FRAMEWORK_AAM,             /**< American Alliance of Museums */
    FRAMEWORK_AIC,             /**< American Institute for Conservation */
    FRAMEWORK_ICOM,            /**< Int'l Council of Museums */
    FRAMEWORK_UK_ACCRED,       /**< UK Museum Accreditation */
    FRAMEWORK_SPECTRUM         /**< Collections Standard */
} compliance_framework_t;

/** Compliance status */
typedef enum {
    CSTATUS_FULLY_COMPLIANT,
    CSTATUS_MINOR_DEVIATIONS,
    CSTATUS_MAJOR_DEVIATIONS,
    CSTATUS_NON_COMPLIANT
} compliance_status_t;

/** Deviation type */
typedef enum {
    DEV_ENVIRONMENTAL,         /**< Temperature/humidity out of range */
    DEV_SECURITY,              /**< Access/handling violation */
    DEV_DOCUMENTATION,         /**< Missing documentation */
    DEV_CONSERVATION,          /**< Overdue conservation */
    DEV_STORAGE                /**< Storage condition issue */
} deviation_type_t;

/** Deviation record */
typedef struct {
    uint32_t deviation_id;
    deviation_type_t type;
    uint64_t artifact_id;      /**< 0 if zone-level */
    uint8_t zone_id;
    uint32_t start_time;
    uint32_t end_time;         /**< 0 if ongoing */
    char description[128];
    bool resolved;
    char corrective_action[128];
} deviation_record_t;

/** Compliance score */
typedef struct {
    compliance_framework_t framework;
    float overall_score;       /**< 0-100% */
    float environmental_score;
    float documentation_score;
    float conservation_score;
    float security_score;
    uint16_t active_deviations;
    uint32_t last_assessment;
} compliance_score_t;

/** Loan condition report */
typedef struct {
    uint64_t artifact_id;
    uint32_t loan_id;
    uint32_t departure_date;
    uint32_t return_date;
    float condition_score_out; /**< 1-5 scale */
    float condition_score_in;
    char notes[256];
    bool damage_detected;
} loan_report_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize compliance system
 * @param institution_id Institution identifier
 * @return 0 on success, negative on error
 */
int heritage_compl_init(uint32_t institution_id);

/**
 * @brief Set active compliance frameworks
 * @param frameworks Array of frameworks
 * @param count Number of frameworks
 * @return 0 on success, negative on error
 */
int heritage_compl_set_frameworks(const compliance_framework_t* frameworks,
                                  uint8_t count);

/**
 * @brief Record deviation
 * @param deviation Deviation record
 * @return Deviation ID, negative on error
 */
int32_t heritage_compl_record_deviation(const deviation_record_t* deviation);

/**
 * @brief Resolve deviation
 * @param deviation_id Deviation to resolve
 * @param corrective_action Action taken
 * @return 0 on success, negative on error
 */
int heritage_compl_resolve_deviation(uint32_t deviation_id,
                                     const char* corrective_action);

/**
 * @brief Get compliance score
 * @param framework Framework to assess
 * @param score Output score
 * @return 0 on success, negative on error
 */
int heritage_compl_get_score(compliance_framework_t framework,
                             compliance_score_t* score);

/**
 * @brief Generate loan condition report
 * @param report Loan report data
 * @return 0 on success, negative on error
 */
int heritage_compl_loan_report(const loan_report_t* report);

/**
 * @brief Export compliance report
 * @param framework Framework for report
 * @param buffer Output buffer
 * @param max_len Maximum length
 * @param len Actual length
 * @return 0 on success, negative on error
 */
int heritage_compl_export(compliance_framework_t framework, uint8_t* buffer,
                          uint32_t max_len, uint32_t* len);

/**
 * @brief Shutdown compliance system
 * @return 0 on success, negative on error
 */
int heritage_compl_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_HERITAGE_COMPLIANCE_H */
