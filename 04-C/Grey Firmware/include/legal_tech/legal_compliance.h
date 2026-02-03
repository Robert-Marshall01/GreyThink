/**
 * @file legal_compliance.h
 * @brief Legal Standards Compliance Logging
 *
 * INDUSTRY RELEVANCE:
 * Legal technology must comply with regulations like GDPR, CCPA, attorney-client
 * privilege rules, and court-mandated retention policies. This module provides
 * compliance tracking and audit capabilities for legal software platforms.
 *
 * Key capabilities demonstrated:
 * - Data retention policy enforcement
 * - Privilege log generation
 * - GDPR Article 30 record-keeping
 * - Audit trail for regulatory review
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_LEGAL_COMPLIANCE_H
#define GF_LEGAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Compliance framework */
typedef enum {
    GF_COMPLIANCE_GDPR,
    GF_COMPLIANCE_CCPA,
    GF_COMPLIANCE_HIPAA,
    GF_COMPLIANCE_SOX,
    GF_COMPLIANCE_COURT_ORDER
} gf_compliance_framework_t;

/** Data subject action types */
typedef enum {
    GF_DSR_ACCESS_REQUEST,          /**< Data access request */
    GF_DSR_DELETION_REQUEST,        /**< Right to erasure */
    GF_DSR_EXPORT_REQUEST,          /**< Data portability */
    GF_DSR_CORRECTION_REQUEST       /**< Data correction */
} gf_dsr_type_t;

/** Compliance audit entry */
typedef struct {
    uint64_t audit_id;
    gf_compliance_framework_t framework;
    uint64_t timestamp;
    char action[64];
    char subject_id[64];
    char data_category[32];
    bool compliant;
    char notes[256];
} gf_compliance_entry_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_legal_compliance_init(void);
int gf_legal_log_action(const gf_compliance_entry_t* entry);
int gf_legal_process_dsr(gf_dsr_type_t type, const char* subject_id);
int gf_legal_generate_audit_report(gf_compliance_framework_t framework, 
                                   void* buffer, size_t* len);
int gf_legal_check_retention(const char* document_id, bool* should_delete);
void gf_legal_compliance_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_LEGAL_COMPLIANCE_H */
