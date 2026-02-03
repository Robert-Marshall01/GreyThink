/**
 * @file case_telemetry.h
 * @brief Case Management Telemetry for Legal Technology
 *
 * INDUSTRY RELEVANCE:
 * Modern legal case management requires real-time tracking of deadlines,
 * document access, and procedural compliance. This module provides telemetry
 * for legal workflow systems used by law firms, courts, and e-discovery
 * platforms.
 *
 * Key capabilities demonstrated:
 * - Deadline and SOL tracking
 * - Document access auditing
 * - Billing time capture
 * - Court filing status monitoring
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_CASE_TELEMETRY_H
#define GF_CASE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Case event types */
typedef enum {
    GF_CASE_DOCUMENT_ACCESS,
    GF_CASE_FILING_SUBMITTED,
    GF_CASE_DEADLINE_APPROACHING,
    GF_CASE_STATUS_CHANGE,
    GF_CASE_BILLING_EVENT
} gf_case_event_t;

/** Case telemetry record */
typedef struct {
    char case_id[32];
    gf_case_event_t event_type;
    uint64_t timestamp;
    char user_id[32];
    char document_id[64];
    char description[128];
} gf_case_telemetry_t;

/** Deadline alert */
typedef struct {
    char case_id[32];
    char deadline_type[32];
    uint64_t due_timestamp;
    uint32_t days_remaining;
    bool acknowledged;
} gf_deadline_alert_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_case_tlm_init(void);
int gf_case_log_event(const gf_case_telemetry_t* event);
int gf_case_get_pending_deadlines(gf_deadline_alert_t* alerts, size_t max, size_t* count);
int gf_case_acknowledge_deadline(const char* case_id, const char* deadline_type);
void gf_case_tlm_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CASE_TELEMETRY_H */
