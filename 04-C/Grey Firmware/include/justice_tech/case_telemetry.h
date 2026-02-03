/**
 * @file case_telemetry.h
 * @brief Case Management Telemetry and Analytics
 * 
 * INDUSTRY RELEVANCE:
 * Legal case management requires comprehensive tracking:
 * - Document access and modification logging
 * - Deadline and filing date tracking
 * - Attorney time and billing capture
 * - Court hearing and deposition scheduling
 * - Client communication archival
 * 
 * This stub demonstrates legal operations expertise for:
 * - Court management systems (Tyler Technologies)
 * - Law firm practice management (Clio, PracticePanther)
 * - Government case tracking systems
 * - Compliance monitoring platforms
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_JUSTICE_TECH_CASE_TELEMETRY_H
#define GF_JUSTICE_TECH_CASE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/** Case status */
typedef enum {
    CASE_OPEN,
    CASE_DISCOVERY,
    CASE_TRIAL,
    CASE_APPEAL,
    CASE_SETTLED,
    CASE_CLOSED
} case_status_t;

/** Event type */
typedef enum {
    EVENT_FILING,
    EVENT_HEARING,
    EVENT_DEPOSITION,
    EVENT_MOTION,
    EVENT_VERDICT,
    EVENT_APPEAL
} case_event_t;

/** Case telemetry frame */
typedef struct {
    uint32_t case_id;
    case_status_t status;
    case_event_t last_event;
    uint32_t event_timestamp;
    uint32_t next_deadline;
    uint32_t documents_filed;
    uint32_t total_hours;
    float billable_amount;
    bool urgent;
} case_frame_t;

/** Court statistics */
typedef struct {
    uint32_t total_cases;
    uint32_t open_cases;
    uint32_t pending_deadlines;
    float avg_case_duration_days;
    float clearance_rate_pct;
} court_stats_t;

/* API Functions */
int case_telemetry_init(void);
int case_telemetry_record(const case_frame_t *frame);
int case_telemetry_get_deadlines(uint32_t *case_ids, uint32_t max_count);
int case_telemetry_get_stats(court_stats_t *stats);
int case_telemetry_export(uint8_t *buffer, size_t max_len);

#endif /* GF_JUSTICE_TECH_CASE_TELEMETRY_H */
