/**
 * @file case_telemetry.h
 * @brief Legal Case Management Telemetry Collector
 * 
 * INDUSTRY RELEVANCE:
 * Legal technology is transforming case management through:
 * - Real-time case status tracking
 * - Deadline and calendar management
 * - Document access and review analytics
 * - Court filing automation
 * - Billing and time tracking integration
 * 
 * LegalTech market projected at $25B by 2025.
 * Key players: Clio, MyCase, PracticePanther
 * 
 * STANDARDS: ABA Model Rules, Court Electronic Filing Standards
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CASE_TELEMETRY_H
#define GF_CASE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Case status */
typedef enum {
    CASE_STATUS_INTAKE,
    CASE_STATUS_ACTIVE,
    CASE_STATUS_DISCOVERY,
    CASE_STATUS_TRIAL_PREP,
    CASE_STATUS_TRIAL,
    CASE_STATUS_APPEAL,
    CASE_STATUS_CLOSED,
    CASE_STATUS_ARCHIVED
} case_status_t;

/* Event types */
typedef enum {
    CASE_EVENT_CREATED,
    CASE_EVENT_DOCUMENT_ADDED,
    CASE_EVENT_DEADLINE_SET,
    CASE_EVENT_FILING_SUBMITTED,
    CASE_EVENT_HEARING_SCHEDULED,
    CASE_EVENT_STATUS_CHANGE,
    CASE_EVENT_NOTE_ADDED,
    CASE_EVENT_TIME_ENTRY
} case_event_t;

/* Case summary */
typedef struct {
    char case_id[24];
    char case_name[128];
    case_status_t status;
    char attorney[64];
    char client[64];
    uint32_t opened_date;
    uint32_t last_activity;
    uint16_t document_count;
    uint16_t pending_deadlines;
    float billable_hours;
    float outstanding_balance;
} case_summary_t;

/* Deadline data */
typedef struct {
    char deadline_id[16];
    char case_id[24];
    char description[128];
    uint32_t due_date;
    uint8_t priority;      /**< 1-5 priority level */
    bool court_deadline;   /**< Non-negotiable court deadline */
    bool completed;
} case_deadline_t;

/**
 * @brief Initialize case telemetry system
 * @return 0 on success
 */
int case_telem_init(void);

/**
 * @brief Register new case
 * @param summary Case summary data
 * @return 0 on success
 */
int case_telem_register(const case_summary_t *summary);

/**
 * @brief Log case event
 * @param case_id Case identifier
 * @param event Event type
 * @param description Event description
 * @return 0 on success
 */
int case_telem_log_event(const char *case_id, case_event_t event,
                         const char *description);

/**
 * @brief Update case status
 * @param case_id Case identifier
 * @param status New status
 * @return 0 on success
 */
int case_telem_update_status(const char *case_id, case_status_t status);

/**
 * @brief Add deadline
 * @param deadline Deadline data
 * @return 0 on success
 */
int case_telem_add_deadline(const case_deadline_t *deadline);

/**
 * @brief Get upcoming deadlines
 * @param days_ahead Number of days to look ahead
 * @param deadlines Output array
 * @param max_deadlines Maximum to return
 * @return Number of deadlines, or negative on error
 */
int case_telem_get_deadlines(uint16_t days_ahead, case_deadline_t *deadlines,
                             uint16_t max_deadlines);

/**
 * @brief Record time entry
 * @param case_id Case identifier
 * @param hours Time in hours
 * @param description Work description
 * @param billable Whether time is billable
 * @return 0 on success
 */
int case_telem_record_time(const char *case_id, float hours,
                           const char *description, bool billable);

/**
 * @brief Generate case activity report
 * @param case_id Case identifier (NULL for all cases)
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written
 */
int case_telem_generate_report(const char *case_id, uint8_t *buffer, size_t max_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_CASE_TELEMETRY_H */
