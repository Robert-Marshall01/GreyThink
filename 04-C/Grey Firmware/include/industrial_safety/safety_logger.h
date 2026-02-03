/**
 * @file safety_logger.h
 * @brief Safety Compliance Logger
 * 
 * INDUSTRY RELEVANCE:
 * Safety-critical systems require comprehensive logging for regulatory compliance,
 * incident investigation, and audit trails. Industries like oil & gas, nuclear,
 * pharmaceutical, and food processing must maintain detailed records (21 CFR Part 11,
 * OSHA Process Safety Management, IEC 61511).
 * 
 * WHY THIS MATTERS:
 * - Tamper-evident logging with integrity checks
 * - Time-stamped event sequencing
 * - Regulatory compliance (FDA, OSHA, EPA)
 * - Incident reconstruction capability
 * - Long-term archival requirements
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Secure logging with integrity verification
 * - Structured event data
 * - Query and export capabilities
 * - Retention policy enforcement
 */

#ifndef GF_SAFETY_LOGGER_H
#define GF_SAFETY_LOGGER_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_SLOG_MAX_ENTRIES         4096    /* Maximum log entries */
#define GF_SLOG_MESSAGE_LEN         64      /* Max message length */
#define GF_SLOG_TAG_LEN             16      /* Equipment tag length */
#define GF_SLOG_USER_LEN            16      /* User ID length */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Event category */
typedef enum {
    GF_SLOG_CAT_ALARM = 0,          /* Alarm events */
    GF_SLOG_CAT_TRIP,               /* Trip/shutdown events */
    GF_SLOG_CAT_BYPASS,             /* Bypass events */
    GF_SLOG_CAT_CALIBRATION,        /* Calibration events */
    GF_SLOG_CAT_MAINTENANCE,        /* Maintenance events */
    GF_SLOG_CAT_CONFIGURATION,      /* Configuration changes */
    GF_SLOG_CAT_SECURITY,           /* Security events */
    GF_SLOG_CAT_DIAGNOSTIC,         /* Diagnostic events */
    GF_SLOG_CAT_OPERATOR,           /* Operator actions */
    GF_SLOG_CAT_SYSTEM              /* System events */
} gf_slog_category_t;

/* Event severity */
typedef enum {
    GF_SLOG_SEV_INFO = 0,           /* Informational */
    GF_SLOG_SEV_WARNING,            /* Warning */
    GF_SLOG_SEV_ALERT,              /* Alert requiring attention */
    GF_SLOG_SEV_CRITICAL,           /* Critical safety event */
    GF_SLOG_SEV_EMERGENCY           /* Emergency condition */
} gf_slog_severity_t;

/* Event action (for compliance) */
typedef enum {
    GF_SLOG_ACT_NONE = 0,           /* No action required */
    GF_SLOG_ACT_ACKNOWLEDGE,        /* Requires acknowledgment */
    GF_SLOG_ACT_INVESTIGATE,        /* Requires investigation */
    GF_SLOG_ACT_REPORT              /* Requires regulatory report */
} gf_slog_action_t;

/* Log entry */
typedef struct {
    uint32_t        sequence;           /* Sequence number */
    uint32_t        timestamp;          /* Unix timestamp */
    uint16_t        timestamp_ms;       /* Milliseconds */
    gf_slog_category_t category;        /* Event category */
    gf_slog_severity_t severity;        /* Severity level */
    uint16_t        event_code;         /* Numeric event code */
    char            equipment_tag[GF_SLOG_TAG_LEN];   /* Equipment tag */
    char            message[GF_SLOG_MESSAGE_LEN];     /* Event message */
    char            user_id[GF_SLOG_USER_LEN];        /* User who caused event */
    float           value_before;       /* Value before event */
    float           value_after;        /* Value after event */
    gf_slog_action_t required_action;   /* Required follow-up */
    bool            acknowledged;       /* Has been acknowledged */
    uint32_t        ack_timestamp;      /* Acknowledgment time */
    char            ack_user[GF_SLOG_USER_LEN];       /* Who acknowledged */
    uint32_t        checksum;           /* Entry integrity checksum */
} gf_slog_entry_t;

/* Query filter */
typedef struct {
    uint32_t        start_time;         /* Start timestamp (0 = any) */
    uint32_t        end_time;           /* End timestamp (0 = any) */
    gf_slog_category_t category;        /* Category filter (0xFF = all) */
    gf_slog_severity_t min_severity;    /* Minimum severity */
    char            equipment_tag[GF_SLOG_TAG_LEN];   /* Tag filter (empty = all) */
    bool            unacknowledged_only;/* Only unacknowledged */
} gf_slog_filter_t;

/* Statistics */
typedef struct {
    uint32_t        total_entries;
    uint32_t        entries_today;
    uint32_t        critical_count;
    uint32_t        unacknowledged;
    uint32_t        oldest_timestamp;
    uint32_t        newest_timestamp;
    uint32_t        storage_used_pct;
} gf_slog_stats_t;

/* New entry callback */
typedef void (*gf_slog_callback_t)(const gf_slog_entry_t *entry, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize safety logger
 */
int gf_slog_init(void);

/**
 * @brief Log an event
 * @return Sequence number or error
 */
int gf_slog_log(gf_slog_category_t category, gf_slog_severity_t severity,
                uint16_t event_code, const char *equipment_tag,
                const char *message, const char *user_id,
                float value_before, float value_after,
                gf_slog_action_t action);

/**
 * @brief Log event with full entry structure
 */
int gf_slog_log_entry(const gf_slog_entry_t *entry);

/**
 * @brief Get entry by sequence number
 */
int gf_slog_get_entry(uint32_t sequence, gf_slog_entry_t *entry);

/**
 * @brief Query entries with filter
 * @param filter Query filter
 * @param entries Output array
 * @param max_entries Array size
 * @param offset Start offset for pagination
 * @return Number of entries returned
 */
int gf_slog_query(const gf_slog_filter_t *filter, gf_slog_entry_t *entries,
                  int max_entries, int offset);

/**
 * @brief Get entry count matching filter
 */
int gf_slog_count(const gf_slog_filter_t *filter);

/**
 * @brief Acknowledge an entry
 * @param sequence Entry sequence number
 * @param user_id User acknowledging
 */
int gf_slog_acknowledge(uint32_t sequence, const char *user_id);

/**
 * @brief Acknowledge multiple entries
 */
int gf_slog_acknowledge_range(uint32_t start_seq, uint32_t end_seq,
                               const char *user_id);

/**
 * @brief Get unacknowledged entries
 */
int gf_slog_get_unacknowledged(gf_slog_entry_t *entries, int max_entries);

/**
 * @brief Get statistics
 */
int gf_slog_get_stats(gf_slog_stats_t *stats);

/**
 * @brief Register callback for new entries
 */
int gf_slog_register_callback(gf_slog_callback_t callback, void *ctx);

/**
 * @brief Verify log integrity
 * @param start_seq Starting sequence number
 * @param end_seq Ending sequence number
 * @return 0 if intact, or first corrupted sequence
 */
int gf_slog_verify_integrity(uint32_t start_seq, uint32_t end_seq);

/**
 * @brief Export log to buffer (CSV format)
 * @param filter Event filter
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Bytes written or error
 */
int gf_slog_export_csv(const gf_slog_filter_t *filter, 
                       char *buffer, int buffer_size);

/**
 * @brief Archive old entries
 * @param before_timestamp Archive entries before this time
 */
int gf_slog_archive(uint32_t before_timestamp);

/**
 * @brief Set retention period
 * @param days Days to retain logs
 */
int gf_slog_set_retention(uint16_t days);

/**
 * @brief Sync log to persistent storage
 */
int gf_slog_sync(void);

/**
 * @brief Clear all logs (requires authorization)
 * @param auth_code Authorization code
 */
int gf_slog_clear(uint32_t auth_code);

#endif /* GF_SAFETY_LOGGER_H */
