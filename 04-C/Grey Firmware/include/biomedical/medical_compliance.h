/**
 * @file medical_compliance.h
 * @brief FDA/Medical Standards Compliance Logging Stub
 * 
 * INDUSTRY RELEVANCE:
 * Medical device firmware must meet rigorous regulatory requirements including
 * FDA 21 CFR Part 11 (electronic records), IEC 62304 (software lifecycle),
 * and ISO 14971 (risk management). Compliance logging provides audit trails
 * for device actions, data integrity verification, and traceability required
 * for market clearance. This is mandatory for any software classified as
 * SaMD (Software as a Medical Device).
 * 
 * Key challenges:
 * - Tamper-evident logging with cryptographic signatures
 * - Precise timestamping and event ordering
 * - Secure storage with redundancy
 * - Export formats for regulatory audits
 * - User access control and authentication logging
 */

#ifndef GF_MEDICAL_COMPLIANCE_H
#define GF_MEDICAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Compliance log status codes */
typedef enum {
    GF_COMPLY_OK = 0,
    GF_COMPLY_STORAGE_FULL,         /* Log storage exhausted */
    GF_COMPLY_INTEGRITY_ERROR,      /* Log tampering detected */
    GF_COMPLY_TIMESTAMP_ERROR,      /* RTC/timestamp failure */
    GF_COMPLY_SIGNATURE_ERROR,      /* Cryptographic signature invalid */
    GF_COMPLY_EXPORT_ERROR,         /* Export to external media failed */
    GF_COMPLY_ACCESS_DENIED         /* Insufficient privileges */
} gf_comply_status_t;

/* Event severity levels per IEC 62304 */
typedef enum {
    GF_SEVERITY_INFO,               /* Informational event */
    GF_SEVERITY_WARNING,            /* Warning - may require attention */
    GF_SEVERITY_ERROR,              /* Error - device function impaired */
    GF_SEVERITY_CRITICAL,           /* Critical - immediate action required */
    GF_SEVERITY_HAZARD              /* Hazardous situation detected */
} gf_comply_severity_t;

/* Event categories for audit trail */
typedef enum {
    GF_EVENT_SYSTEM_STARTUP,        /* Device power on */
    GF_EVENT_SYSTEM_SHUTDOWN,       /* Device power off */
    GF_EVENT_USER_LOGIN,            /* User authentication */
    GF_EVENT_USER_LOGOUT,           /* User session end */
    GF_EVENT_CONFIG_CHANGE,         /* Settings modification */
    GF_EVENT_CALIBRATION,           /* Device calibration */
    GF_EVENT_MEASUREMENT,           /* Clinical measurement taken */
    GF_EVENT_ALARM,                 /* Alarm condition */
    GF_EVENT_ALARM_ACK,             /* Alarm acknowledged */
    GF_EVENT_DATA_EXPORT,           /* Data exported */
    GF_EVENT_FIRMWARE_UPDATE,       /* Software update */
    GF_EVENT_SELF_TEST,             /* Diagnostic test */
    GF_EVENT_ERROR,                 /* System error */
    GF_EVENT_MAINTENANCE            /* Maintenance action */
} gf_comply_event_t;

/* User role levels for access control */
typedef enum {
    GF_ROLE_PATIENT,                /* End user/patient */
    GF_ROLE_CLINICIAN,              /* Healthcare provider */
    GF_ROLE_TECHNICIAN,             /* Service technician */
    GF_ROLE_ADMIN,                  /* Administrator */
    GF_ROLE_MANUFACTURER            /* Factory access */
} gf_comply_role_t;

/* Compliance log configuration */
typedef struct {
    uint32_t max_entries;           /* Maximum log entries */
    bool enable_signatures;         /* Cryptographic signing */
    bool enable_encryption;         /* Log encryption at rest */
    bool circular_buffer;           /* Overwrite oldest when full */
    uint16_t export_batch_size;     /* Entries per export batch */
    uint8_t retention_days;         /* Minimum retention period */
} gf_comply_config_t;

/* Single log entry structure */
typedef struct {
    uint32_t sequence_number;       /* Monotonic sequence */
    uint64_t timestamp_ms;          /* Millisecond timestamp */
    gf_comply_event_t event_type;   /* Event category */
    gf_comply_severity_t severity;  /* Severity level */
    gf_comply_role_t user_role;     /* Acting user role */
    uint32_t user_id;               /* User identifier */
    char description[128];          /* Event description */
    uint8_t hash[32];               /* SHA-256 integrity hash */
} gf_comply_entry_t;

/* Audit statistics */
typedef struct {
    uint32_t total_entries;         /* Total logged events */
    uint32_t available_space;       /* Remaining capacity */
    uint32_t integrity_errors;      /* Detected tampering attempts */
    uint64_t oldest_entry_time;     /* Oldest record timestamp */
    uint64_t newest_entry_time;     /* Newest record timestamp */
    bool storage_warning;           /* Low storage alert */
} gf_comply_stats_t;

/**
 * @brief Initialize compliance logging subsystem
 * @param config Logging configuration
 * @return Status code
 */
gf_comply_status_t gf_comply_init(const gf_comply_config_t* config);

/**
 * @brief Log a compliance event
 * @param event Event type
 * @param severity Event severity
 * @param user_role Role of acting user
 * @param user_id User identifier
 * @param description Event description
 * @return Status code
 */
gf_comply_status_t gf_comply_log(
    gf_comply_event_t event,
    gf_comply_severity_t severity,
    gf_comply_role_t user_role,
    uint32_t user_id,
    const char* description
);

/**
 * @brief Verify log integrity (detect tampering)
 * @param errors_found Output for number of integrity errors
 * @return Status code
 */
gf_comply_status_t gf_comply_verify_integrity(uint32_t* errors_found);

/**
 * @brief Export logs to external format (JSON, CSV, HL7)
 * @param buffer Output buffer for exported data
 * @param buffer_size Buffer size in bytes
 * @param bytes_written Actual bytes written
 * @param start_sequence Starting sequence number
 * @param count Number of entries to export
 * @return Status code
 */
gf_comply_status_t gf_comply_export(
    uint8_t* buffer,
    size_t buffer_size,
    size_t* bytes_written,
    uint32_t start_sequence,
    uint32_t count
);

/**
 * @brief Get logging statistics
 * @param stats Output structure for statistics
 * @return Status code
 */
gf_comply_status_t gf_comply_get_stats(gf_comply_stats_t* stats);

/**
 * @brief Retrieve specific log entry
 * @param sequence Sequence number to retrieve
 * @param entry Output structure for entry
 * @return Status code
 */
gf_comply_status_t gf_comply_get_entry(uint32_t sequence, gf_comply_entry_t* entry);

/**
 * @brief Shutdown and flush pending writes
 */
void gf_comply_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_MEDICAL_COMPLIANCE_H */
