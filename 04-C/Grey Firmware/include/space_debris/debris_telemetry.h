/**
 * @file debris_telemetry.h
 * @brief Space Debris Event Telemetry Interface
 * 
 * INDUSTRY RELEVANCE:
 * Space situational awareness (SSA) data sharing is critical for
 * collision avoidance coordination. The Space Data Association (SDA),
 * 18th Space Defense Squadron, and commercial providers share
 * conjunction data messages (CDMs) to enable coordinated responses.
 * 
 * This module provides:
 * - CCSDS-compliant telemetry formatting
 * - Real-time debris event streaming
 * - Conjunction data message generation
 * - Historical event logging
 * 
 * STANDARDS:
 * - CCSDS 502.0-B-2 (Orbit Data Messages)
 * - CCSDS 508.0-B-1 (Conjunction Data Message)
 * - NASA/GSFC Flight Dynamics CDM format
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_DEBRIS_TELEMETRY_H
#define GF_DEBRIS_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration
 * ============================================================================ */

#define DEBRIS_TELEM_MAX_SIZE     4096   /**< Maximum telemetry packet */
#define DEBRIS_EVENT_LOG_SIZE     1024   /**< Event log entries */
#define DEBRIS_CDM_VERSION        1      /**< CDM format version */

/* ============================================================================
 * Types
 * ============================================================================ */

/** Telemetry packet type */
typedef enum {
    DEBRIS_PKT_STATUS,         /**< System status */
    DEBRIS_PKT_DETECTION,      /**< New detection */
    DEBRIS_PKT_CONJUNCTION,    /**< Conjunction warning */
    DEBRIS_PKT_CDM,            /**< Full Conjunction Data Message */
    DEBRIS_PKT_MANEUVER,       /**< Maneuver notification */
    DEBRIS_PKT_CATALOG_UPDATE  /**< Catalog update */
} debris_pkt_type_t;

/** Event severity for logging */
typedef enum {
    DEBRIS_EVENT_INFO,
    DEBRIS_EVENT_WATCH,
    DEBRIS_EVENT_WARNING,
    DEBRIS_EVENT_ALERT,
    DEBRIS_EVENT_EMERGENCY
} debris_event_severity_t;

/** Telemetry generation configuration */
typedef struct {
    uint16_t status_interval_s;
    uint16_t detection_batch_size;
    bool cdm_auto_generation;
    bool compressed_format;
    uint8_t priority_level;
} debris_telem_config_t;

/** Debris event log entry */
typedef struct {
    uint64_t timestamp_us;
    debris_event_severity_t severity;
    debris_pkt_type_t type;
    uint32_t object_id;
    float miss_distance_km;
    float collision_probability;
    char description[64];
} debris_event_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize telemetry subsystem
 * @return 0 on success
 */
int debris_telem_init(void);

/**
 * @brief Configure telemetry parameters
 * @param config Configuration
 * @return 0 on success
 */
int debris_telem_configure(const debris_telem_config_t *config);

/**
 * @brief Generate telemetry packet
 * @param type Packet type
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Packet length, negative on error
 */
int debris_telem_generate(debris_pkt_type_t type, uint8_t *buffer, 
                          size_t max_len);

/**
 * @brief Log debris event
 * @param severity Event severity
 * @param type Event type
 * @param object_id Related object
 * @param description Event description
 * @return 0 on success
 */
int debris_telem_log_event(debris_event_severity_t severity,
                           debris_pkt_type_t type,
                           uint32_t object_id,
                           const char *description);

/**
 * @brief Get event log
 * @param events Output buffer
 * @param max_count Maximum events
 * @return Number of events
 */
int debris_telem_get_events(debris_event_t *events, uint16_t max_count);

#ifdef __cplusplus
}
#endif

#endif /* GF_DEBRIS_TELEMETRY_H */
