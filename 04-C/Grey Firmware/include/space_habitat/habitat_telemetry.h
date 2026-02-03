/**
 * @file habitat_telemetry.h
 * @brief Telemetry Reporting for Space Habitat Health Monitoring
 * 
 * @details
 * This module provides telemetry aggregation and reporting for space habitat
 * health metrics. Supports CCSDS-compatible packet formats, real-time streaming,
 * and store-and-forward for communication blackouts.
 * 
 * INDUSTRY RELEVANCE:
 * - NASA Deep Space Network (DSN) telemetry
 * - ISS telemetry downlink systems
 * - Commercial crew mission health monitoring
 * - Lunar Gateway telemetry architecture
 * - Artemis mission support systems
 * - Ground operations monitoring consoles
 * 
 * KEY FEATURES:
 * - CCSDS Space Packet Protocol compatible
 * - Multi-resolution telemetry (real-time, summary, archival)
 * - Store-and-forward during comm blackouts
 * - Prioritized alarm forwarding
 * - Crew health integration
 * - Predictive trend analysis
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_HABITAT_TELEMETRY_H
#define GF_HABITAT_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>
#include "env_control.h"

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum telemetry packet size */
#define GF_HT_MAX_PACKET_SIZE       256

/** Maximum stored packets during blackout */
#define GF_HT_MAX_STORED_PACKETS    1024

/** Telemetry stream IDs */
#define GF_HT_STREAM_REALTIME       0x01
#define GF_HT_STREAM_SUMMARY        0x02
#define GF_HT_STREAM_ARCHIVE        0x03
#define GF_HT_STREAM_ALARM          0x04

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Telemetry status codes
 */
typedef enum {
    GF_HT_OK = 0,
    GF_HT_ERROR_NOT_INITIALIZED,
    GF_HT_ERROR_NULL_PTR,
    GF_HT_ERROR_BUFFER_FULL,
    GF_HT_ERROR_NO_COMM,
    GF_HT_ERROR_INVALID_PACKET,
    GF_HT_ERROR_QUEUE_EMPTY,
    GF_HT_WARN_BLACKOUT_ACTIVE,
    GF_HT_WARN_STORAGE_LOW
} gf_ht_status_t;

/**
 * @brief Telemetry packet priority
 */
typedef enum {
    GF_HT_PRIORITY_LOW = 0,       /**< Archival, non-critical */
    GF_HT_PRIORITY_NORMAL,        /**< Standard housekeeping */
    GF_HT_PRIORITY_HIGH,          /**< Important status updates */
    GF_HT_PRIORITY_CRITICAL       /**< Alarms, emergencies */
} gf_ht_priority_t;

/**
 * @brief Telemetry packet types
 */
typedef enum {
    GF_HT_TYPE_ATMOSPHERE,        /**< Atmospheric composition */
    GF_HT_TYPE_THERMAL,           /**< Thermal status */
    GF_HT_TYPE_POWER,             /**< Power consumption */
    GF_HT_TYPE_CREW_HEALTH,       /**< Crew health metrics */
    GF_HT_TYPE_SENSOR_STATUS,     /**< Sensor health */
    GF_HT_TYPE_ACTUATOR_STATUS,   /**< Actuator status */
    GF_HT_TYPE_ALARM,             /**< Alarm notification */
    GF_HT_TYPE_EVENT,             /**< System event */
    GF_HT_TYPE_TREND,             /**< Trend analysis */
    GF_HT_TYPE_COMMAND_RESPONSE   /**< Command acknowledgment */
} gf_ht_packet_type_t;

/**
 * @brief Telemetry configuration
 */
typedef struct {
    uint16_t spacecraft_id;           /**< Spacecraft identifier */
    uint8_t apid;                     /**< Application process ID */
    uint32_t realtime_interval_ms;    /**< Real-time packet interval */
    uint32_t summary_interval_ms;     /**< Summary packet interval */
    bool store_during_blackout;       /**< Store packets during blackout */
    uint16_t storage_capacity;        /**< Number of packets to store */
} gf_ht_config_t;

/**
 * @brief Telemetry packet header (CCSDS-like)
 */
typedef struct {
    uint16_t spacecraft_id;           /**< Spacecraft ID */
    uint8_t apid;                     /**< Application process ID */
    uint16_t sequence_count;          /**< Packet sequence number */
    uint16_t packet_length;           /**< Packet data length */
    uint64_t timestamp_ms;            /**< Mission elapsed time */
    gf_ht_packet_type_t type;         /**< Packet type */
    gf_ht_priority_t priority;        /**< Packet priority */
} gf_ht_packet_header_t;

/**
 * @brief Atmosphere telemetry packet
 */
typedef struct {
    gf_ht_packet_header_t header;     /**< Packet header */
    float o2_pct;                     /**< Oxygen percentage */
    float co2_ppm;                    /**< CO2 level */
    float n2_pct;                     /**< Nitrogen percentage */
    float pressure_kpa;               /**< Cabin pressure */
    float temperature_c;              /**< Temperature */
    float humidity_pct;               /**< Relative humidity */
    uint8_t quality_flags;            /**< Sensor quality flags */
} gf_ht_atmosphere_packet_t;

/**
 * @brief Alarm telemetry packet
 */
typedef struct {
    gf_ht_packet_header_t header;     /**< Packet header */
    uint16_t alarm_id;                /**< Unique alarm ID */
    uint8_t severity;                 /**< Alarm severity */
    uint8_t subsystem;                /**< Originating subsystem */
    char message[64];                 /**< Alarm message */
    float value;                      /**< Triggering value */
    float threshold;                  /**< Alarm threshold */
} gf_ht_alarm_packet_t;

/**
 * @brief Trend analysis packet
 */
typedef struct {
    gf_ht_packet_header_t header;     /**< Packet header */
    uint8_t parameter_id;             /**< Parameter being trended */
    float current_value;              /**< Current value */
    float rate_of_change;             /**< Rate per hour */
    float predicted_1hr;              /**< Prediction +1 hour */
    float predicted_4hr;              /**< Prediction +4 hours */
    uint8_t trend_confidence;         /**< Confidence percentage */
} gf_ht_trend_packet_t;

/**
 * @brief Telemetry statistics
 */
typedef struct {
    uint32_t packets_sent;            /**< Total packets sent */
    uint32_t packets_stored;          /**< Packets in storage */
    uint32_t packets_dropped;         /**< Packets dropped */
    uint32_t bytes_sent;              /**< Total bytes sent */
    uint32_t blackout_count;          /**< Number of blackouts */
    uint32_t blackout_duration_s;     /**< Total blackout time */
    uint64_t last_transmission_ms;    /**< Last successful TX */
} gf_ht_stats_t;

/**
 * @brief Transmission callback (implement for actual hardware)
 */
typedef bool (*gf_ht_tx_cb_t)(const uint8_t* data, uint16_t length, void* user_data);

/**
 * @brief Blackout notification callback
 */
typedef void (*gf_ht_blackout_cb_t)(bool blackout_start, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize habitat telemetry subsystem
 * @param config Telemetry configuration
 * @return Status code
 */
gf_ht_status_t gf_ht_init(const gf_ht_config_t* config);

/**
 * @brief Shutdown telemetry subsystem
 */
void gf_ht_shutdown(void);

/**
 * @brief Register transmission callback
 * @param callback TX callback function
 * @param user_data User context
 * @return Status code
 */
gf_ht_status_t gf_ht_register_tx_callback(gf_ht_tx_cb_t callback, void* user_data);

/**
 * @brief Register blackout notification callback
 * @param callback Blackout callback function
 * @param user_data User context
 * @return Status code
 */
gf_ht_status_t gf_ht_register_blackout_callback(gf_ht_blackout_cb_t callback,
                                                 void* user_data);

/**
 * @brief Send atmosphere telemetry packet
 * @param atmosphere Current atmosphere reading
 * @param priority Packet priority
 * @return Status code
 */
gf_ht_status_t gf_ht_send_atmosphere(const gf_ls_atmosphere_t* atmosphere,
                                      gf_ht_priority_t priority);

/**
 * @brief Send alarm telemetry packet
 * @param alarm_id Alarm identifier
 * @param severity Alarm severity
 * @param subsystem Originating subsystem
 * @param message Alarm message
 * @param value Triggering value
 * @param threshold Alarm threshold
 * @return Status code
 */
gf_ht_status_t gf_ht_send_alarm(uint16_t alarm_id, uint8_t severity,
                                 uint8_t subsystem, const char* message,
                                 float value, float threshold);

/**
 * @brief Send trend analysis packet
 * @param parameter_id Parameter ID
 * @param current Current value
 * @param rate Rate of change per hour
 * @param pred_1hr 1-hour prediction
 * @param pred_4hr 4-hour prediction
 * @param confidence Prediction confidence
 * @return Status code
 */
gf_ht_status_t gf_ht_send_trend(uint8_t parameter_id, float current,
                                 float rate, float pred_1hr, float pred_4hr,
                                 uint8_t confidence);

/**
 * @brief Notify communication blackout start/end
 * @param blackout_active True if blackout starting
 * @return Status code
 */
gf_ht_status_t gf_ht_set_blackout(bool blackout_active);

/**
 * @brief Flush stored packets after blackout
 * @return Status code
 */
gf_ht_status_t gf_ht_flush_stored(void);

/**
 * @brief Get telemetry statistics
 * @param stats Output statistics
 * @return Status code
 */
gf_ht_status_t gf_ht_get_stats(gf_ht_stats_t* stats);

/**
 * @brief Process telemetry (call periodically)
 * @return Status code
 */
gf_ht_status_t gf_ht_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_HABITAT_TELEMETRY_H */
