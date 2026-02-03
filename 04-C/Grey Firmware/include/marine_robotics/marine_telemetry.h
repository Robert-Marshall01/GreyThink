/**
 * @file marine_telemetry.h
 * @brief Telemetry Collector for Marine Exploration Robotics
 * 
 * INDUSTRY RELEVANCE:
 * Marine exploration vehicles collect vast amounts of sensor data in
 * bandwidth-constrained acoustic environments. This module provides
 * efficient data compression, prioritization, and store-forward telemetry
 * for oceanographic research, offshore inspection, and subsea operations.
 * 
 * KEY CAPABILITIES:
 * - Real-time telemetry prioritization (navigation, science, housekeeping)
 * - Acoustic modem packet formatting (limited bandwidth: 300-9600 bps)
 * - Store-forward buffering for surface sync
 * - Data compression for high-volume science data
 * - Event-driven alerts (collision, system fault, mission waypoint)
 * - Post-mission data download optimization
 * 
 * DATA TYPES:
 * - Navigation state (position, velocity, heading)
 * - Science payload (CTD, cameras, samplers)
 * - Vehicle health (battery, thrusters, sensors)
 * - Mission progress (waypoints, timeline)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_MARINE_TELEMETRY_H
#define GF_MARINE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Telemetry priority levels */
typedef enum {
    TELEM_PRIORITY_CRITICAL,   /**< Emergency alerts */
    TELEM_PRIORITY_HIGH,       /**< Navigation updates */
    TELEM_PRIORITY_NORMAL,     /**< Science data */
    TELEM_PRIORITY_LOW         /**< Housekeeping */
} telem_priority_t;

/** Telemetry channel type */
typedef enum {
    TELEM_CHAN_ACOUSTIC,       /**< Underwater acoustic modem */
    TELEM_CHAN_SATELLITE,      /**< Satellite (when surfaced) */
    TELEM_CHAN_WIFI,           /**< WiFi (when surfaced) */
    TELEM_CHAN_STORED          /**< Store for post-mission */
} telem_channel_t;

/** Telemetry packet */
typedef struct {
    uint16_t packet_id;
    telem_priority_t priority;
    uint16_t payload_len;
    uint8_t payload[256];
    uint32_t timestamp_ms;
    bool ack_required;
} telem_packet_t;

/** Channel statistics */
typedef struct {
    uint32_t packets_sent;
    uint32_t packets_acked;
    uint32_t bytes_sent;
    uint32_t bytes_buffered;
    uint16_t avg_latency_ms;
    float link_quality;        /**< 0.0-1.0 */
} telem_stats_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize telemetry collector
 * @param vehicle_id Vehicle identifier
 * @return 0 on success, negative on error
 */
int marine_telem_init(uint32_t vehicle_id);

/**
 * @brief Queue telemetry packet for transmission
 * @param packet Packet to queue
 * @return 0 on success, negative on error
 */
int marine_telem_queue(const telem_packet_t* packet);

/**
 * @brief Set active telemetry channel
 * @param channel Channel to use
 * @return 0 on success, negative on error
 */
int marine_telem_set_channel(telem_channel_t channel);

/**
 * @brief Process telemetry queue (call periodically)
 * @param delta_ms Time since last call
 * @return Number of packets sent, negative on error
 */
int marine_telem_process(uint32_t delta_ms);

/**
 * @brief Get channel statistics
 * @param channel Channel to query
 * @param stats Output statistics
 * @return 0 on success, negative on error
 */
int marine_telem_get_stats(telem_channel_t channel, telem_stats_t* stats);

/**
 * @brief Flush stored data (post-mission download)
 * @param callback Function called for each packet
 * @return Number of packets flushed, negative on error
 */
int marine_telem_flush_stored(void (*callback)(const telem_packet_t*));

/**
 * @brief Shutdown telemetry collector
 * @return 0 on success, negative on error
 */
int marine_telem_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_MARINE_TELEMETRY_H */
