/**
 * @file launch_telemetry.h
 * @brief Launch Vehicle Telemetry Collection Module
 *
 * INDUSTRY RELEVANCE:
 * Real-time telemetry is the nervous system of any launch vehicle. This module
 * demonstrates expertise in high-bandwidth, low-latency data collection critical
 * for mission control operations. Used by commercial launch providers, NASA,
 * ESA, and defense contractors for vehicle health monitoring during flight.
 *
 * Key capabilities demonstrated:
 * - CCSDS-compatible telemetry frame formatting
 * - Prioritized downlink channel management
 * - On-board data compression and buffering
 * - S-band/X-band telemetry encoding support
 *
 * @note This is a stub header for portfolio demonstration.
 * @see docs/rocket_systems.md for spotlight implementation details.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_LAUNCH_TELEMETRY_H
#define GF_LAUNCH_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum telemetry frame size (CCSDS standard) */
#define GF_TLM_FRAME_SIZE_MAX       1115

/** Telemetry buffer depth */
#define GF_TLM_BUFFER_FRAMES        256

/** Downlink rates (bps) */
#define GF_TLM_RATE_S_BAND          2000000
#define GF_TLM_RATE_X_BAND          25000000

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Telemetry priority levels */
typedef enum {
    GF_TLM_PRIORITY_CRITICAL,       /**< Safety-critical, immediate TX */
    GF_TLM_PRIORITY_HIGH,           /**< Engine/GNC data */
    GF_TLM_PRIORITY_NORMAL,         /**< General vehicle health */
    GF_TLM_PRIORITY_LOW,            /**< Diagnostic/debug data */
    GF_TLM_PRIORITY_BEST_EFFORT     /**< Non-essential housekeeping */
} gf_tlm_priority_t;

/** Telemetry channel types */
typedef enum {
    GF_TLM_CHANNEL_REALTIME,        /**< Real-time downlink */
    GF_TLM_CHANNEL_PLAYBACK,        /**< Recorded data playback */
    GF_TLM_CHANNEL_EMERGENCY        /**< Emergency beacon */
} gf_tlm_channel_t;

/** CCSDS-format telemetry frame header */
typedef struct {
    uint16_t spacecraft_id;
    uint16_t virtual_channel;
    uint32_t frame_count;
    uint64_t mission_time_ns;
    uint8_t frame_type;
    uint8_t compression;
} gf_tlm_frame_header_t;

/** Telemetry statistics */
typedef struct {
    uint64_t frames_transmitted;
    uint64_t frames_dropped;
    uint64_t bytes_transmitted;
    uint32_t current_rate_bps;
    float buffer_utilization;
    float link_margin_db;
} gf_tlm_stats_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

/**
 * @brief Initialize telemetry subsystem
 * @param spacecraft_id Vehicle identifier
 * @return 0 on success, negative on error
 */
int gf_tlm_init(uint16_t spacecraft_id);

/**
 * @brief Queue telemetry data for transmission
 * @param data Telemetry payload
 * @param len Payload length
 * @param priority Transmission priority
 * @return 0 on success, negative on error
 */
int gf_tlm_queue(const void* data, size_t len, gf_tlm_priority_t priority);

/**
 * @brief Get current telemetry statistics
 * @param stats Output statistics structure
 * @return 0 on success, negative on error
 */
int gf_tlm_get_stats(gf_tlm_stats_t* stats);

/**
 * @brief Flush telemetry buffer
 * @return Number of frames flushed
 */
int gf_tlm_flush(void);

/**
 * @brief Shutdown telemetry subsystem
 */
void gf_tlm_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_LAUNCH_TELEMETRY_H */
