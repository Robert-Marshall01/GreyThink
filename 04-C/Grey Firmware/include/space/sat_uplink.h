/**
 * @file sat_uplink.h
 * @brief Satellite Uplink/Downlink Communications Stub - Edge Space Communications
 * 
 * @details Industry Relevance:
 * Low Earth Orbit (LEO) satellite constellations (Starlink, OneWeb, Iridium)
 * are transforming remote connectivity. Embedded satellite terminals require:
 * - Efficient framing protocols for limited bandwidth
 * - Doppler compensation for LEO passes
 * - Store-and-forward for non-continuous coverage
 * - Power-efficient transmission scheduling
 * 
 * Applications: Maritime IoT, remote asset monitoring, disaster response,
 * polar region communications, agricultural telemetry from remote fields.
 * 
 * Protocols: DVB-S2X, CCSDS proximity-1, proprietary LEO protocols.
 * Frequencies: L-band (Iridium), Ku/Ka-band (LEO broadband).
 * 
 * This module provides the SPOTLIGHT implementation for Phase 16.
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_SPACE_SAT_UPLINK_H
#define GF_SPACE_SAT_UPLINK_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum packet size (bytes) */
#define GF_SAT_MAX_PACKET_SIZE          512

/** Maximum queue depth for store-and-forward */
#define GF_SAT_MAX_QUEUE_DEPTH          64

/** Link timeout (milliseconds) */
#define GF_SAT_LINK_TIMEOUT_MS          30000

/** Maximum retransmission attempts */
#define GF_SAT_MAX_RETRIES              5

/** Telemetry frame sync word */
#define GF_SAT_SYNC_WORD                0x1ACFFC1D

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Satellite link states
 */
typedef enum {
    GF_SAT_STATE_IDLE,              /**< No active link */
    GF_SAT_STATE_ACQUIRING,         /**< Acquiring satellite */
    GF_SAT_STATE_SYNC,              /**< Synchronizing */
    GF_SAT_STATE_CONNECTED,         /**< Active bidirectional link */
    GF_SAT_STATE_TX_ONLY,           /**< Uplink only (no response) */
    GF_SAT_STATE_STORE_FORWARD,     /**< Storing for later transmission */
    GF_SAT_STATE_ERROR              /**< Link error state */
} gf_sat_state_t;

/**
 * @brief Packet priority levels
 */
typedef enum {
    GF_SAT_PRIORITY_LOW,            /**< Routine telemetry */
    GF_SAT_PRIORITY_NORMAL,         /**< Standard data */
    GF_SAT_PRIORITY_HIGH,           /**< Time-sensitive data */
    GF_SAT_PRIORITY_EMERGENCY       /**< Emergency/distress */
} gf_sat_priority_t;

/**
 * @brief Uplink/downlink packet structure
 */
typedef struct {
    uint32_t sync_word;             /**< Frame synchronization */
    uint16_t sequence_num;          /**< Sequence number */
    uint16_t payload_len;           /**< Payload length */
    uint8_t packet_type;            /**< Packet type ID */
    gf_sat_priority_t priority;     /**< Transmission priority */
    uint8_t payload[GF_SAT_MAX_PACKET_SIZE];
    uint32_t crc32;                 /**< CRC-32 checksum */
    uint8_t fec_parity[64];         /**< FEC parity bytes */
} gf_sat_packet_t;

/**
 * @brief Satellite link status
 */
typedef struct {
    gf_sat_state_t state;           /**< Current link state */
    int8_t rssi_dbm;                /**< Signal strength */
    float snr_db;                   /**< Signal-to-noise ratio */
    float doppler_hz;               /**< Doppler shift */
    uint32_t packets_tx;            /**< Packets transmitted */
    uint32_t packets_rx;            /**< Packets received */
    uint32_t retransmits;           /**< Retransmit count */
    uint32_t errors;                /**< Error count */
    uint16_t queue_depth;           /**< Pending TX packets */
    uint32_t last_contact_ms;       /**< Time since last contact */
    bool encryption_enabled;        /**< Secure channel active */
} gf_sat_status_t;

/**
 * @brief Satellite pass prediction
 */
typedef struct {
    uint32_t aos_time;              /**< Acquisition of signal (Unix time) */
    uint32_t los_time;              /**< Loss of signal */
    float max_elevation_deg;        /**< Maximum elevation angle */
    float azimuth_aos_deg;          /**< Azimuth at AOS */
    float azimuth_los_deg;          /**< Azimuth at LOS */
    uint8_t satellite_id;           /**< Satellite identifier */
} gf_sat_pass_t;

/**
 * @brief Callback for received packets
 */
typedef void (*gf_sat_rx_callback_t)(const gf_sat_packet_t* packet);

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize satellite communication system
 * @return 0 on success, negative error code on failure
 */
int gf_sat_init(void);

/**
 * @brief Shutdown satellite system
 * @return 0 on success
 */
int gf_sat_shutdown(void);

/**
 * @brief Process satellite link (call periodically)
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int gf_sat_process(uint32_t delta_ms);

/**
 * @brief Queue packet for transmission
 * @param packet Packet to transmit
 * @return 0 on success, -1 if queue full
 */
int gf_sat_transmit(const gf_sat_packet_t* packet);

/**
 * @brief Queue telemetry payload for uplink
 * @param data Payload data
 * @param len Payload length
 * @param priority Transmission priority
 * @return 0 on success
 */
int gf_sat_queue_telemetry(const uint8_t* data, uint16_t len,
                           gf_sat_priority_t priority);

/**
 * @brief Register receive callback
 * @param callback Function to call on packet receive
 * @return 0 on success
 */
int gf_sat_register_rx_callback(gf_sat_rx_callback_t callback);

/**
 * @brief Get current link status
 * @param status Output status structure
 * @return 0 on success
 */
int gf_sat_get_status(gf_sat_status_t* status);

/**
 * @brief Get next satellite pass prediction
 * @param pass Output pass prediction
 * @return 0 if pass available, -1 if none predicted
 */
int gf_sat_get_next_pass(gf_sat_pass_t* pass);

/**
 * @brief Force store-and-forward mode
 * @param enable True to enable, false for normal operation
 * @return 0 on success
 */
int gf_sat_set_store_forward(bool enable);

/**
 * @brief Apply Doppler compensation
 * @param doppler_hz Doppler shift in Hz
 * @return 0 on success
 */
int gf_sat_set_doppler(float doppler_hz);

#ifdef __cplusplus
}
#endif

#endif /* GF_SPACE_SAT_UPLINK_H */
