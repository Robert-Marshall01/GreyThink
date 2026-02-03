/**
 * @file orbital_telemetry.h
 * @brief Orbital Telemetry System for Satellite Communications
 * 
 * INDUSTRY RELEVANCE:
 * Spacecraft telemetry systems are critical for mission operations, requiring
 * reliable downlink of housekeeping data, science payloads, and health metrics.
 * This module demonstrates:
 * - CCSDS (Consultative Committee for Space Data Systems) packet formatting
 * - Store-and-forward for orbital blackout periods
 * - Doppler compensation and link budget management
 * - Priority-based telemetry queue with bandwidth constraints
 * 
 * Applications: Earth observation satellites, deep space probes, ISS payloads
 * Standards: CCSDS 133.0-B, ECSS-E-70C, IRIG-106
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_ORBITAL_TELEMETRY_H
#define GF_ORBITAL_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Telemetry Types
 ******************************************************************************/

/** Telemetry priority levels (CCSDS-compliant) */
typedef enum {
    GF_TM_PRIORITY_REALTIME,   /**< Immediate downlink required */
    GF_TM_PRIORITY_HIGH,       /**< Next available contact window */
    GF_TM_PRIORITY_NORMAL,     /**< Standard science data */
    GF_TM_PRIORITY_LOW,        /**< Background housekeeping */
    GF_TM_PRIORITY_BULK        /**< Bulk data transfer */
} gf_tm_priority_t;

/** Telemetry packet types */
typedef enum {
    GF_TM_TYPE_HOUSEKEEPING,   /**< System health and status */
    GF_TM_TYPE_SCIENCE,        /**< Science payload data */
    GF_TM_TYPE_EVENT,          /**< Anomaly or event notification */
    GF_TM_TYPE_MEMORY_DUMP,    /**< Memory readout for debugging */
    GF_TM_TYPE_COMMAND_ECHO    /**< Command acknowledgment */
} gf_tm_type_t;

/** Link status */
typedef enum {
    GF_LINK_DISCONNECTED,
    GF_LINK_ACQUIRING,         /**< Finding ground station */
    GF_LINK_CONNECTED,         /**< Active downlink */
    GF_LINK_BLACKOUT           /**< No contact possible (e.g., eclipse) */
} gf_link_status_t;

/*******************************************************************************
 * Telemetry Configuration
 ******************************************************************************/

/** Orbital telemetry configuration */
typedef struct {
    uint16_t spacecraft_id;           /**< SCID for CCSDS packets */
    uint16_t virtual_channel_id;      /**< VCID */
    uint32_t downlink_rate_bps;       /**< Max downlink bitrate */
    uint32_t buffer_size_bytes;       /**< Onboard storage buffer */
    uint16_t packet_size_max;         /**< Max TM packet size */
    bool enable_store_forward;        /**< Buffer during blackouts */
    bool enable_compression;          /**< Compress before downlink */
    uint8_t compression_level;        /**< 0-9 compression ratio */
} gf_orbital_tm_config_t;

/** CCSDS-style telemetry packet header */
typedef struct {
    uint16_t version_type_sec_apid;   /**< Packet ID field */
    uint16_t seq_flags_count;         /**< Packet sequence control */
    uint16_t data_length;             /**< Packet data length - 1 */
    uint32_t timestamp;               /**< Mission elapsed time */
} gf_ccsds_tm_header_t;

/** Telemetry packet */
typedef struct {
    gf_ccsds_tm_header_t header;
    gf_tm_type_t type;
    gf_tm_priority_t priority;
    uint8_t data[256];
    uint16_t data_len;
    uint32_t queue_time;              /**< Time spent in queue */
} gf_tm_packet_t;

/** Contact window definition */
typedef struct {
    uint32_t aos_time;                /**< Acquisition of Signal */
    uint32_t los_time;                /**< Loss of Signal */
    uint32_t max_elevation_deg;       /**< Peak elevation angle */
    uint32_t data_volume_bytes;       /**< Expected throughput */
    char ground_station[16];          /**< Ground station ID */
} gf_contact_window_t;

/*******************************************************************************
 * Telemetry Statistics
 ******************************************************************************/

typedef struct {
    uint32_t packets_queued;
    uint32_t packets_transmitted;
    uint32_t packets_dropped;
    uint32_t bytes_transmitted;
    uint32_t bytes_compressed;
    uint32_t blackout_duration_sec;
    uint32_t contacts_completed;
    uint32_t store_forward_used;
} gf_orbital_tm_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize orbital telemetry system
 * @param config Telemetry configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_orbital_tm_init(const gf_orbital_tm_config_t *config);

/**
 * @brief Queue telemetry packet for downlink
 * @param packet Packet to queue
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_orbital_tm_queue(const gf_tm_packet_t *packet);

/**
 * @brief Process telemetry queue (call periodically)
 * @return Number of packets transmitted
 */
uint32_t gf_orbital_tm_process(void);

/**
 * @brief Set predicted contact window
 * @param window Contact window parameters
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_orbital_tm_set_contact(const gf_contact_window_t *window);

/**
 * @brief Get current link status
 * @return Link status
 */
gf_link_status_t gf_orbital_tm_link_status(void);

/**
 * @brief Enter blackout mode (store all telemetry)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_orbital_tm_enter_blackout(void);

/**
 * @brief Exit blackout mode (flush stored telemetry)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_orbital_tm_exit_blackout(void);

/**
 * @brief Get telemetry statistics
 * @return Current statistics
 */
gf_orbital_tm_stats_t gf_orbital_tm_get_stats(void);

/**
 * @brief Shutdown telemetry system
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_orbital_tm_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ORBITAL_TELEMETRY_H */
