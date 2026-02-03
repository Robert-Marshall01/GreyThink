/**
 * @file fault_tolerant_comm.h
 * @brief Fault-Tolerant Communication Module for Space Systems
 * 
 * INDUSTRY RELEVANCE:
 * Space communication systems must operate reliably despite radiation-induced
 * errors, link dropouts, and extreme latency. This module demonstrates:
 * - Forward Error Correction (FEC) with Reed-Solomon and LDPC codes
 * - Automatic repeat request (ARQ) with selective acknowledgment
 * - Multi-path redundancy across S-band, X-band, and UHF links
 * - Graceful degradation under partial system failure
 * 
 * Applications: Deep space missions, GEO satellites, lunar gateways
 * Standards: CCSDS 131.0-B (TM Sync), CCSDS 231.0-B (TC Sync)
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_FAULT_TOLERANT_COMM_H
#define GF_FAULT_TOLERANT_COMM_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Communication Types
 ******************************************************************************/

/** Communication channel types */
typedef enum {
    GF_COMM_CHAN_PRIMARY,      /**< Primary link (highest bandwidth) */
    GF_COMM_CHAN_SECONDARY,    /**< Backup link */
    GF_COMM_CHAN_EMERGENCY,    /**< Low-rate emergency beacon */
    GF_COMM_CHAN_CROSSLINK     /**< Inter-satellite link */
} gf_comm_channel_t;

/** Forward error correction schemes */
typedef enum {
    GF_FEC_NONE,
    GF_FEC_REED_SOLOMON,       /**< RS(255,223) - robust */
    GF_FEC_LDPC,               /**< Low-Density Parity Check */
    GF_FEC_TURBO,              /**< Turbo codes - near Shannon limit */
    GF_FEC_CONVOLUTIONAL       /**< Viterbi-decoded convolutional */
} gf_fec_scheme_t;

/** Channel health status */
typedef enum {
    GF_CHAN_HEALTH_GOOD,       /**< BER < 10^-6 */
    GF_CHAN_HEALTH_DEGRADED,   /**< BER < 10^-4 */
    GF_CHAN_HEALTH_POOR,       /**< BER < 10^-2 */
    GF_CHAN_HEALTH_FAILED      /**< Link unusable */
} gf_channel_health_t;

/*******************************************************************************
 * Communication Configuration
 ******************************************************************************/

/** Channel configuration */
typedef struct {
    gf_comm_channel_t channel;
    uint32_t frequency_hz;            /**< Carrier frequency */
    uint32_t bandwidth_hz;            /**< Channel bandwidth */
    uint32_t data_rate_bps;           /**< Data rate */
    gf_fec_scheme_t fec;
    uint8_t fec_interleave_depth;     /**< Interleaving for burst errors */
    int8_t min_snr_db;                /**< Minimum operating SNR */
    bool enable_arq;                  /**< Automatic repeat request */
    uint8_t max_retries;
    uint32_t timeout_ms;
} gf_comm_channel_config_t;

/** Fault-tolerant comm configuration */
typedef struct {
    gf_comm_channel_config_t channels[4];
    uint8_t channel_count;
    bool enable_multipath;            /**< Simultaneous multi-channel TX */
    bool enable_voting;               /**< Vote across redundant channels */
    uint8_t quorum_size;              /**< Minimum agreeing channels */
    uint32_t switchover_threshold_ber;/**< BER to trigger failover */
} gf_ftcomm_config_t;

/** Communication frame */
typedef struct {
    uint32_t sequence_number;
    uint8_t channel_id;
    uint8_t fec_overhead[32];         /**< FEC parity bytes */
    uint8_t payload[224];             /**< Data payload */
    uint16_t payload_len;
    uint32_t checksum;
    uint32_t transmit_time;
    bool ack_required;
} gf_comm_frame_t;

/*******************************************************************************
 * Communication Statistics
 ******************************************************************************/

typedef struct {
    uint32_t frames_transmitted;
    uint32_t frames_received;
    uint32_t frames_corrected;        /**< FEC corrections */
    uint32_t frames_retransmitted;
    uint32_t frames_dropped;
    uint32_t channel_switchovers;
    uint32_t bit_errors_detected;
    uint32_t bit_errors_corrected;
    gf_channel_health_t channel_health[4];
} gf_ftcomm_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize fault-tolerant communication system
 * @param config System configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_ftcomm_init(const gf_ftcomm_config_t *config);

/**
 * @brief Transmit frame with fault tolerance
 * @param frame Frame to transmit
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_ftcomm_transmit(const gf_comm_frame_t *frame);

/**
 * @brief Receive frame with FEC decoding
 * @param frame Output frame
 * @param timeout_ms Receive timeout
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_ftcomm_receive(gf_comm_frame_t *frame, uint32_t timeout_ms);

/**
 * @brief Force channel switchover
 * @param new_channel Channel to switch to
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_ftcomm_switchover(gf_comm_channel_t new_channel);

/**
 * @brief Get current active channel
 * @return Active channel ID
 */
gf_comm_channel_t gf_ftcomm_get_active_channel(void);

/**
 * @brief Get channel health status
 * @param channel Channel to query
 * @return Channel health
 */
gf_channel_health_t gf_ftcomm_get_channel_health(gf_comm_channel_t channel);

/**
 * @brief Run channel diagnostics
 * @param channel Channel to test
 * @return Measured BER
 */
uint32_t gf_ftcomm_diagnose_channel(gf_comm_channel_t channel);

/**
 * @brief Get communication statistics
 * @return Current statistics
 */
gf_ftcomm_stats_t gf_ftcomm_get_stats(void);

/**
 * @brief Shutdown communication system
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_ftcomm_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FAULT_TOLERANT_COMM_H */
