/**
 * @file exploration_telemetry.h
 * @brief Space Exploration Telemetry Collector
 * 
 * INDUSTRY RELEVANCE:
 * Deep space missions face extreme latency (4-24 minutes Mars, hours for outer
 * planets) and limited bandwidth. This collector aggregates, prioritizes, and
 * compresses exploration data for efficient transmission. Demonstrates expertise
 * in CCSDS protocols, autonomous data management, and bandwidth optimization.
 * 
 * Key applications:
 * - Mars/lunar rover data relay
 * - Deep Space Network integration
 * - Autonomous science prioritization
 * - Store-and-forward during comm blackouts
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_EXPLORATION_TELEMETRY_H
#define GF_EXPLORATION_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define EXPL_TLM_MAX_CHANNELS       64      /**< Max telemetry channels */
#define EXPL_TLM_QUEUE_DEPTH        256     /**< Packet queue depth */
#define EXPL_TLM_MAX_PACKET_SIZE    4096    /**< Max packet size (bytes) */
#define EXPL_TLM_COMPRESSION_RATIO  4       /**< Target compression ratio */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Telemetry priority levels
 */
typedef enum {
    EXPL_PRIORITY_HOUSEKEEPING = 0, /**< Routine health data */
    EXPL_PRIORITY_SCIENCE_LOW,      /**< Low priority science */
    EXPL_PRIORITY_SCIENCE_NORMAL,   /**< Normal science data */
    EXPL_PRIORITY_SCIENCE_HIGH,     /**< High priority discovery */
    EXPL_PRIORITY_EMERGENCY         /**< Critical alerts */
} expl_tlm_priority_t;

/**
 * @brief Data type identifiers
 */
typedef enum {
    EXPL_DATA_HOUSEKEEPING = 0,
    EXPL_DATA_IMAGING,
    EXPL_DATA_SPECTRAL,
    EXPL_DATA_ATMOSPHERIC,
    EXPL_DATA_SOIL_ANALYSIS,
    EXPL_DATA_NAVIGATION,
    EXPL_DATA_EVENT_LOG
} expl_data_type_t;

/**
 * @brief Telemetry packet header
 */
typedef struct {
    uint32_t sequence_number;
    uint32_t timestamp;             /**< Mission elapsed time (ms) */
    expl_data_type_t data_type;
    expl_tlm_priority_t priority;
    uint16_t payload_length;
    uint16_t compressed_length;     /**< 0 if uncompressed */
    uint32_t crc32;
} expl_tlm_header_t;

/**
 * @brief Channel statistics
 */
typedef struct {
    uint32_t packets_queued;
    uint32_t packets_transmitted;
    uint32_t packets_dropped;
    uint32_t bytes_total;
    uint32_t bytes_compressed;
    uint32_t last_tx_timestamp;
} expl_channel_stats_t;

/**
 * @brief Collector configuration
 */
typedef struct {
    uint32_t max_queue_bytes;
    bool enable_compression;
    bool enable_prioritization;
    bool store_during_blackout;
    uint16_t housekeeping_interval_ms;
    uint8_t max_retransmissions;
} expl_tlm_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize telemetry collector
 * @param config Collector configuration
 * @return 0 on success, negative on error
 */
int expl_tlm_init(const expl_tlm_config_t* config);

/**
 * @brief Shutdown collector
 * @return 0 on success, negative on error
 */
int expl_tlm_shutdown(void);

/**
 * @brief Queue data for transmission
 * @param type Data type
 * @param priority Priority level
 * @param data Data buffer
 * @param length Data length
 * @return Sequence number on success, negative on error
 */
int expl_tlm_queue(expl_data_type_t type, expl_tlm_priority_t priority,
                   const uint8_t* data, uint16_t length);

/**
 * @brief Process transmission queue
 * @param bandwidth_bps Available bandwidth (bits/sec)
 * @return Packets transmitted, negative on error
 */
int expl_tlm_process(uint32_t bandwidth_bps);

/**
 * @brief Enter communication blackout mode
 * @return 0 on success, negative on error
 */
int expl_tlm_blackout_enter(void);

/**
 * @brief Exit communication blackout mode
 * @return 0 on success, negative on error
 */
int expl_tlm_blackout_exit(void);

/**
 * @brief Get channel statistics
 * @param channel Channel ID
 * @param stats Output statistics
 * @return 0 on success, negative on error
 */
int expl_tlm_get_stats(uint8_t channel, expl_channel_stats_t* stats);

/**
 * @brief Flush all queued data (emergency)
 * @return Packets flushed
 */
int expl_tlm_flush(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EXPLORATION_TELEMETRY_H */
