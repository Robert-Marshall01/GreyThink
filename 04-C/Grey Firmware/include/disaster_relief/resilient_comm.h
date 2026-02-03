/**
 * @file resilient_comm.h
 * @brief Resilient Disaster Communication Module
 * 
 * INDUSTRY RELEVANCE:
 * Communication infrastructure often fails during disasters. This module
 * provides multi-path resilient communications using mesh networking, satellite
 * fallback, and store-and-forward. Essential for first responder coordination
 * when terrestrial networks are degraded or destroyed.
 * 
 * Key applications:
 * - FirstNet/LMR interoperability
 * - Satellite emergency communication
 * - Ad-hoc mesh networking
 * - Emergency alert broadcasting
 * - Interagency coordination
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_RESILIENT_COMM_H
#define GF_RESILIENT_COMM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define RCOMM_MAX_NODES             64      /**< Max mesh nodes */
#define RCOMM_MAX_PATHS             4       /**< Max simultaneous paths */
#define RCOMM_MTU                   256     /**< Max transmission unit */
#define RCOMM_STORE_DEPTH           128     /**< Store-forward queue */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Communication path type
 */
typedef enum {
    RCOMM_PATH_MESH = 0,            /**< RF mesh network */
    RCOMM_PATH_CELLULAR,            /**< Cellular (LTE/5G) */
    RCOMM_PATH_SATELLITE,           /**< Satellite link */
    RCOMM_PATH_HF_RADIO,            /**< HF radio */
    RCOMM_PATH_OFFLINE              /**< No connectivity */
} rcomm_path_t;

/**
 * @brief Message priority
 */
typedef enum {
    RCOMM_PRIORITY_ROUTINE = 0,
    RCOMM_PRIORITY_PRIORITY,
    RCOMM_PRIORITY_IMMEDIATE,
    RCOMM_PRIORITY_FLASH,
    RCOMM_PRIORITY_FLASH_OVERRIDE
} rcomm_priority_t;

/**
 * @brief Node status
 */
typedef struct {
    uint16_t node_id;
    rcomm_path_t primary_path;
    rcomm_path_t backup_path;
    int8_t rssi;                    /**< Signal strength (dBm) */
    uint8_t hop_count;
    bool reachable;
    uint32_t last_seen;
} rcomm_node_t;

/**
 * @brief Message structure
 */
typedef struct {
    uint32_t msg_id;
    uint16_t source;
    uint16_t destination;           /**< 0xFFFF = broadcast */
    rcomm_priority_t priority;
    uint8_t payload[RCOMM_MTU];
    uint16_t payload_len;
    uint32_t timestamp;
    uint8_t ttl;                    /**< Time to live (hops) */
    bool ack_required;
} rcomm_message_t;

/**
 * @brief Path status
 */
typedef struct {
    rcomm_path_t path;
    bool available;
    float bandwidth_bps;
    float latency_ms;
    float packet_loss;
    int8_t signal_strength;
} rcomm_path_status_t;

/**
 * @brief Communication statistics
 */
typedef struct {
    uint32_t messages_sent;
    uint32_t messages_received;
    uint32_t messages_relayed;
    uint32_t messages_stored;
    uint32_t acks_received;
    uint32_t retransmissions;
    uint32_t path_switches;
} rcomm_stats_t;

/**
 * @brief Configuration
 */
typedef struct {
    uint16_t node_id;               /**< This node's ID */
    bool enable_mesh;
    bool enable_satellite;
    bool enable_store_forward;
    uint8_t max_retries;
    uint16_t ack_timeout_ms;
} rcomm_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize resilient communication
 * @param config Communication configuration
 * @return 0 on success, negative on error
 */
int rcomm_init(const rcomm_config_t* config);

/**
 * @brief Shutdown communication
 * @return 0 on success, negative on error
 */
int rcomm_shutdown(void);

/**
 * @brief Send message
 * @param msg Message to send
 * @return Message ID on success, negative on error
 */
int rcomm_send(const rcomm_message_t* msg);

/**
 * @brief Receive message (blocking)
 * @param msg Output message
 * @param timeout_ms Timeout in milliseconds
 * @return 0 on success, negative on error/timeout
 */
int rcomm_receive(rcomm_message_t* msg, uint32_t timeout_ms);

/**
 * @brief Broadcast message
 * @param payload Payload data
 * @param len Payload length
 * @param priority Message priority
 * @return Message ID on success, negative on error
 */
int rcomm_broadcast(const uint8_t* payload, uint16_t len,
                    rcomm_priority_t priority);

/**
 * @brief Get path status
 * @param path Path type
 * @param status Output status
 * @return 0 on success, negative on error
 */
int rcomm_get_path_status(rcomm_path_t path, rcomm_path_status_t* status);

/**
 * @brief Get node status
 * @param node_id Node ID
 * @param node Output node status
 * @return 0 on success, negative on error
 */
int rcomm_get_node(uint16_t node_id, rcomm_node_t* node);

/**
 * @brief Force path switch
 * @param path Path to use
 * @return 0 on success, negative on error
 */
int rcomm_force_path(rcomm_path_t path);

/**
 * @brief Get statistics
 * @param stats Output statistics
 * @return 0 on success, negative on error
 */
int rcomm_get_stats(rcomm_stats_t* stats);

/**
 * @brief Process communication (call periodically)
 * @return 0 on success, negative on error
 */
int rcomm_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_RESILIENT_COMM_H */
