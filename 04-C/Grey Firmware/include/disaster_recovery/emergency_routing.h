/**
 * @file emergency_routing.h
 * @brief Emergency Routing Module for Disaster Recovery Networks
 * 
 * @details
 * Priority-based routing for emergency communications during disaster
 * scenarios. Implements traffic prioritization, alternate path selection,
 * and store-and-forward for intermittent connectivity.
 * 
 * INDUSTRY RELEVANCE:
 * - Emergency management systems (FEMA)
 * - Public safety networks
 * - Military tactical routing
 * - Satellite communication fallback
 * - Delay-tolerant networking (DTN)
 * 
 * KEY FEATURES:
 * - Priority-based forwarding
 * - Multi-path routing
 * - Store-and-forward (DTN)
 * - Congestion avoidance
 * - Deadline-aware scheduling
 * - Gateway failover
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_EMERGENCY_ROUTING_H
#define GF_EMERGENCY_ROUTING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum queued messages */
#define GF_EROUTE_MAX_QUEUE         128

/** Maximum gateways */
#define GF_EROUTE_MAX_GATEWAYS      8

/** Maximum alternate paths */
#define GF_EROUTE_MAX_PATHS         4

/** Default message TTL (seconds) */
#define GF_EROUTE_DEFAULT_TTL_S     3600

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Routing status codes
 */
typedef enum {
    GF_EROUTE_OK = 0,
    GF_EROUTE_ERROR_NOT_INIT,
    GF_EROUTE_ERROR_NULL_PTR,
    GF_EROUTE_ERROR_QUEUE_FULL,
    GF_EROUTE_ERROR_NO_PATH,
    GF_EROUTE_ERROR_EXPIRED,
    GF_EROUTE_ERROR_GATEWAY_DOWN,
    GF_EROUTE_WARN_CONGESTED,
    GF_EROUTE_WARN_DELAYED
} gf_eroute_status_t;

/**
 * @brief Message priority levels
 */
typedef enum {
    GF_EROUTE_PRIO_ROUTINE = 0,   /**< Normal traffic */
    GF_EROUTE_PRIO_PRIORITY,      /**< Priority traffic */
    GF_EROUTE_PRIO_IMMEDIATE,     /**< Immediate delivery */
    GF_EROUTE_PRIO_FLASH,         /**< Flash priority */
    GF_EROUTE_PRIO_OVERRIDE       /**< Emergency override */
} gf_eroute_priority_t;

/**
 * @brief Path status
 */
typedef enum {
    GF_EROUTE_PATH_UP,            /**< Path operational */
    GF_EROUTE_PATH_DEGRADED,      /**< Path degraded */
    GF_EROUTE_PATH_DOWN,          /**< Path down */
    GF_EROUTE_PATH_CONGESTED      /**< Path congested */
} gf_eroute_path_status_t;

/**
 * @brief Gateway type
 */
typedef enum {
    GF_EROUTE_GW_MESH,            /**< Mesh network */
    GF_EROUTE_GW_CELLULAR,        /**< Cellular network */
    GF_EROUTE_GW_SATELLITE,       /**< Satellite link */
    GF_EROUTE_GW_HF_RADIO,        /**< HF radio */
    GF_EROUTE_GW_WIFI             /**< WiFi backhaul */
} gf_eroute_gw_type_t;

/**
 * @brief Routing message
 */
typedef struct {
    uint32_t msg_id;              /**< Message identifier */
    uint8_t destination[16];      /**< Destination address */
    gf_eroute_priority_t priority; /**< Message priority */
    uint32_t deadline_ms;         /**< Delivery deadline */
    uint32_t created_ms;          /**< Creation timestamp */
    bool requires_ack;            /**< Acknowledgement required */
    uint8_t* payload;             /**< Message payload */
    uint16_t payload_len;         /**< Payload length */
    uint8_t retry_count;          /**< Retry attempts */
} gf_eroute_msg_t;

/**
 * @brief Gateway info
 */
typedef struct {
    uint8_t gw_id;                /**< Gateway ID */
    gf_eroute_gw_type_t type;     /**< Gateway type */
    gf_eroute_path_status_t status; /**< Current status */
    uint16_t latency_ms;          /**< Average latency */
    uint16_t bandwidth_kbps;      /**< Available bandwidth */
    uint8_t reliability_pct;      /**< Reliability percentage */
    uint32_t last_success_ms;     /**< Last successful tx */
} gf_eroute_gateway_t;

/**
 * @brief Queue statistics
 */
typedef struct {
    uint16_t total_queued;        /**< Total messages queued */
    uint16_t priority_queued;     /**< Priority messages */
    uint16_t delivered;           /**< Messages delivered */
    uint16_t expired;             /**< Messages expired */
    uint16_t retransmissions;     /**< Retransmission count */
    uint32_t avg_delay_ms;        /**< Average delivery delay */
} gf_eroute_queue_stats_t;

/**
 * @brief Delivery callback
 */
typedef void (*gf_eroute_delivery_cb_t)(uint32_t msg_id, bool success,
                                         void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize emergency routing
 * @return Status code
 */
gf_eroute_status_t gf_eroute_init(void);

/**
 * @brief Shutdown emergency routing
 */
void gf_eroute_shutdown(void);

/**
 * @brief Queue message for routing
 * @param msg Message to route
 * @return Status code
 */
gf_eroute_status_t gf_eroute_send(const gf_eroute_msg_t* msg);

/**
 * @brief Add gateway
 * @param gw Gateway info
 * @return Status code
 */
gf_eroute_status_t gf_eroute_add_gateway(const gf_eroute_gateway_t* gw);

/**
 * @brief Update gateway status
 * @param gw_id Gateway ID
 * @param status New status
 * @return Status code
 */
gf_eroute_status_t gf_eroute_update_gateway(uint8_t gw_id,
                                             gf_eroute_path_status_t status);

/**
 * @brief Get gateway info
 * @param gw_id Gateway ID
 * @param gw Output gateway info
 * @return Status code
 */
gf_eroute_status_t gf_eroute_get_gateway(uint8_t gw_id,
                                          gf_eroute_gateway_t* gw);

/**
 * @brief Get queue statistics
 * @param stats Output statistics
 * @return Status code
 */
gf_eroute_status_t gf_eroute_get_stats(gf_eroute_queue_stats_t* stats);

/**
 * @brief Cancel message
 * @param msg_id Message ID
 * @return Status code
 */
gf_eroute_status_t gf_eroute_cancel(uint32_t msg_id);

/**
 * @brief Register delivery callback
 * @param callback Delivery callback
 * @param user_data User context
 * @return Status code
 */
gf_eroute_status_t gf_eroute_register_callback(gf_eroute_delivery_cb_t callback,
                                                void* user_data);

/**
 * @brief Process routing queue
 * @return Status code
 */
gf_eroute_status_t gf_eroute_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EMERGENCY_ROUTING_H */
