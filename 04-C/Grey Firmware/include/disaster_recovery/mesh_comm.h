/**
 * @file mesh_comm.h
 * @brief Mesh Communication for Disaster Recovery Networks
 * 
 * @details
 * Self-healing mesh network communication for disaster recovery and
 * emergency response scenarios. Implements ad-hoc networking with
 * automatic route discovery and fault tolerance.
 * 
 * INDUSTRY RELEVANCE:
 * - First responder networks (FirstNet)
 * - Disaster relief communications
 * - Military tactical networks
 * - IoT mesh networks (Zigbee, Thread)
 * - Smart utility networks
 * 
 * KEY FEATURES:
 * - Dynamic topology discovery
 * - Multi-hop routing
 * - Self-healing on node failure
 * - Quality of Service (QoS) prioritization
 * - Low-power operation modes
 * - Secure communication
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_MESH_COMM_H
#define GF_MESH_COMM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum mesh nodes */
#define GF_MESH_MAX_NODES           256

/** Maximum hops */
#define GF_MESH_MAX_HOPS            15

/** Maximum neighbors */
#define GF_MESH_MAX_NEIGHBORS       16

/** Node address length */
#define GF_MESH_ADDR_LEN            8

/** Maximum message size */
#define GF_MESH_MAX_MSG_SIZE        256

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Mesh status codes
 */
typedef enum {
    GF_MESH_OK = 0,
    GF_MESH_ERROR_NOT_INIT,
    GF_MESH_ERROR_NULL_PTR,
    GF_MESH_ERROR_NO_ROUTE,
    GF_MESH_ERROR_TIMEOUT,
    GF_MESH_ERROR_BUFFER_FULL,
    GF_MESH_ERROR_TX_FAIL,
    GF_MESH_WARN_DEGRADED,
    GF_MESH_WARN_LOW_BATTERY
} gf_mesh_status_t;

/**
 * @brief Node role
 */
typedef enum {
    GF_MESH_ROLE_ROUTER,          /**< Full routing node */
    GF_MESH_ROLE_END_DEVICE,      /**< End device (no routing) */
    GF_MESH_ROLE_COORDINATOR,     /**< Network coordinator */
    GF_MESH_ROLE_BORDER           /**< Border router (gateway) */
} gf_mesh_role_t;

/**
 * @brief Message priority
 */
typedef enum {
    GF_MESH_PRIORITY_LOW,
    GF_MESH_PRIORITY_NORMAL,
    GF_MESH_PRIORITY_HIGH,
    GF_MESH_PRIORITY_EMERGENCY    /**< Emergency traffic */
} gf_mesh_priority_t;

/**
 * @brief Node address
 */
typedef struct {
    uint8_t addr[GF_MESH_ADDR_LEN];
} gf_mesh_addr_t;

/**
 * @brief Neighbor info
 */
typedef struct {
    gf_mesh_addr_t addr;          /**< Neighbor address */
    int8_t rssi_dbm;              /**< Signal strength */
    uint8_t lqi;                  /**< Link quality indicator */
    uint8_t hop_count;            /**< Hops to coordinator */
    uint32_t last_seen_ms;        /**< Last communication */
    bool reachable;               /**< Currently reachable */
} gf_mesh_neighbor_t;

/**
 * @brief Network statistics
 */
typedef struct {
    uint32_t tx_packets;          /**< Transmitted packets */
    uint32_t rx_packets;          /**< Received packets */
    uint32_t tx_failures;         /**< Transmission failures */
    uint32_t route_discoveries;   /**< Route discoveries */
    uint32_t route_failures;      /**< Route discovery failures */
    uint8_t neighbor_count;       /**< Active neighbors */
    uint8_t avg_hop_count;        /**< Average hop count */
} gf_mesh_stats_t;

/**
 * @brief Mesh message
 */
typedef struct {
    gf_mesh_addr_t src;           /**< Source address */
    gf_mesh_addr_t dst;           /**< Destination address */
    gf_mesh_priority_t priority;  /**< Message priority */
    uint8_t ttl;                  /**< Time to live (hops) */
    uint16_t seq_num;             /**< Sequence number */
    uint8_t data[GF_MESH_MAX_MSG_SIZE]; /**< Payload */
    uint16_t data_len;            /**< Payload length */
} gf_mesh_msg_t;

/**
 * @brief Configuration
 */
typedef struct {
    gf_mesh_role_t role;          /**< Node role */
    uint16_t pan_id;              /**< Network ID */
    uint8_t channel;              /**< RF channel */
    int8_t tx_power_dbm;          /**< Transmit power */
    bool security_enabled;        /**< Enable encryption */
    uint8_t network_key[16];      /**< Network key */
} gf_mesh_config_t;

/**
 * @brief Message receive callback
 */
typedef void (*gf_mesh_rx_cb_t)(const gf_mesh_msg_t* msg, void* user_data);

/**
 * @brief Topology change callback
 */
typedef void (*gf_mesh_topo_cb_t)(const gf_mesh_addr_t* node,
                                   bool joined, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize mesh network
 * @param config Network configuration
 * @return Status code
 */
gf_mesh_status_t gf_mesh_init(const gf_mesh_config_t* config);

/**
 * @brief Shutdown mesh network
 */
void gf_mesh_shutdown(void);

/**
 * @brief Get local address
 * @param addr Output address
 * @return Status code
 */
gf_mesh_status_t gf_mesh_get_address(gf_mesh_addr_t* addr);

/**
 * @brief Send message
 * @param msg Message to send
 * @return Status code
 */
gf_mesh_status_t gf_mesh_send(const gf_mesh_msg_t* msg);

/**
 * @brief Broadcast message
 * @param data Payload data
 * @param len Payload length
 * @param priority Message priority
 * @return Status code
 */
gf_mesh_status_t gf_mesh_broadcast(const uint8_t* data, uint16_t len,
                                    gf_mesh_priority_t priority);

/**
 * @brief Get neighbors
 * @param neighbors Output neighbor array
 * @param max_count Maximum entries
 * @param count Actual count
 * @return Status code
 */
gf_mesh_status_t gf_mesh_get_neighbors(gf_mesh_neighbor_t* neighbors,
                                        uint8_t max_count, uint8_t* count);

/**
 * @brief Get network statistics
 * @param stats Output statistics
 * @return Status code
 */
gf_mesh_status_t gf_mesh_get_stats(gf_mesh_stats_t* stats);

/**
 * @brief Register receive callback
 * @param callback Receive callback
 * @param user_data User context
 * @return Status code
 */
gf_mesh_status_t gf_mesh_register_rx_callback(gf_mesh_rx_cb_t callback,
                                               void* user_data);

/**
 * @brief Register topology callback
 * @param callback Topology callback
 * @param user_data User context
 * @return Status code
 */
gf_mesh_status_t gf_mesh_register_topo_callback(gf_mesh_topo_cb_t callback,
                                                 void* user_data);

/**
 * @brief Process mesh network
 * @return Status code
 */
gf_mesh_status_t gf_mesh_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_MESH_COMM_H */
