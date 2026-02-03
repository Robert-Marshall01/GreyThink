/**
 * @file resilient_comm.h
 * @brief Resilient Communication Module for Disaster Response
 * 
 * INDUSTRY RELEVANCE:
 * Disaster response requires communications that survive:
 * - Infrastructure damage (cell towers, power grid)
 * - Network congestion
 * - Jamming/interference
 * - Terrain obstacles
 * 
 * Technologies: Mesh networks, satellite, HF radio, LoRa
 * Standards: TETRA, P25, DMR, Iridium SBD
 * Companies: Motorola Solutions, L3Harris, Iridium
 */

#ifndef GF_RESILIENT_COMM_H
#define GF_RESILIENT_COMM_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_COMM_MAX_NODES           64
#define GF_COMM_MAX_MSG_SIZE        256
#define GF_COMM_MAX_ROUTES          8
#define GF_COMM_NODE_ID_SIZE        8

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_COMM_OK = 0,
    GF_COMM_ERROR_NULL_PTR,
    GF_COMM_ERROR_NOT_INITIALIZED,
    GF_COMM_ERROR_NO_ROUTE,
    GF_COMM_ERROR_TX_FAILED,
    GF_COMM_ERROR_TIMEOUT,
    GF_COMM_ERROR_QUEUE_FULL,
    GF_COMM_ERROR_ENCRYPTION_FAIL,
    GF_COMM_ERROR_AUTH_FAIL,
    GF_COMM_WARN_WEAK_SIGNAL,
    GF_COMM_WARN_HIGH_LATENCY,
    GF_COMM_WARN_CONGESTION
} gf_comm_status_t;

typedef enum {
    GF_LINK_MESH_RF,            /* RF mesh network */
    GF_LINK_LORA,               /* LoRa long range */
    GF_LINK_SATELLITE,          /* Satellite (Iridium, Starlink) */
    GF_LINK_HF_RADIO,           /* High frequency radio */
    GF_LINK_CELLULAR,           /* 4G/5G */
    GF_LINK_WIFI,               /* WiFi */
    GF_LINK_ETHERNET,           /* Wired */
    GF_LINK_BLUETOOTH           /* Short range */
} gf_link_type_t;

typedef enum {
    GF_LINK_STATE_DISCONNECTED,
    GF_LINK_STATE_CONNECTING,
    GF_LINK_STATE_CONNECTED,
    GF_LINK_STATE_DEGRADED,
    GF_LINK_STATE_FAILING,
    GF_LINK_STATE_DISABLED
} gf_link_state_t;

typedef enum {
    GF_MSG_PRIORITY_LOW,
    GF_MSG_PRIORITY_NORMAL,
    GF_MSG_PRIORITY_HIGH,
    GF_MSG_PRIORITY_EMERGENCY
} gf_msg_priority_t;

typedef enum {
    GF_MSG_DELIVERY_BEST_EFFORT,
    GF_MSG_DELIVERY_ACKNOWLEDGED,
    GF_MSG_DELIVERY_GUARANTEED,
    GF_MSG_DELIVERY_BROADCAST
} gf_delivery_mode_t;

/**
 * @brief Link configuration
 */
typedef struct {
    gf_link_type_t type;
    uint8_t interface_id;
    uint8_t priority;           /* Lower = higher priority */
    bool enabled;
    bool fallback_only;
    uint16_t timeout_ms;
    uint16_t retry_count;
    float bandwidth_kbps;
} gf_link_config_t;

/**
 * @brief Node identity
 */
typedef struct {
    uint8_t node_id[GF_COMM_NODE_ID_SIZE];
    char name[32];
    char role[16];
    double latitude;
    double longitude;
    uint64_t last_seen_ms;
    int8_t rssi_dbm;
    uint8_t hop_count;
} gf_comm_node_t;

/**
 * @brief Link status
 */
typedef struct {
    gf_link_type_t type;
    gf_link_state_t state;
    int8_t rssi_dbm;
    float snr_db;
    uint16_t latency_ms;
    float packet_loss_pct;
    uint32_t bytes_sent;
    uint32_t bytes_received;
    uint64_t uptime_ms;
} gf_link_status_t;

/**
 * @brief Message to send
 */
typedef struct {
    uint8_t dest_node[GF_COMM_NODE_ID_SIZE];
    gf_msg_priority_t priority;
    gf_delivery_mode_t delivery;
    gf_link_type_t preferred_link;
    uint8_t payload[GF_COMM_MAX_MSG_SIZE];
    uint16_t payload_len;
    bool encrypt;
    uint32_t ttl_sec;
} gf_comm_message_t;

/**
 * @brief Received message
 */
typedef struct {
    uint8_t src_node[GF_COMM_NODE_ID_SIZE];
    uint8_t dest_node[GF_COMM_NODE_ID_SIZE];
    gf_link_type_t received_via;
    uint8_t hop_count;
    int8_t rssi_dbm;
    uint8_t payload[GF_COMM_MAX_MSG_SIZE];
    uint16_t payload_len;
    uint64_t receive_time_ms;
    bool verified;
} gf_comm_received_t;

/**
 * @brief Route information
 */
typedef struct {
    uint8_t dest_node[GF_COMM_NODE_ID_SIZE];
    uint8_t next_hop[GF_COMM_NODE_ID_SIZE];
    gf_link_type_t link;
    uint8_t hop_count;
    uint16_t latency_ms;
    float reliability_pct;
    uint64_t last_used_ms;
} gf_comm_route_t;

/**
 * @brief Store-and-forward queue status
 */
typedef struct {
    uint32_t pending_messages;
    uint32_t queue_bytes;
    uint32_t oldest_msg_age_sec;
    uint32_t retry_count;
} gf_comm_queue_status_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_comm_receive_cb_t)(const gf_comm_received_t* msg, void* user_data);

typedef void (*gf_comm_link_cb_t)(gf_link_type_t link, gf_link_state_t state, void* user_data);

typedef void (*gf_comm_node_cb_t)(const gf_comm_node_t* node, bool appeared, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_comm_status_t gf_comm_init(const uint8_t* local_node_id);
void gf_comm_shutdown(void);

/* Link management */
gf_comm_status_t gf_comm_add_link(const gf_link_config_t* config);
gf_comm_status_t gf_comm_enable_link(gf_link_type_t link, bool enable);
gf_comm_status_t gf_comm_get_link_status(gf_link_type_t link, gf_link_status_t* status);
gf_comm_status_t gf_comm_get_best_link(gf_link_type_t* link);

/* Messaging */
gf_comm_status_t gf_comm_send(const gf_comm_message_t* msg, uint32_t* msg_id);
gf_comm_status_t gf_comm_broadcast(const uint8_t* payload, uint16_t len, gf_msg_priority_t priority);
gf_comm_status_t gf_comm_get_delivery_status(uint32_t msg_id, bool* delivered);

/* Node discovery */
gf_comm_status_t gf_comm_discover_nodes(void);
gf_comm_status_t gf_comm_get_nodes(gf_comm_node_t* nodes, uint8_t max_nodes, uint8_t* count);
gf_comm_status_t gf_comm_get_node(const uint8_t* node_id, gf_comm_node_t* node);

/* Routing */
gf_comm_status_t gf_comm_get_routes(gf_comm_route_t* routes, uint8_t max_routes, uint8_t* count);
gf_comm_status_t gf_comm_set_preferred_route(const uint8_t* dest, const uint8_t* next_hop);

/* Queue management */
gf_comm_status_t gf_comm_get_queue_status(gf_comm_queue_status_t* status);
gf_comm_status_t gf_comm_flush_queue(void);

/* Callbacks */
gf_comm_status_t gf_comm_register_receive_callback(gf_comm_receive_cb_t cb, void* user_data);
gf_comm_status_t gf_comm_register_link_callback(gf_comm_link_cb_t cb, void* user_data);
gf_comm_status_t gf_comm_register_node_callback(gf_comm_node_cb_t cb, void* user_data);

/* Periodic processing */
gf_comm_status_t gf_comm_process(void);

#endif /* GF_RESILIENT_COMM_H */
