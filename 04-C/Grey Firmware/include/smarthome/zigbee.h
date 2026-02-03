/**
 * @file zigbee.h
 * @brief Zigbee Protocol Driver Stub
 * 
 * INDUSTRY RELEVANCE:
 * Zigbee is the dominant protocol for smart home devices with over 500 million
 * deployed devices. Used in lighting (Philips Hue), thermostats (Ecobee),
 * sensors, and smart locks. Zigbee 3.0 provides interoperability across vendors.
 * 
 * WHY THIS MATTERS:
 * - Low-power mesh networking (battery-powered sensors)
 * - Self-healing mesh topology (reliability)
 * - AES-128 encryption (security)
 * - Matter-over-Thread compatibility (future-proofing)
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Network layer abstraction
 * - Device discovery and pairing flows
 * - Cluster-based command/attribute model
 * - Security key management
 */

#ifndef GF_ZIGBEE_H
#define GF_ZIGBEE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_ZIGBEE_MAX_DEVICES       32      /* Maximum network devices */
#define GF_ZIGBEE_MAX_ENDPOINTS     8       /* Endpoints per device */
#define GF_ZIGBEE_MAX_CLUSTERS      16      /* Clusters per endpoint */
#define GF_ZIGBEE_PAYLOAD_MAX       127     /* Max ZCL payload bytes */
#define GF_ZIGBEE_SHORT_ADDR_LEN    2       /* 16-bit network address */
#define GF_ZIGBEE_EXT_ADDR_LEN      8       /* 64-bit IEEE address */

/*===========================================================================*/
/* Zigbee Protocol Constants                                                  */
/*===========================================================================*/

/* Device types */
typedef enum {
    GF_ZIGBEE_COORDINATOR = 0x00,   /* Network coordinator (forms network) */
    GF_ZIGBEE_ROUTER      = 0x01,   /* Router (relays messages) */
    GF_ZIGBEE_END_DEVICE  = 0x02    /* End device (sleepy) */
} gf_zigbee_device_type_t;

/* Network state */
typedef enum {
    GF_ZIGBEE_STATE_OFFLINE = 0,    /* Not connected */
    GF_ZIGBEE_STATE_SCANNING,       /* Scanning for networks */
    GF_ZIGBEE_STATE_JOINING,        /* Association in progress */
    GF_ZIGBEE_STATE_ONLINE,         /* Connected to network */
    GF_ZIGBEE_STATE_LEAVING         /* Leave in progress */
} gf_zigbee_state_t;

/* Standard Zigbee clusters (Home Automation profile) */
typedef enum {
    GF_ZCL_CLUSTER_BASIC        = 0x0000,   /* Device information */
    GF_ZCL_CLUSTER_POWER_CFG    = 0x0001,   /* Battery/power status */
    GF_ZCL_CLUSTER_IDENTIFY     = 0x0003,   /* Device identification */
    GF_ZCL_CLUSTER_GROUPS       = 0x0004,   /* Group management */
    GF_ZCL_CLUSTER_SCENES       = 0x0005,   /* Scene management */
    GF_ZCL_CLUSTER_ON_OFF       = 0x0006,   /* On/Off control */
    GF_ZCL_CLUSTER_LEVEL        = 0x0008,   /* Level control (dimming) */
    GF_ZCL_CLUSTER_COLOR        = 0x0300,   /* Color control */
    GF_ZCL_CLUSTER_TEMPERATURE  = 0x0402,   /* Temperature measurement */
    GF_ZCL_CLUSTER_HUMIDITY     = 0x0405,   /* Humidity measurement */
    GF_ZCL_CLUSTER_OCCUPANCY    = 0x0406,   /* Occupancy sensing */
    GF_ZCL_CLUSTER_IAS_ZONE     = 0x0500,   /* Security zones */
    GF_ZCL_CLUSTER_DOOR_LOCK    = 0x0101    /* Door lock control */
} gf_zcl_cluster_id_t;

/* ZCL frame types */
typedef enum {
    GF_ZCL_FRAME_GLOBAL  = 0x00,    /* Global command */
    GF_ZCL_FRAME_CLUSTER = 0x01    /* Cluster-specific command */
} gf_zcl_frame_type_t;

/* ZCL global commands */
typedef enum {
    GF_ZCL_CMD_READ_ATTR        = 0x00,
    GF_ZCL_CMD_READ_ATTR_RSP    = 0x01,
    GF_ZCL_CMD_WRITE_ATTR       = 0x02,
    GF_ZCL_CMD_WRITE_ATTR_RSP   = 0x03,
    GF_ZCL_CMD_REPORT_ATTR      = 0x0A,
    GF_ZCL_CMD_DEFAULT_RSP      = 0x0B,
    GF_ZCL_CMD_DISCOVER_ATTR    = 0x0C
} gf_zcl_global_cmd_t;

/*===========================================================================*/
/* Data Types                                                                 */
/*===========================================================================*/

/* 64-bit IEEE address */
typedef struct {
    uint8_t bytes[GF_ZIGBEE_EXT_ADDR_LEN];
} gf_zigbee_ieee_addr_t;

/* Device entry in network table */
typedef struct {
    uint16_t                short_addr;     /* 16-bit network address */
    gf_zigbee_ieee_addr_t   ieee_addr;      /* 64-bit IEEE address */
    gf_zigbee_device_type_t type;           /* Device type */
    uint8_t                 endpoint_count; /* Active endpoints */
    uint8_t                 endpoints[GF_ZIGBEE_MAX_ENDPOINTS];
    uint16_t                clusters[GF_ZIGBEE_MAX_CLUSTERS];
    uint8_t                 lqi;            /* Link quality indicator */
    int8_t                  rssi;           /* Received signal strength */
    uint32_t                last_seen;      /* Timestamp of last message */
    bool                    is_active;      /* Device responding */
} gf_zigbee_device_t;

/* Zigbee network information */
typedef struct {
    uint16_t    pan_id;             /* Personal Area Network ID */
    uint8_t     extended_pan_id[8]; /* Extended PAN ID */
    uint8_t     channel;            /* Radio channel (11-26) */
    uint8_t     network_key[16];    /* AES-128 network key */
    bool        permit_joining;     /* Network open for joining */
    uint8_t     security_level;     /* 0=none, 5=full encryption */
} gf_zigbee_network_t;

/* ZCL message */
typedef struct {
    uint16_t    cluster_id;
    uint8_t     endpoint;
    uint8_t     frame_type;
    uint8_t     command_id;
    uint8_t     seq_num;
    uint8_t     payload[GF_ZIGBEE_PAYLOAD_MAX];
    uint8_t     payload_len;
    bool        is_response;
} gf_zcl_message_t;

/* Pairing callback */
typedef void (*gf_zigbee_pair_cb_t)(const gf_zigbee_device_t *device, 
                                    bool success, void *ctx);

/* Message receive callback */
typedef void (*gf_zigbee_rx_cb_t)(uint16_t src_addr, 
                                   const gf_zcl_message_t *msg, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize Zigbee stack
 * @param type Device type (coordinator/router/end device)
 * @return GF_OK on success
 */
int gf_zigbee_init(gf_zigbee_device_type_t type);

/**
 * @brief Form or join a Zigbee network
 * @param network Network configuration (NULL for auto-join)
 * @return GF_OK on success
 */
int gf_zigbee_start(const gf_zigbee_network_t *network);

/**
 * @brief Stop Zigbee stack and leave network
 */
int gf_zigbee_stop(void);

/**
 * @brief Get current network state
 */
gf_zigbee_state_t gf_zigbee_get_state(void);

/**
 * @brief Permit devices to join network
 * @param duration_sec Time to allow joining (0 = disable, 255 = permanent)
 */
int gf_zigbee_permit_join(uint8_t duration_sec);

/**
 * @brief Get device by short address
 * @return Device info or NULL if not found
 */
const gf_zigbee_device_t *gf_zigbee_get_device(uint16_t short_addr);

/**
 * @brief Get device by IEEE address
 */
const gf_zigbee_device_t *gf_zigbee_get_device_by_ieee(
    const gf_zigbee_ieee_addr_t *ieee);

/**
 * @brief Get list of all devices in network
 * @param devices Output array
 * @param max_devices Array size
 * @return Number of devices copied
 */
int gf_zigbee_get_devices(gf_zigbee_device_t *devices, int max_devices);

/**
 * @brief Remove device from network
 * @param short_addr Device network address
 * @param force Remove even if device offline
 */
int gf_zigbee_remove_device(uint16_t short_addr, bool force);

/**
 * @brief Send ZCL command to device
 * @param dst_addr Destination short address (0xFFFF = broadcast)
 * @param msg ZCL message to send
 * @return GF_OK on success
 */
int gf_zigbee_send(uint16_t dst_addr, const gf_zcl_message_t *msg);

/**
 * @brief Send ZCL command with response callback
 */
int gf_zigbee_send_with_response(uint16_t dst_addr, 
                                  const gf_zcl_message_t *msg,
                                  gf_zigbee_rx_cb_t callback, void *ctx,
                                  uint32_t timeout_ms);

/**
 * @brief Register message receive callback
 */
int gf_zigbee_register_rx_callback(gf_zigbee_rx_cb_t callback, void *ctx);

/**
 * @brief Register pairing event callback
 */
int gf_zigbee_register_pair_callback(gf_zigbee_pair_cb_t callback, void *ctx);

/**
 * @brief Process Zigbee stack (call from main loop)
 * @param timeout_ms Maximum time to process
 * @return Number of events processed
 */
int gf_zigbee_process(uint32_t timeout_ms);

/**
 * @brief Get network key for secure storage
 */
int gf_zigbee_get_network_key(uint8_t key[16]);

/**
 * @brief Set network key from secure storage
 */
int gf_zigbee_set_network_key(const uint8_t key[16]);

/*===========================================================================*/
/* ZCL Helper Functions                                                       */
/*===========================================================================*/

/**
 * @brief Build On/Off cluster command
 * @param cmd 0=off, 1=on, 2=toggle
 */
int gf_zcl_build_on_off(gf_zcl_message_t *msg, uint8_t endpoint, uint8_t cmd);

/**
 * @brief Build Level control command
 * @param level Target level (0-254)
 * @param transition_time Transition in 100ms units
 */
int gf_zcl_build_level(gf_zcl_message_t *msg, uint8_t endpoint,
                       uint8_t level, uint16_t transition_time);

/**
 * @brief Build Color control command (hue/saturation)
 */
int gf_zcl_build_color_hs(gf_zcl_message_t *msg, uint8_t endpoint,
                          uint8_t hue, uint8_t saturation,
                          uint16_t transition_time);

/**
 * @brief Build Color temperature command
 * @param mireds Color temperature in mireds (1M/Kelvin)
 */
int gf_zcl_build_color_temp(gf_zcl_message_t *msg, uint8_t endpoint,
                            uint16_t mireds, uint16_t transition_time);

/**
 * @brief Build Read Attributes request
 */
int gf_zcl_build_read_attr(gf_zcl_message_t *msg, uint16_t cluster,
                           uint8_t endpoint, const uint16_t *attr_ids,
                           int attr_count);

#endif /* GF_ZIGBEE_H */
