/**
 * @file mqtt_client.h
 * @brief MQTT Client Stub for IoT Connectivity
 * 
 * WHAT: Lightweight MQTT 3.1.1 client for cloud connectivity with QoS
 *       support, automatic reconnection, and TLS integration.
 * 
 * WHY: MQTT is the dominant protocol for IoT device-to-cloud communication.
 *      Its publish/subscribe model, small footprint (~10KB), and support
 *      for unreliable networks make it ideal for resource-constrained
 *      embedded devices. Understanding MQTT is essential for any IoT role.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Clean API for connect/publish/subscribe operations
 *      - Integration with message bus for internal event routing
 *      - Connection state machine with exponential backoff
 *      - Last Will Testament (LWT) for disconnection detection
 * 
 * Industry applications: smart home, industrial IoT, telematics, agriculture
 * 
 * NOTE: This is an annotated stub. Production implementation would include
 *       a full TCP/TLS stack and protocol state machine.
 */

#ifndef GF_MQTT_CLIENT_H
#define GF_MQTT_CLIENT_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_MQTT_MAX_TOPIC_LEN       64
#define GF_MQTT_MAX_PAYLOAD_LEN     1024
#define GF_MQTT_MAX_CLIENT_ID_LEN   24
#define GF_MQTT_KEEPALIVE_SEC       60
#define GF_MQTT_MAX_SUBSCRIPTIONS   8

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_MQTT_DISCONNECTED = 0,
    GF_MQTT_CONNECTING,
    GF_MQTT_CONNECTED,
    GF_MQTT_RECONNECTING,
    GF_MQTT_ERROR
} gf_mqtt_state_t;

typedef enum {
    GF_MQTT_QOS0 = 0,   /* At most once */
    GF_MQTT_QOS1,       /* At least once */
    GF_MQTT_QOS2        /* Exactly once */
} gf_mqtt_qos_t;

/* Connection configuration */
typedef struct {
    const char *broker_host;
    uint16_t    broker_port;
    const char *client_id;
    const char *username;        /* NULL if not used */
    const char *password;        /* NULL if not used */
    bool        use_tls;
    uint16_t    keepalive_sec;
    /* Last Will and Testament */
    const char *lwt_topic;
    const char *lwt_message;
    gf_mqtt_qos_t lwt_qos;
    bool        lwt_retain;
} gf_mqtt_config_t;

/* Message received callback */
typedef void (*gf_mqtt_msg_cb)(const char *topic, const uint8_t *payload,
                                uint16_t len, void *ctx);

/* Connection state callback */
typedef void (*gf_mqtt_state_cb)(gf_mqtt_state_t state, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize MQTT client
 * @param config Connection configuration
 * @return GF_OK on success
 */
int gf_mqtt_init(const gf_mqtt_config_t *config);

/**
 * @brief Connect to MQTT broker
 * @return GF_OK if connection initiated (async)
 */
int gf_mqtt_connect(void);

/**
 * @brief Disconnect from broker
 */
int gf_mqtt_disconnect(void);

/**
 * @brief Subscribe to a topic
 * @param topic Topic filter (wildcards: +, #)
 * @param qos Quality of service level
 * @param callback Function called on message receipt
 * @param ctx User context
 * @return Subscription ID or error code
 */
int gf_mqtt_subscribe(const char *topic, gf_mqtt_qos_t qos,
                       gf_mqtt_msg_cb callback, void *ctx);

/**
 * @brief Unsubscribe from a topic
 */
int gf_mqtt_unsubscribe(int sub_id);

/**
 * @brief Publish a message
 * @param topic Destination topic
 * @param payload Message data
 * @param len Payload length
 * @param qos Quality of service
 * @param retain Retain flag
 * @return GF_OK on success
 */
int gf_mqtt_publish(const char *topic, const void *payload, uint16_t len,
                     gf_mqtt_qos_t qos, bool retain);

/**
 * @brief Process MQTT events (call from main loop)
 */
void gf_mqtt_process(void);

/**
 * @brief Get current connection state
 */
gf_mqtt_state_t gf_mqtt_get_state(void);

/**
 * @brief Set state change callback
 */
void gf_mqtt_set_state_callback(gf_mqtt_state_cb callback, void *ctx);

/**
 * @brief Get MQTT driver descriptor for registration
 */
const void* gf_mqtt_get_driver(void);

#endif /* GF_MQTT_CLIENT_H */
