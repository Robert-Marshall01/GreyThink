/**
 * @file demo.h
 * @brief Grey Firmware Integration Demo
 * 
 * WHAT: End-to-end demonstration connecting all Grey Firmware subsystems:
 *       Sensor → CAN Bus → MQTT → Cloud Endpoint
 * 
 * WHY: Individual modules are useful, but system integration demonstrates
 *      real-world firmware architecture skills. This demo shows:
 *      - Data flow between heterogeneous subsystems
 *      - Error handling across module boundaries
 *      - Secure boot enforcement before operation
 *      - Event-driven architecture using message bus
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Sensor task: Acquires data, publishes to internal bus
 *      - CAN gateway task: Bridges sensor data to CAN network
 *      - MQTT task: Uploads CAN/sensor data to cloud
 *      - Secure boot check before any operation begins
 */

#ifndef GF_DEMO_H
#define GF_DEMO_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Demo Configuration                                                         */
/*===========================================================================*/

/* Demo mode selection */
typedef enum {
    GF_DEMO_MODE_SENSOR_CAN_MQTT = 0,   /* Full pipeline */
    GF_DEMO_MODE_SENSOR_ONLY,           /* Sensor acquisition only */
    GF_DEMO_MODE_CAN_LOOPBACK,          /* CAN bus self-test */
    GF_DEMO_MODE_MQTT_ONLY,             /* MQTT connectivity test */
    GF_DEMO_MODE_SECURE_BOOT_CHECK      /* Security verification */
} gf_demo_mode_t;

/* Demo configuration */
typedef struct {
    gf_demo_mode_t  mode;
    uint32_t        sensor_period_ms;
    uint32_t        can_baud_rate;
    const char     *mqtt_broker;
    uint16_t        mqtt_port;
    const char     *mqtt_topic;
    bool            require_secure_boot;
} gf_demo_config_t;

/* Demo status */
typedef struct {
    bool        running;
    bool        secure_boot_ok;
    uint32_t    samples_processed;
    uint32_t    can_frames_sent;
    uint32_t    mqtt_messages_sent;
    uint32_t    errors;
    float       last_sensor_value;
} gf_demo_status_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize demo with configuration
 */
int gf_demo_init(const gf_demo_config_t *config);

/**
 * @brief Start demo operation
 */
int gf_demo_start(void);

/**
 * @brief Stop demo operation
 */
int gf_demo_stop(void);

/**
 * @brief Get demo status
 */
void gf_demo_get_status(gf_demo_status_t *status);

/**
 * @brief Run demo main loop (blocks)
 */
void gf_demo_run(void);

#endif /* GF_DEMO_H */
