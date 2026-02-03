/**
 * @file demo.c
 * @brief Grey Firmware Integration Demo Implementation
 * 
 * This demo integrates:
 * - Sensor acquisition → CAN bus transmission → MQTT publishing
 * - Secure boot verification before operation
 * - Event-driven architecture via message bus
 * 
 * The demo creates three tasks:
 * 1. Sensor Task: Reads sensor data, publishes to message bus
 * 2. CAN Gateway Task: Converts sensor messages to CAN frames
 * 3. MQTT Task: Publishes sensor/CAN data to cloud
 */

#include "demo/demo.h"
#include "grey_firmware.h"
#include <string.h>
#include <stdio.h>

/*===========================================================================*/
/* Private State                                                              */
/*===========================================================================*/

static struct {
    gf_demo_config_t    config;
    gf_demo_status_t    status;
    uint8_t             sensor_task_id;
    uint8_t             can_task_id;
    uint8_t             mqtt_task_id;
    int                 sensor_sub_handle;
    bool                initialized;
} s_demo;

/*===========================================================================*/
/* Sensor Data Structure                                                      */
/*===========================================================================*/

typedef struct {
    uint32_t    timestamp;
    uint8_t     channel;
    float       value;
    uint8_t     status;
} demo_sensor_data_t;

/*===========================================================================*/
/* Task Functions                                                             */
/*===========================================================================*/

/**
 * @brief Sensor acquisition task
 * 
 * Reads sensor data and publishes to internal message bus.
 * In a real system, this would interface with ADC hardware.
 */
static void sensor_task(void *arg) {
    (void)arg;
    
    /* Simulate sensor reading */
    static float simulated_value = 0.0f;
    simulated_value += 0.1f;
    if (simulated_value > 100.0f) simulated_value = 0.0f;
    
    /* Create sensor data message */
    demo_sensor_data_t data = {
        .timestamp = gf_sched_get_ticks(),
        .channel = 0,
        .value = simulated_value,
        .status = 0  /* OK */
    };
    
    /* Publish to message bus */
    gf_msg_publish(GF_TOPIC_SENSOR_DATA, &data, sizeof(data),
                   GF_MSG_QOS_AT_LEAST_ONCE, GF_MSG_PRIO_NORMAL);
    
    s_demo.status.samples_processed++;
    s_demo.status.last_sensor_value = simulated_value;
}

/**
 * @brief CAN gateway task
 * 
 * Subscribes to sensor data and transmits via CAN bus.
 * Demonstrates protocol translation between domains.
 */
static void can_gateway_callback(const gf_message_t *msg, void *ctx) {
    (void)ctx;
    
    if (msg->payload_len < sizeof(demo_sensor_data_t)) return;
    
    const demo_sensor_data_t *sensor = (const demo_sensor_data_t *)msg->payload;
    
    /* Convert to CAN frame */
    gf_can_frame_t frame = {
        .id = 0x100 + sensor->channel,  /* CAN ID based on channel */
        .extended = false,
        .rtr = false,
        .dlc = 8
    };
    
    /* Pack sensor data into CAN payload */
    /* Format: [timestamp:4][value:4] */
    uint32_t ts = sensor->timestamp;
    frame.data[0] = (ts >> 24) & 0xFF;
    frame.data[1] = (ts >> 16) & 0xFF;
    frame.data[2] = (ts >> 8) & 0xFF;
    frame.data[3] = ts & 0xFF;
    
    /* Pack float value */
    uint32_t value_bits;
    memcpy(&value_bits, &sensor->value, sizeof(float));
    frame.data[4] = (value_bits >> 24) & 0xFF;
    frame.data[5] = (value_bits >> 16) & 0xFF;
    frame.data[6] = (value_bits >> 8) & 0xFF;
    frame.data[7] = value_bits & 0xFF;
    
    /* Transmit via CAN */
    if (gf_can_transmit(&frame, 100) == 0) { /* GF_OK */
        s_demo.status.can_frames_sent++;
        
        /* Also publish to internal bus for MQTT */
        gf_msg_publish(GF_TOPIC_CAN_TX, &frame, sizeof(frame),
                       GF_MSG_QOS_FIRE_FORGET, GF_MSG_PRIO_NORMAL);
    } else {
        s_demo.status.errors++;
    }
}

/**
 * @brief MQTT task
 * 
 * Subscribes to CAN TX messages and publishes to cloud.
 */
static void mqtt_gateway_callback(const gf_message_t *msg, void *ctx) {
    (void)ctx;
    
    if (gf_mqtt_get_state() != GF_MQTT_CONNECTED) return;
    
    if (msg->payload_len < sizeof(gf_can_frame_t)) return;
    
    const gf_can_frame_t *frame = (const gf_can_frame_t *)msg->payload;
    
    /* Build JSON payload */
    char json[128];
    int len = snprintf(json, sizeof(json),
        "{\"id\":\"0x%03X\",\"data\":[%d,%d,%d,%d,%d,%d,%d,%d]}",
        frame->id,
        frame->data[0], frame->data[1], frame->data[2], frame->data[3],
        frame->data[4], frame->data[5], frame->data[6], frame->data[7]);
    
    /* Publish to MQTT */
    if (gf_mqtt_publish(s_demo.config.mqtt_topic, json, len,
                         GF_MQTT_QOS1, false) == 0) { /* GF_OK */
        s_demo.status.mqtt_messages_sent++;
    } else {
        s_demo.status.errors++;
    }
}

/*===========================================================================*/
/* Initialization Helpers                                                     */
/*===========================================================================*/

/**
 * @brief Verify secure boot before operation
 */
static int verify_secure_boot(void) {
    gf_boot_info_t boot_info;
    gf_boot_get_info(&boot_info);
    
    /* Check boot was confirmed */
    if (!boot_info.boot_confirmed) {
        /* Confirm boot now */
        if (gf_boot_confirm() != 0) {
            gf_error_report(GF_SUBSYS_SECURITY, 0x01, GF_SEV_ERROR,
                           "Boot confirmation failed");
            return -1;
        }
    }
    
    /* Check for debug lock in production */
    if (s_demo.config.require_secure_boot && boot_info.debug_enabled) {
        gf_error_report(GF_SUBSYS_SECURITY, 0x02, GF_SEV_WARNING,
                       "Debug enabled - not production secure");
    }
    
    s_demo.status.secure_boot_ok = true;
    
    /* Log boot information */
    gf_error_report(GF_SUBSYS_CORE, 0x00, GF_SEV_INFO,
                   "Secure boot verified, slot confirmed");
    
    return 0;
}

/**
 * @brief Initialize all subsystems
 */
static int init_subsystems(void) {
    int result;
    
    /* Initialize core */
    result = gf_sched_init(NULL);
    if (result != 0) return result;
    
    result = gf_drv_registry_init();
    if (result != 0) return result;
    
    result = gf_msg_init();
    if (result != 0) return result;
    
    gf_wdt_config_t wdt_cfg = {
        .timeout_ms = 5000,
        .window_mode = false
    };
    result = gf_error_init(&wdt_cfg);
    if (result != 0) return result;
    
    /* Initialize sensor */
    result = gf_sensor_init();
    if (result != 0) return result;
    
    /* Initialize CAN */
    gf_can_config_t can_cfg = {
        .baud_rate = s_demo.config.can_baud_rate,
        .auto_retransmit = true,
        .bus_off_recovery_ms = 1000
    };
    result = gf_can_init(&can_cfg);
    if (result != 0) return result;
    
    /* Initialize MQTT */
    gf_mqtt_config_t mqtt_cfg = {
        .broker_host = s_demo.config.mqtt_broker,
        .broker_port = s_demo.config.mqtt_port,
        .client_id = "grey_firmware_demo",
        .keepalive_sec = 60,
        .use_tls = false
    };
    result = gf_mqtt_init(&mqtt_cfg);
    if (result != 0) return result;
    
    return 0;
}

/**
 * @brief Create demo tasks
 */
static int create_tasks(void) {
    int result;
    
    /* Sensor acquisition task - 100ms period */
    gf_task_t sensor_task_cfg = {
        .entry = sensor_task,
        .name = "sensor",
        .priority = GF_PRIORITY_NORMAL,
        .period_ms = s_demo.config.sensor_period_ms,
        .deadline_ms = 50
    };
    result = gf_sched_task_create(&sensor_task_cfg, &s_demo.sensor_task_id);
    if (result != 0) return result;
    
    /* Subscribe to sensor data for CAN gateway */
    s_demo.sensor_sub_handle = gf_msg_subscribe(GF_TOPIC_SENSOR_DATA,
                                                  can_gateway_callback, NULL);
    if (s_demo.sensor_sub_handle < 0) return -1;
    
    /* Subscribe to CAN TX for MQTT gateway */
    int mqtt_sub = gf_msg_subscribe(GF_TOPIC_CAN_TX, mqtt_gateway_callback, NULL);
    if (mqtt_sub < 0) return -1;
    
    return 0;
}

/*===========================================================================*/
/* Public API                                                                 */
/*===========================================================================*/

int gf_demo_init(const gf_demo_config_t *config) {
    if (!config) return -1;
    
    memset(&s_demo, 0, sizeof(s_demo));
    memcpy(&s_demo.config, config, sizeof(gf_demo_config_t));
    
    /* Apply defaults */
    if (s_demo.config.sensor_period_ms == 0) {
        s_demo.config.sensor_period_ms = 100;
    }
    if (s_demo.config.can_baud_rate == 0) {
        s_demo.config.can_baud_rate = GF_CAN_BAUD_500KBIT;
    }
    if (!s_demo.config.mqtt_topic) {
        s_demo.config.mqtt_topic = "grey_firmware/sensor";
    }
    
    /* Initialize subsystems */
    int result = init_subsystems();
    if (result != 0) {
        gf_error_report(GF_SUBSYS_CORE, 0x10, GF_SEV_ERROR,
                       "Subsystem init failed");
        return result;
    }
    
    /* Verify secure boot */
    if (s_demo.config.require_secure_boot) {
        result = verify_secure_boot();
        if (result != 0) return result;
    } else {
        s_demo.status.secure_boot_ok = true;
    }
    
    /* Create tasks */
    result = create_tasks();
    if (result != 0) return result;
    
    s_demo.initialized = true;
    
    gf_error_report(GF_SUBSYS_CORE, 0x00, GF_SEV_INFO,
                   "Grey Firmware demo initialized");
    
    return 0;
}

int gf_demo_start(void) {
    if (!s_demo.initialized) return -1;
    if (s_demo.status.running) return 0;
    
    /* Start CAN bus */
    int result = gf_can_start();
    if (result != 0) {
        gf_error_report(GF_SUBSYS_CAN, 0x10, GF_SEV_ERROR,
                       "CAN start failed");
        return result;
    }
    
    /* Connect MQTT */
    if (s_demo.config.mqtt_broker) {
        result = gf_mqtt_connect();
        if (result != 0) {
            gf_error_report(GF_SUBSYS_MQTT, 0x10, GF_SEV_WARNING,
                           "MQTT connect failed");
            /* Continue without MQTT */
        }
    }
    
    /* Start sensor */
    result = gf_sensor_start();
    if (result != 0) {
        gf_error_report(GF_SUBSYS_SENSOR, 0x10, GF_SEV_ERROR,
                       "Sensor start failed");
        return result;
    }
    
    s_demo.status.running = true;
    
    gf_error_report(GF_SUBSYS_CORE, 0x00, GF_SEV_INFO,
                   "Demo started");
    
    return 0;
}

int gf_demo_stop(void) {
    if (!s_demo.status.running) return 0;
    
    gf_sensor_stop();
    gf_mqtt_disconnect();
    gf_can_stop();
    
    s_demo.status.running = false;
    
    return 0;
}

void gf_demo_get_status(gf_demo_status_t *status) {
    if (status) {
        memcpy(status, &s_demo.status, sizeof(gf_demo_status_t));
    }
}

void gf_demo_run(void) {
    if (!s_demo.initialized) return;
    
    /* Main demo loop */
    while (s_demo.status.running) {
        /* Process message bus */
        gf_msg_process(10);
        
        /* Process CAN events */
        gf_can_process();
        
        /* Process MQTT events */
        gf_mqtt_process();
        
        /* Kick watchdog */
        gf_wdt_kick();
    }
}
