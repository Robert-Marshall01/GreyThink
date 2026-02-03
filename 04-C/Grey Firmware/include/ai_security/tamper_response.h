/**
 * @file tamper_response.h
 * @brief AI-Enabled Tamper Detection and Response
 * 
 * INDUSTRY RELEVANCE:
 * Physical and logical tampering threatens edge devices processing sensitive
 * data. This module combines hardware tamper detection with AI-based anomaly
 * detection for comprehensive protection. Essential for payment terminals,
 * utility meters, and defense systems requiring anti-tamper certification.
 * 
 * Key applications:
 * - Payment terminal PCI-PTS compliance
 * - Smart meter tamper detection
 * - Defense TEMPEST/anti-tamper
 * - IP protection for embedded AI
 * - Critical infrastructure sensors
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_TAMPER_RESPONSE_H
#define GF_TAMPER_RESPONSE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define TAMPER_MAX_SENSORS          16      /**< Max tamper sensors */
#define TAMPER_MAX_ZONES            8       /**< Max protection zones */
#define TAMPER_HISTORY_SIZE         256     /**< Event history */
#define TAMPER_ZEROIZE_TIMEOUT_MS   100     /**< Max zeroization time */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Tamper sensor type
 */
typedef enum {
    TAMPER_SENSOR_MESH = 0,         /**< Conductive mesh */
    TAMPER_SENSOR_SWITCH,           /**< Microswitch */
    TAMPER_SENSOR_LIGHT,            /**< Light detector */
    TAMPER_SENSOR_TEMP,             /**< Temperature anomaly */
    TAMPER_SENSOR_VOLTAGE,          /**< Voltage glitch */
    TAMPER_SENSOR_CLOCK,            /**< Clock manipulation */
    TAMPER_SENSOR_EMI,              /**< EMI/fault injection */
    TAMPER_SENSOR_MOTION            /**< Accelerometer motion */
} tamper_sensor_t;

/**
 * @brief Response action
 */
typedef enum {
    TAMPER_ACTION_LOG = 0,          /**< Log event only */
    TAMPER_ACTION_ALERT,            /**< Alert + log */
    TAMPER_ACTION_LOCK,             /**< Lock device */
    TAMPER_ACTION_ZEROIZE,          /**< Zeroize secrets */
    TAMPER_ACTION_BRICK             /**< Permanent disable */
} tamper_action_t;

/**
 * @brief Detection level
 */
typedef enum {
    TAMPER_LEVEL_NONE = 0,
    TAMPER_LEVEL_PROBE,             /**< Probing detected */
    TAMPER_LEVEL_ATTEMPT,           /**< Tamper attempt */
    TAMPER_LEVEL_BREACH,            /**< Active breach */
    TAMPER_LEVEL_COMPROMISE         /**< Device compromised */
} tamper_level_t;

/**
 * @brief Tamper event
 */
typedef struct {
    uint32_t event_id;
    uint32_t timestamp;
    tamper_sensor_t sensor;
    tamper_level_t level;
    tamper_action_t action_taken;
    float sensor_value;
    float threshold;
    char description[64];
} tamper_event_t;

/**
 * @brief Zone configuration
 */
typedef struct {
    uint8_t zone_id;
    char name[32];
    tamper_sensor_t sensors[4];
    uint8_t sensor_count;
    tamper_action_t action;
    float threshold;
    bool armed;
} tamper_zone_t;

/**
 * @brief System status
 */
typedef struct {
    bool armed;
    tamper_level_t current_level;
    uint32_t events_total;
    uint32_t events_active;
    bool zeroized;
    bool locked;
    uint32_t last_trip_time;
} tamper_status_t;

/**
 * @brief Configuration
 */
typedef struct {
    bool auto_arm_on_boot;
    bool enable_ai_detection;
    bool enable_zeroization;
    uint16_t debounce_ms;
    float ai_sensitivity;           /**< AI detection sensitivity */
    tamper_action_t default_action;
} tamper_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize tamper detection
 * @param config Tamper configuration
 * @return 0 on success, negative on error
 */
int tamper_init(const tamper_config_t* config);

/**
 * @brief Shutdown tamper system
 * @return 0 on success, negative on error
 */
int tamper_shutdown(void);

/**
 * @brief Configure protection zone
 * @param zone Zone configuration
 * @return 0 on success, negative on error
 */
int tamper_configure_zone(const tamper_zone_t* zone);

/**
 * @brief Arm tamper detection
 * @return 0 on success, negative on error
 */
int tamper_arm(void);

/**
 * @brief Disarm tamper detection (requires auth)
 * @param auth_token Authentication token
 * @return 0 on success, negative on error
 */
int tamper_disarm(const uint8_t* auth_token);

/**
 * @brief Get current status
 * @param status Output status
 * @return 0 on success, negative on error
 */
int tamper_get_status(tamper_status_t* status);

/**
 * @brief Get event history
 * @param events Output event array
 * @param max_events Array size
 * @return Number of events returned
 */
int tamper_get_events(tamper_event_t* events, uint16_t max_events);

/**
 * @brief Register zeroization callback
 * @param callback Function to call before zeroization
 * @return 0 on success, negative on error
 */
int tamper_register_zeroize_callback(void (*callback)(void));

/**
 * @brief Manual zeroization trigger
 * @return 0 on success, negative on error
 */
int tamper_trigger_zeroize(void);

/**
 * @brief Process tamper detection (call periodically)
 * @return Current tamper level
 */
tamper_level_t tamper_process(void);

/**
 * @brief Train AI anomaly detector
 * @param baseline_data Normal operation data
 * @param data_size Data size
 * @return 0 on success, negative on error
 */
int tamper_train_ai(const float* baseline_data, uint32_t data_size);

#ifdef __cplusplus
}
#endif

#endif /* GF_TAMPER_RESPONSE_H */
