/**
 * @file structural_sensor.h
 * @brief Structural Sensor Telemetry - Smart Construction Systems
 * 
 * @details Industry Relevance:
 * Structural Health Monitoring (SHM) during construction and throughout
 * building lifecycle prevents failures and extends service life:
 * - Strain gauges on critical members
 * - Accelerometers for vibration monitoring
 * - Tilt sensors for settlement detection
 * - Crack monitors for concrete structures
 * - Temperature compensation for thermal effects
 * 
 * Real-time monitoring during concrete pours, excavation near existing
 * structures, and high-rise construction enables early warning of problems.
 * 
 * Post-construction: Continuous monitoring for bridges, dams, tunnels,
 * and high-value structures. Growing "smart building" market for occupancy
 * and condition-based maintenance.
 * 
 * Standards: ASCE guidelines, fib Model Code, Eurocode monitoring annex
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_CONSTRUCTION_STRUCTURAL_SENSOR_H
#define GF_CONSTRUCTION_STRUCTURAL_SENSOR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum sensors per monitoring system */
#define GF_STRUCT_MAX_SENSORS           128

/** Maximum sensor channels per device */
#define GF_STRUCT_MAX_CHANNELS          8

/** High-speed sample rate for dynamic analysis (Hz) */
#define GF_STRUCT_DYNAMIC_RATE_HZ       1000

/** Static sample rate (Hz) */
#define GF_STRUCT_STATIC_RATE_HZ        1

/** Alarm threshold for unexpected strain (microstrain) */
#define GF_STRUCT_STRAIN_ALARM_UE       2000

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Sensor types for structural monitoring
 */
typedef enum {
    GF_STRUCT_SENSOR_STRAIN,        /**< Strain gauge */
    GF_STRUCT_SENSOR_ACCEL,         /**< Accelerometer */
    GF_STRUCT_SENSOR_TILT,          /**< Tiltmeter/inclinometer */
    GF_STRUCT_SENSOR_CRACK,         /**< Crack width monitor */
    GF_STRUCT_SENSOR_DISPLACEMENT,  /**< LVDT/displacement */
    GF_STRUCT_SENSOR_LOAD_CELL,     /**< Load cell */
    GF_STRUCT_SENSOR_TEMP,          /**< Temperature */
    GF_STRUCT_SENSOR_PRESSURE,      /**< Pore pressure */
    GF_STRUCT_SENSOR_FIBER_OPTIC    /**< Distributed fiber optic */
} gf_struct_sensor_type_t;

/**
 * @brief Monitoring mode
 */
typedef enum {
    GF_STRUCT_MODE_STATIC,          /**< Slow sampling, low power */
    GF_STRUCT_MODE_DYNAMIC,         /**< High-speed sampling */
    GF_STRUCT_MODE_TRIGGERED,       /**< Event-triggered high-speed */
    GF_STRUCT_MODE_SLEEP            /**< Low-power sleep */
} gf_struct_mode_t;

/**
 * @brief Individual sensor reading
 */
typedef struct {
    uint16_t sensor_id;             /**< Unique sensor ID */
    gf_struct_sensor_type_t type;   /**< Sensor type */
    uint8_t channel;                /**< Channel number */
    float value;                    /**< Primary reading */
    float temperature;              /**< Temperature for compensation */
    uint32_t timestamp;             /**< Reading timestamp */
    bool alarm;                     /**< Threshold exceeded */
    bool valid;                     /**< Reading validity */
} gf_struct_reading_t;

/**
 * @brief Sensor node status
 */
typedef struct {
    uint16_t node_id;               /**< Node identifier */
    uint8_t sensor_count;           /**< Sensors on this node */
    float battery_v;                /**< Battery voltage */
    int8_t rssi_dbm;                /**< RF signal strength */
    gf_struct_mode_t mode;          /**< Current mode */
    uint32_t last_reading;          /**< Last reading timestamp */
    uint32_t alarms_active;         /**< Bitmask of active alarms */
} gf_struct_node_t;

/**
 * @brief Structural element being monitored
 */
typedef struct {
    uint16_t element_id;            /**< Element identifier */
    char name[32];                  /**< Element name */
    uint8_t sensor_count;           /**< Sensors on element */
    float max_strain_ue;            /**< Maximum recorded strain */
    float min_strain_ue;            /**< Minimum recorded strain */
    float current_strain_ue;        /**< Current strain */
    float drift_mm;                 /**< Cumulative drift */
    bool alarm_active;              /**< Any sensor in alarm */
} gf_struct_element_t;

/**
 * @brief Telemetry packet for cloud upload
 */
typedef struct {
    uint32_t timestamp;             /**< Packet timestamp */
    uint16_t reading_count;         /**< Number of readings */
    gf_struct_reading_t readings[GF_STRUCT_MAX_SENSORS];
    uint8_t node_count;             /**< Number of nodes */
    gf_struct_node_t nodes[16];     /**< Node status array */
    uint8_t alarms_active;          /**< Total active alarms */
} gf_struct_telemetry_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize structural monitoring system
 * @return 0 on success, negative error code on failure
 */
int gf_struct_init(void);

/**
 * @brief Register a sensor
 * @param sensor_id Unique ID
 * @param type Sensor type
 * @param element_id Associated structural element
 * @return 0 on success
 */
int gf_struct_register_sensor(uint16_t sensor_id,
                              gf_struct_sensor_type_t type,
                              uint16_t element_id);

/**
 * @brief Process sensor reading
 * @param reading Sensor reading data
 * @return 0 on success
 */
int gf_struct_process_reading(const gf_struct_reading_t* reading);

/**
 * @brief Get element status
 * @param element_id Element to query
 * @param element Output element status
 * @return 0 on success
 */
int gf_struct_get_element(uint16_t element_id, gf_struct_element_t* element);

/**
 * @brief Set monitoring mode
 * @param mode Desired mode
 * @return 0 on success
 */
int gf_struct_set_mode(gf_struct_mode_t mode);

/**
 * @brief Trigger high-speed capture
 * @param duration_ms Capture duration
 * @return 0 on success
 */
int gf_struct_trigger_dynamic(uint32_t duration_ms);

/**
 * @brief Generate telemetry packet
 * @param telemetry Output telemetry structure
 * @return 0 on success
 */
int gf_struct_generate_telemetry(gf_struct_telemetry_t* telemetry);

/**
 * @brief Set alarm threshold for sensor
 * @param sensor_id Sensor to configure
 * @param low_threshold Low alarm threshold
 * @param high_threshold High alarm threshold
 * @return 0 on success
 */
int gf_struct_set_threshold(uint16_t sensor_id,
                            float low_threshold,
                            float high_threshold);

/**
 * @brief Shutdown structural monitoring
 * @return 0 on success
 */
int gf_struct_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CONSTRUCTION_STRUCTURAL_SENSOR_H */
