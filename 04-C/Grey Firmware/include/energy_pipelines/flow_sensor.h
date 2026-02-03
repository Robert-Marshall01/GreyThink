/**
 * @file flow_sensor.h
 * @brief Pipeline Flow Sensor Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Pipeline integrity monitoring is critical for oil & gas, water, and
 * chemical transport infrastructure. Flow sensors detect anomalies that
 * indicate leaks, blockages, or unauthorized tapping. Modern SCADA systems
 * rely on accurate flow data for real-time decision making.
 * 
 * KEY CAPABILITIES:
 * - Ultrasonic transit-time flow measurement
 * - Coriolis mass flow sensing
 * - Multi-path velocity profiling
 * - Temperature and pressure compensation
 * - Bi-directional flow detection
 * - Custody transfer accuracy (±0.1%)
 * 
 * STANDARDS COMPLIANCE:
 * - API MPMS (Manual of Petroleum Measurement Standards)
 * - ISO 17089 (Ultrasonic flowmeters)
 * - AGA Report No. 9 (Natural gas measurement)
 * - OIML R117 (Measuring systems for liquids)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_FLOW_SENSOR_H
#define GF_FLOW_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Flow sensor type */
typedef enum {
    FLOW_TYPE_ULTRASONIC,      /**< Transit-time ultrasonic */
    FLOW_TYPE_CORIOLIS,        /**< Coriolis mass flow */
    FLOW_TYPE_MAGNETIC,        /**< Electromagnetic */
    FLOW_TYPE_TURBINE,         /**< Turbine flowmeter */
    FLOW_TYPE_VORTEX           /**< Vortex shedding */
} flow_sensor_type_t;

/** Flow measurement status */
typedef enum {
    FLOW_STATUS_OK,
    FLOW_STATUS_LOW_SIGNAL,
    FLOW_STATUS_AIR_DETECTED,
    FLOW_STATUS_OUT_OF_RANGE,
    FLOW_STATUS_SENSOR_FAULT
} flow_status_t;

/** Flow measurement */
typedef struct {
    float flow_rate;           /**< m³/h (volumetric) */
    float mass_flow_rate;      /**< kg/h (mass) */
    float velocity;            /**< m/s */
    float temperature;         /**< °C */
    float pressure;            /**< bar (gauge) */
    float density;             /**< kg/m³ */
    float totalizer;           /**< Accumulated volume/mass */
    flow_status_t status;
    uint32_t timestamp_ms;
} flow_measurement_t;

/** Sensor configuration */
typedef struct {
    flow_sensor_type_t type;
    float pipe_diameter_mm;    /**< Internal pipe diameter */
    float zero_cutoff;         /**< Low-flow cutoff */
    float high_alarm;          /**< High flow alarm threshold */
    float low_alarm;           /**< Low flow alarm threshold */
    uint16_t sample_rate_hz;   /**< Sampling rate */
} flow_config_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize flow sensor
 * @param sensor_id Sensor identifier
 * @param config Sensor configuration
 * @return 0 on success, negative on error
 */
int flow_sensor_init(uint8_t sensor_id, const flow_config_t* config);

/**
 * @brief Read current flow measurement
 * @param sensor_id Sensor to read
 * @param measurement Output measurement
 * @return 0 on success, negative on error
 */
int flow_sensor_read(uint8_t sensor_id, flow_measurement_t* measurement);

/**
 * @brief Reset totalizer counter
 * @param sensor_id Sensor to reset
 * @return 0 on success, negative on error
 */
int flow_sensor_reset_totalizer(uint8_t sensor_id);

/**
 * @brief Perform zero calibration
 * @param sensor_id Sensor to calibrate
 * @return 0 on success, negative on error
 */
int flow_sensor_zero_cal(uint8_t sensor_id);

/**
 * @brief Get sensor diagnostics
 * @param sensor_id Sensor to query
 * @param signal_strength Output signal strength (0-100%)
 * @param error_code Output error code
 * @return 0 on success, negative on error
 */
int flow_sensor_diagnostics(uint8_t sensor_id, uint8_t* signal_strength,
                            uint16_t* error_code);

/**
 * @brief Shutdown flow sensor
 * @param sensor_id Sensor to shutdown
 * @return 0 on success, negative on error
 */
int flow_sensor_shutdown(uint8_t sensor_id);

#ifdef __cplusplus
}
#endif

#endif /* GF_FLOW_SENSOR_H */
