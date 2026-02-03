/**
 * @file airbag_sensor.h
 * @brief Airbag Crash Sensor Driver for Automotive Safety Systems
 *
 * INDUSTRY RELEVANCE:
 * Airbag deployment is a life-critical automotive safety function requiring:
 * - Sub-millisecond crash detection and decision making
 * - High-g accelerometer interfaces (up to 250g)
 * - Redundant sensing and validation
 * - ASIL-D compliance per ISO 26262
 * - Deterministic real-time response
 *
 * Used in: Vehicle restraint systems, SRS (Supplemental Restraint Systems)
 *
 * @note This is a stub demonstrating automotive safety firmware patterns.
 */

#ifndef GF_AIRBAG_SENSOR_H
#define GF_AIRBAG_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Crash Sensor Definitions                                                  */
/*===========================================================================*/

/**
 * @brief Crash severity levels
 */
typedef enum {
    GF_CRASH_NONE,              /**< No crash detected */
    GF_CRASH_MINOR,             /**< Minor impact, no deployment */
    GF_CRASH_MODERATE,          /**< Moderate impact, belt tensioner only */
    GF_CRASH_SEVERE,            /**< Severe impact, deploy front airbags */
    GF_CRASH_ROLLOVER,          /**< Rollover detected, deploy curtains */
    GF_CRASH_SIDE               /**< Side impact detected */
} gf_crash_severity_t;

/**
 * @brief Sensor location in vehicle
 */
typedef enum {
    GF_SENSOR_CENTRAL,          /**< Central sensor (ACU) */
    GF_SENSOR_FRONT_LEFT,       /**< Front left satellite */
    GF_SENSOR_FRONT_RIGHT,      /**< Front right satellite */
    GF_SENSOR_SIDE_LEFT,        /**< Left side impact */
    GF_SENSOR_SIDE_RIGHT,       /**< Right side impact */
    GF_SENSOR_REAR,             /**< Rear impact sensor */
    GF_SENSOR_ROLLOVER,         /**< Rollover gyroscope */
    GF_SENSOR_COUNT
} gf_sensor_location_t;

/**
 * @brief Crash detection algorithm
 */
typedef enum {
    GF_ALGO_THRESHOLD,          /**< Simple threshold crossing */
    GF_ALGO_INTEGRAL,           /**< Velocity change integration */
    GF_ALGO_PATTERN,            /**< Pattern matching */
    GF_ALGO_NEURAL              /**< Neural network (future) */
} gf_crash_algorithm_t;

/**
 * @brief Accelerometer data (3-axis, high-g)
 */
typedef struct {
    int16_t x_mg;               /**< X-axis in milli-g */
    int16_t y_mg;               /**< Y-axis in milli-g */
    int16_t z_mg;               /**< Z-axis in milli-g */
    uint32_t timestamp_us;      /**< Microsecond timestamp */
    bool valid;                 /**< Data valid flag */
} gf_accel_data_t;

/**
 * @brief Crash sensor configuration
 */
typedef struct {
    gf_sensor_location_t location;
    gf_crash_algorithm_t algorithm;
    uint16_t sample_rate_hz;        /**< Sampling rate (1000-10000) */
    int16_t threshold_mg;           /**< Deployment threshold */
    uint16_t integral_threshold;    /**< Velocity change threshold */
    uint8_t confirmation_samples;   /**< Samples to confirm crash */
    bool enable_safing;             /**< Require safing sensor agreement */
    void (*deploy_callback)(gf_crash_severity_t severity, void* ctx);
    void* callback_ctx;
} gf_airbag_config_t;

/**
 * @brief Sensor diagnostic status
 */
typedef struct {
    bool sensor_ok;                 /**< Sensor functioning */
    bool wiring_ok;                 /**< Wiring integrity */
    bool squib_ok;                  /**< Squib circuit ok */
    bool safing_ok;                 /**< Safing sensor ok */
    uint8_t fault_code;             /**< Diagnostic trouble code */
    uint32_t last_test_time;        /**< Last self-test time */
} gf_airbag_status_t;

/**
 * @brief Crash event record
 */
typedef struct {
    gf_crash_severity_t severity;
    uint32_t timestamp_ms;
    int16_t peak_accel_mg;
    uint16_t delta_v_kmh;           /**< Velocity change km/h * 10 */
    uint8_t algorithm_confidence;   /**< Confidence 0-100% */
    bool deployed;                  /**< Airbag deployed */
    uint8_t deploy_time_ms;         /**< Time to deploy decision */
} gf_crash_event_t;

/**
 * @brief Airbag sensor handle
 */
typedef struct gf_airbag_sensor* gf_airbag_handle_t;

/*===========================================================================*/
/* Airbag Sensor API                                                         */
/*===========================================================================*/

/**
 * @brief Initialize airbag sensor
 * @param config Configuration
 * @param handle Output handle
 * @return 0 on success
 */
int gf_airbag_init(const gf_airbag_config_t* config,
                   gf_airbag_handle_t* handle);

/**
 * @brief Arm the airbag system (enable deployment)
 * @param handle Sensor handle
 * @return 0 on success
 * @note Only arm after all diagnostics pass
 */
int gf_airbag_arm(gf_airbag_handle_t handle);

/**
 * @brief Disarm the airbag system
 * @param handle Sensor handle
 * @return 0 on success
 */
int gf_airbag_disarm(gf_airbag_handle_t handle);

/**
 * @brief Process accelerometer sample (ISR-safe)
 * @param handle Sensor handle
 * @param data Accelerometer reading
 * @return Crash severity (GF_CRASH_NONE if no crash)
 */
gf_crash_severity_t gf_airbag_process(gf_airbag_handle_t handle,
                                       const gf_accel_data_t* data);

/**
 * @brief Get sensor status
 * @param handle Sensor handle
 * @param status Output status
 * @return 0 on success
 */
int gf_airbag_get_status(gf_airbag_handle_t handle,
                         gf_airbag_status_t* status);

/**
 * @brief Run self-test diagnostics
 * @param handle Sensor handle
 * @return 0 on pass, negative on failure
 */
int gf_airbag_self_test(gf_airbag_handle_t handle);

/**
 * @brief Retrieve crash event data (for EDR)
 * @param handle Sensor handle
 * @param event Output event
 * @return 0 on success, -1 if no event
 */
int gf_airbag_get_crash_event(gf_airbag_handle_t handle,
                              gf_crash_event_t* event);

/**
 * @brief Deinitialize sensor
 * @param handle Sensor handle
 */
void gf_airbag_deinit(gf_airbag_handle_t handle);

#ifdef __cplusplus
}
#endif

#endif /* GF_AIRBAG_SENSOR_H */
