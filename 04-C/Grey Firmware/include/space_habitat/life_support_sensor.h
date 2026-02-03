/**
 * @file life_support_sensor.h
 * @brief Life Support Sensor Driver for Space Habitat Environmental Monitoring
 * 
 * @details
 * This module provides drivers for critical life support sensors used in
 * space habitats, orbital stations, and lunar/Mars bases. Sensors monitor
 * atmospheric composition (O2, CO2, N2), trace contaminants, and particulates.
 * 
 * INDUSTRY RELEVANCE:
 * - NASA/ESA/JAXA space station life support (ISS ECLSS)
 * - Commercial space stations (Axiom, Orbital Reef)
 * - Lunar Gateway and Mars habitats
 * - Submarines and closed-loop environments
 * - Clean room and biosphere monitoring
 * 
 * KEY CHALLENGES:
 * - Sensor drift compensation over long missions
 * - Redundancy for crew safety (triple/quad redundant)
 * - Low power operation for orbital constraints
 * - Calibration without Earth-based references
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_LIFE_SUPPORT_SENSOR_H
#define GF_LIFE_SUPPORT_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Sensor status codes
 */
typedef enum {
    GF_LS_SENSOR_OK = 0,
    GF_LS_SENSOR_ERROR_NOT_INITIALIZED,
    GF_LS_SENSOR_ERROR_NULL_PTR,
    GF_LS_SENSOR_ERROR_INVALID_PARAM,
    GF_LS_SENSOR_ERROR_CALIBRATION,
    GF_LS_SENSOR_ERROR_TIMEOUT,
    GF_LS_SENSOR_ERROR_COMMS,
    GF_LS_SENSOR_ERROR_OUT_OF_RANGE,
    GF_LS_SENSOR_WARN_DRIFT_DETECTED,
    GF_LS_SENSOR_WARN_NEEDS_CALIBRATION
} gf_ls_sensor_status_t;

/**
 * @brief Sensor types for life support
 */
typedef enum {
    GF_LS_SENSOR_O2,              /**< Oxygen sensor (electrochemical/optical) */
    GF_LS_SENSOR_CO2,             /**< Carbon dioxide sensor (NDIR) */
    GF_LS_SENSOR_N2,              /**< Nitrogen sensor */
    GF_LS_SENSOR_HUMIDITY,        /**< Relative humidity sensor */
    GF_LS_SENSOR_TEMPERATURE,     /**< Temperature sensor */
    GF_LS_SENSOR_PRESSURE,        /**< Cabin pressure sensor */
    GF_LS_SENSOR_TRACE_CONTAM,    /**< Trace contaminant analyzer */
    GF_LS_SENSOR_PARTICULATE,     /**< Particulate matter sensor */
    GF_LS_SENSOR_CO,              /**< Carbon monoxide (toxic) */
    GF_LS_SENSOR_NH3,             /**< Ammonia (toxic) */
    GF_LS_SENSOR_H2S,             /**< Hydrogen sulfide (toxic) */
    GF_LS_SENSOR_VOC              /**< Volatile organic compounds */
} gf_ls_sensor_type_t;

/**
 * @brief Sensor redundancy level
 */
typedef enum {
    GF_LS_REDUNDANCY_SINGLE = 1,  /**< Single sensor (non-critical) */
    GF_LS_REDUNDANCY_DUAL = 2,    /**< Dual redundant (2-of-2 voting) */
    GF_LS_REDUNDANCY_TRIPLE = 3,  /**< Triple modular redundancy (2-of-3) */
    GF_LS_REDUNDANCY_QUAD = 4     /**< Quad redundant (3-of-4) */
} gf_ls_redundancy_t;

/**
 * @brief Sensor health status
 */
typedef enum {
    GF_LS_HEALTH_NOMINAL,         /**< Operating within spec */
    GF_LS_HEALTH_DEGRADED,        /**< Operating with reduced accuracy */
    GF_LS_HEALTH_FAULTY,          /**< Sensor failed, using backup */
    GF_LS_HEALTH_OFFLINE          /**< Sensor not responding */
} gf_ls_health_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    gf_ls_sensor_type_t type;           /**< Sensor type */
    gf_ls_redundancy_t redundancy;      /**< Redundancy level */
    uint32_t sample_interval_ms;        /**< Sampling interval */
    float low_alarm_threshold;          /**< Low alarm threshold */
    float high_alarm_threshold;         /**< High alarm threshold */
    float low_warning_threshold;        /**< Low warning threshold */
    float high_warning_threshold;       /**< High warning threshold */
    bool auto_calibration;              /**< Enable auto-calibration */
    uint32_t calibration_interval_hr;   /**< Calibration interval (hours) */
} gf_ls_sensor_config_t;

/**
 * @brief Sensor reading with quality metrics
 */
typedef struct {
    float value;                        /**< Sensor value */
    float uncertainty;                  /**< Measurement uncertainty */
    uint8_t quality_pct;                /**< Quality percentage (0-100) */
    gf_ls_health_t health;              /**< Sensor health */
    uint64_t timestamp_ms;              /**< Reading timestamp */
    bool alarm_active;                  /**< Alarm condition active */
    bool warning_active;                /**< Warning condition active */
} gf_ls_sensor_reading_t;

/**
 * @brief Atmospheric composition snapshot
 */
typedef struct {
    float o2_pct;                       /**< Oxygen percentage (20.9% nominal) */
    float co2_ppm;                      /**< CO2 in ppm (400-1000 nominal) */
    float n2_pct;                       /**< Nitrogen percentage (~78%) */
    float pressure_kpa;                 /**< Total pressure (101.3 kPa sea level) */
    float temperature_c;                /**< Temperature in Celsius */
    float humidity_pct;                 /**< Relative humidity */
    float co_ppm;                       /**< Carbon monoxide */
    float nh3_ppm;                      /**< Ammonia */
    uint64_t timestamp_ms;              /**< Snapshot timestamp */
} gf_ls_atmosphere_t;

/**
 * @brief Sensor alarm callback
 */
typedef void (*gf_ls_alarm_cb_t)(gf_ls_sensor_type_t type, float value, 
                                  bool is_high, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize life support sensor subsystem
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_init(void);

/**
 * @brief Shutdown sensor subsystem
 */
void gf_ls_sensor_shutdown(void);

/**
 * @brief Configure a sensor channel
 * @param channel Sensor channel ID (0-15)
 * @param config Sensor configuration
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_configure(uint8_t channel, 
                                              const gf_ls_sensor_config_t* config);

/**
 * @brief Read sensor value
 * @param channel Sensor channel
 * @param reading Output reading with quality metrics
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_read(uint8_t channel, 
                                         gf_ls_sensor_reading_t* reading);

/**
 * @brief Get current atmospheric composition
 * @param atmosphere Output atmosphere snapshot
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_get_atmosphere(gf_ls_atmosphere_t* atmosphere);

/**
 * @brief Trigger sensor calibration
 * @param channel Sensor channel
 * @param reference_value Known reference value
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_calibrate(uint8_t channel, float reference_value);

/**
 * @brief Register alarm callback
 * @param callback Alarm callback function
 * @param user_data User context data
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_register_alarm(gf_ls_alarm_cb_t callback,
                                                   void* user_data);

/**
 * @brief Get sensor health status
 * @param channel Sensor channel
 * @param health Output health status
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_get_health(uint8_t channel, 
                                               gf_ls_health_t* health);

/**
 * @brief Process sensor readings (call periodically)
 * @return Status code
 */
gf_ls_sensor_status_t gf_ls_sensor_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_LIFE_SUPPORT_SENSOR_H */
