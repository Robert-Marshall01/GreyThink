/**
 * @file soil_sensor.h
 * @brief Soil Moisture and Quality Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Precision agriculture requires accurate soil monitoring for optimal
 * irrigation and fertilization. This module demonstrates:
 * - Multi-parameter soil sensing (moisture, temp, EC, pH, NPK)
 * - Capacitive and TDR moisture measurement techniques
 * - Sensor calibration for different soil types
 * - Low-power operation for remote field deployment
 * 
 * Applications: Precision farming, greenhouse automation, turf management,
 *               environmental monitoring, forestry research
 * Standards: ASABE S526.4, ISO 11277
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_SOIL_SENSOR_H
#define GF_SOIL_SENSOR_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Soil Sensor Types
 ******************************************************************************/

/** Soil type enumeration */
typedef enum {
    GF_SOIL_SANDY,            /**< Sand, loamy sand */
    GF_SOIL_LOAMY,            /**< Loam, sandy loam */
    GF_SOIL_CLAY,             /**< Clay, clay loam */
    GF_SOIL_SILTY,            /**< Silt, silty loam */
    GF_SOIL_ORGANIC,          /**< High organic content */
    GF_SOIL_CUSTOM            /**< User-calibrated */
} gf_soil_type_t;

/** Moisture measurement method */
typedef enum {
    GF_MOISTURE_CAPACITIVE,   /**< Capacitance-based */
    GF_MOISTURE_TDR,          /**< Time Domain Reflectometry */
    GF_MOISTURE_RESISTIVE,    /**< Resistance-based (legacy) */
    GF_MOISTURE_NEUTRON       /**< Neutron probe (precision) */
} gf_moisture_method_t;

/** Soil health status */
typedef enum {
    GF_SOIL_OPTIMAL,
    GF_SOIL_DRY,              /**< Below wilting point */
    GF_SOIL_WET,              /**< Above field capacity */
    GF_SOIL_SALINE,           /**< High EC */
    GF_SOIL_ACIDIC,           /**< pH too low */
    GF_SOIL_ALKALINE          /**< pH too high */
} gf_soil_status_t;

/*******************************************************************************
 * Soil Sensor Configuration
 ******************************************************************************/

/** Sensor calibration data */
typedef struct {
    uint16_t dry_value;       /**< ADC reading for 0% moisture */
    uint16_t wet_value;       /**< ADC reading for 100% moisture */
    float ec_slope;           /**< EC calibration slope */
    float ec_intercept;       /**< EC calibration intercept */
    float ph_offset;          /**< pH probe offset */
    float ph_slope;           /**< pH probe slope */
} gf_soil_calibration_t;

/** Soil sensor configuration */
typedef struct {
    gf_soil_type_t soil_type;
    gf_moisture_method_t method;
    gf_soil_calibration_t calibration;
    uint8_t probe_depth_cm;       /**< Installation depth */
    uint8_t adc_channel_moisture;
    uint8_t adc_channel_temp;
    uint8_t adc_channel_ec;
    uint8_t adc_channel_ph;
    uint16_t sample_count;        /**< Readings to average */
    uint32_t warmup_time_ms;      /**< Sensor warmup period */
    bool enable_temperature_comp;
} gf_soil_sensor_config_t;

/** Soil reading */
typedef struct {
    uint8_t moisture_pct;         /**< Volumetric water content % */
    int16_t temperature_c10;      /**< Temperature × 10 (°C) */
    uint16_t ec_us_cm;            /**< Electrical conductivity (µS/cm) */
    uint16_t ph_x100;             /**< pH × 100 (e.g., 700 = 7.00) */
    uint16_t nitrogen_ppm;        /**< Estimated N (if NPK sensor) */
    uint16_t phosphorus_ppm;      /**< Estimated P */
    uint16_t potassium_ppm;       /**< Estimated K */
    gf_soil_status_t status;
    uint32_t timestamp;
    bool sensor_fault;
} gf_soil_reading_t;

/*******************************************************************************
 * Soil Sensor Statistics
 ******************************************************************************/

typedef struct {
    uint32_t readings_taken;
    uint32_t faults_detected;
    uint8_t min_moisture_pct;
    uint8_t max_moisture_pct;
    uint8_t avg_moisture_pct;
    int16_t min_temp_c10;
    int16_t max_temp_c10;
    uint32_t operating_hours;
} gf_soil_sensor_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize soil sensor
 * @param config Sensor configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_soil_init(const gf_soil_sensor_config_t *config);

/**
 * @brief Read soil parameters
 * @param reading Output reading
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_soil_read(gf_soil_reading_t *reading);

/**
 * @brief Calibrate sensor for specific soil
 * @param soil_type Soil type
 * @param calibration Calibration parameters
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_soil_calibrate(gf_soil_type_t soil_type,
                             const gf_soil_calibration_t *calibration);

/**
 * @brief Check if irrigation is needed
 * @param threshold_pct Moisture threshold
 * @return true if moisture below threshold
 */
bool gf_soil_needs_irrigation(uint8_t threshold_pct);

/**
 * @brief Get sensor statistics
 * @return Current statistics
 */
gf_soil_sensor_stats_t gf_soil_get_stats(void);

/**
 * @brief Shutdown soil sensor
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_soil_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SOIL_SENSOR_H */
