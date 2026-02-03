/**
 * @file env_sensor.h
 * @brief Museum Environment Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Museums and cultural heritage institutions require precise environmental
 * control to preserve priceless artifacts. Temperature, humidity, light,
 * and air quality must be continuously monitored and controlled to prevent
 * degradation of paintings, textiles, documents, and organic materials.
 * 
 * KEY CAPABILITIES:
 * - High-precision temperature monitoring (±0.1°C)
 * - Relative humidity control (±1% RH)
 * - Lux level monitoring for light-sensitive materials
 * - UV radiation measurement
 * - Particulate matter (PM2.5/PM10) monitoring
 * - VOC (volatile organic compounds) detection
 * 
 * ENVIRONMENTAL TARGETS:
 * - Paintings: 21°C, 50% RH, <150 lux
 * - Textiles: 18°C, 50% RH, <50 lux
 * - Documents: 20°C, 45% RH, <50 lux
 * - Metals: 20°C, <45% RH
 * 
 * STANDARDS COMPLIANCE:
 * - ASHRAE Handbook (Museums, Galleries, Archives)
 * - CCI (Canadian Conservation Institute) Guidelines
 * - ISO 11799 (Document storage requirements)
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_MUSEUM_ENV_SENSOR_H
#define GF_MUSEUM_ENV_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Artifact material class */
typedef enum {
    MATERIAL_PAINTING,
    MATERIAL_TEXTILE,
    MATERIAL_PAPER,
    MATERIAL_METAL,
    MATERIAL_ORGANIC,
    MATERIAL_CERAMIC,
    MATERIAL_MIXED
} material_class_t;

/** Environmental alert level */
typedef enum {
    ENV_ALERT_NONE,
    ENV_ALERT_CAUTION,         /**< Approaching limits */
    ENV_ALERT_WARNING,         /**< At limits */
    ENV_ALERT_CRITICAL         /**< Potential damage */
} env_alert_t;

/** Environmental reading */
typedef struct {
    float temperature_c;       /**< Temperature °C */
    float humidity_pct;        /**< Relative humidity % */
    float lux;                 /**< Light level lux */
    float uv_mw_m2;           /**< UV irradiance mW/m² */
    float pm25_ug_m3;         /**< PM2.5 µg/m³ */
    float pm10_ug_m3;         /**< PM10 µg/m³ */
    float voc_ppb;            /**< VOC level ppb */
    uint32_t timestamp;
} env_reading_t;

/** Zone configuration */
typedef struct {
    uint8_t zone_id;
    char name[32];
    material_class_t material;
    float temp_min;
    float temp_max;
    float humidity_min;
    float humidity_max;
    float lux_max;
    float uv_max;
} zone_config_t;

/** Zone status */
typedef struct {
    uint8_t zone_id;
    env_reading_t current;
    env_alert_t temp_alert;
    env_alert_t humidity_alert;
    env_alert_t light_alert;
    env_alert_t air_quality_alert;
    float trending_temp;       /**< °C/hour trend */
    float trending_humidity;   /**< %RH/hour trend */
} zone_status_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize museum environment sensor system
 * @param museum_id Museum identifier
 * @return 0 on success, negative on error
 */
int museum_env_init(uint32_t museum_id);

/**
 * @brief Configure monitoring zone
 * @param config Zone configuration
 * @return 0 on success, negative on error
 */
int museum_env_configure_zone(const zone_config_t* config);

/**
 * @brief Read current environmental data
 * @param zone_id Zone to read
 * @param reading Output reading
 * @return 0 on success, negative on error
 */
int museum_env_read(uint8_t zone_id, env_reading_t* reading);

/**
 * @brief Get zone status with alerts
 * @param zone_id Zone to query
 * @param status Output status
 * @return 0 on success, negative on error
 */
int museum_env_get_status(uint8_t zone_id, zone_status_t* status);

/**
 * @brief Get historical data
 * @param zone_id Zone to query
 * @param start_time Period start
 * @param end_time Period end
 * @param readings Output array
 * @param max_readings Maximum to return
 * @param count Actual count
 * @return 0 on success, negative on error
 */
int museum_env_get_history(uint8_t zone_id, uint32_t start_time,
                           uint32_t end_time, env_reading_t* readings,
                           uint16_t max_readings, uint16_t* count);

/**
 * @brief Shutdown environment sensor system
 * @return 0 on success, negative on error
 */
int museum_env_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_MUSEUM_ENV_SENSOR_H */
