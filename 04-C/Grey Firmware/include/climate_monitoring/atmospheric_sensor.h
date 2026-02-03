/**
 * @file atmospheric_sensor.h
 * @brief Atmospheric Sensor Interface for Climate Monitoring
 * 
 * INDUSTRY RELEVANCE:
 * Climate change research and environmental monitoring require distributed
 * sensor networks capturing atmospheric data at high temporal and spatial
 * resolution. This module enables edge-based atmospheric sensing for
 * weather stations, air quality networks, and climate research platforms.
 * 
 * TECHNICAL SCOPE:
 * - Temperature/humidity/pressure (standard meteorological)
 * - Wind speed and direction (ultrasonic anemometer)
 * - Solar radiation (pyranometer, UV sensors)
 * - Precipitation measurement
 * - Greenhouse gas sensing (CO2, CH4, N2O)
 * - Aerosol optical depth
 * 
 * CALIBRATION:
 * - NIST-traceable temperature calibration
 * - WMO-compliant humidity measurement
 * - Periodic auto-zero for gas sensors
 * 
 * STANDARDS COMPLIANCE:
 * - WMO-No. 8 (Meteorological instruments)
 * - EPA CFR 40 Part 58 (Air quality monitoring)
 * - ISO 17025 (Calibration laboratories)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ATMOSPHERIC_SENSOR_H
#define GF_ATMOSPHERIC_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define ATMO_MAX_SENSORS        16    /**< Maximum sensors */
#define ATMO_HISTORY_HOURS      168   /**< 1 week of hourly data */
#define ATMO_SAMPLE_RATE_HZ     1     /**< Default sample rate */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Sensor type */
typedef enum {
    ATMO_SENSOR_TEMP_HUMID,        /**< Temperature/humidity combo */
    ATMO_SENSOR_PRESSURE,          /**< Barometric pressure */
    ATMO_SENSOR_WIND,              /**< Anemometer */
    ATMO_SENSOR_RAIN,              /**< Rain gauge */
    ATMO_SENSOR_SOLAR,             /**< Solar radiation */
    ATMO_SENSOR_CO2,               /**< Carbon dioxide */
    ATMO_SENSOR_CH4,               /**< Methane */
    ATMO_SENSOR_OZONE,             /**< Surface ozone */
    ATMO_SENSOR_PM,                /**< Particulate matter */
    ATMO_SENSOR_UV                 /**< UV index */
} atmo_sensor_type_t;

/** Weather condition */
typedef enum {
    WEATHER_CLEAR,
    WEATHER_PARTLY_CLOUDY,
    WEATHER_CLOUDY,
    WEATHER_RAIN,
    WEATHER_HEAVY_RAIN,
    WEATHER_SNOW,
    WEATHER_THUNDERSTORM,
    WEATHER_FOG
} weather_condition_t;

/** Air quality index category */
typedef enum {
    AQI_GOOD,                      /**< 0-50 */
    AQI_MODERATE,                  /**< 51-100 */
    AQI_USG,                       /**< 101-150 (Unhealthy for sensitive) */
    AQI_UNHEALTHY,                 /**< 151-200 */
    AQI_VERY_UNHEALTHY,            /**< 201-300 */
    AQI_HAZARDOUS                  /**< 301+ */
} aqi_category_t;

/** Atmospheric reading */
typedef struct {
    float temperature_c;
    float humidity_pct;
    float pressure_hpa;
    float dewpoint_c;
    float wind_speed_m_s;
    float wind_direction_deg;
    float wind_gust_m_s;
    float precipitation_mm_hr;
    float solar_radiation_w_m2;
    float uv_index;
    float visibility_km;
} atmo_reading_t;

/** Gas concentrations */
typedef struct {
    float co2_ppm;
    float ch4_ppb;
    float n2o_ppb;
    float ozone_ppb;
    float co_ppm;
    float no2_ppb;
    float so2_ppb;
} gas_reading_t;

/** Particulate reading */
typedef struct {
    float pm1_ug_m3;
    float pm2_5_ug_m3;
    float pm10_ug_m3;
    uint32_t particle_count_0_3um;
    uint32_t particle_count_2_5um;
    aqi_category_t aqi_category;
    uint16_t aqi_value;
} pm_reading_t;

/** Complete atmospheric status */
typedef struct {
    atmo_reading_t atmospheric;
    gas_reading_t gases;
    pm_reading_t particulates;
    weather_condition_t condition;
    float cloud_cover_pct;
    uint64_t timestamp_ms;
    bool sensors_healthy;
} atmo_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize atmospheric sensors */
int atmo_init(void);

/** Configure sensor */
int atmo_config_sensor(uint8_t sensor_id, atmo_sensor_type_t type);

/** Read atmospheric data */
int atmo_read(atmo_reading_t *reading);

/** Read gas concentrations */
int atmo_read_gases(gas_reading_t *gases);

/** Read particulates */
int atmo_read_pm(pm_reading_t *pm);

/** Get complete status */
int atmo_get_status(atmo_status_t *status);

/** Calculate AQI from readings */
uint16_t atmo_calculate_aqi(const pm_reading_t *pm, const gas_reading_t *gases);

/** Trigger sensor calibration */
int atmo_calibrate(uint8_t sensor_id);

/** Process sensor cycle */
int atmo_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_ATMOSPHERIC_SENSOR_H */
