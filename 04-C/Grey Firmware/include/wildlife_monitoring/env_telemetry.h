/**
 * @file env_telemetry.h
 * @brief Environmental Telemetry for Wildlife Monitoring
 * 
 * INDUSTRY RELEVANCE:
 * Environmental sensor networks support conservation efforts, climate
 * research, and ecosystem monitoring. Organizations like NOAA, NASA,
 * national parks, and conservation NGOs deploy sensor networks that
 * require low-power, reliable firmware. Companies like Campbell Scientific,
 * Wildlife Computers, and Lotek build these specialized systems.
 * 
 * This module provides environmental data collection for wildlife habitats
 * including temperature, humidity, light levels, and acoustic monitoring.
 * 
 * KEY CAPABILITIES:
 * - Temperature/humidity sensing
 * - Light level (PAR/UV) measurement
 * - Acoustic monitoring (bioacoustics)
 * - Soil moisture sensing
 * - Water quality parameters
 * - Weather station integration
 * - Solar-powered operation support
 * 
 * STANDARDS COMPLIANCE:
 * - WMO standards for meteorological data
 * - GBIF data standards
 * - Darwin Core biodiversity format
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ENV_TELEMETRY_H
#define GF_ENV_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define ET_MAX_SENSORS         32    /**< Max environmental sensors */
#define ET_SAMPLE_BUFFER_SIZE  1024  /**< Audio sample buffer */
#define ET_LOG_INTERVAL_S      300   /**< Default log interval (5 min) */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Sensor type */
typedef enum {
    ET_SENSOR_TEMPERATURE,
    ET_SENSOR_HUMIDITY,
    ET_SENSOR_BAROMETER,
    ET_SENSOR_LIGHT_PAR,
    ET_SENSOR_LIGHT_UV,
    ET_SENSOR_SOIL_MOISTURE,
    ET_SENSOR_SOIL_TEMP,
    ET_SENSOR_RAIN_GAUGE,
    ET_SENSOR_WIND_SPEED,
    ET_SENSOR_WIND_DIR,
    ET_SENSOR_ACOUSTIC,
    ET_SENSOR_WATER_TEMP,
    ET_SENSOR_WATER_PH,
    ET_SENSOR_WATER_DO
} et_sensor_type_t;

/** Weather data */
typedef struct {
    float air_temp_c;
    float humidity_pct;
    float pressure_hpa;
    float wind_speed_ms;
    float wind_dir_deg;
    float rainfall_mm;
    float solar_rad_wm2;
} et_weather_t;

/** Acoustic event */
typedef struct {
    uint32_t timestamp;
    float frequency_hz;
    float amplitude_db;
    uint16_t duration_ms;
    uint8_t species_id;    /**< Classified species (0 = unknown) */
} et_acoustic_event_t;

/** Environmental record */
typedef struct {
    uint32_t timestamp;
    uint32_t station_id;
    et_weather_t weather;
    float soil_moisture_pct;
    float soil_temp_c;
    uint8_t acoustic_events;
    float battery_v;
    float solar_v;
} et_record_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize environmental telemetry
 * @param station_id Unique station identifier
 * @return 0 on success
 */
int et_init(uint32_t station_id);

/**
 * @brief Read weather data
 * @param weather Output weather structure
 * @return 0 on success
 */
int et_read_weather(et_weather_t* weather);

/**
 * @brief Start acoustic monitoring
 * @param sample_rate_hz Audio sample rate
 * @return 0 on success
 */
int et_start_acoustic(uint32_t sample_rate_hz);

/**
 * @brief Get acoustic events
 * @param events Output event array
 * @param max_events Maximum events to return
 * @param count Output event count
 * @return 0 on success
 */
int et_get_acoustic_events(et_acoustic_event_t* events, 
                           uint8_t max_events, uint8_t* count);

/**
 * @brief Log environmental record
 * @param record Record to store
 * @return 0 on success
 */
int et_log_record(const et_record_t* record);

/**
 * @brief Shutdown telemetry
 */
void et_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ENV_TELEMETRY_H */
