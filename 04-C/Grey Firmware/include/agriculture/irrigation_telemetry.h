/**
 * @file irrigation_telemetry.h
 * @brief Irrigation Telemetry Collector - Smart Agriculture Robotics
 * 
 * @details Industry Relevance:
 * Smart irrigation systems reduce water usage by 20-50% through:
 * - Soil moisture sensor networks (capacitive, TDR, tensiometer)
 * - Weather-based ET (evapotranspiration) calculations
 * - Variable rate irrigation (VRI) pivot control
 * - Leak detection and pressure monitoring
 * 
 * Global water scarcity makes efficient irrigation critical.
 * Connected irrigation enables remote monitoring and autonomous
 * scheduling, reducing labor and improving crop yields.
 * 
 * Protocols: LoRaWAN for sensor networks, Modbus RTU for pumps,
 * cellular for cloud connectivity.
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_AGRICULTURE_IRRIGATION_TELEMETRY_H
#define GF_AGRICULTURE_IRRIGATION_TELEMETRY_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum soil sensors per zone */
#define GF_IRRIGATION_MAX_SENSORS       32

/** Maximum irrigation zones */
#define GF_IRRIGATION_MAX_ZONES         16

/** Sensor poll interval (seconds) */
#define GF_IRRIGATION_POLL_INTERVAL_S   60

/** Low moisture alarm threshold (% volumetric) */
#define GF_IRRIGATION_LOW_MOISTURE_PCT  15

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Sensor types for irrigation monitoring
 */
typedef enum {
    GF_IRRIGATION_SENSOR_SOIL_MOISTURE, /**< Volumetric water content */
    GF_IRRIGATION_SENSOR_SOIL_TEMP,     /**< Soil temperature */
    GF_IRRIGATION_SENSOR_RAIN_GAUGE,    /**< Precipitation sensor */
    GF_IRRIGATION_SENSOR_FLOW_METER,    /**< Water flow rate */
    GF_IRRIGATION_SENSOR_PRESSURE,      /**< Line pressure */
    GF_IRRIGATION_SENSOR_WEATHER,       /**< Weather station (ET) */
    GF_IRRIGATION_SENSOR_LEVEL          /**< Tank/reservoir level */
} gf_irrigation_sensor_type_t;

/**
 * @brief Individual sensor reading
 */
typedef struct {
    uint16_t sensor_id;                 /**< Unique sensor ID */
    gf_irrigation_sensor_type_t type;   /**< Sensor type */
    float value;                        /**< Sensor value */
    float battery_v;                    /**< Sensor battery voltage */
    int8_t rssi_dbm;                    /**< RF signal strength */
    uint32_t timestamp;                 /**< Reading timestamp */
    bool alarm_active;                  /**< Threshold alarm */
} gf_irrigation_reading_t;

/**
 * @brief Irrigation zone status
 */
typedef struct {
    uint8_t zone_id;                    /**< Zone identifier */
    float avg_moisture_pct;             /**< Average soil moisture */
    float min_moisture_pct;             /**< Minimum reading */
    float max_moisture_pct;             /**< Maximum reading */
    float flow_rate_lpm;                /**< Current flow rate L/min */
    float volume_applied_l;             /**< Water applied today */
    bool valve_open;                    /**< Zone valve state */
    uint32_t runtime_min;               /**< Today's runtime minutes */
    uint32_t next_scheduled;            /**< Next scheduled run time */
} gf_irrigation_zone_t;

/**
 * @brief Weather and ET data for scheduling
 */
typedef struct {
    float temp_c;                       /**< Air temperature */
    float humidity_pct;                 /**< Relative humidity */
    float wind_speed_ms;                /**< Wind speed m/s */
    float solar_radiation_wm2;          /**< Solar radiation W/m² */
    float et_mm;                        /**< Reference ET (mm/day) */
    float rain_mm;                      /**< Rainfall today (mm) */
    float rain_forecast_mm;             /**< Forecasted rain (mm) */
} gf_irrigation_weather_t;

/**
 * @brief Telemetry packet for cloud upload
 */
typedef struct {
    uint32_t timestamp;                 /**< Packet timestamp */
    uint8_t zone_count;                 /**< Number of zones */
    gf_irrigation_zone_t zones[GF_IRRIGATION_MAX_ZONES];
    gf_irrigation_weather_t weather;    /**< Current weather data */
    float system_pressure_bar;          /**< Main line pressure */
    float total_volume_m3;              /**< Total water used today */
    bool pump_running;                  /**< Pump status */
    uint8_t active_alarms;              /**< Alarm count */
} gf_irrigation_telemetry_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize irrigation telemetry system
 * @return 0 on success, negative error code on failure
 */
int gf_irrigation_init(void);

/**
 * @brief Register a sensor with the telemetry system
 * @param sensor_id Unique sensor ID
 * @param type Sensor type
 * @param zone_id Associated zone
 * @return 0 on success
 */
int gf_irrigation_register_sensor(uint16_t sensor_id,
                                  gf_irrigation_sensor_type_t type,
                                  uint8_t zone_id);

/**
 * @brief Process incoming sensor reading
 * @param reading Sensor reading data
 * @return 0 on success
 */
int gf_irrigation_process_reading(const gf_irrigation_reading_t* reading);

/**
 * @brief Get current zone status
 * @param zone_id Zone to query
 * @param zone Output zone status
 * @return 0 on success
 */
int gf_irrigation_get_zone(uint8_t zone_id, gf_irrigation_zone_t* zone);

/**
 * @brief Generate telemetry packet for cloud upload
 * @param telemetry Output telemetry structure
 * @return 0 on success
 */
int gf_irrigation_generate_telemetry(gf_irrigation_telemetry_t* telemetry);

/**
 * @brief Control zone valve
 * @param zone_id Zone to control
 * @param open True to open, false to close
 * @return 0 on success
 */
int gf_irrigation_control_valve(uint8_t zone_id, bool open);

/**
 * @brief Calculate irrigation schedule from ET data
 * @param weather Current weather/ET data
 * @return 0 on success
 */
int gf_irrigation_calculate_schedule(const gf_irrigation_weather_t* weather);

/**
 * @brief Shutdown irrigation telemetry
 * @return 0 on success
 */
int gf_irrigation_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_AGRICULTURE_IRRIGATION_TELEMETRY_H */
