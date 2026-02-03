/**
 * @file buoy_sensor.h
 * @brief Buoy Sensor Stub - Smart Ocean Monitoring
 * 
 * @details Industry Relevance:
 * Ocean monitoring buoys provide critical data for:
 * - Weather forecasting and hurricane tracking
 * - Tsunami early warning systems
 * - Navigation safety (wave height, currents)
 * - Climate research (sea surface temperature trends)
 * - Offshore operations (oil rigs, wind farms, aquaculture)
 * - Environmental monitoring (pollution, harmful algal blooms)
 * 
 * Deployed buoys must operate autonomously for months to years with
 * solar power, satellite communications, and extreme reliability.
 * 
 * Organizations: NOAA, NDBC, Argo float program, OOI, Copernicus
 * 
 * Standards: WMO/IOC specifications, NMEA 2000 (sensors),
 * Iridium/Argos/Globalstar (satellite comms)
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_OCEAN_BUOY_SENSOR_H
#define GF_OCEAN_BUOY_SENSOR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum sensors per buoy */
#define GF_BUOY_MAX_SENSORS             16

/** Wave spectrum frequency bins */
#define GF_BUOY_WAVE_FREQ_BINS          64

/** CTD profile max depth (meters) */
#define GF_BUOY_MAX_PROFILE_DEPTH       1000

/** Telemetry report interval (seconds) */
#define GF_BUOY_REPORT_INTERVAL_S       3600

/** Power budget (mAh/day) */
#define GF_BUOY_POWER_BUDGET_MAH        500

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Buoy sensor types
 */
typedef enum {
    GF_BUOY_SENSOR_SST,             /**< Sea surface temperature */
    GF_BUOY_SENSOR_WAVE,            /**< Wave height/period */
    GF_BUOY_SENSOR_WIND,            /**< Wind speed/direction */
    GF_BUOY_SENSOR_BARO,            /**< Barometric pressure */
    GF_BUOY_SENSOR_SOLAR,           /**< Solar radiation */
    GF_BUOY_SENSOR_HUMIDITY,        /**< Relative humidity */
    GF_BUOY_SENSOR_CTD,             /**< Conductivity/temp/depth */
    GF_BUOY_SENSOR_CURRENT,         /**< ADCP current profiler */
    GF_BUOY_SENSOR_CHLOROPHYLL,     /**< Chlorophyll fluorescence */
    GF_BUOY_SENSOR_TURBIDITY,       /**< Turbidity/clarity */
    GF_BUOY_SENSOR_PH,              /**< pH/ocean acidification */
    GF_BUOY_SENSOR_DO               /**< Dissolved oxygen */
} gf_buoy_sensor_type_t;

/**
 * @brief Wave measurement data
 */
typedef struct {
    float significant_height_m;     /**< Significant wave height (Hs) */
    float maximum_height_m;         /**< Maximum wave height */
    float peak_period_s;            /**< Peak wave period */
    float mean_period_s;            /**< Mean wave period */
    float mean_direction_deg;       /**< Mean wave direction */
    float spread_deg;               /**< Directional spread */
    float spectrum[GF_BUOY_WAVE_FREQ_BINS]; /**< Energy spectrum */
} gf_buoy_wave_t;

/**
 * @brief Meteorological observations
 */
typedef struct {
    float wind_speed_ms;            /**< Wind speed at 10m */
    float wind_gust_ms;             /**< Peak gust */
    float wind_dir_deg;             /**< Wind direction */
    float pressure_hpa;             /**< Barometric pressure */
    float air_temp_c;               /**< Air temperature */
    float humidity_pct;             /**< Relative humidity */
    float solar_wm2;                /**< Solar radiation */
    float rain_mm;                  /**< Rainfall (if equipped) */
} gf_buoy_meteo_t;

/**
 * @brief CTD profile sample
 */
typedef struct {
    float depth_m;                  /**< Sample depth */
    float temperature_c;            /**< Temperature */
    float salinity_psu;             /**< Salinity */
    float density_kgm3;             /**< Calculated density */
    float sound_speed_ms;           /**< Calculated sound speed */
} gf_buoy_ctd_t;

/**
 * @brief Buoy status and power
 */
typedef struct {
    char buoy_id[16];               /**< Buoy identifier */
    double latitude_deg;            /**< Current position lat */
    double longitude_deg;           /**< Current position lon */
    float battery_v;                /**< Battery voltage */
    float battery_pct;              /**< Battery state of charge */
    float solar_current_ma;         /**< Solar charge current */
    float power_budget_pct;         /**< Power budget remaining */
    int8_t rssi_dbm;                /**< Satellite signal strength */
    uint32_t last_tx_time;          /**< Last transmission time */
    uint16_t messages_pending;      /**< Queued messages */
    bool anchor_watch;              /**< Excessive drift alarm */
    bool maintenance_due;           /**< Service required */
} gf_buoy_status_t;

/**
 * @brief Observation telemetry packet
 */
typedef struct {
    uint32_t timestamp;             /**< Observation time */
    gf_buoy_wave_t wave;            /**< Wave data */
    gf_buoy_meteo_t meteo;          /**< Meteorological data */
    float sst_c;                    /**< Sea surface temperature */
    float subsurface_temp_c[10];    /**< Subsurface temps at depths */
    float chlorophyll_ugL;          /**< Chlorophyll concentration */
    float turbidity_ntu;            /**< Turbidity */
    float ph;                       /**< pH value */
    float do_mgL;                   /**< Dissolved oxygen */
} gf_buoy_observation_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize buoy sensor system
 * @return 0 on success, negative error code on failure
 */
int gf_buoy_init(void);

/**
 * @brief Process sensor readings (call periodically)
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int gf_buoy_process(uint32_t delta_ms);

/**
 * @brief Get wave measurements
 * @param wave Output wave structure
 * @return 0 on success
 */
int gf_buoy_get_wave(gf_buoy_wave_t* wave);

/**
 * @brief Get meteorological observations
 * @param meteo Output meteo structure
 * @return 0 on success
 */
int gf_buoy_get_meteo(gf_buoy_meteo_t* meteo);

/**
 * @brief Trigger CTD profile descent
 * @param max_depth Maximum depth (meters)
 * @return 0 on success
 */
int gf_buoy_start_ctd_profile(float max_depth);

/**
 * @brief Get CTD profile samples
 * @param samples Output sample array
 * @param max_samples Maximum samples to return
 * @return Number of samples collected
 */
int gf_buoy_get_ctd_profile(gf_buoy_ctd_t* samples, uint16_t max_samples);

/**
 * @brief Generate observation telemetry packet
 * @param obs Output observation structure
 * @return 0 on success
 */
int gf_buoy_get_observation(gf_buoy_observation_t* obs);

/**
 * @brief Get buoy status
 * @param status Output status structure
 * @return 0 on success
 */
int gf_buoy_get_status(gf_buoy_status_t* status);

/**
 * @brief Queue telemetry for satellite transmission
 * @param obs Observation to transmit
 * @param priority Priority level
 * @return 0 on success
 */
int gf_buoy_queue_telemetry(const gf_buoy_observation_t* obs, uint8_t priority);

/**
 * @brief Enter low-power sleep mode
 * @param duration_s Sleep duration
 * @return 0 on success
 */
int gf_buoy_sleep(uint32_t duration_s);

/**
 * @brief Shutdown buoy sensor system
 * @return 0 on success
 */
int gf_buoy_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_OCEAN_BUOY_SENSOR_H */
