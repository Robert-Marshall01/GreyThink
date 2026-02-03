/**
 * @file ocean_current.h
 * @brief Ocean Current Telemetry - Smart Ocean Monitoring
 * 
 * @details Industry Relevance:
 * Understanding ocean currents is essential for:
 * - Navigation and voyage optimization (fuel savings)
 * - Search and rescue operations (drift prediction)
 * - Oil spill trajectory modeling
 * - Marine debris tracking
 * - Offshore structure design (loads from currents)
 * - Climate science (thermohaline circulation)
 * 
 * Measurement technologies:
 * - ADCP (Acoustic Doppler Current Profiler): Full water column profiles
 * - HF Radar: Surface currents over large areas
 * - Drifters: Lagrangian current trackers
 * - Satellite altimetry: Derived geostrophic currents
 * 
 * Standards: NOAA/NDBC formats, WMO/IOC specifications, NetCDF conventions
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_OCEAN_CURRENT_H
#define GF_OCEAN_CURRENT_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum depth bins for ADCP */
#define GF_CURRENT_MAX_BINS             128

/** Default bin size (meters) */
#define GF_CURRENT_BIN_SIZE_M           4

/** Sample averaging period (seconds) */
#define GF_CURRENT_AVERAGE_PERIOD_S     600

/** Minimum correlation for valid velocity */
#define GF_CURRENT_MIN_CORRELATION      64

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief ADCP configuration
 */
typedef struct {
    float frequency_khz;            /**< Acoustic frequency */
    uint8_t bin_count;              /**< Number of depth bins */
    float bin_size_m;               /**< Bin size in meters */
    float first_bin_m;              /**< First bin depth */
    float blank_m;                  /**< Blanking distance */
    uint16_t pings_per_ensemble;    /**< Pings to average */
    uint16_t ensemble_interval_s;   /**< Ensemble rate */
    bool bottom_track;              /**< Bottom tracking enabled */
} gf_current_config_t;

/**
 * @brief Single depth bin velocity
 */
typedef struct {
    float depth_m;                  /**< Bin center depth */
    float east_ms;                  /**< Eastward velocity */
    float north_ms;                 /**< Northward velocity */
    float up_ms;                    /**< Vertical velocity */
    float error_ms;                 /**< Error velocity */
    float speed_ms;                 /**< Horizontal speed */
    float direction_deg;            /**< Current direction (going to) */
    uint8_t correlation;            /**< Echo correlation (0-255) */
    uint8_t amplitude;              /**< Echo amplitude */
    bool valid;                     /**< Velocity valid */
} gf_current_bin_t;

/**
 * @brief Current profile (ensemble)
 */
typedef struct {
    uint32_t timestamp;             /**< Ensemble timestamp */
    uint32_t ensemble_num;          /**< Ensemble number */
    float heading_deg;              /**< Instrument heading */
    float pitch_deg;                /**< Instrument pitch */
    float roll_deg;                 /**< Instrument roll */
    float temperature_c;            /**< Transducer temperature */
    float pressure_dbar;            /**< Pressure (depth) */
    float salinity_psu;             /**< Salinity for sound speed */
    uint8_t bin_count;              /**< Valid bins */
    gf_current_bin_t bins[GF_CURRENT_MAX_BINS];
} gf_current_profile_t;

/**
 * @brief Bottom track data
 */
typedef struct {
    float range_m[4];               /**< Range to bottom (4 beams) */
    float velocity_east_ms;         /**< Bottom-track east */
    float velocity_north_ms;        /**< Bottom-track north */
    float velocity_up_ms;           /**< Bottom-track vertical */
    bool valid;                     /**< Bottom track valid */
} gf_current_bottom_t;

/**
 * @brief Surface current from HF radar
 */
typedef struct {
    double latitude_deg;            /**< Grid point latitude */
    double longitude_deg;           /**< Grid point longitude */
    float u_ms;                     /**< Eastward component */
    float v_ms;                     /**< Northward component */
    float speed_ms;                 /**< Current speed */
    float direction_deg;            /**< Current direction */
    float uncertainty_ms;           /**< Measurement uncertainty */
    uint32_t timestamp;             /**< Observation time */
} gf_current_surface_t;

/**
 * @brief Drifter position/velocity
 */
typedef struct {
    char drifter_id[16];            /**< Drifter identifier */
    double latitude_deg;            /**< Position latitude */
    double longitude_deg;           /**< Position longitude */
    float u_ms;                     /**< Eastward velocity */
    float v_ms;                     /**< Northward velocity */
    float sst_c;                    /**< SST measurement */
    float battery_v;                /**< Battery voltage */
    uint32_t timestamp;             /**< Position time */
} gf_current_drifter_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize ocean current measurement
 * @param config ADCP configuration (NULL for defaults)
 * @return 0 on success, negative error code on failure
 */
int gf_current_init(const gf_current_config_t* config);

/**
 * @brief Process current measurements (call periodically)
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int gf_current_process(uint32_t delta_ms);

/**
 * @brief Get latest current profile
 * @param profile Output profile structure
 * @return 0 on success
 */
int gf_current_get_profile(gf_current_profile_t* profile);

/**
 * @brief Get bottom track data
 * @param bottom Output bottom track structure
 * @return 0 on success
 */
int gf_current_get_bottom(gf_current_bottom_t* bottom);

/**
 * @brief Get velocity at specific depth
 * @param depth_m Depth to query
 * @param bin Output bin structure
 * @return 0 on success
 */
int gf_current_get_at_depth(float depth_m, gf_current_bin_t* bin);

/**
 * @brief Get depth-averaged current
 * @param min_depth_m Minimum depth
 * @param max_depth_m Maximum depth
 * @param avg Output average velocity
 * @return 0 on success
 */
int gf_current_get_average(float min_depth_m,
                           float max_depth_m,
                           gf_current_bin_t* avg);

/**
 * @brief Process drifter GPS report
 * @param drifter Drifter telemetry
 * @return 0 on success
 */
int gf_current_process_drifter(const gf_current_drifter_t* drifter);

/**
 * @brief Set sound speed profile for ADCP correction
 * @param depths Depth array (meters)
 * @param speeds Sound speed array (m/s)
 * @param count Number of points
 * @return 0 on success
 */
int gf_current_set_sound_speed(const float* depths,
                                const float* speeds,
                                uint8_t count);

/**
 * @brief Trigger ping sequence (for manual control)
 * @return 0 on success
 */
int gf_current_trigger_ping(void);

/**
 * @brief Shutdown current measurement
 * @return 0 on success
 */
int gf_current_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_OCEAN_CURRENT_H */
