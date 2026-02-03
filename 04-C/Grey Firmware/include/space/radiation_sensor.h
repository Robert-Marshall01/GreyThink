/**
 * @file radiation_sensor.h
 * @brief Radiation-Hardened Sensor Interface for Space Systems
 * 
 * INDUSTRY RELEVANCE:
 * Space-grade firmware requires radiation-tolerant designs for satellite,
 * deep-space probe, and ISS applications. This module demonstrates:
 * - Total Ionizing Dose (TID) monitoring
 * - Single Event Upset (SEU) detection and mitigation
 * - Latchup current monitoring for rad-hard electronics
 * - Triple Modular Redundancy (TMR) voting patterns
 * 
 * Applications: CubeSats, GEO/LEO satellites, Mars rovers, lunar landers
 * Standards: ECSS-E-ST-50C, MIL-HDBK-814, NASA-STD-8739.4
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_RADIATION_SENSOR_H
#define GF_RADIATION_SENSOR_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Radiation Environment Types
 ******************************************************************************/

/** Radiation environment classification */
typedef enum {
    GF_RAD_ENV_LEO,           /**< Low Earth Orbit (Van Allen belt exposure) */
    GF_RAD_ENV_GEO,           /**< Geosynchronous (full belt transit) */
    GF_RAD_ENV_LUNAR,         /**< Cislunar space */
    GF_RAD_ENV_DEEP_SPACE,    /**< Interplanetary (solar wind + GCR) */
    GF_RAD_ENV_JUPITER        /**< High-radiation jovian environment */
} gf_rad_environment_t;

/** Radiation event types */
typedef enum {
    GF_RAD_EVENT_NONE,
    GF_RAD_EVENT_SEU,         /**< Single Event Upset (bit flip) */
    GF_RAD_EVENT_SET,         /**< Single Event Transient */
    GF_RAD_EVENT_SEL,         /**< Single Event Latchup (critical) */
    GF_RAD_EVENT_SEFI,        /**< Single Event Functional Interrupt */
    GF_RAD_EVENT_TID_WARN,    /**< Total Ionizing Dose warning */
    GF_RAD_EVENT_TID_CRIT     /**< TID critical threshold */
} gf_rad_event_t;

/** Radiation hardening strategy */
typedef enum {
    GF_RAD_HARD_NONE,
    GF_RAD_HARD_TMR,          /**< Triple Modular Redundancy */
    GF_RAD_HARD_EDAC,         /**< Error Detection and Correction */
    GF_RAD_HARD_SCRUBBING,    /**< Memory scrubbing */
    GF_RAD_HARD_WATCHDOG      /**< Radiation-triggered watchdog */
} gf_rad_hardening_t;

/*******************************************************************************
 * Radiation Sensor Configuration
 ******************************************************************************/

/** Radiation sensor configuration */
typedef struct {
    gf_rad_environment_t environment;
    uint32_t tid_warn_threshold_rad;   /**< TID warning (rads) */
    uint32_t tid_crit_threshold_rad;   /**< TID critical (rads) */
    uint16_t seu_rate_threshold;       /**< SEU/hour warning */
    uint16_t latchup_current_ma;       /**< Latchup detect current */
    gf_rad_hardening_t hardening;
    bool enable_tmr_voting;
    bool enable_memory_scrub;
    uint32_t scrub_interval_ms;
} gf_rad_sensor_config_t;

/** Radiation sensor reading */
typedef struct {
    uint32_t total_dose_rad;           /**< Accumulated TID (rads) */
    uint16_t dose_rate_mrad_hr;        /**< Current dose rate (mrad/hr) */
    uint16_t seu_count;                /**< SEU events detected */
    uint16_t set_count;                /**< SET events detected */
    uint8_t latchup_count;             /**< Latchup events */
    int8_t temperature_c;              /**< Sensor temperature */
    uint32_t uptime_seconds;           /**< Mission elapsed time */
    gf_rad_event_t last_event;
} gf_rad_sensor_reading_t;

/** TMR voting result */
typedef struct {
    uint32_t value;
    uint8_t agreement_count;           /**< 3=unanimous, 2=majority */
    bool correction_applied;
    uint8_t disagreeing_channel;
} gf_tmr_vote_result_t;

/*******************************************************************************
 * Radiation Sensor Statistics
 ******************************************************************************/

typedef struct {
    uint32_t total_seu_events;
    uint32_t total_set_events;
    uint32_t total_sel_events;
    uint32_t tmr_corrections;
    uint32_t edac_corrections;
    uint32_t scrub_passes;
    uint32_t mission_dose_rad;
    uint32_t peak_dose_rate;
} gf_rad_sensor_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize radiation sensor subsystem
 * @param config Sensor configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_rad_sensor_init(const gf_rad_sensor_config_t *config);

/**
 * @brief Read current radiation environment
 * @param reading Output reading structure
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_rad_sensor_read(gf_rad_sensor_reading_t *reading);

/**
 * @brief Perform TMR voting on value
 * @param chan0 Channel 0 value
 * @param chan1 Channel 1 value
 * @param chan2 Channel 2 value
 * @param result Voting result
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_rad_tmr_vote(uint32_t chan0, uint32_t chan1, uint32_t chan2,
                           gf_tmr_vote_result_t *result);

/**
 * @brief Trigger memory scrub cycle
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_rad_memory_scrub(void);

/**
 * @brief Register SEU event callback
 * @param callback Function called on SEU detection
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_rad_register_seu_callback(void (*callback)(gf_rad_event_t event));

/**
 * @brief Handle latchup event (power cycle affected component)
 * @param component_id Component experiencing latchup
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_rad_handle_latchup(uint8_t component_id);

/**
 * @brief Get radiation statistics
 * @return Current statistics
 */
gf_rad_sensor_stats_t gf_rad_sensor_get_stats(void);

/**
 * @brief Shutdown radiation sensor
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_rad_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_RADIATION_SENSOR_H */
