/**
 * @file solar_panel.h
 * @brief Solar Panel Driver with MPPT Control
 * 
 * INDUSTRY RELEVANCE:
 * Renewable energy systems require sophisticated power extraction algorithms
 * to maximize efficiency. This module demonstrates:
 * - Maximum Power Point Tracking (MPPT) algorithms (P&O, InC, Fuzzy)
 * - I-V curve characterization and monitoring
 * - Panel health monitoring and degradation tracking
 * - Hot-spot detection and bypass diode management
 * 
 * Applications: Solar farms, residential solar, portable chargers, satellites,
 *               IoT sensor nodes, agricultural systems
 * Standards: IEC 61215, IEC 61730, IEEE 1547
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_SOLAR_PANEL_H
#define GF_SOLAR_PANEL_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Solar Panel Types
 ******************************************************************************/

/** MPPT algorithm selection */
typedef enum {
    GF_MPPT_PERTURB_OBSERVE,   /**< Perturb & Observe - simple, robust */
    GF_MPPT_INCREMENTAL_COND,  /**< Incremental Conductance - more accurate */
    GF_MPPT_FUZZY_LOGIC,       /**< Fuzzy logic - adaptive */
    GF_MPPT_NEURAL_NETWORK,    /**< ML-based prediction */
    GF_MPPT_CONSTANT_VOLTAGE   /**< Fixed ratio (fallback) */
} gf_mppt_algorithm_t;

/** Panel state */
typedef enum {
    GF_PANEL_OFF,
    GF_PANEL_STARTING,        /**< Initial characterization */
    GF_PANEL_TRACKING,        /**< MPPT active */
    GF_PANEL_LIMITED,         /**< Output limited (battery full) */
    GF_PANEL_FAULT,           /**< Fault detected */
    GF_PANEL_NIGHT            /**< Insufficient irradiance */
} gf_panel_state_t;

/** Panel fault types */
typedef enum {
    GF_PANEL_FAULT_NONE,
    GF_PANEL_FAULT_HOTSPOT,   /**< Cell overheating */
    GF_PANEL_FAULT_SHORT,     /**< Short circuit detected */
    GF_PANEL_FAULT_OPEN,      /**< Open circuit / disconnection */
    GF_PANEL_FAULT_DEGRADED,  /**< Performance degradation */
    GF_PANEL_FAULT_BYPASS     /**< Bypass diode failure */
} gf_panel_fault_t;

/*******************************************************************************
 * Solar Panel Configuration
 ******************************************************************************/

/** Panel specifications */
typedef struct {
    uint16_t voc_mv;              /**< Open circuit voltage (mV) */
    uint16_t isc_ma;              /**< Short circuit current (mA) */
    uint16_t vmp_mv;              /**< Voltage at max power (mV) */
    uint16_t imp_ma;              /**< Current at max power (mA) */
    uint16_t pmax_mw;             /**< Maximum power (mW) */
    int8_t temp_coeff_pmax;       /**< %/°C temperature coefficient */
    uint8_t cell_count;           /**< Number of cells in series */
    uint8_t bypass_diode_count;   /**< Number of bypass diodes */
} gf_panel_spec_t;

/** MPPT configuration */
typedef struct {
    gf_mppt_algorithm_t algorithm;
    uint16_t perturbation_mv;     /**< P&O step size */
    uint16_t tracking_interval_ms;/**< MPPT update rate */
    uint16_t min_voltage_mv;      /**< Minimum operating voltage */
    uint16_t max_voltage_mv;      /**< Maximum operating voltage */
    uint16_t min_power_mw;        /**< Night mode threshold */
    bool enable_temperature_comp; /**< Temperature compensation */
    bool enable_iv_sweep;         /**< Periodic I-V characterization */
    uint32_t iv_sweep_interval_s; /**< I-V sweep period */
} gf_mppt_config_t;

/** Solar panel configuration */
typedef struct {
    gf_panel_spec_t spec;
    gf_mppt_config_t mppt;
    uint8_t adc_voltage_channel;  /**< ADC channel for voltage */
    uint8_t adc_current_channel;  /**< ADC channel for current */
    uint8_t pwm_channel;          /**< PWM for DC-DC converter */
    uint16_t adc_samples_avg;     /**< ADC averaging count */
    bool enable_hotspot_detect;
    int8_t hotspot_threshold_c;   /**< Hotspot temp delta */
} gf_solar_panel_config_t;

/** Panel reading */
typedef struct {
    uint16_t voltage_mv;          /**< Panel voltage */
    uint16_t current_ma;          /**< Panel current */
    uint32_t power_mw;            /**< Instantaneous power */
    uint16_t mpp_voltage_mv;      /**< Tracked MPP voltage */
    uint16_t mpp_current_ma;      /**< Tracked MPP current */
    uint32_t energy_mwh;          /**< Accumulated energy */
    int8_t temperature_c;         /**< Panel temperature */
    uint8_t efficiency_pct;       /**< Current efficiency */
    gf_panel_state_t state;
    gf_panel_fault_t fault;
} gf_solar_panel_reading_t;

/*******************************************************************************
 * Solar Panel Statistics
 ******************************************************************************/

typedef struct {
    uint64_t total_energy_mwh;    /**< Lifetime energy */
    uint32_t peak_power_mw;       /**< Record peak power */
    uint32_t tracking_cycles;     /**< MPPT iterations */
    uint32_t iv_sweeps;           /**< I-V characterizations */
    uint32_t hotspot_events;
    uint32_t fault_count;
    uint32_t operating_hours;
    float avg_efficiency;
    float degradation_pct;        /**< Estimated degradation */
} gf_solar_panel_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize solar panel driver
 * @param config Panel configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_solar_panel_init(const gf_solar_panel_config_t *config);

/**
 * @brief Start MPPT tracking
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_solar_panel_start(void);

/**
 * @brief Stop MPPT tracking
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_solar_panel_stop(void);

/**
 * @brief Read current panel state
 * @param reading Output reading
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_solar_panel_read(gf_solar_panel_reading_t *reading);

/**
 * @brief Run MPPT algorithm iteration
 * @return New MPP voltage in mV
 */
uint16_t gf_solar_panel_mppt_step(void);

/**
 * @brief Perform I-V curve sweep
 * @param v_points Output voltage points
 * @param i_points Output current points
 * @param max_points Maximum points to record
 * @return Actual points recorded
 */
uint16_t gf_solar_panel_iv_sweep(uint16_t *v_points, uint16_t *i_points,
                                  uint16_t max_points);

/**
 * @brief Set power output limit
 * @param max_power_mw Maximum power to extract
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_solar_panel_set_limit(uint32_t max_power_mw);

/**
 * @brief Get panel statistics
 * @return Current statistics
 */
gf_solar_panel_stats_t gf_solar_panel_get_stats(void);

/**
 * @brief Shutdown solar panel driver
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_solar_panel_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SOLAR_PANEL_H */
