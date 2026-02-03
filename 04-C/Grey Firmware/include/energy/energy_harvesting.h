/**
 * @file energy_harvesting.h
 * @brief Energy Harvesting Manager with Battery Integration
 * 
 * INDUSTRY RELEVANCE:
 * Battery-powered and energy-autonomous systems require sophisticated
 * power harvesting strategies. This module demonstrates:
 * - Multi-source energy harvesting (solar, thermal, kinetic, RF)
 * - Battery charge management with protection circuitry
 * - Power budget allocation and load shedding
 * - Energy prediction for autonomous operation planning
 * 
 * Applications: Wireless sensors, wearables, remote monitoring,
 *               industrial IoT, environmental sensing, asset tracking
 * Standards: IEEE 802.3bt (PoE++), Qi wireless charging
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_ENERGY_HARVESTING_H
#define GF_ENERGY_HARVESTING_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Energy Harvesting Types
 ******************************************************************************/

/** Energy source types */
typedef enum {
    GF_HARVEST_SOLAR,         /**< Photovoltaic */
    GF_HARVEST_THERMAL,       /**< Thermoelectric (TEG) */
    GF_HARVEST_KINETIC,       /**< Piezoelectric/electromagnetic */
    GF_HARVEST_RF,            /**< RF energy harvesting */
    GF_HARVEST_WIND,          /**< Micro wind turbine */
    GF_HARVEST_GRID           /**< AC mains (backup) */
} gf_harvest_source_t;

/** Battery chemistry types */
typedef enum {
    GF_BATT_LIION,            /**< Lithium-ion */
    GF_BATT_LIPO,             /**< Lithium polymer */
    GF_BATT_LIFEPO4,          /**< Lithium iron phosphate */
    GF_BATT_NIMH,             /**< Nickel-metal hydride */
    GF_BATT_SUPERCAP,         /**< Supercapacitor */
    GF_BATT_SOLID_STATE       /**< Solid-state lithium */
} gf_battery_chemistry_t;

/** Charging state */
typedef enum {
    GF_CHARGE_OFF,
    GF_CHARGE_PRECHARGE,      /**< Low-current trickle */
    GF_CHARGE_CC,             /**< Constant current */
    GF_CHARGE_CV,             /**< Constant voltage */
    GF_CHARGE_TOPOFF,         /**< Top-off phase */
    GF_CHARGE_COMPLETE,       /**< Fully charged */
    GF_CHARGE_FAULT           /**< Charging fault */
} gf_charge_state_t;

/** Power budget state */
typedef enum {
    GF_POWER_CRITICAL,        /**< Emergency power saving */
    GF_POWER_LOW,             /**< Reduced functionality */
    GF_POWER_NORMAL,          /**< Full operation */
    GF_POWER_SURPLUS          /**< Energy available for storage */
} gf_power_budget_t;

/*******************************************************************************
 * Energy Harvesting Configuration
 ******************************************************************************/

/** Harvest source configuration */
typedef struct {
    gf_harvest_source_t type;
    uint16_t max_power_mw;        /**< Peak harvestable power */
    uint16_t min_voltage_mv;      /**< Minimum useful voltage */
    uint16_t max_voltage_mv;      /**< Maximum safe voltage */
    uint8_t efficiency_pct;       /**< Conversion efficiency */
    bool is_intermittent;         /**< True for solar, kinetic */
    uint8_t priority;             /**< Source priority (lower = higher) */
} gf_harvest_source_config_t;

/** Battery configuration */
typedef struct {
    gf_battery_chemistry_t chemistry;
    uint16_t capacity_mah;        /**< Rated capacity */
    uint16_t nominal_voltage_mv;  /**< Nominal voltage */
    uint16_t charge_voltage_mv;   /**< Full charge voltage */
    uint16_t cutoff_voltage_mv;   /**< Discharge cutoff */
    uint16_t max_charge_ma;       /**< Maximum charge current */
    uint16_t max_discharge_ma;    /**< Maximum discharge current */
    int8_t min_temp_c;            /**< Operating temp min */
    int8_t max_temp_c;            /**< Operating temp max */
    uint16_t cycle_count_max;     /**< Expected cycle life */
} gf_battery_config_t;

/** Energy harvesting manager configuration */
typedef struct {
    gf_harvest_source_config_t sources[4];
    uint8_t source_count;
    gf_battery_config_t battery;
    uint16_t system_load_mw_avg;  /**< Average system load */
    uint16_t system_load_mw_peak; /**< Peak system load */
    uint8_t low_battery_pct;      /**< Low battery threshold */
    uint8_t critical_battery_pct; /**< Critical threshold */
    bool enable_load_shedding;    /**< Auto load management */
    bool enable_prediction;       /**< ML-based prediction */
    uint32_t telemetry_interval_ms;
} gf_energy_manager_config_t;

/** Energy status reading */
typedef struct {
    /* Harvesting status */
    uint16_t harvest_power_mw;    /**< Current harvest rate */
    gf_harvest_source_t active_source;
    
    /* Battery status */
    uint16_t battery_voltage_mv;
    int16_t battery_current_ma;   /**< Positive = charging */
    uint8_t battery_soc_pct;      /**< State of charge */
    uint8_t battery_soh_pct;      /**< State of health */
    int8_t battery_temp_c;
    gf_charge_state_t charge_state;
    
    /* Power budget */
    gf_power_budget_t budget_state;
    int16_t power_balance_mw;     /**< Harvest - load */
    uint32_t runtime_remaining_min;/**< Estimated runtime */
    
    /* System */
    uint16_t system_load_mw;
    uint32_t energy_harvested_mwh;
    uint32_t energy_consumed_mwh;
} gf_energy_status_t;

/*******************************************************************************
 * Energy Harvesting Statistics
 ******************************************************************************/

typedef struct {
    uint64_t total_harvested_mwh;
    uint64_t total_consumed_mwh;
    uint32_t charge_cycles;
    uint32_t load_shed_events;
    uint32_t critical_events;
    uint32_t source_switchovers;
    uint32_t uptime_hours;
    float avg_harvest_efficiency;
    float battery_degradation_pct;
    uint32_t harvest_by_source[6]; /**< mWh per source type */
} gf_energy_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize energy harvesting manager
 * @param config Manager configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_init(const gf_energy_manager_config_t *config);

/**
 * @brief Start energy harvesting
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_start(void);

/**
 * @brief Stop energy harvesting
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_stop(void);

/**
 * @brief Get current energy status
 * @param status Output status
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_get_status(gf_energy_status_t *status);

/**
 * @brief Process energy harvesting (call from scheduler)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_process(void);

/**
 * @brief Request power budget
 * @param power_mw Requested power
 * @return Granted power in mW
 */
uint16_t gf_energy_request_power(uint16_t power_mw);

/**
 * @brief Release allocated power budget
 * @param power_mw Power to release
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_release_power(uint16_t power_mw);

/**
 * @brief Force source selection
 * @param source Source to activate
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_select_source(gf_harvest_source_t source);

/**
 * @brief Get energy prediction
 * @param hours_ahead Prediction horizon
 * @return Predicted harvestable energy (mWh)
 */
uint32_t gf_energy_predict(uint8_t hours_ahead);

/**
 * @brief Register load shed callback
 * @param callback Called when load shedding needed
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_register_loadshed_callback(
    void (*callback)(gf_power_budget_t state));

/**
 * @brief Get energy statistics
 * @return Current statistics
 */
gf_energy_stats_t gf_energy_get_stats(void);

/**
 * @brief Shutdown energy manager
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_energy_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ENERGY_HARVESTING_H */
