/**
 * @file retail_energy_optimization.h
 * @brief Retail Energy Optimization Module
 * 
 * INDUSTRY RELEVANCE:
 * Retail stores consume significant energy for HVAC, lighting, and
 * refrigeration. This module enables demand-responsive energy management
 * that balances customer comfort with operational cost reduction and
 * sustainability goals.
 * 
 * TECHNICAL SCOPE:
 * - Occupancy-based HVAC scheduling
 * - Daylight harvesting for lighting
 * - Refrigeration duty cycle optimization
 * - Demand response participation
 * - Solar/battery integration
 * - Carbon footprint tracking
 * 
 * OPTIMIZATION STRATEGIES:
 * - Predictive pre-conditioning
 * - Zone-based temperature setbacks
 * - LED dimming based on daylight
 * - Off-peak refrigeration defrost
 * - Demand limiting during peaks
 * 
 * STANDARDS COMPLIANCE:
 * - ASHRAE 90.1 (Energy efficiency)
 * - OpenADR 2.0 (Demand response)
 * - ENERGY STAR certification
 * - ISO 50001 (Energy management)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_RETAIL_ENERGY_OPTIMIZATION_H
#define GF_RETAIL_ENERGY_OPTIMIZATION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define ENERGY_MAX_HVAC_ZONES   16    /**< HVAC zones */
#define ENERGY_MAX_LIGHTING     32    /**< Lighting circuits */
#define ENERGY_MAX_REFRIG       8     /**< Refrigeration units */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Energy load type */
typedef enum {
    LOAD_HVAC,
    LOAD_LIGHTING,
    LOAD_REFRIGERATION,
    LOAD_EQUIPMENT,
    LOAD_SIGNAGE,
    LOAD_POS
} load_type_t;

/** Demand response level */
typedef enum {
    DR_NORMAL,                     /**< No curtailment */
    DR_MODERATE,                   /**< 10-20% reduction */
    DR_HIGH,                       /**< 20-40% reduction */
    DR_CRITICAL                    /**< Maximum reduction */
} dr_level_t;

/** Optimization mode */
typedef enum {
    OPT_COMFORT,                   /**< Prioritize customer comfort */
    OPT_BALANCED,                  /**< Balance comfort and savings */
    OPT_ECONOMY,                   /**< Prioritize savings */
    OPT_DEMAND_RESPONSE            /**< External DR event active */
} opt_mode_t;

/** Zone status */
typedef struct {
    uint8_t zone_id;
    load_type_t type;
    float power_kw;
    float setpoint;
    float actual;
    float savings_pct;
    bool occupied;
    bool curtailed;
} zone_energy_t;

/** Store energy status */
typedef struct {
    float total_power_kw;
    float hvac_power_kw;
    float lighting_power_kw;
    float refrig_power_kw;
    float other_power_kw;
    float solar_generation_kw;
    float battery_soc_pct;
    float grid_import_kw;
    float demand_peak_kw;
    float savings_today_kwh;
    float carbon_kg_today;
    opt_mode_t mode;
    dr_level_t dr_status;
} store_energy_t;

/** Daily energy summary */
typedef struct {
    float total_consumption_kwh;
    float hvac_kwh;
    float lighting_kwh;
    float refrig_kwh;
    float solar_kwh;
    float savings_kwh;
    float cost_usd;
    float carbon_kg;
    float pue;                     /**< Power Usage Effectiveness */
} daily_energy_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize energy optimization */
int energy_opt_init(void);

/** Set optimization mode */
int energy_set_mode(opt_mode_t mode);

/** Update zone occupancy */
int energy_update_occupancy(uint8_t zone_id, bool occupied, uint32_t count);

/** Set demand response level */
int energy_set_dr_level(dr_level_t level);

/** Get zone energy status */
int energy_get_zone(uint8_t zone_id, zone_energy_t *status);

/** Get store energy status */
int energy_get_store(store_energy_t *status);

/** Get daily summary */
int energy_get_daily(daily_energy_t *summary);

/** Set HVAC setpoint */
int energy_set_hvac(uint8_t zone_id, float setpoint_c);

/** Set lighting level */
int energy_set_lighting(uint8_t zone_id, float level_pct);

/** Process optimization cycle */
int energy_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_RETAIL_ENERGY_OPTIMIZATION_H */
