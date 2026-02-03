/**
 * @file nutrient_control.h
 * @brief Nutrient Delivery Control Module for Vertical Farms
 *
 * INDUSTRY RELEVANCE:
 * Automated nutrient delivery ensures consistent crop quality while
 * minimizing waste and labor costs. Precise dosing based on real-time
 * sensor feedback enables optimal plant nutrition throughout growth stages.
 *
 * MARKET CONTEXT:
 * - Commercial vertical farm automation systems
 * - Greenhouse fertigation control
 * - Research facility growth chambers
 * - Home hydroponic systems (Rise Gardens, AeroGarden)
 * - Aquaponics nutrient balancing
 *
 * TECHNICAL APPROACH:
 * - PID control for pH and EC regulation
 * - Multi-pump dosing system coordination
 * - Reservoir management and auto-fill
 * - Growth stage recipe management
 * - Fault detection for pump and sensor failures
 *
 * @author Grey Firmware Project
 */

#ifndef GF_NUTRIENT_CONTROL_H
#define GF_NUTRIENT_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Nutrient pump identifiers
 */
typedef enum {
    PUMP_PH_UP,
    PUMP_PH_DOWN,
    PUMP_NUTRIENT_A,
    PUMP_NUTRIENT_B,
    PUMP_CALCIUM,
    PUMP_MAGNESIUM,
    PUMP_MICRONUTRIENTS,
    PUMP_MAX
} gf_nutrient_pump_t;

/**
 * @brief Growth stage for recipe selection
 */
typedef enum {
    STAGE_GERMINATION,
    STAGE_SEEDLING,
    STAGE_VEGETATIVE,
    STAGE_FLOWERING,
    STAGE_FRUITING,
    STAGE_HARVEST
} gf_growth_stage_t;

/**
 * @brief Nutrient recipe parameters
 */
typedef struct {
    gf_growth_stage_t stage;
    float target_ph;
    float target_ec;
    float nutrient_ratio[PUMP_MAX];
    uint16_t dose_interval_min;
    bool auto_adjust;
} gf_nutrient_recipe_t;

/**
 * @brief Dosing status
 */
typedef struct {
    bool pump_active[PUMP_MAX];
    float total_dosed_ml[PUMP_MAX];
    float current_ph;
    float current_ec;
    uint32_t last_dose_time;
    uint8_t fault_flags;
} gf_dosing_status_t;

/* Function prototypes */
int gf_nutrient_init(void);
int gf_nutrient_set_recipe(const gf_nutrient_recipe_t *recipe);
int gf_nutrient_dose(gf_nutrient_pump_t pump, float ml);
int gf_nutrient_auto_adjust(void);
int gf_nutrient_get_status(gf_dosing_status_t *status);
int gf_nutrient_prime_pump(gf_nutrient_pump_t pump);
int gf_nutrient_flush_system(void);
void gf_nutrient_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_NUTRIENT_CONTROL_H */
