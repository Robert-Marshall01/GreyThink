/**
 * @file smart_oven_driver.h
 * @brief Smart Oven Control Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Commercial smart kitchens and food service automation require precise
 * temperature control, multi-zone cooking, and IoT connectivity. This driver
 * enables intelligent cooking with recipe automation, energy optimization,
 * and HACCP compliance logging.
 * 
 * TECHNICAL SCOPE:
 * - Multi-zone temperature control (±0.5°C precision)
 * - Convection, steam, and combination cooking modes
 * - Recipe-driven cooking profiles
 * - Safety interlocks (door, temperature, smoke)
 * - Energy consumption monitoring
 * - Cloud connectivity for recipe updates
 * 
 * SAFETY FEATURES:
 * - Thermal runaway protection
 * - Door interlock with cooking pause
 * - Smoke detection with automatic shutdown
 * - Child lock functionality
 * 
 * STANDARDS COMPLIANCE:
 * - IEC 60335-2-6 (Cooking range safety)
 * - NSF/ANSI 4 (Commercial cooking equipment)
 * - UL 197 (Commercial electric cooking)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_SMART_OVEN_DRIVER_H
#define GF_SMART_OVEN_DRIVER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define OVEN_MAX_ZONES          4     /**< Maximum heating zones */
#define OVEN_MAX_RECIPES        64    /**< Recipe storage capacity */
#define OVEN_TEMP_MAX_C         300   /**< Maximum temperature */
#define OVEN_TEMP_MIN_C         30    /**< Minimum temperature */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Oven operating mode */
typedef enum {
    OVEN_MODE_OFF,
    OVEN_MODE_PREHEAT,
    OVEN_MODE_BAKE,
    OVEN_MODE_CONVECTION,
    OVEN_MODE_BROIL,
    OVEN_MODE_STEAM,
    OVEN_MODE_COMBINATION,
    OVEN_MODE_SELF_CLEAN,
    OVEN_MODE_PROOF
} oven_mode_t;

/** Heating element type */
typedef enum {
    OVEN_ELEMENT_TOP,
    OVEN_ELEMENT_BOTTOM,
    OVEN_ELEMENT_CONVECTION,
    OVEN_ELEMENT_STEAM_GEN,
    OVEN_ELEMENT_BROIL
} oven_element_t;

/** Cooking status */
typedef enum {
    OVEN_STATUS_IDLE,
    OVEN_STATUS_PREHEATING,
    OVEN_STATUS_COOKING,
    OVEN_STATUS_HOLDING,
    OVEN_STATUS_COOLING,
    OVEN_STATUS_ERROR
} oven_status_t;

/** Zone configuration */
typedef struct {
    float target_temp_c;
    float current_temp_c;
    oven_mode_t mode;
    uint32_t cook_time_remaining_s;
    bool element_on[5];
    float power_pct;
} oven_zone_t;

/** Overall oven status */
typedef struct {
    oven_status_t status;
    oven_zone_t zones[OVEN_MAX_ZONES];
    bool door_open;
    bool door_locked;
    float energy_kwh;
    float steam_level_pct;
    uint32_t uptime_hours;
    uint8_t active_recipe_id;
} oven_status_struct_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize smart oven driver */
int oven_init(void);

/** Set zone temperature and mode */
int oven_set_zone(uint8_t zone, float temp_c, oven_mode_t mode);

/** Start cooking with timer */
int oven_start_cook(uint8_t zone, uint32_t duration_s);

/** Stop cooking */
int oven_stop(uint8_t zone);

/** Load recipe profile */
int oven_load_recipe(uint8_t recipe_id);

/** Get oven status */
int oven_get_status(oven_status_struct_t *status);

/** Process control loop */
int oven_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_SMART_OVEN_DRIVER_H */
