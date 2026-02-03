/**
 * @file shielding_control.h
 * @brief Dynamic Radiation Shielding Control Module
 * 
 * INDUSTRY RELEVANCE:
 * Future spacecraft and lunar habitats will employ active and reconfigurable
 * shielding systems. Water walls, hydrogen-rich polymers, and electromagnetic
 * deflectors can be dynamically adjusted based on radiation environment.
 * This module enables intelligent shielding optimization.
 * 
 * TECHNICAL SCOPE:
 * - Multi-zone shielding management (crew quarters, command, EVA airlock)
 * - Active water redistribution for storm shelters
 * - Electromagnetic deflector control (for charged particles)
 * - Material degradation tracking
 * - Power-optimized shielding schedules
 * 
 * SHIELDING STRATEGIES:
 * - Passive: Polyethylene, water, regolith bags
 * - Active: Superconducting magnets, electrostatic deflectors
 * - Hybrid: Configurable water walls with magnetic augmentation
 * 
 * STANDARDS COMPLIANCE:
 * - NASA-STD-3001 Vol. 2 (Habitability)
 * - NASA/TP-2020-220002 (Radiation protection)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_SHIELDING_CONTROL_H
#define GF_SHIELDING_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define SHIELD_MAX_ZONES        16    /**< Maximum shielding zones */
#define SHIELD_MAX_ACTUATORS    32    /**< Maximum actuators */
#define SHIELD_HISTORY_SIZE     256   /**< Configuration history */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Shielding zone type */
typedef enum {
    SHIELD_ZONE_CREW_QUARTERS,
    SHIELD_ZONE_COMMAND,
    SHIELD_ZONE_GALLEY,
    SHIELD_ZONE_MEDICAL,
    SHIELD_ZONE_AIRLOCK,
    SHIELD_ZONE_STORM_SHELTER,
    SHIELD_ZONE_CARGO
} shield_zone_t;

/** Shielding mechanism type */
typedef enum {
    SHIELD_TYPE_PASSIVE_POLY,      /**< Polyethylene panels */
    SHIELD_TYPE_WATER_WALL,        /**< Configurable water bags */
    SHIELD_TYPE_MAGNETIC,          /**< Superconducting magnet */
    SHIELD_TYPE_ELECTROSTATIC,     /**< Electrostatic deflector */
    SHIELD_TYPE_REGOLITH           /**< In-situ regolith bags */
} shield_type_t;

/** Shield configuration mode */
typedef enum {
    SHIELD_MODE_NOMINAL,           /**< Normal operation */
    SHIELD_MODE_ENHANCED,          /**< Elevated threat response */
    SHIELD_MODE_STORM_SHELTER,     /**< Maximum protection */
    SHIELD_MODE_EVA_SUPPORT,       /**< Airlock priority */
    SHIELD_MODE_POWER_SAVE         /**< Minimum active shielding */
} shield_mode_t;

/** Zone shielding status */
typedef struct {
    shield_zone_t zone;
    float areal_density_gcm2;      /**< g/cm² shielding thickness */
    float attenuation_factor;      /**< Dose reduction factor */
    float power_consumption_w;     /**< Active shield power draw */
    shield_mode_t mode;
    bool actuator_healthy;
    uint32_t reconfiguration_count;
} shield_zone_status_t;

/** Overall shielding status */
typedef struct {
    shield_mode_t current_mode;
    float total_power_w;
    float crew_protection_factor;
    uint8_t zones_configured;
    uint8_t actuators_active;
    bool storm_shelter_ready;
    uint32_t uptime_hours;
} shield_system_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize shielding control system */
int shield_init(void);

/** Configure a shielding zone */
int shield_zone_config(uint8_t zone_id, shield_zone_t zone, shield_type_t type);

/** Set system-wide shielding mode */
int shield_set_mode(shield_mode_t mode);

/** Get zone status */
int shield_get_zone_status(uint8_t zone_id, shield_zone_status_t *status);

/** Get overall system status */
int shield_get_status(shield_system_status_t *status);

/** Activate storm shelter mode */
int shield_activate_storm_shelter(void);

/** Process shielding control loop */
int shield_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_SHIELDING_CONTROL_H */
