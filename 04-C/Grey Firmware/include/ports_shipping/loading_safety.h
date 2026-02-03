/**
 * @file loading_safety.h
 * @brief Safety Interlock for Automated Container Loading
 * 
 * INDUSTRY RELEVANCE:
 * Automated port operations require SIL-rated safety systems to protect
 * workers and prevent equipment damage. This module demonstrates:
 * - Safety-rated interlock logic per IEC 62443 / IEC 61508
 * - Zone-based collision avoidance for automated vehicles
 * - Emergency stop coordination across multiple subsystems
 * - Lockout/tagout (LOTO) integration for maintenance
 * 
 * Target applications: Automated guided vehicles (AGVs), automated stacking
 * cranes, quay-side automation, terminal safety systems.
 * 
 * Standards: IEC 61508 SIL-2/3, ISO 3691-4 (AGV safety), OSHA 1910.147 (LOTO)
 */

#ifndef GF_LOADING_SAFETY_H
#define GF_LOADING_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Safety Interlock Types                                                     */
/*===========================================================================*/

typedef enum {
    SAFETY_ZONE_QUAY,       /* Quay-side (ship loading) */
    SAFETY_ZONE_YARD,       /* Container yard */
    SAFETY_ZONE_GATE,       /* Terminal gate area */
    SAFETY_ZONE_RAIL,       /* Rail loading area */
    SAFETY_ZONE_COUNT
} safety_zone_t;

typedef enum {
    SAFETY_STATE_NORMAL,    /* Normal operation */
    SAFETY_STATE_WARNING,   /* Warning condition */
    SAFETY_STATE_RESTRICTED,/* Restricted operation */
    SAFETY_STATE_ESTOP,     /* Emergency stop active */
    SAFETY_STATE_LOCKOUT    /* Maintenance lockout */
} safety_state_t;

typedef enum {
    INTERLOCK_CRANE,        /* Crane in motion */
    INTERLOCK_AGV,          /* AGV in zone */
    INTERLOCK_PERSONNEL,    /* Personnel detected */
    INTERLOCK_VESSEL,       /* Vessel movement */
    INTERLOCK_WEATHER,      /* High wind/visibility */
    INTERLOCK_FIRE,         /* Fire alarm active */
    INTERLOCK_COUNT
} interlock_type_t;

typedef struct {
    safety_zone_t zone;
    safety_state_t state;
    uint32_t active_interlocks; /* Bitmap of active interlocks */
    uint32_t personnel_count;   /* Workers in zone */
    bool estop_pressed;
    bool loto_active;
    uint32_t last_clear_time;   /* Last safety clear timestamp */
} zone_safety_state_t;

typedef struct {
    bool require_two_hand;  /* Two-hand control required */
    bool require_scanner;   /* Area scanner required */
    float max_wind_speed;   /* Maximum wind speed (m/s) */
    uint32_t clear_timeout; /* Auto-clear timeout (s) */
} safety_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int loading_safety_init(void);
int loading_safety_shutdown(void);

int safety_set_config(safety_zone_t zone, const safety_config_t* config);
int safety_get_zone_state(safety_zone_t zone, zone_safety_state_t* state);

int safety_set_interlock(safety_zone_t zone, interlock_type_t interlock, bool active);
int safety_clear_zone(safety_zone_t zone, uint32_t operator_id);

int safety_request_entry(safety_zone_t zone, uint32_t personnel_id);
int safety_confirm_exit(safety_zone_t zone, uint32_t personnel_id);

int safety_activate_estop(safety_zone_t zone);
int safety_reset_estop(safety_zone_t zone, uint32_t operator_id);

int safety_activate_loto(safety_zone_t zone, uint32_t operator_id);
int safety_release_loto(safety_zone_t zone, uint32_t operator_id);

bool safety_is_operation_permitted(safety_zone_t zone);

#endif /* GF_LOADING_SAFETY_H */
