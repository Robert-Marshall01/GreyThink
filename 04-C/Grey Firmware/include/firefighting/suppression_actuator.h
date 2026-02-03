/**
 * @file suppression_actuator.h
 * @brief Fire Suppression Actuator Control for Smart Firefighting Systems
 * 
 * INDUSTRY RELEVANCE:
 * Automated fire suppression protects data centers, industrial facilities,
 * and heritage sites. Modern systems require:
 * - Precise agent delivery (water, foam, gas, powder)
 * - Zone-based activation for targeted response
 * - Pre-action and deluge system coordination
 * - Integration with building management systems (BMS)
 * 
 * Target applications: Data center fire protection, industrial suppression,
 * aircraft hangars, marine engine rooms, museum/archive protection.
 * 
 * Standards: NFPA 13 (sprinkler), NFPA 17 (foam), NFPA 2001 (clean agent),
 *            FM Global, EN 12845 (European sprinkler)
 */

#ifndef GF_SUPPRESSION_ACTUATOR_H
#define GF_SUPPRESSION_ACTUATOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Suppression Actuator Types                                                 */
/*===========================================================================*/

typedef enum {
    AGENT_WATER,            /* Standard water spray */
    AGENT_FOAM,             /* AFFF/AR-AFFF foam */
    AGENT_CO2,              /* Carbon dioxide */
    AGENT_FM200,            /* FM-200 (HFC-227ea) */
    AGENT_NOVEC1230,        /* 3M Novec 1230 */
    AGENT_INERGEN,          /* Inert gas blend */
    AGENT_POWDER,           /* Dry chemical powder */
    AGENT_WATER_MIST        /* High-pressure water mist */
} suppression_agent_t;

typedef enum {
    SYSTEM_WET_PIPE,        /* Wet pipe sprinkler */
    SYSTEM_DRY_PIPE,        /* Dry pipe (freeze protection) */
    SYSTEM_PREACTION,       /* Pre-action (double interlock) */
    SYSTEM_DELUGE,          /* Deluge (open heads) */
    SYSTEM_TOTAL_FLOOD,     /* Total flooding (gas) */
    SYSTEM_LOCAL_APP        /* Local application */
} system_type_t;

typedef enum {
    VALVE_CLOSED,
    VALVE_OPENING,
    VALVE_OPEN,
    VALVE_CLOSING,
    VALVE_FAULT
} valve_state_t;

typedef struct {
    uint8_t zone_id;
    char zone_name[24];
    system_type_t system;
    suppression_agent_t agent;
    valve_state_t valve_state;
    float pressure_psi;     /* System pressure */
    float agent_level_pct;  /* Agent tank level */
    bool armed;             /* System armed */
    bool discharged;        /* System has discharged */
    bool supervised;        /* All devices supervised */
    uint32_t last_test;     /* Last test timestamp */
} zone_status_t;

typedef struct {
    uint8_t zone_id;
    system_type_t system;
    suppression_agent_t agent;
    float discharge_time_s; /* Expected discharge duration */
    float hold_time_s;      /* Agent hold time (gas systems) */
    uint8_t discharge_delay_s; /* Pre-discharge delay */
    bool require_double_interlock; /* Require 2 alarms */
    bool abort_enabled;     /* Allow manual abort */
} zone_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int suppression_init(void);
int suppression_shutdown(void);

int suppression_configure_zone(const zone_config_t* config);
int suppression_get_zone_status(uint8_t zone_id, zone_status_t* status);

int suppression_arm_zone(uint8_t zone_id);
int suppression_disarm_zone(uint8_t zone_id);

int suppression_activate_zone(uint8_t zone_id);
int suppression_abort_zone(uint8_t zone_id);
int suppression_reset_zone(uint8_t zone_id);

int suppression_open_valve(uint8_t zone_id);
int suppression_close_valve(uint8_t zone_id);

int suppression_test_zone(uint8_t zone_id);
bool suppression_is_zone_ready(uint8_t zone_id);

int suppression_get_agent_level(uint8_t zone_id, float* level_pct);
int suppression_get_pressure(uint8_t zone_id, float* pressure_psi);

#endif /* GF_SUPPRESSION_ACTUATOR_H */
