/**
 * @file life_support_control.h
 * @brief Life Support Control Module for Mars Habitats
 * 
 * INDUSTRY RELEVANCE:
 * Closed-loop life support systems for extraterrestrial habitats must maintain
 * atmospheric composition, temperature, humidity, and pressure within narrow
 * tolerances. This module implements ECLSS (Environmental Control and Life
 * Support System) control algorithms for autonomous habitat management.
 * 
 * Applications:
 * - Mars surface habitats
 * - Lunar Gateway/Artemis missions
 * - Deep space transit vehicles
 * - Submarine life support
 * - Isolated research stations
 * 
 * Standards: NASA-STD-3001, ECSS-E-ST-34C, ISS ECLSS heritage
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_LIFE_SUPPORT_CONTROL_H
#define GF_LIFE_SUPPORT_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define ECLSS_MAX_ZONES             16      /* Maximum habitat zones */
#define ECLSS_MAX_CREW              8       /* Maximum crew capacity */

/* Atmospheric setpoints */
#define O2_NOMINAL_PCT              21.0f   /* Oxygen percentage */
#define CO2_MAX_PCT                 0.5f    /* CO₂ maximum percentage */
#define PRESSURE_NOMINAL_KPA        101.3f  /* Sea-level equivalent */
#define HUMIDITY_NOMINAL_PCT        50.0f   /* Relative humidity */
#define TEMP_NOMINAL_C              22.0f   /* Comfortable temperature */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** ECLSS operational mode */
typedef enum {
    ECLSS_MODE_NOMINAL,         /* Normal operations */
    ECLSS_MODE_REDUCED,         /* Power conservation */
    ECLSS_MODE_EMERGENCY,       /* Emergency response */
    ECLSS_MODE_EVA_SUPPORT,     /* Supporting EVA operations */
    ECLSS_MODE_SLEEP,           /* Crew sleep period */
    ECLSS_MODE_SHUTDOWN         /* Controlled shutdown */
} eclss_mode_t;

/** Zone status */
typedef enum {
    ZONE_NOMINAL,
    ZONE_CAUTION,
    ZONE_WARNING,
    ZONE_CRITICAL,
    ZONE_ISOLATED
} zone_status_t;

/** Life support subsystem */
typedef enum {
    SUBSYS_ATMOSPHERE,          /* O₂/N₂/CO₂ management */
    SUBSYS_THERMAL,             /* Temperature control */
    SUBSYS_HUMIDITY,            /* Water vapor management */
    SUBSYS_PRESSURE,            /* Pressure regulation */
    SUBSYS_WATER,               /* Water recycling */
    SUBSYS_WASTE,               /* Waste processing */
    SUBSYS_FIRE_SUPPRESSION     /* Fire detection/suppression */
} eclss_subsystem_t;

/** Atmospheric state */
typedef struct {
    float o2_pct;
    float co2_pct;
    float n2_pct;
    float pressure_kpa;
    float temperature_c;
    float humidity_pct;
    float trace_contaminants_ppm;
} atmosphere_t;

/** Zone state */
typedef struct {
    uint8_t zone_id;
    char zone_name[32];
    zone_status_t status;
    atmosphere_t atmosphere;
    uint8_t crew_count;
    bool door_sealed;
    bool isolated;
} zone_state_t;

/** ECLSS configuration */
typedef struct {
    uint8_t num_zones;
    uint8_t crew_capacity;
    float o2_setpoint_pct;
    float pressure_setpoint_kpa;
    float temp_setpoint_c;
    float humidity_setpoint_pct;
    bool auto_isolation;
} eclss_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int eclss_init(const eclss_config_t *config);
void eclss_shutdown(void);

int eclss_set_mode(eclss_mode_t mode);
eclss_mode_t eclss_get_mode(void);

int eclss_get_zone_state(uint8_t zone_id, zone_state_t *state);
int eclss_isolate_zone(uint8_t zone_id);
int eclss_reconnect_zone(uint8_t zone_id);

int eclss_set_temperature(uint8_t zone_id, float temp_c);
int eclss_set_humidity(uint8_t zone_id, float humidity_pct);
int eclss_adjust_o2(float delta_pct);

bool eclss_is_atmosphere_safe(uint8_t zone_id);
int eclss_emergency_procedures(uint8_t zone_id);

void eclss_update(uint32_t elapsed_ms);

#endif /* GF_LIFE_SUPPORT_CONTROL_H */
