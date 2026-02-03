/**
 * @file room_automation.h
 * @brief Room Automation Driver for Smart Hospitality Systems
 * 
 * INDUSTRY RELEVANCE:
 * Smart hotel technology improves guest experience while reducing energy costs
 * by 20-30%. Room automation enables:
 * - Personalized climate and lighting control
 * - Keyless entry via mobile/BLE
 * - Occupancy-based energy management
 * - Integration with property management systems (PMS)
 * 
 * Target applications: Hotels, resorts, cruise ships, corporate offices,
 * senior living facilities, student housing.
 * 
 * Standards: ASHRAE 90.1 (energy), IEEE 802.11 (WiFi), BLE (Bluetooth),
 *            HTNG (hospitality integration), OpenBAN (building automation)
 */

#ifndef GF_ROOM_AUTOMATION_H
#define GF_ROOM_AUTOMATION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Room Automation Types                                                      */
/*===========================================================================*/

typedef enum {
    ROOM_TYPE_STANDARD,
    ROOM_TYPE_SUITE,
    ROOM_TYPE_CONFERENCE,
    ROOM_TYPE_LOBBY,
    ROOM_TYPE_RESTAURANT,
    ROOM_TYPE_POOL,
    ROOM_TYPE_GYM
} room_type_t;

typedef enum {
    OCCUPANCY_VACANT,
    OCCUPANCY_OCCUPIED,
    OCCUPANCY_CHECKED_OUT,
    OCCUPANCY_DO_NOT_DISTURB,
    OCCUPANCY_MAKE_UP_ROOM
} occupancy_state_t;

typedef enum {
    SCENE_WELCOME,          /* Guest arrival scene */
    SCENE_RELAXATION,       /* Evening relaxation */
    SCENE_WORK,             /* Business work mode */
    SCENE_SLEEP,            /* Night/sleep mode */
    SCENE_CHECKOUT,         /* Guest departure */
    SCENE_CLEANING,         /* Housekeeping mode */
    SCENE_ENERGY_SAVE       /* Unoccupied energy save */
} room_scene_t;

typedef struct {
    float temperature_c;    /* Current temperature */
    float setpoint_c;       /* Temperature setpoint */
    float humidity_pct;     /* Current humidity */
    bool hvac_on;           /* HVAC running */
    bool heating;           /* Heating active (vs cooling) */
    uint8_t fan_speed;      /* Fan speed 0-100% */
} climate_state_t;

typedef struct {
    uint8_t brightness;     /* Overall brightness 0-100% */
    uint16_t color_temp_k;  /* Color temperature 2700-6500K */
    uint8_t zone_count;     /* Number of zones */
    uint8_t* zone_levels;   /* Per-zone brightness */
    bool curtains_open;     /* Curtain/shade position */
} lighting_state_t;

typedef struct {
    char room_id[12];
    room_type_t type;
    occupancy_state_t occupancy;
    room_scene_t active_scene;
    climate_state_t climate;
    lighting_state_t lighting;
    bool door_locked;
    bool window_open;
    bool minibar_accessed;
    uint32_t last_activity;
} room_state_t;

typedef struct {
    float temp_setpoint_min;
    float temp_setpoint_max;
    float unoccupied_setback;   /* Temperature setback when empty */
    uint32_t vacancy_timeout_s; /* Time before energy-save mode */
    bool allow_guest_override;
    uint8_t default_brightness;
} room_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int room_automation_init(const char* room_id, room_type_t type);
int room_automation_shutdown(void);

int room_set_config(const room_config_t* config);
int room_get_state(room_state_t* state);

int room_set_scene(room_scene_t scene);
int room_set_climate(float temp_c, uint8_t fan_speed);
int room_set_lighting(uint8_t brightness, uint16_t color_temp_k);
int room_set_curtains(bool open);

int room_set_occupancy(occupancy_state_t state);
int room_lock_door(bool lock);

int room_guest_checkin(const char* guest_id);
int room_guest_checkout(void);

int room_get_energy_usage(float* kwh_today, float* kwh_month);
bool room_is_occupied(void);

#endif /* GF_ROOM_AUTOMATION_H */
