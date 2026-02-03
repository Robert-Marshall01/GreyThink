/**
 * @file stage_lighting_sensor.h
 * @brief Stage Lighting Sensor Driver for Performance Arts Systems
 * 
 * INDUSTRY RELEVANCE:
 * Modern theatrical productions and live events require intelligent lighting
 * systems with real-time sensing for adaptive ambiance, performer tracking,
 * and audience safety. This driver interfaces with DMX fixtures, LED arrays,
 * and ambient light sensors for coordinated lighting control.
 * 
 * Applications:
 * - Broadway/West End theatrical productions
 * - Concert and festival lighting
 * - Theme park attractions
 * - Architectural lighting installations
 * - Broadcast studio lighting
 * 
 * Standards: DMX512, RDM (Remote Device Management), sACN (E1.31)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_STAGE_LIGHTING_SENSOR_H
#define GF_STAGE_LIGHTING_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define LIGHT_MAX_FIXTURES          512     /* DMX universe capacity */
#define LIGHT_MAX_ZONES             32      /* Lighting zones */
#define LIGHT_DMX_CHANNELS          512     /* DMX channels per universe */
#define LIGHT_CONTROL_RATE_HZ       44      /* DMX refresh rate */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Fixture type */
typedef enum {
    FIXTURE_PAR,                /* PAR can */
    FIXTURE_ELLIPSOIDAL,        /* Ellipsoidal reflector */
    FIXTURE_MOVING_HEAD,        /* Moving head wash/spot */
    FIXTURE_LED_WASH,           /* LED wash panel */
    FIXTURE_LED_STRIP,          /* LED tape/strip */
    FIXTURE_STROBE,             /* Strobe light */
    FIXTURE_LASER,              /* Laser projector */
    FIXTURE_FOG,                /* Fog/haze machine */
    FIXTURE_PYRO                /* Pyrotechnics controller */
} fixture_type_t;

/** Color mode */
typedef enum {
    COLOR_RGB,
    COLOR_RGBW,
    COLOR_RGBAW,                /* RGB + Amber + White */
    COLOR_CMY,
    COLOR_TEMPERATURE,          /* Color temp in Kelvin */
    COLOR_GEL                   /* Gel wheel selection */
} color_mode_t;

/** Fixture state */
typedef struct {
    uint16_t fixture_id;
    fixture_type_t type;
    uint16_t dmx_address;
    uint8_t universe;
    
    /* Intensity */
    uint8_t intensity;          /* 0-255 */
    
    /* Color (RGB) */
    uint8_t red;
    uint8_t green;
    uint8_t blue;
    uint8_t white;
    uint8_t amber;
    
    /* Position (moving heads) */
    uint16_t pan;               /* 0-65535 */
    uint16_t tilt;              /* 0-65535 */
    
    /* Gobo/effects */
    uint8_t gobo;
    uint8_t effect;
    uint8_t strobe_speed;
    
    bool on;
    bool fault;
} fixture_state_t;

/** Ambient sensor reading */
typedef struct {
    uint8_t zone_id;
    float lux;
    float color_temp_k;
    float rgb_ratio[3];
    uint32_t timestamp_ms;
} ambient_reading_t;

/** Lighting zone */
typedef struct {
    uint8_t zone_id;
    char zone_name[24];
    uint16_t fixture_ids[16];
    uint8_t fixture_count;
    uint8_t master_intensity;
    float target_lux;
    float current_lux;
} lighting_zone_t;

/** Cue state */
typedef struct {
    uint16_t cue_number;
    char cue_name[32];
    float fade_time_s;
    float delay_s;
    bool running;
    float progress_pct;
} cue_state_t;

/** Module configuration */
typedef struct {
    uint8_t num_universes;
    bool rdm_enabled;
    bool sacn_enabled;
    const char *sacn_source_name;
} lighting_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int stage_lighting_init(const lighting_config_t *config);
void stage_lighting_shutdown(void);

int stage_lighting_add_fixture(uint16_t fixture_id, fixture_type_t type,
                                uint8_t universe, uint16_t dmx_address);
int stage_lighting_set_fixture(uint16_t fixture_id, const fixture_state_t *state);
int stage_lighting_get_fixture(uint16_t fixture_id, fixture_state_t *state);

int stage_lighting_set_zone_intensity(uint8_t zone_id, uint8_t intensity);
int stage_lighting_get_zone(uint8_t zone_id, lighting_zone_t *zone);

int stage_lighting_read_ambient(uint8_t zone_id, ambient_reading_t *reading);
int stage_lighting_execute_cue(uint16_t cue_number, float fade_time_s);
int stage_lighting_get_cue_state(cue_state_t *state);

void stage_lighting_update(uint32_t elapsed_ms);

#endif /* GF_STAGE_LIGHTING_SENSOR_H */
