/**
 * @file lighting.h
 * @brief Smart Lighting Control Driver
 * 
 * INDUSTRY RELEVANCE:
 * Smart lighting is the largest segment of the smart home market ($15B+ annually).
 * Modern LED lighting requires precise PWM control, color temperature management,
 * and integration with circadian rhythm systems. Products range from consumer
 * bulbs (Philips Hue) to commercial building automation (Lutron, Crestron).
 * 
 * WHY THIS MATTERS:
 * - PWM dimming with flicker-free operation
 * - Color temperature for human-centric lighting
 * - RGBW color mixing for ambiance
 * - Scene management and transitions
 * - Power monitoring and efficiency tracking
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Hardware timer configuration for PWM
 * - Color space conversions (HSV → RGB → PWM)
 * - Smooth transition algorithms
 * - Power optimization strategies
 */

#ifndef GF_LIGHTING_H
#define GF_LIGHTING_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_LIGHT_MAX_ZONES          8       /* Maximum lighting zones */
#define GF_LIGHT_MAX_FIXTURES       32      /* Maximum light fixtures */
#define GF_LIGHT_MAX_SCENES         16      /* Maximum stored scenes */
#define GF_LIGHT_PWM_FREQ_HZ        1000    /* PWM frequency */
#define GF_LIGHT_TRANSITION_STEP_MS 10      /* Transition update interval */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Light capability flags */
typedef enum {
    GF_LIGHT_CAP_ON_OFF     = 0x01,     /* Simple on/off */
    GF_LIGHT_CAP_DIMMING    = 0x02,     /* Brightness control */
    GF_LIGHT_CAP_CCT        = 0x04,     /* Color temperature (tunable white) */
    GF_LIGHT_CAP_RGB        = 0x08,     /* RGB color */
    GF_LIGHT_CAP_RGBW       = 0x10,     /* RGB + white channel */
    GF_LIGHT_CAP_RGBWW      = 0x20      /* RGB + warm white + cool white */
} gf_light_capability_t;

/* Color mode */
typedef enum {
    GF_LIGHT_MODE_OFF = 0,
    GF_LIGHT_MODE_WHITE,        /* White with brightness */
    GF_LIGHT_MODE_CCT,          /* Tunable white (warm/cool) */
    GF_LIGHT_MODE_RGB,          /* Full color */
    GF_LIGHT_MODE_SCENE         /* Playing scene */
} gf_light_mode_t;

/* Color representation */
typedef struct {
    uint8_t r, g, b;            /* RGB values (0-255) */
} gf_light_rgb_t;

typedef struct {
    uint8_t h;                  /* Hue (0-255 maps to 0-360°) */
    uint8_t s;                  /* Saturation (0-255) */
    uint8_t v;                  /* Value/brightness (0-255) */
} gf_light_hsv_t;

typedef struct {
    uint16_t mireds;            /* Color temperature in mireds (1M/Kelvin) */
                                /* 153 = 6500K (cool), 500 = 2000K (warm) */
} gf_light_cct_t;

/* Light fixture state */
typedef struct {
    uint8_t             id;             /* Fixture ID */
    uint8_t             zone;           /* Zone assignment */
    gf_light_mode_t     mode;           /* Current mode */
    uint8_t             capabilities;   /* gf_light_capability_t flags */
    bool                on;             /* Power state */
    uint8_t             brightness;     /* 0-255 */
    gf_light_rgb_t      color;          /* Current RGB */
    gf_light_cct_t      cct;            /* Current CCT */
    uint32_t            power_mw;       /* Current power consumption */
    uint32_t            on_time_sec;    /* Total on time */
} gf_light_fixture_t;

/* Light zone (group of fixtures) */
typedef struct {
    uint8_t     id;
    char        name[16];
    uint8_t     fixture_ids[GF_LIGHT_MAX_FIXTURES];
    uint8_t     fixture_count;
    bool        occupied;               /* Occupancy sensor state */
    uint16_t    ambient_lux;            /* Ambient light level */
} gf_light_zone_t;

/* Scene definition */
typedef struct {
    uint8_t         id;
    char            name[16];
    uint8_t         zone_id;
    gf_light_mode_t mode;
    uint8_t         brightness;
    gf_light_rgb_t  color;
    gf_light_cct_t  cct;
    uint16_t        transition_ms;      /* Transition time */
    bool            is_active;
} gf_light_scene_t;

/* Transition state */
typedef struct {
    uint8_t         fixture_id;
    uint8_t         start_brightness;
    uint8_t         target_brightness;
    gf_light_rgb_t  start_color;
    gf_light_rgb_t  target_color;
    uint32_t        start_time;
    uint32_t        duration_ms;
    bool            active;
} gf_light_transition_t;

/* Light event callback */
typedef void (*gf_light_event_cb_t)(uint8_t fixture_id, gf_light_mode_t mode,
                                    uint8_t brightness, void *ctx);

/* Occupancy event callback */
typedef void (*gf_light_occupancy_cb_t)(uint8_t zone_id, bool occupied, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize lighting subsystem
 */
int gf_light_init(void);

/**
 * @brief Register a light fixture
 * @param fixture Fixture configuration
 * @return Fixture ID or error
 */
int gf_light_register_fixture(const gf_light_fixture_t *fixture);

/**
 * @brief Create a lighting zone
 * @param zone Zone configuration
 * @return Zone ID or error
 */
int gf_light_create_zone(const gf_light_zone_t *zone);

/**
 * @brief Set fixture on/off state
 */
int gf_light_set_power(uint8_t fixture_id, bool on);

/**
 * @brief Set zone on/off state (all fixtures in zone)
 */
int gf_light_set_zone_power(uint8_t zone_id, bool on);

/**
 * @brief Set fixture brightness
 * @param fixture_id Fixture to control
 * @param brightness 0-255 brightness level
 * @param transition_ms Transition time in ms (0 = instant)
 */
int gf_light_set_brightness(uint8_t fixture_id, uint8_t brightness,
                            uint16_t transition_ms);

/**
 * @brief Set zone brightness
 */
int gf_light_set_zone_brightness(uint8_t zone_id, uint8_t brightness,
                                  uint16_t transition_ms);

/**
 * @brief Set fixture RGB color
 */
int gf_light_set_rgb(uint8_t fixture_id, gf_light_rgb_t color,
                     uint16_t transition_ms);

/**
 * @brief Set fixture HSV color
 */
int gf_light_set_hsv(uint8_t fixture_id, gf_light_hsv_t color,
                     uint16_t transition_ms);

/**
 * @brief Set fixture color temperature
 * @param mireds Color temp in mireds (153-500)
 */
int gf_light_set_cct(uint8_t fixture_id, uint16_t mireds,
                     uint16_t transition_ms);

/**
 * @brief Set zone RGB color
 */
int gf_light_set_zone_rgb(uint8_t zone_id, gf_light_rgb_t color,
                          uint16_t transition_ms);

/**
 * @brief Set zone color temperature
 */
int gf_light_set_zone_cct(uint8_t zone_id, uint16_t mireds,
                          uint16_t transition_ms);

/**
 * @brief Create a scene
 */
int gf_light_create_scene(const gf_light_scene_t *scene);

/**
 * @brief Activate a scene
 */
int gf_light_activate_scene(uint8_t scene_id);

/**
 * @brief Get fixture state
 */
int gf_light_get_fixture(uint8_t fixture_id, gf_light_fixture_t *fixture);

/**
 * @brief Get zone state
 */
int gf_light_get_zone(uint8_t zone_id, gf_light_zone_t *zone);

/**
 * @brief Update occupancy sensor state for zone
 */
int gf_light_update_occupancy(uint8_t zone_id, bool occupied);

/**
 * @brief Update ambient light level for zone
 */
int gf_light_update_ambient(uint8_t zone_id, uint16_t lux);

/**
 * @brief Register state change callback
 */
int gf_light_register_event_cb(gf_light_event_cb_t callback, void *ctx);

/**
 * @brief Register occupancy callback
 */
int gf_light_register_occupancy_cb(gf_light_occupancy_cb_t callback, void *ctx);

/**
 * @brief Process lighting (call from main loop for transitions)
 */
int gf_light_process(void);

/*===========================================================================*/
/* Color Conversion Utilities                                                 */
/*===========================================================================*/

/**
 * @brief Convert HSV to RGB
 */
gf_light_rgb_t gf_light_hsv_to_rgb(gf_light_hsv_t hsv);

/**
 * @brief Convert RGB to HSV
 */
gf_light_hsv_t gf_light_rgb_to_hsv(gf_light_rgb_t rgb);

/**
 * @brief Convert Kelvin to mireds
 */
uint16_t gf_light_kelvin_to_mireds(uint16_t kelvin);

/**
 * @brief Convert mireds to Kelvin
 */
uint16_t gf_light_mireds_to_kelvin(uint16_t mireds);

/**
 * @brief Convert CCT to warm/cool white PWM values
 * @param mireds Color temperature
 * @param brightness Overall brightness
 * @param warm_pwm Output warm white PWM (0-255)
 * @param cool_pwm Output cool white PWM (0-255)
 */
void gf_light_cct_to_ww_cw(uint16_t mireds, uint8_t brightness,
                           uint8_t *warm_pwm, uint8_t *cool_pwm);

/**
 * @brief Apply gamma correction for LED output
 */
uint8_t gf_light_gamma_correct(uint8_t linear);

/**
 * @brief Get total power consumption across all fixtures
 */
uint32_t gf_light_get_total_power_mw(void);

#endif /* GF_LIGHTING_H */
