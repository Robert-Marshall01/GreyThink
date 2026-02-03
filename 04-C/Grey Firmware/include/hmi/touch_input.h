/**
 * @file touch_input.h
 * @brief Capacitive Touch Input Driver
 * 
 * INDUSTRY RELEVANCE:
 *   Touch interfaces dominate modern embedded HMI:
 *   - Consumer: Smartphones, tablets, smart appliances
 *   - Automotive: Center consoles, climate controls, steering buttons
 *   - Industrial: HMI panels, process control, kiosks
 *   - Medical: Bedside monitors, diagnostic equipment, wearables
 * 
 * Common controllers: FT6336, GT911, CST816, MPR121
 * This stub demonstrates touch event handling, gesture recognition,
 * and multi-touch coordinate processing.
 */

#ifndef GF_HMI_TOUCH_H
#define GF_HMI_TOUCH_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

#define GF_TOUCH_MAX_POINTS     5

typedef enum {
    GF_TOUCH_CTRL_FT6336 = 0,   /* FocalTech */
    GF_TOUCH_CTRL_GT911,        /* Goodix */
    GF_TOUCH_CTRL_CST816,       /* Hynitron */
    GF_TOUCH_CTRL_MPR121,       /* NXP proximity */
    GF_TOUCH_CTRL_GENERIC
} gf_touch_controller_t;

typedef enum {
    GF_TOUCH_EVENT_NONE = 0,
    GF_TOUCH_EVENT_DOWN,
    GF_TOUCH_EVENT_UP,
    GF_TOUCH_EVENT_MOVE,
    GF_TOUCH_EVENT_HOLD
} gf_touch_event_t;

typedef enum {
    GF_GESTURE_NONE = 0,
    GF_GESTURE_TAP,
    GF_GESTURE_DOUBLE_TAP,
    GF_GESTURE_LONG_PRESS,
    GF_GESTURE_SWIPE_LEFT,
    GF_GESTURE_SWIPE_RIGHT,
    GF_GESTURE_SWIPE_UP,
    GF_GESTURE_SWIPE_DOWN,
    GF_GESTURE_PINCH_IN,
    GF_GESTURE_PINCH_OUT
} gf_gesture_t;

typedef struct {
    uint16_t            x;
    uint16_t            y;
    uint8_t             pressure;       /* 0-255 if supported */
    uint8_t             id;             /* Touch point ID */
    gf_touch_event_t    event;
} gf_touch_point_t;

typedef struct {
    gf_touch_point_t    points[GF_TOUCH_MAX_POINTS];
    uint8_t             point_count;
    uint32_t            timestamp;
    gf_gesture_t        gesture;
} gf_touch_data_t;

typedef struct {
    gf_touch_controller_t   controller;
    uint8_t                 i2c_addr;
    uint16_t                screen_width;
    uint16_t                screen_height;
    bool                    invert_x;
    bool                    invert_y;
    bool                    swap_xy;
    uint8_t                 threshold;      /* Touch sensitivity */
} gf_touch_config_t;

typedef void (*gf_touch_callback_t)(const gf_touch_data_t *data, void *ctx);
typedef void (*gf_gesture_callback_t)(gf_gesture_t gesture, void *ctx);

/*===========================================================================*/
/* API                                                                        */
/*===========================================================================*/

/**
 * @brief Initialize touch controller
 */
int gf_touch_init(const gf_touch_config_t *config);

/**
 * @brief Deinitialize touch controller
 */
void gf_touch_deinit(void);

/**
 * @brief Poll touch state (call periodically)
 */
int gf_touch_poll(gf_touch_data_t *data);

/**
 * @brief Set touch event callback
 */
void gf_touch_set_callback(gf_touch_callback_t cb, void *ctx);

/**
 * @brief Set gesture callback
 */
void gf_touch_set_gesture_callback(gf_gesture_callback_t cb, void *ctx);

/**
 * @brief Enable/disable gesture detection
 */
void gf_touch_enable_gestures(bool enable);

/**
 * @brief Set touch threshold
 */
void gf_touch_set_threshold(uint8_t threshold);

/**
 * @brief Calibrate touch panel
 */
int gf_touch_calibrate(void);

/**
 * @brief Check if screen is currently touched
 */
bool gf_touch_is_pressed(void);

/**
 * @brief Get last touch position
 */
void gf_touch_get_position(uint16_t *x, uint16_t *y);

#endif /* GF_HMI_TOUCH_H */
