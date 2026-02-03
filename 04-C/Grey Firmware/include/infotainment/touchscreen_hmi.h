/**
 * @file touchscreen_hmi.h
 * @brief Automotive Touchscreen HMI Interface
 *
 * INDUSTRY RELEVANCE:
 * Automotive HMI systems are critical for driver interaction and safety.
 * Modern vehicles feature large touchscreens requiring specialized firmware:
 * - Multi-touch gesture recognition with low latency
 * - Haptic feedback for eyes-free operation
 * - Glove/wet finger detection
 * - ASIL compliance for safety-related displays
 *
 * These skills apply to automotive display suppliers (Continental, Visteon,
 * Panasonic), touch controller manufacturers (Synaptics, Atmel, Cypress),
 * and OEMs building unified cockpit platforms.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires automotive-qualified touch controllers.
 */

#ifndef GF_TOUCHSCREEN_HMI_H
#define GF_TOUCHSCREEN_HMI_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_TOUCH_MAX_POINTS         10      /**< Maximum simultaneous touches */
#define GF_TOUCH_MAX_GESTURES       8       /**< Maximum gesture queue depth */
#define GF_TOUCH_REPORT_RATE_HZ     120     /**< Touch report rate */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Touch event types
 */
typedef enum {
    GF_TOUCH_DOWN,              /**< Finger down */
    GF_TOUCH_UP,                /**< Finger up */
    GF_TOUCH_MOVE,              /**< Finger moved */
    GF_TOUCH_CANCEL             /**< Touch cancelled (palm rejection) */
} gf_touch_event_t;

/**
 * @brief Gesture types
 */
typedef enum {
    GF_GESTURE_NONE,            /**< No gesture */
    GF_GESTURE_TAP,             /**< Single tap */
    GF_GESTURE_DOUBLE_TAP,      /**< Double tap */
    GF_GESTURE_LONG_PRESS,      /**< Long press */
    GF_GESTURE_SWIPE_LEFT,      /**< Swipe left */
    GF_GESTURE_SWIPE_RIGHT,     /**< Swipe right */
    GF_GESTURE_SWIPE_UP,        /**< Swipe up */
    GF_GESTURE_SWIPE_DOWN,      /**< Swipe down */
    GF_GESTURE_PINCH_IN,        /**< Pinch zoom in */
    GF_GESTURE_PINCH_OUT,       /**< Pinch zoom out */
    GF_GESTURE_ROTATE           /**< Two-finger rotate */
} gf_gesture_t;

/**
 * @brief Touch input mode
 */
typedef enum {
    GF_TOUCH_MODE_FINGER,       /**< Normal finger touch */
    GF_TOUCH_MODE_GLOVE,        /**< Glove mode (increased sensitivity) */
    GF_TOUCH_MODE_STYLUS,       /**< Stylus mode */
    GF_TOUCH_MODE_HOVER         /**< Hover detection (no contact) */
} gf_touch_mode_t;

/**
 * @brief Single touch point
 */
typedef struct {
    uint8_t id;                 /**< Touch point ID (tracking) */
    gf_touch_event_t event;     /**< Touch event type */
    uint16_t x;                 /**< X coordinate (pixels) */
    uint16_t y;                 /**< Y coordinate (pixels) */
    uint8_t pressure;           /**< Pressure (0-255) */
    uint8_t major_axis;         /**< Touch ellipse major axis */
    uint8_t minor_axis;         /**< Touch ellipse minor axis */
    uint8_t orientation;        /**< Touch orientation (degrees) */
} gf_touch_point_t;

/**
 * @brief Touch frame (all current touches)
 */
typedef struct {
    uint32_t timestamp_ms;      /**< Frame timestamp */
    uint8_t num_points;         /**< Number of active touches */
    gf_touch_point_t points[GF_TOUCH_MAX_POINTS]; /**< Touch points */
} gf_touch_frame_t;

/**
 * @brief Gesture data
 */
typedef struct {
    gf_gesture_t type;          /**< Gesture type */
    uint16_t x;                 /**< Gesture center X */
    uint16_t y;                 /**< Gesture center Y */
    float velocity;             /**< Gesture velocity (for swipes) */
    float scale;                /**< Scale factor (for pinch) */
    float rotation_deg;         /**< Rotation angle (for rotate) */
    uint8_t num_fingers;        /**< Number of fingers involved */
} gf_gesture_data_t;

/**
 * @brief Haptic feedback types
 */
typedef enum {
    GF_HAPTIC_CLICK,            /**< Button click feedback */
    GF_HAPTIC_DOUBLE_CLICK,     /**< Double click feedback */
    GF_HAPTIC_LONG_PRESS,       /**< Long press confirmation */
    GF_HAPTIC_SUCCESS,          /**< Success feedback */
    GF_HAPTIC_ERROR,            /**< Error feedback */
    GF_HAPTIC_WARNING,          /**< Warning feedback */
    GF_HAPTIC_SLIDER,           /**< Slider tick feedback */
    GF_HAPTIC_TEXTURE           /**< Texture rendering */
} gf_haptic_effect_t;

/**
 * @brief Display configuration
 */
typedef struct {
    uint16_t width;             /**< Display width (pixels) */
    uint16_t height;            /**< Display height (pixels) */
    uint8_t touch_rotation;     /**< Touch coordinate rotation (0/90/180/270) */
    bool invert_x;              /**< Invert X axis */
    bool invert_y;              /**< Invert Y axis */
    uint8_t edge_dead_zone;     /**< Edge rejection zone (pixels) */
    uint16_t palm_rejection_area; /**< Palm rejection threshold area */
} gf_touch_config_t;

/**
 * @brief Touch status
 */
typedef struct {
    bool initialized;           /**< Subsystem initialized */
    bool calibrated;            /**< Touch calibrated */
    gf_touch_mode_t current_mode; /**< Current input mode */
    uint8_t active_touches;     /**< Current active touch count */
    uint32_t total_touches;     /**< Total touches since init */
    uint32_t gestures_detected; /**< Total gestures detected */
    uint8_t noise_level;        /**< Environmental noise level */
} gf_touch_status_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_touch_callback_t)(const gf_touch_frame_t* frame, void* user_data);
typedef void (*gf_gesture_callback_t)(const gf_gesture_data_t* gesture, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize touchscreen subsystem
 * @param config Display/touch configuration
 * @return 0 on success, negative error code on failure
 */
int gf_touch_init(const gf_touch_config_t* config);

/**
 * @brief Shutdown touchscreen subsystem
 */
void gf_touch_deinit(void);

/**
 * @brief Register touch callback
 * @param callback Touch event callback
 * @param user_data User context
 */
void gf_touch_register_callback(gf_touch_callback_t callback, void* user_data);

/**
 * @brief Register gesture callback
 * @param callback Gesture event callback
 * @param user_data User context
 */
void gf_touch_register_gesture_callback(gf_gesture_callback_t callback, void* user_data);

/**
 * @brief Set input mode
 * @param mode Input mode (finger, glove, stylus)
 */
void gf_touch_set_mode(gf_touch_mode_t mode);

/**
 * @brief Enable/disable gesture recognition
 * @param enable True to enable
 */
void gf_touch_enable_gestures(bool enable);

/**
 * @brief Trigger haptic feedback
 * @param effect Haptic effect type
 * @param intensity Intensity (0-100)
 */
void gf_touch_haptic(gf_haptic_effect_t effect, uint8_t intensity);

/**
 * @brief Run touch calibration routine
 * @return 0 on success
 */
int gf_touch_calibrate(void);

/**
 * @brief Get current touch status
 * @param[out] status Status output
 */
void gf_touch_get_status(gf_touch_status_t* status);

/**
 * @brief Run touch controller self-test
 * @return 0 on success, negative error code on failure
 */
int gf_touch_self_test(void);

/**
 * @brief Enter low-power mode
 */
void gf_touch_enter_sleep(void);

/**
 * @brief Wake from low-power mode
 */
void gf_touch_wake(void);

/**
 * @brief Create touch exclusion zone (ui elements that reject touch)
 * @param x X position
 * @param y Y position
 * @param width Zone width
 * @param height Zone height
 * @return Zone ID, or negative error code
 */
int gf_touch_add_exclusion_zone(uint16_t x, uint16_t y, uint16_t width, uint16_t height);

/**
 * @brief Remove touch exclusion zone
 * @param zone_id Zone ID to remove
 */
void gf_touch_remove_exclusion_zone(int zone_id);

#ifdef __cplusplus
}
#endif

#endif /* GF_TOUCHSCREEN_HMI_H */
