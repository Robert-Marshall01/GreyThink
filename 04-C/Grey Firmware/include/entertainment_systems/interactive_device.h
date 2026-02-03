/**
 * @file interactive_device.h
 * @brief Interactive Device Interface for Entertainment Systems
 * 
 * INDUSTRY RELEVANCE:
 * Interactive entertainment devices include VR controllers, haptic gloves,
 * motion platforms, and arcade systems. Companies like Sony, Nintendo,
 * Microsoft, Razer, and specialized haptics firms (bHaptics, SenseGlove)
 * need firmware engineers for input processing, haptic feedback, and
 * low-latency wireless communication.
 * 
 * This module provides interfaces for interactive devices including
 * controllers, haptic feedback, and motion tracking.
 * 
 * KEY CAPABILITIES:
 * - Button and trigger input
 * - Analog stick processing
 * - Haptic feedback (LRA, ERM)
 * - HD haptics (waveform playback)
 * - Motion sensing (6-DOF)
 * - Finger tracking (capacitive)
 * - Low-latency wireless
 * - Battery management
 * 
 * WIRELESS TECHNOLOGIES:
 * - Bluetooth Low Energy
 * - Proprietary 2.4GHz
 * - UWB for precise tracking
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_INTERACTIVE_DEVICE_H
#define GF_INTERACTIVE_DEVICE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define ID_MAX_BUTTONS         16    /**< Max discrete buttons */
#define ID_MAX_AXES            8     /**< Max analog axes */
#define ID_MAX_HAPTIC_CH       4     /**< Haptic output channels */
#define ID_POLLING_RATE_HZ     1000  /**< 1ms polling */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Device type */
typedef enum {
    ID_TYPE_CONTROLLER,    /**< Standard game controller */
    ID_TYPE_VR_CONTROLLER, /**< VR motion controller */
    ID_TYPE_HAPTIC_GLOVE,  /**< Haptic feedback glove */
    ID_TYPE_MOTION_PLATFORM, /**< Motion simulator */
    ID_TYPE_ARCADE,        /**< Arcade input device */
    ID_TYPE_WHEEL          /**< Racing wheel */
} id_type_t;

/** Button ID */
typedef enum {
    ID_BTN_A, ID_BTN_B, ID_BTN_X, ID_BTN_Y,
    ID_BTN_L1, ID_BTN_R1, ID_BTN_L2, ID_BTN_R2,
    ID_BTN_LSTICK, ID_BTN_RSTICK,
    ID_BTN_START, ID_BTN_SELECT,
    ID_BTN_DPAD_UP, ID_BTN_DPAD_DOWN,
    ID_BTN_DPAD_LEFT, ID_BTN_DPAD_RIGHT
} id_button_t;

/** Axis ID */
typedef enum {
    ID_AXIS_LX, ID_AXIS_LY,  /**< Left stick */
    ID_AXIS_RX, ID_AXIS_RY,  /**< Right stick */
    ID_AXIS_LT, ID_AXIS_RT,  /**< Triggers */
    ID_AXIS_GYRO_YAW,
    ID_AXIS_GYRO_PITCH
} id_axis_t;

/** Haptic effect type */
typedef enum {
    ID_HAPTIC_CLICK,
    ID_HAPTIC_BUMP,
    ID_HAPTIC_RUMBLE,
    ID_HAPTIC_VIBRATE,
    ID_HAPTIC_WAVEFORM
} id_haptic_type_t;

/** Input state */
typedef struct {
    uint16_t buttons;      /**< Button bitmask */
    int16_t axes[ID_MAX_AXES];  /**< Axis values (-32768 to 32767) */
    float position[3];     /**< XYZ position (tracked controllers) */
    float orientation[4];  /**< Quaternion (tracked controllers) */
    float battery_pct;
    bool connected;
} id_state_t;

/** Haptic command */
typedef struct {
    uint8_t channel;
    id_haptic_type_t type;
    float intensity;       /**< 0.0-1.0 */
    uint16_t duration_ms;
    const uint8_t* waveform; /**< For HD haptics */
    uint16_t waveform_len;
} id_haptic_cmd_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize interactive device
 * @param type Device type
 * @return 0 on success
 */
int id_init(id_type_t type);

/**
 * @brief Poll input state
 * @param state Output state
 * @return 0 on success
 */
int id_poll(id_state_t* state);

/**
 * @brief Check button state
 * @param button Button to check
 * @return true if pressed
 */
bool id_button_pressed(id_button_t button);

/**
 * @brief Get axis value
 * @param axis Axis to read
 * @return Axis value (-32768 to 32767)
 */
int16_t id_get_axis(id_axis_t axis);

/**
 * @brief Play haptic effect
 * @param cmd Haptic command
 * @return 0 on success
 */
int id_play_haptic(const id_haptic_cmd_t* cmd);

/**
 * @brief Stop all haptic effects
 * @return 0 on success
 */
int id_stop_haptic(void);

/**
 * @brief Set LED color (if supported)
 * @param r Red 0-255
 * @param g Green 0-255
 * @param b Blue 0-255
 * @return 0 on success
 */
int id_set_led(uint8_t r, uint8_t g, uint8_t b);

/**
 * @brief Shutdown device
 */
void id_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_INTERACTIVE_DEVICE_H */
