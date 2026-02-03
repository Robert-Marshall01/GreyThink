/**
 * @file controller_input.h
 * @brief Gaming Controller Input Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Gaming peripherals represent a multi-billion dollar market with
 * demanding latency requirements (<1ms for competitive gaming).
 * Controllers from Sony, Microsoft, and third parties use custom
 * embedded firmware for input processing, haptic feedback, and
 * wireless communication.
 * 
 * This stub demonstrates:
 * - Low-latency input sampling
 * - Analog stick calibration and deadzones
 * - Haptic/rumble motor control
 * - Wireless protocol support (BT/2.4GHz)
 * 
 * STANDARDS:
 * - USB HID (Human Interface Device)
 * - Bluetooth HID over GATT
 * - Platform-specific (XInput, DirectInput)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CONTROLLER_INPUT_H
#define GF_CONTROLLER_INPUT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    CTRL_BUTTON_A = 0x0001,
    CTRL_BUTTON_B = 0x0002,
    CTRL_BUTTON_X = 0x0004,
    CTRL_BUTTON_Y = 0x0008,
    CTRL_BUTTON_LB = 0x0010,
    CTRL_BUTTON_RB = 0x0020,
    CTRL_BUTTON_LT = 0x0040,
    CTRL_BUTTON_RT = 0x0080,
    CTRL_BUTTON_START = 0x0100,
    CTRL_BUTTON_SELECT = 0x0200,
    CTRL_BUTTON_L3 = 0x0400,
    CTRL_BUTTON_R3 = 0x0800,
    CTRL_BUTTON_DPAD_UP = 0x1000,
    CTRL_BUTTON_DPAD_DOWN = 0x2000,
    CTRL_BUTTON_DPAD_LEFT = 0x4000,
    CTRL_BUTTON_DPAD_RIGHT = 0x8000
} controller_button_t;

typedef struct {
    int16_t x;    /**< -32768 to 32767 */
    int16_t y;
} analog_stick_t;

typedef struct {
    uint16_t buttons;       /**< Button bitmap */
    analog_stick_t left;
    analog_stick_t right;
    uint8_t left_trigger;   /**< 0-255 */
    uint8_t right_trigger;
    uint64_t timestamp_us;
    uint8_t battery_pct;
} controller_state_t;

typedef struct {
    float deadzone_inner;   /**< 0-1 inner deadzone */
    float deadzone_outer;   /**< 0-1 outer deadzone */
    float curve_exponent;   /**< Response curve */
    bool invert_x;
    bool invert_y;
} stick_config_t;

int controller_init(void);
int controller_configure_stick(uint8_t stick_id, const stick_config_t *config);
int controller_get_state(controller_state_t *state);
int controller_set_rumble(uint8_t left_motor, uint8_t right_motor);
int controller_set_led(uint8_t r, uint8_t g, uint8_t b);
int controller_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_CONTROLLER_INPUT_H */
