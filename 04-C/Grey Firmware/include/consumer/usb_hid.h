/**
 * @file usb_hid.h
 * @brief USB HID (Human Interface Device) Driver Stub
 * 
 * WHAT: USB HID class driver for keyboards, mice, game controllers, and
 *       custom HID devices with report descriptor support.
 * 
 * WHY: USB HID is the universal interface for human input devices. Every
 *      computer, game console, and smart device supports it. Understanding
 *      USB descriptors, endpoints, and the HID report format is essential
 *      for consumer electronics firmware development.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Device descriptor configuration
 *      - HID report descriptor building
 *      - Interrupt endpoint handling
 *      - Boot protocol support for BIOS compatibility
 * 
 * Industry applications: gaming peripherals, industrial HMI, medical input
 * 
 * NOTE: Annotated stub. Production requires USB stack integration.
 */

#ifndef GF_USB_HID_H
#define GF_USB_HID_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_USB_HID_MAX_REPORT_SIZE  64
#define GF_USB_HID_POLL_INTERVAL_MS 1   /* 1ms = 1000Hz polling */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_USB_STATE_DETACHED = 0,
    GF_USB_STATE_ATTACHED,
    GF_USB_STATE_POWERED,
    GF_USB_STATE_DEFAULT,
    GF_USB_STATE_ADDRESS,
    GF_USB_STATE_CONFIGURED,
    GF_USB_STATE_SUSPENDED
} gf_usb_state_t;

typedef enum {
    GF_HID_TYPE_KEYBOARD = 0,
    GF_HID_TYPE_MOUSE,
    GF_HID_TYPE_GAMEPAD,
    GF_HID_TYPE_CUSTOM
} gf_hid_type_t;

/* Keyboard report (boot protocol compatible) */
typedef struct {
    uint8_t modifiers;      /* Ctrl, Shift, Alt, GUI modifiers */
    uint8_t reserved;
    uint8_t keys[6];        /* Up to 6 simultaneous keys */
} __attribute__((packed)) gf_hid_keyboard_report_t;

/* Mouse report */
typedef struct {
    uint8_t buttons;        /* Button bitmap */
    int8_t  x;              /* X movement (-127 to 127) */
    int8_t  y;              /* Y movement */
    int8_t  wheel;          /* Scroll wheel */
} __attribute__((packed)) gf_hid_mouse_report_t;

/* Gamepad report */
typedef struct {
    uint16_t buttons;       /* 16 buttons */
    int8_t   left_x;        /* Left stick X */
    int8_t   left_y;        /* Left stick Y */
    int8_t   right_x;       /* Right stick X */
    int8_t   right_y;       /* Right stick Y */
    uint8_t  left_trigger;  /* Left trigger */
    uint8_t  right_trigger; /* Right trigger */
} __attribute__((packed)) gf_hid_gamepad_report_t;

/* HID configuration */
typedef struct {
    gf_hid_type_t   type;
    uint16_t        vid;            /* USB Vendor ID */
    uint16_t        pid;            /* USB Product ID */
    const char     *manufacturer;
    const char     *product;
    const char     *serial;
    uint8_t         poll_interval_ms;
} gf_usb_hid_config_t;

/* State change callback */
typedef void (*gf_usb_state_cb)(gf_usb_state_t state, void *ctx);

/* Host request callback (GET_REPORT, SET_REPORT) */
typedef void (*gf_hid_report_cb)(uint8_t report_id, uint8_t *data, uint16_t len);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize USB HID driver
 */
int gf_usb_hid_init(const gf_usb_hid_config_t *config);

/**
 * @brief Connect to USB host (enable pull-up)
 */
int gf_usb_hid_connect(void);

/**
 * @brief Disconnect from USB host
 */
int gf_usb_hid_disconnect(void);

/**
 * @brief Send keyboard report
 */
int gf_usb_hid_send_keyboard(const gf_hid_keyboard_report_t *report);

/**
 * @brief Send mouse report
 */
int gf_usb_hid_send_mouse(const gf_hid_mouse_report_t *report);

/**
 * @brief Send gamepad report
 */
int gf_usb_hid_send_gamepad(const gf_hid_gamepad_report_t *report);

/**
 * @brief Send custom HID report
 */
int gf_usb_hid_send_raw(const uint8_t *report, uint16_t len);

/**
 * @brief Get current USB state
 */
gf_usb_state_t gf_usb_hid_get_state(void);

/**
 * @brief Set state change callback
 */
void gf_usb_hid_set_state_callback(gf_usb_state_cb callback, void *ctx);

/**
 * @brief Set report request callback
 */
void gf_usb_hid_set_report_callback(gf_hid_report_cb callback);

/**
 * @brief Process USB events (call from main loop)
 */
void gf_usb_hid_process(void);

/**
 * @brief Get USB HID driver descriptor
 */
const void* gf_usb_hid_get_driver(void);

/*===========================================================================*/
/* Keyboard Helper Functions                                                  */
/*===========================================================================*/

/** @brief Press a key (use HID key codes) */
int gf_usb_hid_key_press(uint8_t keycode);

/** @brief Release a key */
int gf_usb_hid_key_release(uint8_t keycode);

/** @brief Release all keys */
int gf_usb_hid_key_release_all(void);

/** @brief Type a character (ASCII) */
int gf_usb_hid_type_char(char c);

/** @brief Type a string */
int gf_usb_hid_type_string(const char *str);

#endif /* GF_USB_HID_H */
