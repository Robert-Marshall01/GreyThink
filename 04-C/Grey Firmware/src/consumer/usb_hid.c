/**
 * @file usb_hid.c
 * @brief USB HID Driver Stub Implementation
 */

#include "consumer/usb_hid.h"
#include "core/driver_registry.h"
#include <string.h>

static struct {
    gf_usb_hid_config_t config;
    gf_usb_state_t state;
    gf_usb_state_cb state_cb;
    void *state_cb_ctx;
    gf_hid_report_cb report_cb;
    gf_hid_keyboard_report_t kbd_report;
    bool initialized;
} s_hid;

static void set_state(gf_usb_state_t state) {
    s_hid.state = state;
    if (s_hid.state_cb) s_hid.state_cb(state, s_hid.state_cb_ctx);
}

int gf_usb_hid_init(const gf_usb_hid_config_t *config) {
    if (!config) return -1;
    memcpy(&s_hid.config, config, sizeof(gf_usb_hid_config_t));
    s_hid.state = GF_USB_STATE_DETACHED;
    s_hid.initialized = true;
    return 0;
}

int gf_usb_hid_connect(void) {
    /* Enable USB pull-up resistor */
    set_state(GF_USB_STATE_ATTACHED);
    return 0;
}

int gf_usb_hid_disconnect(void) {
    set_state(GF_USB_STATE_DETACHED);
    return 0;
}

int gf_usb_hid_send_keyboard(const gf_hid_keyboard_report_t *report) {
    (void)report;
    /* Would send via USB interrupt endpoint */
    return 0;
}

int gf_usb_hid_send_mouse(const gf_hid_mouse_report_t *report) {
    (void)report;
    return 0;
}

int gf_usb_hid_send_gamepad(const gf_hid_gamepad_report_t *report) {
    (void)report;
    return 0;
}

int gf_usb_hid_send_raw(const uint8_t *report, uint16_t len) {
    (void)report; (void)len;
    return 0;
}

gf_usb_state_t gf_usb_hid_get_state(void) { return s_hid.state; }

void gf_usb_hid_set_state_callback(gf_usb_state_cb cb, void *ctx) {
    s_hid.state_cb = cb;
    s_hid.state_cb_ctx = ctx;
}

void gf_usb_hid_set_report_callback(gf_hid_report_cb cb) {
    s_hid.report_cb = cb;
}

void gf_usb_hid_process(void) {
    /* Would handle USB events, descriptors, endpoints */
}

int gf_usb_hid_key_press(uint8_t keycode) {
    for (int i = 0; i < 6; i++) {
        if (s_hid.kbd_report.keys[i] == 0) {
            s_hid.kbd_report.keys[i] = keycode;
            return gf_usb_hid_send_keyboard(&s_hid.kbd_report);
        }
    }
    return -1; /* 6KRO limit */
}

int gf_usb_hid_key_release(uint8_t keycode) {
    for (int i = 0; i < 6; i++) {
        if (s_hid.kbd_report.keys[i] == keycode) {
            s_hid.kbd_report.keys[i] = 0;
            return gf_usb_hid_send_keyboard(&s_hid.kbd_report);
        }
    }
    return 0;
}

int gf_usb_hid_key_release_all(void) {
    memset(&s_hid.kbd_report, 0, sizeof(s_hid.kbd_report));
    return gf_usb_hid_send_keyboard(&s_hid.kbd_report);
}

int gf_usb_hid_type_char(char c) {
    (void)c;
    /* Would convert ASCII to HID keycode and send */
    return 0;
}

int gf_usb_hid_type_string(const char *str) {
    if (!str) return -1;
    while (*str) gf_usb_hid_type_char(*str++);
    return 0;
}

static int hid_drv_init(void *cfg) { return gf_usb_hid_init(cfg); }
static gf_driver_t s_hid_driver = {
    .name = "usb_hid", .version = 0x0100,
    .ops = { .init = hid_drv_init }
};
const void* gf_usb_hid_get_driver(void) { return &s_hid_driver; }
