/**
 * @file led_matrix.h
 * @brief LED Matrix Control Module
 * 
 * INDUSTRY RELEVANCE:
 *   LED matrices and addressable LEDs are used in:
 *   - Consumer: Gaming peripherals, smart lighting, wearables
 *   - Industrial: Status displays, machine indicators, warning systems
 *   - Signage: Digital displays, scoreboards, information panels
 *   - Automotive: Interior ambient lighting, exterior indicators
 *   - IoT: Visual feedback, notification systems
 * 
 * Common drivers: WS2812B (NeoPixel), APA102, MAX7219, HT16K33
 * This stub demonstrates LED animation, color management, and matrix addressing.
 */

#ifndef GF_HMI_LED_MATRIX_H
#define GF_HMI_LED_MATRIX_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_LED_TYPE_WS2812B = 0,    /* NeoPixel (GRB) */
    GF_LED_TYPE_APA102,         /* DotStar (BGR + brightness) */
    GF_LED_TYPE_SK6812,         /* RGBW variant */
    GF_LED_TYPE_MAX7219,        /* 7-segment/matrix driver */
    GF_LED_TYPE_SINGLE_COLOR    /* Simple on/off matrix */
} gf_led_type_t;

typedef enum {
    GF_LED_IFACE_WS_BITBANG = 0,    /* Timing-critical GPIO */
    GF_LED_IFACE_SPI,               /* SPI-based (APA102) */
    GF_LED_IFACE_I2C,               /* I2C driver (HT16K33) */
    GF_LED_IFACE_PWM_DMA            /* PWM + DMA for WS2812 */
} gf_led_interface_t;

typedef struct {
    uint8_t     r, g, b, w;     /* RGBW (w=0 for RGB-only) */
} gf_led_color_t;

typedef struct {
    gf_led_type_t       type;
    gf_led_interface_t  interface;
    uint8_t             rows;
    uint8_t             cols;
    uint16_t            total_leds;
    uint8_t             brightness;     /* Global brightness 0-255 */
    bool                serpentine;     /* Zig-zag wiring pattern */
} gf_led_matrix_config_t;

typedef enum {
    GF_LED_ANIM_NONE = 0,
    GF_LED_ANIM_SOLID,
    GF_LED_ANIM_BLINK,
    GF_LED_ANIM_BREATHE,
    GF_LED_ANIM_RAINBOW,
    GF_LED_ANIM_CHASE,
    GF_LED_ANIM_SPARKLE,
    GF_LED_ANIM_WAVE
} gf_led_animation_t;

typedef struct {
    gf_led_animation_t  type;
    gf_led_color_t      primary;
    gf_led_color_t      secondary;
    uint16_t            speed_ms;
    uint8_t             intensity;
    bool                reverse;
} gf_led_anim_config_t;

/*===========================================================================*/
/* API                                                                        */
/*===========================================================================*/

/**
 * @brief Initialize LED matrix
 */
int gf_led_matrix_init(const gf_led_matrix_config_t *config);

/**
 * @brief Deinitialize LED matrix
 */
void gf_led_matrix_deinit(void);

/**
 * @brief Set single LED color
 */
void gf_led_matrix_set_pixel(uint8_t row, uint8_t col, gf_led_color_t color);

/**
 * @brief Set LED by linear index
 */
void gf_led_matrix_set_index(uint16_t index, gf_led_color_t color);

/**
 * @brief Fill entire matrix with color
 */
void gf_led_matrix_fill(gf_led_color_t color);

/**
 * @brief Clear matrix (all off)
 */
void gf_led_matrix_clear(void);

/**
 * @brief Set global brightness
 */
void gf_led_matrix_set_brightness(uint8_t brightness);

/**
 * @brief Update matrix (send data to LEDs)
 */
void gf_led_matrix_show(void);

/**
 * @brief Start animation
 */
int gf_led_matrix_animate(const gf_led_anim_config_t *anim);

/**
 * @brief Stop animation
 */
void gf_led_matrix_stop_animation(void);

/**
 * @brief Update animation (call periodically)
 */
void gf_led_matrix_update(void);

/**
 * @brief Draw character on matrix
 */
void gf_led_matrix_draw_char(uint8_t x, uint8_t y, char c, gf_led_color_t color);

/**
 * @brief Scroll text across matrix
 */
int gf_led_matrix_scroll_text(const char *text, gf_led_color_t color, 
                               uint16_t scroll_delay_ms);

/**
 * @brief Helper: create RGB color
 */
gf_led_color_t gf_led_rgb(uint8_t r, uint8_t g, uint8_t b);

/**
 * @brief Helper: create color from HSV
 */
gf_led_color_t gf_led_hsv(uint8_t h, uint8_t s, uint8_t v);

#endif /* GF_HMI_LED_MATRIX_H */
