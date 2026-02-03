/**
 * @file display_driver.h
 * @brief LCD/OLED Display Driver Abstraction
 * 
 * INDUSTRY RELEVANCE:
 *   Display drivers are essential across embedded domains:
 *   - Consumer: Smartwatches, appliances, remote controls
 *   - Industrial: HMI panels, machine status, process visualization
 *   - Medical: Patient monitors, diagnostic equipment, portable devices
 *   - Automotive: Dashboard clusters, infotainment, HUD
 *   - IoT: Smart thermostats, security panels, wearables
 * 
 * Common interfaces: SPI (ST7789, ILI9341), I2C (SSD1306), Parallel (FSMC).
 * This stub demonstrates display-agnostic graphics abstraction.
 */

#ifndef GF_HMI_DISPLAY_H
#define GF_HMI_DISPLAY_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_DISPLAY_ST7789 = 0,      /* 240x240 IPS LCD */
    GF_DISPLAY_ILI9341,         /* 320x240 TFT LCD */
    GF_DISPLAY_SSD1306,         /* 128x64 OLED */
    GF_DISPLAY_SH1106,          /* 128x64 OLED variant */
    GF_DISPLAY_ST7735,          /* 160x128 TFT LCD */
    GF_DISPLAY_GENERIC
} gf_display_type_t;

typedef enum {
    GF_DISPLAY_IFACE_SPI = 0,
    GF_DISPLAY_IFACE_I2C,
    GF_DISPLAY_IFACE_PARALLEL_8,
    GF_DISPLAY_IFACE_PARALLEL_16
} gf_display_iface_t;

typedef enum {
    GF_DISPLAY_ROTATE_0 = 0,
    GF_DISPLAY_ROTATE_90,
    GF_DISPLAY_ROTATE_180,
    GF_DISPLAY_ROTATE_270
} gf_display_rotation_t;

typedef struct {
    gf_display_type_t   type;
    gf_display_iface_t  interface;
    uint16_t            width;
    uint16_t            height;
    uint8_t             color_depth;    /* 1, 16, or 24 bits */
    gf_display_rotation_t rotation;
    bool                invert_colors;
} gf_display_config_t;

typedef struct {
    uint8_t     r, g, b;
} gf_color_rgb_t;

typedef struct {
    bool        initialized;
    bool        backlight_on;
    uint8_t     brightness;
    uint16_t    width;
    uint16_t    height;
} gf_display_status_t;

/*===========================================================================*/
/* API                                                                        */
/*===========================================================================*/

/**
 * @brief Initialize display
 */
int gf_display_init(const gf_display_config_t *config);

/**
 * @brief Deinitialize display
 */
void gf_display_deinit(void);

/**
 * @brief Clear display
 */
void gf_display_clear(uint16_t color);

/**
 * @brief Fill rectangle
 */
void gf_display_fill_rect(uint16_t x, uint16_t y, uint16_t w, uint16_t h, 
                          uint16_t color);

/**
 * @brief Draw pixel
 */
void gf_display_pixel(uint16_t x, uint16_t y, uint16_t color);

/**
 * @brief Draw line
 */
void gf_display_line(uint16_t x0, uint16_t y0, uint16_t x1, uint16_t y1, 
                     uint16_t color);

/**
 * @brief Draw rectangle outline
 */
void gf_display_rect(uint16_t x, uint16_t y, uint16_t w, uint16_t h, 
                     uint16_t color);

/**
 * @brief Draw circle
 */
void gf_display_circle(uint16_t cx, uint16_t cy, uint16_t r, uint16_t color);

/**
 * @brief Draw text
 */
void gf_display_text(uint16_t x, uint16_t y, const char *text, 
                     uint16_t color, uint16_t bg);

/**
 * @brief Draw bitmap
 */
void gf_display_bitmap(uint16_t x, uint16_t y, uint16_t w, uint16_t h,
                       const uint16_t *data);

/**
 * @brief Set backlight brightness (0-100)
 */
void gf_display_set_brightness(uint8_t percent);

/**
 * @brief Turn backlight on/off
 */
void gf_display_backlight(bool on);

/**
 * @brief Flush framebuffer to display (if double-buffered)
 */
void gf_display_flush(void);

/**
 * @brief Set rotation
 */
void gf_display_set_rotation(gf_display_rotation_t rotation);

/**
 * @brief Get display status
 */
void gf_display_get_status(gf_display_status_t *status);

/**
 * @brief Convert RGB to 16-bit color (RGB565)
 */
uint16_t gf_display_rgb565(uint8_t r, uint8_t g, uint8_t b);

#endif /* GF_HMI_DISPLAY_H */
