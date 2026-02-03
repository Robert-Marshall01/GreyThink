/**
 * @file display_driver.h
 * @brief Display Driver Stub for Embedded LCDs
 * 
 * WHAT: Generic display driver supporting character LCDs, graphic LCDs,
 *       OLED displays, and TFT panels with common interface protocols.
 * 
 * WHY: User interfaces are critical for consumer products. A well-abstracted
 *      display driver enables UI code reuse across different display types
 *      and sizes. Understanding display protocols (SPI, I2C, parallel) and
 *      graphics primitives is essential for consumer electronics roles.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Hardware-agnostic graphics API
 *      - Font rendering with multiple sizes
 *      - Double buffering for flicker-free updates
 *      - DMA-based transfers for performance
 * 
 * Industry applications: appliances, consumer electronics, industrial HMI
 * 
 * NOTE: Annotated stub. Production requires display-specific initialization.
 */

#ifndef GF_DISPLAY_DRIVER_H
#define GF_DISPLAY_DRIVER_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_DISP_MAX_WIDTH       320
#define GF_DISP_MAX_HEIGHT      240
#define GF_DISP_FONT_SMALL      0
#define GF_DISP_FONT_MEDIUM     1
#define GF_DISP_FONT_LARGE      2

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_DISP_TYPE_CHAR_LCD = 0,  /* Character LCD (HD44780) */
    GF_DISP_TYPE_MONO_OLED,     /* Monochrome OLED (SSD1306) */
    GF_DISP_TYPE_COLOR_TFT,     /* Color TFT (ILI9341, ST7789) */
    GF_DISP_TYPE_EPAPER         /* E-paper display */
} gf_disp_type_t;

typedef enum {
    GF_DISP_IFACE_SPI = 0,
    GF_DISP_IFACE_I2C,
    GF_DISP_IFACE_PARALLEL_8BIT,
    GF_DISP_IFACE_PARALLEL_16BIT
} gf_disp_interface_t;

typedef enum {
    GF_DISP_ROT_0 = 0,
    GF_DISP_ROT_90,
    GF_DISP_ROT_180,
    GF_DISP_ROT_270
} gf_disp_rotation_t;

/* 16-bit RGB565 color */
typedef uint16_t gf_color_t;

/* Common colors (RGB565 format) */
#define GF_COLOR_BLACK      0x0000
#define GF_COLOR_WHITE      0xFFFF
#define GF_COLOR_RED        0xF800
#define GF_COLOR_GREEN      0x07E0
#define GF_COLOR_BLUE       0x001F
#define GF_COLOR_YELLOW     0xFFE0
#define GF_COLOR_CYAN       0x07FF
#define GF_COLOR_MAGENTA    0xF81F

/* Point structure */
typedef struct {
    int16_t x;
    int16_t y;
} gf_point_t;

/* Rectangle structure */
typedef struct {
    int16_t x;
    int16_t y;
    uint16_t width;
    uint16_t height;
} gf_rect_t;

/* Display configuration */
typedef struct {
    gf_disp_type_t      type;
    gf_disp_interface_t interface;
    uint16_t            width;
    uint16_t            height;
    gf_disp_rotation_t  rotation;
    bool                double_buffer;
} gf_disp_config_t;

/* Display statistics */
typedef struct {
    uint32_t    frames_rendered;
    uint32_t    last_frame_ms;
    uint8_t     fps;
    bool        busy;
} gf_disp_stats_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize display driver
 */
int gf_disp_init(const gf_disp_config_t *config);

/**
 * @brief Set display brightness (0-255)
 */
int gf_disp_set_brightness(uint8_t level);

/**
 * @brief Set display rotation
 */
int gf_disp_set_rotation(gf_disp_rotation_t rotation);

/**
 * @brief Clear display to specified color
 */
void gf_disp_clear(gf_color_t color);

/**
 * @brief Draw a single pixel
 */
void gf_disp_draw_pixel(int16_t x, int16_t y, gf_color_t color);

/**
 * @brief Draw a line
 */
void gf_disp_draw_line(int16_t x0, int16_t y0, int16_t x1, int16_t y1, 
                        gf_color_t color);

/**
 * @brief Draw a rectangle (outline)
 */
void gf_disp_draw_rect(const gf_rect_t *rect, gf_color_t color);

/**
 * @brief Draw a filled rectangle
 */
void gf_disp_fill_rect(const gf_rect_t *rect, gf_color_t color);

/**
 * @brief Draw a circle (outline)
 */
void gf_disp_draw_circle(int16_t cx, int16_t cy, uint16_t radius, 
                          gf_color_t color);

/**
 * @brief Draw a filled circle
 */
void gf_disp_fill_circle(int16_t cx, int16_t cy, uint16_t radius, 
                          gf_color_t color);

/**
 * @brief Draw text string
 */
void gf_disp_draw_text(int16_t x, int16_t y, const char *text,
                        uint8_t font_size, gf_color_t fg, gf_color_t bg);

/**
 * @brief Get text width in pixels
 */
uint16_t gf_disp_text_width(const char *text, uint8_t font_size);

/**
 * @brief Draw bitmap image
 */
void gf_disp_draw_bitmap(int16_t x, int16_t y, const uint8_t *bitmap,
                          uint16_t width, uint16_t height, gf_color_t color);

/**
 * @brief Draw RGB565 image
 */
void gf_disp_draw_image(int16_t x, int16_t y, const uint16_t *image,
                         uint16_t width, uint16_t height);

/**
 * @brief Flush framebuffer to display (for double-buffered mode)
 */
void gf_disp_flush(void);

/**
 * @brief Check if display is busy with transfer
 */
bool gf_disp_is_busy(void);

/**
 * @brief Get display statistics
 */
void gf_disp_get_stats(gf_disp_stats_t *stats);

/**
 * @brief Get display driver descriptor
 */
const void* gf_disp_get_driver(void);

/*===========================================================================*/
/* Utility Functions                                                          */
/*===========================================================================*/

/** @brief Convert RGB to RGB565 color */
static inline gf_color_t gf_color_rgb(uint8_t r, uint8_t g, uint8_t b) {
    return ((r & 0xF8) << 8) | ((g & 0xFC) << 3) | (b >> 3);
}

#endif /* GF_DISPLAY_DRIVER_H */
