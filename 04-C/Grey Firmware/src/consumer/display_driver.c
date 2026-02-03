/**
 * @file display_driver.c
 * @brief Display Driver Stub Implementation
 */

#include "consumer/display_driver.h"
#include "core/driver_registry.h"
#include <string.h>

static struct {
    gf_disp_config_t config;
    gf_disp_stats_t stats;
    uint32_t last_flush;
    bool initialized;
} s_disp;

/* Framebuffer would be allocated based on display size */
/* static uint16_t s_framebuffer[GF_DISP_MAX_WIDTH * GF_DISP_MAX_HEIGHT]; */

int gf_disp_init(const gf_disp_config_t *config) {
    if (!config) return -1;
    memcpy(&s_disp.config, config, sizeof(gf_disp_config_t));
    
    /* Would initialize SPI/I2C interface and send display init sequence */
    s_disp.initialized = true;
    return 0;
}

int gf_disp_set_brightness(uint8_t level) {
    (void)level;
    /* Would send brightness command or control backlight PWM */
    return 0;
}

int gf_disp_set_rotation(gf_disp_rotation_t rotation) {
    s_disp.config.rotation = rotation;
    /* Would send MADCTL or equivalent command */
    return 0;
}

void gf_disp_clear(gf_color_t color) {
    gf_rect_t r = {0, 0, s_disp.config.width, s_disp.config.height};
    gf_disp_fill_rect(&r, color);
}

void gf_disp_draw_pixel(int16_t x, int16_t y, gf_color_t color) {
    if (x < 0 || y < 0 || x >= s_disp.config.width || y >= s_disp.config.height)
        return;
    /* Write to framebuffer or directly to display */
    (void)color;
}

void gf_disp_draw_line(int16_t x0, int16_t y0, int16_t x1, int16_t y1, gf_color_t color) {
    /* Bresenham's line algorithm */
    int16_t dx = (x1 > x0) ? (x1 - x0) : (x0 - x1);
    int16_t dy = (y1 > y0) ? (y1 - y0) : (y0 - y1);
    int16_t sx = (x0 < x1) ? 1 : -1;
    int16_t sy = (y0 < y1) ? 1 : -1;
    int16_t err = dx - dy;
    
    while (1) {
        gf_disp_draw_pixel(x0, y0, color);
        if (x0 == x1 && y0 == y1) break;
        int16_t e2 = 2 * err;
        if (e2 > -dy) { err -= dy; x0 += sx; }
        if (e2 < dx)  { err += dx; y0 += sy; }
    }
}

void gf_disp_draw_rect(const gf_rect_t *rect, gf_color_t color) {
    if (!rect) return;
    gf_disp_draw_line(rect->x, rect->y, rect->x + rect->width - 1, rect->y, color);
    gf_disp_draw_line(rect->x, rect->y + rect->height - 1, 
                      rect->x + rect->width - 1, rect->y + rect->height - 1, color);
    gf_disp_draw_line(rect->x, rect->y, rect->x, rect->y + rect->height - 1, color);
    gf_disp_draw_line(rect->x + rect->width - 1, rect->y, 
                      rect->x + rect->width - 1, rect->y + rect->height - 1, color);
}

void gf_disp_fill_rect(const gf_rect_t *rect, gf_color_t color) {
    if (!rect) return;
    /* Would use hardware fill or DMA for performance */
    for (int16_t y = rect->y; y < rect->y + rect->height; y++) {
        for (int16_t x = rect->x; x < rect->x + rect->width; x++) {
            gf_disp_draw_pixel(x, y, color);
        }
    }
}

void gf_disp_draw_circle(int16_t cx, int16_t cy, uint16_t r, gf_color_t color) {
    /* Midpoint circle algorithm */
    int16_t x = r, y = 0, p = 1 - r;
    while (x >= y) {
        gf_disp_draw_pixel(cx + x, cy + y, color);
        gf_disp_draw_pixel(cx - x, cy + y, color);
        gf_disp_draw_pixel(cx + x, cy - y, color);
        gf_disp_draw_pixel(cx - x, cy - y, color);
        gf_disp_draw_pixel(cx + y, cy + x, color);
        gf_disp_draw_pixel(cx - y, cy + x, color);
        gf_disp_draw_pixel(cx + y, cy - x, color);
        gf_disp_draw_pixel(cx - y, cy - x, color);
        y++;
        if (p <= 0) p += 2*y + 1;
        else { x--; p += 2*y - 2*x + 1; }
    }
}

void gf_disp_fill_circle(int16_t cx, int16_t cy, uint16_t r, gf_color_t color) {
    for (int16_t y = -r; y <= r; y++) {
        for (int16_t x = -r; x <= r; x++) {
            if (x*x + y*y <= r*r) {
                gf_disp_draw_pixel(cx + x, cy + y, color);
            }
        }
    }
}

void gf_disp_draw_text(int16_t x, int16_t y, const char *text, 
                        uint8_t font_size, gf_color_t fg, gf_color_t bg) {
    (void)x; (void)y; (void)text; (void)font_size; (void)fg; (void)bg;
    /* Would render using font bitmap data */
}

uint16_t gf_disp_text_width(const char *text, uint8_t font_size) {
    if (!text) return 0;
    uint8_t char_width = (font_size == 0) ? 6 : (font_size == 1) ? 8 : 12;
    return strlen(text) * char_width;
}

void gf_disp_draw_bitmap(int16_t x, int16_t y, const uint8_t *bmp,
                          uint16_t w, uint16_t h, gf_color_t color) {
    (void)x; (void)y; (void)bmp; (void)w; (void)h; (void)color;
}

void gf_disp_draw_image(int16_t x, int16_t y, const uint16_t *img,
                         uint16_t w, uint16_t h) {
    (void)x; (void)y; (void)img; (void)w; (void)h;
}

void gf_disp_flush(void) {
    extern uint32_t gf_sched_get_ticks(void);
    uint32_t now = gf_sched_get_ticks();
    s_disp.stats.last_frame_ms = now - s_disp.last_flush;
    s_disp.stats.fps = 1000 / (s_disp.stats.last_frame_ms + 1);
    s_disp.last_flush = now;
    s_disp.stats.frames_rendered++;
}

bool gf_disp_is_busy(void) { return s_disp.stats.busy; }

void gf_disp_get_stats(gf_disp_stats_t *stats) {
    if (stats) memcpy(stats, &s_disp.stats, sizeof(gf_disp_stats_t));
}

static int disp_drv_init(void *cfg) { return gf_disp_init(cfg); }
static gf_driver_t s_disp_driver = {
    .name = "display", .version = 0x0100,
    .ops = { .init = disp_drv_init }
};
const void* gf_disp_get_driver(void) { return &s_disp_driver; }
