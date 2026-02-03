/**
 * @file watchdog.c
 * @brief Automotive Watchdog Stub Implementation
 */

#include "automotive/watchdog.h"
#include "core/error_handler.h"
#include "core/driver_registry.h"
#include <string.h>

static struct {
    gf_auto_wdt_config_t config;
    gf_auto_wdt_stats_t stats;
    uint32_t challenge;
    bool initialized;
} s_wdt;

/* Simple challenge-response: result = challenge XOR 0x5A5A5A5A */
static uint32_t calc_response(uint32_t challenge) {
    return challenge ^ 0x5A5A5A5A;
}

int gf_auto_wdt_init(const gf_auto_wdt_config_t *config) {
    if (!config) return -1;
    memcpy(&s_wdt.config, config, sizeof(gf_auto_wdt_config_t));
    memset(&s_wdt.stats, 0, sizeof(gf_auto_wdt_stats_t));
    s_wdt.initialized = true;
    return 0;
}

int gf_auto_wdt_start(void) {
    /* Hardware: Start watchdog timer */
    s_wdt.stats.active = true;
    return 0;
}

int gf_auto_wdt_refresh(void) {
    if (!s_wdt.stats.active) return -1;
    
    extern uint32_t gf_sched_get_ticks(void);
    uint32_t now = gf_sched_get_ticks();
    uint32_t interval = now - s_wdt.stats.last_refresh_ms;
    
    /* Window mode check */
    if (s_wdt.config.mode == GF_WDT_MODE_WINDOW) {
        if (interval < s_wdt.config.window_min_ms) {
            s_wdt.stats.early_refresh++;
            return -2; /* Refresh too early */
        }
    }
    
    /* Track statistics */
    if (interval > s_wdt.stats.max_interval_ms) {
        s_wdt.stats.max_interval_ms = interval;
    }
    s_wdt.stats.last_refresh_ms = now;
    s_wdt.stats.refresh_count++;
    
    /* Hardware: Refresh watchdog */
    return 0;
}

int gf_auto_wdt_refresh_cr(uint32_t response) {
    if (s_wdt.config.mode != GF_WDT_MODE_CHALLENGE) return -1;
    
    if (response != calc_response(s_wdt.challenge)) {
        return -2; /* Wrong response */
    }
    
    /* Generate new challenge */
    s_wdt.challenge = s_wdt.challenge * 1103515245 + 12345;
    
    return gf_auto_wdt_refresh();
}

uint32_t gf_auto_wdt_get_challenge(void) {
    return s_wdt.challenge;
}

void gf_auto_wdt_get_stats(gf_auto_wdt_stats_t *stats) {
    if (stats) memcpy(stats, &s_wdt.stats, sizeof(gf_auto_wdt_stats_t));
}

uint32_t gf_auto_wdt_remaining_ms(void) {
    /* Would read hardware counter */
    return s_wdt.config.timeout_ms;
}

void gf_auto_wdt_force_reset(void) {
    /* Intentionally don't refresh */
}

static int wdt_drv_init(void *config) { 
    return gf_auto_wdt_init((gf_auto_wdt_config_t*)config); 
}

static gf_driver_t s_wdt_driver = {
    .name = "auto_wdt",
    .version = 0x0100,
    .ops = { .init = wdt_drv_init }
};

const void* gf_auto_wdt_get_driver(void) {
    return &s_wdt_driver;
}
