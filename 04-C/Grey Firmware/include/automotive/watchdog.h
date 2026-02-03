/**
 * @file watchdog.h
 * @brief Automotive Watchdog Stub
 * 
 * WHAT: Independent hardware watchdog with window mode support for
 *       safety-critical automotive applications.
 * 
 * WHY: Automotive safety standards (ISO 26262) require independent monitoring
 *      of MCU operation. A window watchdog detects both stuck software
 *      (timeout) and runaway code (early refresh). This is a mandatory
 *      component for any ASIL-rated system.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Window mode configuration (min/max refresh intervals)
 *      - Challenge-response refresh (prevents simple loop workarounds)
 *      - Integration with error handler for pre-reset logging
 *      - Runtime statistics for safety case evidence
 * 
 * Industry applications: engine control, braking systems, ADAS
 * 
 * NOTE: Annotated stub. Production requires hardware-specific implementation.
 */

#ifndef GF_WATCHDOG_H
#define GF_WATCHDOG_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_WDT_MIN_TIMEOUT_MS   10      /* Minimum timeout */
#define GF_WDT_MAX_TIMEOUT_MS   30000   /* Maximum timeout (30s) */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_WDT_MODE_STANDARD = 0,   /* Simple timeout mode */
    GF_WDT_MODE_WINDOW,         /* Window mode (min + max) */
    GF_WDT_MODE_CHALLENGE       /* Challenge-response mode */
} gf_wdt_mode_t;

typedef struct {
    gf_wdt_mode_t   mode;
    uint32_t        timeout_ms;     /* Max time before reset */
    uint32_t        window_min_ms;  /* Min time before refresh allowed */
    bool            lock_config;    /* Prevent runtime reconfiguration */
} gf_auto_wdt_config_t;

typedef struct {
    uint32_t    refresh_count;      /* Total refreshes */
    uint32_t    early_refresh;      /* Refreshes in closed window */
    uint32_t    late_refresh;       /* Refreshes after warning */
    uint32_t    last_refresh_ms;    /* Time of last refresh */
    uint32_t    max_interval_ms;    /* Longest interval seen */
    bool        active;
} gf_auto_wdt_stats_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize automotive watchdog
 */
int gf_auto_wdt_init(const gf_auto_wdt_config_t *config);

/**
 * @brief Start watchdog countdown
 */
int gf_auto_wdt_start(void);

/**
 * @brief Refresh watchdog (standard mode)
 */
int gf_auto_wdt_refresh(void);

/**
 * @brief Refresh with challenge response
 * @param response Expected response to current challenge
 */
int gf_auto_wdt_refresh_cr(uint32_t response);

/**
 * @brief Get current challenge value (for challenge-response mode)
 */
uint32_t gf_auto_wdt_get_challenge(void);

/**
 * @brief Get watchdog statistics
 */
void gf_auto_wdt_get_stats(gf_auto_wdt_stats_t *stats);

/**
 * @brief Get remaining time before timeout
 */
uint32_t gf_auto_wdt_remaining_ms(void);

/**
 * @brief Force watchdog reset (for testing)
 */
void gf_auto_wdt_force_reset(void);

/**
 * @brief Get watchdog driver descriptor
 */
const void* gf_auto_wdt_get_driver(void);

#endif /* GF_WATCHDOG_H */
