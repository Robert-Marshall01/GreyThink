/**
 * @file power_state.h
 * @brief Power State Machine
 * 
 * WHAT: Comprehensive power management state machine implementing
 *       RUN → IDLE → SLEEP → DEEP SLEEP transitions with configurable
 *       wake sources, clock gating, and peripheral power domains.
 * 
 * WHY: Power efficiency is critical for battery-powered and energy-conscious
 *      embedded systems. Understanding low-power modes, wake source management,
 *      and power domain partitioning demonstrates expertise valuable in
 *      wearables, IoT sensors, and portable medical devices.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Consumer: Wearables, wireless earbuds, remote controls
 *   - Industrial: Wireless sensors, asset trackers
 *   - Medical: Continuous glucose monitors, portable diagnostics
 *   - Automotive: Key fobs, tire pressure sensors
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Power state machine design
 *   - Wake source configuration (GPIO, timer, RTC, UART)
 *   - Clock gating strategies
 *   - Peripheral power domains
 *   - Energy profiling hooks
 *   - Low-power scheduler integration
 * 
 * NOTE: This is the Phase 3 "spotlight" subsystem with full implementation.
 */

#ifndef GF_POWER_STATE_H
#define GF_POWER_STATE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Power State Definitions                                                    */
/*===========================================================================*/

/**
 * Power States (lowest to highest power consumption):
 * 
 * DEEP_SLEEP: ~1-10 µA
 *   - CPU halted, RAM retention optional
 *   - Only async wake sources (GPIO, RTC)
 *   - Full peripheral power-down
 *   - Wake time: 1-10 ms
 * 
 * SLEEP: ~10-100 µA
 *   - CPU halted, RAM retained
 *   - Timer wake sources available
 *   - Most peripherals powered down
 *   - Wake time: 100-500 µs
 * 
 * IDLE: ~1-5 mA
 *   - CPU halted, clocks running
 *   - All peripherals available
 *   - Fast wake on any interrupt
 *   - Wake time: <10 µs
 * 
 * RUN: Full power (~10-100 mA)
 *   - CPU active, all clocks running
 *   - Normal operation mode
 */

typedef enum {
    GF_POWER_STATE_RUN = 0,
    GF_POWER_STATE_IDLE,
    GF_POWER_STATE_SLEEP,
    GF_POWER_STATE_DEEP_SLEEP
} gf_power_state_t;

/*===========================================================================*/
/* Wake Source Configuration                                                  */
/*===========================================================================*/

typedef enum {
    GF_WAKE_GPIO        = (1 << 0),     /* GPIO pin edge/level */
    GF_WAKE_TIMER       = (1 << 1),     /* Low-power timer */
    GF_WAKE_RTC         = (1 << 2),     /* RTC alarm */
    GF_WAKE_UART        = (1 << 3),     /* UART RX start bit */
    GF_WAKE_I2C         = (1 << 4),     /* I2C address match */
    GF_WAKE_COMPARATOR  = (1 << 5),     /* Analog comparator */
    GF_WAKE_WATCHDOG    = (1 << 6),     /* Watchdog timeout */
    GF_WAKE_USB         = (1 << 7)      /* USB activity */
} gf_wake_source_t;

typedef enum {
    GF_WAKE_EDGE_RISING = 0,
    GF_WAKE_EDGE_FALLING,
    GF_WAKE_EDGE_BOTH,
    GF_WAKE_LEVEL_HIGH,
    GF_WAKE_LEVEL_LOW
} gf_wake_edge_t;

typedef struct {
    uint8_t             gpio_pin;
    gf_wake_edge_t      edge;
    bool                enable_pullup;
    bool                enable_pulldown;
} gf_wake_gpio_config_t;

typedef struct {
    uint32_t            timeout_ms;     /* Wake after this duration */
    bool                periodic;       /* Repeat vs one-shot */
} gf_wake_timer_config_t;

typedef struct {
    uint8_t             hour;
    uint8_t             minute;
    uint8_t             second;
    bool                daily_repeat;   /* Wake daily at this time */
} gf_wake_rtc_config_t;

/*===========================================================================*/
/* Clock and Power Domains                                                    */
/*===========================================================================*/

typedef enum {
    GF_CLOCK_CPU = 0,
    GF_CLOCK_BUS,
    GF_CLOCK_PERIPHERAL,
    GF_CLOCK_ADC,
    GF_CLOCK_TIMER,
    GF_CLOCK_DMA,
    GF_CLOCK_CRYPTO,
    GF_CLOCK_USB,
    GF_CLOCK_MAX
} gf_clock_domain_t;

typedef enum {
    GF_POWER_DOMAIN_CORE = 0,       /* CPU, RAM, essential logic */
    GF_POWER_DOMAIN_RADIO,          /* BLE/WiFi/LoRa radio */
    GF_POWER_DOMAIN_ANALOG,         /* ADC, DAC, comparators */
    GF_POWER_DOMAIN_DISPLAY,        /* LCD controller */
    GF_POWER_DOMAIN_SENSOR,         /* Sensor hub */
    GF_POWER_DOMAIN_HIGHSPEED,      /* USB, high-speed peripherals */
    GF_POWER_DOMAIN_MAX
} gf_power_domain_t;

/*===========================================================================*/
/* Configuration Structures                                                   */
/*===========================================================================*/

typedef struct {
    gf_power_state_t    min_state;          /* Deepest allowed state */
    uint32_t            idle_timeout_ms;    /* RUN → IDLE after idle */
    uint32_t            sleep_timeout_ms;   /* IDLE → SLEEP after idle */
    uint32_t            deep_sleep_timeout_ms;  /* SLEEP → DEEPSLEEP */
    bool                ram_retention;      /* Retain RAM in deep sleep */
    bool                enable_profiling;   /* Energy profiling hooks */
} gf_power_config_t;

/*===========================================================================*/
/* Status and Statistics                                                      */
/*===========================================================================*/

typedef struct {
    gf_power_state_t    current_state;
    gf_power_state_t    last_state;
    uint32_t            time_in_run_ms;
    uint32_t            time_in_idle_ms;
    uint32_t            time_in_sleep_ms;
    uint32_t            time_in_deep_sleep_ms;
    uint32_t            wake_count;
    gf_wake_source_t    last_wake_source;
    uint32_t            transition_count;
} gf_power_status_t;

/* Energy profiling snapshot */
typedef struct {
    uint32_t            timestamp_ms;
    gf_power_state_t    state;
    uint16_t            cpu_freq_mhz;
    uint8_t             active_domains;     /* Bitmask */
    uint8_t             active_clocks;      /* Bitmask */
    uint32_t            estimated_ua;       /* Estimated current draw */
} gf_power_profile_t;

/* Callbacks */
typedef void (*gf_power_transition_cb)(gf_power_state_t from, 
                                        gf_power_state_t to, void *ctx);
typedef void (*gf_power_wake_cb)(gf_wake_source_t source, void *ctx);
typedef void (*gf_power_profile_cb)(const gf_power_profile_t *profile, void *ctx);

/*===========================================================================*/
/* Primary API                                                                */
/*===========================================================================*/

/**
 * @brief Initialize power management subsystem
 */
int gf_power_init(const gf_power_config_t *config);

/**
 * @brief Request transition to power state
 * 
 * The system will transition when all locks are released.
 * Returns 0 on success, -1 if state not allowed.
 */
int gf_power_request_state(gf_power_state_t state);

/**
 * @brief Get current power state
 */
gf_power_state_t gf_power_get_state(void);

/**
 * @brief Lock system in current or higher power state
 * 
 * Prevents transition below the specified state until unlocked.
 * Returns lock handle, or 0 on failure.
 */
uint32_t gf_power_lock(gf_power_state_t min_state);

/**
 * @brief Release power lock
 */
int gf_power_unlock(uint32_t lock_handle);

/*===========================================================================*/
/* Wake Source Configuration                                                  */
/*===========================================================================*/

/**
 * @brief Configure GPIO wake source
 */
int gf_power_configure_wake_gpio(const gf_wake_gpio_config_t *config);

/**
 * @brief Configure timer wake source
 */
int gf_power_configure_wake_timer(const gf_wake_timer_config_t *config);

/**
 * @brief Configure RTC wake source
 */
int gf_power_configure_wake_rtc(const gf_wake_rtc_config_t *config);

/**
 * @brief Enable/disable wake source
 */
int gf_power_enable_wake_source(gf_wake_source_t source, bool enable);

/**
 * @brief Get last wake source after waking from sleep
 */
gf_wake_source_t gf_power_get_wake_source(void);

/*===========================================================================*/
/* Clock Gating                                                               */
/*===========================================================================*/

/**
 * @brief Enable/disable clock domain
 */
int gf_power_set_clock(gf_clock_domain_t clock, bool enable);

/**
 * @brief Set CPU frequency
 */
int gf_power_set_cpu_freq(uint32_t freq_hz);

/**
 * @brief Get current CPU frequency
 */
uint32_t gf_power_get_cpu_freq(void);

/*===========================================================================*/
/* Peripheral Power Domains                                                   */
/*===========================================================================*/

/**
 * @brief Power on/off peripheral domain
 */
int gf_power_set_domain(gf_power_domain_t domain, bool enable);

/**
 * @brief Check if domain is powered
 */
bool gf_power_is_domain_on(gf_power_domain_t domain);

/*===========================================================================*/
/* Status and Profiling                                                       */
/*===========================================================================*/

/**
 * @brief Get power management status
 */
void gf_power_get_status(gf_power_status_t *status);

/**
 * @brief Reset power statistics
 */
void gf_power_reset_stats(void);

/**
 * @brief Take energy profile snapshot
 */
void gf_power_take_profile_snapshot(gf_power_profile_t *profile);

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

/**
 * @brief Register state transition callback
 */
void gf_power_set_transition_callback(gf_power_transition_cb callback, void *ctx);

/**
 * @brief Register wake callback
 */
void gf_power_set_wake_callback(gf_power_wake_cb callback, void *ctx);

/**
 * @brief Register profiling callback (called periodically)
 */
void gf_power_set_profile_callback(gf_power_profile_cb callback, void *ctx);

/*===========================================================================*/
/* Low-Power Scheduler Integration                                            */
/*===========================================================================*/

/**
 * @brief Called by scheduler when idle
 * 
 * Evaluates pending work and enters appropriate low-power state.
 * Returns when woken by interrupt or wake source.
 */
void gf_power_enter_idle(void);

/**
 * @brief Called by scheduler before sleeping
 * 
 * Notifies power manager to prepare for sleep transition.
 * Returns maximum allowed sleep duration in ms.
 */
uint32_t gf_power_prepare_sleep(void);

/**
 * @brief Called by scheduler after wake
 * 
 * Restores clocks and peripherals after wake from sleep.
 */
void gf_power_restore_from_sleep(void);

/**
 * @brief Get driver descriptor
 */
const void* gf_power_get_driver(void);

#endif /* GF_POWER_STATE_H */
