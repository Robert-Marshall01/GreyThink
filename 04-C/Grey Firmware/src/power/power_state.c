/**
 * @file power_state.c
 * @brief Power State Machine Implementation
 * 
 * SPOTLIGHT SUBSYSTEM: Power Management
 * =====================================
 * 
 * This is a production-grade power management implementation demonstrating:
 *   - State machine design (RUN → IDLE → SLEEP → DEEP SLEEP)
 *   - Wake source configuration (GPIO, Timer, RTC)
 *   - Clock gating and power domain control
 *   - Energy profiling hooks
 *   - Low-power scheduler integration
 * 
 * Implementation follows ARM Cortex-M low-power patterns with abstraction
 * for portability across different MCU families.
 */

#include "power/power_state.h"
#include "core/error_handler.h"
#include "core/logging.h"
#include <string.h>

/*===========================================================================*/
/* Internal State                                                             */
/*===========================================================================*/

#define MAX_POWER_LOCKS         16
#define PROFILE_SAMPLE_PERIOD   100     /* ms between profile samples */

typedef struct {
    uint32_t            handle;
    gf_power_state_t    min_state;
    bool                active;
} power_lock_t;

typedef struct {
    /* Configuration */
    gf_power_config_t   config;
    bool                initialized;
    
    /* Current state */
    gf_power_state_t    current_state;
    gf_power_state_t    requested_state;
    gf_power_state_t    last_state;
    
    /* Wake sources */
    uint32_t            wake_sources_enabled;   /* Bitmask */
    gf_wake_source_t    last_wake_source;
    gf_wake_gpio_config_t gpio_wake_config;
    gf_wake_timer_config_t timer_wake_config;
    gf_wake_rtc_config_t rtc_wake_config;
    
    /* Clock and power domains */
    uint8_t             clock_enabled;          /* Bitmask */
    uint8_t             domain_enabled;         /* Bitmask */
    uint32_t            cpu_freq_hz;
    
    /* Power locks */
    power_lock_t        locks[MAX_POWER_LOCKS];
    uint32_t            next_lock_handle;
    
    /* Statistics */
    uint32_t            time_in_run_ms;
    uint32_t            time_in_idle_ms;
    uint32_t            time_in_sleep_ms;
    uint32_t            time_in_deep_sleep_ms;
    uint32_t            wake_count;
    uint32_t            transition_count;
    uint32_t            state_entry_time;
    
    /* Profiling */
    uint32_t            last_profile_time;
    
    /* Callbacks */
    gf_power_transition_cb transition_cb;
    void                   *transition_ctx;
    gf_power_wake_cb       wake_cb;
    void                   *wake_ctx;
    gf_power_profile_cb    profile_cb;
    void                   *profile_ctx;
    
} power_state_t;

static power_state_t g_power = {0};

/*===========================================================================*/
/* Simulated Hardware Interface                                               */
/*===========================================================================*/

/* 
 * NOTE: In a real implementation, these would interact with MCU registers.
 * For demonstration, we simulate the hardware behavior.
 */

/* Simulated system tick (milliseconds) */
static uint32_t sim_get_tick_ms(void) {
    static uint32_t tick = 0;
    return ++tick;  /* In real code: return HAL_GetTick() or similar */
}

/* Simulated low-power entry */
static void sim_enter_idle(void) {
    /* __WFI(); on ARM Cortex-M */
    /* CPU halts until interrupt */
}

__attribute__((unused))
static void sim_enter_sleep(void) {
    /* Configure SCB->SCR for SLEEPDEEP */
    /* Disable unneeded clocks */
    /* __WFI(); */
}

__attribute__((unused))
static void sim_enter_deep_sleep(void) {
    /* Configure for lowest power mode */
    /* Disable all peripherals except wake sources */
    /* Enter STOP/STANDBY mode */
    /* __WFI(); */
}

/* Simulated clock control */
static void sim_set_clock(gf_clock_domain_t clock, bool enable) {
    /* RCC->AHBxENR, APBxENR bit manipulation */
    (void)clock;
    (void)enable;
}

/* Simulated power domain control */
static void sim_set_power_domain(gf_power_domain_t domain, bool enable) {
    /* PWR->CR register manipulation */
    (void)domain;
    (void)enable;
}

/* Simulated wake source configuration */
static void sim_configure_gpio_wake(const gf_wake_gpio_config_t *config) {
    /* EXTI configuration */
    (void)config;
}

static void sim_configure_timer_wake(const gf_wake_timer_config_t *config) {
    /* LPTIM configuration */
    (void)config;
}

static void sim_configure_rtc_wake(const gf_wake_rtc_config_t *config) {
    /* RTC alarm configuration */
    (void)config;
}

/*===========================================================================*/
/* Internal Functions                                                         */
/*===========================================================================*/

/**
 * @brief Calculate minimum allowed state based on active locks
 */
static gf_power_state_t get_min_locked_state(void) {
    gf_power_state_t min_state = GF_POWER_STATE_DEEP_SLEEP;
    
    for (int i = 0; i < MAX_POWER_LOCKS; i++) {
        if (g_power.locks[i].active) {
            if (g_power.locks[i].min_state < min_state) {
                min_state = g_power.locks[i].min_state;
            }
        }
    }
    
    /* Also respect config minimum */
    if (g_power.config.min_state < min_state) {
        min_state = g_power.config.min_state;
    }
    
    return min_state;
}

/**
 * @brief Update time tracking when changing states
 */
static void update_state_time(gf_power_state_t old_state, uint32_t now) {
    uint32_t elapsed = now - g_power.state_entry_time;
    
    switch (old_state) {
        case GF_POWER_STATE_RUN:
            g_power.time_in_run_ms += elapsed;
            break;
        case GF_POWER_STATE_IDLE:
            g_power.time_in_idle_ms += elapsed;
            break;
        case GF_POWER_STATE_SLEEP:
            g_power.time_in_sleep_ms += elapsed;
            break;
        case GF_POWER_STATE_DEEP_SLEEP:
            g_power.time_in_deep_sleep_ms += elapsed;
            break;
    }
    
    g_power.state_entry_time = now;
}

/**
 * @brief Prepare hardware for low-power state
 */
static void prepare_for_state(gf_power_state_t state) {
    switch (state) {
        case GF_POWER_STATE_RUN:
            /* Restore all clocks and domains */
            for (int i = 0; i < GF_CLOCK_MAX; i++) {
                if (g_power.clock_enabled & (1 << i)) {
                    sim_set_clock((gf_clock_domain_t)i, true);
                }
            }
            for (int i = 0; i < GF_POWER_DOMAIN_MAX; i++) {
                if (g_power.domain_enabled & (1 << i)) {
                    sim_set_power_domain((gf_power_domain_t)i, true);
                }
            }
            break;
            
        case GF_POWER_STATE_IDLE:
            /* Keep peripheral clocks for fast wake */
            sim_set_clock(GF_CLOCK_CPU, true);
            sim_set_clock(GF_CLOCK_BUS, true);
            break;
            
        case GF_POWER_STATE_SLEEP:
            /* Disable non-essential clocks */
            sim_set_clock(GF_CLOCK_PERIPHERAL, false);
            sim_set_clock(GF_CLOCK_DMA, false);
            sim_set_clock(GF_CLOCK_USB, false);
            /* Keep timer for wake */
            sim_set_clock(GF_CLOCK_TIMER, true);
            break;
            
        case GF_POWER_STATE_DEEP_SLEEP:
            /* Disable all except RTC */
            for (int i = 0; i < GF_CLOCK_MAX; i++) {
                sim_set_clock((gf_clock_domain_t)i, false);
            }
            /* Only keep core domain for RAM retention */
            if (g_power.config.ram_retention) {
                sim_set_power_domain(GF_POWER_DOMAIN_CORE, true);
            }
            break;
    }
}

/**
 * @brief Execute state transition
 */
static int do_transition(gf_power_state_t new_state) {
    gf_power_state_t old_state = g_power.current_state;
    
    if (old_state == new_state) {
        return 0;
    }
    
    /* Check locks */
    gf_power_state_t min_allowed = get_min_locked_state();
    if (new_state > min_allowed) {
        /* Can't go deeper than locked state */
        new_state = min_allowed;
    }
    
    if (old_state == new_state) {
        return 0;
    }
    
    #ifdef GF_LOG_ENABLED
    static const char *state_names[] = {"RUN", "IDLE", "SLEEP", "DEEP_SLEEP"};
    GF_LOG_DEBUG("Power", "Transition: %s -> %s", 
                 state_names[old_state], state_names[new_state]);
    #endif
    
    /* Update timing */
    uint32_t now = sim_get_tick_ms();
    update_state_time(old_state, now);
    
    /* Notify callback before transition */
    if (g_power.transition_cb) {
        g_power.transition_cb(old_state, new_state, g_power.transition_ctx);
    }
    
    /* Prepare hardware */
    prepare_for_state(new_state);
    
    /* Update state */
    g_power.last_state = old_state;
    g_power.current_state = new_state;
    g_power.transition_count++;
    
    return 0;
}

/**
 * @brief Estimate current consumption for profiling
 */
static uint32_t estimate_current_ua(void) {
    uint32_t base_ua;
    
    switch (g_power.current_state) {
        case GF_POWER_STATE_RUN:
            base_ua = 50000;    /* 50 mA typical active */
            break;
        case GF_POWER_STATE_IDLE:
            base_ua = 2000;     /* 2 mA idle */
            break;
        case GF_POWER_STATE_SLEEP:
            base_ua = 50;       /* 50 µA sleep */
            break;
        case GF_POWER_STATE_DEEP_SLEEP:
            base_ua = 5;        /* 5 µA deep sleep */
            break;
        default:
            base_ua = 100000;
    }
    
    /* Add for active domains */
    if (g_power.domain_enabled & (1 << GF_POWER_DOMAIN_RADIO)) {
        base_ua += 15000;   /* Radio TX/RX */
    }
    if (g_power.domain_enabled & (1 << GF_POWER_DOMAIN_DISPLAY)) {
        base_ua += 5000;    /* Display */
    }
    if (g_power.domain_enabled & (1 << GF_POWER_DOMAIN_ANALOG)) {
        base_ua += 1000;    /* ADC */
    }
    
    return base_ua;
}

/*===========================================================================*/
/* Public API Implementation                                                  */
/*===========================================================================*/

int gf_power_init(const gf_power_config_t *config) {
    if (!config) {
        return -1;
    }
    
    memset(&g_power, 0, sizeof(g_power));
    memcpy(&g_power.config, config, sizeof(gf_power_config_t));
    
    /* Default state */
    g_power.current_state = GF_POWER_STATE_RUN;
    g_power.requested_state = GF_POWER_STATE_RUN;
    g_power.cpu_freq_hz = 48000000;     /* 48 MHz default */
    g_power.next_lock_handle = 1;
    g_power.state_entry_time = sim_get_tick_ms();
    
    /* Enable all clocks initially */
    g_power.clock_enabled = 0xFF;
    g_power.domain_enabled = (1 << GF_POWER_DOMAIN_CORE);
    
    g_power.initialized = true;
    
    #ifdef GF_LOG_ENABLED
    GF_LOG_INFO("Power", "Power manager initialized, min_state=%d", 
                config->min_state);
    #endif
    
    return 0;
}

int gf_power_request_state(gf_power_state_t state) {
    if (!g_power.initialized) {
        return -1;
    }
    
    if (state > GF_POWER_STATE_DEEP_SLEEP) {
        return -1;
    }
    
    g_power.requested_state = state;
    return do_transition(state);
}

gf_power_state_t gf_power_get_state(void) {
    return g_power.current_state;
}

uint32_t gf_power_lock(gf_power_state_t min_state) {
    if (!g_power.initialized) {
        return 0;
    }
    
    /* Find free slot */
    for (int i = 0; i < MAX_POWER_LOCKS; i++) {
        if (!g_power.locks[i].active) {
            g_power.locks[i].handle = g_power.next_lock_handle++;
            g_power.locks[i].min_state = min_state;
            g_power.locks[i].active = true;
            
            #ifdef GF_LOG_ENABLED
            GF_LOG_DEBUG("Power", "Lock acquired: handle=%u, min_state=%d",
                        g_power.locks[i].handle, min_state);
            #endif
            
            /* If we're deeper than allowed, come back up */
            if (g_power.current_state > min_state) {
                do_transition(min_state);
            }
            
            return g_power.locks[i].handle;
        }
    }
    
    gf_error_report(GF_SUBSYS_CORE, 0x10, GF_SEV_WARNING, "Power lock slots exhausted");
    return 0;  /* No free slots */
}

int gf_power_unlock(uint32_t lock_handle) {
    if (!g_power.initialized || lock_handle == 0) {
        return -1;
    }
    
    for (int i = 0; i < MAX_POWER_LOCKS; i++) {
        if (g_power.locks[i].active && 
            g_power.locks[i].handle == lock_handle) {
            g_power.locks[i].active = false;
            
            #ifdef GF_LOG_ENABLED
            GF_LOG_DEBUG("Power", "Lock released: handle=%u", lock_handle);
            #endif
            
            /* Try to go to requested deeper state if possible */
            if (g_power.requested_state > g_power.current_state) {
                do_transition(g_power.requested_state);
            }
            
            return 0;
        }
    }
    
    return -1;  /* Lock not found */
}

/*===========================================================================*/
/* Wake Source Configuration                                                  */
/*===========================================================================*/

int gf_power_configure_wake_gpio(const gf_wake_gpio_config_t *config) {
    if (!config) {
        return -1;
    }
    
    memcpy(&g_power.gpio_wake_config, config, sizeof(gf_wake_gpio_config_t));
    sim_configure_gpio_wake(config);
    
    #ifdef GF_LOG_ENABLED
    GF_LOG_DEBUG("Power", "GPIO wake configured: pin=%d, edge=%d",
                config->gpio_pin, config->edge);
    #endif
    
    return 0;
}

int gf_power_configure_wake_timer(const gf_wake_timer_config_t *config) {
    if (!config) {
        return -1;
    }
    
    memcpy(&g_power.timer_wake_config, config, sizeof(gf_wake_timer_config_t));
    sim_configure_timer_wake(config);
    
    #ifdef GF_LOG_ENABLED
    GF_LOG_DEBUG("Power", "Timer wake configured: timeout=%u ms, periodic=%d",
                config->timeout_ms, config->periodic);
    #endif
    
    return 0;
}

int gf_power_configure_wake_rtc(const gf_wake_rtc_config_t *config) {
    if (!config) {
        return -1;
    }
    
    memcpy(&g_power.rtc_wake_config, config, sizeof(gf_wake_rtc_config_t));
    sim_configure_rtc_wake(config);
    
    #ifdef GF_LOG_ENABLED
    GF_LOG_DEBUG("Power", "RTC wake configured: %02d:%02d:%02d",
                config->hour, config->minute, config->second);
    #endif
    
    return 0;
}

int gf_power_enable_wake_source(gf_wake_source_t source, bool enable) {
    if (enable) {
        g_power.wake_sources_enabled |= source;
    } else {
        g_power.wake_sources_enabled &= ~source;
    }
    return 0;
}

gf_wake_source_t gf_power_get_wake_source(void) {
    return g_power.last_wake_source;
}

/*===========================================================================*/
/* Clock and Domain Control                                                   */
/*===========================================================================*/

int gf_power_set_clock(gf_clock_domain_t clock, bool enable) {
    if (clock >= GF_CLOCK_MAX) {
        return -1;
    }
    
    if (enable) {
        g_power.clock_enabled |= (1 << clock);
    } else {
        g_power.clock_enabled &= ~(1 << clock);
    }
    
    sim_set_clock(clock, enable);
    return 0;
}

int gf_power_set_cpu_freq(uint32_t freq_hz) {
    /* In real implementation: configure PLL, flash wait states */
    g_power.cpu_freq_hz = freq_hz;
    
    #ifdef GF_LOG_ENABLED
    GF_LOG_INFO("Power", "CPU frequency set to %u Hz", freq_hz);
    #endif
    
    return 0;
}

uint32_t gf_power_get_cpu_freq(void) {
    return g_power.cpu_freq_hz;
}

int gf_power_set_domain(gf_power_domain_t domain, bool enable) {
    if (domain >= GF_POWER_DOMAIN_MAX) {
        return -1;
    }
    
    if (enable) {
        g_power.domain_enabled |= (1 << domain);
    } else {
        g_power.domain_enabled &= ~(1 << domain);
    }
    
    sim_set_power_domain(domain, enable);
    return 0;
}

bool gf_power_is_domain_on(gf_power_domain_t domain) {
    if (domain >= GF_POWER_DOMAIN_MAX) {
        return false;
    }
    return (g_power.domain_enabled & (1 << domain)) != 0;
}

/*===========================================================================*/
/* Status and Profiling                                                       */
/*===========================================================================*/

void gf_power_get_status(gf_power_status_t *status) {
    if (!status) {
        return;
    }
    
    /* Update current state time */
    uint32_t now = sim_get_tick_ms();
    uint32_t elapsed = now - g_power.state_entry_time;
    
    status->current_state = g_power.current_state;
    status->last_state = g_power.last_state;
    status->last_wake_source = g_power.last_wake_source;
    status->wake_count = g_power.wake_count;
    status->transition_count = g_power.transition_count;
    
    /* Add current elapsed to appropriate counter */
    status->time_in_run_ms = g_power.time_in_run_ms;
    status->time_in_idle_ms = g_power.time_in_idle_ms;
    status->time_in_sleep_ms = g_power.time_in_sleep_ms;
    status->time_in_deep_sleep_ms = g_power.time_in_deep_sleep_ms;
    
    switch (g_power.current_state) {
        case GF_POWER_STATE_RUN:
            status->time_in_run_ms += elapsed;
            break;
        case GF_POWER_STATE_IDLE:
            status->time_in_idle_ms += elapsed;
            break;
        case GF_POWER_STATE_SLEEP:
            status->time_in_sleep_ms += elapsed;
            break;
        case GF_POWER_STATE_DEEP_SLEEP:
            status->time_in_deep_sleep_ms += elapsed;
            break;
    }
}

void gf_power_reset_stats(void) {
    g_power.time_in_run_ms = 0;
    g_power.time_in_idle_ms = 0;
    g_power.time_in_sleep_ms = 0;
    g_power.time_in_deep_sleep_ms = 0;
    g_power.wake_count = 0;
    g_power.transition_count = 0;
    g_power.state_entry_time = sim_get_tick_ms();
}

void gf_power_take_profile_snapshot(gf_power_profile_t *profile) {
    if (!profile) {
        return;
    }
    
    profile->timestamp_ms = sim_get_tick_ms();
    profile->state = g_power.current_state;
    profile->cpu_freq_mhz = (uint16_t)(g_power.cpu_freq_hz / 1000000);
    profile->active_domains = g_power.domain_enabled;
    profile->active_clocks = g_power.clock_enabled;
    profile->estimated_ua = estimate_current_ua();
}

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

void gf_power_set_transition_callback(gf_power_transition_cb callback, void *ctx) {
    g_power.transition_cb = callback;
    g_power.transition_ctx = ctx;
}

void gf_power_set_wake_callback(gf_power_wake_cb callback, void *ctx) {
    g_power.wake_cb = callback;
    g_power.wake_ctx = ctx;
}

void gf_power_set_profile_callback(gf_power_profile_cb callback, void *ctx) {
    g_power.profile_cb = callback;
    g_power.profile_ctx = ctx;
}

/*===========================================================================*/
/* Scheduler Integration                                                      */
/*===========================================================================*/

void gf_power_enter_idle(void) {
    if (g_power.current_state != GF_POWER_STATE_IDLE) {
        do_transition(GF_POWER_STATE_IDLE);
    }
    
    /* Enter idle */
    sim_enter_idle();
    
    /* Woken by interrupt */
    g_power.wake_count++;
    do_transition(GF_POWER_STATE_RUN);
}

uint32_t gf_power_prepare_sleep(void) {
    /* Check what state we can enter */
    gf_power_state_t target = get_min_locked_state();
    
    if (target >= GF_POWER_STATE_SLEEP) {
        do_transition(GF_POWER_STATE_SLEEP);
        
        /* Calculate max sleep time based on wake timer */
        if (g_power.wake_sources_enabled & GF_WAKE_TIMER) {
            return g_power.timer_wake_config.timeout_ms;
        }
        return 0xFFFFFFFF;  /* Sleep until external wake */
    }
    
    return 0;  /* Can't sleep */
}

void gf_power_restore_from_sleep(void) {
    g_power.wake_count++;
    
    /* Notify wake callback */
    if (g_power.wake_cb) {
        g_power.wake_cb(g_power.last_wake_source, g_power.wake_ctx);
    }
    
    /* Return to run state */
    do_transition(GF_POWER_STATE_RUN);
    
    /* Trigger profiling if enabled */
    if (g_power.config.enable_profiling && g_power.profile_cb) {
        gf_power_profile_t profile;
        gf_power_take_profile_snapshot(&profile);
        g_power.profile_cb(&profile, g_power.profile_ctx);
    }
}

/*===========================================================================*/
/* Driver Descriptor                                                          */
/*===========================================================================*/

#include "core/driver_registry.h"

static int power_drv_init_wrapper(void *config) {
    (void)config;
    gf_power_config_t default_config = {
        .min_state = GF_POWER_STATE_DEEP_SLEEP,
        .idle_timeout_ms = 10,
        .sleep_timeout_ms = 1000,
        .deep_sleep_timeout_ms = 60000,
        .ram_retention = true,
        .enable_profiling = true
    };
    return gf_power_init(&default_config);
}

static int power_drv_deinit(void) {
    g_power.initialized = false;
    return 0;
}

static gf_driver_t g_power_driver = {
    .name = "power_mgr",
    .version = 0x0100,
    .capabilities = GF_DRV_CAP_POWER_MGMT,
    .state = GF_DRV_STATE_UNLOADED,
    .power_state = GF_DRV_POWER_ACTIVE,
    .ops = {
        .init = power_drv_init_wrapper,
        .deinit = power_drv_deinit,
        .suspend = NULL,
        .resume = NULL,
        .ioctl = NULL
    },
    .deps = {NULL},
    .private_data = NULL
};

const void* gf_power_get_driver(void) {
    return &g_power_driver;
}
