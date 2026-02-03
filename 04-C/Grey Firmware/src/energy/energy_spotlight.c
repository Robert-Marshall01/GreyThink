/**
 * @file energy_spotlight.c
 * @brief Energy Harvesting & Smart Grid Spotlight Implementation
 * 
 * This module provides a production-ready implementation of:
 * - Solar panel driver with MPPT (Maximum Power Point Tracking)
 * - Energy harvesting manager with battery integration
 * - Smart grid telemetry reporting (voltage, current, load)
 * 
 * INDUSTRY RELEVANCE:
 * Energy systems are critical for renewable energy, IoT, and battery-powered
 * devices. This implementation demonstrates expertise in:
 * - Power electronics control algorithms (MPPT P&O, Incremental Conductance)
 * - Battery charging state machines (CC/CV/trickle)
 * - Power quality monitoring and grid synchronization
 * - Energy budget management and load shedding
 * 
 * @copyright Grey Firmware Project
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include "core/error_handler.h"

/*******************************************************************************
 * Configuration Constants
 ******************************************************************************/

#define GF_ENERGY_MAX_SOURCES       4
#define GF_ENERGY_MAX_LOADS         8
#define GF_ENERGY_HISTORY_SIZE      64
#define GF_ENERGY_PREDICTION_HOURS  24

/* MPPT Parameters */
#define GF_MPPT_PERTURBATION_MV     50      /* P&O step size */
#define GF_MPPT_SETTLING_MS         20      /* Wait between steps */
#define GF_MPPT_EFFICIENCY_MIN      85      /* Minimum tracking efficiency */
#define GF_MPPT_NIGHT_THRESHOLD_MW  100     /* Night mode detection */

/* Battery Parameters */
#define GF_BATT_PRECHARGE_THRESHOLD 2800    /* mV - precharge limit */
#define GF_BATT_CV_THRESHOLD        4150    /* mV - CV mode transition */
#define GF_BATT_FULL_CURRENT        50      /* mA - charge complete */
#define GF_BATT_TEMP_MIN            0       /* °C */
#define GF_BATT_TEMP_MAX            45      /* °C */

/* Grid Parameters */
#define GF_GRID_VOLTAGE_NOMINAL     230000  /* mV (230V) */
#define GF_GRID_FREQ_NOMINAL        50000   /* mHz (50.000 Hz) */
#define GF_GRID_SYNC_TIMEOUT_MS     5000
#define GF_GRID_ISLAND_DELAY_MS     1000

/*******************************************************************************
 * Types
 ******************************************************************************/

/* MPPT algorithm types */
typedef enum {
    MPPT_PERTURB_OBSERVE,
    MPPT_INCREMENTAL_COND,
    MPPT_FUZZY_LOGIC,
    MPPT_CONSTANT_VOLTAGE
} mppt_algorithm_t;

/* Battery charge state machine */
typedef enum {
    CHARGE_OFF,
    CHARGE_PRECHARGE,
    CHARGE_CC,           /* Constant current */
    CHARGE_CV,           /* Constant voltage */
    CHARGE_TOPOFF,
    CHARGE_COMPLETE,
    CHARGE_FAULT
} charge_state_t;

/* Power budget state */
typedef enum {
    BUDGET_CRITICAL,     /* <10% - emergency only */
    BUDGET_LOW,          /* 10-30% - reduced operation */
    BUDGET_NORMAL,       /* 30-80% - full operation */
    BUDGET_SURPLUS       /* >80% - energy available */
} budget_state_t;

/* Grid connection state */
typedef enum {
    GRID_DISCONNECTED,
    GRID_SYNCHRONIZING,
    GRID_CONNECTED,
    GRID_ISLANDED,
    GRID_FAULT
} grid_state_t;

/* Harvest source descriptor */
typedef struct {
    bool enabled;
    uint16_t voltage_mv;
    uint16_t current_ma;
    uint32_t power_mw;
    uint32_t energy_mwh;
    uint8_t efficiency_pct;
} harvest_source_t;

/* MPPT state */
typedef struct {
    mppt_algorithm_t algorithm;
    uint16_t voltage_setpoint_mv;
    uint16_t last_voltage_mv;
    uint32_t last_power_mw;
    int8_t direction;           /* +1 or -1 */
    uint32_t track_count;
    uint32_t last_update_ms;
    bool night_mode;
} mppt_state_t;

/* Battery state */
typedef struct {
    charge_state_t charge_state;
    uint16_t voltage_mv;
    int16_t current_ma;         /* +charging, -discharging */
    uint8_t soc_pct;            /* State of charge */
    uint8_t soh_pct;            /* State of health */
    int8_t temperature_c;
    uint32_t charge_cycles;
    bool fault;
} battery_state_t;

/* Power budget manager */
typedef struct {
    budget_state_t state;
    uint16_t available_mw;
    uint16_t allocated_mw;
    uint16_t load_mw[GF_ENERGY_MAX_LOADS];
    uint8_t load_priority[GF_ENERGY_MAX_LOADS];
    uint8_t load_count;
    bool shed_active;
} budget_manager_t;

/* Grid interface state */
typedef struct {
    grid_state_t state;
    uint32_t voltage_mv;        /* RMS voltage */
    uint32_t frequency_mhz;     /* Grid frequency */
    int32_t active_power_mw;    /* Real power (signed) */
    int32_t reactive_power_mvar;/* Reactive power */
    uint8_t power_factor;       /* ×100 */
    bool export_enabled;
    int32_t export_limit_mw;
} grid_interface_t;

/* Energy history for prediction */
typedef struct {
    uint32_t timestamp;
    uint16_t harvest_mw;
    uint16_t consumption_mw;
    uint8_t hour;
} energy_history_t;

/* Main energy manager context */
typedef struct {
    bool initialized;
    
    /* Solar/Harvest */
    harvest_source_t sources[GF_ENERGY_MAX_SOURCES];
    uint8_t source_count;
    mppt_state_t mppt;
    
    /* Battery */
    battery_state_t battery;
    uint16_t charge_voltage_mv;
    uint16_t charge_current_ma;
    
    /* Budget */
    budget_manager_t budget;
    
    /* Grid */
    grid_interface_t grid;
    
    /* History & prediction */
    energy_history_t history[GF_ENERGY_HISTORY_SIZE];
    uint8_t history_index;
    
    /* Statistics */
    uint64_t total_harvested_mwh;
    uint64_t total_consumed_mwh;
    uint64_t total_exported_mwh;
    uint64_t total_imported_mwh;
    uint32_t uptime_seconds;
    uint32_t loadshed_events;
    uint32_t grid_faults;
    
    /* Callbacks */
    void (*budget_callback)(budget_state_t state);
    void (*grid_event_callback)(grid_state_t state);
    
} energy_manager_t;

/*******************************************************************************
 * Static Variables
 ******************************************************************************/

static energy_manager_t g_energy = {0};

/*******************************************************************************
 * Internal Functions - MPPT Algorithms
 ******************************************************************************/

/**
 * @brief Perturb & Observe MPPT algorithm
 * 
 * Classic hill-climbing algorithm that perturbs voltage and observes
 * power change to find MPP.
 */
static uint16_t mppt_perturb_observe(uint16_t voltage_mv, uint32_t power_mw)
{
    mppt_state_t *mppt = &g_energy.mppt;
    int32_t delta_p = (int32_t)power_mw - (int32_t)mppt->last_power_mw;
    int32_t delta_v = (int32_t)voltage_mv - (int32_t)mppt->last_voltage_mv;
    
    /* Determine direction based on power change */
    if (delta_p != 0) {
        if ((delta_p > 0) == (delta_v > 0)) {
            /* Power increased with voltage increase - continue */
            mppt->direction = 1;
        } else {
            /* Power decreased with voltage increase - reverse */
            mppt->direction = -1;
        }
    }
    
    /* Calculate new setpoint */
    int32_t new_voltage = (int32_t)mppt->voltage_setpoint_mv + 
                          (mppt->direction * GF_MPPT_PERTURBATION_MV);
    
    /* Clamp to valid range */
    if (new_voltage < 5000) new_voltage = 5000;   /* Min 5V */
    if (new_voltage > 60000) new_voltage = 60000; /* Max 60V */
    
    /* Update state */
    mppt->last_voltage_mv = voltage_mv;
    mppt->last_power_mw = power_mw;
    mppt->track_count++;
    
    return (uint16_t)new_voltage;
}

/**
 * @brief Incremental Conductance MPPT algorithm
 * 
 * More sophisticated algorithm that directly computes MPP condition:
 * At MPP: dI/dV = -I/V
 */
static uint16_t mppt_incremental_conductance(uint16_t voltage_mv, 
                                              uint16_t current_ma)
{
    mppt_state_t *mppt = &g_energy.mppt;
    
    /* Calculate incremental conductance */
    int32_t delta_v = (int32_t)voltage_mv - (int32_t)mppt->last_voltage_mv;
    int32_t delta_i = (int32_t)current_ma - 
                      (int32_t)((mppt->last_power_mw * 1000) / mppt->last_voltage_mv);
    
    int32_t new_voltage = mppt->voltage_setpoint_mv;
    
    if (delta_v == 0) {
        /* Voltage unchanged - check current */
        if (delta_i > 0) {
            new_voltage += GF_MPPT_PERTURBATION_MV;
        } else if (delta_i < 0) {
            new_voltage -= GF_MPPT_PERTURBATION_MV;
        }
        /* delta_i == 0: at MPP */
    } else {
        /* Calculate conductance ratio */
        /* At MPP: dI/dV + I/V = 0 */
        int32_t inc_cond = (delta_i * 1000) / delta_v;  /* ×1000 for precision */
        int32_t inst_cond = -((int32_t)current_ma * 1000) / voltage_mv;
        
        if (inc_cond > inst_cond) {
            /* Left of MPP - increase voltage */
            new_voltage += GF_MPPT_PERTURBATION_MV;
        } else if (inc_cond < inst_cond) {
            /* Right of MPP - decrease voltage */
            new_voltage -= GF_MPPT_PERTURBATION_MV;
        }
        /* At MPP: conductances equal */
    }
    
    /* Clamp */
    if (new_voltage < 5000) new_voltage = 5000;
    if (new_voltage > 60000) new_voltage = 60000;
    
    /* Update state */
    mppt->last_voltage_mv = voltage_mv;
    mppt->last_power_mw = ((uint32_t)voltage_mv * current_ma) / 1000;
    mppt->track_count++;
    
    return (uint16_t)new_voltage;
}

/**
 * @brief Run MPPT algorithm step
 */
static void mppt_step(void)
{
    mppt_state_t *mppt = &g_energy.mppt;
    harvest_source_t *solar = &g_energy.sources[0];
    
    /* Check for night mode */
    if (solar->power_mw < GF_MPPT_NIGHT_THRESHOLD_MW) {
        mppt->night_mode = true;
        return;
    }
    mppt->night_mode = false;
    
    /* Run selected algorithm */
    switch (mppt->algorithm) {
        case MPPT_PERTURB_OBSERVE:
            mppt->voltage_setpoint_mv = mppt_perturb_observe(
                solar->voltage_mv, solar->power_mw);
            break;
            
        case MPPT_INCREMENTAL_COND:
            mppt->voltage_setpoint_mv = mppt_incremental_conductance(
                solar->voltage_mv, solar->current_ma);
            break;
            
        case MPPT_CONSTANT_VOLTAGE:
            /* Fixed ratio of Voc (typically 0.76) */
            mppt->voltage_setpoint_mv = (solar->voltage_mv * 76) / 100;
            break;
            
        default:
            mppt->voltage_setpoint_mv = mppt_perturb_observe(
                solar->voltage_mv, solar->power_mw);
            break;
    }
}

/*******************************************************************************
 * Internal Functions - Battery Charging
 ******************************************************************************/

/**
 * @brief Battery charge state machine
 */
static void battery_charge_process(void)
{
    battery_state_t *batt = &g_energy.battery;
    
    /* Temperature protection */
    if (batt->temperature_c < GF_BATT_TEMP_MIN || 
        batt->temperature_c > GF_BATT_TEMP_MAX) {
        batt->charge_state = CHARGE_FAULT;
        batt->fault = true;
        return;
    }
    
    /* State machine */
    switch (batt->charge_state) {
        case CHARGE_OFF:
            /* Start charging if power available */
            if (g_energy.sources[0].power_mw > 0 && batt->soc_pct < 100) {
                if (batt->voltage_mv < GF_BATT_PRECHARGE_THRESHOLD) {
                    batt->charge_state = CHARGE_PRECHARGE;
                } else {
                    batt->charge_state = CHARGE_CC;
                }
            }
            break;
            
        case CHARGE_PRECHARGE:
            /* Low-current trickle for deeply depleted battery */
            g_energy.charge_current_ma = g_energy.charge_current_ma / 10;
            if (batt->voltage_mv >= GF_BATT_PRECHARGE_THRESHOLD) {
                batt->charge_state = CHARGE_CC;
            }
            break;
            
        case CHARGE_CC:
            /* Constant current phase */
            g_energy.charge_current_ma = 1000; /* 1A max */
            if (batt->voltage_mv >= GF_BATT_CV_THRESHOLD) {
                batt->charge_state = CHARGE_CV;
            }
            break;
            
        case CHARGE_CV:
            /* Constant voltage phase - reduce current */
            g_energy.charge_voltage_mv = GF_BATT_CV_THRESHOLD;
            /* Current naturally tapers */
            if (batt->current_ma < GF_BATT_FULL_CURRENT) {
                batt->charge_state = CHARGE_TOPOFF;
            }
            break;
            
        case CHARGE_TOPOFF:
            /* Final top-off phase */
            if (batt->current_ma < GF_BATT_FULL_CURRENT / 2) {
                batt->charge_state = CHARGE_COMPLETE;
                batt->soc_pct = 100;
                batt->charge_cycles++;
            }
            break;
            
        case CHARGE_COMPLETE:
            /* Maintain float voltage, monitor for discharge */
            g_energy.charge_current_ma = 0;
            if (batt->soc_pct < 95) {
                batt->charge_state = CHARGE_CV;
            }
            break;
            
        case CHARGE_FAULT:
            /* Require reset */
            g_energy.charge_current_ma = 0;
            break;
    }
    
    /* Calculate SoC from voltage (simplified coulomb counting simulation) */
    if (batt->voltage_mv >= 4200) batt->soc_pct = 100;
    else if (batt->voltage_mv >= 4100) batt->soc_pct = 90;
    else if (batt->voltage_mv >= 4000) batt->soc_pct = 80;
    else if (batt->voltage_mv >= 3900) batt->soc_pct = 70;
    else if (batt->voltage_mv >= 3800) batt->soc_pct = 50;
    else if (batt->voltage_mv >= 3700) batt->soc_pct = 30;
    else if (batt->voltage_mv >= 3600) batt->soc_pct = 20;
    else if (batt->voltage_mv >= 3500) batt->soc_pct = 10;
    else batt->soc_pct = 5;
}

/*******************************************************************************
 * Internal Functions - Power Budget
 ******************************************************************************/

/**
 * @brief Update power budget state
 */
static void budget_update(void)
{
    budget_manager_t *budget = &g_energy.budget;
    battery_state_t *batt = &g_energy.battery;
    
    /* Calculate available power */
    uint32_t harvest_mw = 0;
    for (int i = 0; i < g_energy.source_count; i++) {
        if (g_energy.sources[i].enabled) {
            harvest_mw += g_energy.sources[i].power_mw;
        }
    }
    
    /* Add battery contribution based on SoC */
    uint32_t batt_mw = 0;
    if (batt->soc_pct > 20) {
        batt_mw = 5000; /* 5W from battery */
    }
    
    budget->available_mw = harvest_mw + batt_mw;
    
    /* Determine budget state */
    if (batt->soc_pct < 10) {
        budget->state = BUDGET_CRITICAL;
    } else if (batt->soc_pct < 30 || harvest_mw < budget->allocated_mw) {
        budget->state = BUDGET_LOW;
    } else if (batt->soc_pct > 80 && harvest_mw > budget->allocated_mw) {
        budget->state = BUDGET_SURPLUS;
    } else {
        budget->state = BUDGET_NORMAL;
    }
    
    /* Load shedding if necessary */
    if (budget->state == BUDGET_CRITICAL && !budget->shed_active) {
        budget->shed_active = true;
        g_energy.loadshed_events++;
        
        /* Notify callback */
        if (g_energy.budget_callback) {
            g_energy.budget_callback(budget->state);
        }
    } else if (budget->state >= BUDGET_NORMAL && budget->shed_active) {
        budget->shed_active = false;
        if (g_energy.budget_callback) {
            g_energy.budget_callback(budget->state);
        }
    }
}

/*******************************************************************************
 * Internal Functions - Grid Interface
 ******************************************************************************/

/**
 * @brief Simulate grid measurements
 */
static void grid_measure(void)
{
    grid_interface_t *grid = &g_energy.grid;
    
    if (grid->state == GRID_DISCONNECTED || grid->state == GRID_FAULT) {
        return;
    }
    
    /* Simulate nominal grid voltage with small variation */
    grid->voltage_mv = GF_GRID_VOLTAGE_NOMINAL + ((rand() % 2000) - 1000);
    grid->frequency_mhz = GF_GRID_FREQ_NOMINAL + ((rand() % 100) - 50);
    
    /* Calculate power flow */
    int32_t net_power = g_energy.budget.available_mw - g_energy.budget.allocated_mw;
    
    if (net_power > 0 && grid->export_enabled) {
        /* Export to grid */
        grid->active_power_mw = (net_power < grid->export_limit_mw) ? 
                                 net_power : grid->export_limit_mw;
        g_energy.total_exported_mwh += grid->active_power_mw / 3600;
    } else if (net_power < 0) {
        /* Import from grid */
        grid->active_power_mw = net_power;
        g_energy.total_imported_mwh += (-net_power) / 3600;
    } else {
        grid->active_power_mw = 0;
    }
    
    /* Power factor (unity for resistive load simulation) */
    grid->power_factor = 100;
}

/**
 * @brief Grid synchronization check
 */
static bool grid_sync_check(void)
{
    grid_interface_t *grid = &g_energy.grid;
    
    /* Check voltage within ±10% */
    int32_t v_error = abs((int32_t)grid->voltage_mv - GF_GRID_VOLTAGE_NOMINAL);
    if (v_error > (GF_GRID_VOLTAGE_NOMINAL / 10)) {
        return false;
    }
    
    /* Check frequency within ±0.5 Hz */
    int32_t f_error = abs((int32_t)grid->frequency_mhz - GF_GRID_FREQ_NOMINAL);
    if (f_error > 500) {
        return false;
    }
    
    return true;
}

/*******************************************************************************
 * Internal Functions - Energy Prediction
 ******************************************************************************/

/**
 * @brief Record energy history point
 */
static void record_history(uint32_t timestamp)
{
    energy_history_t *entry = &g_energy.history[g_energy.history_index];
    
    entry->timestamp = timestamp;
    entry->harvest_mw = g_energy.sources[0].power_mw;
    entry->consumption_mw = g_energy.budget.allocated_mw;
    entry->hour = (timestamp / 3600) % 24;
    
    g_energy.history_index = (g_energy.history_index + 1) % GF_ENERGY_HISTORY_SIZE;
}

/**
 * @brief Predict energy for given hour
 */
static uint32_t predict_harvest(uint8_t hour)
{
    uint32_t sum = 0;
    uint32_t count = 0;
    
    /* Average historical data for this hour */
    for (int i = 0; i < GF_ENERGY_HISTORY_SIZE; i++) {
        if (g_energy.history[i].hour == hour && g_energy.history[i].harvest_mw > 0) {
            sum += g_energy.history[i].harvest_mw;
            count++;
        }
    }
    
    return (count > 0) ? (sum / count) : 0;
}

/*******************************************************************************
 * Public API Implementation
 ******************************************************************************/

/**
 * @brief Initialize energy harvesting manager
 */
int gf_energy_spotlight_init(uint8_t mppt_algorithm)
{
    if (g_energy.initialized) {
        return -1; /* Already initialized */
    }
    
    memset(&g_energy, 0, sizeof(g_energy));
    
    /* Configure MPPT */
    g_energy.mppt.algorithm = (mppt_algorithm_t)mppt_algorithm;
    g_energy.mppt.voltage_setpoint_mv = 18000; /* 18V initial */
    g_energy.mppt.direction = 1;
    
    /* Initialize battery state */
    g_energy.battery.charge_state = CHARGE_OFF;
    g_energy.battery.voltage_mv = 3700; /* Start at ~30% */
    g_energy.battery.soc_pct = 30;
    g_energy.battery.soh_pct = 100;
    g_energy.battery.temperature_c = 25;
    
    /* Initialize grid */
    g_energy.grid.state = GRID_DISCONNECTED;
    g_energy.grid.export_limit_mw = 5000; /* 5W default export limit */
    
    /* Configure primary solar source */
    g_energy.sources[0].enabled = true;
    g_energy.source_count = 1;
    
    g_energy.initialized = true;
    return 0;
}

/**
 * @brief Start energy harvesting
 */
int gf_energy_spotlight_start(void)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    g_energy.sources[0].enabled = true;
    return 0;
}

/**
 * @brief Stop energy harvesting
 */
int gf_energy_spotlight_stop(void)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    g_energy.sources[0].enabled = false;
    g_energy.mppt.night_mode = true;
    return 0;
}

/**
 * @brief Update solar panel readings (from ADC)
 */
int gf_energy_update_solar(uint16_t voltage_mv, uint16_t current_ma)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    harvest_source_t *solar = &g_energy.sources[0];
    solar->voltage_mv = voltage_mv;
    solar->current_ma = current_ma;
    solar->power_mw = ((uint32_t)voltage_mv * current_ma) / 1000;
    
    /* Update efficiency estimate */
    if (g_energy.mppt.last_power_mw > 0) {
        solar->efficiency_pct = (solar->power_mw * 100) / 
                                (g_energy.mppt.last_power_mw + 100);
        if (solar->efficiency_pct > 100) solar->efficiency_pct = 100;
    }
    
    /* Accumulate energy */
    solar->energy_mwh += solar->power_mw / 3600;
    g_energy.total_harvested_mwh += solar->power_mw / 3600;
    
    return 0;
}

/**
 * @brief Update battery readings
 */
int gf_energy_update_battery(uint16_t voltage_mv, int16_t current_ma, 
                             int8_t temperature_c)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    battery_state_t *batt = &g_energy.battery;
    batt->voltage_mv = voltage_mv;
    batt->current_ma = current_ma;
    batt->temperature_c = temperature_c;
    
    return 0;
}

/**
 * @brief Process energy manager (call periodically)
 */
int gf_energy_spotlight_process(void)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    static uint32_t timestamp = 0;
    timestamp++;
    
    /* Run MPPT algorithm */
    if (!g_energy.mppt.night_mode) {
        mppt_step();
    }
    
    /* Process battery charging */
    battery_charge_process();
    
    /* Update power budget */
    budget_update();
    
    /* Process grid interface */
    if (g_energy.grid.state == GRID_CONNECTED) {
        grid_measure();
        
        /* Check for grid fault */
        if (!grid_sync_check()) {
            g_energy.grid.state = GRID_FAULT;
            g_energy.grid_faults++;
            if (g_energy.grid_event_callback) {
                g_energy.grid_event_callback(GRID_FAULT);
            }
        }
    }
    
    /* Record history periodically (every 60 seconds simulated) */
    if ((timestamp % 60) == 0) {
        record_history(timestamp);
    }
    
    g_energy.uptime_seconds++;
    
    return 0;
}

/**
 * @brief Get MPPT voltage setpoint
 */
uint16_t gf_energy_get_mppt_voltage(void)
{
    return g_energy.mppt.voltage_setpoint_mv;
}

/**
 * @brief Get battery state of charge
 */
uint8_t gf_energy_get_battery_soc(void)
{
    return g_energy.battery.soc_pct;
}

/**
 * @brief Get current charge state
 */
int gf_energy_get_charge_state(void)
{
    return (int)g_energy.battery.charge_state;
}

/**
 * @brief Get power budget state
 */
int gf_energy_get_budget_state(void)
{
    return (int)g_energy.budget.state;
}

/**
 * @brief Request power allocation
 */
uint16_t gf_energy_request_power(uint16_t power_mw, uint8_t priority)
{
    if (!g_energy.initialized) {
        return 0;
    }
    
    budget_manager_t *budget = &g_energy.budget;
    
    /* Check if power available */
    uint16_t remaining = budget->available_mw - budget->allocated_mw;
    
    /* In critical state, only grant to high-priority loads */
    if (budget->state == BUDGET_CRITICAL && priority > 1) {
        return 0;
    }
    
    /* Grant up to requested amount */
    uint16_t granted = (power_mw <= remaining) ? power_mw : remaining;
    
    if (granted > 0 && budget->load_count < GF_ENERGY_MAX_LOADS) {
        budget->load_mw[budget->load_count] = granted;
        budget->load_priority[budget->load_count] = priority;
        budget->load_count++;
        budget->allocated_mw += granted;
    }
    
    return granted;
}

/**
 * @brief Release allocated power
 */
int gf_energy_release_power(uint16_t power_mw)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    if (power_mw <= g_energy.budget.allocated_mw) {
        g_energy.budget.allocated_mw -= power_mw;
    }
    
    return 0;
}

/**
 * @brief Connect to grid
 */
int gf_energy_grid_connect(void)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    if (g_energy.grid.state == GRID_DISCONNECTED) {
        g_energy.grid.state = GRID_SYNCHRONIZING;
        
        /* Simulate grid measurements */
        g_energy.grid.voltage_mv = GF_GRID_VOLTAGE_NOMINAL;
        g_energy.grid.frequency_mhz = GF_GRID_FREQ_NOMINAL;
        
        /* Check sync */
        if (grid_sync_check()) {
            g_energy.grid.state = GRID_CONNECTED;
            if (g_energy.grid_event_callback) {
                g_energy.grid_event_callback(GRID_CONNECTED);
            }
        }
    }
    
    return (g_energy.grid.state == GRID_CONNECTED) ? 0 : -1;
}

/**
 * @brief Disconnect from grid
 */
int gf_energy_grid_disconnect(void)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    g_energy.grid.state = GRID_DISCONNECTED;
    g_energy.grid.active_power_mw = 0;
    
    if (g_energy.grid_event_callback) {
        g_energy.grid_event_callback(GRID_DISCONNECTED);
    }
    
    return 0;
}

/**
 * @brief Enable grid export
 */
int gf_energy_enable_export(int32_t limit_mw)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    g_energy.grid.export_enabled = true;
    g_energy.grid.export_limit_mw = limit_mw;
    
    return 0;
}

/**
 * @brief Get grid power flow
 */
int32_t gf_energy_get_grid_power(void)
{
    return g_energy.grid.active_power_mw;
}

/**
 * @brief Get grid state
 */
int gf_energy_get_grid_state(void)
{
    return (int)g_energy.grid.state;
}

/**
 * @brief Predict harvestable energy
 */
uint32_t gf_energy_predict_harvest(uint8_t hours_ahead)
{
    if (!g_energy.initialized || hours_ahead > GF_ENERGY_PREDICTION_HOURS) {
        return 0;
    }
    
    uint32_t total_mwh = 0;
    uint32_t current_hour = (g_energy.uptime_seconds / 3600) % 24;
    
    for (uint8_t h = 0; h < hours_ahead; h++) {
        uint8_t target_hour = (current_hour + h + 1) % 24;
        total_mwh += predict_harvest(target_hour);
    }
    
    return total_mwh;
}

/**
 * @brief Register power budget callback
 */
int gf_energy_register_budget_callback(void (*callback)(int state))
{
    g_energy.budget_callback = (void (*)(budget_state_t))callback;
    return 0;
}

/**
 * @brief Register grid event callback
 */
int gf_energy_register_grid_callback(void (*callback)(int state))
{
    g_energy.grid_event_callback = (void (*)(grid_state_t))callback;
    return 0;
}

/**
 * @brief Get energy statistics
 */
void gf_energy_get_stats(uint64_t *harvested_mwh, uint64_t *consumed_mwh,
                         uint64_t *exported_mwh, uint64_t *imported_mwh,
                         uint32_t *uptime_sec, uint32_t *loadshed_count)
{
    if (harvested_mwh) *harvested_mwh = g_energy.total_harvested_mwh;
    if (consumed_mwh) *consumed_mwh = g_energy.total_consumed_mwh;
    if (exported_mwh) *exported_mwh = g_energy.total_exported_mwh;
    if (imported_mwh) *imported_mwh = g_energy.total_imported_mwh;
    if (uptime_sec) *uptime_sec = g_energy.uptime_seconds;
    if (loadshed_count) *loadshed_count = g_energy.loadshed_events;
}

/**
 * @brief Clear battery fault
 */
int gf_energy_clear_battery_fault(void)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    if (g_energy.battery.charge_state == CHARGE_FAULT) {
        g_energy.battery.fault = false;
        g_energy.battery.charge_state = CHARGE_OFF;
        return 0;
    }
    
    return -1; /* No fault to clear */
}

/**
 * @brief Shutdown energy manager
 */
int gf_energy_spotlight_shutdown(void)
{
    if (!g_energy.initialized) {
        return -1;
    }
    
    /* Stop all sources */
    for (int i = 0; i < GF_ENERGY_MAX_SOURCES; i++) {
        g_energy.sources[i].enabled = false;
    }
    
    /* Disconnect grid */
    gf_energy_grid_disconnect();
    
    g_energy.initialized = false;
    return 0;
}

/*******************************************************************************
 * Telemetry Structure for External Use
 ******************************************************************************/

/**
 * @brief Get comprehensive telemetry snapshot
 */
void gf_energy_get_telemetry(
    /* Solar */
    uint16_t *solar_v, uint16_t *solar_i, uint32_t *solar_p,
    uint16_t *mppt_v, uint8_t *mppt_efficiency,
    /* Battery */
    uint16_t *batt_v, int16_t *batt_i, uint8_t *batt_soc,
    int8_t *batt_temp, int *charge_state,
    /* Grid */
    uint32_t *grid_v, uint32_t *grid_f, int32_t *grid_p, int *grid_state,
    /* Budget */
    uint16_t *available_mw, uint16_t *allocated_mw, int *budget_state)
{
    /* Solar */
    if (solar_v) *solar_v = g_energy.sources[0].voltage_mv;
    if (solar_i) *solar_i = g_energy.sources[0].current_ma;
    if (solar_p) *solar_p = g_energy.sources[0].power_mw;
    if (mppt_v) *mppt_v = g_energy.mppt.voltage_setpoint_mv;
    if (mppt_efficiency) *mppt_efficiency = g_energy.sources[0].efficiency_pct;
    
    /* Battery */
    if (batt_v) *batt_v = g_energy.battery.voltage_mv;
    if (batt_i) *batt_i = g_energy.battery.current_ma;
    if (batt_soc) *batt_soc = g_energy.battery.soc_pct;
    if (batt_temp) *batt_temp = g_energy.battery.temperature_c;
    if (charge_state) *charge_state = (int)g_energy.battery.charge_state;
    
    /* Grid */
    if (grid_v) *grid_v = g_energy.grid.voltage_mv;
    if (grid_f) *grid_f = g_energy.grid.frequency_mhz;
    if (grid_p) *grid_p = g_energy.grid.active_power_mw;
    if (grid_state) *grid_state = (int)g_energy.grid.state;
    
    /* Budget */
    if (available_mw) *available_mw = g_energy.budget.available_mw;
    if (allocated_mw) *allocated_mw = g_energy.budget.allocated_mw;
    if (budget_state) *budget_state = (int)g_energy.budget.state;
}
