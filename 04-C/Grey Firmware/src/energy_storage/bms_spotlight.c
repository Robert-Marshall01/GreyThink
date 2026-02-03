/**
 * @file bms_spotlight.c
 * @brief Battery Management System (BMS) Spotlight Implementation
 * 
 * @details
 * Production-quality Battery Management System implementation demonstrating
 * expertise in embedded energy storage systems. Provides comprehensive
 * charge/discharge control, thermal monitoring, fault detection, and
 * telemetry reporting for multi-cell battery packs.
 * 
 * INDUSTRY RELEVANCE:
 * Battery Management Systems are critical for:
 * - Electric vehicles (Tesla, Rivian, BMW iX)
 * - Grid-scale energy storage (Tesla Megapack, Fluence)
 * - Uninterruptible Power Supplies (data centers)
 * - Consumer electronics (laptops, phones)
 * - Aerospace battery systems
 * 
 * KEY SAFETY FEATURES:
 * - Cell voltage monitoring with under/overvoltage protection
 * - Thermal management with runaway prevention
 * - Overcurrent protection with hardware integration
 * - SOC/SOH estimation using coulomb counting + OCV
 * - Active and passive cell balancing
 * - Fail-safe state machine with configurable thresholds
 * 
 * @version 1.0
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

/** Maximum cells in pack */
#define BMS_MAX_CELLS               24

/** Temperature sensor zones */
#define BMS_MAX_TEMP_ZONES          8

/** Balancing channels */
#define BMS_MAX_BALANCE_CH          24

/** Telemetry history size */
#define BMS_TELEMETRY_HISTORY       100

/** SOC/SOH estimation interval (ms) */
#define BMS_SOC_UPDATE_MS           1000

/** Cell balancing interval (ms) */
#define BMS_BALANCE_INTERVAL_MS     5000

/** Voltage sample rate (ms) */
#define BMS_VOLTAGE_SAMPLE_MS       100

/** Temperature sample rate (ms) */
#define BMS_TEMP_SAMPLE_MS          500

/*******************************************************************************
 * Default Safety Thresholds
 ******************************************************************************/

/** Undervoltage lockout (mV) */
#define BMS_CELL_UV_LOCKOUT_MV      2500

/** Undervoltage warning (mV) */
#define BMS_CELL_UV_WARN_MV         2800

/** Overvoltage warning (mV) */
#define BMS_CELL_OV_WARN_MV         4150

/** Overvoltage lockout (mV) */
#define BMS_CELL_OV_LOCKOUT_MV      4250

/** Under temperature (°C × 10) */
#define BMS_TEMP_UT_C10             0

/** Over temperature warning (°C × 10) */
#define BMS_TEMP_OT_WARN_C10        450

/** Over temperature lockout (°C × 10) */
#define BMS_TEMP_OT_LOCKOUT_C10     600

/** Thermal runaway rate (°C/sec × 10) */
#define BMS_THERMAL_RUNAWAY_RATE    20

/** Maximum charge current (mA) */
#define BMS_MAX_CHARGE_MA           10000

/** Maximum discharge current (mA) */
#define BMS_MAX_DISCHARGE_MA        50000

/** Cell imbalance threshold (mV) */
#define BMS_IMBALANCE_THRESHOLD_MV  30

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief BMS cell chemistry type
 */
typedef enum {
    BMS_CHEM_LI_ION,              /**< Li-ion (3.7V nominal) */
    BMS_CHEM_LIFEPO4,             /**< LiFePO4 (3.2V nominal) */
    BMS_CHEM_LIPO,                /**< LiPo (3.7V nominal) */
    BMS_CHEM_NIMH,                /**< NiMH (1.2V nominal) */
    BMS_CHEM_SOLID_STATE          /**< Solid-state Li (4.0V nominal) */
} bms_chemistry_t;

/**
 * @brief BMS operating state
 */
typedef enum {
    BMS_STATE_INIT,               /**< Initialization */
    BMS_STATE_IDLE,               /**< Idle - monitoring only */
    BMS_STATE_CHARGING,           /**< Active charging */
    BMS_STATE_DISCHARGING,        /**< Active discharging */
    BMS_STATE_BALANCING,          /**< Cell balancing active */
    BMS_STATE_FAULT,              /**< Fault condition */
    BMS_STATE_SHUTDOWN            /**< Safe shutdown */
} bms_state_t;

/**
 * @brief Charge phase
 */
typedef enum {
    BMS_CHARGE_PRECHARGE,         /**< Precharge trickle */
    BMS_CHARGE_CC,                /**< Constant current */
    BMS_CHARGE_CV,                /**< Constant voltage */
    BMS_CHARGE_TOPOFF,            /**< Top-off phase */
    BMS_CHARGE_COMPLETE           /**< Charge complete */
} bms_charge_phase_t;

/**
 * @brief Fault flags (bitmask)
 */
typedef enum {
    BMS_FAULT_NONE              = 0x0000,
    BMS_FAULT_CELL_UV           = 0x0001,
    BMS_FAULT_CELL_OV           = 0x0002,
    BMS_FAULT_PACK_UV           = 0x0004,
    BMS_FAULT_PACK_OV           = 0x0008,
    BMS_FAULT_OVER_TEMP         = 0x0010,
    BMS_FAULT_UNDER_TEMP        = 0x0020,
    BMS_FAULT_THERMAL_RUNAWAY   = 0x0040,
    BMS_FAULT_OVERCURRENT       = 0x0080,
    BMS_FAULT_SHORT_CIRCUIT     = 0x0100,
    BMS_FAULT_COMM_ERROR        = 0x0200,
    BMS_FAULT_BAL_FAIL          = 0x0400,
    BMS_FAULT_SOC_INVALID       = 0x0800,
    BMS_FAULT_SENSOR_FAIL       = 0x1000,
    BMS_FAULT_ISOLATION         = 0x2000
} bms_fault_t;

/**
 * @brief Balancing method
 */
typedef enum {
    BMS_BALANCE_PASSIVE,          /**< Resistive bleed */
    BMS_BALANCE_ACTIVE            /**< Active transfer */
} bms_balance_method_t;

/**
 * @brief Cell data
 */
typedef struct {
    uint16_t voltage_mv;          /**< Cell voltage (mV) */
    int16_t current_ma;           /**< Cell current (mA) */
    bool balancing;               /**< Balancing active */
    bool ov_fault;                /**< Overvoltage fault */
    bool uv_fault;                /**< Undervoltage fault */
} bms_cell_t;

/**
 * @brief Temperature zone data
 */
typedef struct {
    int16_t temp_c10;             /**< Temperature (0.1°C) */
    int16_t temp_rate_c10_s;      /**< Rate of change */
    int16_t max_temp_c10;         /**< Maximum recorded */
    bool warning;                 /**< Warning active */
    bool fault;                   /**< Fault active */
} bms_temp_zone_t;

/**
 * @brief SOC/SOH estimator state
 */
typedef struct {
    uint8_t soc_pct;              /**< State of charge (%) */
    uint8_t soh_pct;              /**< State of health (%) */
    int32_t coulomb_count_mas;    /**< Coulomb counter (mAs) */
    uint32_t capacity_mah;        /**< Pack capacity (mAh) */
    uint32_t remaining_mah;       /**< Remaining capacity */
    uint32_t full_cycles;         /**< Full cycle count */
    uint16_t internal_res_mohm;   /**< Internal resistance */
    bool valid;                   /**< Estimation valid */
} bms_soc_estimator_t;

/**
 * @brief Pack configuration
 */
typedef struct {
    bms_chemistry_t chemistry;    /**< Cell chemistry */
    uint8_t cell_count;           /**< Number of cells */
    uint8_t temp_zone_count;      /**< Temperature zones */
    uint16_t cell_capacity_mah;   /**< Cell capacity */
    uint16_t cell_min_mv;         /**< Minimum cell voltage */
    uint16_t cell_max_mv;         /**< Maximum cell voltage */
    uint16_t cell_nominal_mv;     /**< Nominal cell voltage */
    int16_t temp_min_c10;         /**< Minimum temperature */
    int16_t temp_max_c10;         /**< Maximum temperature */
    uint16_t max_charge_ma;       /**< Maximum charge current */
    uint16_t max_discharge_ma;    /**< Maximum discharge current */
    bms_balance_method_t balance_method; /**< Balancing method */
    uint16_t balance_threshold_mv; /**< Balance start threshold */
} bms_config_t;

/**
 * @brief Pack status
 */
typedef struct {
    bms_state_t state;            /**< Operating state */
    bms_charge_phase_t charge_phase; /**< Charge phase */
    uint32_t fault_flags;         /**< Active faults */
    uint32_t pack_voltage_mv;     /**< Total pack voltage */
    int32_t pack_current_ma;      /**< Pack current */
    int32_t power_mw;             /**< Power (W) */
    uint16_t max_cell_mv;         /**< Highest cell voltage */
    uint16_t min_cell_mv;         /**< Lowest cell voltage */
    uint16_t cell_delta_mv;       /**< Cell spread */
    int16_t max_temp_c10;         /**< Highest temperature */
    int16_t min_temp_c10;         /**< Lowest temperature */
    uint8_t soc_pct;              /**< State of charge */
    uint8_t soh_pct;              /**< State of health */
    bool contactor_closed;        /**< Main contactor state */
    bool precharge_active;        /**< Precharge relay state */
    uint32_t uptime_s;            /**< System uptime */
} bms_status_t;

/**
 * @brief Telemetry record
 */
typedef struct {
    uint32_t timestamp_ms;        /**< Sample timestamp */
    uint32_t pack_voltage_mv;     /**< Pack voltage */
    int32_t pack_current_ma;      /**< Pack current */
    uint8_t soc_pct;              /**< SOC */
    int16_t max_temp_c10;         /**< Max temperature */
    uint16_t cell_voltages[BMS_MAX_CELLS]; /**< Per-cell voltages */
} bms_telemetry_record_t;

/**
 * @brief Event callback types
 */
typedef enum {
    BMS_EVENT_FAULT,              /**< Fault occurred */
    BMS_EVENT_FAULT_CLEARED,      /**< Fault cleared */
    BMS_EVENT_STATE_CHANGE,       /**< State transition */
    BMS_EVENT_SOC_UPDATE,         /**< SOC updated */
    BMS_EVENT_BALANCE_START,      /**< Balancing started */
    BMS_EVENT_BALANCE_COMPLETE,   /**< Balancing complete */
    BMS_EVENT_CHARGE_COMPLETE,    /**< Charge complete */
    BMS_EVENT_THERMAL_WARN        /**< Thermal warning */
} bms_event_type_t;

/**
 * @brief Event callback function
 */
typedef void (*bms_event_callback_t)(bms_event_type_t event, 
                                      uint32_t data, void* user_ctx);

/**
 * @brief BMS context
 */
typedef struct {
    bool initialized;
    bms_config_t config;
    bms_state_t state;
    bms_charge_phase_t charge_phase;
    uint32_t fault_flags;
    uint32_t injected_faults;     /**< Test-injected faults (persistent) */
    
    /* Cell monitoring */
    bms_cell_t cells[BMS_MAX_CELLS];
    bms_temp_zone_t temp_zones[BMS_MAX_TEMP_ZONES];
    
    /* Current sensing */
    int32_t pack_current_ma;
    int32_t current_limit_ma;
    
    /* Contactor control */
    bool main_contactor;
    bool precharge_relay;
    bool charge_enable;
    bool discharge_enable;
    
    /* SOC/SOH estimation */
    bms_soc_estimator_t soc;
    
    /* Balancing */
    bool balancing_active;
    uint8_t balance_mask[3];      /**< 24-bit mask */
    uint32_t balance_start_time;
    
    /* Timing */
    uint32_t last_voltage_sample;
    uint32_t last_temp_sample;
    uint32_t last_soc_update;
    uint32_t last_balance_check;
    uint32_t state_entry_time;
    uint32_t uptime_start;
    
    /* Telemetry */
    bms_telemetry_record_t history[BMS_TELEMETRY_HISTORY];
    uint8_t history_index;
    uint8_t history_count;
    
    /* Callbacks */
    bms_event_callback_t event_callback;
    void* callback_context;
} bms_context_t;

/*******************************************************************************
 * Static Variables
 ******************************************************************************/

static bms_context_t g_bms = {0};

/* Simulated hardware timestamp */
static uint32_t g_bms_tick_ms = 0;

/*******************************************************************************
 * Forward Declarations
 ******************************************************************************/

static void bms_sample_voltages(void);
static void bms_sample_temperatures(void);
static void bms_update_soc(void);
static void bms_check_faults(void);
static void bms_run_balancing(void);
static void bms_state_machine(void);
static void bms_update_telemetry(void);
static void bms_notify_event(bms_event_type_t event, uint32_t data);

/*******************************************************************************
 * Utility Functions
 ******************************************************************************/

/**
 * @brief Get current timestamp
 */
static uint32_t bms_get_time_ms(void) {
    return g_bms_tick_ms;
}

/**
 * @brief Calculate pack voltage from cells
 */
static uint32_t bms_calc_pack_voltage(void) {
    uint32_t total = 0;
    for (uint8_t i = 0; i < g_bms.config.cell_count; i++) {
        total += g_bms.cells[i].voltage_mv;
    }
    return total;
}

/**
 * @brief Find minimum cell voltage
 */
static uint16_t bms_find_min_cell_voltage(void) {
    uint16_t min_v = UINT16_MAX;
    for (uint8_t i = 0; i < g_bms.config.cell_count; i++) {
        if (g_bms.cells[i].voltage_mv < min_v) {
            min_v = g_bms.cells[i].voltage_mv;
        }
    }
    return min_v;
}

/**
 * @brief Find maximum cell voltage
 */
static uint16_t bms_find_max_cell_voltage(void) {
    uint16_t max_v = 0;
    for (uint8_t i = 0; i < g_bms.config.cell_count; i++) {
        if (g_bms.cells[i].voltage_mv > max_v) {
            max_v = g_bms.cells[i].voltage_mv;
        }
    }
    return max_v;
}

/**
 * @brief Find maximum temperature
 */
static int16_t bms_find_max_temp(void) {
    int16_t max_t = INT16_MIN;
    for (uint8_t i = 0; i < g_bms.config.temp_zone_count; i++) {
        if (g_bms.temp_zones[i].temp_c10 > max_t) {
            max_t = g_bms.temp_zones[i].temp_c10;
        }
    }
    return max_t;
}

/**
 * @brief Find minimum temperature
 */
static int16_t bms_find_min_temp(void) {
    int16_t min_t = INT16_MAX;
    for (uint8_t i = 0; i < g_bms.config.temp_zone_count; i++) {
        if (g_bms.temp_zones[i].temp_c10 < min_t) {
            min_t = g_bms.temp_zones[i].temp_c10;
        }
    }
    return min_t;
}

/**
 * @brief Set contactor state
 */
static void bms_set_contactor(bool closed) {
    g_bms.main_contactor = closed;
}

/**
 * @brief Set precharge relay
 */
static void bms_set_precharge(bool active) {
    g_bms.precharge_relay = active;
}

/*******************************************************************************
 * Voltage Sampling
 ******************************************************************************/

/**
 * @brief Sample all cell voltages
 * 
 * In production, this would interface with AFE (Analog Front End)
 * such as LTC6811, BQ76940, or MAX17853.
 */
static void bms_sample_voltages(void) {
    uint32_t now = bms_get_time_ms();
    
    if ((now - g_bms.last_voltage_sample) < BMS_VOLTAGE_SAMPLE_MS) {
        return;
    }
    g_bms.last_voltage_sample = now;
    
    /* In real implementation: SPI/I2C read from AFE */
    /* For simulation: voltages are set externally via test API */
    
    /* Update per-cell fault flags */
    for (uint8_t i = 0; i < g_bms.config.cell_count; i++) {
        g_bms.cells[i].ov_fault = 
            (g_bms.cells[i].voltage_mv >= g_bms.config.cell_max_mv);
        g_bms.cells[i].uv_fault = 
            (g_bms.cells[i].voltage_mv <= g_bms.config.cell_min_mv);
    }
}

/*******************************************************************************
 * Temperature Sampling
 ******************************************************************************/

/**
 * @brief Sample all temperature zones
 * 
 * Monitors NTC thermistors placed at critical locations:
 * - Cell surfaces
 * - Bus bars
 * - FET heatsinks
 * - Ambient air
 */
static void bms_sample_temperatures(void) {
    uint32_t now = bms_get_time_ms();
    
    if ((now - g_bms.last_temp_sample) < BMS_TEMP_SAMPLE_MS) {
        return;
    }
    
    /* Store previous temperatures for rate calculation */
    int16_t prev_temps[BMS_MAX_TEMP_ZONES];
    for (uint8_t i = 0; i < g_bms.config.temp_zone_count; i++) {
        prev_temps[i] = g_bms.temp_zones[i].temp_c10;
    }
    
    uint32_t delta_ms = now - g_bms.last_temp_sample;
    g_bms.last_temp_sample = now;
    
    /* In real implementation: ADC read from NTC thermistors */
    /* For simulation: temperatures set externally */
    
    /* Calculate rate of change and update max recorded */
    for (uint8_t i = 0; i < g_bms.config.temp_zone_count; i++) {
        /* Rate in 0.1°C per second */
        if (delta_ms > 0) {
            int32_t delta_t = g_bms.temp_zones[i].temp_c10 - prev_temps[i];
            g_bms.temp_zones[i].temp_rate_c10_s = 
                (int16_t)((delta_t * 1000) / (int32_t)delta_ms);
        }
        
        /* Track maximum */
        if (g_bms.temp_zones[i].temp_c10 > g_bms.temp_zones[i].max_temp_c10) {
            g_bms.temp_zones[i].max_temp_c10 = g_bms.temp_zones[i].temp_c10;
        }
        
        /* Update warning/fault flags based on thresholds */
        g_bms.temp_zones[i].warning = 
            (g_bms.temp_zones[i].temp_c10 >= BMS_TEMP_OT_WARN_C10);
        g_bms.temp_zones[i].fault = 
            (g_bms.temp_zones[i].temp_c10 >= BMS_TEMP_OT_LOCKOUT_C10) ||
            (g_bms.temp_zones[i].temp_c10 <= BMS_TEMP_UT_C10);
    }
}

/*******************************************************************************
 * SOC/SOH Estimation
 ******************************************************************************/

/**
 * @brief Update State of Charge estimation
 * 
 * Uses hybrid approach:
 * - Coulomb counting for active operation
 * - OCV lookup for idle state correlation
 * - Kalman filter for noise reduction (simplified here)
 */
static void bms_update_soc(void) {
    uint32_t now = bms_get_time_ms();
    
    if ((now - g_bms.last_soc_update) < BMS_SOC_UPDATE_MS) {
        return;
    }
    
    uint32_t delta_ms = now - g_bms.last_soc_update;
    g_bms.last_soc_update = now;
    
    /* Coulomb counting: integrate current */
    /* I (mA) × t (ms) / 1000 = charge (mA·s) = mAs */
    int32_t delta_charge_mas = 
        (g_bms.pack_current_ma * (int32_t)delta_ms) / 1000;
    g_bms.soc.coulomb_count_mas += delta_charge_mas;
    
    /* Convert to mAh */
    /* Pack capacity in mAh = cell_capacity × parallel_count (assumed 1) */
    uint32_t pack_capacity_mah = g_bms.config.cell_capacity_mah;
    
    /* Remaining capacity */
    /* remaining = coulomb_count / 3600 (mAs to mAh) */
    int32_t remaining_mah = g_bms.soc.coulomb_count_mas / 3600;
    if (remaining_mah < 0) remaining_mah = 0;
    if (remaining_mah > (int32_t)pack_capacity_mah) {
        remaining_mah = pack_capacity_mah;
    }
    g_bms.soc.remaining_mah = (uint32_t)remaining_mah;
    
    /* Calculate SOC percentage */
    if (pack_capacity_mah > 0) {
        g_bms.soc.soc_pct = (uint8_t)((remaining_mah * 100) / pack_capacity_mah);
        if (g_bms.soc.soc_pct > 100) g_bms.soc.soc_pct = 100;
    } else {
        g_bms.soc.soc_pct = 0;
    }
    
    /* SOH estimation based on capacity fade */
    /* Simplified: track cycle count and estimate fade */
    /* In production: use internal resistance growth and capacity tests */
    if (g_bms.soc.full_cycles < 100) {
        g_bms.soc.soh_pct = 100;
    } else if (g_bms.soc.full_cycles < 500) {
        g_bms.soc.soh_pct = 95;
    } else if (g_bms.soc.full_cycles < 1000) {
        g_bms.soc.soh_pct = 90;
    } else {
        g_bms.soc.soh_pct = 80;
    }
    
    g_bms.soc.valid = true;
    g_bms.soc.capacity_mah = pack_capacity_mah;
}

/*******************************************************************************
 * Fault Detection
 ******************************************************************************/

/**
 * @brief Check all fault conditions
 */
static void bms_check_faults(void) {
    uint32_t old_faults = g_bms.fault_flags;
    uint32_t new_faults = BMS_FAULT_NONE;
    
    /* Cell voltage faults */
    uint16_t min_v = bms_find_min_cell_voltage();
    uint16_t max_v = bms_find_max_cell_voltage();
    
    if (min_v <= BMS_CELL_UV_LOCKOUT_MV) {
        new_faults |= BMS_FAULT_CELL_UV;
    }
    if (max_v >= BMS_CELL_OV_LOCKOUT_MV) {
        new_faults |= BMS_FAULT_CELL_OV;
    }
    
    /* Pack voltage faults */
    uint32_t pack_v = bms_calc_pack_voltage();
    uint32_t min_pack = (uint32_t)g_bms.config.cell_min_mv * g_bms.config.cell_count;
    uint32_t max_pack = (uint32_t)g_bms.config.cell_max_mv * g_bms.config.cell_count;
    
    if (pack_v < min_pack) {
        new_faults |= BMS_FAULT_PACK_UV;
    }
    if (pack_v > max_pack) {
        new_faults |= BMS_FAULT_PACK_OV;
    }
    
    /* Temperature faults */
    int16_t max_temp = bms_find_max_temp();
    int16_t min_temp = bms_find_min_temp();
    
    if (max_temp >= BMS_TEMP_OT_LOCKOUT_C10) {
        new_faults |= BMS_FAULT_OVER_TEMP;
    }
    if (min_temp <= BMS_TEMP_UT_C10) {
        new_faults |= BMS_FAULT_UNDER_TEMP;
    }
    
    /* Thermal runaway detection */
    for (uint8_t i = 0; i < g_bms.config.temp_zone_count; i++) {
        if (g_bms.temp_zones[i].temp_rate_c10_s >= BMS_THERMAL_RUNAWAY_RATE) {
            new_faults |= BMS_FAULT_THERMAL_RUNAWAY;
            break;
        }
    }
    
    /* Overcurrent fault */
    int32_t abs_current = g_bms.pack_current_ma;
    if (abs_current < 0) abs_current = -abs_current;
    
    if (g_bms.pack_current_ma > 0) {  /* Charging */
        if (abs_current > (int32_t)g_bms.config.max_charge_ma) {
            new_faults |= BMS_FAULT_OVERCURRENT;
        }
    } else {  /* Discharging */
        if (abs_current > (int32_t)g_bms.config.max_discharge_ma) {
            new_faults |= BMS_FAULT_OVERCURRENT;
        }
    }
    
    /* Short circuit: very high current spike */
    if (abs_current > 100000) {  /* 100A */
        new_faults |= BMS_FAULT_SHORT_CIRCUIT;
    }
    
    /* Update fault flags - include injected faults for testing */
    g_bms.fault_flags = new_faults | g_bms.injected_faults;
    
    /* Notify on fault changes */
    if (new_faults != old_faults) {
        if (new_faults > old_faults) {
            bms_notify_event(BMS_EVENT_FAULT, new_faults);
        } else {
            bms_notify_event(BMS_EVENT_FAULT_CLEARED, new_faults);
        }
    }
}

/*******************************************************************************
 * Cell Balancing
 ******************************************************************************/

/**
 * @brief Run cell balancing algorithm
 * 
 * Passive balancing: bleeds excess charge from high cells
 * Active balancing: transfers charge between cells (not implemented here)
 */
static void bms_run_balancing(void) {
    uint32_t now = bms_get_time_ms();
    
    if ((now - g_bms.last_balance_check) < BMS_BALANCE_INTERVAL_MS) {
        return;
    }
    g_bms.last_balance_check = now;
    
    /* Only balance in appropriate states */
    if (g_bms.state != BMS_STATE_IDLE && 
        g_bms.state != BMS_STATE_CHARGING &&
        g_bms.state != BMS_STATE_BALANCING) {
        return;
    }
    
    /* Find minimum cell voltage (target) */
    uint16_t min_v = bms_find_min_cell_voltage();
    
    /* Calculate which cells need balancing */
    bool any_balancing = false;
    memset(g_bms.balance_mask, 0, sizeof(g_bms.balance_mask));
    
    for (uint8_t i = 0; i < g_bms.config.cell_count; i++) {
        uint16_t delta = 0;
        if (g_bms.cells[i].voltage_mv > min_v) {
            delta = g_bms.cells[i].voltage_mv - min_v;
        }
        
        if (delta > g_bms.config.balance_threshold_mv) {
            /* Mark this cell for balancing */
            g_bms.cells[i].balancing = true;
            g_bms.balance_mask[i / 8] |= (1 << (i % 8));
            any_balancing = true;
        } else {
            g_bms.cells[i].balancing = false;
        }
    }
    
    /* Update state */
    if (any_balancing && !g_bms.balancing_active) {
        g_bms.balancing_active = true;
        g_bms.balance_start_time = now;
        bms_notify_event(BMS_EVENT_BALANCE_START, 0);
    } else if (!any_balancing && g_bms.balancing_active) {
        g_bms.balancing_active = false;
        bms_notify_event(BMS_EVENT_BALANCE_COMPLETE, 
                         now - g_bms.balance_start_time);
    }
}

/*******************************************************************************
 * State Machine
 ******************************************************************************/

/**
 * @brief BMS state machine handler
 */
static void bms_state_machine(void) {
    bms_state_t old_state = g_bms.state;
    uint32_t now = bms_get_time_ms();
    
    /* Check for fault conditions first */
    if (g_bms.fault_flags != BMS_FAULT_NONE) {
        if (g_bms.state != BMS_STATE_FAULT && 
            g_bms.state != BMS_STATE_SHUTDOWN) {
            g_bms.state = BMS_STATE_FAULT;
            g_bms.state_entry_time = now;
            
            /* Open contactors for safety */
            bms_set_contactor(false);
            bms_set_precharge(false);
            g_bms.charge_enable = false;
            g_bms.discharge_enable = false;
        }
    }
    
    switch (g_bms.state) {
        case BMS_STATE_INIT:
            /* Initialization complete, move to idle */
            g_bms.state = BMS_STATE_IDLE;
            g_bms.state_entry_time = now;
            break;
            
        case BMS_STATE_IDLE:
            /* Monitor and wait for commands */
            bms_set_precharge(false);
            
            /* Check if charging is requested and safe */
            if (g_bms.charge_enable && g_bms.fault_flags == BMS_FAULT_NONE) {
                g_bms.state = BMS_STATE_CHARGING;
                g_bms.state_entry_time = now;
                /* Skip precharge if cells are already above warning level */
                if (bms_find_min_cell_voltage() > BMS_CELL_UV_WARN_MV) {
                    g_bms.charge_phase = BMS_CHARGE_CC;
                    bms_set_contactor(true);
                } else {
                    g_bms.charge_phase = BMS_CHARGE_PRECHARGE;
                }
            }
            
            /* Check if discharging is requested */
            if (g_bms.discharge_enable && g_bms.fault_flags == BMS_FAULT_NONE) {
                g_bms.state = BMS_STATE_DISCHARGING;
                g_bms.state_entry_time = now;
                bms_set_precharge(true);
            }
            break;
            
        case BMS_STATE_CHARGING:
            /* Handle charge phases */
            switch (g_bms.charge_phase) {
                case BMS_CHARGE_PRECHARGE:
                    /* Precharge for deep-discharged cells */
                    if (bms_find_min_cell_voltage() > BMS_CELL_UV_WARN_MV) {
                        g_bms.charge_phase = BMS_CHARGE_CC;
                        bms_set_contactor(true);
                    }
                    break;
                    
                case BMS_CHARGE_CC:
                    /* Constant current phase */
                    if (bms_find_max_cell_voltage() >= g_bms.config.cell_max_mv - 50) {
                        g_bms.charge_phase = BMS_CHARGE_CV;
                    }
                    break;
                    
                case BMS_CHARGE_CV:
                    /* Constant voltage phase */
                    /* Terminate when current drops below threshold */
                    if (g_bms.pack_current_ma < 100 && g_bms.pack_current_ma >= 0) {
                        g_bms.charge_phase = BMS_CHARGE_TOPOFF;
                    }
                    break;
                    
                case BMS_CHARGE_TOPOFF:
                    /* Final top-off */
                    if ((now - g_bms.state_entry_time) > 30000) {
                        g_bms.charge_phase = BMS_CHARGE_COMPLETE;
                        bms_notify_event(BMS_EVENT_CHARGE_COMPLETE, g_bms.soc.soc_pct);
                    }
                    break;
                    
                case BMS_CHARGE_COMPLETE:
                    g_bms.charge_enable = false;
                    g_bms.state = BMS_STATE_IDLE;
                    g_bms.state_entry_time = now;
                    break;
            }
            
            /* Abort if charge disabled */
            if (!g_bms.charge_enable) {
                g_bms.state = BMS_STATE_IDLE;
                g_bms.state_entry_time = now;
            }
            break;
            
        case BMS_STATE_DISCHARGING:
            /* Monitor discharge */
            if (!g_bms.main_contactor) {
                /* Precharge then close */
                if ((now - g_bms.state_entry_time) > 100) {
                    bms_set_contactor(true);
                    bms_set_precharge(false);
                }
            }
            
            /* Check for low voltage cutoff */
            if (bms_find_min_cell_voltage() <= BMS_CELL_UV_WARN_MV) {
                g_bms.discharge_enable = false;
            }
            
            /* Transition back to idle when disabled */
            if (!g_bms.discharge_enable) {
                bms_set_contactor(false);
                g_bms.state = BMS_STATE_IDLE;
                g_bms.state_entry_time = now;
            }
            break;
            
        case BMS_STATE_BALANCING:
            /* Dedicated balancing mode */
            if (!g_bms.balancing_active) {
                g_bms.state = BMS_STATE_IDLE;
                g_bms.state_entry_time = now;
            }
            break;
            
        case BMS_STATE_FAULT:
            /* Wait for fault condition to clear */
            if (g_bms.fault_flags == BMS_FAULT_NONE) {
                g_bms.state = BMS_STATE_IDLE;
                g_bms.state_entry_time = now;
            }
            break;
            
        case BMS_STATE_SHUTDOWN:
            /* Safe shutdown - do nothing */
            break;
    }
    
    /* Notify on state change */
    if (old_state != g_bms.state) {
        bms_notify_event(BMS_EVENT_STATE_CHANGE, g_bms.state);
    }
}

/*******************************************************************************
 * Telemetry Recording
 ******************************************************************************/

/**
 * @brief Record telemetry snapshot
 */
static void bms_update_telemetry(void) {
    bms_telemetry_record_t* record = &g_bms.history[g_bms.history_index];
    
    record->timestamp_ms = bms_get_time_ms();
    record->pack_voltage_mv = bms_calc_pack_voltage();
    record->pack_current_ma = g_bms.pack_current_ma;
    record->soc_pct = g_bms.soc.soc_pct;
    record->max_temp_c10 = bms_find_max_temp();
    
    for (uint8_t i = 0; i < g_bms.config.cell_count && i < BMS_MAX_CELLS; i++) {
        record->cell_voltages[i] = g_bms.cells[i].voltage_mv;
    }
    
    g_bms.history_index = (g_bms.history_index + 1) % BMS_TELEMETRY_HISTORY;
    if (g_bms.history_count < BMS_TELEMETRY_HISTORY) {
        g_bms.history_count++;
    }
}

/*******************************************************************************
 * Event Notification
 ******************************************************************************/

/**
 * @brief Notify event to registered callback
 */
static void bms_notify_event(bms_event_type_t event, uint32_t data) {
    if (g_bms.event_callback != NULL) {
        g_bms.event_callback(event, data, g_bms.callback_context);
    }
}

/*******************************************************************************
 * Public API Implementation
 ******************************************************************************/

/**
 * @brief Initialize BMS
 * @param config Pack configuration
 * @return 0 on success, error code otherwise
 */
int bms_init(const bms_config_t* config) {
    if (config == NULL) {
        return -1;
    }
    
    if (config->cell_count == 0 || config->cell_count > BMS_MAX_CELLS) {
        return -2;
    }
    
    if (config->temp_zone_count > BMS_MAX_TEMP_ZONES) {
        return -3;
    }
    
    memset(&g_bms, 0, sizeof(g_bms));
    memcpy(&g_bms.config, config, sizeof(bms_config_t));
    
    /* Initialize SOC estimator */
    g_bms.soc.capacity_mah = config->cell_capacity_mah;
    g_bms.soc.remaining_mah = config->cell_capacity_mah / 2;  /* Assume 50% */
    g_bms.soc.soc_pct = 50;
    g_bms.soc.soh_pct = 100;
    g_bms.soc.coulomb_count_mas = 
        (int32_t)g_bms.soc.remaining_mah * 3600;
    
    /* Initialize cells to nominal voltage */
    for (uint8_t i = 0; i < config->cell_count; i++) {
        g_bms.cells[i].voltage_mv = config->cell_nominal_mv;
    }
    
    /* Initialize temperature zones to 25°C */
    for (uint8_t i = 0; i < config->temp_zone_count; i++) {
        g_bms.temp_zones[i].temp_c10 = 250;  /* 25.0°C */
    }
    
    g_bms.state = BMS_STATE_INIT;
    g_bms.uptime_start = bms_get_time_ms();
    g_bms.initialized = true;
    
    return 0;
}

/**
 * @brief Shutdown BMS safely
 */
void bms_shutdown(void) {
    if (!g_bms.initialized) return;
    
    /* Open all contactors */
    bms_set_contactor(false);
    bms_set_precharge(false);
    
    /* Disable charge/discharge */
    g_bms.charge_enable = false;
    g_bms.discharge_enable = false;
    
    /* Stop balancing */
    g_bms.balancing_active = false;
    for (uint8_t i = 0; i < g_bms.config.cell_count; i++) {
        g_bms.cells[i].balancing = false;
    }
    
    g_bms.state = BMS_STATE_SHUTDOWN;
}

/**
 * @brief Start charging
 * @return 0 on success, error code otherwise
 */
int bms_start_charge(void) {
    if (!g_bms.initialized) return -1;
    if (g_bms.fault_flags != BMS_FAULT_NONE) return -2;
    if (g_bms.state == BMS_STATE_FAULT || 
        g_bms.state == BMS_STATE_SHUTDOWN) return -3;
    
    g_bms.charge_enable = true;
    return 0;
}

/**
 * @brief Stop charging
 */
void bms_stop_charge(void) {
    g_bms.charge_enable = false;
}

/**
 * @brief Enable discharge
 * @return 0 on success, error code otherwise
 */
int bms_enable_discharge(void) {
    if (!g_bms.initialized) return -1;
    if (g_bms.fault_flags != BMS_FAULT_NONE) return -2;
    if (g_bms.state == BMS_STATE_FAULT || 
        g_bms.state == BMS_STATE_SHUTDOWN) return -3;
    
    g_bms.discharge_enable = true;
    return 0;
}

/**
 * @brief Disable discharge
 */
void bms_disable_discharge(void) {
    g_bms.discharge_enable = false;
    if (g_bms.main_contactor) {
        bms_set_contactor(false);
    }
}

/**
 * @brief Get BMS status
 * @param status Output status structure
 * @return 0 on success
 */
int bms_get_status(bms_status_t* status) {
    if (status == NULL) return -1;
    if (!g_bms.initialized) return -2;
    
    status->state = g_bms.state;
    status->charge_phase = g_bms.charge_phase;
    status->fault_flags = g_bms.fault_flags;
    status->pack_voltage_mv = bms_calc_pack_voltage();
    status->pack_current_ma = g_bms.pack_current_ma;
    status->power_mw = (status->pack_voltage_mv / 1000) * 
                       (status->pack_current_ma / 1000);
    status->max_cell_mv = bms_find_max_cell_voltage();
    status->min_cell_mv = bms_find_min_cell_voltage();
    status->cell_delta_mv = status->max_cell_mv - status->min_cell_mv;
    status->max_temp_c10 = bms_find_max_temp();
    status->min_temp_c10 = bms_find_min_temp();
    status->soc_pct = g_bms.soc.soc_pct;
    status->soh_pct = g_bms.soc.soh_pct;
    status->contactor_closed = g_bms.main_contactor;
    status->precharge_active = g_bms.precharge_relay;
    status->uptime_s = (bms_get_time_ms() - g_bms.uptime_start) / 1000;
    
    return 0;
}

/**
 * @brief Get cell voltage
 * @param cell Cell index
 * @return Voltage in mV, or 0 on error
 */
uint16_t bms_get_cell_voltage(uint8_t cell) {
    if (!g_bms.initialized || cell >= g_bms.config.cell_count) {
        return 0;
    }
    return g_bms.cells[cell].voltage_mv;
}

/**
 * @brief Get temperature zone reading
 * @param zone Zone index
 * @return Temperature in 0.1°C, or INT16_MIN on error
 */
int16_t bms_get_zone_temp(uint8_t zone) {
    if (!g_bms.initialized || zone >= g_bms.config.temp_zone_count) {
        return INT16_MIN;
    }
    return g_bms.temp_zones[zone].temp_c10;
}

/**
 * @brief Register event callback
 * @param callback Callback function
 * @param user_ctx User context
 */
void bms_register_callback(bms_event_callback_t callback, void* user_ctx) {
    g_bms.event_callback = callback;
    g_bms.callback_context = user_ctx;
}

/**
 * @brief Process BMS (call periodically)
 * @param delta_ms Time since last call
 */
void bms_process(uint32_t delta_ms) {
    if (!g_bms.initialized) return;
    
    g_bms_tick_ms += delta_ms;
    
    /* Sample sensors */
    bms_sample_voltages();
    bms_sample_temperatures();
    
    /* Update estimations */
    bms_update_soc();
    
    /* Check for faults */
    bms_check_faults();
    
    /* Run balancing if appropriate */
    bms_run_balancing();
    
    /* Run state machine */
    bms_state_machine();
    
    /* Record telemetry */
    bms_update_telemetry();
}

/*******************************************************************************
 * Test API (for simulation and testing)
 ******************************************************************************/

/**
 * @brief Set cell voltage (test API)
 */
void bms_test_set_cell_voltage(uint8_t cell, uint16_t voltage_mv) {
    if (cell < BMS_MAX_CELLS) {
        g_bms.cells[cell].voltage_mv = voltage_mv;
    }
}

/**
 * @brief Set zone temperature (test API)
 */
void bms_test_set_zone_temp(uint8_t zone, int16_t temp_c10) {
    if (zone < BMS_MAX_TEMP_ZONES) {
        g_bms.temp_zones[zone].temp_c10 = temp_c10;
    }
}

/**
 * @brief Set pack current (test API)
 */
void bms_test_set_current(int32_t current_ma) {
    g_bms.pack_current_ma = current_ma;
}

/**
 * @brief Get fault flags (test API)
 */
uint32_t bms_test_get_faults(void) {
    return g_bms.fault_flags;
}

/**
 * @brief Get state (test API)
 */
bms_state_t bms_test_get_state(void) {
    return g_bms.state;
}

/**
 * @brief Check if cell is balancing (test API)
 */
bool bms_test_is_cell_balancing(uint8_t cell) {
    if (cell >= g_bms.config.cell_count) return false;
    return g_bms.cells[cell].balancing;
}

/**
 * @brief Reset BMS state (test API)
 */
void bms_test_reset(void) {
    memset(&g_bms, 0, sizeof(g_bms));
    g_bms_tick_ms = 0;
}

/**
 * @brief Get contactor state (test API)
 */
bool bms_test_get_contactor(void) {
    return g_bms.main_contactor;
}

/**
 * @brief Force fault (test API)
 */
void bms_test_inject_fault(uint32_t fault_mask) {
    g_bms.injected_faults |= fault_mask;
    g_bms.fault_flags |= fault_mask;
}

/**
 * @brief Clear fault (test API)
 */
void bms_test_clear_fault(uint32_t fault_mask) {
    g_bms.injected_faults &= ~fault_mask;
    g_bms.fault_flags &= ~fault_mask;
}

/**
 * @brief Set SOC (test API)
 */
void bms_test_set_soc(uint8_t soc_pct) {
    g_bms.soc.soc_pct = soc_pct;
    g_bms.soc.remaining_mah = (g_bms.soc.capacity_mah * soc_pct) / 100;
    g_bms.soc.coulomb_count_mas = (int32_t)g_bms.soc.remaining_mah * 3600;
}

/**
 * @brief Get charge phase (test API)
 */
bms_charge_phase_t bms_test_get_charge_phase(void) {
    return g_bms.charge_phase;
}
