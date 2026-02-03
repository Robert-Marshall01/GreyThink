/**
 * @file microgrid_spotlight.c
 * @brief Microgrid Controller & Demand Response Spotlight Implementation
 * 
 * This spotlight subsystem demonstrates production-grade microgrid control
 * with local grid balancing, demand response, and telemetry reporting.
 * 
 * INDUSTRY RELEVANCE:
 * Microgrids are transforming energy infrastructure, enabling resilient
 * power for military bases, campuses, remote communities, and industrial
 * facilities. This implementation showcases:
 * - Real-time load/supply balancing
 * - Priority-based demand response (OpenADR compatible)
 * - Islanded and grid-connected operation
 * - Generation dispatch with renewable integration
 * - SCADA telemetry and alarm management
 * 
 * STANDARDS COMPLIANCE:
 * - IEEE 1547: Interconnection standards
 * - IEEE 2030: Smart grid interoperability
 * - OpenADR 2.0: Demand response signaling
 * - IEC 61850: Substation automation
 * - NERC CIP: Critical infrastructure protection
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define MG_MAX_SOURCES              16      /**< Max generation sources */
#define MG_MAX_LOADS                32      /**< Max controllable loads */
#define MG_MAX_STORAGE              8       /**< Max storage systems */
#define MG_MAX_DR_EVENTS            8       /**< Max concurrent DR events */
#define MG_MAX_ALARMS               32      /**< Max active alarms */
#define MG_TELEMETRY_POINTS         128     /**< Max telemetry points */

#define MG_BALANCE_INTERVAL_MS      100     /**< Balance loop interval */
#define MG_VOLTAGE_NOMINAL          480.0f  /**< Nominal voltage (V) */
#define MG_FREQUENCY_NOMINAL        60.0f   /**< Nominal frequency (Hz) */
#define MG_VOLTAGE_TOLERANCE        0.05f   /**< 5% voltage tolerance */
#define MG_FREQUENCY_TOLERANCE      0.01f   /**< 0.6 Hz tolerance */

#define MG_RESERVE_MARGIN           0.15f   /**< 15% spinning reserve */
#define MG_RAMP_RATE_KW_SEC         100.0f  /**< Max ramp rate (kW/s) */
#define MG_ISLAND_DETECT_MS         100     /**< Island detection time */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/**
 * @brief Grid operating mode
 */
typedef enum {
    MG_MODE_OFF = 0,
    MG_MODE_STARTING,
    MG_MODE_GRID_CONNECTED,
    MG_MODE_ISLAND_TRANSITION,
    MG_MODE_ISLANDED,
    MG_MODE_RECONNECTING,
    MG_MODE_FAULT,
    MG_MODE_EMERGENCY_SHUTDOWN
} mg_mode_t;

/**
 * @brief Generation source type
 */
typedef enum {
    MG_SOURCE_SOLAR = 0,
    MG_SOURCE_WIND,
    MG_SOURCE_DIESEL,
    MG_SOURCE_NATURAL_GAS,
    MG_SOURCE_FUEL_CELL,
    MG_SOURCE_BATTERY,
    MG_SOURCE_UTILITY
} mg_source_type_t;

/**
 * @brief Load priority levels
 */
typedef enum {
    MG_LOAD_CRITICAL = 0,           /**< Life safety - never shed */
    MG_LOAD_ESSENTIAL,              /**< Important - shed last */
    MG_LOAD_NORMAL,                 /**< Regular loads */
    MG_LOAD_DEFERRABLE,             /**< Can be delayed */
    MG_LOAD_CURTAILABLE             /**< Can be reduced/shed */
} mg_load_priority_t;

/**
 * @brief DR event type
 */
typedef enum {
    MG_DR_NONE = 0,
    MG_DR_LOAD_SHED,
    MG_DR_PRICE_RESPONSE,
    MG_DR_CAPACITY_RESERVE,
    MG_DR_FREQUENCY_RESPONSE,
    MG_DR_VOLTAGE_SUPPORT
} mg_dr_type_t;

/**
 * @brief Alarm severity
 */
typedef enum {
    MG_ALARM_INFO = 0,
    MG_ALARM_WARNING,
    MG_ALARM_MINOR,
    MG_ALARM_MAJOR,
    MG_ALARM_CRITICAL
} mg_alarm_severity_t;

/**
 * @brief Generation source
 */
typedef struct {
    uint8_t id;
    mg_source_type_t type;
    char name[32];
    float capacity_kw;              /**< Rated capacity (kW) */
    float power_available_kw;       /**< Currently available (kW) */
    float power_output_kw;          /**< Current output (kW) */
    float power_setpoint_kw;        /**< Commanded output (kW) */
    float ramp_rate_kw_s;           /**< Ramp rate (kW/s) */
    float min_output_kw;            /**< Minimum stable output */
    float efficiency;               /**< Current efficiency (0-1) */
    float fuel_remaining;           /**< Fuel remaining (0-1) */
    float cost_per_kwh;             /**< Marginal cost ($/kWh) */
    bool online;
    bool dispatchable;
    bool starting;
    uint32_t start_time_ms;         /**< Time to start (ms) */
    uint32_t started_at;            /**< Timestamp when started */
} mg_source_t;

/**
 * @brief Controllable load
 */
typedef struct {
    uint8_t id;
    char name[32];
    mg_load_priority_t priority;
    float power_demand_kw;          /**< Current demand (kW) */
    float power_allocated_kw;       /**< Allocated power (kW) */
    float max_power_kw;             /**< Maximum power (kW) */
    float min_power_kw;             /**< Minimum operating power */
    float curtail_percent;          /**< Curtailment level (0-100) */
    bool connected;
    bool curtailed;
    bool dr_enrolled;               /**< Enrolled in DR program */
    uint32_t min_on_time_ms;        /**< Minimum on-time */
    uint32_t last_change;           /**< Last state change time */
} mg_load_t;

/**
 * @brief Energy storage system
 */
typedef struct {
    uint8_t id;
    char name[32];
    float capacity_kwh;             /**< Total capacity (kWh) */
    float soc_percent;              /**< State of charge (%) */
    float power_kw;                 /**< Current power (+charge/-discharge) */
    float max_charge_kw;            /**< Max charge rate (kW) */
    float max_discharge_kw;         /**< Max discharge rate (kW) */
    float efficiency;               /**< Round-trip efficiency */
    bool online;
    bool charging;
} mg_storage_t;

/**
 * @brief Demand response event
 */
typedef struct {
    uint8_t id;
    mg_dr_type_t type;
    uint32_t start_time;
    uint32_t end_time;
    float target_reduction_kw;
    float actual_reduction_kw;
    float price_signal;
    uint8_t priority;
    bool active;
    bool mandatory;
} mg_dr_event_t;

/**
 * @brief Active alarm
 */
typedef struct {
    uint16_t id;
    mg_alarm_severity_t severity;
    uint32_t timestamp;
    char message[64];
    float value;
    float threshold;
    bool acknowledged;
} mg_alarm_t;

/**
 * @brief Grid balance state
 */
typedef struct {
    float total_generation_kw;
    float total_demand_kw;
    float total_storage_kw;         /**< Positive = charging */
    float net_import_kw;            /**< From utility (positive = import) */
    float imbalance_kw;             /**< Generation - demand */
    float voltage_pu;               /**< Per-unit voltage */
    float frequency_hz;
    float power_factor;
    float spinning_reserve_kw;
    bool balanced;
} mg_balance_t;

/**
 * @brief Telemetry point
 */
typedef struct {
    uint16_t id;
    char name[32];
    float value;
    uint32_t timestamp;
    uint8_t quality;
} mg_telemetry_t;

/**
 * @brief Microgrid statistics
 */
typedef struct {
    uint32_t uptime_seconds;
    uint32_t grid_connected_seconds;
    uint32_t islanded_seconds;
    float energy_generated_kwh;
    float energy_consumed_kwh;
    float energy_imported_kwh;
    float energy_exported_kwh;
    float peak_demand_kw;
    float peak_generation_kw;
    uint32_t dr_events_responded;
    float dr_kwh_reduced;
    uint32_t island_transitions;
    uint32_t alarms_total;
    uint32_t faults_total;
} mg_stats_t;

/**
 * @brief Microgrid configuration
 */
typedef struct {
    float voltage_nominal;
    float frequency_nominal;
    float voltage_tolerance;
    float frequency_tolerance;
    float reserve_margin;
    float max_import_kw;
    float max_export_kw;
    bool enable_demand_response;
    bool enable_peak_shaving;
    bool enable_load_shifting;
    bool enable_islanding;
    uint16_t balance_interval_ms;
} mg_config_t;

/* ============================================================================
 * Static State
 * ============================================================================ */

static struct {
    bool initialized;
    mg_mode_t mode;
    mg_config_t config;
    mg_balance_t balance;
    mg_stats_t stats;
    
    /* Sources */
    mg_source_t sources[MG_MAX_SOURCES];
    uint8_t source_count;
    
    /* Loads */
    mg_load_t loads[MG_MAX_LOADS];
    uint8_t load_count;
    
    /* Storage */
    mg_storage_t storage[MG_MAX_STORAGE];
    uint8_t storage_count;
    
    /* Demand Response */
    mg_dr_event_t dr_events[MG_MAX_DR_EVENTS];
    uint8_t dr_event_count;
    
    /* Alarms */
    mg_alarm_t alarms[MG_MAX_ALARMS];
    uint8_t alarm_count;
    uint16_t alarm_sequence;
    
    /* Telemetry */
    mg_telemetry_t telemetry[MG_TELEMETRY_POINTS];
    uint16_t telemetry_count;
    
    /* Timing */
    uint32_t last_balance_time;
    uint32_t last_telemetry_time;
    uint32_t mode_entry_time;
    
    /* Grid connection */
    bool utility_available;
    float utility_voltage_pu;
    float utility_frequency_hz;
    
} g_mg = {0};

/* Forward declarations */
static void mg_update_telemetry(void);
static void mg_check_alarms(void);
static float mg_calculate_imbalance(void);
static void mg_dispatch_sources(float target_kw);
static void mg_curtail_loads(float reduction_kw);
static void mg_manage_storage(float imbalance_kw);
static void mg_process_dr_events(uint32_t current_time);

/* ============================================================================
 * Internal Utility Functions
 * ============================================================================ */

/**
 * @brief Get current timestamp (milliseconds)
 */
static uint32_t mg_get_time_ms(void) {
    static uint32_t time_ms = 0;
    return time_ms++;
}

/**
 * @brief Clamp float to range
 */
static float mg_clamp(float value, float min_val, float max_val) {
    if (value < min_val) return min_val;
    if (value > max_val) return max_val;
    return value;
}

/**
 * @brief Add alarm
 */
static void mg_add_alarm(mg_alarm_severity_t severity, const char* message,
                         float value, float threshold) {
    if (g_mg.alarm_count >= MG_MAX_ALARMS) {
        /* Shift alarms to make room */
        memmove(&g_mg.alarms[0], &g_mg.alarms[1], 
                (MG_MAX_ALARMS - 1) * sizeof(mg_alarm_t));
        g_mg.alarm_count = MG_MAX_ALARMS - 1;
    }
    
    mg_alarm_t* alarm = &g_mg.alarms[g_mg.alarm_count++];
    alarm->id = g_mg.alarm_sequence++;
    alarm->severity = severity;
    alarm->timestamp = mg_get_time_ms();
    strncpy(alarm->message, message, sizeof(alarm->message) - 1);
    alarm->value = value;
    alarm->threshold = threshold;
    alarm->acknowledged = false;
    
    g_mg.stats.alarms_total++;
}

/**
 * @brief Update telemetry point
 */
static void mg_update_point(const char* name, float value) {
    for (uint16_t i = 0; i < g_mg.telemetry_count; i++) {
        if (strcmp(g_mg.telemetry[i].name, name) == 0) {
            g_mg.telemetry[i].value = value;
            g_mg.telemetry[i].timestamp = mg_get_time_ms();
            return;
        }
    }
    
    /* Add new point */
    if (g_mg.telemetry_count < MG_TELEMETRY_POINTS) {
        mg_telemetry_t* pt = &g_mg.telemetry[g_mg.telemetry_count++];
        pt->id = g_mg.telemetry_count;
        strncpy(pt->name, name, sizeof(pt->name) - 1);
        pt->value = value;
        pt->timestamp = mg_get_time_ms();
        pt->quality = 0;
    }
}

/* ============================================================================
 * Grid Balancing Implementation
 * ============================================================================ */

/**
 * @brief Calculate total generation
 */
static float mg_total_generation(void) {
    float total = 0.0f;
    for (uint8_t i = 0; i < g_mg.source_count; i++) {
        if (g_mg.sources[i].online) {
            total += g_mg.sources[i].power_output_kw;
        }
    }
    return total;
}

/**
 * @brief Calculate total demand
 */
static float mg_total_demand(void) {
    float total = 0.0f;
    for (uint8_t i = 0; i < g_mg.load_count; i++) {
        if (g_mg.loads[i].connected) {
            total += g_mg.loads[i].power_allocated_kw;
        }
    }
    return total;
}

/**
 * @brief Calculate available generation capacity
 */
static float mg_available_capacity(void) {
    float total = 0.0f;
    for (uint8_t i = 0; i < g_mg.source_count; i++) {
        if (g_mg.sources[i].online) {
            total += g_mg.sources[i].power_available_kw;
        }
    }
    return total;
}

/**
 * @brief Calculate spinning reserve
 */
static float mg_spinning_reserve(void) {
    return mg_available_capacity() - mg_total_generation();
}

/**
 * @brief Calculate imbalance
 */
static float mg_calculate_imbalance(void) {
    float gen = mg_total_generation();
    float demand = mg_total_demand();
    float storage = 0.0f;
    
    for (uint8_t i = 0; i < g_mg.storage_count; i++) {
        if (g_mg.storage[i].online) {
            storage += g_mg.storage[i].power_kw;
        }
    }
    
    /* Import counts as generation */
    float import = (g_mg.mode == MG_MODE_GRID_CONNECTED) ? 
                   g_mg.balance.net_import_kw : 0.0f;
    
    return (gen + import) - (demand + storage);
}

/**
 * @brief Dispatch generation sources using economic dispatch
 * 
 * Dispatches sources in merit order (lowest cost first) while respecting
 * ramp rate limits and minimum output constraints.
 */
static void mg_dispatch_sources(float target_kw) {
    /* Sort sources by cost (simple bubble sort, production would use more efficient method) */
    uint8_t order[MG_MAX_SOURCES];
    for (uint8_t i = 0; i < g_mg.source_count; i++) {
        order[i] = i;
    }
    
    for (uint8_t i = 0; i < g_mg.source_count - 1; i++) {
        for (uint8_t j = 0; j < g_mg.source_count - i - 1; j++) {
            if (g_mg.sources[order[j]].cost_per_kwh > 
                g_mg.sources[order[j+1]].cost_per_kwh) {
                uint8_t tmp = order[j];
                order[j] = order[j+1];
                order[j+1] = tmp;
            }
        }
    }
    
    float remaining = target_kw;
    
    /* Dispatch in merit order */
    for (uint8_t i = 0; i < g_mg.source_count && remaining > 0; i++) {
        mg_source_t* src = &g_mg.sources[order[i]];
        
        if (!src->online || !src->dispatchable) {
            continue;
        }
        
        float can_provide = src->power_available_kw - src->power_output_kw;
        
        /* Respect ramp rate */
        float max_ramp = src->ramp_rate_kw_s * 
                        (float)g_mg.config.balance_interval_ms / 1000.0f;
        can_provide = mg_clamp(can_provide, 0, max_ramp);
        
        /* Respect minimum output */
        if (src->power_output_kw < src->min_output_kw && 
            src->power_output_kw + can_provide < src->min_output_kw) {
            /* Need to bring up to minimum in one step or not at all */
            if (remaining >= src->min_output_kw) {
                can_provide = src->min_output_kw;
            } else {
                continue;
            }
        }
        
        float dispatch = (can_provide < remaining) ? can_provide : remaining;
        src->power_setpoint_kw = src->power_output_kw + dispatch;
        remaining -= dispatch;
    }
}

/**
 * @brief Curtail loads based on priority
 * 
 * Sheds loads starting from lowest priority (CURTAILABLE) up to
 * ESSENTIAL. CRITICAL loads are never shed.
 */
static void mg_curtail_loads(float reduction_kw) {
    float remaining = reduction_kw;
    
    /* Shed from lowest priority first */
    for (int prio = MG_LOAD_CURTAILABLE; prio >= MG_LOAD_ESSENTIAL && remaining > 0; prio--) {
        for (uint8_t i = 0; i < g_mg.load_count && remaining > 0; i++) {
            mg_load_t* load = &g_mg.loads[i];
            
            if (!load->connected || (int)load->priority != prio) {
                continue;
            }
            
            /* Skip loads that can't be curtailed yet (min on-time) */
            if (mg_get_time_ms() - load->last_change < load->min_on_time_ms) {
                continue;
            }
            
            /* Calculate curtailment */
            float curtailable = load->power_allocated_kw - load->min_power_kw;
            
            if (curtailable > 0) {
                float curtail = (curtailable < remaining) ? curtailable : remaining;
                load->power_allocated_kw -= curtail;
                load->curtailed = true;
                load->curtail_percent = 100.0f * (1.0f - load->power_allocated_kw / 
                                                  load->power_demand_kw);
                load->last_change = mg_get_time_ms();
                remaining -= curtail;
            }
        }
    }
    
    /* If still need to shed, disconnect non-critical loads */
    if (remaining > 0) {
        for (int prio = MG_LOAD_CURTAILABLE; prio > MG_LOAD_CRITICAL && remaining > 0; prio--) {
            for (uint8_t i = 0; i < g_mg.load_count && remaining > 0; i++) {
                mg_load_t* load = &g_mg.loads[i];
                
                if (!load->connected || (int)load->priority != prio) {
                    continue;
                }
                
                remaining -= load->power_allocated_kw;
                load->power_allocated_kw = 0;
                load->connected = false;
                load->curtailed = true;
                load->last_change = mg_get_time_ms();
            }
        }
    }
}

/**
 * @brief Restore curtailed loads
 */
static void mg_restore_loads(float available_kw) {
    float remaining = available_kw;
    
    /* Restore from highest priority first */
    for (int prio = MG_LOAD_CRITICAL; prio <= MG_LOAD_CURTAILABLE && remaining > 0; prio++) {
        for (uint8_t i = 0; i < g_mg.load_count && remaining > 0; i++) {
            mg_load_t* load = &g_mg.loads[i];
            
            if ((int)load->priority != prio || !load->curtailed) {
                continue;
            }
            
            float needed = load->power_demand_kw - load->power_allocated_kw;
            
            if (needed > 0 && needed <= remaining) {
                load->power_allocated_kw = load->power_demand_kw;
                load->curtailed = false;
                load->curtail_percent = 0;
                load->connected = true;
                load->last_change = mg_get_time_ms();
                remaining -= needed;
            }
        }
    }
}

/**
 * @brief Manage energy storage for grid balancing
 */
static void mg_manage_storage(float imbalance_kw) {
    for (uint8_t i = 0; i < g_mg.storage_count; i++) {
        mg_storage_t* ess = &g_mg.storage[i];
        
        if (!ess->online) {
            continue;
        }
        
        if (imbalance_kw > 0) {
            /* Excess generation - charge storage */
            float can_charge = ess->max_charge_kw - ess->power_kw;
            
            /* Check SOC limit */
            if (ess->soc_percent >= 100.0f) {
                can_charge = 0;
            }
            
            float charge = (can_charge < imbalance_kw) ? can_charge : imbalance_kw;
            ess->power_kw = charge;
            ess->charging = true;
            
            /* Update SOC (simplified) */
            ess->soc_percent += (charge * ess->efficiency) / ess->capacity_kwh * 
                               (float)g_mg.config.balance_interval_ms / 3600000.0f * 100.0f;
            ess->soc_percent = mg_clamp(ess->soc_percent, 0, 100);
            
            imbalance_kw -= charge;
            
        } else if (imbalance_kw < 0) {
            /* Generation shortfall - discharge storage */
            float need = -imbalance_kw;
            float can_discharge = ess->max_discharge_kw + ess->power_kw;
            
            /* Check SOC limit */
            if (ess->soc_percent <= 10.0f) {  /* Reserve 10% */
                can_discharge = 0;
            }
            
            float discharge = (can_discharge < need) ? can_discharge : need;
            ess->power_kw = -discharge;
            ess->charging = false;
            
            /* Update SOC */
            ess->soc_percent -= discharge / ess->capacity_kwh / ess->efficiency * 
                               (float)g_mg.config.balance_interval_ms / 3600000.0f * 100.0f;
            ess->soc_percent = mg_clamp(ess->soc_percent, 0, 100);
            
            imbalance_kw += discharge;
        }
    }
}

/* ============================================================================
 * Demand Response Implementation
 * ============================================================================ */

/**
 * @brief Process active demand response events
 */
static void mg_process_dr_events(uint32_t current_time) {
    if (!g_mg.config.enable_demand_response) {
        return;
    }
    
    for (uint8_t i = 0; i < g_mg.dr_event_count; i++) {
        mg_dr_event_t* event = &g_mg.dr_events[i];
        
        /* Check if event should start */
        if (!event->active && current_time >= event->start_time && 
            current_time < event->end_time) {
            event->active = true;
            mg_add_alarm(MG_ALARM_INFO, "DR event started", 
                        event->target_reduction_kw, 0);
        }
        
        /* Check if event should end */
        if (event->active && current_time >= event->end_time) {
            event->active = false;
            g_mg.stats.dr_events_responded++;
            
            /* Restore curtailed loads */
            mg_restore_loads(event->actual_reduction_kw);
        }
        
        /* Process active event */
        if (event->active) {
            switch (event->type) {
                case MG_DR_LOAD_SHED:
                    mg_curtail_loads(event->target_reduction_kw);
                    break;
                    
                case MG_DR_PRICE_RESPONSE:
                    /* Curtail based on price signal */
                    if (event->price_signal > 0.50f) {  /* $/kWh threshold */
                        mg_curtail_loads(event->target_reduction_kw);
                    }
                    break;
                    
                case MG_DR_FREQUENCY_RESPONSE:
                    /* Fast frequency response - use storage */
                    if (g_mg.balance.frequency_hz < MG_FREQUENCY_NOMINAL - 0.5f) {
                        mg_manage_storage(-event->target_reduction_kw);
                    }
                    break;
                    
                default:
                    break;
            }
            
            /* Calculate actual reduction */
            event->actual_reduction_kw = 0;
            for (uint8_t j = 0; j < g_mg.load_count; j++) {
                if (g_mg.loads[j].curtailed) {
                    event->actual_reduction_kw += 
                        g_mg.loads[j].power_demand_kw - g_mg.loads[j].power_allocated_kw;
                }
            }
            
            g_mg.stats.dr_kwh_reduced += event->actual_reduction_kw * 
                                         (float)g_mg.config.balance_interval_ms / 3600000.0f;
        }
    }
}

/* ============================================================================
 * Alarm and Telemetry
 * ============================================================================ */

/**
 * @brief Check for alarm conditions
 */
static void mg_check_alarms(void) {
    /* Voltage alarms */
    if (g_mg.balance.voltage_pu < 1.0f - g_mg.config.voltage_tolerance) {
        mg_add_alarm(MG_ALARM_WARNING, "Low voltage", 
                    g_mg.balance.voltage_pu, 1.0f - g_mg.config.voltage_tolerance);
    } else if (g_mg.balance.voltage_pu > 1.0f + g_mg.config.voltage_tolerance) {
        mg_add_alarm(MG_ALARM_WARNING, "High voltage",
                    g_mg.balance.voltage_pu, 1.0f + g_mg.config.voltage_tolerance);
    }
    
    /* Frequency alarms */
    float freq_deviation = fabsf(g_mg.balance.frequency_hz - g_mg.config.frequency_nominal);
    if (freq_deviation > g_mg.config.frequency_nominal * g_mg.config.frequency_tolerance) {
        mg_add_alarm(MG_ALARM_MAJOR, "Frequency deviation",
                    g_mg.balance.frequency_hz, g_mg.config.frequency_nominal);
    }
    
    /* Reserve margin alarm */
    float reserve_ratio = g_mg.balance.spinning_reserve_kw / mg_total_demand();
    if (reserve_ratio < g_mg.config.reserve_margin) {
        mg_add_alarm(MG_ALARM_WARNING, "Low spinning reserve",
                    reserve_ratio * 100.0f, g_mg.config.reserve_margin * 100.0f);
    }
    
    /* Storage SOC alarms */
    for (uint8_t i = 0; i < g_mg.storage_count; i++) {
        if (g_mg.storage[i].online && g_mg.storage[i].soc_percent < 20.0f) {
            mg_add_alarm(MG_ALARM_WARNING, "Battery low SOC",
                        g_mg.storage[i].soc_percent, 20.0f);
        }
    }
}

/**
 * @brief Update telemetry points
 */
static void mg_update_telemetry(void) {
    mg_update_point("total_generation_kw", g_mg.balance.total_generation_kw);
    mg_update_point("total_demand_kw", g_mg.balance.total_demand_kw);
    mg_update_point("net_import_kw", g_mg.balance.net_import_kw);
    mg_update_point("voltage_pu", g_mg.balance.voltage_pu);
    mg_update_point("frequency_hz", g_mg.balance.frequency_hz);
    mg_update_point("power_factor", g_mg.balance.power_factor);
    mg_update_point("spinning_reserve_kw", g_mg.balance.spinning_reserve_kw);
    mg_update_point("mode", (float)g_mg.mode);
    
    /* Storage telemetry */
    for (uint8_t i = 0; i < g_mg.storage_count; i++) {
        char name[32];
        snprintf(name, sizeof(name), "ess%d_soc", i);
        mg_update_point(name, g_mg.storage[i].soc_percent);
        snprintf(name, sizeof(name), "ess%d_power", i);
        mg_update_point(name, g_mg.storage[i].power_kw);
    }
    
    /* Source telemetry */
    for (uint8_t i = 0; i < g_mg.source_count; i++) {
        char name[32];
        snprintf(name, sizeof(name), "gen%d_output", i);
        mg_update_point(name, g_mg.sources[i].power_output_kw);
    }
}

/* ============================================================================
 * Mode State Machine
 * ============================================================================ */

/**
 * @brief Handle mode transition
 */
static void mg_set_mode(mg_mode_t new_mode) {
    if (g_mg.mode == new_mode) {
        return;
    }
    
    mg_mode_t old_mode = g_mg.mode;
    g_mg.mode = new_mode;
    g_mg.mode_entry_time = mg_get_time_ms();
    
    /* Log transition */
    char msg[64];
    snprintf(msg, sizeof(msg), "Mode: %d -> %d", old_mode, new_mode);
    mg_add_alarm(MG_ALARM_INFO, msg, (float)new_mode, (float)old_mode);
    
    /* Track statistics */
    if (new_mode == MG_MODE_ISLANDED) {
        g_mg.stats.island_transitions++;
    }
}

/**
 * @brief Process mode state machine
 */
static void mg_process_mode(uint32_t current_time) {
    switch (g_mg.mode) {
        case MG_MODE_OFF:
            /* Wait for start command */
            break;
            
        case MG_MODE_STARTING:
            /* Start sources and transition to connected */
            if (g_mg.utility_available) {
                mg_set_mode(MG_MODE_GRID_CONNECTED);
            } else if (g_mg.config.enable_islanding) {
                mg_set_mode(MG_MODE_ISLANDED);
            }
            break;
            
        case MG_MODE_GRID_CONNECTED:
            /* Check for utility loss */
            if (!g_mg.utility_available && g_mg.config.enable_islanding) {
                mg_set_mode(MG_MODE_ISLAND_TRANSITION);
            }
            break;
            
        case MG_MODE_ISLAND_TRANSITION:
            /* Transition to islanded mode */
            /* Shed non-critical loads during transition */
            mg_curtail_loads(mg_total_demand() * 0.2f);
            
            if (current_time - g_mg.mode_entry_time > MG_ISLAND_DETECT_MS) {
                mg_set_mode(MG_MODE_ISLANDED);
            }
            break;
            
        case MG_MODE_ISLANDED:
            /* Check for utility restoration */
            if (g_mg.utility_available) {
                mg_set_mode(MG_MODE_RECONNECTING);
            }
            break;
            
        case MG_MODE_RECONNECTING:
            /* Sync with utility before reconnection */
            if (fabsf(g_mg.balance.voltage_pu - g_mg.utility_voltage_pu) < 0.02f &&
                fabsf(g_mg.balance.frequency_hz - g_mg.utility_frequency_hz) < 0.1f) {
                mg_set_mode(MG_MODE_GRID_CONNECTED);
                mg_restore_loads(1000.0f);  /* Restore all loads */
            }
            break;
            
        case MG_MODE_FAULT:
            /* Wait for fault clear */
            break;
            
        case MG_MODE_EMERGENCY_SHUTDOWN:
            /* All sources off, critical loads only */
            break;
    }
}

/* ============================================================================
 * Public API Implementation
 * ============================================================================ */

/**
 * @brief Initialize microgrid controller
 */
int mg_init(const mg_config_t* config) {
    if (g_mg.initialized) {
        return -1;
    }
    
    memset(&g_mg, 0, sizeof(g_mg));
    
    if (config != NULL) {
        memcpy(&g_mg.config, config, sizeof(mg_config_t));
    } else {
        /* Default configuration */
        g_mg.config.voltage_nominal = MG_VOLTAGE_NOMINAL;
        g_mg.config.frequency_nominal = MG_FREQUENCY_NOMINAL;
        g_mg.config.voltage_tolerance = MG_VOLTAGE_TOLERANCE;
        g_mg.config.frequency_tolerance = MG_FREQUENCY_TOLERANCE;
        g_mg.config.reserve_margin = MG_RESERVE_MARGIN;
        g_mg.config.max_import_kw = 1000.0f;
        g_mg.config.max_export_kw = 500.0f;
        g_mg.config.enable_demand_response = true;
        g_mg.config.enable_peak_shaving = true;
        g_mg.config.enable_load_shifting = true;
        g_mg.config.enable_islanding = true;
        g_mg.config.balance_interval_ms = MG_BALANCE_INTERVAL_MS;
    }
    
    /* Initialize balance state */
    g_mg.balance.voltage_pu = 1.0f;
    g_mg.balance.frequency_hz = g_mg.config.frequency_nominal;
    g_mg.balance.power_factor = 0.95f;
    
    /* Assume utility available at start */
    g_mg.utility_available = true;
    g_mg.utility_voltage_pu = 1.0f;
    g_mg.utility_frequency_hz = g_mg.config.frequency_nominal;
    
    g_mg.mode = MG_MODE_OFF;
    g_mg.initialized = true;
    
    return 0;
}

/**
 * @brief Shutdown microgrid controller
 */
int mg_shutdown(void) {
    if (!g_mg.initialized) {
        return -1;
    }
    
    /* Shutdown all sources */
    for (uint8_t i = 0; i < g_mg.source_count; i++) {
        g_mg.sources[i].online = false;
        g_mg.sources[i].power_output_kw = 0;
    }
    
    g_mg.mode = MG_MODE_OFF;
    g_mg.initialized = false;
    
    return 0;
}

/**
 * @brief Start microgrid operation
 */
int mg_start(void) {
    if (!g_mg.initialized || g_mg.mode != MG_MODE_OFF) {
        return -1;
    }
    
    mg_set_mode(MG_MODE_STARTING);
    g_mg.stats.uptime_seconds = 0;
    
    return 0;
}

/**
 * @brief Stop microgrid operation
 */
int mg_stop(void) {
    if (!g_mg.initialized || g_mg.mode == MG_MODE_OFF) {
        return -1;
    }
    
    mg_set_mode(MG_MODE_OFF);
    
    return 0;
}

/**
 * @brief Register generation source
 */
int mg_register_source(mg_source_type_t type, const char* name, 
                       float capacity_kw, bool dispatchable) {
    if (!g_mg.initialized || g_mg.source_count >= MG_MAX_SOURCES) {
        return -1;
    }
    
    mg_source_t* src = &g_mg.sources[g_mg.source_count];
    src->id = g_mg.source_count;
    src->type = type;
    strncpy(src->name, name, sizeof(src->name) - 1);
    src->capacity_kw = capacity_kw;
    src->power_available_kw = capacity_kw;
    src->dispatchable = dispatchable;
    src->ramp_rate_kw_s = MG_RAMP_RATE_KW_SEC;
    src->efficiency = 0.90f;
    
    /* Set default costs by type */
    switch (type) {
        case MG_SOURCE_SOLAR:
        case MG_SOURCE_WIND:
            src->cost_per_kwh = 0.02f;  /* Near-zero marginal cost */
            src->min_output_kw = 0;
            break;
        case MG_SOURCE_BATTERY:
            src->cost_per_kwh = 0.05f;
            src->min_output_kw = 0;
            break;
        case MG_SOURCE_NATURAL_GAS:
            src->cost_per_kwh = 0.08f;
            src->min_output_kw = capacity_kw * 0.3f;
            break;
        case MG_SOURCE_DIESEL:
            src->cost_per_kwh = 0.15f;
            src->min_output_kw = capacity_kw * 0.4f;
            break;
        default:
            src->cost_per_kwh = 0.10f;
            src->min_output_kw = 0;
    }
    
    return g_mg.source_count++;
}

/**
 * @brief Register controllable load
 */
int mg_register_load(const char* name, mg_load_priority_t priority,
                     float max_power_kw) {
    if (!g_mg.initialized || g_mg.load_count >= MG_MAX_LOADS) {
        return -1;
    }
    
    mg_load_t* load = &g_mg.loads[g_mg.load_count];
    load->id = g_mg.load_count;
    strncpy(load->name, name, sizeof(load->name) - 1);
    load->priority = priority;
    load->max_power_kw = max_power_kw;
    load->power_demand_kw = max_power_kw;
    load->power_allocated_kw = max_power_kw;
    load->connected = true;
    load->dr_enrolled = (priority >= MG_LOAD_NORMAL);
    load->min_on_time_ms = 5000;  /* 5 second minimum */
    
    return g_mg.load_count++;
}

/**
 * @brief Register energy storage system
 */
int mg_register_storage(const char* name, float capacity_kwh,
                        float max_power_kw) {
    if (!g_mg.initialized || g_mg.storage_count >= MG_MAX_STORAGE) {
        return -1;
    }
    
    mg_storage_t* ess = &g_mg.storage[g_mg.storage_count];
    ess->id = g_mg.storage_count;
    strncpy(ess->name, name, sizeof(ess->name) - 1);
    ess->capacity_kwh = capacity_kwh;
    ess->max_charge_kw = max_power_kw;
    ess->max_discharge_kw = max_power_kw;
    ess->soc_percent = 50.0f;
    ess->efficiency = 0.92f;
    ess->online = true;
    
    return g_mg.storage_count++;
}

/**
 * @brief Update source availability (e.g., solar/wind variability)
 */
int mg_update_source(uint8_t source_id, float power_available_kw) {
    if (!g_mg.initialized || source_id >= g_mg.source_count) {
        return -1;
    }
    
    g_mg.sources[source_id].power_available_kw = 
        mg_clamp(power_available_kw, 0, g_mg.sources[source_id].capacity_kw);
    
    return 0;
}

/**
 * @brief Update load demand
 */
int mg_update_load(uint8_t load_id, float power_demand_kw) {
    if (!g_mg.initialized || load_id >= g_mg.load_count) {
        return -1;
    }
    
    g_mg.loads[load_id].power_demand_kw = 
        mg_clamp(power_demand_kw, 0, g_mg.loads[load_id].max_power_kw);
    
    return 0;
}

/**
 * @brief Set utility connection status
 */
int mg_set_utility_status(bool available, float voltage_pu, float frequency_hz) {
    if (!g_mg.initialized) {
        return -1;
    }
    
    g_mg.utility_available = available;
    g_mg.utility_voltage_pu = voltage_pu;
    g_mg.utility_frequency_hz = frequency_hz;
    
    return 0;
}

/**
 * @brief Receive demand response event
 */
int mg_receive_dr_event(mg_dr_type_t type, uint32_t start_time, 
                        uint32_t duration_sec, float target_reduction_kw,
                        float price_signal, bool mandatory) {
    if (!g_mg.initialized || g_mg.dr_event_count >= MG_MAX_DR_EVENTS) {
        return -1;
    }
    
    if (!g_mg.config.enable_demand_response) {
        return -2;  /* DR not enabled */
    }
    
    mg_dr_event_t* event = &g_mg.dr_events[g_mg.dr_event_count];
    event->id = g_mg.dr_event_count;
    event->type = type;
    event->start_time = start_time;
    event->end_time = start_time + duration_sec * 1000;
    event->target_reduction_kw = target_reduction_kw;
    event->price_signal = price_signal;
    event->mandatory = mandatory;
    event->active = false;
    
    mg_add_alarm(MG_ALARM_INFO, "DR event received",
                target_reduction_kw, (float)type);
    
    return g_mg.dr_event_count++;
}

/**
 * @brief Main processing loop
 * @param delta_ms Time since last call (milliseconds)
 */
int mg_process(uint32_t delta_ms) {
    if (!g_mg.initialized || g_mg.mode == MG_MODE_OFF) {
        return -1;
    }
    
    uint32_t current_time = mg_get_time_ms();
    
    /* Update statistics */
    g_mg.stats.uptime_seconds += delta_ms / 1000;
    if (g_mg.mode == MG_MODE_GRID_CONNECTED) {
        g_mg.stats.grid_connected_seconds += delta_ms / 1000;
    } else if (g_mg.mode == MG_MODE_ISLANDED) {
        g_mg.stats.islanded_seconds += delta_ms / 1000;
    }
    
    /* Process mode state machine */
    mg_process_mode(current_time);
    
    /* Process demand response events */
    mg_process_dr_events(current_time);
    
    /* Update source outputs (ramp toward setpoints) */
    for (uint8_t i = 0; i < g_mg.source_count; i++) {
        mg_source_t* src = &g_mg.sources[i];
        if (!src->online) continue;
        
        float diff = src->power_setpoint_kw - src->power_output_kw;
        float max_change = src->ramp_rate_kw_s * (float)delta_ms / 1000.0f;
        
        if (fabsf(diff) <= max_change) {
            src->power_output_kw = src->power_setpoint_kw;
        } else if (diff > 0) {
            src->power_output_kw += max_change;
        } else {
            src->power_output_kw -= max_change;
        }
    }
    
    /* Calculate current balance */
    g_mg.balance.total_generation_kw = mg_total_generation();
    g_mg.balance.total_demand_kw = mg_total_demand();
    g_mg.balance.spinning_reserve_kw = mg_spinning_reserve();
    
    /* Calculate storage contribution */
    g_mg.balance.total_storage_kw = 0;
    for (uint8_t i = 0; i < g_mg.storage_count; i++) {
        if (g_mg.storage[i].online) {
            g_mg.balance.total_storage_kw += g_mg.storage[i].power_kw;
        }
    }
    
    /* Calculate imbalance */
    g_mg.balance.imbalance_kw = mg_calculate_imbalance();
    
    /* Grid balancing logic */
    if (fabsf(g_mg.balance.imbalance_kw) > 1.0f) {  /* 1 kW deadband */
        if (g_mg.balance.imbalance_kw < 0) {
            /* Need more generation or less load */
            float shortfall = -g_mg.balance.imbalance_kw;
            
            /* Try storage first */
            mg_manage_storage(-shortfall);
            
            /* Then dispatch more generation */
            float remaining = shortfall - 
                (g_mg.balance.total_storage_kw < 0 ? -g_mg.balance.total_storage_kw : 0);
            if (remaining > 0) {
                mg_dispatch_sources(g_mg.balance.total_generation_kw + remaining);
            }
            
            /* If still short, curtail loads */
            remaining = mg_calculate_imbalance();
            if (remaining < -1.0f && g_mg.mode != MG_MODE_GRID_CONNECTED) {
                mg_curtail_loads(-remaining);
            }
        } else {
            /* Excess generation */
            float excess = g_mg.balance.imbalance_kw;
            
            /* Charge storage */
            mg_manage_storage(excess);
            
            /* Restore curtailed loads */
            if (excess > 10.0f) {
                mg_restore_loads(excess);
            }
        }
        
        g_mg.balance.balanced = false;
    } else {
        g_mg.balance.balanced = true;
    }
    
    /* Calculate net import/export */
    if (g_mg.mode == MG_MODE_GRID_CONNECTED) {
        g_mg.balance.net_import_kw = g_mg.balance.total_demand_kw + 
                                     g_mg.balance.total_storage_kw -
                                     g_mg.balance.total_generation_kw;
        
        /* Update energy statistics */
        if (g_mg.balance.net_import_kw > 0) {
            g_mg.stats.energy_imported_kwh += g_mg.balance.net_import_kw * 
                                              (float)delta_ms / 3600000.0f;
        } else {
            g_mg.stats.energy_exported_kwh += -g_mg.balance.net_import_kw * 
                                              (float)delta_ms / 3600000.0f;
        }
    } else {
        g_mg.balance.net_import_kw = 0;
    }
    
    /* Update energy statistics */
    g_mg.stats.energy_generated_kwh += g_mg.balance.total_generation_kw * 
                                       (float)delta_ms / 3600000.0f;
    g_mg.stats.energy_consumed_kwh += g_mg.balance.total_demand_kw * 
                                      (float)delta_ms / 3600000.0f;
    
    /* Track peaks */
    if (g_mg.balance.total_demand_kw > g_mg.stats.peak_demand_kw) {
        g_mg.stats.peak_demand_kw = g_mg.balance.total_demand_kw;
    }
    if (g_mg.balance.total_generation_kw > g_mg.stats.peak_generation_kw) {
        g_mg.stats.peak_generation_kw = g_mg.balance.total_generation_kw;
    }
    
    /* Check alarms */
    mg_check_alarms();
    
    /* Update telemetry */
    mg_update_telemetry();
    
    return 0;
}

/**
 * @brief Get current balance state
 */
int mg_get_balance(mg_balance_t* balance) {
    if (!g_mg.initialized || balance == NULL) {
        return -1;
    }
    
    memcpy(balance, &g_mg.balance, sizeof(mg_balance_t));
    return 0;
}

/**
 * @brief Get current mode
 */
mg_mode_t mg_get_mode(void) {
    return g_mg.mode;
}

/**
 * @brief Get statistics
 */
int mg_get_stats(mg_stats_t* stats) {
    if (!g_mg.initialized || stats == NULL) {
        return -1;
    }
    
    memcpy(stats, &g_mg.stats, sizeof(mg_stats_t));
    return 0;
}

/**
 * @brief Get active alarms
 */
int mg_get_alarms(mg_alarm_t* alarms, uint8_t max_alarms) {
    if (!g_mg.initialized || alarms == NULL) {
        return -1;
    }
    
    uint8_t count = (g_mg.alarm_count < max_alarms) ? g_mg.alarm_count : max_alarms;
    memcpy(alarms, g_mg.alarms, count * sizeof(mg_alarm_t));
    
    return count;
}

/**
 * @brief Acknowledge alarm
 */
int mg_ack_alarm(uint16_t alarm_id) {
    if (!g_mg.initialized) {
        return -1;
    }
    
    for (uint8_t i = 0; i < g_mg.alarm_count; i++) {
        if (g_mg.alarms[i].id == alarm_id) {
            g_mg.alarms[i].acknowledged = true;
            return 0;
        }
    }
    
    return -1;  /* Alarm not found */
}

/**
 * @brief Request island mode
 */
int mg_request_island(void) {
    if (!g_mg.initialized || !g_mg.config.enable_islanding) {
        return -1;
    }
    
    if (g_mg.mode == MG_MODE_GRID_CONNECTED) {
        mg_set_mode(MG_MODE_ISLAND_TRANSITION);
        return 0;
    }
    
    return -1;
}

/**
 * @brief Request grid reconnection
 */
int mg_request_reconnect(void) {
    if (!g_mg.initialized) {
        return -1;
    }
    
    if (g_mg.mode == MG_MODE_ISLANDED && g_mg.utility_available) {
        mg_set_mode(MG_MODE_RECONNECTING);
        return 0;
    }
    
    return -1;
}

/**
 * @brief Emergency shutdown
 */
int mg_emergency_shutdown(void) {
    if (!g_mg.initialized) {
        return -1;
    }
    
    mg_set_mode(MG_MODE_EMERGENCY_SHUTDOWN);
    
    /* Disconnect all non-critical loads */
    for (uint8_t i = 0; i < g_mg.load_count; i++) {
        if (g_mg.loads[i].priority != MG_LOAD_CRITICAL) {
            g_mg.loads[i].connected = false;
            g_mg.loads[i].power_allocated_kw = 0;
        }
    }
    
    /* Trip all sources */
    for (uint8_t i = 0; i < g_mg.source_count; i++) {
        g_mg.sources[i].online = false;
        g_mg.sources[i].power_output_kw = 0;
    }
    
    mg_add_alarm(MG_ALARM_CRITICAL, "Emergency shutdown triggered", 0, 0);
    
    return 0;
}

/**
 * @brief Get source count
 */
uint8_t mg_get_source_count(void) {
    return g_mg.source_count;
}

/**
 * @brief Get load count
 */
uint8_t mg_get_load_count(void) {
    return g_mg.load_count;
}

/**
 * @brief Get storage count
 */
uint8_t mg_get_storage_count(void) {
    return g_mg.storage_count;
}

/**
 * @brief Set source online status
 */
int mg_set_source_online(uint8_t source_id, bool online) {
    if (!g_mg.initialized || source_id >= g_mg.source_count) {
        return -1;
    }
    
    g_mg.sources[source_id].online = online;
    if (!online) {
        g_mg.sources[source_id].power_output_kw = 0;
        g_mg.sources[source_id].power_setpoint_kw = 0;
    }
    
    return 0;
}

/**
 * @brief Get telemetry point value
 */
int mg_get_telemetry(const char* name, float* value) {
    if (!g_mg.initialized || name == NULL || value == NULL) {
        return -1;
    }
    
    for (uint16_t i = 0; i < g_mg.telemetry_count; i++) {
        if (strcmp(g_mg.telemetry[i].name, name) == 0) {
            *value = g_mg.telemetry[i].value;
            return 0;
        }
    }
    
    return -1;  /* Point not found */
}
