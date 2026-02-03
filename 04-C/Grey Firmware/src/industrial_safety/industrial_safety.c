/**
 * @file industrial_safety.c
 * @brief Industrial Safety Domain Implementation Stubs
 * 
 * This module provides stub implementations for industrial safety functionality
 * including gas detection, emergency shutdown systems, and compliance logging.
 * Production implementations would integrate with actual safety-rated hardware
 * and undergo SIL certification.
 */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#include "industrial_safety/gas_sensor.h"
#include "industrial_safety/emergency_shutdown.h"
#include "industrial_safety/safety_logger.h"

/*===========================================================================*/
/* Error Codes                                                                */
/*===========================================================================*/

#define GF_OK                   0
#define GF_ERR_INVALID_PARAM   -1
#define GF_ERR_NO_RESOURCE     -2
#define GF_ERR_NOT_READY       -3

/*===========================================================================*/
/* Gas Sensor State                                                           */
/*===========================================================================*/

static struct {
    bool                initialized;
    gf_gas_config_t     sensors[GF_GAS_MAX_SENSORS];
    gf_gas_reading_t    readings[GF_GAS_MAX_SENSORS];
    gf_gas_cal_t        calibrations[GF_GAS_MAX_SENSORS];
    int                 sensor_count;
    gf_gas_alarm_cb_t   alarm_cb;
    void               *alarm_ctx;
    gf_gas_fault_cb_t   fault_cb;
    void               *fault_ctx;
    uint32_t            sample_count;
} g_gas;

/*===========================================================================*/
/* Gas Sensor API Implementation                                              */
/*===========================================================================*/

int gf_gas_init(void)
{
    memset(&g_gas, 0, sizeof(g_gas));
    g_gas.initialized = true;
    return GF_OK;
}

int gf_gas_register(const gf_gas_config_t *config)
{
    if (!config || g_gas.sensor_count >= GF_GAS_MAX_SENSORS) {
        return GF_ERR_INVALID_PARAM;
    }
    
    int idx = g_gas.sensor_count;
    memcpy(&g_gas.sensors[idx], config, sizeof(gf_gas_config_t));
    g_gas.readings[idx].channel = config->channel;
    g_gas.readings[idx].gas_type = config->gas_type;
    g_gas.readings[idx].state = GF_GAS_STATE_OFF;
    
    /* Default calibration */
    g_gas.calibrations[idx].zero_offset = 0.0f;
    g_gas.calibrations[idx].span_factor = 1.0f;
    g_gas.calibrations[idx].temp_coeff = 0.02f;
    
    g_gas.sensor_count++;
    return idx;
}

int gf_gas_start(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_gas.readings[channel].state = GF_GAS_STATE_WARMING;
    /* After warmup period, would transition to READY */
    g_gas.readings[channel].state = GF_GAS_STATE_READY;
    return GF_OK;
}

int gf_gas_stop(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_gas.readings[channel].state = GF_GAS_STATE_OFF;
    return GF_OK;
}

int gf_gas_get_reading(uint8_t channel, gf_gas_reading_t *reading)
{
    if (channel >= g_gas.sensor_count || !reading) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(reading, &g_gas.readings[channel], sizeof(gf_gas_reading_t));
    return GF_OK;
}

int gf_gas_get_all_readings(gf_gas_reading_t *readings, int max_readings)
{
    int count = (g_gas.sensor_count < max_readings) ? 
                 g_gas.sensor_count : max_readings;
    memcpy(readings, g_gas.readings, count * sizeof(gf_gas_reading_t));
    return count;
}

int gf_gas_set_alarms(uint8_t channel, float low, float high, float critical)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_gas.sensors[channel].alarm_low = low;
    g_gas.sensors[channel].alarm_high = high;
    g_gas.sensors[channel].alarm_critical = critical;
    return GF_OK;
}

int gf_gas_ack_alarm(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    /* Alarm acknowledgment logic */
    return GF_OK;
}

int gf_gas_calibrate_zero(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_gas.readings[channel].state = GF_GAS_STATE_CALIBRATING;
    /* Stub: would sample sensor in clean air */
    g_gas.calibrations[channel].zero_offset = 0.0f;
    g_gas.readings[channel].state = GF_GAS_STATE_READY;
    return GF_OK;
}

int gf_gas_calibrate_span(uint8_t channel, float gas_concentration)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_gas.readings[channel].state = GF_GAS_STATE_CALIBRATING;
    /* Stub: would calculate span factor */
    g_gas.calibrations[channel].span_factor = 1.0f;
    g_gas.calibrations[channel].cal_gas_conc = (uint8_t)gas_concentration;
    g_gas.readings[channel].state = GF_GAS_STATE_READY;
    return GF_OK;
}

int gf_gas_get_calibration(uint8_t channel, gf_gas_cal_t *cal)
{
    if (channel >= g_gas.sensor_count || !cal) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(cal, &g_gas.calibrations[channel], sizeof(gf_gas_cal_t));
    return GF_OK;
}

int gf_gas_set_calibration(uint8_t channel, const gf_gas_cal_t *cal)
{
    if (channel >= g_gas.sensor_count || !cal) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_gas.calibrations[channel], cal, sizeof(gf_gas_cal_t));
    return GF_OK;
}

int gf_gas_register_alarm_cb(gf_gas_alarm_cb_t callback, void *ctx)
{
    g_gas.alarm_cb = callback;
    g_gas.alarm_ctx = ctx;
    return GF_OK;
}

int gf_gas_register_fault_cb(gf_gas_fault_cb_t callback, void *ctx)
{
    g_gas.fault_cb = callback;
    g_gas.fault_ctx = ctx;
    return GF_OK;
}

int gf_gas_reset_peak(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_gas.readings[channel].peak = 0.0f;
    return GF_OK;
}

int gf_gas_reset_twa(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_gas.readings[channel].twa_8hr = 0.0f;
    g_gas.readings[channel].stel_15min = 0.0f;
    return GF_OK;
}

int gf_gas_self_test(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    /* Stub: would run sensor diagnostics */
    return GF_OK;
}

int gf_gas_get_lifetime_days(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_ERR_INVALID_PARAM;
    }
    return g_gas.sensors[channel].lifetime_days;
}

int gf_gas_process(void)
{
    g_gas.sample_count++;
    
    for (int i = 0; i < g_gas.sensor_count; i++) {
        if (g_gas.readings[i].state != GF_GAS_STATE_READY) {
            continue;
        }
        
        /* Stub: simulate sensor reading */
        g_gas.readings[i].concentration = 0.5f; /* Normal level */
        g_gas.readings[i].confidence = 95;
        
        /* Check alarms */
        gf_gas_alarm_t new_alarm = GF_GAS_ALARM_NONE;
        float conc = g_gas.readings[i].concentration;
        
        if (conc >= g_gas.sensors[i].alarm_critical) {
            new_alarm = GF_GAS_ALARM_CRITICAL;
        } else if (conc >= g_gas.sensors[i].alarm_high) {
            new_alarm = GF_GAS_ALARM_HIGH;
        } else if (conc >= g_gas.sensors[i].alarm_low) {
            new_alarm = GF_GAS_ALARM_LOW;
        }
        
        if (new_alarm != g_gas.readings[i].alarm) {
            g_gas.readings[i].alarm = new_alarm;
            if (g_gas.alarm_cb && new_alarm != GF_GAS_ALARM_NONE) {
                g_gas.alarm_cb(i, g_gas.sensors[i].gas_type, new_alarm,
                              conc, g_gas.alarm_ctx);
            }
        }
        
        /* Update peak */
        if (conc > g_gas.readings[i].peak) {
            g_gas.readings[i].peak = conc;
        }
    }
    
    return 0;
}

gf_gas_state_t gf_gas_get_state(uint8_t channel)
{
    if (channel >= g_gas.sensor_count) {
        return GF_GAS_STATE_FAULT;
    }
    return g_gas.readings[channel].state;
}

/*===========================================================================*/
/* Emergency Shutdown System State                                            */
/*===========================================================================*/

static struct {
    bool                    initialized;
    gf_esd_state_t          state;
    gf_esd_input_config_t   inputs[GF_ESD_MAX_INPUTS];
    gf_esd_input_state_t    input_states[GF_ESD_MAX_INPUTS];
    int                     input_count;
    gf_esd_output_config_t  outputs[GF_ESD_MAX_OUTPUTS];
    gf_esd_output_state_t   output_states[GF_ESD_MAX_OUTPUTS];
    int                     output_count;
    gf_esd_function_t       functions[GF_ESD_MAX_FUNCTIONS];
    int                     function_count;
    gf_esd_trip_cb_t        trip_cb;
    void                   *trip_ctx;
    gf_esd_fault_cb_t       fault_cb;
    void                   *fault_ctx;
    uint32_t                trip_count;
} g_esd;

/*===========================================================================*/
/* Emergency Shutdown API Implementation                                      */
/*===========================================================================*/

int gf_esd_init(void)
{
    memset(&g_esd, 0, sizeof(g_esd));
    g_esd.state = GF_ESD_STATE_INIT;
    g_esd.initialized = true;
    return GF_OK;
}

int gf_esd_register_input(const gf_esd_input_config_t *config)
{
    if (!config || g_esd.input_count >= GF_ESD_MAX_INPUTS) {
        return GF_ERR_INVALID_PARAM;
    }
    int idx = g_esd.input_count;
    memcpy(&g_esd.inputs[idx], config, sizeof(gf_esd_input_config_t));
    g_esd.input_states[idx] = GF_ESD_INPUT_NORMAL;
    g_esd.input_count++;
    return idx;
}

int gf_esd_register_output(const gf_esd_output_config_t *config)
{
    if (!config || g_esd.output_count >= GF_ESD_MAX_OUTPUTS) {
        return GF_ERR_INVALID_PARAM;
    }
    int idx = g_esd.output_count;
    memcpy(&g_esd.outputs[idx], config, sizeof(gf_esd_output_config_t));
    g_esd.output_states[idx] = GF_ESD_OUTPUT_OPERATIONAL;
    g_esd.output_count++;
    return idx;
}

int gf_esd_register_function(const gf_esd_function_t *function)
{
    if (!function || g_esd.function_count >= GF_ESD_MAX_FUNCTIONS) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(&g_esd.functions[g_esd.function_count], function,
           sizeof(gf_esd_function_t));
    return g_esd.function_count++;
}

int gf_esd_start(void)
{
    if (!g_esd.initialized) {
        return GF_ERR_NOT_READY;
    }
    g_esd.state = GF_ESD_STATE_OPERATIONAL;
    return GF_OK;
}

int gf_esd_stop(void)
{
    g_esd.state = GF_ESD_STATE_MAINTENANCE;
    return GF_OK;
}

gf_esd_state_t gf_esd_get_state(void)
{
    return g_esd.state;
}

gf_esd_input_state_t gf_esd_get_input_state(uint8_t input_id)
{
    if (input_id >= g_esd.input_count) {
        return GF_ESD_INPUT_FAULT;
    }
    return g_esd.input_states[input_id];
}

gf_esd_output_state_t gf_esd_get_output_state(uint8_t output_id)
{
    if (output_id >= g_esd.output_count) {
        return GF_ESD_OUTPUT_FAULT;
    }
    return g_esd.output_states[output_id];
}

int gf_esd_trip(uint8_t function_id)
{
    if (function_id >= g_esd.function_count) {
        return GF_ERR_INVALID_PARAM;
    }
    
    gf_esd_function_t *func = &g_esd.functions[function_id];
    
    /* Trip all associated outputs */
    for (int i = 0; i < func->output_count; i++) {
        uint8_t out_id = func->outputs[i];
        if (out_id < g_esd.output_count) {
            g_esd.output_states[out_id] = GF_ESD_OUTPUT_TRIPPED;
        }
    }
    
    g_esd.state = GF_ESD_STATE_TRIP;
    g_esd.trip_count++;
    
    if (g_esd.trip_cb) {
        g_esd.trip_cb(function_id, func->inputs, func->input_count,
                     g_esd.trip_ctx);
    }
    
    return GF_OK;
}

int gf_esd_reset(uint8_t function_id)
{
    if (function_id == 0xFF) {
        /* Reset all */
        for (int i = 0; i < g_esd.output_count; i++) {
            if (g_esd.output_states[i] == GF_ESD_OUTPUT_TRIPPED) {
                g_esd.output_states[i] = GF_ESD_OUTPUT_OPERATIONAL;
            }
        }
        g_esd.state = GF_ESD_STATE_OPERATIONAL;
    } else if (function_id < g_esd.function_count) {
        gf_esd_function_t *func = &g_esd.functions[function_id];
        for (int i = 0; i < func->output_count; i++) {
            uint8_t out_id = func->outputs[i];
            if (out_id < g_esd.output_count) {
                g_esd.output_states[out_id] = GF_ESD_OUTPUT_OPERATIONAL;
            }
        }
    }
    return GF_OK;
}

int gf_esd_bypass_input(uint8_t input_id, uint16_t duration_hr,
                        uint16_t reason)
{
    (void)duration_hr;
    (void)reason;
    if (input_id >= g_esd.input_count) {
        return GF_ERR_INVALID_PARAM;
    }
    if (!g_esd.inputs[input_id].can_bypass) {
        return GF_ERR_INVALID_PARAM;
    }
    g_esd.input_states[input_id] = GF_ESD_INPUT_BYPASSED;
    return GF_OK;
}

int gf_esd_remove_bypass(uint8_t input_id)
{
    if (input_id >= g_esd.input_count) {
        return GF_ERR_INVALID_PARAM;
    }
    g_esd.input_states[input_id] = GF_ESD_INPUT_NORMAL;
    return GF_OK;
}

int gf_esd_force_output(uint8_t output_id, bool state, uint16_t duration_sec)
{
    (void)state;
    (void)duration_sec;
    if (output_id >= g_esd.output_count) {
        return GF_ERR_INVALID_PARAM;
    }
    /* Stub: force output for testing */
    return GF_OK;
}

int gf_esd_partial_stroke_test(uint8_t output_id)
{
    if (output_id >= g_esd.output_count) {
        return GF_ERR_INVALID_PARAM;
    }
    /* Stub: partial stroke test for valve diagnostics */
    return GF_OK;
}

int gf_esd_proof_test(uint8_t function_id)
{
    if (function_id >= g_esd.function_count) {
        return GF_ERR_INVALID_PARAM;
    }
    /* Stub: full proof test */
    return GF_OK;
}

int gf_esd_get_diagnostics(uint8_t function_id, gf_esd_diagnostics_t *diag)
{
    if (function_id >= g_esd.function_count || !diag) {
        return GF_ERR_INVALID_PARAM;
    }
    
    memset(diag, 0, sizeof(gf_esd_diagnostics_t));
    diag->total_trips = g_esd.trip_count;
    diag->diagnostic_coverage = 60.0f; /* 60% DC typical */
    diag->pfd_avg = 0.01f; /* 10^-2 */
    
    return GF_OK;
}

int gf_esd_register_trip_cb(gf_esd_trip_cb_t callback, void *ctx)
{
    g_esd.trip_cb = callback;
    g_esd.trip_ctx = ctx;
    return GF_OK;
}

int gf_esd_register_fault_cb(gf_esd_fault_cb_t callback, void *ctx)
{
    g_esd.fault_cb = callback;
    g_esd.fault_ctx = ctx;
    return GF_OK;
}

int gf_esd_process(void)
{
    if (g_esd.state != GF_ESD_STATE_OPERATIONAL) {
        return 0;
    }
    
    /* Process each safety function */
    for (int f = 0; f < g_esd.function_count; f++) {
        gf_esd_function_t *func = &g_esd.functions[f];
        if (!func->enabled) continue;
        
        int tripped_count = 0;
        for (int i = 0; i < func->input_count; i++) {
            uint8_t in_id = func->inputs[i];
            if (in_id < g_esd.input_count) {
                if (g_esd.input_states[in_id] == GF_ESD_INPUT_TRIPPED) {
                    tripped_count++;
                }
            }
        }
        
        /* Voting logic */
        bool should_trip = false;
        switch (func->voting) {
            case GF_VOTE_1OO1:
                should_trip = (tripped_count >= 1);
                break;
            case GF_VOTE_1OO2:
                should_trip = (tripped_count >= 1);
                break;
            case GF_VOTE_2OO2:
                should_trip = (tripped_count >= 2);
                break;
            case GF_VOTE_2OO3:
                should_trip = (tripped_count >= 2);
                break;
            default:
                should_trip = (tripped_count >= 1);
                break;
        }
        
        if (should_trip) {
            gf_esd_trip(f);
        }
    }
    
    return 0;
}

int gf_esd_get_bypass_count(void)
{
    int count = 0;
    for (int i = 0; i < g_esd.input_count; i++) {
        if (g_esd.input_states[i] == GF_ESD_INPUT_BYPASSED) {
            count++;
        }
    }
    return count;
}

bool gf_esd_any_trip_active(void)
{
    return g_esd.state == GF_ESD_STATE_TRIP;
}

uint32_t gf_esd_measure_response_time(uint8_t function_id)
{
    (void)function_id;
    return 50; /* 50ms typical */
}

/*===========================================================================*/
/* Safety Logger State                                                        */
/*===========================================================================*/

static struct {
    bool                initialized;
    gf_slog_entry_t     entries[GF_SLOG_MAX_ENTRIES];
    int                 entry_count;
    uint32_t            next_sequence;
    gf_slog_callback_t  callback;
    void               *callback_ctx;
    uint16_t            retention_days;
} g_slog;

/*===========================================================================*/
/* Safety Logger API Implementation                                           */
/*===========================================================================*/

int gf_slog_init(void)
{
    memset(&g_slog, 0, sizeof(g_slog));
    g_slog.next_sequence = 1;
    g_slog.retention_days = 365;
    g_slog.initialized = true;
    return GF_OK;
}

static uint32_t slog_calculate_checksum(const gf_slog_entry_t *entry)
{
    /* Simple checksum for integrity */
    const uint8_t *data = (const uint8_t *)entry;
    uint32_t sum = 0;
    for (size_t i = 0; i < sizeof(gf_slog_entry_t) - sizeof(uint32_t); i++) {
        sum = ((sum << 5) + sum) + data[i];
    }
    return sum;
}

int gf_slog_log(gf_slog_category_t category, gf_slog_severity_t severity,
                uint16_t event_code, const char *equipment_tag,
                const char *message, const char *user_id,
                float value_before, float value_after,
                gf_slog_action_t action)
{
    gf_slog_entry_t entry = {0};
    
    entry.sequence = g_slog.next_sequence++;
    entry.timestamp = 1700000000; /* Stub timestamp */
    entry.timestamp_ms = 0;
    entry.category = category;
    entry.severity = severity;
    entry.event_code = event_code;
    entry.value_before = value_before;
    entry.value_after = value_after;
    entry.required_action = action;
    
    if (equipment_tag) {
        strncpy(entry.equipment_tag, equipment_tag, GF_SLOG_TAG_LEN - 1);
    }
    if (message) {
        strncpy(entry.message, message, GF_SLOG_MESSAGE_LEN - 1);
    }
    if (user_id) {
        strncpy(entry.user_id, user_id, GF_SLOG_USER_LEN - 1);
    }
    
    entry.checksum = slog_calculate_checksum(&entry);
    
    return gf_slog_log_entry(&entry);
}

int gf_slog_log_entry(const gf_slog_entry_t *entry)
{
    if (!entry || g_slog.entry_count >= GF_SLOG_MAX_ENTRIES) {
        return GF_ERR_INVALID_PARAM;
    }
    
    memcpy(&g_slog.entries[g_slog.entry_count], entry, sizeof(gf_slog_entry_t));
    
    if (g_slog.callback) {
        g_slog.callback(entry, g_slog.callback_ctx);
    }
    
    return g_slog.entries[g_slog.entry_count++].sequence;
}

int gf_slog_get_entry(uint32_t sequence, gf_slog_entry_t *entry)
{
    if (!entry) {
        return GF_ERR_INVALID_PARAM;
    }
    
    for (int i = 0; i < g_slog.entry_count; i++) {
        if (g_slog.entries[i].sequence == sequence) {
            memcpy(entry, &g_slog.entries[i], sizeof(gf_slog_entry_t));
            return GF_OK;
        }
    }
    return GF_ERR_INVALID_PARAM;
}

int gf_slog_query(const gf_slog_filter_t *filter, gf_slog_entry_t *entries,
                  int max_entries, int offset)
{
    if (!entries) {
        return GF_ERR_INVALID_PARAM;
    }
    
    int count = 0;
    int skipped = 0;
    
    for (int i = 0; i < g_slog.entry_count && count < max_entries; i++) {
        gf_slog_entry_t *e = &g_slog.entries[i];
        
        /* Apply filters */
        if (filter) {
            if (filter->start_time && e->timestamp < filter->start_time) continue;
            if (filter->end_time && e->timestamp > filter->end_time) continue;
            if (filter->category != 0xFF && e->category != filter->category) continue;
            if (e->severity < filter->min_severity) continue;
            if (filter->unacknowledged_only && e->acknowledged) continue;
        }
        
        if (skipped < offset) {
            skipped++;
            continue;
        }
        
        memcpy(&entries[count++], e, sizeof(gf_slog_entry_t));
    }
    
    return count;
}

int gf_slog_count(const gf_slog_filter_t *filter)
{
    if (!filter) {
        return g_slog.entry_count;
    }
    
    int count = 0;
    for (int i = 0; i < g_slog.entry_count; i++) {
        gf_slog_entry_t *e = &g_slog.entries[i];
        if (filter->category != 0xFF && e->category != filter->category) continue;
        if (e->severity < filter->min_severity) continue;
        count++;
    }
    return count;
}

int gf_slog_acknowledge(uint32_t sequence, const char *user_id)
{
    for (int i = 0; i < g_slog.entry_count; i++) {
        if (g_slog.entries[i].sequence == sequence) {
            g_slog.entries[i].acknowledged = true;
            g_slog.entries[i].ack_timestamp = 1700000000;
            if (user_id) {
                strncpy(g_slog.entries[i].ack_user, user_id, GF_SLOG_USER_LEN - 1);
            }
            return GF_OK;
        }
    }
    return GF_ERR_INVALID_PARAM;
}

int gf_slog_acknowledge_range(uint32_t start_seq, uint32_t end_seq,
                               const char *user_id)
{
    int count = 0;
    for (uint32_t seq = start_seq; seq <= end_seq; seq++) {
        if (gf_slog_acknowledge(seq, user_id) == GF_OK) {
            count++;
        }
    }
    return count;
}

int gf_slog_get_unacknowledged(gf_slog_entry_t *entries, int max_entries)
{
    gf_slog_filter_t filter = {0};
    filter.category = 0xFF;
    filter.unacknowledged_only = true;
    return gf_slog_query(&filter, entries, max_entries, 0);
}

int gf_slog_get_stats(gf_slog_stats_t *stats)
{
    if (!stats) {
        return GF_ERR_INVALID_PARAM;
    }
    
    memset(stats, 0, sizeof(gf_slog_stats_t));
    stats->total_entries = g_slog.entry_count;
    
    for (int i = 0; i < g_slog.entry_count; i++) {
        if (g_slog.entries[i].severity >= GF_SLOG_SEV_CRITICAL) {
            stats->critical_count++;
        }
        if (!g_slog.entries[i].acknowledged) {
            stats->unacknowledged++;
        }
    }
    
    if (g_slog.entry_count > 0) {
        stats->oldest_timestamp = g_slog.entries[0].timestamp;
        stats->newest_timestamp = g_slog.entries[g_slog.entry_count - 1].timestamp;
    }
    
    stats->storage_used_pct = (g_slog.entry_count * 100) / GF_SLOG_MAX_ENTRIES;
    
    return GF_OK;
}

int gf_slog_register_callback(gf_slog_callback_t callback, void *ctx)
{
    g_slog.callback = callback;
    g_slog.callback_ctx = ctx;
    return GF_OK;
}

int gf_slog_verify_integrity(uint32_t start_seq, uint32_t end_seq)
{
    for (int i = 0; i < g_slog.entry_count; i++) {
        if (g_slog.entries[i].sequence >= start_seq && 
            g_slog.entries[i].sequence <= end_seq) {
            uint32_t calc = slog_calculate_checksum(&g_slog.entries[i]);
            if (calc != g_slog.entries[i].checksum) {
                return g_slog.entries[i].sequence;
            }
        }
    }
    return 0; /* All intact */
}

int gf_slog_export_csv(const gf_slog_filter_t *filter, 
                       char *buffer, int buffer_size)
{
    (void)filter;
    if (!buffer || buffer_size < 100) {
        return GF_ERR_INVALID_PARAM;
    }
    
    int len = snprintf(buffer, buffer_size,
                      "Sequence,Timestamp,Category,Severity,Code,Tag,Message\n");
    return len;
}

int gf_slog_archive(uint32_t before_timestamp)
{
    (void)before_timestamp;
    /* Stub: would archive old entries */
    return GF_OK;
}

int gf_slog_set_retention(uint16_t days)
{
    g_slog.retention_days = days;
    return GF_OK;
}

int gf_slog_sync(void)
{
    /* Stub: sync to persistent storage */
    return GF_OK;
}

int gf_slog_clear(uint32_t auth_code)
{
    if (auth_code != 0xDEADBEEF) {
        return GF_ERR_INVALID_PARAM; /* Auth required */
    }
    g_slog.entry_count = 0;
    g_slog.next_sequence = 1;
    return GF_OK;
}
