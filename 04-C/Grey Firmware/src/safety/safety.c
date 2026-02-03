/**
 * @file safety.c
 * @brief Safety-Critical System Implementation
 */

#include "safety/safety.h"
#include "core/error_handler.h"
#include <string.h>

/* Local error codes (if error_handler.h not available) */
#ifndef GF_OK
#define GF_OK                       0
#define GF_ERROR_INVALID_PARAM      -1
#define GF_ERROR_NOT_INITIALIZED    -2
#endif

/*===========================================================================*/
/* Redundancy Checker Implementation                                          */
/*===========================================================================*/

static gf_redundancy_config_t s_redundancy_config;
static gf_redundancy_status_t s_redundancy_status;

int gf_redundancy_init(const gf_redundancy_config_t *config)
{
    if (!config) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    memcpy(&s_redundancy_config, config, sizeof(gf_redundancy_config_t));
    memset(&s_redundancy_status, 0, sizeof(gf_redundancy_status_t));
    s_redundancy_status.active_channel = 0;
    
    return GF_OK;
}

gf_redundancy_result_t gf_redundancy_check_u32(const uint32_t *values,
                                                uint8_t count,
                                                uint32_t *voted_value)
{
    if (!values || count < 2 || !voted_value) {
        return GF_REDUNDANCY_CHANNEL_FAULT;
    }
    
    if (count == 2) {
        /* Dual Modular Redundancy - detect disagreement */
        int64_t diff = (int64_t)values[0] - (int64_t)values[1];
        if (diff < 0) diff = -diff;
        
        if ((uint32_t)diff <= s_redundancy_config.tolerance) {
            *voted_value = values[s_redundancy_status.active_channel];
            s_redundancy_status.last_result = GF_REDUNDANCY_OK;
            return GF_REDUNDANCY_OK;
        }
        
        s_redundancy_status.mismatch_count++;
        s_redundancy_status.last_result = GF_REDUNDANCY_MISMATCH;
        
        if (s_redundancy_config.auto_failover) {
            /* Use last known good channel */
            *voted_value = values[s_redundancy_status.active_channel];
        }
        return GF_REDUNDANCY_MISMATCH;
    }
    
    if (count >= 3) {
        /* Triple Modular Redundancy - majority voting */
        uint32_t v0 = values[0], v1 = values[1], v2 = values[2];
        uint32_t tol = s_redundancy_config.tolerance;
        
        int match_01 = ((v0 > v1) ? (v0 - v1) : (v1 - v0)) <= tol;
        int match_02 = ((v0 > v2) ? (v0 - v2) : (v2 - v0)) <= tol;
        int match_12 = ((v1 > v2) ? (v1 - v2) : (v2 - v1)) <= tol;
        
        if (match_01 && match_02 && match_12) {
            /* All agree */
            *voted_value = v0;
            s_redundancy_status.last_result = GF_REDUNDANCY_OK;
            return GF_REDUNDANCY_OK;
        } else if (match_01) {
            /* Channel 2 failed */
            *voted_value = v0;
            s_redundancy_status.failed_channels |= (1 << 2);
            s_redundancy_status.last_result = GF_REDUNDANCY_CHANNEL_FAULT;
            return GF_REDUNDANCY_CHANNEL_FAULT;
        } else if (match_02) {
            /* Channel 1 failed */
            *voted_value = v0;
            s_redundancy_status.failed_channels |= (1 << 1);
            s_redundancy_status.last_result = GF_REDUNDANCY_CHANNEL_FAULT;
            return GF_REDUNDANCY_CHANNEL_FAULT;
        } else if (match_12) {
            /* Channel 0 failed */
            *voted_value = v1;
            s_redundancy_status.failed_channels |= (1 << 0);
            s_redundancy_status.last_result = GF_REDUNDANCY_CHANNEL_FAULT;
            return GF_REDUNDANCY_CHANNEL_FAULT;
        }
        
        /* No majority - critical fault */
        s_redundancy_status.mismatch_count++;
        s_redundancy_status.last_result = GF_REDUNDANCY_MAJORITY_FAULT;
        return GF_REDUNDANCY_MAJORITY_FAULT;
    }
    
    return GF_REDUNDANCY_CHANNEL_FAULT;
}

gf_redundancy_result_t gf_redundancy_check_float(const float *values,
                                                  uint8_t count,
                                                  float tolerance,
                                                  float *voted_value)
{
    if (!values || count < 2 || !voted_value) {
        return GF_REDUNDANCY_CHANNEL_FAULT;
    }
    
    /* Convert to fixed-point for comparison */
    uint32_t fixed_values[3];
    for (uint8_t i = 0; i < count && i < 3; i++) {
        fixed_values[i] = (uint32_t)(values[i] * 1000.0f);
    }
    
    uint32_t fixed_tolerance = s_redundancy_config.tolerance;
    s_redundancy_config.tolerance = (uint32_t)(tolerance * 1000.0f);
    
    uint32_t fixed_result;
    gf_redundancy_result_t result = gf_redundancy_check_u32(fixed_values, 
                                                            count, 
                                                            &fixed_result);
    
    s_redundancy_config.tolerance = fixed_tolerance;
    *voted_value = (float)fixed_result / 1000.0f;
    
    return result;
}

void gf_redundancy_mark_failed(uint8_t channel)
{
    s_redundancy_status.failed_channels |= (1 << channel);
    
    if (s_redundancy_config.auto_failover && 
        s_redundancy_status.active_channel == channel) {
        /* Switch to next available channel */
        for (uint8_t i = 0; i < s_redundancy_config.channel_count; i++) {
            if (!(s_redundancy_status.failed_channels & (1 << i))) {
                s_redundancy_status.active_channel = i;
                s_redundancy_status.failover_count++;
                break;
            }
        }
    }
}

void gf_redundancy_get_status(gf_redundancy_status_t *status)
{
    if (status) {
        memcpy(status, &s_redundancy_status, sizeof(gf_redundancy_status_t));
    }
}

/*===========================================================================*/
/* Fail-Safe State Machine Implementation                                     */
/*===========================================================================*/

static gf_safety_fsm_config_t s_fsm_config;
static gf_safety_state_t s_current_state;
static uint32_t s_state_entry_time;
static const char *s_safe_reason;

/* State transition table */
static const struct {
    gf_safety_state_t   from;
    gf_safety_event_t   event;
    gf_safety_state_t   to;
} s_transitions[] = {
    { GF_SAFETY_STATE_INIT,      GF_SAFETY_EVENT_INIT_OK,       GF_SAFETY_STATE_NORMAL },
    { GF_SAFETY_STATE_INIT,      GF_SAFETY_EVENT_INIT_FAIL,     GF_SAFETY_STATE_FAULT },
    { GF_SAFETY_STATE_NORMAL,    GF_SAFETY_EVENT_FAULT_MINOR,   GF_SAFETY_STATE_NORMAL },
    { GF_SAFETY_STATE_NORMAL,    GF_SAFETY_EVENT_FAULT_MAJOR,   GF_SAFETY_STATE_DEGRADED },
    { GF_SAFETY_STATE_NORMAL,    GF_SAFETY_EVENT_FAULT_CRITICAL,GF_SAFETY_STATE_SAFE },
    { GF_SAFETY_STATE_NORMAL,    GF_SAFETY_EVENT_EXTERNAL_TRIP, GF_SAFETY_STATE_EMERGENCY },
    { GF_SAFETY_STATE_DEGRADED,  GF_SAFETY_EVENT_RECOVERY,      GF_SAFETY_STATE_NORMAL },
    { GF_SAFETY_STATE_DEGRADED,  GF_SAFETY_EVENT_FAULT_MAJOR,   GF_SAFETY_STATE_SAFE },
    { GF_SAFETY_STATE_DEGRADED,  GF_SAFETY_EVENT_FAULT_CRITICAL,GF_SAFETY_STATE_SAFE },
    { GF_SAFETY_STATE_SAFE,      GF_SAFETY_EVENT_RECOVERY,      GF_SAFETY_STATE_DEGRADED },
    { GF_SAFETY_STATE_SAFE,      GF_SAFETY_EVENT_RESET,         GF_SAFETY_STATE_INIT },
    { GF_SAFETY_STATE_EMERGENCY, GF_SAFETY_EVENT_RESET,         GF_SAFETY_STATE_INIT },
    { GF_SAFETY_STATE_FAULT,     GF_SAFETY_EVENT_RESET,         GF_SAFETY_STATE_INIT },
};

int gf_safety_fsm_init(const gf_safety_fsm_config_t *config)
{
    if (!config) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    memcpy(&s_fsm_config, config, sizeof(gf_safety_fsm_config_t));
    s_current_state = config->initial_state;
    s_state_entry_time = 0; /* Would use HAL timer */
    s_safe_reason = NULL;
    
    return GF_OK;
}

gf_safety_state_t gf_safety_fsm_get_state(void)
{
    return s_current_state;
}

gf_safety_state_t gf_safety_fsm_event(gf_safety_event_t event)
{
    gf_safety_state_t old_state = s_current_state;
    
    /* Find matching transition */
    for (size_t i = 0; i < sizeof(s_transitions) / sizeof(s_transitions[0]); i++) {
        if (s_transitions[i].from == s_current_state &&
            s_transitions[i].event == event) {
            
            gf_safety_state_t new_state = s_transitions[i].to;
            
            /* Execute leave action */
            if (old_state == GF_SAFETY_STATE_SAFE && s_fsm_config.on_leave_safe) {
                s_fsm_config.on_leave_safe(old_state, new_state, s_fsm_config.action_ctx);
            }
            
            s_current_state = new_state;
            s_state_entry_time = 0; /* Would use HAL timer */
            
            /* Execute enter action */
            if (new_state == GF_SAFETY_STATE_SAFE && s_fsm_config.on_enter_safe) {
                s_fsm_config.on_enter_safe(old_state, new_state, s_fsm_config.action_ctx);
            }
            
            break;
        }
    }
    
    return s_current_state;
}

void gf_safety_fsm_force_safe(const char *reason)
{
    if (s_current_state != GF_SAFETY_STATE_SAFE) {
        gf_safety_state_t old_state = s_current_state;
        s_current_state = GF_SAFETY_STATE_SAFE;
        s_safe_reason = reason;
        
        if (s_fsm_config.on_enter_safe) {
            s_fsm_config.on_enter_safe(old_state, GF_SAFETY_STATE_SAFE, 
                                        s_fsm_config.action_ctx);
        }
    }
}

bool gf_safety_fsm_is_operational(void)
{
    return (s_current_state == GF_SAFETY_STATE_NORMAL ||
            s_current_state == GF_SAFETY_STATE_DEGRADED);
}

void gf_safety_fsm_tick(void)
{
    /* Periodic safety checks - stub */
    (void)s_state_entry_time;
}

/*===========================================================================*/
/* Watchdog Safety Escalation Implementation                                  */
/*===========================================================================*/

static gf_safety_wdt_config_t s_wdt_config;
static gf_safety_wdt_status_t s_wdt_status;

int gf_safety_wdt_init(const gf_safety_wdt_config_t *config)
{
    if (!config) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    memcpy(&s_wdt_config, config, sizeof(gf_safety_wdt_config_t));
    memset(&s_wdt_status, 0, sizeof(gf_safety_wdt_status_t));
    
    return GF_OK;
}

void gf_safety_wdt_kick(void)
{
    s_wdt_status.last_kick = 0; /* Would use HAL timer */
    
    /* Reset escalation on successful kick */
    if (s_wdt_status.current_level > GF_SAFETY_WDT_LEVEL_NORMAL) {
        s_wdt_status.current_level = GF_SAFETY_WDT_LEVEL_NORMAL;
        s_wdt_status.escalation_active = false;
    }
}

gf_safety_wdt_level_t gf_safety_wdt_check(void)
{
    /* Stub: would check against HAL timer */
    return s_wdt_status.current_level;
}

void gf_safety_wdt_reset_escalation(void)
{
    s_wdt_status.current_level = GF_SAFETY_WDT_LEVEL_NORMAL;
    s_wdt_status.escalation_active = false;
}

void gf_safety_wdt_get_status(gf_safety_wdt_status_t *status)
{
    if (status) {
        memcpy(status, &s_wdt_status, sizeof(gf_safety_wdt_status_t));
    }
}
