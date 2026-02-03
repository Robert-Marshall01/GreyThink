/**
 * @file wearables.c
 * @brief Wearables Domain Implementation Stubs
 * 
 * This module provides stub implementations for wearable device functionality
 * including heart rate monitoring, step counting, and haptic feedback.
 * Production implementations would integrate with actual sensor hardware
 * (PPG sensors, accelerometers, haptic drivers).
 */

#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

#include "wearables/heart_rate.h"
#include "wearables/step_counter.h"
#include "wearables/haptic.h"

/*===========================================================================*/
/* Error Codes                                                                */
/*===========================================================================*/

#define GF_OK                   0
#define GF_ERR_INVALID_PARAM   -1
#define GF_ERR_NO_RESOURCE     -2
#define GF_ERR_NOT_READY       -3

/*===========================================================================*/
/* Heart Rate Sensor State                                                    */
/*===========================================================================*/

static struct {
    bool                initialized;
    gf_hr_state_t       state;
    gf_hr_config_t      config;
    gf_hr_result_t      current_result;
    gf_hr_result_t      history[GF_HR_HISTORY_SIZE];
    int                 history_count;
    gf_hr_callback_t    callback;
    void               *callback_ctx;
    bool                has_contact;
    uint32_t            sample_count;
} g_hr;

/*===========================================================================*/
/* Heart Rate API Implementation                                              */
/*===========================================================================*/

int gf_hr_init(const gf_hr_config_t *config)
{
    memset(&g_hr, 0, sizeof(g_hr));
    
    if (config) {
        memcpy(&g_hr.config, config, sizeof(gf_hr_config_t));
    } else {
        /* Defaults */
        g_hr.config.sample_rate_hz = GF_HR_SAMPLE_RATE_HZ;
        g_hr.config.led_current_ma = 20;
        g_hr.config.adc_resolution = 16;
        g_hr.config.integration_time_us = 400;
        g_hr.config.enable_spo2 = true;
        g_hr.config.enable_hrv = true;
    }
    
    g_hr.state = GF_HR_STATE_IDLE;
    g_hr.initialized = true;
    return GF_OK;
}

int gf_hr_start(void)
{
    if (!g_hr.initialized) {
        return GF_ERR_NOT_READY;
    }
    g_hr.state = GF_HR_STATE_MEASURING;
    return GF_OK;
}

int gf_hr_stop(void)
{
    g_hr.state = GF_HR_STATE_IDLE;
    return GF_OK;
}

gf_hr_state_t gf_hr_get_state(void)
{
    return g_hr.state;
}

const gf_hr_result_t *gf_hr_get_result(void)
{
    if (g_hr.current_result.bpm == 0) {
        return NULL;
    }
    return &g_hr.current_result;
}

int gf_hr_get_history(gf_hr_result_t *history)
{
    if (!history) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(history, g_hr.history, g_hr.history_count * sizeof(gf_hr_result_t));
    return g_hr.history_count;
}

int gf_hr_get_hrv(gf_hr_hrv_t *hrv)
{
    if (!hrv) {
        return GF_ERR_INVALID_PARAM;
    }
    /* Stub: would calculate HRV from R-R intervals */
    hrv->sdnn_ms = 50;
    hrv->rmssd_ms = 40;
    hrv->lf_power = 0.4f;
    hrv->hf_power = 0.3f;
    hrv->lf_hf_ratio = 1.33f;
    return GF_OK;
}

int gf_hr_register_callback(gf_hr_callback_t callback, void *ctx)
{
    g_hr.callback = callback;
    g_hr.callback_ctx = ctx;
    return GF_OK;
}

int gf_hr_measure_once(uint32_t timeout_ms)
{
    (void)timeout_ms;
    
    /* Stub: simulate a measurement */
    g_hr.current_result.bpm = 72;
    g_hr.current_result.spo2_percent = 98;
    g_hr.current_result.quality = GF_HR_QUALITY_GOOD;
    g_hr.current_result.confidence = 0.95f;
    g_hr.current_result.rr_interval_ms = 833;
    
    if (g_hr.callback) {
        g_hr.callback(&g_hr.current_result, g_hr.callback_ctx);
    }
    
    return GF_OK;
}

int gf_hr_set_led_current(gf_hr_led_t led, uint8_t current_ma)
{
    (void)led;
    g_hr.config.led_current_ma = current_ma;
    return GF_OK;
}

int gf_hr_calibrate(void)
{
    gf_hr_state_t prev_state = g_hr.state;
    g_hr.state = GF_HR_STATE_CALIBRATING;
    /* Stub: calibration would run here */
    g_hr.state = prev_state;
    return GF_OK;
}

int gf_hr_process(void)
{
    if (g_hr.state != GF_HR_STATE_MEASURING) {
        return 0;
    }
    
    /* Stub: would read sensor and process PPG */
    g_hr.sample_count++;
    
    /* Simulate periodic result updates */
    if (g_hr.sample_count % (GF_HR_SAMPLE_RATE_HZ * 2) == 0) {
        /* Every 2 seconds, update result */
        g_hr.current_result.bpm = 70 + (g_hr.sample_count % 10);
        g_hr.current_result.quality = GF_HR_QUALITY_GOOD;
        
        if (g_hr.callback) {
            g_hr.callback(&g_hr.current_result, g_hr.callback_ctx);
        }
        return 1;
    }
    
    return 0;
}

int gf_hr_get_raw_samples(gf_hr_sample_t *samples, int max_samples)
{
    (void)samples;
    (void)max_samples;
    return 0;
}

bool gf_hr_has_contact(void)
{
    return g_hr.has_contact;
}

uint32_t gf_hr_get_power_ua(void)
{
    if (g_hr.state == GF_HR_STATE_MEASURING) {
        return 600; /* ~600 µA while measuring */
    }
    return 10; /* ~10 µA idle */
}

/*===========================================================================*/
/* Step Counter State                                                         */
/*===========================================================================*/

static struct {
    bool                initialized;
    bool                running;
    gf_step_config_t    config;
    gf_step_state_t     state;
    gf_step_history_t   history;
    gf_step_goals_t     goals;
    gf_step_callback_t  step_cb;
    void               *step_ctx;
    gf_activity_callback_t activity_cb;
    void               *activity_ctx;
    gf_goal_callback_t  goal_cb;
    void               *goal_ctx;
    uint32_t            sample_count;
} g_step;

/*===========================================================================*/
/* Step Counter API Implementation                                            */
/*===========================================================================*/

int gf_step_init(const gf_step_config_t *config)
{
    memset(&g_step, 0, sizeof(g_step));
    
    if (config) {
        memcpy(&g_step.config, config, sizeof(gf_step_config_t));
    } else {
        /* Defaults */
        g_step.config.sample_rate_hz = GF_STEP_SAMPLE_RATE_HZ;
        g_step.config.step_threshold = 1000; /* 1g threshold */
        g_step.config.min_step_interval_ms = 250;
        g_step.config.max_step_interval_ms = 2000;
        g_step.config.stride_length_cm = 70;
        g_step.config.user_height_cm = 170;
        g_step.config.user_weight_kg = 70;
        g_step.config.sensitivity = GF_STEP_SENS_NORMAL;
    }
    
    g_step.goals.daily_step_goal = 10000;
    g_step.goals.daily_distance_goal_m = 8000;
    g_step.goals.daily_calorie_goal = 500;
    g_step.goals.daily_active_minutes_goal = 30;
    
    g_step.initialized = true;
    return GF_OK;
}

int gf_step_start(void)
{
    if (!g_step.initialized) {
        return GF_ERR_NOT_READY;
    }
    g_step.running = true;
    return GF_OK;
}

int gf_step_stop(void)
{
    g_step.running = false;
    return GF_OK;
}

int gf_step_reset_session(void)
{
    g_step.state.steps_session = 0;
    return GF_OK;
}

int gf_step_reset_daily(void)
{
    g_step.state.steps_today = 0;
    g_step.state.distance_m = 0;
    g_step.state.calories = 0;
    g_step.state.active_minutes = 0;
    memset(&g_step.history, 0, sizeof(g_step.history));
    memset(&g_step.goals.goal_reached_steps, 0, 4);
    return GF_OK;
}

int gf_step_get_state(gf_step_state_t *state)
{
    if (!state) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(state, &g_step.state, sizeof(gf_step_state_t));
    return GF_OK;
}

uint32_t gf_step_get_today(void)
{
    return g_step.state.steps_today;
}

int gf_step_get_history(gf_step_history_t *history)
{
    if (!history) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(history, &g_step.history, sizeof(gf_step_history_t));
    return GF_OK;
}

int gf_step_set_goals(const gf_step_goals_t *goals)
{
    if (!goals) {
        return GF_ERR_INVALID_PARAM;
    }
    g_step.goals.daily_step_goal = goals->daily_step_goal;
    g_step.goals.daily_distance_goal_m = goals->daily_distance_goal_m;
    g_step.goals.daily_calorie_goal = goals->daily_calorie_goal;
    g_step.goals.daily_active_minutes_goal = goals->daily_active_minutes_goal;
    return GF_OK;
}

int gf_step_get_goals(gf_step_goals_t *goals)
{
    if (!goals) {
        return GF_ERR_INVALID_PARAM;
    }
    memcpy(goals, &g_step.goals, sizeof(gf_step_goals_t));
    return GF_OK;
}

int gf_step_set_user_profile(uint16_t height_cm, uint8_t weight_kg,
                             uint16_t stride_cm)
{
    g_step.config.user_height_cm = height_cm;
    g_step.config.user_weight_kg = weight_kg;
    if (stride_cm > 0) {
        g_step.config.stride_length_cm = stride_cm;
    } else {
        /* Auto-estimate: stride ~= 0.415 * height */
        g_step.config.stride_length_cm = (height_cm * 415) / 1000;
    }
    return GF_OK;
}

int gf_step_register_callback(gf_step_callback_t callback, void *ctx)
{
    g_step.step_cb = callback;
    g_step.step_ctx = ctx;
    return GF_OK;
}

int gf_step_register_activity_cb(gf_activity_callback_t callback, void *ctx)
{
    g_step.activity_cb = callback;
    g_step.activity_ctx = ctx;
    return GF_OK;
}

int gf_step_register_goal_cb(gf_goal_callback_t callback, void *ctx)
{
    g_step.goal_cb = callback;
    g_step.goal_ctx = ctx;
    return GF_OK;
}

int gf_step_feed_sample(const gf_accel_sample_t *sample)
{
    (void)sample;
    /* Stub: would process accelerometer data for step detection */
    return 0;
}

int gf_step_process(void)
{
    if (!g_step.running) {
        return 0;
    }
    
    /* Stub: simulate occasional step detection */
    g_step.sample_count++;
    
    if (g_step.sample_count % 50 == 0) {
        /* Simulate step detected */
        g_step.state.steps_today++;
        g_step.state.steps_session++;
        g_step.state.steps_lifetime++;
        g_step.state.distance_m += g_step.config.stride_length_cm / 100;
        g_step.state.calories += 1; /* ~0.04 cal per step simplified */
        
        /* Update cadence (simplified) */
        g_step.state.cadence = 100;
        
        /* Check goals */
        if (!g_step.goals.goal_reached_steps && 
            g_step.state.steps_today >= g_step.goals.daily_step_goal) {
            g_step.goals.goal_reached_steps = true;
            if (g_step.goal_cb) {
                g_step.goal_cb(&g_step.goals, g_step.goal_ctx);
            }
        }
        
        if (g_step.step_cb) {
            g_step.step_cb(&g_step.state, g_step.step_ctx);
        }
        
        return 1;
    }
    
    return 0;
}

gf_activity_t gf_step_get_activity(void)
{
    return g_step.state.activity;
}

uint16_t gf_step_get_speed(void)
{
    /* Return speed * 100 (m/s * 100) */
    if (g_step.state.cadence == 0) return 0;
    return (g_step.config.stride_length_cm * g_step.state.cadence) / 60;
}

uint16_t gf_step_get_floors(void)
{
    /* Stub: would need barometer for floor detection */
    return 0;
}

uint32_t gf_step_get_power_ua(void)
{
    if (g_step.running) {
        return 200; /* ~200 µA while counting */
    }
    return 5; /* ~5 µA idle */
}

/*===========================================================================*/
/* Haptic Driver State                                                        */
/*===========================================================================*/

static struct {
    bool                initialized;
    bool                enabled;
    gf_haptic_state_t   state;
    gf_haptic_config_t  config;
    float               amplitude_scale;
    gf_haptic_effect_def_t effects[GF_HAPTIC_MAX_EFFECTS];
    int                 effect_count;
    gf_haptic_callback_t callback;
    void               *callback_ctx;
    uint8_t             current_effect;
    uint32_t            play_start_time;
    uint16_t            play_duration;
} g_haptic;

/*===========================================================================*/
/* Haptic API Implementation                                                  */
/*===========================================================================*/

int gf_haptic_init(const gf_haptic_config_t *config)
{
    memset(&g_haptic, 0, sizeof(g_haptic));
    
    if (config) {
        memcpy(&g_haptic.config, config, sizeof(gf_haptic_config_t));
    } else {
        /* Defaults for LRA */
        g_haptic.config.actuator = GF_HAPTIC_LRA;
        g_haptic.config.resonant_freq_hz = 170;
        g_haptic.config.overdrive_percent = 20;
        g_haptic.config.brake_percent = 20;
        g_haptic.config.max_amplitude = 255;
        g_haptic.config.auto_calibrate = true;
    }
    
    g_haptic.amplitude_scale = 1.0f;
    g_haptic.enabled = true;
    g_haptic.state = GF_HAPTIC_STATE_IDLE;
    g_haptic.initialized = true;
    
    return GF_OK;
}

gf_haptic_state_t gf_haptic_get_state(void)
{
    return g_haptic.state;
}

int gf_haptic_play(gf_haptic_effect_t effect)
{
    return gf_haptic_play_with_amplitude(effect, 255);
}

int gf_haptic_play_with_amplitude(gf_haptic_effect_t effect, uint8_t amplitude)
{
    if (!g_haptic.initialized || !g_haptic.enabled) {
        return GF_ERR_NOT_READY;
    }
    
    /* Scale amplitude */
    uint8_t scaled = (uint8_t)(amplitude * g_haptic.amplitude_scale);
    if (scaled > g_haptic.config.max_amplitude) {
        scaled = g_haptic.config.max_amplitude;
    }
    
    g_haptic.current_effect = effect;
    g_haptic.state = GF_HAPTIC_STATE_PLAYING;
    
    /* Set duration based on effect */
    switch (effect) {
        case GF_HAPTIC_EFFECT_CLICK:
            g_haptic.play_duration = 10;
            break;
        case GF_HAPTIC_EFFECT_DOUBLE_CLICK:
            g_haptic.play_duration = 50;
            break;
        case GF_HAPTIC_EFFECT_TICK:
            g_haptic.play_duration = 5;
            break;
        case GF_HAPTIC_EFFECT_THUD:
            g_haptic.play_duration = 100;
            break;
        case GF_HAPTIC_EFFECT_NOTIFICATION:
            g_haptic.play_duration = 200;
            break;
        case GF_HAPTIC_EFFECT_ALARM:
            g_haptic.play_duration = 500;
            break;
        default:
            g_haptic.play_duration = 50;
            break;
    }
    
    (void)scaled;
    
    if (g_haptic.callback) {
        g_haptic.callback(effect, GF_HAPTIC_STATE_PLAYING, g_haptic.callback_ctx);
    }
    
    return GF_OK;
}

int gf_haptic_play_sequence(const gf_haptic_sequence_t *sequence)
{
    if (!sequence || sequence->count == 0) {
        return GF_ERR_INVALID_PARAM;
    }
    
    /* Play first effect */
    return gf_haptic_play(sequence->effects[0]);
}

int gf_haptic_stop(void)
{
    g_haptic.state = GF_HAPTIC_STATE_IDLE;
    if (g_haptic.callback) {
        g_haptic.callback(g_haptic.current_effect, GF_HAPTIC_STATE_IDLE,
                         g_haptic.callback_ctx);
    }
    return GF_OK;
}

int gf_haptic_create_effect(const gf_haptic_effect_def_t *def)
{
    if (!def || g_haptic.effect_count >= GF_HAPTIC_MAX_EFFECTS) {
        return GF_ERR_INVALID_PARAM;
    }
    
    memcpy(&g_haptic.effects[g_haptic.effect_count], def,
           sizeof(gf_haptic_effect_def_t));
    g_haptic.effects[g_haptic.effect_count].id = 
        GF_HAPTIC_EFFECT_CUSTOM_BASE + g_haptic.effect_count;
    
    return g_haptic.effects[g_haptic.effect_count++].id;
}

int gf_haptic_delete_effect(uint8_t id)
{
    if (id < GF_HAPTIC_EFFECT_CUSTOM_BASE) {
        return GF_ERR_INVALID_PARAM; /* Can't delete built-in */
    }
    
    int idx = id - GF_HAPTIC_EFFECT_CUSTOM_BASE;
    if (idx >= g_haptic.effect_count) {
        return GF_ERR_INVALID_PARAM;
    }
    
    memmove(&g_haptic.effects[idx], &g_haptic.effects[idx + 1],
           (g_haptic.effect_count - idx - 1) * sizeof(gf_haptic_effect_def_t));
    g_haptic.effect_count--;
    
    return GF_OK;
}

int gf_haptic_set_amplitude_scale(float scale)
{
    if (scale < 0.0f || scale > 1.0f) {
        return GF_ERR_INVALID_PARAM;
    }
    g_haptic.amplitude_scale = scale;
    return GF_OK;
}

int gf_haptic_enable(bool enable)
{
    g_haptic.enabled = enable;
    if (!enable) {
        gf_haptic_stop();
    }
    return GF_OK;
}

bool gf_haptic_is_enabled(void)
{
    return g_haptic.enabled;
}

int gf_haptic_calibrate(void)
{
    gf_haptic_state_t prev = g_haptic.state;
    g_haptic.state = GF_HAPTIC_STATE_CALIBRATING;
    
    /* Stub: would run auto-resonance detection */
    g_haptic.config.resonant_freq_hz = 170;
    
    g_haptic.state = prev;
    return g_haptic.config.resonant_freq_hz;
}

int gf_haptic_set_resonant_freq(uint16_t freq_hz)
{
    g_haptic.config.resonant_freq_hz = freq_hz;
    return GF_OK;
}

uint16_t gf_haptic_get_resonant_freq(void)
{
    return g_haptic.config.resonant_freq_hz;
}

int gf_haptic_register_callback(gf_haptic_callback_t callback, void *ctx)
{
    g_haptic.callback = callback;
    g_haptic.callback_ctx = ctx;
    return GF_OK;
}

int gf_haptic_process(void)
{
    if (g_haptic.state != GF_HAPTIC_STATE_PLAYING) {
        return 0;
    }
    
    /* Stub: would update PWM output */
    /* Effect would complete after duration */
    
    return 0;
}

int gf_haptic_vibrate(uint16_t duration_ms, uint8_t amplitude)
{
    if (!g_haptic.initialized || !g_haptic.enabled) {
        return GF_ERR_NOT_READY;
    }
    
    g_haptic.play_duration = duration_ms;
    g_haptic.state = GF_HAPTIC_STATE_PLAYING;
    (void)amplitude;
    
    return GF_OK;
}

int gf_haptic_pattern(uint16_t pattern, uint16_t period_ms, uint8_t repeat)
{
    (void)pattern;
    (void)period_ms;
    (void)repeat;
    /* Stub: would play bit pattern */
    return GF_OK;
}

uint16_t gf_haptic_get_power_ma(void)
{
    if (g_haptic.state == GF_HAPTIC_STATE_PLAYING) {
        return 100; /* ~100 mA while playing */
    }
    return 0;
}
