/**
 * @file sensor_acquisition.c
 * @brief Medical Sensor Acquisition Stub Implementation
 */

#include "medical/sensor_acquisition.h"
#include "core/driver_registry.h"
#include "core/message_bus.h"
#include <string.h>
#include <math.h>

static struct {
    gf_sensor_channel_config_t  channels[GF_SENSOR_MAX_CHANNELS];
    gf_sensor_channel_stats_t   channel_stats[GF_SENSOR_MAX_CHANNELS];
    gf_sensor_sample_t          latest[GF_SENSOR_MAX_CHANNELS];
    uint8_t                     enabled_mask;
    gf_sensor_stats_t           stats;
    gf_sensor_sample_cb         sample_cb;
    void                       *sample_ctx;
    gf_sensor_alarm_cb          alarm_cb;
    void                       *alarm_ctx;
    bool                        running;
    bool                        initialized;
} s_sensor;

/* IIR filter state */
static float s_filter_state[GF_SENSOR_MAX_CHANNELS];

static float apply_scale(uint8_t ch, int32_t raw) {
    float normalized = (float)raw / (1 << s_sensor.channels[ch].resolution_bits);
    float range = s_sensor.channels[ch].scale_max - s_sensor.channels[ch].scale_min;
    return s_sensor.channels[ch].scale_min + normalized * range;
}

static float apply_filter(uint8_t ch, float value) {
    gf_sensor_filter_t type = s_sensor.channels[ch].filter_type;
    if (type == GF_SENSOR_FILTER_NONE) return value;
    
    /* Simple first-order IIR filter */
    float alpha = 0.1f; /* Would calculate from cutoff frequency */
    s_filter_state[ch] = alpha * value + (1.0f - alpha) * s_filter_state[ch];
    return s_filter_state[ch];
}

static void update_stats(uint8_t ch, float value) {
    gf_sensor_channel_stats_t *st = &s_sensor.channel_stats[ch];
    st->sample_count++;
    if (value < st->min) st->min = value;
    if (value > st->max) st->max = value;
    st->mean += (value - st->mean) / st->sample_count;
    st->rms = sqrtf(st->rms * st->rms + value * value) / sqrtf(2.0f);
}

static void check_alarm(uint8_t ch, float value) {
    gf_sensor_channel_config_t *cfg = &s_sensor.channels[ch];
    if (value < cfg->alarm_low || value > cfg->alarm_high) {
        s_sensor.channel_stats[ch].alarm_count++;
        if (s_sensor.alarm_cb) {
            s_sensor.alarm_cb(ch, value, value > cfg->alarm_high, s_sensor.alarm_ctx);
        }
    }
}

int gf_sensor_init(void) {
    memset(&s_sensor, 0, sizeof(s_sensor));
    s_sensor.initialized = true;
    return 0;
}

int gf_sensor_config_channel(uint8_t ch, const gf_sensor_channel_config_t *cfg) {
    if (ch >= GF_SENSOR_MAX_CHANNELS || !cfg) return -1;
    memcpy(&s_sensor.channels[ch], cfg, sizeof(gf_sensor_channel_config_t));
    return 0;
}

int gf_sensor_enable_channel(uint8_t ch) {
    if (ch >= GF_SENSOR_MAX_CHANNELS) return -1;
    s_sensor.enabled_mask |= (1 << ch);
    s_sensor.stats.active_channels++;
    return 0;
}

int gf_sensor_disable_channel(uint8_t ch) {
    if (ch >= GF_SENSOR_MAX_CHANNELS) return -1;
    s_sensor.enabled_mask &= ~(1 << ch);
    s_sensor.stats.active_channels--;
    return 0;
}

int gf_sensor_start(void) {
    /* Would configure DMA, start ADC conversion */
    s_sensor.running = true;
    return 0;
}

int gf_sensor_stop(void) {
    s_sensor.running = false;
    return 0;
}

int gf_sensor_read(uint8_t ch, gf_sensor_sample_t *sample) {
    if (ch >= GF_SENSOR_MAX_CHANNELS || !sample) return -1;
    memcpy(sample, &s_sensor.latest[ch], sizeof(gf_sensor_sample_t));
    return 0;
}

int gf_sensor_read_buffer(uint8_t ch, gf_sensor_sample_t *samples, uint16_t max) {
    (void)ch; (void)samples; (void)max;
    /* Would read from circular buffer */
    return 0;
}

int gf_sensor_available(uint8_t ch) {
    (void)ch;
    return 0;
}

void gf_sensor_set_callback(gf_sensor_sample_cb cb, void *ctx) {
    s_sensor.sample_cb = cb;
    s_sensor.sample_ctx = ctx;
}

void gf_sensor_set_alarm_callback(gf_sensor_alarm_cb cb, void *ctx) {
    s_sensor.alarm_cb = cb;
    s_sensor.alarm_ctx = ctx;
}

void gf_sensor_get_channel_stats(uint8_t ch, gf_sensor_channel_stats_t *stats) {
    if (ch < GF_SENSOR_MAX_CHANNELS && stats) {
        memcpy(stats, &s_sensor.channel_stats[ch], sizeof(gf_sensor_channel_stats_t));
    }
}

void gf_sensor_get_stats(gf_sensor_stats_t *stats) {
    if (stats) memcpy(stats, &s_sensor.stats, sizeof(gf_sensor_stats_t));
}

void gf_sensor_reset_stats(void) {
    memset(&s_sensor.stats, 0, sizeof(gf_sensor_stats_t));
    memset(s_sensor.channel_stats, 0, sizeof(s_sensor.channel_stats));
}

int gf_sensor_trigger(void) {
    /* Single acquisition trigger */
    return 0;
}

/* ADC ISR would call this */
void gf_sensor_isr_handler(uint8_t ch, int32_t raw_value) {
    if (ch >= GF_SENSOR_MAX_CHANNELS) return;
    
    gf_sensor_sample_t sample;
    sample.raw = raw_value;
    sample.value = apply_scale(ch, raw_value);
    sample.value = apply_filter(ch, sample.value);
    sample.timestamp = 0; /* Would get from timer */
    sample.status = GF_SENSOR_STATUS_OK;
    
    /* Check for saturation */
    int32_t max_raw = (1 << s_sensor.channels[ch].resolution_bits) - 1;
    if (raw_value <= 0 || raw_value >= max_raw) {
        sample.status = GF_SENSOR_STATUS_SATURATED;
    }
    
    s_sensor.latest[ch] = sample;
    s_sensor.stats.total_samples++;
    
    update_stats(ch, sample.value);
    check_alarm(ch, sample.value);
    
    if (s_sensor.sample_cb) {
        s_sensor.sample_cb(ch, &sample, s_sensor.sample_ctx);
    }
}

static int sensor_drv_init(void *cfg) { (void)cfg; return gf_sensor_init(); }
static gf_driver_t s_sensor_driver = {
    .name = "sensor", .version = 0x0100,
    .ops = { .init = sensor_drv_init }
};
const void* gf_sensor_get_driver(void) { return &s_sensor_driver; }
