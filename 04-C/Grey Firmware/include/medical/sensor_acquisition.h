/**
 * @file sensor_acquisition.h
 * @brief Medical Sensor Acquisition Stub
 * 
 * WHAT: High-precision analog sensor acquisition for medical devices with
 *       oversampling, digital filtering, and anomaly detection.
 * 
 * WHY: Medical devices require exceptional accuracy and reliability.
 *      Sensor acquisition for vital signs (ECG, SpO2, blood pressure)
 *      demands understanding of signal conditioning, ADC operation,
 *      and real-time processing. Errors can be life-threatening.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Multi-channel ADC management
 *      - Oversampling for improved resolution
 *      - Moving average and IIR filtering
 *      - Sample rate accuracy verification
 *      - Out-of-range and fault detection
 * 
 * Industry applications: patient monitors, diagnostic equipment, wearables
 * 
 * NOTE: Annotated stub. Production requires regulatory compliance (FDA/CE).
 */

#ifndef GF_SENSOR_ACQUISITION_H
#define GF_SENSOR_ACQUISITION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_SENSOR_MAX_CHANNELS      8
#define GF_SENSOR_BUFFER_SIZE       256     /* Sample buffer per channel */
#define GF_SENSOR_MAX_SAMPLE_RATE   10000   /* 10 kHz max */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_SENSOR_TYPE_VOLTAGE = 0,
    GF_SENSOR_TYPE_CURRENT,
    GF_SENSOR_TYPE_RESISTANCE,
    GF_SENSOR_TYPE_TEMPERATURE,
    GF_SENSOR_TYPE_PRESSURE,
    GF_SENSOR_TYPE_OPTICAL
} gf_sensor_type_t;

typedef enum {
    GF_SENSOR_FILTER_NONE = 0,
    GF_SENSOR_FILTER_MOVING_AVG,
    GF_SENSOR_FILTER_IIR_LOWPASS,
    GF_SENSOR_FILTER_IIR_HIGHPASS,
    GF_SENSOR_FILTER_IIR_BANDPASS
} gf_sensor_filter_t;

typedef enum {
    GF_SENSOR_STATUS_OK = 0,
    GF_SENSOR_STATUS_FAULT,
    GF_SENSOR_STATUS_OUT_OF_RANGE,
    GF_SENSOR_STATUS_LEAD_OFF,      /* Electrode disconnected */
    GF_SENSOR_STATUS_NOISE,         /* Signal too noisy */
    GF_SENSOR_STATUS_SATURATED      /* ADC saturated */
} gf_sensor_status_t;

/* Channel configuration */
typedef struct {
    gf_sensor_type_t    type;
    uint16_t            sample_rate_hz;
    uint8_t             oversample;     /* 1, 2, 4, 8, 16 */
    uint8_t             resolution_bits; /* ADC resolution */
    float               scale_min;      /* Minimum value in units */
    float               scale_max;      /* Maximum value in units */
    gf_sensor_filter_t  filter_type;
    float               filter_cutoff_hz;
    float               alarm_low;      /* Low alarm threshold */
    float               alarm_high;     /* High alarm threshold */
} gf_sensor_channel_config_t;

/* Sample data */
typedef struct {
    int32_t             raw;            /* Raw ADC value */
    float               value;          /* Scaled value in units */
    uint32_t            timestamp;      /* Sample timestamp (us) */
    gf_sensor_status_t  status;
} gf_sensor_sample_t;

/* Channel statistics */
typedef struct {
    float       min;
    float       max;
    float       mean;
    float       rms;
    uint32_t    sample_count;
    uint32_t    fault_count;
    uint32_t    alarm_count;
} gf_sensor_channel_stats_t;

/* Global sensor statistics */
typedef struct {
    uint32_t    total_samples;
    uint32_t    missed_samples;
    uint32_t    overruns;
    uint8_t     active_channels;
    float       actual_sample_rate;
} gf_sensor_stats_t;

/* Sample callback */
typedef void (*gf_sensor_sample_cb)(uint8_t channel, const gf_sensor_sample_t *sample, void *ctx);

/* Alarm callback */
typedef void (*gf_sensor_alarm_cb)(uint8_t channel, float value, bool high, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize sensor acquisition subsystem
 */
int gf_sensor_init(void);

/**
 * @brief Configure a sensor channel
 */
int gf_sensor_config_channel(uint8_t channel, const gf_sensor_channel_config_t *config);

/**
 * @brief Enable a channel for acquisition
 */
int gf_sensor_enable_channel(uint8_t channel);

/**
 * @brief Disable a channel
 */
int gf_sensor_disable_channel(uint8_t channel);

/**
 * @brief Start continuous acquisition on all enabled channels
 */
int gf_sensor_start(void);

/**
 * @brief Stop acquisition
 */
int gf_sensor_stop(void);

/**
 * @brief Read latest sample from a channel
 */
int gf_sensor_read(uint8_t channel, gf_sensor_sample_t *sample);

/**
 * @brief Read multiple samples from buffer
 */
int gf_sensor_read_buffer(uint8_t channel, gf_sensor_sample_t *samples, 
                           uint16_t max_samples);

/**
 * @brief Get samples available in buffer
 */
int gf_sensor_available(uint8_t channel);

/**
 * @brief Set sample callback
 */
void gf_sensor_set_callback(gf_sensor_sample_cb callback, void *ctx);

/**
 * @brief Set alarm callback
 */
void gf_sensor_set_alarm_callback(gf_sensor_alarm_cb callback, void *ctx);

/**
 * @brief Get channel statistics
 */
void gf_sensor_get_channel_stats(uint8_t channel, gf_sensor_channel_stats_t *stats);

/**
 * @brief Get global statistics
 */
void gf_sensor_get_stats(gf_sensor_stats_t *stats);

/**
 * @brief Reset statistics
 */
void gf_sensor_reset_stats(void);

/**
 * @brief Trigger single acquisition (for one-shot mode)
 */
int gf_sensor_trigger(void);

/**
 * @brief Get sensor driver descriptor
 */
const void* gf_sensor_get_driver(void);

#endif /* GF_SENSOR_ACQUISITION_H */
