/**
 * @file heart_rate.h
 * @brief Heart Rate Sensor Driver Stub
 * 
 * INDUSTRY RELEVANCE:
 * Heart rate monitoring is fundamental to wearables, fitness devices, and
 * medical wearables (Apple Watch, Fitbit, Garmin). PPG (photoplethysmography)
 * sensors measure blood volume changes through light absorption. This market
 * exceeds $30B annually with growing demand for continuous health monitoring.
 * 
 * WHY THIS MATTERS:
 * - PPG signal acquisition and filtering
 * - Heart rate variability (HRV) for stress/fitness tracking
 * - SpO2 (blood oxygen) measurement
 * - Arrhythmia detection preparation for medical-grade devices
 * - Low-power operation for battery life
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Analog sensor acquisition patterns
 * - Digital signal processing for biomedical signals
 * - Power-aware sampling strategies
 * - Data quality monitoring
 */

#ifndef GF_HEART_RATE_H
#define GF_HEART_RATE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_HR_SAMPLE_RATE_HZ        25      /* PPG sample rate */
#define GF_HR_BUFFER_SIZE           256     /* Samples for HR calculation */
#define GF_HR_HISTORY_SIZE          32      /* HR history for averaging */
#define GF_HR_LED_CHANNELS          2       /* Red + IR for SpO2 */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Sensor state */
typedef enum {
    GF_HR_STATE_OFF = 0,            /* Sensor powered off */
    GF_HR_STATE_IDLE,               /* Sensor ready */
    GF_HR_STATE_MEASURING,          /* Active measurement */
    GF_HR_STATE_CALIBRATING,        /* Calibration in progress */
    GF_HR_STATE_ERROR               /* Sensor error */
} gf_hr_state_t;

/* Measurement quality */
typedef enum {
    GF_HR_QUALITY_NO_CONTACT = 0,   /* No finger/skin contact */
    GF_HR_QUALITY_POOR,             /* Too much motion/noise */
    GF_HR_QUALITY_FAIR,             /* Usable reading */
    GF_HR_QUALITY_GOOD,             /* Reliable reading */
    GF_HR_QUALITY_EXCELLENT         /* High confidence */
} gf_hr_quality_t;

/* LED channel */
typedef enum {
    GF_HR_LED_RED = 0,              /* Red LED (~660nm) */
    GF_HR_LED_IR                    /* Infrared LED (~940nm) */
} gf_hr_led_t;

/* Configuration */
typedef struct {
    uint16_t    sample_rate_hz;     /* Sampling frequency */
    uint8_t     led_current_ma;     /* LED drive current (higher = brighter) */
    uint8_t     adc_resolution;     /* ADC bits (12-18 typical) */
    uint16_t    integration_time_us;/* Light integration time */
    bool        enable_spo2;        /* Enable SpO2 measurement */
    bool        enable_hrv;         /* Enable HRV calculation */
} gf_hr_config_t;

/* Raw PPG sample */
typedef struct {
    uint32_t    red;                /* Red LED reading */
    uint32_t    ir;                 /* IR LED reading */
    uint32_t    ambient;            /* Ambient light */
    uint32_t    timestamp_ms;
} gf_hr_sample_t;

/* Processed heart rate result */
typedef struct {
    uint8_t     bpm;                /* Heart rate in BPM */
    uint8_t     spo2_percent;       /* Blood oxygen (0-100%) */
    gf_hr_quality_t quality;        /* Signal quality */
    float       confidence;         /* Confidence score (0.0-1.0) */
    uint16_t    rr_interval_ms;     /* R-R interval for HRV */
    uint32_t    timestamp_ms;
} gf_hr_result_t;

/* HRV metrics */
typedef struct {
    uint16_t    sdnn_ms;            /* Standard deviation of NN intervals */
    uint16_t    rmssd_ms;           /* Root mean square of successive diffs */
    float       lf_power;           /* Low frequency power (0.04-0.15 Hz) */
    float       hf_power;           /* High frequency power (0.15-0.4 Hz) */
    float       lf_hf_ratio;        /* LF/HF ratio (stress indicator) */
} gf_hr_hrv_t;

/* Callback for new heart rate reading */
typedef void (*gf_hr_callback_t)(const gf_hr_result_t *result, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize heart rate sensor
 * @param config Sensor configuration (NULL for defaults)
 */
int gf_hr_init(const gf_hr_config_t *config);

/**
 * @brief Start continuous heart rate measurement
 */
int gf_hr_start(void);

/**
 * @brief Stop measurement and enter low-power mode
 */
int gf_hr_stop(void);

/**
 * @brief Get current sensor state
 */
gf_hr_state_t gf_hr_get_state(void);

/**
 * @brief Get latest heart rate result
 * @return Latest result or NULL if none available
 */
const gf_hr_result_t *gf_hr_get_result(void);

/**
 * @brief Get heart rate history for averaging
 * @param history Output array (must hold GF_HR_HISTORY_SIZE entries)
 * @return Number of valid entries
 */
int gf_hr_get_history(gf_hr_result_t *history);

/**
 * @brief Get HRV metrics (requires sufficient R-R data)
 * @param hrv Output HRV metrics
 */
int gf_hr_get_hrv(gf_hr_hrv_t *hrv);

/**
 * @brief Register result callback
 * @param callback Function called on new HR reading
 * @param ctx User context
 */
int gf_hr_register_callback(gf_hr_callback_t callback, void *ctx);

/**
 * @brief Trigger single-shot measurement
 * @param timeout_ms Maximum time to wait for valid reading
 */
int gf_hr_measure_once(uint32_t timeout_ms);

/**
 * @brief Set LED current (brightness)
 * Higher current = better signal but higher power consumption
 */
int gf_hr_set_led_current(gf_hr_led_t led, uint8_t current_ma);

/**
 * @brief Run sensor calibration
 */
int gf_hr_calibrate(void);

/**
 * @brief Process sensor (call from main loop or timer ISR)
 * @return Number of new readings available
 */
int gf_hr_process(void);

/**
 * @brief Get raw PPG samples (for debugging/analysis)
 * @param samples Output buffer
 * @param max_samples Buffer size
 * @return Number of samples copied
 */
int gf_hr_get_raw_samples(gf_hr_sample_t *samples, int max_samples);

/**
 * @brief Check if skin contact is detected
 */
bool gf_hr_has_contact(void);

/**
 * @brief Get power consumption estimate
 * @return Current draw in microamps
 */
uint32_t gf_hr_get_power_ua(void);

#endif /* GF_HEART_RATE_H */
