/**
 * @file glucose_monitor.h
 * @brief Continuous Glucose Monitoring (CGM) Driver Stub
 * 
 * INDUSTRY RELEVANCE:
 * CGM devices are critical in diabetes management, with the global market
 * exceeding $8B annually. Firmware engineers working on CGM systems must
 * handle real-time sensor readings, data filtering for accuracy, Bluetooth
 * Low Energy (BLE) communication, and strict FDA/CE regulatory compliance.
 * This stub demonstrates understanding of medical device firmware architecture.
 * 
 * Key challenges:
 * - Ultra-low power operation (14-day sensor life)
 * - Drift compensation and calibration algorithms
 * - Secure data transmission (HIPAA compliance)
 * - Factory calibration storage and retrieval
 */

#ifndef GF_GLUCOSE_MONITOR_H
#define GF_GLUCOSE_MONITOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Glucose reading status codes */
typedef enum {
    GF_GLUCOSE_OK = 0,
    GF_GLUCOSE_SENSOR_WARMUP,      /* Initial warmup period (1-2 hours) */
    GF_GLUCOSE_CALIBRATING,         /* Factory/user calibration in progress */
    GF_GLUCOSE_SENSOR_ERROR,        /* Sensor malfunction detected */
    GF_GLUCOSE_OUT_OF_RANGE,        /* Reading outside valid range */
    GF_GLUCOSE_SENSOR_EXPIRED,      /* Sensor past useful life */
    GF_GLUCOSE_COMM_ERROR           /* Communication failure */
} gf_glucose_status_t;

/* Glucose trend indicators (FDA-standard arrows) */
typedef enum {
    GF_TREND_FALLING_FAST,          /* > 3 mg/dL per minute */
    GF_TREND_FALLING,               /* 2-3 mg/dL per minute */
    GF_TREND_FALLING_SLOW,          /* 1-2 mg/dL per minute */
    GF_TREND_STABLE,                /* < 1 mg/dL per minute */
    GF_TREND_RISING_SLOW,           /* 1-2 mg/dL per minute */
    GF_TREND_RISING,                /* 2-3 mg/dL per minute */
    GF_TREND_RISING_FAST,           /* > 3 mg/dL per minute */
    GF_TREND_UNKNOWN                /* Insufficient data */
} gf_glucose_trend_t;

/* CGM sensor configuration */
typedef struct {
    uint16_t sample_interval_sec;   /* Reading interval (typically 60-300s) */
    uint16_t warmup_minutes;        /* Sensor warmup time */
    uint16_t sensor_life_days;      /* Expected sensor lifetime */
    float calibration_factor;       /* Factory calibration coefficient */
    float temp_compensation;        /* Temperature compensation factor */
    bool enable_raw_data;           /* Export raw sensor values */
} gf_glucose_config_t;

/* Glucose reading with metadata */
typedef struct {
    float glucose_mg_dl;            /* Glucose level in mg/dL */
    float glucose_mmol_l;           /* Glucose level in mmol/L */
    gf_glucose_trend_t trend;       /* Rate of change indicator */
    gf_glucose_status_t status;     /* Reading validity status */
    uint32_t timestamp;             /* Unix timestamp of reading */
    int8_t temperature_c;           /* Sensor temperature */
    uint16_t raw_value;             /* Raw ADC value (for diagnostics) */
    uint8_t signal_quality;         /* Signal quality 0-100% */
} gf_glucose_reading_t;

/* Alert thresholds for hypo/hyperglycemia */
typedef struct {
    float urgent_low_mg_dl;         /* Urgent low alert (default: 55) */
    float low_mg_dl;                /* Low glucose alert (default: 70) */
    float high_mg_dl;               /* High glucose alert (default: 180) */
    float urgent_high_mg_dl;        /* Urgent high alert (default: 250) */
    bool enable_predictive;         /* Predictive alerts enabled */
    uint8_t predict_minutes;        /* Prediction horizon */
} gf_glucose_alerts_t;

/**
 * @brief Initialize glucose monitoring subsystem
 * @param config Sensor configuration parameters
 * @return Status code
 */
gf_glucose_status_t gf_glucose_init(const gf_glucose_config_t* config);

/**
 * @brief Start sensor warmup sequence
 * @return Status code
 */
gf_glucose_status_t gf_glucose_start_warmup(void);

/**
 * @brief Get current glucose reading
 * @param reading Output structure for glucose data
 * @return Status code
 */
gf_glucose_status_t gf_glucose_read(gf_glucose_reading_t* reading);

/**
 * @brief Perform user calibration with blood glucose meter reading
 * @param reference_mg_dl Reference blood glucose value
 * @return Status code
 */
gf_glucose_status_t gf_glucose_calibrate(float reference_mg_dl);

/**
 * @brief Configure alert thresholds
 * @param alerts Alert configuration
 * @return Status code
 */
gf_glucose_status_t gf_glucose_set_alerts(const gf_glucose_alerts_t* alerts);

/**
 * @brief Get remaining sensor life
 * @param hours_remaining Output for hours until expiration
 * @return Status code
 */
gf_glucose_status_t gf_glucose_get_sensor_life(uint16_t* hours_remaining);

/**
 * @brief Enter low-power mode between readings
 * @return Status code
 */
gf_glucose_status_t gf_glucose_sleep(void);

/**
 * @brief Shutdown and clean up resources
 */
void gf_glucose_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_GLUCOSE_MONITOR_H */
