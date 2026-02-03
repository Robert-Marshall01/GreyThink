/**
 * @file water_quality.h
 * @brief Water Quality Telemetry Module Stub
 * 
 * INDUSTRY RELEVANCE:
 * Water quality monitoring is critical for municipal water systems,
 * aquaculture, environmental compliance, and industrial processes.
 * The smart water management market exceeds $15B. Firmware engineers
 * work with electrochemical sensors, turbidity meters, and ion-selective
 * electrodes while meeting EPA/WHO standards for drinking water safety.
 * 
 * Key challenges:
 * - Electrode fouling and drift compensation
 * - Multi-parameter sensor fusion
 * - Low-power solar-powered deployments
 * - Harsh underwater environments
 * - Regulatory compliance logging
 */

#ifndef GF_WATER_QUALITY_H
#define GF_WATER_QUALITY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Water quality status codes */
typedef enum {
    GF_WQ_OK = 0,
    GF_WQ_CALIBRATION_NEEDED,       /* Sensor needs calibration */
    GF_WQ_PROBE_FOULED,             /* Probe needs cleaning */
    GF_WQ_SENSOR_FAULT,             /* Sensor hardware failure */
    GF_WQ_OUT_OF_RANGE,             /* Reading outside range */
    GF_WQ_COMM_ERROR,               /* Communication failure */
    GF_WQ_POWER_LOW                 /* Low battery/power */
} gf_wq_status_t;

/* Water quality parameters */
typedef enum {
    GF_WQ_PH,                       /* pH (0-14) */
    GF_WQ_DISSOLVED_OXYGEN,         /* DO (mg/L) */
    GF_WQ_CONDUCTIVITY,             /* Conductivity (uS/cm) */
    GF_WQ_TURBIDITY,                /* Turbidity (NTU) */
    GF_WQ_TEMPERATURE,              /* Temperature (°C) */
    GF_WQ_ORP,                      /* Oxidation-Reduction Potential (mV) */
    GF_WQ_TDS,                      /* Total Dissolved Solids (ppm) */
    GF_WQ_SALINITY,                 /* Salinity (ppt) */
    GF_WQ_CHLORINE,                 /* Free chlorine (ppm) */
    GF_WQ_AMMONIA,                  /* Ammonia-nitrogen (mg/L) */
    GF_WQ_NITRATE,                  /* Nitrate (mg/L) */
    GF_WQ_PARAM_COUNT
} gf_wq_param_t;

/* Water quality classification */
typedef enum {
    GF_WQ_CLASS_EXCELLENT,          /* Meets all standards */
    GF_WQ_CLASS_GOOD,               /* Minor deviations */
    GF_WQ_CLASS_FAIR,               /* Some concerns */
    GF_WQ_CLASS_POOR,               /* Multiple violations */
    GF_WQ_CLASS_CRITICAL            /* Immediate action required */
} gf_wq_class_t;

/* Sensor configuration */
typedef struct {
    uint16_t sample_interval_sec;   /* Sampling interval */
    uint8_t enabled_params;         /* Bitmask of enabled parameters */
    bool enable_auto_clean;         /* Automatic wiper cleaning */
    uint16_t clean_interval_min;    /* Cleaning interval */
    bool enable_alerts;             /* Alert generation */
    float depth_m;                  /* Sensor deployment depth */
} gf_wq_config_t;

/* Parameter threshold for alerts */
typedef struct {
    gf_wq_param_t parameter;        /* Parameter type */
    float low_threshold;            /* Low alert threshold */
    float high_threshold;           /* High alert threshold */
    float critical_low;             /* Critical low threshold */
    float critical_high;            /* Critical high threshold */
} gf_wq_threshold_t;

/* Complete water quality reading */
typedef struct {
    float values[GF_WQ_PARAM_COUNT]; /* Parameter values */
    uint8_t quality_scores[GF_WQ_PARAM_COUNT]; /* Quality 0-100 per param */
    gf_wq_class_t overall_class;    /* Overall water classification */
    uint16_t wqi_score;             /* Water Quality Index (0-100) */
    gf_wq_status_t status;          /* Reading status */
    uint32_t timestamp;             /* Reading timestamp */
    float battery_volts;            /* Battery voltage */
} gf_wq_reading_t;

/* Calibration data for parameter */
typedef struct {
    gf_wq_param_t parameter;        /* Parameter type */
    float offset;                   /* Offset correction */
    float slope;                    /* Slope correction */
    float reference_value;          /* Reference standard value */
    uint64_t calibration_date;      /* Calibration timestamp */
} gf_wq_calibration_t;

/* Historical statistics */
typedef struct {
    gf_wq_param_t parameter;        /* Parameter type */
    float min_value;                /* Minimum recorded */
    float max_value;                /* Maximum recorded */
    float avg_value;                /* Average value */
    float std_deviation;            /* Standard deviation */
    uint32_t sample_count;          /* Number of samples */
    uint32_t alert_count;           /* Alert occurrences */
} gf_wq_stats_t;

/**
 * @brief Initialize water quality sensor
 * @param config Sensor configuration
 * @return Status code
 */
gf_wq_status_t gf_wq_init(const gf_wq_config_t* config);

/**
 * @brief Start water quality monitoring
 * @return Status code
 */
gf_wq_status_t gf_wq_start(void);

/**
 * @brief Stop monitoring
 * @return Status code
 */
gf_wq_status_t gf_wq_stop(void);

/**
 * @brief Get current water quality reading
 * @param reading Output for reading data
 * @return Status code
 */
gf_wq_status_t gf_wq_read(gf_wq_reading_t* reading);

/**
 * @brief Get specific parameter value
 * @param param Parameter type
 * @param value Output for value
 * @return Status code
 */
gf_wq_status_t gf_wq_get_param(gf_wq_param_t param, float* value);

/**
 * @brief Set alert thresholds
 * @param threshold Threshold configuration
 * @return Status code
 */
gf_wq_status_t gf_wq_set_threshold(const gf_wq_threshold_t* threshold);

/**
 * @brief Apply calibration for parameter
 * @param calibration Calibration data
 * @return Status code
 */
gf_wq_status_t gf_wq_calibrate(const gf_wq_calibration_t* calibration);

/**
 * @brief Trigger probe cleaning cycle
 * @return Status code
 */
gf_wq_status_t gf_wq_clean_probe(void);

/**
 * @brief Get historical statistics
 * @param param Parameter type
 * @param stats Output for statistics
 * @return Status code
 */
gf_wq_status_t gf_wq_get_stats(gf_wq_param_t param, gf_wq_stats_t* stats);

/**
 * @brief Calculate Water Quality Index
 * @param wqi Output for WQI score
 * @return Status code
 */
gf_wq_status_t gf_wq_calculate_wqi(uint16_t* wqi);

/**
 * @brief Perform sensor self-test
 * @return Status code (OK if passed)
 */
gf_wq_status_t gf_wq_self_test(void);

/**
 * @brief Shutdown and release resources
 */
void gf_wq_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_WATER_QUALITY_H */
