/**
 * @file air_quality.h
 * @brief Air Quality Sensor Stub
 * 
 * INDUSTRY RELEVANCE:
 * Air quality monitoring is essential for smart cities, industrial safety,
 * and HVAC systems. The market exceeds $5B with growth driven by pollution
 * concerns and indoor air quality awareness post-COVID. Firmware engineers
 * work with electrochemical sensors, particulate matter detectors, and
 * calibration algorithms for gases like CO2, NO2, O3, and PM2.5.
 * 
 * Key challenges:
 * - Sensor cross-sensitivity compensation
 * - Temperature and humidity correction
 * - Baseline drift compensation
 * - Low-power operation for battery devices
 * - AQI calculation per EPA/EU standards
 */

#ifndef GF_AIR_QUALITY_H
#define GF_AIR_QUALITY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Air quality sensor status codes */
typedef enum {
    GF_AQ_OK = 0,
    GF_AQ_WARMING_UP,               /* Sensor warmup period */
    GF_AQ_CALIBRATING,              /* Calibration in progress */
    GF_AQ_SENSOR_FAULT,             /* Sensor hardware fault */
    GF_AQ_OUT_OF_RANGE,             /* Reading outside valid range */
    GF_AQ_BASELINE_DRIFT,           /* Significant baseline drift */
    GF_AQ_MAINTENANCE_NEEDED        /* Sensor needs replacement */
} gf_aq_status_t;

/* Gas types measured */
typedef enum {
    GF_GAS_CO2,                     /* Carbon dioxide (ppm) */
    GF_GAS_CO,                      /* Carbon monoxide (ppm) */
    GF_GAS_NO2,                     /* Nitrogen dioxide (ppb) */
    GF_GAS_O3,                      /* Ozone (ppb) */
    GF_GAS_SO2,                     /* Sulfur dioxide (ppb) */
    GF_GAS_TVOC,                    /* Total VOCs (ppb) */
    GF_GAS_CH4,                     /* Methane (ppm) */
    GF_GAS_NH3,                     /* Ammonia (ppm) */
    GF_GAS_COUNT
} gf_aq_gas_t;

/* EPA AQI categories */
typedef enum {
    GF_AQI_GOOD,                    /* 0-50: Good */
    GF_AQI_MODERATE,                /* 51-100: Moderate */
    GF_AQI_UNHEALTHY_SENSITIVE,     /* 101-150: Unhealthy for sensitive */
    GF_AQI_UNHEALTHY,               /* 151-200: Unhealthy */
    GF_AQI_VERY_UNHEALTHY,          /* 201-300: Very unhealthy */
    GF_AQI_HAZARDOUS                /* 301-500: Hazardous */
} gf_aqi_category_t;

/* Sensor configuration */
typedef struct {
    uint16_t sample_interval_sec;   /* Sampling interval */
    uint16_t warmup_seconds;        /* Sensor warmup time */
    bool enable_compensation;       /* Temperature/humidity correction */
    bool enable_baseline_update;    /* Automatic baseline adjustment */
    uint8_t enabled_gases;          /* Bitmask of enabled gas sensors */
    uint8_t altitude_m;             /* Altitude for pressure compensation */
} gf_aq_config_t;

/* Particulate matter reading */
typedef struct {
    uint16_t pm1_0_ug_m3;           /* PM1.0 concentration */
    uint16_t pm2_5_ug_m3;           /* PM2.5 concentration */
    uint16_t pm10_ug_m3;            /* PM10 concentration */
    uint32_t particle_count_0_3um;  /* Particles > 0.3um per 0.1L */
    uint32_t particle_count_2_5um;  /* Particles > 2.5um per 0.1L */
} gf_aq_particulate_t;

/* Complete air quality reading */
typedef struct {
    float gas_readings[GF_GAS_COUNT]; /* Gas concentrations */
    gf_aq_particulate_t particulates; /* Particulate matter */
    float temperature_c;            /* Ambient temperature */
    float humidity_percent;         /* Relative humidity */
    uint16_t aqi_value;             /* Calculated AQI */
    gf_aqi_category_t aqi_category; /* AQI category */
    gf_aq_status_t status;          /* Reading status */
    uint32_t timestamp;             /* Reading timestamp */
} gf_aq_reading_t;

/* Calibration data */
typedef struct {
    float zero_offset[GF_GAS_COUNT];  /* Zero calibration offsets */
    float span_factor[GF_GAS_COUNT];  /* Span calibration factors */
    uint64_t calibration_date;        /* Last calibration timestamp */
    uint16_t sensor_age_days;         /* Sensor age in days */
} gf_aq_calibration_t;

/**
 * @brief Initialize air quality sensor
 * @param config Sensor configuration
 * @return Status code
 */
gf_aq_status_t gf_aq_init(const gf_aq_config_t* config);

/**
 * @brief Start air quality monitoring
 * @return Status code
 */
gf_aq_status_t gf_aq_start(void);

/**
 * @brief Stop monitoring
 * @return Status code
 */
gf_aq_status_t gf_aq_stop(void);

/**
 * @brief Get current air quality reading
 * @param reading Output for reading data
 * @return Status code
 */
gf_aq_status_t gf_aq_read(gf_aq_reading_t* reading);

/**
 * @brief Get specific gas concentration
 * @param gas Gas type
 * @param concentration Output for concentration
 * @return Status code
 */
gf_aq_status_t gf_aq_get_gas(gf_aq_gas_t gas, float* concentration);

/**
 * @brief Calculate EPA AQI from current readings
 * @param aqi_value Output for AQI value
 * @param aqi_category Output for AQI category
 * @return Status code
 */
gf_aq_status_t gf_aq_calculate_aqi(uint16_t* aqi_value, gf_aqi_category_t* aqi_category);

/**
 * @brief Apply calibration data
 * @param calibration Calibration coefficients
 * @return Status code
 */
gf_aq_status_t gf_aq_set_calibration(const gf_aq_calibration_t* calibration);

/**
 * @brief Perform zero calibration (in clean air)
 * @return Status code
 */
gf_aq_status_t gf_aq_zero_calibrate(void);

/**
 * @brief Force baseline update
 * @return Status code
 */
gf_aq_status_t gf_aq_update_baseline(void);

/**
 * @brief Perform sensor self-test
 * @return Status code (OK if passed)
 */
gf_aq_status_t gf_aq_self_test(void);

/**
 * @brief Shutdown and release resources
 */
void gf_aq_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_AIR_QUALITY_H */
