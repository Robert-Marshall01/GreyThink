/**
 * @file pressure_depth.h
 * @brief Pressure/Depth Monitoring Interface
 * 
 * INDUSTRY RELEVANCE:
 * Accurate depth measurement is critical for:
 * - Submarine and AUV operations (depth control, crush depth avoidance)
 * - Commercial diving systems (decompression management, safety limits)
 * - Oceanographic instruments (depth profiling, CTD sensors)
 * - Offshore ROV operations (precise positioning for subsea work)
 * - Naval systems (tactical depth management, stealth operations)
 * - Scientific research (bathymetric surveys, deep-sea exploration)
 * 
 * This module demonstrates expertise in:
 * - High-precision pressure sensing (mbar to bar range)
 * - Depth calculation with salinity/density compensation
 * - Temperature compensation for sensor accuracy
 * - Calibration routines for field deployment
 * - Safety limit monitoring and alerting
 * 
 * Pressure-to-depth conversion considers:
 * - Seawater density variations (salinity, temperature)
 * - Atmospheric pressure offset
 * - Sensor nonlinearity and hysteresis
 */

#ifndef GF_PRESSURE_DEPTH_H
#define GF_PRESSURE_DEPTH_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ===== Configuration ===== */

#define GF_DEPTH_MAX_RATING_M       1000    /* Maximum depth rating */
#define GF_DEPTH_SAMPLE_RATE_HZ     10      /* Default sample rate */
#define GF_DEPTH_FILTER_SAMPLES     8       /* Moving average window */
#define GF_DEPTH_CALIBRATION_POINTS 5       /* Multi-point calibration */

/* Standard values */
#define GF_PRESSURE_ATM_MBAR        1013.25f    /* Standard atmospheric pressure */
#define GF_SEAWATER_DENSITY         1025.0f     /* kg/m³ at 35 PSU, 15°C */
#define GF_FRESHWATER_DENSITY       1000.0f     /* kg/m³ at 4°C */

/* ===== Status Codes ===== */

typedef enum {
    GF_DEPTH_OK = 0,
    GF_DEPTH_ERROR_NOT_INITIALIZED,
    GF_DEPTH_ERROR_INVALID_PARAM,
    GF_DEPTH_ERROR_SENSOR_FAULT,
    GF_DEPTH_ERROR_OUT_OF_RANGE,
    GF_DEPTH_ERROR_CALIBRATION,
    GF_DEPTH_WARN_APPROACHING_LIMIT,
    GF_DEPTH_WARN_RATE_EXCEEDED
} gf_depth_status_t;

/* ===== Sensor Types ===== */

typedef enum {
    GF_DEPTH_SENSOR_STRAIN_GAUGE,   /* Strain gauge pressure sensor */
    GF_DEPTH_SENSOR_PIEZO,          /* Piezoresistive sensor */
    GF_DEPTH_SENSOR_CAPACITIVE,     /* Capacitive pressure sensor */
    GF_DEPTH_SENSOR_OPTICAL         /* Fiber optic pressure sensor */
} gf_depth_sensor_type_t;

typedef enum {
    GF_WATER_TYPE_SEAWATER,         /* ~1025 kg/m³ */
    GF_WATER_TYPE_FRESHWATER,       /* ~1000 kg/m³ */
    GF_WATER_TYPE_BRACKISH,         /* ~1010 kg/m³ */
    GF_WATER_TYPE_CUSTOM            /* User-defined density */
} gf_water_type_t;

/* ===== Data Structures ===== */

/**
 * @brief Depth reading with metadata
 */
typedef struct {
    float depth_m;              /* Calculated depth in meters */
    float pressure_mbar;        /* Raw pressure in millibars */
    float temperature_c;        /* Sensor temperature */
    float rate_mps;             /* Descent/ascent rate m/s (+ve = descending) */
    uint32_t timestamp_ms;      /* Measurement timestamp */
    uint8_t quality;            /* Measurement quality (0-100) */
} gf_depth_reading_t;

/**
 * @brief Depth limits configuration
 */
typedef struct {
    float max_depth_m;          /* Maximum operational depth */
    float warning_depth_m;      /* Warning threshold */
    float max_descent_rate_mps; /* Maximum descent rate */
    float max_ascent_rate_mps;  /* Maximum ascent rate */
} gf_depth_limits_t;

/**
 * @brief Calibration data
 */
typedef struct {
    float zero_offset_mbar;     /* Atmospheric offset */
    float scale_factor;         /* Pressure scale factor */
    float temp_coefficient;     /* Temperature compensation factor */
    float known_pressures[GF_DEPTH_CALIBRATION_POINTS];
    float measured_pressures[GF_DEPTH_CALIBRATION_POINTS];
    uint8_t calibration_points;
    uint32_t calibration_date;  /* Unix timestamp */
} gf_depth_calibration_t;

/**
 * @brief Depth sensor configuration
 */
typedef struct {
    gf_depth_sensor_type_t sensor_type;
    gf_water_type_t water_type;
    float custom_density;           /* For CUSTOM water type */
    float salinity_psu;             /* Practical Salinity Units */
    uint8_t sample_rate_hz;
    uint8_t filter_samples;
    gf_depth_limits_t limits;
    bool auto_zero;                 /* Auto-zero at surface */
} gf_depth_config_t;

/**
 * @brief Depth alert callback
 */
typedef void (*gf_depth_alert_callback_t)(gf_depth_status_t alert, 
                                           const gf_depth_reading_t* reading,
                                           void* user_data);

/* ===== API Functions ===== */

/**
 * @brief Initialize depth sensor
 */
gf_depth_status_t gf_depth_init(const gf_depth_config_t* config);

/**
 * @brief Shutdown depth sensor
 */
void gf_depth_shutdown(void);

/**
 * @brief Get current depth reading
 */
gf_depth_status_t gf_depth_read(gf_depth_reading_t* reading);

/**
 * @brief Get filtered depth (averaged)
 */
gf_depth_status_t gf_depth_read_filtered(gf_depth_reading_t* reading);

/**
 * @brief Zero sensor at surface (atmospheric calibration)
 */
gf_depth_status_t gf_depth_zero(void);

/**
 * @brief Full calibration with known pressures
 */
gf_depth_status_t gf_depth_calibrate(const gf_depth_calibration_t* cal);

/**
 * @brief Get current calibration data
 */
gf_depth_status_t gf_depth_get_calibration(gf_depth_calibration_t* cal);

/**
 * @brief Set depth limits and alerts
 */
gf_depth_status_t gf_depth_set_limits(const gf_depth_limits_t* limits);

/**
 * @brief Register alert callback
 */
gf_depth_status_t gf_depth_register_alert(gf_depth_alert_callback_t callback,
                                           void* user_data);

/**
 * @brief Calculate density from temperature and salinity
 */
float gf_depth_calculate_density(float temperature_c, float salinity_psu);

/**
 * @brief Convert pressure to depth
 */
float gf_depth_pressure_to_depth(float pressure_mbar, float density);

/**
 * @brief Get sensor statistics
 */
gf_depth_status_t gf_depth_get_stats(float* min_depth, float* max_depth,
                                      float* avg_depth, uint32_t* sample_count);

#ifdef __cplusplus
}
#endif

#endif /* GF_PRESSURE_DEPTH_H */
