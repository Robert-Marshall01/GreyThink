/**
 * @file climate_prediction.h
 * @brief Edge Climate Prediction Algorithm Module
 * 
 * INDUSTRY RELEVANCE:
 * Local weather prediction at edge nodes enables autonomous agricultural
 * systems, smart grids, and disaster preparedness. This module implements
 * nowcasting and short-term prediction algorithms that run on embedded
 * hardware without cloud connectivity.
 * 
 * TECHNICAL SCOPE:
 * - Nowcasting (0-2 hour predictions)
 * - Short-term forecasting (2-24 hours)
 * - Precipitation probability estimation
 * - Severe weather detection
 * - Machine learning inference (TinyML)
 * - Ensemble model fusion
 * 
 * ALGORITHMS:
 * - Kalman filtering for sensor fusion
 * - LSTM networks for temporal prediction
 * - Random forest for classification
 * - Optical flow for radar extrapolation
 * 
 * APPLICATIONS:
 * - Precision agriculture (irrigation, harvest timing)
 * - Renewable energy (solar/wind forecasting)
 * - Aviation (drone operations, airport management)
 * - Emergency management (flood, fire weather)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CLIMATE_PREDICTION_H
#define GF_CLIMATE_PREDICTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define PREDICT_HORIZON_HOURS   24    /**< Maximum forecast horizon */
#define PREDICT_INTERVALS       48    /**< 30-minute intervals */
#define PREDICT_ENSEMBLE_SIZE   5     /**< Ensemble members */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Prediction confidence level */
typedef enum {
    PREDICT_CONF_HIGH,             /**< > 80% confidence */
    PREDICT_CONF_MEDIUM,           /**< 60-80% confidence */
    PREDICT_CONF_LOW,              /**< 40-60% confidence */
    PREDICT_CONF_UNCERTAIN         /**< < 40% confidence */
} predict_confidence_t;

/** Severe weather type */
typedef enum {
    SEVERE_NONE,
    SEVERE_THUNDERSTORM,
    SEVERE_HAIL,
    SEVERE_TORNADO,
    SEVERE_FLASH_FLOOD,
    SEVERE_HIGH_WIND,
    SEVERE_EXTREME_HEAT,
    SEVERE_EXTREME_COLD,
    SEVERE_ICE_STORM,
    SEVERE_BLIZZARD,
    SEVERE_WILDFIRE_WEATHER
} severe_weather_t;

/** Point forecast */
typedef struct {
    float temperature_c;
    float feels_like_c;
    float humidity_pct;
    float pressure_hpa;
    float wind_speed_m_s;
    float wind_gust_m_s;
    float wind_direction_deg;
    float precip_prob_pct;
    float precip_amount_mm;
    float cloud_cover_pct;
    float visibility_km;
    float uv_index;
    predict_confidence_t confidence;
} point_forecast_t;

/** Hourly forecast array */
typedef struct {
    point_forecast_t hours[PREDICT_HORIZON_HOURS];
    severe_weather_t severe_risk;
    float severe_probability;
    uint64_t generated_time;
    uint64_t valid_from;
    bool model_healthy;
} hourly_forecast_t;

/** Nowcast (immediate conditions) */
typedef struct {
    float temp_trend_c_hr;         /**< Temperature change rate */
    float pressure_trend_hpa_hr;   /**< Pressure change rate */
    float precip_onset_minutes;    /**< Minutes until precipitation */
    float precip_end_minutes;      /**< Minutes until precipitation ends */
    severe_weather_t imminent_severe;
    predict_confidence_t confidence;
} nowcast_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize prediction module */
int predict_init(void);

/** Load ML model weights */
int predict_load_model(const uint8_t *weights, size_t len);

/** Update with new observation */
int predict_update_observation(const void *atmo_reading);

/** Generate nowcast */
int predict_nowcast(nowcast_t *nowcast);

/** Generate hourly forecast */
int predict_hourly(hourly_forecast_t *forecast);

/** Get severe weather probability */
float predict_severe_probability(severe_weather_t type, uint32_t hours_ahead);

/** Check if conditions safe for operation */
bool predict_is_safe_for_ops(const char *operation_type);

/** Process prediction cycle */
int predict_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_CLIMATE_PREDICTION_H */
