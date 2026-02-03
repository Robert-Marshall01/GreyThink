/**
 * @file prediction_algorithm.h
 * @brief Disaster Prediction Algorithm Module
 * 
 * INDUSTRY RELEVANCE:
 * Machine learning and statistical algorithms power modern disaster prediction
 * systems. Earthquake early warning, tsunami detection, wildfire prediction,
 * and flood forecasting all require embedded AI/ML inference. Companies like
 * Google (ShakeAlert partner), One Concern, and Jupiter Intelligence need
 * firmware engineers who can deploy ML models on edge devices.
 * 
 * This module provides prediction algorithm interfaces for various natural
 * disasters including earthquakes, tsunamis, floods, and wildfires.
 * 
 * KEY CAPABILITIES:
 * - Earthquake magnitude estimation
 * - Epicenter triangulation
 * - Shaking intensity prediction (MMI)
 * - Tsunami propagation modeling
 * - Flood level forecasting
 * - Wildfire spread prediction
 * - Multi-sensor fusion
 * - Uncertainty quantification
 * 
 * ALGORITHMS:
 * - STA/LTA trigger detection
 * - τ-c magnitude estimation
 * - Machine learning classifiers
 * - Kalman filtering for state estimation
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_PREDICTION_ALGORITHM_H
#define GF_PREDICTION_ALGORITHM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define PA_MAX_SENSORS         64    /**< Max sensors in network */
#define PA_HISTORY_SAMPLES     1024  /**< Prediction history */
#define PA_MMI_LEVELS          12    /**< Modified Mercalli Intensity */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Disaster type */
typedef enum {
    PA_DISASTER_EARTHQUAKE,
    PA_DISASTER_TSUNAMI,
    PA_DISASTER_FLOOD,
    PA_DISASTER_WILDFIRE,
    PA_DISASTER_LANDSLIDE,
    PA_DISASTER_VOLCANO
} pa_disaster_t;

/** Alert level */
typedef enum {
    PA_ALERT_NONE,
    PA_ALERT_ADVISORY,
    PA_ALERT_WATCH,
    PA_ALERT_WARNING,
    PA_ALERT_EMERGENCY
} pa_alert_t;

/** Earthquake prediction */
typedef struct {
    float magnitude;           /**< Estimated magnitude */
    float magnitude_uncertainty;
    double epicenter_lat;
    double epicenter_lon;
    float depth_km;
    float pga_estimate_g;      /**< Predicted PGA at location */
    uint8_t mmi_estimate;      /**< Predicted MMI */
    float warning_time_s;      /**< Seconds until shaking */
    float confidence;          /**< 0.0-1.0 */
} pa_earthquake_t;

/** Flood prediction */
typedef struct {
    float water_level_m;
    float flow_rate_m3s;
    float crest_time_hours;
    float peak_level_m;
    pa_alert_t alert_level;
} pa_flood_t;

/** Wildfire prediction */
typedef struct {
    float spread_rate_kmh;
    float fire_intensity;
    float burn_probability;
    float wind_speed_kmh;
    float wind_direction_deg;
    uint32_t threat_area_ha;
} pa_wildfire_t;

/** Generic prediction result */
typedef struct {
    pa_disaster_t type;
    pa_alert_t alert;
    uint32_t timestamp;
    float confidence;
    union {
        pa_earthquake_t earthquake;
        pa_flood_t flood;
        pa_wildfire_t wildfire;
    } data;
} pa_prediction_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize prediction algorithms
 * @param type Disaster type to monitor
 * @return 0 on success
 */
int pa_init(pa_disaster_t type);

/**
 * @brief Update with sensor data
 * @param sensor_id Sensor identifier
 * @param data Sensor data (type-specific)
 * @param data_len Data length
 * @return 0 on success
 */
int pa_update_sensor(uint32_t sensor_id, const void* data, uint16_t data_len);

/**
 * @brief Run prediction algorithm
 * @param prediction Output prediction
 * @return 0 on success
 */
int pa_run_prediction(pa_prediction_t* prediction);

/**
 * @brief Get current alert level
 * @param alert Output alert level
 * @return 0 on success
 */
int pa_get_alert_level(pa_alert_t* alert);

/**
 * @brief Estimate arrival time at location
 * @param lat Latitude
 * @param lon Longitude
 * @param arrival_s Output arrival time in seconds
 * @return 0 on success
 */
int pa_estimate_arrival(double lat, double lon, float* arrival_s);

/**
 * @brief Register callback for alerts
 * @param callback Alert callback function
 * @param ctx User context
 * @return 0 on success
 */
int pa_register_alert_callback(void (*callback)(pa_alert_t, void*), void* ctx);

/**
 * @brief Shutdown prediction
 */
void pa_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PREDICTION_ALGORITHM_H */
