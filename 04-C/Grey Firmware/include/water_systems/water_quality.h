/**
 * @file water_quality.h
 * @brief Water Quality Monitoring Module
 * 
 * @details
 * This module provides comprehensive water quality monitoring including
 * online sensor integration, regulatory compliance checking, and trend
 * analysis for potable and process water systems.
 * 
 * INDUSTRY RELEVANCE:
 * - Municipal water utilities (EPA Safe Drinking Water Act)
 * - Industrial process water quality
 * - Aquaculture water management
 * - Swimming pool/spa monitoring
 * - Environmental monitoring
 * - Pharmaceutical water systems (USP purified water)
 * 
 * REGULATORY COMPLIANCE:
 * - EPA SDWA (Safe Drinking Water Act)
 * - WHO drinking water guidelines
 * - EU Drinking Water Directive
 * - FDA/USP water for pharma
 * - ISO 17025 lab accreditation
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_WATER_QUALITY_H
#define GF_WATER_QUALITY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_WQ_OK = 0,
    GF_WQ_ERROR_NOT_INITIALIZED,
    GF_WQ_ERROR_NULL_PTR,
    GF_WQ_ERROR_SENSOR_FAULT,
    GF_WQ_ERROR_CALIBRATION,
    GF_WQ_ERROR_OUT_OF_RANGE,
    GF_WQ_WARN_EXCEEDANCE,
    GF_WQ_WARN_TREND_ADVERSE
} gf_wq_status_t;

typedef enum {
    GF_WQ_SENSOR_PH,              /**< pH sensor */
    GF_WQ_SENSOR_CONDUCTIVITY,    /**< Conductivity/TDS */
    GF_WQ_SENSOR_TURBIDITY,       /**< Turbidity (NTU) */
    GF_WQ_SENSOR_CHLORINE,        /**< Free/total chlorine */
    GF_WQ_SENSOR_DO,              /**< Dissolved oxygen */
    GF_WQ_SENSOR_ORP,             /**< Oxidation-reduction potential */
    GF_WQ_SENSOR_TEMPERATURE,     /**< Temperature */
    GF_WQ_SENSOR_AMMONIA,         /**< Ammonia (NH3/NH4+) */
    GF_WQ_SENSOR_NITRATE,         /**< Nitrate */
    GF_WQ_SENSOR_PHOSPHATE,       /**< Phosphate */
    GF_WQ_SENSOR_TOC,             /**< Total organic carbon */
    GF_WQ_SENSOR_UV254,           /**< UV absorbance at 254nm */
    GF_WQ_SENSOR_FLOW             /**< Flow rate */
} gf_wq_sensor_t;

typedef enum {
    GF_WQ_WATER_POTABLE,          /**< Drinking water */
    GF_WQ_WATER_PROCESS,          /**< Industrial process */
    GF_WQ_WATER_WASTEWATER,       /**< Treated wastewater */
    GF_WQ_WATER_AQUACULTURE,      /**< Fish/shellfish farming */
    GF_WQ_WATER_POOL,             /**< Pool/spa water */
    GF_WQ_WATER_PHARMA            /**< Pharmaceutical grade */
} gf_wq_water_type_t;

typedef struct {
    gf_wq_sensor_t sensor;        /**< Sensor type */
    float value;                  /**< Reading value */
    float uncertainty;            /**< Measurement uncertainty */
    uint8_t quality_pct;          /**< Quality score (0-100) */
    bool alarm_active;            /**< Limit exceeded */
    uint64_t timestamp_ms;        /**< Reading timestamp */
} gf_wq_reading_t;

typedef struct {
    float limit_min;              /**< Minimum limit */
    float limit_max;              /**< Maximum limit */
    float target;                 /**< Target value */
    float alarm_delay_sec;        /**< Alarm delay */
    bool enabled;                 /**< Limit enabled */
} gf_wq_limit_t;

typedef struct {
    float ph;
    float conductivity_us;
    float turbidity_ntu;
    float chlorine_mgl;
    float do_mgl;
    float orp_mv;
    float temperature_c;
    float flow_lpm;
    uint8_t num_alarms;
    bool compliant;
    uint64_t timestamp_ms;
} gf_wq_summary_t;

typedef struct {
    gf_wq_sensor_t sensor;        /**< Parameter being trended */
    float current_value;          /**< Current value */
    float avg_1hr;                /**< 1-hour average */
    float avg_24hr;               /**< 24-hour average */
    float rate_per_hr;            /**< Rate of change */
    float predicted_4hr;          /**< 4-hour prediction */
    bool trend_adverse;           /**< Trend toward limit */
} gf_wq_trend_t;

typedef void (*gf_wq_alarm_cb_t)(gf_wq_sensor_t sensor, float value, 
                                  bool is_high, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_wq_status_t gf_wq_init(gf_wq_water_type_t water_type);
void gf_wq_shutdown(void);
gf_wq_status_t gf_wq_configure_sensor(gf_wq_sensor_t sensor, bool enabled,
                                       uint32_t sample_interval_ms);
gf_wq_status_t gf_wq_set_limits(gf_wq_sensor_t sensor, const gf_wq_limit_t* limits);
gf_wq_status_t gf_wq_calibrate(gf_wq_sensor_t sensor, float reference_value);
gf_wq_status_t gf_wq_get_reading(gf_wq_sensor_t sensor, gf_wq_reading_t* reading);
gf_wq_status_t gf_wq_get_summary(gf_wq_summary_t* summary);
gf_wq_status_t gf_wq_get_trend(gf_wq_sensor_t sensor, gf_wq_trend_t* trend);
gf_wq_status_t gf_wq_check_compliance(bool* compliant, uint8_t* violation_count);
gf_wq_status_t gf_wq_register_alarm_callback(gf_wq_alarm_cb_t callback, void* user_data);
gf_wq_status_t gf_wq_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_WATER_QUALITY_H */
