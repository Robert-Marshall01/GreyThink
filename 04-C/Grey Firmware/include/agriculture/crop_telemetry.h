/**
 * @file crop_telemetry.h
 * @brief Crop Telemetry and Analytics Collector
 * 
 * INDUSTRY RELEVANCE:
 * Modern precision agriculture relies on comprehensive field data collection
 * for yield prediction and resource optimization. This module demonstrates:
 * - Multi-sensor data aggregation (weather, soil, crop health)
 * - Growing degree day (GDD) and crop stage tracking
 * - Yield prediction and anomaly detection
 * - Cloud-based analytics integration
 * 
 * Applications: Precision farming platforms, agricultural research,
 *               crop insurance, supply chain forecasting
 * Standards: AgGateway ADAPT, ISO 11783 (ISOBUS)
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_CROP_TELEMETRY_H
#define GF_CROP_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Crop Telemetry Types
 ******************************************************************************/

/** Crop type */
typedef enum {
    GF_CROP_CORN,
    GF_CROP_WHEAT,
    GF_CROP_SOYBEAN,
    GF_CROP_COTTON,
    GF_CROP_RICE,
    GF_CROP_VEGETABLE,
    GF_CROP_FRUIT,
    GF_CROP_CUSTOM
} gf_crop_type_t;

/** Growth stage (Zadoks scale approximation) */
typedef enum {
    GF_STAGE_GERMINATION,     /**< 0-9: Seed to emergence */
    GF_STAGE_SEEDLING,        /**< 10-19: Leaf development */
    GF_STAGE_TILLERING,       /**< 20-29: Tillering */
    GF_STAGE_STEM_EXTENSION,  /**< 30-39: Stem elongation */
    GF_STAGE_BOOTING,         /**< 40-49: Booting */
    GF_STAGE_HEADING,         /**< 50-59: Head emergence */
    GF_STAGE_FLOWERING,       /**< 60-69: Flowering */
    GF_STAGE_GRAIN_FILL,      /**< 70-79: Grain development */
    GF_STAGE_RIPENING,        /**< 80-89: Ripening */
    GF_STAGE_HARVEST          /**< 90-99: Ready for harvest */
} gf_growth_stage_t;

/** Crop health indicator */
typedef enum {
    GF_HEALTH_EXCELLENT,
    GF_HEALTH_GOOD,
    GF_HEALTH_FAIR,
    GF_HEALTH_POOR,
    GF_HEALTH_CRITICAL
} gf_crop_health_t;

/** Stress type */
typedef enum {
    GF_STRESS_NONE,
    GF_STRESS_WATER,          /**< Drought stress */
    GF_STRESS_HEAT,           /**< Heat stress */
    GF_STRESS_COLD,           /**< Cold stress */
    GF_STRESS_NUTRIENT,       /**< Nutrient deficiency */
    GF_STRESS_DISEASE,        /**< Disease detected */
    GF_STRESS_PEST            /**< Pest damage */
} gf_stress_type_t;

/*******************************************************************************
 * Crop Telemetry Configuration
 ******************************************************************************/

/** Field configuration */
typedef struct {
    char field_id[16];
    char field_name[32];
    gf_crop_type_t crop_type;
    float area_hectares;
    int32_t latitude_deg_e7;      /**< Latitude × 10^7 */
    int32_t longitude_deg_e7;     /**< Longitude × 10^7 */
    uint16_t planting_doy;        /**< Day of year planted */
    uint16_t base_gdd_c10;        /**< Base temp for GDD (°C × 10) */
    uint16_t max_gdd_c10;         /**< Max temp cap (°C × 10) */
} gf_field_config_t;

/** Telemetry configuration */
typedef struct {
    gf_field_config_t field;
    uint32_t sample_interval_s;   /**< Data collection interval */
    uint32_t upload_interval_s;   /**< Cloud upload interval */
    bool enable_ndvi;             /**< Vegetation index sensor */
    bool enable_weather;          /**< Weather station */
    bool enable_yield_predict;    /**< ML yield prediction */
    char cloud_endpoint[64];
    char api_key[32];
} gf_crop_telemetry_config_t;

/** Weather data point */
typedef struct {
    int16_t temp_c10;             /**< Temperature (°C × 10) */
    int16_t temp_min_c10;
    int16_t temp_max_c10;
    uint8_t humidity_pct;
    uint16_t solar_rad_wm2;       /**< Solar radiation (W/m²) */
    uint8_t rainfall_mm;
    uint8_t wind_speed_kmh;
    uint16_t wind_direction_deg;
    uint16_t evapotranspiration_mm10; /**< ET (mm × 10) */
} gf_weather_data_t;

/** Crop telemetry reading */
typedef struct {
    uint32_t timestamp;
    gf_weather_data_t weather;
    
    /* Soil data (from soil sensors) */
    uint8_t soil_moisture_pct;
    int16_t soil_temp_c10;
    
    /* Crop data */
    gf_growth_stage_t stage;
    gf_crop_health_t health;
    gf_stress_type_t stress;
    uint16_t gdd_accumulated;     /**< Growing degree days */
    uint16_t ndvi_x1000;          /**< NDVI × 1000 (0-1000) */
    uint8_t canopy_cover_pct;     /**< Estimated canopy cover */
    
    /* Predictions */
    uint16_t yield_predict_kgha;  /**< Predicted yield (kg/ha) */
    uint8_t harvest_days;         /**< Days to harvest */
} gf_crop_telemetry_reading_t;

/*******************************************************************************
 * Crop Telemetry Statistics
 ******************************************************************************/

typedef struct {
    uint32_t readings_collected;
    uint32_t readings_uploaded;
    uint32_t upload_failures;
    uint16_t gdd_total;
    uint16_t rainfall_total_mm;
    float avg_ndvi;
    float yield_trend;            /**< +/- vs historical */
    uint32_t stress_events;
} gf_crop_telemetry_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize crop telemetry
 * @param config Telemetry configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_crop_telemetry_init(const gf_crop_telemetry_config_t *config);

/**
 * @brief Collect telemetry reading
 * @param reading Output reading
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_crop_telemetry_collect(gf_crop_telemetry_reading_t *reading);

/**
 * @brief Upload telemetry to cloud
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_crop_telemetry_upload(void);

/**
 * @brief Process telemetry (scheduler hook)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_crop_telemetry_process(void);

/**
 * @brief Update weather data
 * @param weather Weather reading
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_crop_telemetry_update_weather(const gf_weather_data_t *weather);

/**
 * @brief Get current growth stage
 * @return Current stage
 */
gf_growth_stage_t gf_crop_get_stage(void);

/**
 * @brief Get accumulated GDD
 * @return Growing degree days
 */
uint16_t gf_crop_get_gdd(void);

/**
 * @brief Predict yield
 * @return Predicted yield (kg/ha)
 */
uint16_t gf_crop_predict_yield(void);

/**
 * @brief Get telemetry statistics
 * @return Current statistics
 */
gf_crop_telemetry_stats_t gf_crop_telemetry_get_stats(void);

/**
 * @brief Shutdown crop telemetry
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_crop_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CROP_TELEMETRY_H */
