/**
 * @file crop_monitor.h
 * @brief Crop Monitoring Sensor Module - Smart Agriculture Robotics
 * 
 * @details Industry Relevance:
 * Precision agriculture leverages multispectral and hyperspectral sensors
 * to monitor crop health, enabling early detection of:
 * - Nutrient deficiencies (nitrogen, phosphorus, potassium)
 * - Water stress through NDVI/NDRE indices
 * - Pest and disease outbreaks before visible symptoms
 * - Yield prediction and harvest timing optimization
 * 
 * ROI: Early intervention reduces crop losses by 10-30% and optimizes
 * input costs. Global precision ag market projected at $12B by 2027.
 * 
 * Sensors: RGB cameras, NIR/SWIR spectral bands, thermal imaging,
 * LiDAR for canopy structure analysis.
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_AGRICULTURE_CROP_MONITOR_H
#define GF_AGRICULTURE_CROP_MONITOR_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum spectral bands supported */
#define GF_CROP_MAX_SPECTRAL_BANDS      12

/** NDVI healthy threshold */
#define GF_CROP_NDVI_HEALTHY            0.6f

/** NDVI stress threshold */
#define GF_CROP_NDVI_STRESS             0.3f

/** Sample rate for spectral sensors (Hz) */
#define GF_CROP_SAMPLE_RATE_HZ          5

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Vegetation index types
 */
typedef enum {
    GF_CROP_INDEX_NDVI,             /**< Normalized Difference Vegetation Index */
    GF_CROP_INDEX_NDRE,             /**< Normalized Difference Red Edge */
    GF_CROP_INDEX_GNDVI,            /**< Green NDVI */
    GF_CROP_INDEX_SAVI,             /**< Soil Adjusted Vegetation Index */
    GF_CROP_INDEX_EVI,              /**< Enhanced Vegetation Index */
    GF_CROP_INDEX_LAI,              /**< Leaf Area Index */
    GF_CROP_INDEX_CHL,              /**< Chlorophyll content */
    GF_CROP_INDEX_CWSI              /**< Crop Water Stress Index */
} gf_crop_index_t;

/**
 * @brief Crop health status
 */
typedef enum {
    GF_CROP_HEALTH_EXCELLENT,       /**< Optimal growth conditions */
    GF_CROP_HEALTH_GOOD,            /**< Normal healthy crop */
    GF_CROP_HEALTH_MODERATE,        /**< Minor stress detected */
    GF_CROP_HEALTH_STRESSED,        /**< Significant stress visible */
    GF_CROP_HEALTH_CRITICAL,        /**< Intervention required */
    GF_CROP_HEALTH_UNKNOWN          /**< Insufficient data */
} gf_crop_health_t;

/**
 * @brief Spectral band reading
 */
typedef struct {
    uint16_t band_nm;               /**< Center wavelength in nm */
    float reflectance;              /**< Reflectance value 0.0-1.0 */
    float radiance;                 /**< Radiance W/m²/sr/nm */
    bool valid;                     /**< Reading validity flag */
} gf_crop_spectral_t;

/**
 * @brief Crop monitoring sample
 */
typedef struct {
    double latitude_deg;            /**< Sample location lat */
    double longitude_deg;           /**< Sample location lon */
    uint32_t timestamp;             /**< Unix timestamp */
    gf_crop_spectral_t bands[GF_CROP_MAX_SPECTRAL_BANDS];
    uint8_t band_count;             /**< Number of valid bands */
    float ndvi;                     /**< Computed NDVI */
    float ndre;                     /**< Computed NDRE */
    float canopy_temp_c;            /**< Thermal reading */
    gf_crop_health_t health;        /**< Derived health status */
} gf_crop_sample_t;

/**
 * @brief Zone-level analysis result
 */
typedef struct {
    uint16_t zone_id;               /**< Management zone ID */
    float avg_ndvi;                 /**< Zone average NDVI */
    float avg_ndre;                 /**< Zone average NDRE */
    float variability;              /**< Within-zone variability */
    gf_crop_health_t health;        /**< Zone health assessment */
    float recommended_n_rate;       /**< Nitrogen rate kg/ha */
    float recommended_water_mm;     /**< Irrigation recommendation */
} gf_crop_zone_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize crop monitoring system
 * @return 0 on success, negative error code on failure
 */
int gf_crop_init(void);

/**
 * @brief Capture spectral sample at current location
 * @param sample Output sample structure
 * @return 0 on success
 */
int gf_crop_capture_sample(gf_crop_sample_t* sample);

/**
 * @brief Compute vegetation index from spectral data
 * @param sample Input spectral sample
 * @param index Index type to compute
 * @param result Output index value
 * @return 0 on success
 */
int gf_crop_compute_index(const gf_crop_sample_t* sample,
                          gf_crop_index_t index,
                          float* result);

/**
 * @brief Analyze management zone from samples
 * @param samples Array of samples in zone
 * @param count Number of samples
 * @param zone Output zone analysis
 * @return 0 on success
 */
int gf_crop_analyze_zone(const gf_crop_sample_t* samples,
                         uint16_t count,
                         gf_crop_zone_t* zone);

/**
 * @brief Generate prescription map for variable rate application
 * @param zones Array of zone analyses
 * @param zone_count Number of zones
 * @param prescription Output prescription data
 * @return 0 on success
 */
int gf_crop_generate_prescription(const gf_crop_zone_t* zones,
                                  uint16_t zone_count,
                                  void* prescription);

/**
 * @brief Shutdown crop monitoring system
 * @return 0 on success
 */
int gf_crop_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_AGRICULTURE_CROP_MONITOR_H */
