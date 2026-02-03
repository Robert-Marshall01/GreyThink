/**
 * @file field_mapping.h
 * @brief Agricultural Drone Field Mapping & Sensor Fusion
 * 
 * INDUSTRY RELEVANCE:
 * Precision agriculture relies on accurate field mapping from multiple
 * sensor sources to optimize crop management:
 * - Multispectral imaging for NDVI vegetation health analysis
 * - Thermal imaging for irrigation monitoring
 * - LiDAR for terrain and canopy height mapping
 * - GPS/RTK for centimeter-level positioning
 * 
 * Data fusion enables prescription maps for variable rate application.
 * Market: $4.5B by 2025 (Grand View Research)
 * 
 * STANDARDS: ASABE S593, ISO 11783 (ISOBUS)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_FIELD_MAPPING_H
#define GF_FIELD_MAPPING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Sensor types for fusion */
typedef enum {
    SENSOR_RGB_CAMERA,
    SENSOR_MULTISPECTRAL,
    SENSOR_THERMAL_IR,
    SENSOR_LIDAR,
    SENSOR_GPS_RTK,
    SENSOR_COUNT
} field_sensor_t;

/* Vegetation indices */
typedef enum {
    INDEX_NDVI,    /**< Normalized Difference Vegetation Index */
    INDEX_NDRE,    /**< Normalized Difference Red Edge */
    INDEX_GNDVI,   /**< Green NDVI */
    INDEX_SAVI,    /**< Soil Adjusted Vegetation Index */
    INDEX_CWSI     /**< Crop Water Stress Index */
} vegetation_index_t;

/* Field zone data */
typedef struct {
    uint16_t zone_id;
    float center_lat;
    float center_lon;
    float area_ha;
    float ndvi;
    float canopy_height_m;
    float soil_moisture_pct;
    float temperature_c;
    uint8_t health_score;   /**< 0-100 composite health */
} field_zone_t;

/* Mapping configuration */
typedef struct {
    float overlap_pct;          /**< Image overlap (60-80% typical) */
    float altitude_m;           /**< Flight altitude AGL */
    float gsd_cm;              /**< Ground sample distance */
    bool rtk_enabled;          /**< RTK GPS enabled */
    uint32_t active_sensors;   /**< Bitmask of enabled sensors */
} mapping_config_t;

/**
 * @brief Initialize field mapping system
 * @param config Mapping configuration
 * @return 0 on success
 */
int field_map_init(const mapping_config_t *config);

/**
 * @brief Process sensor data and update map
 * @param sensor Source sensor type
 * @param data Raw sensor data
 * @param data_len Data length
 * @param lat Capture latitude
 * @param lon Capture longitude
 * @return 0 on success
 */
int field_map_process(field_sensor_t sensor, const uint8_t *data,
                      size_t data_len, float lat, float lon);

/**
 * @brief Calculate vegetation index for zone
 * @param zone_id Zone to analyze
 * @param index Index type to calculate
 * @return Index value (-1 to 1 for NDVI-type)
 */
float field_map_calc_index(uint16_t zone_id, vegetation_index_t index);

/**
 * @brief Get zone analysis data
 * @param zone_id Zone to query
 * @param zone Output zone data
 * @return 0 on success
 */
int field_map_get_zone(uint16_t zone_id, field_zone_t *zone);

/**
 * @brief Generate prescription map for VRA
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written
 */
int field_map_generate_prescription(uint8_t *buffer, size_t max_len);

/**
 * @brief Get mapping coverage percentage
 * @return Coverage percentage (0-100)
 */
float field_map_get_coverage(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FIELD_MAPPING_H */
