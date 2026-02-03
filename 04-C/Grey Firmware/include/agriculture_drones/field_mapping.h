/**
 * @file field_mapping.h
 * @brief Agricultural Sensor Fusion for Field Mapping
 * 
 * INDUSTRY RELEVANCE:
 * Precision agriculture requires real-time field mapping for:
 * - NDVI (Normalized Difference Vegetation Index) analysis
 * - Soil moisture and nutrient mapping
 * - Weed detection and targeted spraying
 * - Yield prediction and harvest optimization
 * - Variable-rate seeding and fertilization
 * 
 * This stub demonstrates sensor fusion expertise for:
 * - Multispectral camera integration (RedEdge, Sequoia)
 * - LiDAR terrain mapping for drainage planning
 * - Thermal imaging for irrigation management
 * - GPS/RTK positioning for centimeter accuracy
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_AGRICULTURE_DRONES_FIELD_MAPPING_H
#define GF_AGRICULTURE_DRONES_FIELD_MAPPING_H

#include <stdint.h>
#include <stdbool.h>

/** Spectral bands */
typedef enum {
    BAND_RED,
    BAND_GREEN,
    BAND_BLUE,
    BAND_NIR,           /**< Near Infrared */
    BAND_REDEDGE,
    BAND_THERMAL
} spectral_band_t;

/** Field zone health */
typedef enum {
    ZONE_HEALTH_EXCELLENT,
    ZONE_HEALTH_GOOD,
    ZONE_HEALTH_MODERATE,
    ZONE_HEALTH_STRESSED,
    ZONE_HEALTH_CRITICAL
} zone_health_t;

/** Field mapping point */
typedef struct {
    double latitude;
    double longitude;
    float altitude_m;
    float ndvi;                 /**< -1.0 to 1.0 */
    float soil_moisture_pct;
    float canopy_height_m;
    zone_health_t health;
    uint32_t timestamp;
} mapping_point_t;

/** Field statistics */
typedef struct {
    float total_area_ha;
    float mapped_area_ha;
    float avg_ndvi;
    float min_ndvi;
    float max_ndvi;
    float stressed_area_pct;
    uint32_t point_count;
} field_stats_t;

/* API Functions */
int field_mapping_init(void);
int field_mapping_start(void);
int field_mapping_stop(void);
int field_mapping_add_point(const mapping_point_t *point);
int field_mapping_get_stats(field_stats_t *stats);
float field_mapping_calculate_ndvi(float red, float nir);
int field_mapping_export_geotiff(const char *filename);

#endif /* GF_AGRICULTURE_DRONES_FIELD_MAPPING_H */
