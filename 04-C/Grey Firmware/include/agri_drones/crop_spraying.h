/**
 * @file crop_spraying.h
 * @brief Agricultural Drone Crop Spraying Actuator Interface
 * 
 * INDUSTRY RELEVANCE:
 * Precision agriculture using drone-based crop spraying offers significant
 * cost savings and environmental benefits over traditional methods:
 * - Variable rate application (VRA) reduces chemical usage by 30-50%
 * - GPS-guided precision reduces overlap and missed areas
 * - Night operation capability extends application windows
 * - Real-time coverage mapping for compliance
 * 
 * Major players: DJI Agras, XAG, HSE-UAV
 * 
 * STANDARDS: EPA pesticide application, FAA Part 107 waiver requirements
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CROP_SPRAYING_H
#define GF_CROP_SPRAYING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_SPRAY_NOZZLES 8

/* Spray pattern types */
typedef enum {
    SPRAY_PATTERN_BROADCAST,
    SPRAY_PATTERN_BAND,
    SPRAY_PATTERN_SPOT,
    SPRAY_PATTERN_VARIABLE
} spray_pattern_t;

/* Actuator status */
typedef enum {
    SPRAY_STATUS_IDLE,
    SPRAY_STATUS_PRIMING,
    SPRAY_STATUS_ACTIVE,
    SPRAY_STATUS_BLOCKED,
    SPRAY_STATUS_EMPTY,
    SPRAY_STATUS_FAULT
} spray_status_t;

/* Spray configuration */
typedef struct {
    spray_pattern_t pattern;
    float flow_rate_l_min;
    float droplet_size_um;
    float swath_width_m;
    uint8_t active_nozzles;
    bool drift_reduction;
} spray_config_t;

/* Spray telemetry */
typedef struct {
    float tank_level_pct;
    float flow_rate_actual;
    float pressure_bar;
    float area_covered_ha;
    float volume_dispensed_l;
    uint32_t nozzle_status; /* Bitmask */
    spray_status_t status;
} spray_telemetry_t;

/**
 * @brief Initialize spray system
 * @param num_nozzles Number of spray nozzles
 * @return 0 on success
 */
int crop_spray_init(uint8_t num_nozzles);

/**
 * @brief Configure spray parameters
 * @param config Spray configuration
 * @return 0 on success
 */
int crop_spray_config(const spray_config_t *config);

/**
 * @brief Start spraying at specified rate
 * @param rate_l_ha Application rate in liters per hectare
 * @return 0 on success
 */
int crop_spray_start(float rate_l_ha);

/**
 * @brief Stop spraying
 * @return 0 on success
 */
int crop_spray_stop(void);

/**
 * @brief Set variable rate based on prescription map
 * @param zone_id Current zone from prescription map
 * @param rate_l_ha Zone-specific rate
 * @return 0 on success
 */
int crop_spray_set_vra(uint16_t zone_id, float rate_l_ha);

/**
 * @brief Get current spray telemetry
 * @param telem Output telemetry structure
 * @return 0 on success
 */
int crop_spray_get_telemetry(spray_telemetry_t *telem);

/**
 * @brief Check nozzle health
 * @return Bitmask of blocked/faulty nozzles
 */
uint32_t crop_spray_check_nozzles(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CROP_SPRAYING_H */
