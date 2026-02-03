/**
 * @file irrigation.h
 * @brief Smart Irrigation Control Module
 * 
 * INDUSTRY RELEVANCE:
 * Water-efficient irrigation systems are critical for sustainable agriculture.
 * This module demonstrates:
 * - Zone-based irrigation scheduling and control
 * - Weather-responsive watering (ET-based calculations)
 * - Flow monitoring and leak detection
 * - Integration with soil sensors for closed-loop control
 * 
 * Applications: Farm irrigation, greenhouse systems, landscape management,
 *               golf courses, municipal water conservation
 * Standards: EPA WaterSense, IEEE 1451 (smart sensors)
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_IRRIGATION_H
#define GF_IRRIGATION_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Irrigation Types
 ******************************************************************************/

/** Irrigation method */
typedef enum {
    GF_IRRIG_DRIP,            /**< Drip irrigation */
    GF_IRRIG_SPRINKLER,       /**< Overhead sprinklers */
    GF_IRRIG_PIVOT,           /**< Center pivot */
    GF_IRRIG_FLOOD,           /**< Flood/furrow */
    GF_IRRIG_SUBSURFACE       /**< Subsurface drip */
} gf_irrigation_method_t;

/** Zone state */
typedef enum {
    GF_ZONE_OFF,
    GF_ZONE_SCHEDULED,        /**< Waiting for schedule */
    GF_ZONE_ACTIVE,           /**< Currently watering */
    GF_ZONE_PAUSED,           /**< Paused (rain, etc.) */
    GF_ZONE_FAULT,            /**< Fault detected */
    GF_ZONE_MAINTENANCE       /**< Maintenance mode */
} gf_zone_state_t;

/** Skip reason */
typedef enum {
    GF_SKIP_NONE,
    GF_SKIP_RAIN,             /**< Recent rainfall */
    GF_SKIP_MOISTURE,         /**< Soil moisture sufficient */
    GF_SKIP_FREEZE,           /**< Freeze protection */
    GF_SKIP_WIND,             /**< High wind */
    GF_SKIP_MANUAL            /**< User override */
} gf_skip_reason_t;

/*******************************************************************************
 * Irrigation Configuration
 ******************************************************************************/

/** Zone configuration */
typedef struct {
    uint8_t zone_id;
    char name[24];
    gf_irrigation_method_t method;
    uint16_t flow_rate_lpm;       /**< Nominal flow (L/min) */
    uint16_t area_sqm;            /**< Zone area (m²) */
    uint8_t soil_sensor_id;       /**< Associated soil sensor */
    uint8_t moisture_target_pct;  /**< Target moisture level */
    uint8_t valve_gpio;           /**< Valve control GPIO */
    bool is_master_valve;         /**< Requires master valve */
} gf_zone_config_t;

/** Schedule entry */
typedef struct {
    uint8_t zone_id;
    uint8_t start_hour;
    uint8_t start_minute;
    uint16_t duration_min;
    uint8_t days_mask;            /**< Bit mask: Sun=0x01 ... Sat=0x40 */
    bool enabled;
} gf_irrigation_schedule_t;

/** Irrigation controller configuration */
typedef struct {
    gf_zone_config_t zones[8];
    uint8_t zone_count;
    uint8_t master_valve_gpio;
    bool enable_rain_skip;
    uint8_t rain_threshold_mm;    /**< Skip if rainfall > threshold */
    bool enable_freeze_skip;
    int8_t freeze_threshold_c;
    bool enable_flow_monitor;
    uint8_t flow_sensor_gpio;
    uint16_t max_concurrent_zones;/**< Max zones running together */
} gf_irrigation_config_t;

/** Zone status */
typedef struct {
    uint8_t zone_id;
    gf_zone_state_t state;
    uint16_t elapsed_seconds;
    uint16_t remaining_seconds;
    uint16_t flow_rate_lpm;       /**< Actual flow */
    uint32_t volume_liters;       /**< Water used this cycle */
    uint8_t moisture_before_pct;
    uint8_t moisture_current_pct;
    gf_skip_reason_t skip_reason;
} gf_zone_status_t;

/*******************************************************************************
 * Irrigation Statistics
 ******************************************************************************/

typedef struct {
    uint64_t total_volume_liters;
    uint32_t total_cycles;
    uint32_t skipped_cycles;
    uint32_t fault_count;
    uint32_t runtime_hours;
    uint32_t water_saved_liters;  /**< From smart skip */
    uint32_t leak_events;
    float avg_flow_efficiency;
} gf_irrigation_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize irrigation controller
 * @param config Controller configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_init(const gf_irrigation_config_t *config);

/**
 * @brief Start zone manually
 * @param zone_id Zone to start
 * @param duration_min Duration in minutes
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_start_zone(uint8_t zone_id, uint16_t duration_min);

/**
 * @brief Stop zone
 * @param zone_id Zone to stop
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_stop_zone(uint8_t zone_id);

/**
 * @brief Stop all zones
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_stop_all(void);

/**
 * @brief Get zone status
 * @param zone_id Zone to query
 * @param status Output status
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_get_zone_status(uint8_t zone_id, 
                                          gf_zone_status_t *status);

/**
 * @brief Add schedule entry
 * @param schedule Schedule entry
 * @return Schedule ID or negative error
 */
int32_t gf_irrigation_add_schedule(const gf_irrigation_schedule_t *schedule);

/**
 * @brief Remove schedule entry
 * @param schedule_id Schedule to remove
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_remove_schedule(uint8_t schedule_id);

/**
 * @brief Process scheduler (call periodically)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_process(void);

/**
 * @brief Report rainfall (for skip calculation)
 * @param amount_mm Rainfall amount
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_report_rain(uint8_t amount_mm);

/**
 * @brief Get controller statistics
 * @return Current statistics
 */
gf_irrigation_stats_t gf_irrigation_get_stats(void);

/**
 * @brief Shutdown irrigation controller
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_irrigation_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_IRRIGATION_H */
