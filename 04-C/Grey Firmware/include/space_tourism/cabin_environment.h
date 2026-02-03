/**
 * @file cabin_environment.h
 * @brief Space Tourism Cabin Environment Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Commercial space tourism (Virgin Galactic, Blue Origin, SpaceX) requires
 * sophisticated cabin environment monitoring to ensure passenger safety and
 * comfort during suborbital and orbital flights. This includes:
 * - Atmospheric pressure and composition monitoring
 * - Temperature and humidity control
 * - CO2/O2 level tracking
 * - Cabin leak detection
 * 
 * This stub demonstrates understanding of human-rated spacecraft systems
 * and the unique challenges of maintaining habitable conditions in space.
 * 
 * STANDARDS: NASA-STD-3001, FAA Commercial Space Regulations
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CABIN_ENVIRONMENT_H
#define GF_CABIN_ENVIRONMENT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Cabin zones */
typedef enum {
    CABIN_ZONE_COCKPIT,
    CABIN_ZONE_PASSENGER,
    CABIN_ZONE_AIRLOCK,
    CABIN_ZONE_CARGO,
    CABIN_ZONE_COUNT
} cabin_zone_t;

/* Environmental parameters */
typedef struct {
    float pressure_kpa;        /**< Cabin pressure (101.3 nominal) */
    float temperature_c;       /**< Temperature (20-24°C nominal) */
    float humidity_pct;        /**< Relative humidity (40-60%) */
    float o2_pct;              /**< Oxygen percentage (20.9% nominal) */
    float co2_ppm;             /**< CO2 level (<5000 ppm limit) */
    float co_ppm;              /**< Carbon monoxide (<25 ppm limit) */
    float leak_rate_kpa_min;   /**< Pressure loss rate */
} cabin_env_data_t;

/* Alarm conditions */
typedef enum {
    CABIN_ALARM_NONE,
    CABIN_ALARM_LOW_PRESSURE,
    CABIN_ALARM_HIGH_CO2,
    CABIN_ALARM_LOW_O2,
    CABIN_ALARM_TEMP_EXCEED,
    CABIN_ALARM_LEAK_DETECTED,
    CABIN_ALARM_CO_DETECTED
} cabin_alarm_t;

/**
 * @brief Initialize cabin environment monitoring
 * @return 0 on success, negative on error
 */
int cabin_env_init(void);

/**
 * @brief Read environmental data for a zone
 * @param zone Cabin zone to query
 * @param data Output data structure
 * @return 0 on success, negative on error
 */
int cabin_env_read(cabin_zone_t zone, cabin_env_data_t *data);

/**
 * @brief Check for active alarms
 * @param zone Zone to check (or -1 for all zones)
 * @return Active alarm type, CABIN_ALARM_NONE if clear
 */
cabin_alarm_t cabin_env_check_alarms(int zone);

/**
 * @brief Set environmental setpoints
 * @param zone Target zone
 * @param temp_c Target temperature
 * @param pressure_kpa Target pressure
 * @return 0 on success
 */
int cabin_env_set_target(cabin_zone_t zone, float temp_c, float pressure_kpa);

/**
 * @brief Get cabin pressurization status
 * @return true if cabin is pressurized and stable
 */
bool cabin_env_is_pressurized(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CABIN_ENVIRONMENT_H */
