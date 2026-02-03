/**
 * @file navigation_safety.h
 * @brief Maritime Navigation Safety Interlock Module
 * 
 * INDUSTRY RELEVANCE:
 * Navigation safety systems are critical for preventing:
 * - Collisions (COLREGS compliance)
 * - Groundings (chart and depth monitoring)
 * - Weather-related incidents
 * - Piracy zone incursions
 * 
 * IMO regulations mandate voyage data recorders and safety management.
 * Essential for P&I club insurance and flag state compliance.
 * 
 * STANDARDS: SOLAS Ch. V, IMO Resolution A.694(17), IEC 61174
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_NAVIGATION_SAFETY_H
#define GF_NAVIGATION_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Safety zones */
typedef enum {
    ZONE_OPEN_WATER,
    ZONE_COASTAL,
    ZONE_RESTRICTED,
    ZONE_PORT_APPROACH,
    ZONE_TRAFFIC_SEPARATION,
    ZONE_PROHIBITED,
    ZONE_HIGH_RISK           /**< Piracy/war risk */
} safety_zone_t;

/* Alert types */
typedef enum {
    ALERT_COLLISION_WARNING,
    ALERT_GROUNDING_DANGER,
    ALERT_WEATHER_WARNING,
    ALERT_ZONE_VIOLATION,
    ALERT_EQUIPMENT_FAILURE,
    ALERT_SECURITY_THREAT,
    ALERT_MAN_OVERBOARD
} nav_alert_t;

/* Safety interlock states */
typedef enum {
    INTERLOCK_CLEAR,
    INTERLOCK_CAUTION,
    INTERLOCK_WARNING,
    INTERLOCK_ALARM,
    INTERLOCK_EMERGENCY
} interlock_state_t;

/* Weather data */
typedef struct {
    float wind_speed_knots;
    float wind_direction;
    float wave_height_m;
    float wave_period_s;
    float visibility_nm;
    float current_speed_knots;
    float current_direction;
} weather_data_t;

/* Safety status */
typedef struct {
    interlock_state_t state;
    safety_zone_t current_zone;
    float min_depth_m;
    float under_keel_clearance_m;
    uint8_t active_alerts;
    bool ecdis_healthy;
    bool radar_healthy;
    bool ais_healthy;
    bool gps_healthy;
    float collision_risk;     /**< 0-1 risk factor */
} safety_status_t;

/**
 * @brief Initialize navigation safety system
 * @param vessel_draft Draft in meters
 * @return 0 on success
 */
int nav_safety_init(float vessel_draft);

/**
 * @brief Update weather conditions
 * @param weather Current weather data
 * @return 0 on success
 */
int nav_safety_update_weather(const weather_data_t *weather);

/**
 * @brief Update depth information
 * @param chart_depth Chart depth in meters
 * @param measured_depth Sounder depth in meters
 * @return 0 on success
 */
int nav_safety_update_depth(float chart_depth, float measured_depth);

/**
 * @brief Get current safety status
 * @param status Output safety status
 * @return 0 on success
 */
int nav_safety_get_status(safety_status_t *status);

/**
 * @brief Check if route is safe
 * @param waypoints Array of lat/lon pairs
 * @param num_waypoints Number of waypoints
 * @return true if route is safe
 */
bool nav_safety_check_route(const float *waypoints, uint16_t num_waypoints);

/**
 * @brief Trigger alert
 * @param alert Alert type
 * @param severity Severity level
 * @param message Alert message
 * @return 0 on success
 */
int nav_safety_trigger_alert(nav_alert_t alert, interlock_state_t severity,
                             const char *message);

/**
 * @brief Acknowledge alert
 * @param alert_id Alert to acknowledge
 * @return 0 on success
 */
int nav_safety_acknowledge(uint8_t alert_id);

/**
 * @brief Man overboard trigger
 * @param lat Estimated position latitude
 * @param lon Estimated position longitude
 * @return 0 on success
 */
int nav_safety_man_overboard(float lat, float lon);

/**
 * @brief Check if emergency stop is required
 * @return true if immediate stop needed
 */
bool nav_safety_emergency_required(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_NAVIGATION_SAFETY_H */
