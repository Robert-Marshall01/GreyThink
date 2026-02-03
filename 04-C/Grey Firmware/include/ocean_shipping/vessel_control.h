/**
 * @file vessel_control.h
 * @brief Autonomous Vessel Control System Interface
 * 
 * INDUSTRY RELEVANCE:
 * Autonomous shipping represents a $150B+ market transformation:
 * - Reduced crew costs (60% of operating expenses)
 * - 24/7 operation without fatigue concerns
 * - Optimal fuel efficiency through AI route planning
 * - Reduced human error (cause of 75-96% of maritime incidents)
 * 
 * Major initiatives: Yara Birkeland, Rolls-Royce AAWA, Kongsberg Maritime
 * 
 * STANDARDS: IMO MSC.1/Circ.1638, COLREGS, SOLAS
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_VESSEL_CONTROL_H
#define GF_VESSEL_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Autonomy levels (based on IMO definitions) */
typedef enum {
    AUTONOMY_MANNED,           /**< Fully manned operation */
    AUTONOMY_ASSISTED,         /**< Decision support only */
    AUTONOMY_REMOTE,           /**< Remote human control */
    AUTONOMY_SUPERVISED,       /**< Autonomous with supervision */
    AUTONOMY_FULL              /**< Fully autonomous */
} autonomy_level_t;

/* Navigation modes */
typedef enum {
    NAV_MODE_MANUAL,
    NAV_MODE_HEADING_HOLD,
    NAV_MODE_WAYPOINT,
    NAV_MODE_TRACK_FOLLOWING,
    NAV_MODE_DYNAMIC_POSITIONING,
    NAV_MODE_COLLISION_AVOIDANCE,
    NAV_MODE_EMERGENCY
} nav_mode_t;

/* Vessel state */
typedef struct {
    float latitude;
    float longitude;
    float heading_deg;
    float course_over_ground;
    float speed_knots;
    float rate_of_turn_deg_min;
    float roll_deg;
    float pitch_deg;
    float heave_m;
} vessel_state_t;

/* Control commands */
typedef struct {
    float commanded_heading;
    float commanded_speed;
    float rudder_angle;
    float throttle_pct;
    bool bow_thruster_active;
    bool stern_thruster_active;
} vessel_command_t;

/* Collision avoidance target */
typedef struct {
    uint32_t mmsi;             /**< Maritime Mobile Service Identity */
    float range_nm;
    float bearing_deg;
    float cpa_nm;              /**< Closest Point of Approach */
    float tcpa_min;            /**< Time to CPA */
    bool dangerous;
} ais_target_t;

/**
 * @brief Initialize vessel control system
 * @param level Initial autonomy level
 * @return 0 on success
 */
int vessel_control_init(autonomy_level_t level);

/**
 * @brief Set navigation mode
 * @param mode Desired navigation mode
 * @return 0 on success
 */
int vessel_control_set_mode(nav_mode_t mode);

/**
 * @brief Update vessel state from sensors
 * @param state Current vessel state
 * @return 0 on success
 */
int vessel_control_update_state(const vessel_state_t *state);

/**
 * @brief Get computed control commands
 * @param cmd Output command structure
 * @return 0 on success
 */
int vessel_control_get_command(vessel_command_t *cmd);

/**
 * @brief Add waypoint to route
 * @param lat Waypoint latitude
 * @param lon Waypoint longitude
 * @param speed Target speed at waypoint
 * @return Waypoint index, or negative on error
 */
int vessel_control_add_waypoint(float lat, float lon, float speed);

/**
 * @brief Process AIS target for collision avoidance
 * @param target AIS target data
 * @return true if evasive action required
 */
bool vessel_control_process_ais(const ais_target_t *target);

/**
 * @brief Emergency stop
 * @return 0 on success
 */
int vessel_control_emergency_stop(void);

/**
 * @brief Get current autonomy level
 * @return Current autonomy level
 */
autonomy_level_t vessel_control_get_autonomy(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_VESSEL_CONTROL_H */
