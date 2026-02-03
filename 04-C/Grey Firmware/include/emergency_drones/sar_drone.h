/**
 * @file sar_drone.h
 * @brief Search-and-Rescue Drone Control Stub
 * 
 * Industry Relevance:
 * SAR drones are increasingly critical for disaster response, wilderness
 * rescue, and urban emergency operations. This module demonstrates:
 * - Autonomous search pattern execution
 * - GPS-denied navigation using visual/thermal cues
 * - Multi-drone coordination and swarm behavior
 * - Real-time mission replanning based on sensor feedback
 * 
 * Applications: Disaster response, maritime rescue, wilderness SAR, urban emergency
 * 
 * @author Grey Firmware Project
 */

#ifndef SAR_DRONE_H
#define SAR_DRONE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** SAR mission type */
typedef enum {
    SAR_MISSION_GRID_SEARCH,     /**< Systematic grid pattern */
    SAR_MISSION_EXPANDING_SPIRAL,/**< Spiral from last known position */
    SAR_MISSION_CONTOUR_SEARCH,  /**< Follow terrain contours */
    SAR_MISSION_POINT_OF_INTEREST,/**< Investigate specific location */
    SAR_MISSION_ESCORT           /**< Escort ground team */
} sar_mission_t;

/** Drone operational status */
typedef enum {
    SAR_STATUS_STANDBY,
    SAR_STATUS_LAUNCHING,
    SAR_STATUS_SEARCHING,
    SAR_STATUS_HOVERING,
    SAR_STATUS_TRACKING,
    SAR_STATUS_RETURNING,
    SAR_STATUS_EMERGENCY
} sar_status_t;

/** Search area definition */
typedef struct {
    double center_lat;           /**< Center latitude */
    double center_lon;           /**< Center longitude */
    float radius_m;              /**< Search radius (meters) */
    float altitude_m;            /**< Search altitude AGL */
    float speed_mps;             /**< Search speed (m/s) */
} search_area_t;

/** Drone state telemetry */
typedef struct {
    double latitude;             /**< Current latitude */
    double longitude;            /**< Current longitude */
    float altitude_m;            /**< Current altitude AGL */
    float battery_pct;           /**< Battery remaining */
    float ground_speed;          /**< Ground speed (m/s) */
    sar_status_t status;         /**< Operational status */
    uint16_t search_coverage_pct;/**< Area covered percentage */
} sar_drone_state_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize SAR drone subsystem
 * @return 0 on success, negative on error
 */
int sar_drone_init(void);

/**
 * @brief Start SAR mission
 * @param mission Mission type
 * @param area Search area parameters
 * @return 0 on success, negative on error
 */
int sar_drone_start_mission(sar_mission_t mission, const search_area_t *area);

/**
 * @brief Get current drone state
 * @param state Output state structure
 * @return 0 on success, negative on error
 */
int sar_drone_get_state(sar_drone_state_t *state);

/**
 * @brief Mark point of interest for investigation
 * @param lat Latitude of POI
 * @param lon Longitude of POI
 * @return POI ID, negative on error
 */
int32_t sar_drone_mark_poi(double lat, double lon);

/**
 * @brief Abort mission and return to base
 * @return 0 on success, negative on error
 */
int sar_drone_abort(void);

/**
 * @brief Shutdown SAR drone subsystem
 */
void sar_drone_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* SAR_DRONE_H */
