/**
 * @file navigation.h
 * @brief Automotive Navigation Data Interface
 *
 * INDUSTRY RELEVANCE:
 * Connected navigation systems integrate real-time traffic, mapping, and
 * ADAS features for modern vehicles. This module demonstrates:
 * - Map data ingestion and tile management
 * - Route calculation and maneuver generation
 * - Integration with ADAS (lane guidance, speed limits)
 * - Connected services (traffic, POI, OTA map updates)
 *
 * These skills apply to companies developing navigation solutions including
 * HERE Technologies, TomTom, Google Maps Platform, Apple Maps, and automotive
 * OEMs building in-house navigation stacks (Tesla, Rivian).
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires licensed map data and algorithms.
 */

#ifndef GF_NAVIGATION_H
#define GF_NAVIGATION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_NAV_MAX_WAYPOINTS        50      /**< Maximum route waypoints */
#define GF_NAV_MAX_MANEUVERS        200     /**< Maximum route maneuvers */
#define GF_NAV_MAX_NAME_LEN         128     /**< Maximum place name length */
#define GF_NAV_MAX_LANES            8       /**< Maximum lane count */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Geographic coordinate
 */
typedef struct {
    double latitude;            /**< Latitude in degrees (-90 to 90) */
    double longitude;           /**< Longitude in degrees (-180 to 180) */
    float altitude_m;           /**< Altitude in meters (optional) */
} gf_nav_coord_t;

/**
 * @brief Maneuver type
 */
typedef enum {
    GF_NAV_MANEUVER_STRAIGHT,       /**< Continue straight */
    GF_NAV_MANEUVER_TURN_LEFT,      /**< Turn left */
    GF_NAV_MANEUVER_TURN_RIGHT,     /**< Turn right */
    GF_NAV_MANEUVER_SLIGHT_LEFT,    /**< Bear left */
    GF_NAV_MANEUVER_SLIGHT_RIGHT,   /**< Bear right */
    GF_NAV_MANEUVER_SHARP_LEFT,     /**< Sharp left */
    GF_NAV_MANEUVER_SHARP_RIGHT,    /**< Sharp right */
    GF_NAV_MANEUVER_UTURN,          /**< U-turn */
    GF_NAV_MANEUVER_MERGE,          /**< Merge onto */
    GF_NAV_MANEUVER_EXIT,           /**< Take exit */
    GF_NAV_MANEUVER_FORK_LEFT,      /**< Take left fork */
    GF_NAV_MANEUVER_FORK_RIGHT,     /**< Take right fork */
    GF_NAV_MANEUVER_ROUNDABOUT,     /**< Roundabout */
    GF_NAV_MANEUVER_DESTINATION     /**< Arrive at destination */
} gf_nav_maneuver_type_t;

/**
 * @brief Road type classification
 */
typedef enum {
    GF_NAV_ROAD_MOTORWAY,       /**< Motorway/highway */
    GF_NAV_ROAD_TRUNK,          /**< Trunk road */
    GF_NAV_ROAD_PRIMARY,        /**< Primary road */
    GF_NAV_ROAD_SECONDARY,      /**< Secondary road */
    GF_NAV_ROAD_TERTIARY,       /**< Tertiary road */
    GF_NAV_ROAD_RESIDENTIAL,    /**< Residential street */
    GF_NAV_ROAD_UNPAVED         /**< Unpaved/dirt road */
} gf_nav_road_type_t;

/**
 * @brief Lane direction indicator
 */
typedef enum {
    GF_NAV_LANE_STRAIGHT    = 0x01,
    GF_NAV_LANE_LEFT        = 0x02,
    GF_NAV_LANE_RIGHT       = 0x04,
    GF_NAV_LANE_SLIGHT_LEFT = 0x08,
    GF_NAV_LANE_SLIGHT_RIGHT= 0x10,
    GF_NAV_LANE_UTURN       = 0x20
} gf_nav_lane_dir_t;

/**
 * @brief Traffic severity
 */
typedef enum {
    GF_NAV_TRAFFIC_FREE,        /**< Free flow */
    GF_NAV_TRAFFIC_LIGHT,       /**< Light traffic */
    GF_NAV_TRAFFIC_MODERATE,    /**< Moderate traffic */
    GF_NAV_TRAFFIC_HEAVY,       /**< Heavy traffic */
    GF_NAV_TRAFFIC_STANDSTILL   /**< Standstill/jam */
} gf_nav_traffic_t;

/**
 * @brief Route maneuver
 */
typedef struct {
    gf_nav_maneuver_type_t type;        /**< Maneuver type */
    gf_nav_coord_t location;            /**< Maneuver location */
    float distance_m;                   /**< Distance from previous */
    uint32_t duration_s;                /**< Time from previous */
    char road_name[GF_NAV_MAX_NAME_LEN];/**< Current road name */
    char next_road[GF_NAV_MAX_NAME_LEN];/**< Road after maneuver */
    uint8_t exit_number;                /**< Roundabout/highway exit */
    uint8_t lane_count;                 /**< Number of lanes */
    uint8_t lanes[GF_NAV_MAX_LANES];    /**< Lane directions (bitmask) */
    uint8_t recommended_lane;           /**< Recommended lane (1-based) */
} gf_nav_maneuver_t;

/**
 * @brief Route summary
 */
typedef struct {
    float total_distance_m;             /**< Total distance */
    uint32_t total_duration_s;          /**< Total time (no traffic) */
    uint32_t traffic_delay_s;           /**< Traffic delay */
    uint32_t eta_utc;                   /**< Estimated arrival (UTC) */
    uint16_t num_maneuvers;             /**< Number of maneuvers */
    gf_nav_traffic_t overall_traffic;   /**< Overall traffic severity */
    float toll_cost;                    /**< Estimated tolls (local currency) */
} gf_nav_route_summary_t;

/**
 * @brief Speed limit information
 */
typedef struct {
    uint8_t speed_limit_kmh;            /**< Posted speed limit */
    bool speed_limit_explicit;          /**< Explicit sign present */
    bool school_zone;                   /**< School zone active */
    bool construction_zone;             /**< Construction zone */
    bool camera_ahead;                  /**< Speed camera warning */
} gf_nav_speed_info_t;

/**
 * @brief Navigation guidance state
 */
typedef struct {
    gf_nav_coord_t current_position;    /**< Current GPS position */
    float heading_deg;                  /**< Current heading */
    float speed_kmh;                    /**< Current speed */
    gf_nav_maneuver_t* next_maneuver;   /**< Upcoming maneuver */
    float distance_to_maneuver_m;       /**< Distance to next maneuver */
    uint32_t time_to_maneuver_s;        /**< Time to next maneuver */
    gf_nav_speed_info_t speed_info;     /**< Speed limit info */
    bool off_route;                     /**< Off-route flag */
} gf_nav_guidance_t;

/**
 * @brief Route calculation options
 */
typedef struct {
    bool avoid_tolls;                   /**< Avoid toll roads */
    bool avoid_highways;                /**< Avoid highways */
    bool avoid_ferries;                 /**< Avoid ferries */
    bool prefer_scenic;                 /**< Scenic route preference */
    float vehicle_height_m;             /**< Vehicle height for restrictions */
    float vehicle_weight_kg;            /**< Vehicle weight for restrictions */
    uint8_t hazmat_class;               /**< Hazmat class (0 = none) */
} gf_nav_route_options_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_nav_guidance_cb_t)(const gf_nav_guidance_t* guidance, void* user_data);
typedef void (*gf_nav_reroute_cb_t)(const gf_nav_route_summary_t* new_route, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize navigation subsystem
 * @param map_path Path to map data
 * @return 0 on success, negative error code on failure
 */
int gf_nav_init(const char* map_path);

/**
 * @brief Shutdown navigation subsystem
 */
void gf_nav_deinit(void);

/**
 * @brief Calculate route
 * @param origin Start location
 * @param destination End location
 * @param waypoints Intermediate waypoints (can be NULL)
 * @param num_waypoints Number of waypoints
 * @param options Route options
 * @param[out] summary Route summary output
 * @return 0 on success, negative error code on failure
 */
int gf_nav_calculate_route(const gf_nav_coord_t* origin,
                            const gf_nav_coord_t* destination,
                            const gf_nav_coord_t* waypoints,
                            uint8_t num_waypoints,
                            const gf_nav_route_options_t* options,
                            gf_nav_route_summary_t* summary);

/**
 * @brief Start navigation guidance
 * @param callback Guidance update callback
 * @param user_data User context
 * @return 0 on success
 */
int gf_nav_start_guidance(gf_nav_guidance_cb_t callback, void* user_data);

/**
 * @brief Stop navigation guidance
 */
void gf_nav_stop_guidance(void);

/**
 * @brief Update GPS position
 * @param position Current position
 * @param heading Heading in degrees
 * @param speed Speed in km/h
 */
void gf_nav_update_position(const gf_nav_coord_t* position, float heading, float speed);

/**
 * @brief Get maneuver at index
 * @param index Maneuver index
 * @param[out] maneuver Output maneuver
 * @return 0 on success, -1 if index out of range
 */
int gf_nav_get_maneuver(uint16_t index, gf_nav_maneuver_t* maneuver);

/**
 * @brief Request traffic update
 * @return 0 on success
 */
int gf_nav_request_traffic_update(void);

/**
 * @brief Set reroute callback
 * @param callback Reroute notification callback
 * @param user_data User context
 */
void gf_nav_set_reroute_callback(gf_nav_reroute_cb_t callback, void* user_data);

/**
 * @brief Cancel current route
 */
void gf_nav_cancel_route(void);

/**
 * @brief Get text-to-speech maneuver instruction
 * @param maneuver Maneuver to describe
 * @param[out] text Output text buffer
 * @param text_len Buffer length
 * @return 0 on success
 */
int gf_nav_get_voice_instruction(const gf_nav_maneuver_t* maneuver, char* text, size_t text_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_NAVIGATION_H */
