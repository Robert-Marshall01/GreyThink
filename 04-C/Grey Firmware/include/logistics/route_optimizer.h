/**
 * @file route_optimizer.h
 * @brief Routing Optimization Module for Logistics Operations
 * 
 * @details
 * This module provides embedded routing optimization for logistics vehicles
 * and warehouse robots, including path planning, traffic-aware routing,
 * and multi-stop optimization.
 * 
 * INDUSTRY RELEVANCE:
 * - Last-mile delivery optimization
 * - Fleet management systems
 * - Warehouse robot path planning
 * - Public transit scheduling
 * - Food delivery platforms
 * - Field service management
 * 
 * KEY ALGORITHMS:
 * - A* pathfinding for indoor navigation
 * - Vehicle Routing Problem (VRP) heuristics
 * - Time-window constrained optimization
 * - Dynamic re-routing on obstacles
 * - Multi-depot optimization
 * - Capacity-constrained routing
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_ROUTE_OPTIMIZER_H
#define GF_ROUTE_OPTIMIZER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define GF_RO_MAX_WAYPOINTS     64
#define GF_RO_MAX_VEHICLES      32
#define GF_RO_MAX_ZONES         128

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_RO_OK = 0,
    GF_RO_ERROR_NOT_INITIALIZED,
    GF_RO_ERROR_NULL_PTR,
    GF_RO_ERROR_NO_PATH,
    GF_RO_ERROR_CAPACITY_EXCEEDED,
    GF_RO_ERROR_TIME_WINDOW,
    GF_RO_ERROR_MAP_INVALID,
    GF_RO_ERROR_MAX_WAYPOINTS,
    GF_RO_WARN_SUBOPTIMAL
} gf_ro_status_t;

typedef enum {
    GF_RO_ALGO_ASTAR,             /**< A* pathfinding */
    GF_RO_ALGO_DIJKSTRA,          /**< Dijkstra shortest path */
    GF_RO_ALGO_GREEDY,            /**< Nearest neighbor greedy */
    GF_RO_ALGO_GENETIC,           /**< Genetic algorithm */
    GF_RO_ALGO_SIMULATED_ANNEALING /**< Simulated annealing */
} gf_ro_algorithm_t;

typedef enum {
    GF_RO_OBJECTIVE_DISTANCE,     /**< Minimize total distance */
    GF_RO_OBJECTIVE_TIME,         /**< Minimize total time */
    GF_RO_OBJECTIVE_COST,         /**< Minimize operational cost */
    GF_RO_OBJECTIVE_BALANCED      /**< Balance multiple objectives */
} gf_ro_objective_t;

typedef struct {
    float x;                      /**< X coordinate */
    float y;                      /**< Y coordinate */
    uint8_t floor_id;             /**< Floor/level ID */
} gf_ro_point_t;

typedef struct {
    uint32_t waypoint_id;         /**< Waypoint identifier */
    gf_ro_point_t location;       /**< Waypoint location */
    uint32_t service_time_sec;    /**< Service time at waypoint */
    uint32_t time_window_start;   /**< Earliest arrival (epoch) */
    uint32_t time_window_end;     /**< Latest arrival (epoch) */
    float demand_units;           /**< Demand/capacity units */
    uint8_t priority;             /**< Priority (0-255) */
} gf_ro_waypoint_t;

typedef struct {
    uint16_t vehicle_id;          /**< Vehicle identifier */
    gf_ro_point_t depot;          /**< Home depot location */
    float capacity_units;         /**< Vehicle capacity */
    float speed_mps;              /**< Average speed (m/s) */
    uint32_t work_start;          /**< Shift start (epoch) */
    uint32_t work_end;            /**< Shift end (epoch) */
    float cost_per_km;            /**< Operating cost per km */
} gf_ro_vehicle_t;

typedef struct {
    gf_ro_waypoint_t* waypoints;  /**< Ordered waypoints */
    uint8_t waypoint_count;       /**< Number of waypoints */
    float total_distance_m;       /**< Total route distance */
    uint32_t total_time_sec;      /**< Total route time */
    float total_cost;             /**< Total route cost */
    uint32_t eta_arrival;         /**< Estimated arrival (epoch) */
} gf_ro_route_t;

typedef struct {
    uint32_t routes_computed;     /**< Total routes computed */
    uint32_t optimizations_run;   /**< Optimizations performed */
    float avg_improvement_pct;    /**< Average improvement */
    uint32_t avg_compute_time_ms; /**< Average compute time */
} gf_ro_stats_t;

typedef void (*gf_ro_progress_cb_t)(uint8_t progress_pct, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_ro_status_t gf_ro_init(gf_ro_algorithm_t algorithm, gf_ro_objective_t objective);
void gf_ro_shutdown(void);
gf_ro_status_t gf_ro_add_waypoint(const gf_ro_waypoint_t* waypoint);
gf_ro_status_t gf_ro_remove_waypoint(uint32_t waypoint_id);
gf_ro_status_t gf_ro_clear_waypoints(void);
gf_ro_status_t gf_ro_set_vehicle(const gf_ro_vehicle_t* vehicle);
gf_ro_status_t gf_ro_compute_route(gf_ro_route_t* route);
gf_ro_status_t gf_ro_optimize_route(gf_ro_route_t* route, uint32_t timeout_ms);
gf_ro_status_t gf_ro_recalculate_from(uint8_t waypoint_index, gf_ro_route_t* route);
gf_ro_status_t gf_ro_find_path(const gf_ro_point_t* start, const gf_ro_point_t* end,
                                gf_ro_point_t* path, uint16_t max_points, uint16_t* count);
gf_ro_status_t gf_ro_register_progress_callback(gf_ro_progress_cb_t callback, void* user_data);
gf_ro_status_t gf_ro_get_stats(gf_ro_stats_t* stats);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROUTE_OPTIMIZER_H */
