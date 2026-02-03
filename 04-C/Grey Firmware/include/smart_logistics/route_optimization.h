/**
 * @file route_optimization.h
 * @brief Logistics Route Optimization Engine Interface
 * 
 * INDUSTRY RELEVANCE:
 * Route optimization reduces costs by 10-30% in logistics operations:
 * - Last-mile delivery efficiency
 * - Multi-stop routing (TSP/VRP variants)
 * - Dynamic rerouting for traffic/conditions
 * - Fleet utilization balancing
 * 
 * Used by FedEx, UPS, Amazon Logistics with embedded edge compute
 * for real-time optimization. Embedded engineers contribute:
 * - Constrained optimization algorithms
 * - GPS/IMU sensor fusion
 * - Low-latency decision loops
 * - Power-efficient computation
 * 
 * ALGORITHMS:
 * - Clarke-Wright Savings
 * - Nearest Neighbor heuristics
 * - Genetic algorithms for VRP
 * - A* for warehouse pathfinding
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ROUTE_OPTIMIZATION_H
#define GF_ROUTE_OPTIMIZATION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define ROUTE_MAX_WAYPOINTS       1024 /**< Maximum waypoints per route */
#define ROUTE_MAX_VEHICLES        256  /**< Maximum fleet vehicles */
#define ROUTE_MAX_CONSTRAINTS     64   /**< Maximum constraints */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Route optimization objective */
typedef enum {
    OBJECTIVE_MIN_DISTANCE,
    OBJECTIVE_MIN_TIME,
    OBJECTIVE_MIN_FUEL,
    OBJECTIVE_MIN_COST,
    OBJECTIVE_BALANCED
} route_objective_t;

/** Vehicle type */
typedef enum {
    VEHICLE_TRUCK,
    VEHICLE_VAN,
    VEHICLE_DRONE,
    VEHICLE_BIKE,
    VEHICLE_ROBOT_AMR
} vehicle_type_t;

/** Constraint type */
typedef enum {
    CONSTRAINT_TIME_WINDOW,
    CONSTRAINT_CAPACITY_WEIGHT,
    CONSTRAINT_CAPACITY_VOLUME,
    CONSTRAINT_VEHICLE_TYPE,
    CONSTRAINT_DRIVER_HOURS,
    CONSTRAINT_ZONE_RESTRICTION
} constraint_type_t;

/** GPS coordinate */
typedef struct {
    double latitude;
    double longitude;
    float altitude_m;
} gps_coord_t;

/** Waypoint definition */
typedef struct {
    uint32_t id;
    gps_coord_t coord;
    uint32_t time_window_start;  /**< Unix timestamp */
    uint32_t time_window_end;
    float service_time_min;
    float demand_weight_kg;
    float demand_volume_m3;
    uint8_t priority;
} route_waypoint_t;

/** Route solution */
typedef struct {
    uint32_t vehicle_id;
    uint32_t waypoint_ids[ROUTE_MAX_WAYPOINTS];
    uint16_t waypoint_count;
    float total_distance_km;
    float total_time_min;
    float total_cost;
    float utilization_pct;
} route_solution_t;

/** Optimization result */
typedef struct {
    route_solution_t *solutions;
    uint16_t vehicle_count;
    float total_cost;
    float savings_pct;      /**< Vs. naive routing */
    uint32_t compute_time_ms;
} optimization_result_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int route_optimizer_init(route_objective_t objective);
int route_add_waypoint(const route_waypoint_t *waypoint);
int route_add_vehicle(uint32_t vehicle_id, vehicle_type_t type,
                      float capacity_kg, float capacity_m3);
int route_add_constraint(constraint_type_t type, const void *params);
int route_optimize(optimization_result_t *result);
int route_reoptimize_dynamic(uint32_t vehicle_id, const gps_coord_t *current_pos);
int route_get_next_waypoint(uint32_t vehicle_id, route_waypoint_t *waypoint);
float route_estimate_eta(uint32_t vehicle_id, uint32_t waypoint_id);
void route_optimizer_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROUTE_OPTIMIZATION_H */
