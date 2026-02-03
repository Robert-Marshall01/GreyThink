/**
 * @file routing_optimization.h
 * @brief Humanitarian Logistics Routing Module
 * 
 * INDUSTRY RELEVANCE:
 * Optimal routing in humanitarian contexts must account for damaged
 * infrastructure, security concerns, access restrictions, and urgency.
 * Organizations use GIS and optimization algorithms to plan convoys
 * and prioritize deliveries based on need assessments.
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_HUMANITARIAN_ROUTING_H
#define GF_HUMANITARIAN_ROUTING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    ROUTE_PRIORITY_ROUTINE,
    ROUTE_PRIORITY_URGENT,
    ROUTE_PRIORITY_EMERGENCY,
    ROUTE_PRIORITY_LIFE_SAVING
} route_priority_t;

typedef struct {
    float lat_start, lon_start;
    float lat_end, lon_end;
    route_priority_t priority;
    float distance_km;
    float est_time_hours;
    bool access_verified;
    bool security_cleared;
} route_plan_t;

int routing_init(void);
int routing_calculate(float lat_from, float lon_from, 
                      float lat_to, float lon_to,
                      route_priority_t priority,
                      route_plan_t *plan);
int routing_update_access(float lat, float lon, bool accessible);
int routing_update_security(float lat, float lon, bool safe);

#ifdef __cplusplus
}
#endif

#endif /* GF_HUMANITARIAN_ROUTING_H */
