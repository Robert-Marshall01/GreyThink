/**
 * @file nuclear_safety.h
 * @brief Nuclear Safety Interlock Module
 * 
 * INDUSTRY RELEVANCE:
 * Reactor Protection Systems (RPS) are the ultimate safety barrier
 * preventing core damage. They must meet strict reliability targets
 * (10^-6 failure probability) with diverse, redundant channels.
 * 
 * This module demonstrates:
 * - 2-out-of-4 voting logic
 * - Automatic reactor trip (SCRAM)
 * - Diverse actuation paths
 * - Surveillance testing support
 * 
 * STANDARDS:
 * - IEEE 603-2018 (Safety systems criteria)
 * - IEEE 7-4.3.2 (Digital computers in safety)
 * - 10 CFR 50.55a (Codes and standards)
 * - NUREG-0800 (NRC review guidance)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_NUCLEAR_SAFETY_H
#define GF_NUCLEAR_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    TRIP_NONE,
    TRIP_HIGH_NEUTRON_FLUX,
    TRIP_HIGH_TEMPERATURE,
    TRIP_LOW_COOLANT_FLOW,
    TRIP_HIGH_PRESSURE,
    TRIP_LOW_LEVEL,
    TRIP_CONTAINMENT,
    TRIP_MANUAL,
    TRIP_TEST
} trip_reason_t;

typedef enum {
    REACTOR_MODE_SHUTDOWN,
    REACTOR_MODE_STARTUP,
    REACTOR_MODE_POWER,
    REACTOR_MODE_REFUELING,
    REACTOR_MODE_MAINTENANCE
} reactor_mode_t;

typedef struct {
    bool trip_active;
    trip_reason_t reason;
    reactor_mode_t mode;
    uint8_t trips_this_cycle;
    uint32_t last_trip_time;
    bool all_rods_inserted;
    float power_percent;
} safety_status_t;

int nuclear_safety_init(void);
int nuclear_safety_arm(void);
int nuclear_safety_disarm(void);
int nuclear_safety_manual_trip(void);
int nuclear_safety_trip_reset(void);
int nuclear_safety_get_status(safety_status_t *status);
bool nuclear_safety_check_2of4(uint8_t trip_type);

#ifdef __cplusplus
}
#endif

#endif /* GF_NUCLEAR_SAFETY_H */
