/**
 * @file crane_sensor.h
 * @brief Container Crane Sensor Driver for Smart Ports & Shipping
 * 
 * INDUSTRY RELEVANCE:
 * Global container shipping moves $14 trillion in goods annually. Modern port
 * automation relies on precise crane positioning, load monitoring, and collision
 * avoidance. This driver demonstrates expertise in:
 * - Heavy industrial sensor integration (load cells, encoders, LiDAR)
 * - Safety-critical real-time feedback for automated gantry cranes
 * - Integration with Terminal Operating Systems (TOS)
 * 
 * Target applications: Ship-to-Shore (STS) cranes, Rubber-Tired Gantry (RTG)
 * cranes, Automated Stacking Cranes (ASC), port automation systems.
 * 
 * Standards: ISO 10855 (crane safety), IEC 61508 (functional safety)
 */

#ifndef GF_CRANE_SENSOR_H
#define GF_CRANE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Crane Sensor Types                                                         */
/*===========================================================================*/

typedef enum {
    CRANE_TYPE_STS,         /* Ship-to-Shore gantry crane */
    CRANE_TYPE_RTG,         /* Rubber-Tired Gantry crane */
    CRANE_TYPE_ASC,         /* Automated Stacking Crane */
    CRANE_TYPE_REACH,       /* Reach stacker */
    CRANE_TYPE_STRADDLE     /* Straddle carrier */
} crane_type_t;

typedef enum {
    CRANE_AXIS_X,           /* Gantry travel (along quay) */
    CRANE_AXIS_Y,           /* Trolley travel (cross quay) */
    CRANE_AXIS_Z,           /* Hoist (vertical) */
    CRANE_AXIS_SPREADER     /* Spreader rotation */
} crane_axis_t;

typedef struct {
    float position_m;       /* Position in meters */
    float velocity_mps;     /* Velocity in m/s */
    float acceleration;     /* Acceleration in m/s² */
    uint32_t encoder_ticks; /* Raw encoder count */
    bool limit_min;         /* At minimum limit switch */
    bool limit_max;         /* At maximum limit switch */
    bool fault;             /* Sensor fault detected */
} crane_axis_state_t;

typedef struct {
    float load_kg;          /* Current load in kg */
    float rated_capacity;   /* Maximum rated load */
    float overload_pct;     /* Percentage of rated capacity */
    bool overload_warning;  /* >90% capacity */
    bool overload_alarm;    /* >100% capacity */
} crane_load_state_t;

typedef struct {
    uint16_t length_ft;     /* Container length (20/40/45) */
    bool locked;            /* Twist locks engaged */
    bool aligned;           /* Container aligned */
    float skew_deg;         /* Skew angle in degrees */
} spreader_state_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int crane_sensor_init(crane_type_t type);
int crane_sensor_shutdown(void);

int crane_get_axis_state(crane_axis_t axis, crane_axis_state_t* state);
int crane_get_load_state(crane_load_state_t* state);
int crane_get_spreader_state(spreader_state_t* state);

int crane_calibrate_axis(crane_axis_t axis);
int crane_set_load_limit(float max_kg);

bool crane_is_safe_to_move(void);
int crane_emergency_stop(void);

#endif /* GF_CRANE_SENSOR_H */
