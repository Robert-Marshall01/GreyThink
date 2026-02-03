/**
 * @file pod_control.h
 * @brief Pod Control Loop Module for Hyperloop Systems
 * 
 * INDUSTRY RELEVANCE:
 * Hyperloop pods require precise control of acceleration, levitation, braking,
 * and cabin pressure to ensure passenger safety and comfort at speeds exceeding
 * 1000 km/h. This module implements PID-based control loops for propulsion,
 * magnetic levitation, and aerodynamic systems.
 * 
 * Applications:
 * - Hyperloop passenger pods
 * - High-speed maglev trains
 * - Vacuum tube freight capsules
 * - Urban air mobility pod control
 * - Autonomous transit vehicles
 * 
 * Standards: EN 50126 (Railway Safety), IEC 62278, ISO 26262 (adapted for transit)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_POD_CONTROL_H
#define GF_POD_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define POD_MAX_UNITS               16      /* Maximum pods in system */
#define POD_CONTROL_RATE_HZ         1000    /* Control loop frequency */

/* Operating limits */
#define POD_MAX_SPEED_MPS           350.0f  /* Maximum speed (~1260 km/h) */
#define POD_MAX_ACCEL_MPS2          5.0f    /* Maximum acceleration (0.5g) */
#define POD_MAX_DECEL_MPS2          10.0f   /* Maximum braking (1g) */
#define POD_LEVITATION_GAP_MM       15.0f   /* Nominal levitation gap */
#define POD_CABIN_PRESSURE_KPA      101.3f  /* Sea-level cabin pressure */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Pod operational state */
typedef enum {
    POD_STATE_IDLE,             /* Docked at station */
    POD_STATE_BOARDING,         /* Passengers boarding */
    POD_STATE_SEALED,           /* Door sealed, ready */
    POD_STATE_LEVITATING,       /* Achieving levitation */
    POD_STATE_ACCELERATING,     /* Accelerating to cruise */
    POD_STATE_CRUISING,         /* Constant velocity cruise */
    POD_STATE_DECELERATING,     /* Braking for station */
    POD_STATE_LANDING,          /* Reducing levitation */
    POD_STATE_DOCKING,          /* Docking at station */
    POD_STATE_EMERGENCY,        /* Emergency stop active */
    POD_STATE_MAINTENANCE       /* Maintenance mode */
} pod_state_t;

/** Pod fault codes */
typedef enum {
    POD_FAULT_NONE = 0,
    POD_FAULT_LEVITATION,       /* Levitation system failure */
    POD_FAULT_PROPULSION,       /* Propulsion failure */
    POD_FAULT_BRAKING,          /* Braking system failure */
    POD_FAULT_PRESSURE,         /* Cabin pressure anomaly */
    POD_FAULT_GUIDANCE,         /* Guidance/navigation failure */
    POD_FAULT_POWER,            /* Power system failure */
    POD_FAULT_COMMS,            /* Communication loss */
    POD_FAULT_DOOR_SEAL,        /* Door seal integrity */
    POD_FAULT_THERMAL,          /* Thermal management failure */
    POD_FAULT_TUBE_BREACH       /* Tube pressure anomaly */
} pod_fault_t;

/** Propulsion type */
typedef enum {
    PROPULSION_LINEAR_INDUCTION,    /* Linear induction motor */
    PROPULSION_LINEAR_SYNCHRONOUS,  /* Linear synchronous motor */
    PROPULSION_MAGNETIC_RAIL,       /* Electromagnetic rail */
    PROPULSION_AIR_COMPRESSOR       /* Compressed air bearing */
} propulsion_type_t;

/** Pod kinematics */
typedef struct {
    float position_m;           /* Position along track */
    float velocity_mps;         /* Current velocity */
    float acceleration_mps2;    /* Current acceleration */
    float heading_deg;          /* Track heading */
    float levitation_gap_mm;    /* Current levitation height */
} pod_kinematics_t;

/** Pod cabin environment */
typedef struct {
    float pressure_kpa;
    float temperature_c;
    float humidity_pct;
    float o2_pct;
    float co2_ppm;
    float noise_db;
    float vibration_g;
} pod_cabin_t;

/** Pod health metrics */
typedef struct {
    float battery_soc_pct;
    float motor_temp_c;
    float brake_pad_pct;
    float levitation_current_a;
    uint32_t distance_km;
    uint32_t trip_count;
} pod_health_t;

/** Pod configuration */
typedef struct {
    uint8_t pod_id;
    propulsion_type_t propulsion;
    uint8_t passenger_capacity;
    float max_speed_mps;
    float cruise_speed_mps;
    bool autonomous_mode;
} pod_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int pod_control_init(const pod_config_t *config);
void pod_control_shutdown(void);

int pod_set_target_speed(uint8_t pod_id, float speed_mps);
int pod_set_target_position(uint8_t pod_id, float position_m);
int pod_emergency_stop(uint8_t pod_id);
int pod_emergency_brake(uint8_t pod_id);

int pod_get_state(uint8_t pod_id, pod_state_t *state);
int pod_get_fault(uint8_t pod_id, pod_fault_t *fault);
int pod_get_kinematics(uint8_t pod_id, pod_kinematics_t *kinematics);
int pod_get_cabin(uint8_t pod_id, pod_cabin_t *cabin);
int pod_get_health(uint8_t pod_id, pod_health_t *health);

int pod_seal_door(uint8_t pod_id);
int pod_open_door(uint8_t pod_id);
int pod_engage_levitation(uint8_t pod_id);
int pod_disengage_levitation(uint8_t pod_id);

void pod_control_update(uint32_t elapsed_ms);

#endif /* GF_POD_CONTROL_H */
