/**
 * @file rubble_actuator.h
 * @brief Rubble-Clearing Actuator Driver for Disaster Robotics
 * 
 * INDUSTRY RELEVANCE:
 * Disaster response robots require powerful actuators for debris removal,
 * victim extraction, and structural stabilization. This driver interfaces
 * with hydraulic/pneumatic actuators, grippers, and cutting tools for
 * autonomous and teleoperated rubble-clearing operations.
 * 
 * Applications:
 * - Earthquake search and rescue
 * - Building collapse response
 * - Bomb disposal robots
 * - Industrial demolition
 * - Underground mining rescue
 * 
 * Standards: ISO 13482 (Service Robots Safety), EN 13849 (Safety-Related Parts)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_RUBBLE_ACTUATOR_H
#define GF_RUBBLE_ACTUATOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define RUBBLE_MAX_ACTUATORS        8       /* Maximum actuator channels */
#define RUBBLE_MAX_FORCE_KN         50.0f   /* Maximum force capability */
#define RUBBLE_CONTROL_RATE_HZ      100     /* Control loop frequency */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Actuator technology */
typedef enum {
    ACTUATOR_HYDRAULIC,         /* Hydraulic cylinder */
    ACTUATOR_PNEUMATIC,         /* Pneumatic cylinder */
    ACTUATOR_ELECTRIC_LINEAR,   /* Electric linear actuator */
    ACTUATOR_ELECTRIC_ROTARY,   /* Electric servo motor */
    ACTUATOR_CHAIN_DRIVE        /* Chain/track drive */
} actuator_type_t;

/** End effector type */
typedef enum {
    EFFECTOR_GRIPPER,           /* Parallel gripper */
    EFFECTOR_CLAW,              /* Multi-finger claw */
    EFFECTOR_BUCKET,            /* Excavator bucket */
    EFFECTOR_SHEARS,            /* Cutting shears */
    EFFECTOR_SPREADER,          /* Hydraulic spreader */
    EFFECTOR_DRILL,             /* Rotary drill */
    EFFECTOR_VACUUM             /* Suction/vacuum */
} effector_type_t;

/** Actuator state */
typedef enum {
    RUBBLE_ACT_IDLE,
    RUBBLE_ACT_EXTENDING,
    RUBBLE_ACT_RETRACTING,
    RUBBLE_ACT_GRIPPING,
    RUBBLE_ACT_RELEASING,
    RUBBLE_ACT_CUTTING,
    RUBBLE_ACT_STALLED,
    RUBBLE_ACT_FAULT
} actuator_state_t;

/** Actuator reading */
typedef struct {
    uint8_t channel_id;
    actuator_type_t type;
    effector_type_t effector;
    actuator_state_t state;
    float position_mm;
    float velocity_mms;
    float force_kn;
    float current_a;
    float temperature_c;
    uint32_t timestamp_ms;
    bool valid;
} actuator_reading_t;

/** Force feedback */
typedef struct {
    float grip_force_kn;
    float push_force_kn;
    float torque_nm;
    bool object_detected;
    bool stall_detected;
    bool overload_detected;
} force_feedback_t;

/** Actuator configuration */
typedef struct {
    uint8_t channel_id;
    actuator_type_t type;
    effector_type_t effector;
    float max_position_mm;
    float max_force_kn;
    float max_velocity_mms;
    bool force_limiting;
} actuator_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int rubble_actuator_init(void);
void rubble_actuator_shutdown(void);

int rubble_actuator_configure(const actuator_config_t *config);
int rubble_actuator_extend(uint8_t channel_id, float position_mm, float velocity_mms);
int rubble_actuator_retract(uint8_t channel_id, float velocity_mms);
int rubble_actuator_grip(uint8_t channel_id, float force_kn);
int rubble_actuator_release(uint8_t channel_id);
int rubble_actuator_stop(uint8_t channel_id);
int rubble_actuator_emergency_stop(void);

int rubble_actuator_get_reading(uint8_t channel_id, actuator_reading_t *reading);
int rubble_actuator_get_force_feedback(uint8_t channel_id, force_feedback_t *feedback);

void rubble_actuator_update(uint32_t elapsed_ms);

#endif /* GF_RUBBLE_ACTUATOR_H */
