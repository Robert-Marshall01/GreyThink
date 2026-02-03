/**
 * @file rover_motor.h
 * @brief Planetary Rover Motor Controller Interface
 * 
 * INDUSTRY RELEVANCE:
 * Space robotics motor control is critical for:
 * - Mars/Lunar rover wheel and arm actuation (NASA/ESA/JAXA)
 * - Asteroid sample collection mechanisms
 * - Satellite servicing and orbital manipulation
 * - Space station robotic arms (Canadarm, ERA)
 * - Planetary drilling and sample handling
 * - Extreme environment survivability (-120°C to +120°C)
 * 
 * This controller demonstrates expertise in:
 * - Radiation-tolerant motor control algorithms
 * - Position/velocity control with limited sensor feedback
 * - Power-constrained motion planning
 * - Fault-tolerant operation (continue on partial failure)
 * - Communication latency handling (minutes of delay)
 * - Thermal management in vacuum
 */

#ifndef GF_ROVER_MOTOR_H
#define GF_ROVER_MOTOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_ROVER_MAX_MOTORS         12      /* Max motor channels */
#define GF_ROVER_MAX_JOINTS         7       /* Max arm joints */
#define GF_ROVER_WHEEL_COUNT        6       /* Standard 6-wheel config */

typedef enum {
    GF_ROVER_OK = 0,
    GF_ROVER_ERROR_NOT_INITIALIZED,
    GF_ROVER_ERROR_MOTOR_FAULT,
    GF_ROVER_ERROR_OVERCURRENT,
    GF_ROVER_ERROR_OVERTEMP,
    GF_ROVER_ERROR_POSITION_ERROR,
    GF_ROVER_ERROR_COMM_TIMEOUT,
    GF_ROVER_ERROR_POWER_LOW,
    GF_ROVER_WARN_DEGRADED_MODE
} gf_rover_status_t;

typedef enum {
    GF_ROVER_MOTOR_WHEEL,
    GF_ROVER_MOTOR_STEERING,
    GF_ROVER_MOTOR_ARM_JOINT,
    GF_ROVER_MOTOR_GRIPPER,
    GF_ROVER_MOTOR_CAMERA_PAN,
    GF_ROVER_MOTOR_CAMERA_TILT,
    GF_ROVER_MOTOR_DRILL,
    GF_ROVER_MOTOR_SAMPLE_HANDLER
} gf_rover_motor_type_t;

typedef enum {
    GF_ROVER_MODE_POSITION,         /* Position control */
    GF_ROVER_MODE_VELOCITY,         /* Velocity control */
    GF_ROVER_MODE_TORQUE,           /* Torque/current control */
    GF_ROVER_MODE_IMPEDANCE         /* Compliant manipulation */
} gf_rover_control_mode_t;

typedef struct {
    uint8_t motor_id;
    gf_rover_motor_type_t type;
    float position_deg;             /* Current position */
    float velocity_dps;             /* Degrees per second */
    float current_ma;               /* Motor current */
    float temperature_c;            /* Motor temperature */
    uint8_t health;                 /* 0-100 health score */
    bool fault;
} gf_rover_motor_state_t;

typedef struct {
    float position_deg;
    float velocity_dps;
    float max_current_ma;
    float acceleration_limit;
    gf_rover_control_mode_t mode;
} gf_rover_motor_cmd_t;

typedef struct {
    uint8_t motor_count;
    float supply_voltage;
    float max_power_w;
    float operating_temp_min;
    float operating_temp_max;
    bool redundant_encoders;
    bool thermal_protection;
} gf_rover_config_t;

gf_rover_status_t gf_rover_init(const gf_rover_config_t* config);
void gf_rover_shutdown(void);
gf_rover_status_t gf_rover_command(uint8_t motor_id, const gf_rover_motor_cmd_t* cmd);
gf_rover_status_t gf_rover_get_state(uint8_t motor_id, gf_rover_motor_state_t* state);
gf_rover_status_t gf_rover_drive(float left_speed, float right_speed);
gf_rover_status_t gf_rover_stop_all(void);
gf_rover_status_t gf_rover_arm_move(const float* joint_positions, uint8_t num_joints);
gf_rover_status_t gf_rover_home_motor(uint8_t motor_id);
gf_rover_status_t gf_rover_calibrate(uint8_t motor_id);
gf_rover_status_t gf_rover_enter_safe_mode(void);
float gf_rover_get_power_budget(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROVER_MOTOR_H */
