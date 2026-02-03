/**
 * @file cnc_interface.h
 * @brief CNC Machine Interface Driver
 * 
 * INDUSTRY RELEVANCE:
 * CNC (Computer Numerical Control) integration is essential for:
 * - Precision machining and milling operations
 * - Multi-axis motion coordination
 * - Tool path optimization and execution
 * - Real-time process monitoring and quality control
 * - Industry 4.0 smart factory integration
 * - Adaptive machining based on sensor feedback
 * 
 * This interface demonstrates expertise in:
 * - G-code and M-code parsing and execution
 * - Interpolation algorithms (linear, circular, spline)
 * - Spindle speed and feed rate control
 * - Coordinate system transformations
 * - Tool offset and compensation
 * - Real-time position feedback and servo control
 */

#ifndef GF_CNC_INTERFACE_H
#define GF_CNC_INTERFACE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_CNC_MAX_AXES             9       /* X,Y,Z,A,B,C,U,V,W */
#define GF_CNC_MAX_TOOLS            100
#define GF_CNC_PROGRAM_BUFFER       8192

typedef enum {
    GF_CNC_OK = 0,
    GF_CNC_ERROR_NOT_INITIALIZED,
    GF_CNC_ERROR_AXIS_FAULT,
    GF_CNC_ERROR_SPINDLE_FAULT,
    GF_CNC_ERROR_LIMIT_SWITCH,
    GF_CNC_ERROR_FOLLOWING_ERROR,
    GF_CNC_ERROR_TOOLCHANGE_FAIL,
    GF_CNC_ERROR_ESTOP,
    GF_CNC_ERROR_PROGRAM_ERROR,
    GF_CNC_WARN_TOOL_WEAR
} gf_cnc_status_t;

typedef enum {
    GF_CNC_MODE_MANUAL,
    GF_CNC_MODE_MDI,                /* Manual Data Input */
    GF_CNC_MODE_AUTO,               /* Program execution */
    GF_CNC_MODE_JOG,
    GF_CNC_MODE_HOMING
} gf_cnc_mode_t;

typedef enum {
    GF_CNC_INTERP_LINEAR,
    GF_CNC_INTERP_CIRCULAR_CW,
    GF_CNC_INTERP_CIRCULAR_CCW,
    GF_CNC_INTERP_SPLINE,
    GF_CNC_INTERP_RAPID
} gf_cnc_interpolation_t;

typedef struct {
    float position[GF_CNC_MAX_AXES];    /* Current position */
    float velocity[GF_CNC_MAX_AXES];    /* Current velocity */
    float target[GF_CNC_MAX_AXES];      /* Target position */
    bool axis_enabled[GF_CNC_MAX_AXES];
    bool axis_fault[GF_CNC_MAX_AXES];
    bool axis_homed[GF_CNC_MAX_AXES];
} gf_cnc_axis_state_t;

typedef struct {
    uint8_t tool_id;
    float length_offset;
    float radius_offset;
    float wear_offset;
    uint32_t usage_time_s;
    uint16_t usage_count;
    bool broken;
} gf_cnc_tool_t;

typedef struct {
    float spindle_rpm;
    float spindle_load_pct;
    float feed_rate_mmpm;
    float feed_override_pct;
    float spindle_override_pct;
    uint8_t current_tool;
    uint32_t program_line;
    gf_cnc_mode_t mode;
    bool program_running;
    bool coolant_on;
} gf_cnc_state_t;

typedef struct {
    uint8_t num_axes;
    float max_velocity[GF_CNC_MAX_AXES];
    float max_accel[GF_CNC_MAX_AXES];
    float max_jerk[GF_CNC_MAX_AXES];
    float axis_range_min[GF_CNC_MAX_AXES];
    float axis_range_max[GF_CNC_MAX_AXES];
    float spindle_max_rpm;
    bool use_tool_changer;
} gf_cnc_config_t;

gf_cnc_status_t gf_cnc_init(const gf_cnc_config_t* config);
void gf_cnc_shutdown(void);
gf_cnc_status_t gf_cnc_home_axis(uint8_t axis);
gf_cnc_status_t gf_cnc_home_all(void);
gf_cnc_status_t gf_cnc_jog(uint8_t axis, float distance_mm, float feed_mmpm);
gf_cnc_status_t gf_cnc_move(float* target, gf_cnc_interpolation_t interp, float feed);
gf_cnc_status_t gf_cnc_set_spindle(float rpm, bool clockwise);
gf_cnc_status_t gf_cnc_tool_change(uint8_t tool_id);
gf_cnc_status_t gf_cnc_load_program(const char* gcode, uint16_t length);
gf_cnc_status_t gf_cnc_run_program(void);
gf_cnc_status_t gf_cnc_pause_program(void);
gf_cnc_status_t gf_cnc_stop_program(void);
gf_cnc_status_t gf_cnc_get_state(gf_cnc_state_t* state);
gf_cnc_status_t gf_cnc_get_axis_state(gf_cnc_axis_state_t* axis);
gf_cnc_status_t gf_cnc_estop(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CNC_INTERFACE_H */
