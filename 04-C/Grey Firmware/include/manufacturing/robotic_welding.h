/**
 * @file robotic_welding.h
 * @brief Robotic Welding Controller Interface
 * 
 * INDUSTRY RELEVANCE:
 * Robotic welding is fundamental to modern manufacturing:
 * - Automotive body assembly (MIG/MAG welding)
 * - Aerospace structural joining (TIG, friction stir)
 * - Shipbuilding and heavy equipment fabrication
 * - Pipeline construction and maintenance
 * - Electronics and micro-welding applications
 * - Additive manufacturing (WAAM - Wire Arc Additive)
 * 
 * This controller demonstrates expertise in:
 * - Weld parameter optimization (current, voltage, speed)
 * - Seam tracking and adaptive path correction
 * - Multi-pass weld sequencing
 * - Heat input management and distortion control
 * - Quality monitoring via arc signature analysis
 * - Safety interlocks and fume extraction control
 */

#ifndef GF_ROBOTIC_WELDING_H
#define GF_ROBOTIC_WELDING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_WELD_MAX_PASSES          20
#define GF_WELD_MAX_SEAMS           100
#define GF_WELD_MAX_SCHEDULES       50

typedef enum {
    GF_WELD_OK = 0,
    GF_WELD_ERROR_NOT_INITIALIZED,
    GF_WELD_ERROR_ARC_FAULT,
    GF_WELD_ERROR_WIRE_FAULT,
    GF_WELD_ERROR_GAS_FAULT,
    GF_WELD_ERROR_OVERCURRENT,
    GF_WELD_ERROR_SEAM_LOST,
    GF_WELD_ERROR_COLLISION,
    GF_WELD_ERROR_QUALITY_FAIL,
    GF_WELD_WARN_SPATTER_HIGH
} gf_weld_status_t;

typedef enum {
    GF_WELD_PROCESS_MIG,            /* Metal Inert Gas */
    GF_WELD_PROCESS_MAG,            /* Metal Active Gas */
    GF_WELD_PROCESS_TIG,            /* Tungsten Inert Gas */
    GF_WELD_PROCESS_SPOT,           /* Resistance spot welding */
    GF_WELD_PROCESS_LASER,          /* Laser welding */
    GF_WELD_PROCESS_PLASMA,         /* Plasma arc welding */
    GF_WELD_PROCESS_FSW             /* Friction stir welding */
} gf_weld_process_t;

typedef enum {
    GF_WELD_JOINT_BUTT,
    GF_WELD_JOINT_LAP,
    GF_WELD_JOINT_CORNER,
    GF_WELD_JOINT_TEE,
    GF_WELD_JOINT_FILLET
} gf_weld_joint_t;

typedef struct {
    float current_a;                /* Welding current */
    float voltage_v;                /* Arc voltage */
    float wire_feed_mpm;            /* Wire feed rate m/min */
    float travel_speed_mmps;        /* Travel speed mm/s */
    float gas_flow_lpm;             /* Shielding gas flow L/min */
    float stick_out_mm;             /* Contact tip to work distance */
    float weave_amplitude_mm;       /* Weave pattern width */
    float weave_frequency_hz;       /* Weave frequency */
} gf_weld_params_t;

typedef struct {
    float arc_current;
    float arc_voltage;
    float heat_input_kjmm;          /* kJ/mm heat input */
    float wire_consumption_m;
    float gas_consumption_l;
    uint32_t arc_on_time_ms;
    float quality_score;            /* 0-100 weld quality */
    uint16_t defect_count;
} gf_weld_stats_t;

typedef struct {
    uint16_t seam_id;
    gf_weld_joint_t joint_type;
    float start_pos[6];             /* X,Y,Z,Rx,Ry,Rz */
    float end_pos[6];
    uint8_t num_passes;
    gf_weld_params_t pass_params[GF_WELD_MAX_PASSES];
} gf_weld_seam_t;

typedef struct {
    gf_weld_process_t process;
    float wire_diameter_mm;
    uint8_t wire_type;              /* Material code */
    uint8_t gas_type;               /* Shielding gas code */
    bool seam_tracking_enabled;
    bool adaptive_control;
    float max_current_a;
    float max_heat_input;
} gf_weld_config_t;

gf_weld_status_t gf_weld_init(const gf_weld_config_t* config);
void gf_weld_shutdown(void);
gf_weld_status_t gf_weld_start_arc(void);
gf_weld_status_t gf_weld_stop_arc(void);
gf_weld_status_t gf_weld_set_params(const gf_weld_params_t* params);
gf_weld_status_t gf_weld_execute_seam(const gf_weld_seam_t* seam);
gf_weld_status_t gf_weld_get_stats(gf_weld_stats_t* stats);
gf_weld_status_t gf_weld_track_seam(float* correction);
gf_weld_status_t gf_weld_get_quality(float* score, uint16_t* defects);
gf_weld_status_t gf_weld_crater_fill(void);
gf_weld_status_t gf_weld_wire_stick_recovery(void);
bool gf_weld_arc_established(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROBOTIC_WELDING_H */
