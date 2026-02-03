/**
 * @file drill_control.h
 * @brief Mining Drill Control Loop Interface
 * 
 * @details
 * This module provides control systems for mining drill operations,
 * including rotary and percussive drilling in underground and
 * surface mining applications.
 * 
 * INDUSTRY RELEVANCE:
 * - Mining Giants: Caterpillar, Komatsu, Sandvik autonomous drills
 * - Oil & Gas: Schlumberger, Halliburton drilling systems
 * - Tunneling: Boring machines (Herrenknecht, Robbins)
 * - Geothermal: Deep drilling for energy extraction
 * - Space Resource: Asteroid and lunar mining (NASA RASSOR)
 * 
 * AUTOMATION LEVELS:
 * - Teleoperation: Remote operator control
 * - Semi-autonomous: Assisted drilling with safety overrides
 * - Fully autonomous: AI-driven operation cycles
 * 
 * SAFETY COMPLIANCE:
 * - MSHA (Mine Safety and Health Administration)
 * - ISO 19296: Mining machinery safety
 * - IEC 61508: Functional safety
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_DRILL_CONTROL_H
#define GF_DRILL_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum drill depth (meters) */
#define GF_DRILL_MAX_DEPTH_M            5000

/** Maximum rotation speed (RPM) */
#define GF_DRILL_MAX_RPM                200

/** Maximum thrust force (kN) */
#define GF_DRILL_MAX_THRUST_KN          500

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Drill type
 */
typedef enum {
    GF_DRILL_ROTARY,              /**< Rotary drill */
    GF_DRILL_PERCUSSIVE,          /**< Percussive/hammer */
    GF_DRILL_ROTARY_PERCUSSIVE,   /**< Combined */
    GF_DRILL_DTH,                 /**< Down-the-hole */
    GF_DRILL_CORE                 /**< Core sampling */
} gf_drill_type_t;

/**
 * @brief Drill state
 */
typedef enum {
    GF_DRILL_STATE_IDLE,          /**< Idle */
    GF_DRILL_STATE_POSITIONING,   /**< Positioning */
    GF_DRILL_STATE_DRILLING,      /**< Active drilling */
    GF_DRILL_STATE_RETRACTING,    /**< Bit retraction */
    GF_DRILL_STATE_ROD_CHANGE,    /**< Rod change */
    GF_DRILL_STATE_FAULT          /**< Fault condition */
} gf_drill_state_t;

/**
 * @brief Rock formation type
 */
typedef enum {
    GF_ROCK_SOFT,                 /**< Soft rock/soil */
    GF_ROCK_MEDIUM,               /**< Medium hardite */
    GF_ROCK_HARD,                 /**< Hard rock */
    GF_ROCK_ABRASIVE,             /**< Abrasive formation */
    GF_ROCK_FRACTURED             /**< Fractured zone */
} gf_rock_type_t;

/**
 * @brief Drill parameters
 */
typedef struct {
    float rpm;                    /**< Rotation speed */
    float thrust_kn;              /**< Thrust force */
    float torque_knm;             /**< Torque */
    float depth_m;                /**< Current depth */
    float rate_m_hr;              /**< Penetration rate */
    float vibration_g;            /**< Vibration level */
    float bit_wear_pct;           /**< Bit wear estimate */
} gf_drill_params_t;

/**
 * @brief Drill setpoints
 */
typedef struct {
    float target_rpm;             /**< Target RPM */
    float target_thrust_kn;       /**< Target thrust */
    float max_torque_knm;         /**< Torque limit */
    float target_depth_m;         /**< Target depth */
    gf_rock_type_t rock_type;     /**< Formation type */
} gf_drill_setpoints_t;

/**
 * @brief Drill configuration
 */
typedef struct {
    gf_drill_type_t type;         /**< Drill type */
    float rod_length_m;           /**< Rod length */
    float bit_diameter_mm;        /**< Bit diameter */
    bool enable_adaptive;         /**< Adaptive drilling */
} gf_drill_config_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize drill control
 * @param config Drill configuration
 * @return 0 on success
 */
int gf_drill_init(const gf_drill_config_t* config);

/**
 * @brief Shutdown drill control
 */
void gf_drill_shutdown(void);

/**
 * @brief Start drilling
 * @param setpoints Target parameters
 * @return 0 on success
 */
int gf_drill_start(const gf_drill_setpoints_t* setpoints);

/**
 * @brief Stop drilling
 */
void gf_drill_stop(void);

/**
 * @brief E-stop (emergency)
 */
void gf_drill_estop(void);

/**
 * @brief Get current parameters
 * @param params Output parameters
 * @return 0 on success
 */
int gf_drill_get_params(gf_drill_params_t* params);

/**
 * @brief Get drill state
 * @return Current state
 */
gf_drill_state_t gf_drill_get_state(void);

/**
 * @brief Process drill control (call periodically)
 * @param delta_ms Time since last call
 */
void gf_drill_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_DRILL_CONTROL_H */
