/**
 * @file process_control.h
 * @brief Orbital Manufacturing Process Control Loop
 *
 * INDUSTRY RELEVANCE:
 * Process control in microgravity requires specialized algorithms accounting
 * for lack of convection, surface tension dominance, and containerless
 * processing. This module implements control loops for various fabrication
 * processes including crystal growth, fiber pulling, and additive manufacturing.
 *
 * MARKET CONTEXT:
 * - Space-grown protein crystals for pharmaceutical research
 * - ZBLAN fiber optic production with superior clarity
 * - Metallic glass alloys impossible under gravity
 * - Organ bioprinting in microgravity
 * - Semiconductor wafer production
 *
 * TECHNICAL APPROACH:
 * - PID control with adaptive gain scheduling
 * - Thermal profile management for crystallization
 * - Material feed rate control for fiber pulling
 * - Atmosphere composition regulation
 * - Contamination detection and response
 *
 * @author Grey Firmware Project
 */

#ifndef GF_PROCESS_CONTROL_H
#define GF_PROCESS_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Fabrication process types
 */
typedef enum {
    FAB_PROCESS_CRYSTAL_GROWTH,  /**< Protein/semiconductor crystals */
    FAB_PROCESS_FIBER_PULLING,   /**< ZBLAN/specialty fibers */
    FAB_PROCESS_METAL_ALLOY,     /**< Metallic glass formation */
    FAB_PROCESS_ADDITIVE,        /**< 3D printing */
    FAB_PROCESS_BIOPRINT,        /**< Tissue/organ printing */
    FAB_PROCESS_SINTERING        /**< Powder metallurgy */
} gf_fab_process_t;

/**
 * @brief Process state
 */
typedef enum {
    PROCESS_STATE_IDLE,
    PROCESS_STATE_SETUP,
    PROCESS_STATE_PREHEAT,
    PROCESS_STATE_RUNNING,
    PROCESS_STATE_COOLDOWN,
    PROCESS_STATE_COMPLETE,
    PROCESS_STATE_FAULT,
    PROCESS_STATE_ABORT
} gf_process_state_t;

/**
 * @brief Process control parameters
 */
typedef struct {
    gf_fab_process_t type;
    float target_temp_c;
    float temp_tolerance_c;
    float feed_rate_mm_s;
    float pull_speed_mm_s;
    float pressure_mbar;
    uint32_t duration_ms;
    bool auto_abort_on_fault;
} gf_process_params_t;

/**
 * @brief Process status
 */
typedef struct {
    gf_process_state_t state;
    float current_temp_c;
    float current_rate_mm_s;
    float yield_percent;
    uint32_t elapsed_ms;
    uint32_t remaining_ms;
    uint8_t fault_code;
    bool interlock_active;
} gf_process_status_t;

/* Function prototypes */
int gf_process_init(void);
int gf_process_configure(const gf_process_params_t *params);
int gf_process_start(void);
int gf_process_pause(void);
int gf_process_resume(void);
int gf_process_abort(void);
int gf_process_get_status(gf_process_status_t *status);
int gf_process_update(uint32_t time_ms);
void gf_process_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PROCESS_CONTROL_H */
