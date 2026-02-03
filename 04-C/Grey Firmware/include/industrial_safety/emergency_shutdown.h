/**
 * @file emergency_shutdown.h
 * @brief Emergency Shutdown System (ESD) Module
 * 
 * INDUSTRY RELEVANCE:
 * Emergency Shutdown Systems (ESD) are safety-critical components in process
 * industries (oil & gas, chemical, nuclear). They must achieve SIL (Safety
 * Integrity Level) 1-3 certification per IEC 61511/61508. ESD systems protect
 * personnel and equipment by bringing processes to a safe state when hazards
 * are detected.
 * 
 * WHY THIS MATTERS:
 * - Safety Integrity Level (SIL) architecture
 * - Redundant voting logic (1oo2, 2oo3)
 * - Fail-safe design principles
 * - Proof test and diagnostic coverage
 * - Response time guarantees
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Safety function architecture
 * - Redundancy and voting patterns
 * - Diagnostic coverage implementation
 * - Safe state management
 */

#ifndef GF_EMERGENCY_SHUTDOWN_H
#define GF_EMERGENCY_SHUTDOWN_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_ESD_MAX_INPUTS           16      /* Maximum inputs */
#define GF_ESD_MAX_OUTPUTS          8       /* Maximum outputs */
#define GF_ESD_MAX_FUNCTIONS        8       /* Maximum safety functions */
#define GF_ESD_RESPONSE_TIME_MS     100     /* Maximum response time */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Safety Integrity Level */
typedef enum {
    GF_SIL_NONE = 0,                /* No SIL requirement */
    GF_SIL_1,                       /* SIL 1 (PFD 10^-1 to 10^-2) */
    GF_SIL_2,                       /* SIL 2 (PFD 10^-2 to 10^-3) */
    GF_SIL_3                        /* SIL 3 (PFD 10^-3 to 10^-4) */
} gf_sil_level_t;

/* Voting architecture */
typedef enum {
    GF_VOTE_1OO1 = 0,               /* Single channel */
    GF_VOTE_1OO2,                   /* 1 out of 2 (OR) */
    GF_VOTE_2OO2,                   /* 2 out of 2 (AND) */
    GF_VOTE_2OO3,                   /* 2 out of 3 (majority) */
    GF_VOTE_1OO2D                   /* 1 out of 2 with diagnostics */
} gf_vote_arch_t;

/* Input state */
typedef enum {
    GF_ESD_INPUT_NORMAL = 0,
    GF_ESD_INPUT_TRIPPED,           /* Trip condition active */
    GF_ESD_INPUT_FAULT,             /* Input fault detected */
    GF_ESD_INPUT_BYPASSED           /* Input bypassed */
} gf_esd_input_state_t;

/* Output state */
typedef enum {
    GF_ESD_OUTPUT_SAFE = 0,         /* Output in safe state */
    GF_ESD_OUTPUT_OPERATIONAL,      /* Normal operation */
    GF_ESD_OUTPUT_TRIPPED,          /* Tripped to safe state */
    GF_ESD_OUTPUT_FAULT             /* Output fault */
} gf_esd_output_state_t;

/* System state */
typedef enum {
    GF_ESD_STATE_INIT = 0,          /* Initialization */
    GF_ESD_STATE_OPERATIONAL,       /* Normal operation */
    GF_ESD_STATE_TRIP,              /* Trip active */
    GF_ESD_STATE_RESET,             /* Reset in progress */
    GF_ESD_STATE_MAINTENANCE,       /* Maintenance mode */
    GF_ESD_STATE_FAULT              /* System fault */
} gf_esd_state_t;

/* Input configuration */
typedef struct {
    uint8_t             id;             /* Input ID */
    char                tag[16];        /* Tag name (e.g., "PAHH-101") */
    bool                normally_open;  /* NO or NC contact */
    uint16_t            debounce_ms;    /* Debounce time */
    uint16_t            delay_ms;       /* Trip delay */
    bool                can_bypass;     /* Bypass allowed */
    uint16_t            max_bypass_hr;  /* Max bypass duration */
} gf_esd_input_config_t;

/* Output configuration */
typedef struct {
    uint8_t             id;             /* Output ID */
    char                tag[16];        /* Tag name (e.g., "SDV-101") */
    bool                fail_safe;      /* True = de-energize on trip */
    uint16_t            response_ms;    /* Response time requirement */
    bool                requires_reset; /* Manual reset required */
} gf_esd_output_config_t;

/* Safety function (links inputs to outputs) */
typedef struct {
    uint8_t             id;             /* Function ID */
    char                name[24];       /* Function name */
    gf_sil_level_t      sil;            /* SIL requirement */
    gf_vote_arch_t      voting;         /* Voting architecture */
    uint8_t             inputs[4];      /* Input IDs (up to 4) */
    uint8_t             input_count;    /* Number of inputs */
    uint8_t             outputs[4];     /* Output IDs */
    uint8_t             output_count;   /* Number of outputs */
    uint32_t            max_response_ms;/* Response time requirement */
    bool                enabled;        /* Function enabled */
} gf_esd_function_t;

/* Diagnostic results */
typedef struct {
    uint32_t        total_trips;        /* Total trip count */
    uint32_t        spurious_trips;     /* Spurious trip count */
    uint32_t        last_trip_time;     /* Last trip timestamp */
    uint32_t        last_proof_test;    /* Last proof test date */
    uint32_t        next_proof_test;    /* Next proof test due */
    float           diagnostic_coverage;/* Achieved DC percentage */
    float           pfd_avg;            /* Average PFD */
    uint32_t        mttr_hours;         /* Mean time to repair */
} gf_esd_diagnostics_t;

/* Trip callback */
typedef void (*gf_esd_trip_cb_t)(uint8_t function_id, const uint8_t *inputs,
                                  uint8_t input_count, void *ctx);

/* Fault callback */
typedef void (*gf_esd_fault_cb_t)(uint8_t component_id, uint16_t fault_code,
                                   const char *message, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize ESD system
 */
int gf_esd_init(void);

/**
 * @brief Register an input
 */
int gf_esd_register_input(const gf_esd_input_config_t *config);

/**
 * @brief Register an output
 */
int gf_esd_register_output(const gf_esd_output_config_t *config);

/**
 * @brief Register a safety function
 */
int gf_esd_register_function(const gf_esd_function_t *function);

/**
 * @brief Start ESD system
 */
int gf_esd_start(void);

/**
 * @brief Stop ESD system (for maintenance only)
 */
int gf_esd_stop(void);

/**
 * @brief Get system state
 */
gf_esd_state_t gf_esd_get_state(void);

/**
 * @brief Get input state
 */
gf_esd_input_state_t gf_esd_get_input_state(uint8_t input_id);

/**
 * @brief Get output state
 */
gf_esd_output_state_t gf_esd_get_output_state(uint8_t output_id);

/**
 * @brief Manually trip a safety function
 */
int gf_esd_trip(uint8_t function_id);

/**
 * @brief Reset after trip
 * @param function_id Function to reset (0xFF = all)
 */
int gf_esd_reset(uint8_t function_id);

/**
 * @brief Bypass an input
 * @param input_id Input to bypass
 * @param duration_hr Bypass duration in hours
 * @param reason Bypass reason code
 */
int gf_esd_bypass_input(uint8_t input_id, uint16_t duration_hr,
                        uint16_t reason);

/**
 * @brief Remove bypass
 */
int gf_esd_remove_bypass(uint8_t input_id);

/**
 * @brief Force output for testing
 * @param output_id Output to force
 * @param state State to force
 * @param duration_sec Force duration
 */
int gf_esd_force_output(uint8_t output_id, bool state, uint16_t duration_sec);

/**
 * @brief Run partial stroke test on output
 */
int gf_esd_partial_stroke_test(uint8_t output_id);

/**
 * @brief Start proof test
 * @param function_id Function to test
 */
int gf_esd_proof_test(uint8_t function_id);

/**
 * @brief Get diagnostics for function
 */
int gf_esd_get_diagnostics(uint8_t function_id, gf_esd_diagnostics_t *diag);

/**
 * @brief Register trip callback
 */
int gf_esd_register_trip_cb(gf_esd_trip_cb_t callback, void *ctx);

/**
 * @brief Register fault callback
 */
int gf_esd_register_fault_cb(gf_esd_fault_cb_t callback, void *ctx);

/**
 * @brief Process ESD logic (call from main loop)
 * Must be called at least every GF_ESD_RESPONSE_TIME_MS / 2
 */
int gf_esd_process(void);

/**
 * @brief Get current bypass count
 */
int gf_esd_get_bypass_count(void);

/**
 * @brief Check if any trips are active
 */
bool gf_esd_any_trip_active(void);

/**
 * @brief Measure actual response time
 */
uint32_t gf_esd_measure_response_time(uint8_t function_id);

#endif /* GF_EMERGENCY_SHUTDOWN_H */
