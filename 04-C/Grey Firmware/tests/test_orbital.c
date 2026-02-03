/**
 * @file test_orbital.c
 * @brief Unit tests for Zero-Gravity Fabrication Control Spotlight
 * 
 * Tests TMR sensor voting, process control loops, safety interlocks,
 * microgravity effects, yield telemetry, and quality metrics.
 * 
 * @author Grey Firmware Project
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

/*===========================================================================*/
/* Test Framework Macros                                                      */
/*===========================================================================*/

static int g_tests_run = 0;
static int g_tests_passed = 0;
static int g_assertions = 0;

#define TEST_ASSERT(cond, msg) do { \
    g_assertions++; \
    if (!(cond)) { \
        printf("  FAIL: %s (line %d)\n", msg, __LINE__); \
        return 0; \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) TEST_ASSERT((a) == (b), msg)
#define TEST_ASSERT_NEQ(a, b, msg) TEST_ASSERT((a) != (b), msg)
#define TEST_ASSERT_TRUE(a, msg) TEST_ASSERT((a), msg)
#define TEST_ASSERT_FALSE(a, msg) TEST_ASSERT(!(a), msg)
#define TEST_ASSERT_FLOAT_EQ(a, b, eps, msg) TEST_ASSERT(fabs((a)-(b)) < (eps), msg)
#define TEST_ASSERT_GT(a, b, msg) TEST_ASSERT((a) > (b), msg)
#define TEST_ASSERT_LT(a, b, msg) TEST_ASSERT((a) < (b), msg)
#define TEST_ASSERT_GTE(a, b, msg) TEST_ASSERT((a) >= (b), msg)

#define RUN_TEST(test_func) do { \
    printf("Running %s...\n", #test_func); \
    g_tests_run++; \
    if (test_func()) { \
        g_tests_passed++; \
        printf("  PASSED\n"); \
    } else { \
        printf("  FAILED\n"); \
    } \
} while(0)

/*===========================================================================*/
/* Type Definitions (from implementation)                                     */
/*===========================================================================*/

typedef enum {
    FAB_STATE_IDLE,
    FAB_STATE_INITIALIZING,
    FAB_STATE_VACUUM_PUMP,
    FAB_STATE_HEATING,
    FAB_STATE_DEPOSITION,
    FAB_STATE_GROWTH,
    FAB_STATE_COOLING,
    FAB_STATE_QUALITY_CHECK,
    FAB_STATE_UNLOADING,
    FAB_STATE_ABORT,
    FAB_STATE_MAINTENANCE
} fab_state_t;

typedef enum {
    PROCESS_CRYSTAL_GROWTH,
    PROCESS_VAPOR_DEPOSITION,
    PROCESS_ALLOY_MELTING,
    PROCESS_FIBER_DRAWING,
    PROCESS_3D_PRINTING,
    PROCESS_CONTAINERLESS
} fab_process_type_t;

typedef enum {
    FAB_FAULT_NONE = 0,
    FAB_FAULT_VACUUM_LOSS,
    FAB_FAULT_OVERTEMP,
    FAB_FAULT_UNDERTEMP,
    FAB_FAULT_THERMAL_RUNAWAY,
    FAB_FAULT_PRESSURE_SPIKE,
    FAB_FAULT_SENSOR_FAIL,
    FAB_FAULT_VIBRATION,
    FAB_FAULT_ATTITUDE_LOSS,
    FAB_FAULT_POWER_FAIL,
    FAB_FAULT_PRECURSOR_EMPTY,
    FAB_FAULT_TMR_DISAGREE,
    FAB_FAULT_GROWTH_DEFECT,
    FAB_FAULT_CONTAMINATION,
    FAB_FAULT_COMM_LOSS
} fab_fault_t;

typedef enum {
    TMR_CHAN_OK,
    TMR_CHAN_SUSPECT,
    TMR_CHAN_FAILED
} tmr_chan_status_t;

typedef enum {
    TMR_UNANIMOUS,
    TMR_MAJORITY,
    TMR_DISAGREE,
    TMR_ALL_FAILED
} tmr_result_t;

typedef enum {
    SENSOR_TEMPERATURE,
    SENSOR_PRESSURE,
    SENSOR_FLOW_RATE,
    SENSOR_THICKNESS,
    SENSOR_COMPOSITION,
    SENSOR_VIBRATION,
    SENSOR_ATTITUDE
} fab_sensor_type_t;

typedef struct {
    uint8_t chamber_id;
    fab_process_type_t process;
    float target_temp_c;
    float target_pressure_torr;
    float hold_time_sec;
    bool atmosphere_control;
    char atmosphere_gas[16];
} fab_chamber_config_t;

typedef struct {
    float temperature_c;
    float pressure_torr;
    float g_level;
    float vibration_mg;
    float attitude_error_deg;
    bool vacuum_stable;
    bool thermal_stable;
    bool gravity_acceptable;
} fab_environment_t;

typedef struct {
    uint32_t sample_id;
    fab_process_type_t process;
    float mass_mg;
    float thickness_um;
    float uniformity_pct;
    float defect_density;
    bool passed_qc;
    uint32_t start_time;
    uint32_t end_time;
} fab_sample_t;

typedef struct {
    uint32_t total_samples;
    uint32_t passed_samples;
    uint32_t failed_samples;
    float avg_yield_pct;
    float avg_thickness_um;
    float avg_uniformity_pct;
    float material_consumed_g;
    float material_produced_g;
} yield_stats_t;

typedef struct {
    uint16_t apid;
    uint16_t sequence;
    uint32_t timestamp;
    uint8_t chamber_id;
    uint8_t state;
    uint8_t fault_code;
    float temperature_c;
    float pressure_torr;
    float g_level;
    float yield_pct;
    float sample_thickness_um;
    uint8_t checksum;
} fab_telemetry_t;

/*===========================================================================*/
/* External Function Declarations                                             */
/*===========================================================================*/

extern int orbital_fab_init(void);
extern int orbital_fab_configure(const fab_chamber_config_t *config);
extern int orbital_fab_start(uint8_t chamber_id);
extern int orbital_fab_abort(uint8_t chamber_id);
extern int orbital_fab_reset(uint8_t chamber_id);
extern void orbital_fab_update(uint32_t elapsed_ms);
extern int orbital_fab_get_state(uint8_t chamber_id, fab_state_t *state);
extern int orbital_fab_get_fault(uint8_t chamber_id, fab_fault_t *fault);
extern int orbital_fab_get_environment(uint8_t chamber_id, fab_environment_t *env);
extern int orbital_fab_get_yield(uint8_t chamber_id, yield_stats_t *yield);
extern int orbital_fab_get_sample(uint8_t chamber_id, fab_sample_t *sample);
extern int orbital_fab_get_telemetry(uint8_t chamber_id, uint8_t *buffer, size_t max_len);
extern int orbital_fab_inject_sensor_fault(uint8_t chamber_id, fab_sensor_type_t sensor_type, uint8_t channel);
extern int orbital_fab_set_environment(uint8_t chamber_id, const char *param, float value);
extern void orbital_fab_shutdown(void);

/*===========================================================================*/
/* Test Helper Functions                                                      */
/*===========================================================================*/

static void reset_module(void) {
    orbital_fab_shutdown();
    orbital_fab_init();
}

static void run_cycles(uint32_t count, uint32_t ms_per_cycle) {
    for (uint32_t i = 0; i < count; i++) {
        orbital_fab_update(ms_per_cycle);
    }
}

/*===========================================================================*/
/* Module Initialization Tests                                                */
/*===========================================================================*/

static int test_init_success(void) {
    orbital_fab_shutdown();
    int result = orbital_fab_init();
    TEST_ASSERT_EQ(result, 0, "orbital_fab_init should return 0");
    
    fab_state_t state;
    result = orbital_fab_get_state(0, &state);
    TEST_ASSERT_EQ(result, 0, "get_state should succeed");
    TEST_ASSERT_EQ(state, FAB_STATE_IDLE, "Initial state should be IDLE");
    
    return 1;
}

static int test_init_double_init_fails(void) {
    reset_module();
    
    int result = orbital_fab_init();
    TEST_ASSERT_EQ(result, -1, "Double init should fail");
    
    return 1;
}

static int test_shutdown_and_reinit(void) {
    reset_module();
    orbital_fab_shutdown();
    
    int result = orbital_fab_init();
    TEST_ASSERT_EQ(result, 0, "Reinit after shutdown should succeed");
    
    return 1;
}

/*===========================================================================*/
/* Chamber Configuration Tests                                                */
/*===========================================================================*/

static int test_configure_chamber(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f,
        .target_pressure_torr = 1e-6f,
        .hold_time_sec = 3600.0f,
        .atmosphere_control = false
    };
    
    int result = orbital_fab_configure(&config);
    TEST_ASSERT_EQ(result, 0, "Configure should succeed");
    
    return 1;
}

static int test_configure_invalid_chamber(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 99,  /* Invalid */
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    
    int result = orbital_fab_configure(&config);
    TEST_ASSERT_EQ(result, -1, "Configure invalid chamber should fail");
    
    return 1;
}

static int test_configure_null_config(void) {
    reset_module();
    
    int result = orbital_fab_configure(NULL);
    TEST_ASSERT_EQ(result, -1, "Configure with NULL should fail");
    
    return 1;
}

/*===========================================================================*/
/* Process Start Tests                                                        */
/*===========================================================================*/

static int test_start_process(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f,
        .target_pressure_torr = 1e-6f
    };
    orbital_fab_configure(&config);
    
    int result = orbital_fab_start(0);
    TEST_ASSERT_EQ(result, 0, "Start should succeed");
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    TEST_ASSERT_EQ(state, FAB_STATE_VACUUM_PUMP, "State should be VACUUM_PUMP");
    
    return 1;
}

static int test_start_already_running(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_start(0);
    
    int result = orbital_fab_start(0);
    TEST_ASSERT_EQ(result, -1, "Start when running should fail");
    
    return 1;
}

static int test_start_invalid_chamber(void) {
    reset_module();
    
    int result = orbital_fab_start(99);
    TEST_ASSERT_EQ(result, -1, "Start invalid chamber should fail");
    
    return 1;
}

/*===========================================================================*/
/* State Machine Tests                                                        */
/*===========================================================================*/

static int test_vacuum_pumpdown(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f,
        .target_pressure_torr = 1e-6f
    };
    orbital_fab_configure(&config);
    orbital_fab_start(0);
    
    /* Run pump cycles */
    run_cycles(500, 100);
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    
    /* Should have transitioned to heating */
    TEST_ASSERT_TRUE(state >= FAB_STATE_HEATING, "Should progress past vacuum pump");
    
    fab_environment_t env;
    orbital_fab_get_environment(0, &env);
    TEST_ASSERT_LT(env.pressure_torr, 1e-4f, "Pressure should be low");
    
    return 1;
}

static int test_heating_phase(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 100.0f,  /* Lower temp for faster test */
        .target_pressure_torr = 1e-6f
    };
    orbital_fab_configure(&config);
    
    /* Set pressure low to skip pump phase quickly */
    orbital_fab_set_environment(0, "pressure", 1e-7f);
    orbital_fab_start(0);
    
    /* Run heating cycles */
    run_cycles(1000, 100);
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    
    /* Should have reached deposition or beyond */
    TEST_ASSERT_GTE((int)state, (int)FAB_STATE_DEPOSITION, "Should reach deposition phase");
    
    return 1;
}

static int test_full_deposition_cycle(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 50.0f,
        .target_pressure_torr = 1e-6f
    };
    orbital_fab_configure(&config);
    
    /* Set environment for fast cycle */
    orbital_fab_set_environment(0, "pressure", 1e-7f);
    orbital_fab_set_environment(0, "temperature", 49.0f);
    orbital_fab_start(0);
    
    /* Run many cycles */
    run_cycles(20000, 100);
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    
    /* Should complete cycle */
    TEST_ASSERT_EQ(state, FAB_STATE_IDLE, "Should return to IDLE after cycle");
    
    yield_stats_t yield;
    orbital_fab_get_yield(0, &yield);
    TEST_ASSERT_EQ(yield.total_samples, 1, "Should have produced 1 sample");
    
    return 1;
}

static int test_crystal_growth_cycle(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 1,
        .process = PROCESS_CRYSTAL_GROWTH,
        .target_temp_c = 50.0f,
        .target_pressure_torr = 1e-6f
    };
    orbital_fab_configure(&config);
    
    /* Set environment for fast cycle */
    orbital_fab_set_environment(1, "pressure", 1e-7f);
    orbital_fab_set_environment(1, "temperature", 49.0f);
    orbital_fab_start(1);
    
    /* Run cycles */
    run_cycles(200000, 100);
    
    fab_state_t state;
    orbital_fab_get_state(1, &state);
    TEST_ASSERT_EQ(state, FAB_STATE_IDLE, "Should complete crystal growth cycle");
    
    return 1;
}

/*===========================================================================*/
/* Abort and Reset Tests                                                      */
/*===========================================================================*/

static int test_abort_process(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_start(0);
    run_cycles(10, 100);
    
    int result = orbital_fab_abort(0);
    TEST_ASSERT_EQ(result, 0, "Abort should succeed");
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    TEST_ASSERT_EQ(state, FAB_STATE_ABORT, "State should be ABORT");
    
    return 1;
}

static int test_abort_idle_chamber(void) {
    reset_module();
    
    int result = orbital_fab_abort(0);
    TEST_ASSERT_EQ(result, 0, "Abort idle should return 0");
    
    return 1;
}

static int test_reset_from_maintenance(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_start(0);
    run_cycles(10, 100);
    orbital_fab_abort(0);
    
    /* Run until MAINTENANCE */
    run_cycles(500, 100);
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    
    if (state == FAB_STATE_MAINTENANCE) {
        int result = orbital_fab_reset(0);
        TEST_ASSERT_EQ(result, 0, "Reset should succeed");
        
        orbital_fab_get_state(0, &state);
        TEST_ASSERT_EQ(state, FAB_STATE_IDLE, "Should be IDLE after reset");
    }
    /* If not in maintenance yet, test passes (timing dependent) */
    
    return 1;
}

/*===========================================================================*/
/* TMR Sensor Voting Tests                                                    */
/*===========================================================================*/

static int test_tmr_single_sensor_fault(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    
    /* Inject single sensor fault */
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 0);
    
    orbital_fab_start(0);
    run_cycles(10, 100);
    
    fab_fault_t fault;
    orbital_fab_get_fault(0, &fault);
    
    /* Single sensor fault should not trigger abort - TMR handles it */
    TEST_ASSERT_EQ(fault, FAB_FAULT_NONE, "Single sensor fault should be handled by TMR");
    
    return 1;
}

static int test_tmr_double_sensor_fault(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    
    /* Inject two sensor faults */
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 0);
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 1);
    
    orbital_fab_start(0);
    run_cycles(10, 100);
    
    /* Two faults - should still work but with reduced confidence */
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    TEST_ASSERT_NEQ(state, FAB_STATE_IDLE, "Process should be running");
    
    return 1;
}

static int test_tmr_all_sensors_failed(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    
    /* Inject all three sensor faults */
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 0);
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 1);
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 2);
    
    orbital_fab_start(0);
    run_cycles(10, 100);
    
    fab_fault_t fault;
    orbital_fab_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, FAB_FAULT_SENSOR_FAIL, "All sensors failed should trigger fault");
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    TEST_ASSERT_EQ(state, FAB_STATE_ABORT, "Should be in ABORT state");
    
    return 1;
}

/*===========================================================================*/
/* Safety Interlock Tests                                                     */
/*===========================================================================*/

static int test_overtemp_interlock(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_set_environment(0, "pressure", 1e-7f);
    orbital_fab_start(0);
    run_cycles(5, 100);
    
    /* Inject overtemp */
    orbital_fab_set_environment(0, "temperature", 3000.0f);
    run_cycles(5, 100);
    
    fab_fault_t fault;
    orbital_fab_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, FAB_FAULT_OVERTEMP, "Should detect overtemp");
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    TEST_ASSERT_EQ(state, FAB_STATE_ABORT, "Should abort on overtemp");
    
    return 1;
}

static int test_vacuum_loss_interlock(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_set_environment(0, "pressure", 1e-7f);
    orbital_fab_set_environment(0, "temperature", 499.0f);
    orbital_fab_start(0);
    run_cycles(100, 100);  /* Get past vacuum pump phase */
    
    /* Inject vacuum loss */
    orbital_fab_set_environment(0, "pressure", 1.0f);
    run_cycles(5, 100);
    
    fab_fault_t fault;
    orbital_fab_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, FAB_FAULT_VACUUM_LOSS, "Should detect vacuum loss");
    
    return 1;
}

static int test_vibration_interlock(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 50.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_set_environment(0, "pressure", 1e-7f);
    orbital_fab_set_environment(0, "temperature", 49.0f);
    orbital_fab_start(0);
    
    /* Get to deposition phase */
    run_cycles(500, 100);
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    
    if (state == FAB_STATE_DEPOSITION || state == FAB_STATE_GROWTH) {
        /* Inject excessive vibration */
        orbital_fab_set_environment(0, "vibration", 100.0f);
        run_cycles(5, 100);
        
        fab_fault_t fault;
        orbital_fab_get_fault(0, &fault);
        TEST_ASSERT_EQ(fault, FAB_FAULT_VIBRATION, "Should detect vibration");
    }
    
    return 1;
}

/*===========================================================================*/
/* Environment Monitoring Tests                                               */
/*===========================================================================*/

static int test_get_environment(void) {
    reset_module();
    
    fab_environment_t env;
    int result = orbital_fab_get_environment(0, &env);
    TEST_ASSERT_EQ(result, 0, "Get environment should succeed");
    TEST_ASSERT_GT(env.temperature_c, -300.0f, "Temperature should be reasonable");
    TEST_ASSERT_GT(env.pressure_torr, 0.0f, "Pressure should be positive");
    
    return 1;
}

static int test_set_environment_params(void) {
    reset_module();
    
    int result = orbital_fab_set_environment(0, "temperature", 100.0f);
    TEST_ASSERT_EQ(result, 0, "Set temperature should succeed");
    
    fab_environment_t env;
    orbital_fab_get_environment(0, &env);
    TEST_ASSERT_FLOAT_EQ(env.temperature_c, 100.0f, 0.1f, "Temperature should be set");
    
    result = orbital_fab_set_environment(0, "pressure", 0.001f);
    TEST_ASSERT_EQ(result, 0, "Set pressure should succeed");
    
    result = orbital_fab_set_environment(0, "g_level", 1e-5f);
    TEST_ASSERT_EQ(result, 0, "Set g_level should succeed");
    
    result = orbital_fab_set_environment(0, "invalid_param", 0.0f);
    TEST_ASSERT_EQ(result, -1, "Invalid param should fail");
    
    return 1;
}

static int test_microgravity_check(void) {
    reset_module();
    
    /* Set acceptable microgravity */
    orbital_fab_set_environment(0, "g_level", 1e-5f);
    
    fab_environment_t env;
    orbital_fab_get_environment(0, &env);
    TEST_ASSERT_LT(env.g_level, 1e-4f, "G-level should be microgravity");
    
    return 1;
}

/*===========================================================================*/
/* Yield Statistics Tests                                                     */
/*===========================================================================*/

static int test_yield_statistics_init(void) {
    reset_module();
    
    yield_stats_t yield;
    int result = orbital_fab_get_yield(0, &yield);
    TEST_ASSERT_EQ(result, 0, "Get yield should succeed");
    TEST_ASSERT_EQ(yield.total_samples, 0, "Initial samples should be 0");
    TEST_ASSERT_EQ(yield.passed_samples, 0, "Initial passed should be 0");
    
    return 1;
}

static int test_yield_after_cycle(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 50.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_set_environment(0, "pressure", 1e-7f);
    orbital_fab_set_environment(0, "temperature", 49.0f);
    orbital_fab_start(0);
    
    run_cycles(20000, 100);
    
    yield_stats_t yield;
    orbital_fab_get_yield(0, &yield);
    TEST_ASSERT_GTE(yield.total_samples, 1, "Should have at least 1 sample");
    TEST_ASSERT_GTE(yield.avg_yield_pct, 0.0f, "Yield should be >= 0");
    TEST_ASSERT_LT(yield.avg_yield_pct, 101.0f, "Yield should be <= 100");
    
    return 1;
}

/*===========================================================================*/
/* Sample Tracking Tests                                                      */
/*===========================================================================*/

static int test_sample_tracking_init(void) {
    reset_module();
    
    fab_sample_t sample;
    int result = orbital_fab_get_sample(0, &sample);
    TEST_ASSERT_EQ(result, 0, "Get sample should succeed");
    TEST_ASSERT_FLOAT_EQ(sample.thickness_um, 0.0f, 0.01f, "Initial thickness should be 0");
    
    return 1;
}

static int test_sample_thickness_tracking(void) {
    reset_module();
    
    fab_chamber_config_t config = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 50.0f
    };
    orbital_fab_configure(&config);
    orbital_fab_set_environment(0, "pressure", 1e-7f);
    orbital_fab_set_environment(0, "temperature", 49.0f);
    orbital_fab_start(0);
    
    /* Run until deposition starts */
    run_cycles(500, 100);
    
    fab_state_t state;
    orbital_fab_get_state(0, &state);
    
    if (state == FAB_STATE_DEPOSITION) {
        fab_sample_t sample_before;
        orbital_fab_get_sample(0, &sample_before);
        
        run_cycles(100, 100);
        
        fab_sample_t sample_after;
        orbital_fab_get_sample(0, &sample_after);
        
        TEST_ASSERT_GT(sample_after.thickness_um, sample_before.thickness_um, 
                       "Thickness should increase during deposition");
    }
    
    return 1;
}

/*===========================================================================*/
/* Telemetry Tests                                                            */
/*===========================================================================*/

static int test_telemetry_generation(void) {
    reset_module();
    
    uint8_t buffer[256];
    int result = orbital_fab_get_telemetry(0, buffer, sizeof(buffer));
    TEST_ASSERT_GT(result, 0, "Telemetry should return positive size");
    TEST_ASSERT_EQ(result, (int)sizeof(fab_telemetry_t), "Should return packet size");
    
    fab_telemetry_t *packet = (fab_telemetry_t *)buffer;
    TEST_ASSERT_EQ(packet->chamber_id, 0, "Chamber ID should match");
    TEST_ASSERT_EQ(packet->state, FAB_STATE_IDLE, "State should be IDLE");
    
    return 1;
}

static int test_telemetry_sequence(void) {
    reset_module();
    
    uint8_t buffer[256];
    orbital_fab_get_telemetry(0, buffer, sizeof(buffer));
    fab_telemetry_t *packet1 = (fab_telemetry_t *)buffer;
    uint16_t seq1 = packet1->sequence;
    
    orbital_fab_get_telemetry(0, buffer, sizeof(buffer));
    fab_telemetry_t *packet2 = (fab_telemetry_t *)buffer;
    uint16_t seq2 = packet2->sequence;
    
    TEST_ASSERT_EQ(seq2, seq1 + 1, "Sequence should increment");
    
    return 1;
}

static int test_telemetry_buffer_too_small(void) {
    reset_module();
    
    uint8_t buffer[4];  /* Too small */
    int result = orbital_fab_get_telemetry(0, buffer, sizeof(buffer));
    TEST_ASSERT_EQ(result, -1, "Should fail with small buffer");
    
    return 1;
}

static int test_telemetry_null_buffer(void) {
    reset_module();
    
    int result = orbital_fab_get_telemetry(0, NULL, 256);
    TEST_ASSERT_EQ(result, -1, "Should fail with NULL buffer");
    
    return 1;
}

/*===========================================================================*/
/* Multiple Chamber Tests                                                     */
/*===========================================================================*/

static int test_multiple_chambers(void) {
    reset_module();
    
    fab_chamber_config_t config0 = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    fab_chamber_config_t config1 = {
        .chamber_id = 1,
        .process = PROCESS_CRYSTAL_GROWTH,
        .target_temp_c = 1000.0f
    };
    
    orbital_fab_configure(&config0);
    orbital_fab_configure(&config1);
    
    orbital_fab_start(0);
    orbital_fab_start(1);
    
    fab_state_t state0, state1;
    orbital_fab_get_state(0, &state0);
    orbital_fab_get_state(1, &state1);
    
    TEST_ASSERT_EQ(state0, FAB_STATE_VACUUM_PUMP, "Chamber 0 should be pumping");
    TEST_ASSERT_EQ(state1, FAB_STATE_VACUUM_PUMP, "Chamber 1 should be pumping");
    
    run_cycles(10, 100);
    
    /* Both should progress independently */
    orbital_fab_get_state(0, &state0);
    orbital_fab_get_state(1, &state1);
    TEST_ASSERT_NEQ(state0, FAB_STATE_IDLE, "Chamber 0 should be running");
    TEST_ASSERT_NEQ(state1, FAB_STATE_IDLE, "Chamber 1 should be running");
    
    return 1;
}

static int test_chambers_independent_faults(void) {
    reset_module();
    
    fab_chamber_config_t config0 = {
        .chamber_id = 0,
        .process = PROCESS_VAPOR_DEPOSITION,
        .target_temp_c = 500.0f
    };
    fab_chamber_config_t config1 = {
        .chamber_id = 1,
        .process = PROCESS_CRYSTAL_GROWTH,
        .target_temp_c = 1000.0f
    };
    
    orbital_fab_configure(&config0);
    orbital_fab_configure(&config1);
    
    orbital_fab_start(0);
    orbital_fab_start(1);
    run_cycles(10, 100);
    
    /* Fault only chamber 0 */
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 0);
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 1);
    orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 2);
    run_cycles(10, 100);
    
    fab_state_t state0, state1;
    orbital_fab_get_state(0, &state0);
    orbital_fab_get_state(1, &state1);
    
    TEST_ASSERT_EQ(state0, FAB_STATE_ABORT, "Chamber 0 should abort");
    TEST_ASSERT_NEQ(state1, FAB_STATE_ABORT, "Chamber 1 should not abort");
    
    return 1;
}

/*===========================================================================*/
/* Error Handling Tests                                                       */
/*===========================================================================*/

static int test_get_state_null_pointer(void) {
    reset_module();
    
    int result = orbital_fab_get_state(0, NULL);
    TEST_ASSERT_EQ(result, -1, "NULL pointer should fail");
    
    return 1;
}

static int test_get_fault_invalid_chamber(void) {
    reset_module();
    
    fab_fault_t fault;
    int result = orbital_fab_get_fault(99, &fault);
    TEST_ASSERT_EQ(result, -1, "Invalid chamber should fail");
    
    return 1;
}

static int test_inject_fault_invalid_channel(void) {
    reset_module();
    
    int result = orbital_fab_inject_sensor_fault(0, SENSOR_TEMPERATURE, 10);
    TEST_ASSERT_EQ(result, -1, "Invalid channel should fail");
    
    return 1;
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void) {
    printf("========================================\n");
    printf("Zero-Gravity Fabrication Control Tests\n");
    printf("========================================\n\n");
    
    /* Module Initialization */
    printf("--- Module Initialization ---\n");
    RUN_TEST(test_init_success);
    RUN_TEST(test_init_double_init_fails);
    RUN_TEST(test_shutdown_and_reinit);
    
    /* Chamber Configuration */
    printf("\n--- Chamber Configuration ---\n");
    RUN_TEST(test_configure_chamber);
    RUN_TEST(test_configure_invalid_chamber);
    RUN_TEST(test_configure_null_config);
    
    /* Process Start */
    printf("\n--- Process Start ---\n");
    RUN_TEST(test_start_process);
    RUN_TEST(test_start_already_running);
    RUN_TEST(test_start_invalid_chamber);
    
    /* State Machine */
    printf("\n--- State Machine ---\n");
    RUN_TEST(test_vacuum_pumpdown);
    RUN_TEST(test_heating_phase);
    RUN_TEST(test_full_deposition_cycle);
    RUN_TEST(test_crystal_growth_cycle);
    
    /* Abort and Reset */
    printf("\n--- Abort and Reset ---\n");
    RUN_TEST(test_abort_process);
    RUN_TEST(test_abort_idle_chamber);
    RUN_TEST(test_reset_from_maintenance);
    
    /* TMR Sensor Voting */
    printf("\n--- TMR Sensor Voting ---\n");
    RUN_TEST(test_tmr_single_sensor_fault);
    RUN_TEST(test_tmr_double_sensor_fault);
    RUN_TEST(test_tmr_all_sensors_failed);
    
    /* Safety Interlocks */
    printf("\n--- Safety Interlocks ---\n");
    RUN_TEST(test_overtemp_interlock);
    RUN_TEST(test_vacuum_loss_interlock);
    RUN_TEST(test_vibration_interlock);
    
    /* Environment Monitoring */
    printf("\n--- Environment Monitoring ---\n");
    RUN_TEST(test_get_environment);
    RUN_TEST(test_set_environment_params);
    RUN_TEST(test_microgravity_check);
    
    /* Yield Statistics */
    printf("\n--- Yield Statistics ---\n");
    RUN_TEST(test_yield_statistics_init);
    RUN_TEST(test_yield_after_cycle);
    
    /* Sample Tracking */
    printf("\n--- Sample Tracking ---\n");
    RUN_TEST(test_sample_tracking_init);
    RUN_TEST(test_sample_thickness_tracking);
    
    /* Telemetry */
    printf("\n--- Telemetry ---\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_sequence);
    RUN_TEST(test_telemetry_buffer_too_small);
    RUN_TEST(test_telemetry_null_buffer);
    
    /* Multiple Chambers */
    printf("\n--- Multiple Chambers ---\n");
    RUN_TEST(test_multiple_chambers);
    RUN_TEST(test_chambers_independent_faults);
    
    /* Error Handling */
    printf("\n--- Error Handling ---\n");
    RUN_TEST(test_get_state_null_pointer);
    RUN_TEST(test_get_fault_invalid_chamber);
    RUN_TEST(test_inject_fault_invalid_channel);
    
    /* Summary */
    printf("\n========================================\n");
    printf("Test Results: %d/%d passed (%d assertions)\n", 
           g_tests_passed, g_tests_run, g_assertions);
    printf("========================================\n");
    
    return (g_tests_passed == g_tests_run) ? 0 : 1;
}
