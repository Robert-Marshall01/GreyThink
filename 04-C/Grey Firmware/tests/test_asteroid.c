/**
 * @file test_asteroid.c
 * @brief Unit tests for Asteroid Drill Control & Resource Telemetry Spotlight
 * 
 * Tests drill control loop, TMR sensor voting, safety interlocks,
 * material classification, yield telemetry, and fault conditions.
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
/* Type Redefinitions (matching implementation)                               */
/*===========================================================================*/

typedef enum {
    DRILL_STATE_IDLE,
    DRILL_STATE_PRE_CHECK,
    DRILL_STATE_ANCHORING,
    DRILL_STATE_SURFACE_SURVEY,
    DRILL_STATE_DRILLING,
    DRILL_STATE_SAMPLE_COLLECT,
    DRILL_STATE_RETRACTING,
    DRILL_STATE_FAULT,
    DRILL_STATE_MAINTENANCE
} drill_state_t;

typedef enum {
    DRILL_FAULT_NONE = 0,
    DRILL_FAULT_OVERHEAT,
    DRILL_FAULT_OVERTORQUE,
    DRILL_FAULT_STALL,
    DRILL_FAULT_BIT_BREAK,
    DRILL_FAULT_POWER_LOSS,
    DRILL_FAULT_VIBRATION,
    DRILL_FAULT_ANCHOR_SLIP,
    DRILL_FAULT_SENSOR_FAIL,
    DRILL_FAULT_COMM_LOSS,
    DRILL_FAULT_DEPTH_LIMIT
} drill_fault_t;

typedef enum {
    MATERIAL_UNKNOWN,
    MATERIAL_REGOLITH,
    MATERIAL_SILICATE,
    MATERIAL_CARBONACEOUS,
    MATERIAL_METALLIC,
    MATERIAL_ICE
} material_t;

typedef enum {
    SENSOR_TORQUE,
    SENSOR_TEMPERATURE,
    SENSOR_VIBRATION,
    SENSOR_DEPTH
} sensor_type_t;

typedef struct {
    uint32_t total_samples;
    uint32_t successful_samples;
    float total_mass_kg;
    float ice_mass_kg;
    float metal_mass_kg;
    float silicate_mass_kg;
    float avg_purity;
    float extraction_rate;
    uint32_t drilling_time_s;
} yield_stats_t;

typedef struct {
    uint32_t sample_id;
    material_t material;
    float depth_m;
    float mass_kg;
    float purity_pct;
    uint32_t start_time;
    uint32_t end_time;
    bool valid;
} sample_record_t;

typedef struct {
    uint8_t unit_id;
    float max_torque_nm;
    float target_rpm;
    float max_depth_m;
    float overheat_temp_c;
    bool percussion_enabled;
} drill_config_t;

/*===========================================================================*/
/* External Function Declarations                                             */
/*===========================================================================*/

extern int asteroid_drill_init(void);
extern int asteroid_drill_configure(const drill_config_t *config);
extern int asteroid_drill_start(uint8_t unit_id);
extern int asteroid_drill_abort(uint8_t unit_id);
extern int asteroid_drill_reset(uint8_t unit_id);
extern void asteroid_drill_update(uint32_t elapsed_ms);
extern int asteroid_drill_get_state(uint8_t unit_id, drill_state_t *state);
extern int asteroid_drill_get_fault(uint8_t unit_id, drill_fault_t *fault);
extern int asteroid_drill_get_yield(uint8_t unit_id, yield_stats_t *stats);
extern int asteroid_drill_get_sample(uint8_t unit_id, sample_record_t *sample);
extern int asteroid_drill_get_telemetry(uint8_t unit_id, uint8_t *buffer, size_t max_len);
extern int asteroid_drill_set_environment(uint8_t unit_id, const char *param, float value);
extern int asteroid_drill_inject_fault(uint8_t unit_id, sensor_type_t sensor, uint8_t channel);
extern void asteroid_drill_shutdown(void);

/*===========================================================================*/
/* Test Helper Functions                                                      */
/*===========================================================================*/

static void reset_module(void) {
    asteroid_drill_shutdown();
    asteroid_drill_init();
}

static void run_cycles(uint32_t count, uint32_t ms_per_cycle) {
    for (uint32_t i = 0; i < count; i++) {
        asteroid_drill_update(ms_per_cycle);
    }
}

/*===========================================================================*/
/* Module Initialization Tests                                                */
/*===========================================================================*/

static int test_init_success(void) {
    asteroid_drill_shutdown();
    int result = asteroid_drill_init();
    TEST_ASSERT_EQ(result, 0, "asteroid_drill_init should return 0");
    
    drill_state_t state;
    result = asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(result, 0, "get_state should succeed");
    TEST_ASSERT_EQ(state, DRILL_STATE_IDLE, "Initial state should be IDLE");
    
    return 1;
}

static int test_init_double_init_fails(void) {
    asteroid_drill_shutdown();
    asteroid_drill_init();
    int result = asteroid_drill_init();
    TEST_ASSERT_EQ(result, -1, "Double init should fail");
    
    return 1;
}

static int test_shutdown_and_reinit(void) {
    reset_module();
    asteroid_drill_shutdown();
    int result = asteroid_drill_init();
    TEST_ASSERT_EQ(result, 0, "Reinit after shutdown should succeed");
    
    return 1;
}

/*===========================================================================*/
/* Configuration Tests                                                        */
/*===========================================================================*/

static int test_configure_drill(void) {
    reset_module();
    
    drill_config_t config = {
        .unit_id = 0,
        .max_torque_nm = 400.0f,
        .target_rpm = 800.0f,
        .max_depth_m = 20.0f,
        .overheat_temp_c = 140.0f,
        .percussion_enabled = false
    };
    
    int result = asteroid_drill_configure(&config);
    TEST_ASSERT_EQ(result, 0, "Configure should succeed");
    
    return 1;
}

static int test_configure_invalid_unit(void) {
    reset_module();
    
    drill_config_t config = {
        .unit_id = 99,
        .target_rpm = 800.0f
    };
    
    int result = asteroid_drill_configure(&config);
    TEST_ASSERT_EQ(result, -1, "Configure invalid unit should fail");
    
    return 1;
}

static int test_configure_null_config(void) {
    reset_module();
    int result = asteroid_drill_configure(NULL);
    TEST_ASSERT_EQ(result, -1, "Configure with NULL should fail");
    
    return 1;
}

/*===========================================================================*/
/* Drilling Operation Tests                                                   */
/*===========================================================================*/

static int test_start_drilling(void) {
    reset_module();
    
    drill_config_t config = {
        .unit_id = 0,
        .target_rpm = 600.0f
    };
    asteroid_drill_configure(&config);
    
    int result = asteroid_drill_start(0);
    TEST_ASSERT_EQ(result, 0, "Start should succeed");
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(state, DRILL_STATE_PRE_CHECK, "State should be PRE_CHECK");
    
    return 1;
}

static int test_start_already_running(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    int result = asteroid_drill_start(0);
    TEST_ASSERT_EQ(result, -1, "Start when running should fail");
    
    return 1;
}

static int test_start_invalid_unit(void) {
    reset_module();
    int result = asteroid_drill_start(99);
    TEST_ASSERT_EQ(result, -1, "Start invalid unit should fail");
    
    return 1;
}

/*===========================================================================*/
/* State Machine Tests                                                        */
/*===========================================================================*/

static int test_pre_check_to_anchoring(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Run pre-check phase */
    run_cycles(50, 100);
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(state, DRILL_STATE_ANCHORING, "Should progress to ANCHORING");
    
    return 1;
}

static int test_anchoring_phase(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Run through pre-check and anchoring */
    run_cycles(200, 100);
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_GTE(state, DRILL_STATE_SURFACE_SURVEY, "Should progress past ANCHORING");
    
    return 1;
}

static int test_full_drilling_cycle(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Run through complete cycle */
    run_cycles(50000, 100);
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(state, DRILL_STATE_IDLE, "Should return to IDLE after cycle");
    
    yield_stats_t yield;
    asteroid_drill_get_yield(0, &yield);
    TEST_ASSERT_GT(yield.total_samples, 0, "Should have collected samples");
    TEST_ASSERT_GT(yield.total_mass_kg, 0.0f, "Should have extracted mass");
    
    return 1;
}

/*===========================================================================*/
/* Abort and Reset Tests                                                      */
/*===========================================================================*/

static int test_abort_drilling(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Get to drilling state */
    run_cycles(300, 100);
    
    int result = asteroid_drill_abort(0);
    TEST_ASSERT_EQ(result, 0, "Abort should succeed");
    
    run_cycles(200, 100);
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(state, DRILL_STATE_IDLE, "Should be IDLE after abort+retract");
    
    return 1;
}

static int test_abort_idle(void) {
    reset_module();
    int result = asteroid_drill_abort(0);
    TEST_ASSERT_EQ(result, 0, "Abort idle should return 0");
    
    return 1;
}

static int test_reset_from_fault(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Get to drilling */
    run_cycles(300, 100);
    
    /* Inject overheat */
    asteroid_drill_set_environment(0, "temperature", 200.0f);
    run_cycles(5, 100);
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(state, DRILL_STATE_FAULT, "Should be in FAULT state");
    
    /* Cool down and reset */
    asteroid_drill_set_environment(0, "temperature", 30.0f);
    run_cycles(10, 100);
    
    int result = asteroid_drill_reset(0);
    TEST_ASSERT_EQ(result, 0, "Reset should succeed");
    
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(state, DRILL_STATE_IDLE, "Should be IDLE after reset");
    
    return 1;
}

/*===========================================================================*/
/* TMR Sensor Tests                                                           */
/*===========================================================================*/

static int test_tmr_single_sensor_fault(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    
    /* Inject single sensor fault */
    asteroid_drill_inject_fault(0, SENSOR_TORQUE, 0);
    
    /* System should continue with 2/3 sensors */
    asteroid_drill_start(0);
    run_cycles(100, 100);
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_NEQ(state, DRILL_STATE_FAULT, "Should not fault on single sensor");
    
    return 1;
}

static int test_tmr_double_sensor_fault(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    
    /* Inject two sensor faults */
    asteroid_drill_inject_fault(0, SENSOR_TORQUE, 0);
    asteroid_drill_inject_fault(0, SENSOR_TORQUE, 1);
    
    /* System should continue with 1/3 sensors (degraded) */
    asteroid_drill_start(0);
    run_cycles(100, 100);
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    /* May continue or fault depending on criticality */
    TEST_ASSERT_TRUE(state != DRILL_STATE_IDLE, "Should have progressed");
    
    return 1;
}

static int test_tmr_all_sensors_failed(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Get past pre-check first */
    run_cycles(50, 100);
    
    /* Inject all sensor faults */
    asteroid_drill_inject_fault(0, SENSOR_TEMPERATURE, 0);
    asteroid_drill_inject_fault(0, SENSOR_TEMPERATURE, 1);
    asteroid_drill_inject_fault(0, SENSOR_TEMPERATURE, 2);
    
    run_cycles(100, 100);
    
    drill_fault_t fault;
    asteroid_drill_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, DRILL_FAULT_SENSOR_FAIL, "Should detect sensor failure");
    
    return 1;
}

/*===========================================================================*/
/* Safety Interlock Tests                                                     */
/*===========================================================================*/

static int test_overheat_interlock(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    run_cycles(300, 100);  /* Get to drilling */
    
    /* Inject overheat */
    asteroid_drill_set_environment(0, "temperature", 200.0f);
    run_cycles(5, 100);
    
    drill_fault_t fault;
    asteroid_drill_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, DRILL_FAULT_OVERHEAT, "Should detect overheat");
    
    drill_state_t state;
    asteroid_drill_get_state(0, &state);
    TEST_ASSERT_EQ(state, DRILL_STATE_FAULT, "Should be in FAULT state");
    
    return 1;
}

static int test_overtorque_interlock(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    run_cycles(300, 100);  /* Get to drilling */
    
    /* Inject overtorque */
    asteroid_drill_set_environment(0, "torque", 600.0f);
    run_cycles(5, 100);
    
    drill_fault_t fault;
    asteroid_drill_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, DRILL_FAULT_OVERTORQUE, "Should detect overtorque");
    
    return 1;
}

static int test_vibration_interlock(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    run_cycles(300, 100);  /* Get to drilling */
    
    /* Inject excessive vibration */
    asteroid_drill_set_environment(0, "vibration", 25.0f);
    run_cycles(5, 100);
    
    drill_fault_t fault;
    asteroid_drill_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, DRILL_FAULT_VIBRATION, "Should detect excessive vibration");
    
    return 1;
}

static int test_anchor_slip_interlock(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    run_cycles(300, 100);  /* Get to drilling */
    
    /* Inject anchor slip */
    asteroid_drill_set_environment(0, "anchor_force", 100.0f);
    run_cycles(5, 100);
    
    drill_fault_t fault;
    asteroid_drill_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, DRILL_FAULT_ANCHOR_SLIP, "Should detect anchor slip");
    
    return 1;
}

static int test_depth_limit_interlock(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    run_cycles(300, 100);  /* Get to drilling */
    
    /* Force depth limit */
    asteroid_drill_set_environment(0, "depth", 30.0f);
    run_cycles(5, 100);
    
    drill_fault_t fault;
    asteroid_drill_get_fault(0, &fault);
    TEST_ASSERT_EQ(fault, DRILL_FAULT_DEPTH_LIMIT, "Should detect depth limit");
    
    return 1;
}

/*===========================================================================*/
/* Yield Statistics Tests                                                     */
/*===========================================================================*/

static int test_yield_init(void) {
    reset_module();
    
    yield_stats_t yield;
    int result = asteroid_drill_get_yield(0, &yield);
    TEST_ASSERT_EQ(result, 0, "get_yield should succeed");
    TEST_ASSERT_EQ(yield.total_samples, 0, "Initial samples should be 0");
    TEST_ASSERT_FLOAT_EQ(yield.total_mass_kg, 0.0f, 0.01f, "Initial mass should be 0");
    
    return 1;
}

static int test_yield_after_cycle(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Complete a cycle */
    run_cycles(50000, 100);
    
    yield_stats_t yield;
    asteroid_drill_get_yield(0, &yield);
    TEST_ASSERT_GT(yield.total_samples, 0, "Should have samples");
    TEST_ASSERT_GT(yield.total_mass_kg, 0.0f, "Should have extracted mass");
    TEST_ASSERT_GT(yield.avg_purity, 0.0f, "Should have purity data");
    
    return 1;
}

/*===========================================================================*/
/* Sample Tracking Tests                                                      */
/*===========================================================================*/

static int test_sample_tracking(void) {
    reset_module();
    
    drill_config_t config = { .unit_id = 0, .target_rpm = 600.0f };
    asteroid_drill_configure(&config);
    asteroid_drill_start(0);
    
    /* Get to drilling and collect sample */
    run_cycles(50000, 100);
    
    sample_record_t sample;
    asteroid_drill_get_sample(0, &sample);
    TEST_ASSERT_TRUE(sample.valid, "Sample should be valid");
    TEST_ASSERT_GT(sample.mass_kg, 0.0f, "Sample should have mass");
    
    return 1;
}

/*===========================================================================*/
/* Telemetry Tests                                                            */
/*===========================================================================*/

static int test_telemetry_generation(void) {
    reset_module();
    
    uint8_t buffer[128];
    int len = asteroid_drill_get_telemetry(0, buffer, sizeof(buffer));
    TEST_ASSERT_GT(len, 0, "Telemetry should be generated");
    
    return 1;
}

static int test_telemetry_buffer_too_small(void) {
    reset_module();
    
    uint8_t buffer[8];  /* Too small */
    int len = asteroid_drill_get_telemetry(0, buffer, sizeof(buffer));
    TEST_ASSERT_EQ(len, -1, "Should fail with small buffer");
    
    return 1;
}

static int test_telemetry_null_buffer(void) {
    reset_module();
    
    int len = asteroid_drill_get_telemetry(0, NULL, 128);
    TEST_ASSERT_EQ(len, -1, "Should fail with NULL buffer");
    
    return 1;
}

/*===========================================================================*/
/* Multiple Drill Tests                                                       */
/*===========================================================================*/

static int test_multiple_drills(void) {
    reset_module();
    
    drill_config_t config0 = { .unit_id = 0, .target_rpm = 600.0f };
    drill_config_t config1 = { .unit_id = 1, .target_rpm = 800.0f };
    
    asteroid_drill_configure(&config0);
    asteroid_drill_configure(&config1);
    
    asteroid_drill_start(0);
    asteroid_drill_start(1);
    
    run_cycles(100, 100);
    
    drill_state_t state0, state1;
    asteroid_drill_get_state(0, &state0);
    asteroid_drill_get_state(1, &state1);
    
    TEST_ASSERT_NEQ(state0, DRILL_STATE_IDLE, "Drill 0 should be running");
    TEST_ASSERT_NEQ(state1, DRILL_STATE_IDLE, "Drill 1 should be running");
    
    return 1;
}

static int test_drills_independent_faults(void) {
    reset_module();
    
    drill_config_t config0 = { .unit_id = 0, .target_rpm = 600.0f };
    drill_config_t config1 = { .unit_id = 1, .target_rpm = 600.0f };
    
    asteroid_drill_configure(&config0);
    asteroid_drill_configure(&config1);
    
    asteroid_drill_start(0);
    asteroid_drill_start(1);
    
    run_cycles(300, 100);  /* Get to drilling */
    
    /* Fault only drill 0 */
    asteroid_drill_set_environment(0, "temperature", 200.0f);
    run_cycles(5, 100);
    
    drill_state_t state0, state1;
    asteroid_drill_get_state(0, &state0);
    asteroid_drill_get_state(1, &state1);
    
    TEST_ASSERT_EQ(state0, DRILL_STATE_FAULT, "Drill 0 should be faulted");
    TEST_ASSERT_NEQ(state1, DRILL_STATE_FAULT, "Drill 1 should not be faulted");
    
    return 1;
}

/*===========================================================================*/
/* Error Handling Tests                                                       */
/*===========================================================================*/

static int test_get_state_null_pointer(void) {
    reset_module();
    int result = asteroid_drill_get_state(0, NULL);
    TEST_ASSERT_EQ(result, -1, "Should fail with NULL state pointer");
    
    return 1;
}

static int test_get_fault_invalid_unit(void) {
    reset_module();
    drill_fault_t fault;
    int result = asteroid_drill_get_fault(99, &fault);
    TEST_ASSERT_EQ(result, -1, "Should fail with invalid unit");
    
    return 1;
}

static int test_inject_fault_invalid_channel(void) {
    reset_module();
    int result = asteroid_drill_inject_fault(0, SENSOR_TORQUE, 99);
    TEST_ASSERT_EQ(result, -1, "Should fail with invalid channel");
    
    return 1;
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void) {
    printf("========================================\n");
    printf("Asteroid Drill Control & Telemetry Tests\n");
    printf("========================================\n\n");
    
    printf("--- Module Initialization ---\n");
    RUN_TEST(test_init_success);
    RUN_TEST(test_init_double_init_fails);
    RUN_TEST(test_shutdown_and_reinit);
    
    printf("\n--- Configuration ---\n");
    RUN_TEST(test_configure_drill);
    RUN_TEST(test_configure_invalid_unit);
    RUN_TEST(test_configure_null_config);
    
    printf("\n--- Drilling Operations ---\n");
    RUN_TEST(test_start_drilling);
    RUN_TEST(test_start_already_running);
    RUN_TEST(test_start_invalid_unit);
    
    printf("\n--- State Machine ---\n");
    RUN_TEST(test_pre_check_to_anchoring);
    RUN_TEST(test_anchoring_phase);
    RUN_TEST(test_full_drilling_cycle);
    
    printf("\n--- Abort and Reset ---\n");
    RUN_TEST(test_abort_drilling);
    RUN_TEST(test_abort_idle);
    RUN_TEST(test_reset_from_fault);
    
    printf("\n--- TMR Sensor Voting ---\n");
    RUN_TEST(test_tmr_single_sensor_fault);
    RUN_TEST(test_tmr_double_sensor_fault);
    RUN_TEST(test_tmr_all_sensors_failed);
    
    printf("\n--- Safety Interlocks ---\n");
    RUN_TEST(test_overheat_interlock);
    RUN_TEST(test_overtorque_interlock);
    RUN_TEST(test_vibration_interlock);
    RUN_TEST(test_anchor_slip_interlock);
    RUN_TEST(test_depth_limit_interlock);
    
    printf("\n--- Yield Statistics ---\n");
    RUN_TEST(test_yield_init);
    RUN_TEST(test_yield_after_cycle);
    
    printf("\n--- Sample Tracking ---\n");
    RUN_TEST(test_sample_tracking);
    
    printf("\n--- Telemetry ---\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_buffer_too_small);
    RUN_TEST(test_telemetry_null_buffer);
    
    printf("\n--- Multiple Drills ---\n");
    RUN_TEST(test_multiple_drills);
    RUN_TEST(test_drills_independent_faults);
    
    printf("\n--- Error Handling ---\n");
    RUN_TEST(test_get_state_null_pointer);
    RUN_TEST(test_get_fault_invalid_unit);
    RUN_TEST(test_inject_fault_invalid_channel);
    
    printf("\n========================================\n");
    printf("Test Results: %d/%d passed (%d assertions)\n", 
           g_tests_passed, g_tests_run, g_assertions);
    printf("========================================\n");
    
    return (g_tests_passed == g_tests_run) ? 0 : 1;
}
