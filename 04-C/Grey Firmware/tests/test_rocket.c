/**
 * @file test_rocket.c
 * @brief Unit tests for Rocket Engine Telemetry & Safety Spotlight
 * 
 * Tests cover:
 * - System initialization and configuration
 * - Engine state machine transitions
 * - TMR sensor voting
 * - Safety interlock logic
 * - Abort trigger conditions
 * - Telemetry generation
 * - Launch phase progression
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Include spotlight implementation */
#include "../src/rocket/rocket_spotlight.c"

/* ============================================================================
 * Test Framework
 * ============================================================================ */

static int tests_run = 0;
static int tests_passed = 0;

#define TEST_ASSERT(cond, msg) do { \
    tests_run++; \
    if (!(cond)) { \
        printf("FAIL [%s:%d]: %s\n", __func__, __LINE__, msg); \
        return 0; \
    } \
    tests_passed++; \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) TEST_ASSERT((a) == (b), msg)
#define TEST_ASSERT_NEQ(a, b, msg) TEST_ASSERT((a) != (b), msg)
#define TEST_ASSERT_FLOAT_EQ(a, b, tol, msg) TEST_ASSERT(fabsf((a) - (b)) < (tol), msg)

#define RUN_TEST(test) do { \
    printf("Running %s...\n", #test); \
    if (test()) { \
        printf("  PASS\n"); \
    } else { \
        printf("  FAILED\n"); \
    } \
} while(0)

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

static void reset_system(void)
{
    memset(&g_rocket, 0, sizeof(g_rocket));
    rocket_init();
}

static engine_config_t make_merlin_config(void)
{
    engine_config_t cfg = {
        .type = ENGINE_TYPE_LIQUID_RP1_LOX,
        .thrust_nominal_kn = 845.0f,
        .throttle_min_pct = 40.0f,
        .throttle_max_pct = 100.0f,
        .isp_sea_level = 282.0f,
        .isp_vacuum = 311.0f,
        .chamber_pressure_nom = 97.0f,
        .mixture_ratio_nom = 2.36f,
        .ignition_delay_ms = 500,
        .shutdown_time_ms = 300,
        .throttleable = true,
        .restartable = true
    };
    return cfg;
}

static void simulate_nominal_sensors(uint8_t engine_id, float thrust_pct)
{
    float thrust_kn = 845.0f * thrust_pct;
    float thrust[3] = {thrust_kn, thrust_kn, thrust_kn};
    float temp[3] = {3400.0f, 3400.0f, 3400.0f};
    float press[3] = {97.0f, 97.0f, 97.0f};
    float vib[3] = {1.5f, 1.5f, 1.5f};
    
    rocket_update_sensors(engine_id, thrust, temp, press, vib, g_rocket.current_time_ms * 1000);
}

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

static int test_init(void)
{
    reset_system();
    
    TEST_ASSERT(g_rocket.initialized, "System should be initialized");
    TEST_ASSERT_EQ(g_rocket.engine_count, 0, "No engines configured");
    TEST_ASSERT_EQ(g_rocket.interlock.status, INTERLOCK_CLEAR, "Interlocks should be clear");
    TEST_ASSERT_EQ(g_rocket.vehicle.phase, LAUNCH_PHASE_COUNTDOWN, "Should be in countdown");
    
    return 1;
}

static int test_double_init(void)
{
    reset_system();
    
    int result = rocket_init();
    TEST_ASSERT_EQ(result, -1, "Double init should fail");
    
    return 1;
}

static int test_engine_config(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    int result = rocket_engine_config(0, &cfg);
    
    TEST_ASSERT_EQ(result, 0, "Config should succeed");
    TEST_ASSERT_EQ(g_rocket.engine_count, 1, "Should have 1 engine");
    TEST_ASSERT_EQ(g_rocket.engines[0].config.type, ENGINE_TYPE_LIQUID_RP1_LOX, "Type should match");
    TEST_ASSERT_FLOAT_EQ(g_rocket.engines[0].config.thrust_nominal_kn, 845.0f, 0.1f, "Thrust should match");
    
    return 1;
}

static int test_multi_engine_config(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    
    for (uint8_t i = 0; i < 9; i++) {
        int result = rocket_engine_config(i, &cfg);
        TEST_ASSERT_EQ(result, 0, "Config should succeed");
    }
    
    TEST_ASSERT_EQ(g_rocket.engine_count, 9, "Should have 9 engines");
    
    return 1;
}

static int test_engine_config_invalid(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    int result = rocket_engine_config(ROCKET_MAX_ENGINES, &cfg);
    
    TEST_ASSERT_EQ(result, -1, "Config with invalid ID should fail");
    
    return 1;
}

/* ============================================================================
 * Engine Arming Tests
 * ============================================================================ */

static int test_engine_arm(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    int result = rocket_engine_arm(0);
    TEST_ASSERT_EQ(result, 0, "Arm should succeed");
    TEST_ASSERT(g_rocket.engines[0].armed, "Engine should be armed");
    
    return 1;
}

static int test_engine_disarm(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    rocket_engine_arm(0);
    
    int result = rocket_engine_disarm(0);
    TEST_ASSERT_EQ(result, 0, "Disarm should succeed");
    TEST_ASSERT(!g_rocket.engines[0].armed, "Engine should be disarmed");
    
    return 1;
}

static int test_arm_invalid_engine(void)
{
    reset_system();
    
    int result = rocket_engine_arm(0);
    TEST_ASSERT_EQ(result, -1, "Arm unconfigured engine should fail");
    
    return 1;
}

/* ============================================================================
 * Engine Ignition Tests
 * ============================================================================ */

static int test_ignition_success(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    rocket_engine_arm(0);
    
    int result = rocket_engine_ignite(0);
    TEST_ASSERT_EQ(result, 0, "Ignition should succeed");
    TEST_ASSERT_EQ(g_rocket.engines[0].state, ENGINE_STATE_IGNITION, "Should be in ignition");
    TEST_ASSERT(g_rocket.engines[0].igniter_fired, "Igniter should be fired");
    
    return 1;
}

static int test_ignition_not_armed(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    int result = rocket_engine_ignite(0);
    TEST_ASSERT_EQ(result, -2, "Ignition without arming should fail");
    
    return 1;
}

static int test_ignition_interlock_violation(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    rocket_engine_arm(0);
    
    g_rocket.interlock.status = INTERLOCK_HOLD;
    
    int result = rocket_engine_ignite(0);
    TEST_ASSERT_EQ(result, -4, "Ignition with interlock should fail");
    
    return 1;
}

/* ============================================================================
 * Engine State Machine Tests
 * ============================================================================ */

static int test_engine_startup_transition(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    rocket_engine_arm(0);
    rocket_engine_ignite(0);
    
    /* Advance time past ignition */
    g_rocket.current_time_ms = 200;
    g_rocket.engines[0].state_entry_time_ms = 0;
    
    advance_engine_state(&g_rocket.engines[0]);
    
    TEST_ASSERT_EQ(g_rocket.engines[0].state, ENGINE_STATE_STARTUP, "Should transition to startup");
    
    return 1;
}

static int test_engine_mainstage_transition(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    rocket_engine_arm(0);
    rocket_engine_ignite(0);
    
    /* Set to startup state */
    g_rocket.engines[0].state = ENGINE_STATE_STARTUP;
    g_rocket.engines[0].state_entry_time_ms = 0;
    g_rocket.current_time_ms = 100;
    
    /* Simulate 95% thrust */
    simulate_nominal_sensors(0, 0.95f);
    
    advance_engine_state(&g_rocket.engines[0]);
    
    TEST_ASSERT_EQ(g_rocket.engines[0].state, ENGINE_STATE_MAINSTAGE, "Should transition to mainstage");
    TEST_ASSERT_EQ(g_rocket.engines_running, 1, "Should count as running");
    
    return 1;
}

static int test_engine_shutdown(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    rocket_engine_arm(0);
    
    /* Put engine in mainstage */
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    g_rocket.engines_running = 1;
    
    int result = rocket_engine_shutdown(0);
    TEST_ASSERT_EQ(result, 0, "Shutdown should succeed");
    TEST_ASSERT_EQ(g_rocket.engines[0].state, ENGINE_STATE_SHUTDOWN, "Should be in shutdown");
    
    return 1;
}

static int test_engine_shutdown_completion(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    g_rocket.engines[0].state = ENGINE_STATE_SHUTDOWN;
    g_rocket.engines[0].state_entry_time_ms = 0;
    g_rocket.engines_running = 1;
    g_rocket.current_time_ms = 100;
    
    /* Thrust drops to zero */
    float thrust[3] = {0.0f, 0.0f, 0.0f};
    float temp[3] = {3400.0f, 3400.0f, 3400.0f};
    float press[3] = {10.0f, 10.0f, 10.0f};
    float vib[3] = {0.1f, 0.1f, 0.1f};
    rocket_update_sensors(0, thrust, temp, press, vib, g_rocket.current_time_ms * 1000);
    
    advance_engine_state(&g_rocket.engines[0]);
    
    TEST_ASSERT_EQ(g_rocket.engines[0].state, ENGINE_STATE_SAFED, "Should be safed");
    TEST_ASSERT_EQ(g_rocket.engines_running, 0, "No engines running");
    
    return 1;
}

/* ============================================================================
 * TMR Voting Tests
 * ============================================================================ */

static int test_tmr_all_agree(void)
{
    float values[3] = {100.0f, 100.0f, 100.0f};
    float result;
    
    tmr_result_t status = tmr_vote(values, &result);
    
    TEST_ASSERT_EQ(status, TMR_AGREE_ALL, "Should show all agree");
    TEST_ASSERT_FLOAT_EQ(result, 100.0f, 0.01f, "Result should be average");
    
    return 1;
}

static int test_tmr_two_of_three(void)
{
    float values[3] = {100.0f, 100.0f, 200.0f};
    float result;
    
    tmr_result_t status = tmr_vote(values, &result);
    
    TEST_ASSERT_EQ(status, TMR_AGREE_2OF3, "Should show 2-of-3");
    TEST_ASSERT_FLOAT_EQ(result, 100.0f, 0.01f, "Result should be agreeing pair");
    
    return 1;
}

static int test_tmr_all_disagree(void)
{
    float values[3] = {100.0f, 200.0f, 300.0f};
    float result;
    
    tmr_result_t status = tmr_vote(values, &result);
    
    TEST_ASSERT(status == TMR_DISAGREE || status == TMR_FAULT, "Should show disagreement");
    TEST_ASSERT_FLOAT_EQ(result, 200.0f, 0.01f, "Result should be median");
    
    return 1;
}

static int test_tmr_sensor_update(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    tmr_sensor_t *sensor = &g_rocket.engines[0].telemetry.thrust_kn;
    
    tmr_sensor_update(sensor, 845.0f, 845.0f, 845.0f, 1000);
    
    TEST_ASSERT_EQ(sensor->vote_status, TMR_AGREE_ALL, "Vote should show agreement");
    TEST_ASSERT_FLOAT_EQ(sensor->voted_value, 845.0f, 0.1f, "Voted value should match");
    TEST_ASSERT_EQ(sensor->health, SENSOR_HEALTH_OK, "Health should be OK");
    
    return 1;
}

static int test_tmr_sensor_degraded(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    tmr_sensor_t *sensor = &g_rocket.engines[0].telemetry.thrust_kn;
    
    /* One sensor off */
    tmr_sensor_update(sensor, 845.0f, 845.0f, 900.0f, 1000);
    
    TEST_ASSERT_EQ(sensor->vote_status, TMR_AGREE_2OF3, "Vote should show 2-of-3");
    TEST_ASSERT_EQ(sensor->health, SENSOR_HEALTH_DEGRADED, "Health should be degraded");
    
    return 1;
}

/* ============================================================================
 * Safety Interlock Tests
 * ============================================================================ */

static int test_interlock_clear(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    simulate_nominal_sensors(0, 1.0f);
    
    bool clear = check_safety_interlocks();
    
    TEST_ASSERT(clear, "Interlocks should be clear");
    TEST_ASSERT_EQ(g_rocket.interlock.status, INTERLOCK_CLEAR, "Status should be clear");
    
    return 1;
}

static int test_interlock_temp_violation(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    
    /* Overheat */
    float thrust[3] = {845.0f, 845.0f, 845.0f};
    float temp[3] = {3800.0f, 3800.0f, 3800.0f}; /* Above 3700K limit */
    float press[3] = {97.0f, 97.0f, 97.0f};
    float vib[3] = {1.5f, 1.5f, 1.5f};
    rocket_update_sensors(0, thrust, temp, press, vib, g_rocket.current_time_ms * 1000);
    
    bool clear = check_safety_interlocks();
    
    TEST_ASSERT(!clear, "Interlocks should trip");
    TEST_ASSERT(g_rocket.interlock.temp_violation, "Temp violation should be set");
    TEST_ASSERT_EQ(g_rocket.interlock.status, INTERLOCK_ABORT, "Status should be abort");
    
    return 1;
}

static int test_interlock_pressure_violation(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    
    /* Overpressure */
    float thrust[3] = {845.0f, 845.0f, 845.0f};
    float temp[3] = {3400.0f, 3400.0f, 3400.0f};
    float press[3] = {130.0f, 130.0f, 130.0f}; /* Above 120 bar limit */
    float vib[3] = {1.5f, 1.5f, 1.5f};
    rocket_update_sensors(0, thrust, temp, press, vib, g_rocket.current_time_ms * 1000);
    
    bool clear = check_safety_interlocks();
    
    TEST_ASSERT(!clear, "Interlocks should trip");
    TEST_ASSERT(g_rocket.interlock.pressure_violation, "Pressure violation should be set");
    
    return 1;
}

static int test_interlock_vibration_violation(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    
    /* POGO vibration */
    float thrust[3] = {845.0f, 845.0f, 845.0f};
    float temp[3] = {3400.0f, 3400.0f, 3400.0f};
    float press[3] = {97.0f, 97.0f, 97.0f};
    float vib[3] = {10.0f, 10.0f, 10.0f}; /* Above 8g limit */
    rocket_update_sensors(0, thrust, temp, press, vib, g_rocket.current_time_ms * 1000);
    
    bool clear = check_safety_interlocks();
    
    TEST_ASSERT(!clear, "Interlocks should trip");
    TEST_ASSERT(g_rocket.interlock.vibration_violation, "Vibration violation should be set");
    
    return 1;
}

static int test_interlock_config(void)
{
    reset_system();
    
    interlock_config_t cfg = {
        .temp_abort_k = 3600.0f,
        .pressure_abort_bar = 110.0f,
        .vibration_abort_g = 6.0f,
        .thrust_min_pct = 0.5f,
        .response_time_us = 5000,
        .fts_enabled = true,
        .range_safety_enabled = true
    };
    
    int result = rocket_interlock_config(&cfg);
    TEST_ASSERT_EQ(result, 0, "Config should succeed");
    TEST_ASSERT_FLOAT_EQ(g_rocket.interlock_cfg.temp_abort_k, 3600.0f, 0.1f, "Temp limit should match");
    
    return 1;
}

/* ============================================================================
 * Abort Tests
 * ============================================================================ */

static int test_abort_trigger(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    g_rocket.engines_running = 1;
    g_rocket.current_time_ms = 1000;
    
    trigger_abort(ABORT_PAD, 0, 0x01, 3700.0f, 3800.0f);
    
    TEST_ASSERT_EQ(g_rocket.interlock.abort_type, ABORT_PAD, "Abort type should be PAD");
    TEST_ASSERT_EQ(g_rocket.interlock.status, INTERLOCK_ABORT, "Status should be abort");
    TEST_ASSERT_EQ(g_rocket.engines[0].state, ENGINE_STATE_ABORT, "Engine should abort");
    TEST_ASSERT_EQ(g_rocket.vehicle.phase, LAUNCH_PHASE_ABORT, "Vehicle should be in abort");
    
    return 1;
}

static int test_abort_no_double_trigger(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    
    /* First trigger */
    trigger_abort(ABORT_PAD, 0, 0x01, 3700.0f, 3800.0f);
    abort_type_t first_type = g_rocket.interlock.abort_type;
    
    /* Second trigger should not change type */
    trigger_abort(ABORT_FTS, 0, 0xFF, 0.0f, 0.0f);
    
    TEST_ASSERT_EQ(g_rocket.interlock.abort_type, first_type, "Abort type should not change");
    
    return 1;
}

static int test_fts_arm(void)
{
    reset_system();
    
    g_rocket.interlock_cfg.fts_enabled = true;
    
    int result = rocket_fts_arm();
    TEST_ASSERT_EQ(result, 0, "FTS arm should succeed");
    TEST_ASSERT(g_rocket.interlock.fts_armed, "FTS should be armed");
    
    return 1;
}

static int test_fts_arm_not_enabled(void)
{
    reset_system();
    
    g_rocket.interlock_cfg.fts_enabled = false;
    
    int result = rocket_fts_arm();
    TEST_ASSERT_EQ(result, -2, "FTS arm should fail when disabled");
    
    return 1;
}

static int test_fts_command(void)
{
    reset_system();
    
    g_rocket.interlock_cfg.fts_enabled = true;
    rocket_fts_arm();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    
    int result = rocket_fts_command();
    TEST_ASSERT_EQ(result, 0, "FTS command should succeed");
    TEST_ASSERT(g_rocket.interlock.fts_commanded, "FTS should be commanded");
    TEST_ASSERT_EQ(g_rocket.interlock.abort_type, ABORT_FTS, "Abort type should be FTS");
    
    return 1;
}

/* ============================================================================
 * Telemetry Tests
 * ============================================================================ */

static int test_telemetry_generation(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    simulate_nominal_sensors(0, 1.0f);
    
    uint8_t buffer[256];
    int len = rocket_get_telemetry(buffer, sizeof(buffer));
    
    TEST_ASSERT(len > 0, "Telemetry should be generated");
    TEST_ASSERT(len >= (int)sizeof(telemetry_header_t), "Should include header");
    
    telemetry_header_t *hdr = (telemetry_header_t *)buffer;
    TEST_ASSERT_EQ(hdr->sync_word, 0x1ACF, "Sync word should match");
    TEST_ASSERT_EQ(hdr->apid, 0x0100, "APID should match");
    
    return 1;
}

static int test_telemetry_sequence(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    uint8_t buffer[256];
    
    rocket_get_telemetry(buffer, sizeof(buffer));
    telemetry_header_t *hdr1 = (telemetry_header_t *)buffer;
    uint16_t seq1 = hdr1->sequence_count;
    
    rocket_get_telemetry(buffer, sizeof(buffer));
    telemetry_header_t *hdr2 = (telemetry_header_t *)buffer;
    uint16_t seq2 = hdr2->sequence_count;
    
    TEST_ASSERT_EQ(seq2, seq1 + 1, "Sequence should increment");
    
    return 1;
}

static int test_telemetry_buffer_too_small(void)
{
    reset_system();
    
    uint8_t buffer[4]; /* Too small */
    int len = rocket_get_telemetry(buffer, sizeof(buffer));
    
    TEST_ASSERT_EQ(len, -1, "Should fail with small buffer");
    
    return 1;
}

/* ============================================================================
 * Vehicle State Tests
 * ============================================================================ */

static int test_countdown_update(void)
{
    reset_system();
    
    g_rocket.vehicle.t_minus_ms = -10000; /* T-10 seconds */
    g_rocket.last_process_ms = 0;
    g_rocket.current_time_ms = 100;
    
    update_vehicle_state();
    
    TEST_ASSERT(g_rocket.vehicle.t_minus_ms > -10000, "Countdown should advance");
    
    return 1;
}

static int test_terminal_count_entry(void)
{
    reset_system();
    
    g_rocket.vehicle.t_minus_ms = -700000; /* T-11:40 */
    g_rocket.last_process_ms = 0;
    g_rocket.current_time_ms = 200000; /* Advance 200s */
    
    update_vehicle_state();
    
    /* After advance: -700000 + 200000 = -500000 (T-8:20) */
    TEST_ASSERT(g_rocket.vehicle.in_terminal_count, "Should enter terminal count");
    
    return 1;
}

static int test_nav_update(void)
{
    reset_system();
    
    int result = rocket_update_nav(10000.0f, 1500.0f, 50000.0f, 30000.0f);
    
    TEST_ASSERT_EQ(result, 0, "Nav update should succeed");
    TEST_ASSERT_FLOAT_EQ(g_rocket.vehicle.altitude_m, 10000.0f, 1.0f, "Altitude should match");
    TEST_ASSERT_FLOAT_EQ(g_rocket.vehicle.velocity_m_s, 1500.0f, 1.0f, "Velocity should match");
    TEST_ASSERT_FLOAT_EQ(g_rocket.vehicle.dynamic_pressure_pa, 30000.0f, 1.0f, "Q should match");
    
    return 1;
}

/* ============================================================================
 * Status Query Tests
 * ============================================================================ */

static int test_get_engine_state(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    
    engine_state_t state = rocket_get_engine_state(0);
    TEST_ASSERT_EQ(state, ENGINE_STATE_MAINSTAGE, "State should match");
    
    return 1;
}

static int test_get_launch_phase(void)
{
    reset_system();
    
    g_rocket.vehicle.phase = LAUNCH_PHASE_ASCENT;
    
    launch_phase_t phase = rocket_get_launch_phase();
    TEST_ASSERT_EQ(phase, LAUNCH_PHASE_ASCENT, "Phase should match");
    
    return 1;
}

static int test_get_thrust(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    simulate_nominal_sensors(0, 1.0f);
    
    float thrust = rocket_get_thrust(0);
    TEST_ASSERT_FLOAT_EQ(thrust, 845.0f, 1.0f, "Thrust should match");
    
    return 1;
}

static int test_ready_for_launch(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    g_rocket.ready_for_launch = true;
    
    bool ready = rocket_is_ready();
    TEST_ASSERT(ready, "Should be ready");
    
    return 1;
}

static int test_running_engines_count(void)
{
    reset_system();
    
    g_rocket.engines_running = 9;
    
    uint8_t count = rocket_get_running_engines();
    TEST_ASSERT_EQ(count, 9, "Count should match");
    
    return 1;
}

/* ============================================================================
 * Event Logging Tests
 * ============================================================================ */

static int test_event_logging(void)
{
    reset_system();
    
    rocket_log_event(2, 1, 0x1234, 100, 200);
    
    uint16_t count = rocket_get_event_count();
    TEST_ASSERT(count > 0, "Should have events");
    
    return 1;
}

/* ============================================================================
 * Process Loop Tests
 * ============================================================================ */

static int test_process_not_initialized(void)
{
    memset(&g_rocket, 0, sizeof(g_rocket));
    
    int result = rocket_process(1000);
    TEST_ASSERT_EQ(result, -1, "Should fail when not initialized");
    
    return 1;
}

static int test_process_normal(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    
    int result = rocket_process(100);
    TEST_ASSERT_EQ(result, 0, "Process should succeed");
    
    return 1;
}

/* ============================================================================
 * Reset and Shutdown Tests
 * ============================================================================ */

static int test_reset(void)
{
    reset_system();
    
    g_rocket.engine_count = 5;
    
    int result = rocket_reset();
    TEST_ASSERT_EQ(result, 0, "Reset should succeed");
    TEST_ASSERT_EQ(g_rocket.engine_count, 0, "Engine count should be reset");
    TEST_ASSERT(g_rocket.initialized, "Should be reinitialized");
    
    return 1;
}

static int test_shutdown(void)
{
    reset_system();
    
    engine_config_t cfg = make_merlin_config();
    rocket_engine_config(0, &cfg);
    g_rocket.engines[0].state = ENGINE_STATE_MAINSTAGE;
    
    rocket_shutdown();
    
    TEST_ASSERT(!g_rocket.initialized, "Should not be initialized");
    TEST_ASSERT_EQ(g_rocket.engines[0].state, ENGINE_STATE_SHUTDOWN, "Engine should be shutting down");
    
    return 1;
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(void)
{
    printf("=== Rocket Engine Telemetry & Safety Tests ===\n\n");
    
    /* Initialization tests */
    RUN_TEST(test_init);
    RUN_TEST(test_double_init);
    RUN_TEST(test_engine_config);
    RUN_TEST(test_multi_engine_config);
    RUN_TEST(test_engine_config_invalid);
    
    /* Arming tests */
    RUN_TEST(test_engine_arm);
    RUN_TEST(test_engine_disarm);
    RUN_TEST(test_arm_invalid_engine);
    
    /* Ignition tests */
    RUN_TEST(test_ignition_success);
    RUN_TEST(test_ignition_not_armed);
    RUN_TEST(test_ignition_interlock_violation);
    
    /* State machine tests */
    RUN_TEST(test_engine_startup_transition);
    RUN_TEST(test_engine_mainstage_transition);
    RUN_TEST(test_engine_shutdown);
    RUN_TEST(test_engine_shutdown_completion);
    
    /* TMR voting tests */
    RUN_TEST(test_tmr_all_agree);
    RUN_TEST(test_tmr_two_of_three);
    RUN_TEST(test_tmr_all_disagree);
    RUN_TEST(test_tmr_sensor_update);
    RUN_TEST(test_tmr_sensor_degraded);
    
    /* Interlock tests */
    RUN_TEST(test_interlock_clear);
    RUN_TEST(test_interlock_temp_violation);
    RUN_TEST(test_interlock_pressure_violation);
    RUN_TEST(test_interlock_vibration_violation);
    RUN_TEST(test_interlock_config);
    
    /* Abort tests */
    RUN_TEST(test_abort_trigger);
    RUN_TEST(test_abort_no_double_trigger);
    RUN_TEST(test_fts_arm);
    RUN_TEST(test_fts_arm_not_enabled);
    RUN_TEST(test_fts_command);
    
    /* Telemetry tests */
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_sequence);
    RUN_TEST(test_telemetry_buffer_too_small);
    
    /* Vehicle state tests */
    RUN_TEST(test_countdown_update);
    RUN_TEST(test_terminal_count_entry);
    RUN_TEST(test_nav_update);
    
    /* Status query tests */
    RUN_TEST(test_get_engine_state);
    RUN_TEST(test_get_launch_phase);
    RUN_TEST(test_get_thrust);
    RUN_TEST(test_ready_for_launch);
    RUN_TEST(test_running_engines_count);
    
    /* Event logging tests */
    RUN_TEST(test_event_logging);
    
    /* Process loop tests */
    RUN_TEST(test_process_not_initialized);
    RUN_TEST(test_process_normal);
    
    /* Reset/shutdown tests */
    RUN_TEST(test_reset);
    RUN_TEST(test_shutdown);
    
    printf("\n=== Results: %d/%d tests passed ===\n", tests_passed, tests_run);
    
    return (tests_passed == tests_run) ? 0 : 1;
}
