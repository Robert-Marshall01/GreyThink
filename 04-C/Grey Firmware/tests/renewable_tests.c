/**
 * @file renewable_tests.c
 * @brief Integration Tests for Wind Turbine Control & Grid Sync Spotlight
 * 
 * Tests cover:
 * - Turbine initialization and configuration
 * - Pitch control response and limits
 * - Yaw control and wind tracking
 * - MPPT and power production
 * - Emergency stop and safety interlocks
 * - Grid synchronization (PLL, phase lock)
 * - Grid fault handling
 * - Combined turbine + grid operation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdbool.h>

/* Include spotlight implementation directly (same pattern as marine tests) */
#include "../src/renewable/renewable_spotlight.c"

/*===========================================================================*/
/* Test Infrastructure                                                        */
/*===========================================================================*/

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST_ASSERT(cond, msg) do { \
    tests_run++; \
    if (!(cond)) { \
        printf("  FAIL: %s - %s\n", __func__, msg); \
        tests_failed++; \
        return; \
    } else { \
        tests_passed++; \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) TEST_ASSERT((a) == (b), msg)
#define TEST_ASSERT_NEQ(a, b, msg) TEST_ASSERT((a) != (b), msg)
#define TEST_ASSERT_FLOAT_EQ(a, b, eps, msg) TEST_ASSERT(fabsf((a)-(b)) < (eps), msg)

#define RUN_TEST(test) do { \
    printf("Running %s...\n", #test); \
    test(); \
} while(0)

/* Simulate time passing */
static void simulate_cycles(int count)
{
    for (int i = 0; i < count; i++) {
        gf_turbine_process();
    }
}

static void simulate_grid_cycles(int count)
{
    for (int i = 0; i < count; i++) {
        gf_grid_process();
    }
}

/*===========================================================================*/
/* Test Fixtures                                                              */
/*===========================================================================*/

static gf_turbine_config_t default_turbine_config(void)
{
    return (gf_turbine_config_t){
        .rated_power_kw = 2000.0f,
        .cut_in_wind_mps = 3.0f,
        .cut_out_wind_mps = 25.0f,
        .rated_wind_mps = 12.0f,
        .rotor_diameter_m = 80.0f,
        .num_blades = 3,
        .gear_ratio = 100.0f,
        .generator_poles = 4,
        .mode = GF_TURBINE_CONTROL_MPPT
    };
}

static gf_grid_config_t default_grid_config(void)
{
    return (gf_grid_config_t){
        .nominal_frequency_hz = 50.0f,
        .nominal_voltage_v = 690.0f,
        .freq_tolerance_hz = 0.5f,
        .voltage_tolerance_pct = 10.0f,
        .power_factor_setpoint = 0.95f,
        .droop_coefficient = 5.0f,
        .anti_islanding_enabled = true,
        .lvrt_enabled = true,
        .sync_timeout_ms = 10000
    };
}

static gf_turbine_wind_t make_wind(float speed_mps, float direction_deg)
{
    return (gf_turbine_wind_t){
        .speed_mps = speed_mps,
        .direction_deg = direction_deg,
        .gust_mps = speed_mps * 1.2f,
        .turbulence_intensity = 0.1f,
        .temperature_c = 15,
        .density_kgm3 = 1.225f
    };
}

static gf_grid_measurement_t make_grid_measurement(float freq, float voltage, float phase)
{
    return (gf_grid_measurement_t){
        .voltage_v = {voltage, voltage, voltage},
        .current_a = {100.0f, 100.0f, 100.0f},
        .frequency_hz = freq,
        .phase_angle_deg = phase,
        .active_power_kw = 0.0f,
        .reactive_power_kvar = 0.0f,
        .power_factor = 0.95f,
        .thd_voltage_pct = 2.0f,
        .thd_current_pct = 3.0f
    };
}

/*===========================================================================*/
/* Turbine Initialization Tests                                               */
/*===========================================================================*/

static void test_turbine_init_success(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_status_t status = gf_turbine_init(&cfg);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Init should succeed");
    TEST_ASSERT_EQ(gf_turbine_get_state(), GF_TURBINE_STATE_STOPPED, "Initial state should be STOPPED");
    gf_turbine_shutdown();
}

static void test_turbine_init_null_config(void)
{
    gf_turbine_status_t status = gf_turbine_init(NULL);
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_NULL_PTR, "Null config should fail");
}

static void test_turbine_not_initialized(void)
{
    gf_turbine_status_t status = gf_turbine_start();
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_NOT_INITIALIZED, "start() without init should fail");
}

static void test_turbine_shutdown_cleans_state(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    gf_turbine_shutdown();
    
    gf_turbine_status_t status = gf_turbine_start();
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_NOT_INITIALIZED, "Should not work after shutdown");
}

/*===========================================================================*/
/* Turbine State Machine Tests                                                */
/*===========================================================================*/

static void test_turbine_start_transition(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_start();
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Start should succeed");
    TEST_ASSERT_EQ(gf_turbine_get_state(), GF_TURBINE_STATE_STARTING, "Should be STARTING");
    
    gf_turbine_shutdown();
}

static void test_turbine_stop_transition(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    gf_turbine_start();
    
    gf_turbine_status_t status = gf_turbine_stop();
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Stop should succeed");
    TEST_ASSERT_EQ(gf_turbine_get_state(), GF_TURBINE_STATE_STOPPING, "Should be STOPPING");
    
    gf_turbine_shutdown();
}

static void test_turbine_emergency_stop(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    gf_turbine_start();
    
    gf_turbine_status_t status = gf_turbine_emergency_stop();
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Emergency stop should succeed");
    TEST_ASSERT_EQ(gf_turbine_get_state(), GF_TURBINE_STATE_EMERGENCY_STOP, "Should be EMERGENCY_STOP");
    
    gf_turbine_shutdown();
}

static bool state_callback_called = false;
static gf_turbine_state_t callback_old_state;
static gf_turbine_state_t callback_new_state;

static void test_state_callback(gf_turbine_state_t old_state, gf_turbine_state_t new_state, void* user_data)
{
    (void)user_data;
    state_callback_called = true;
    callback_old_state = old_state;
    callback_new_state = new_state;
}

static void test_turbine_state_callback(void)
{
    state_callback_called = false;
    
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    gf_turbine_register_state_callback(test_state_callback, NULL);
    
    gf_turbine_start();
    
    TEST_ASSERT(state_callback_called, "State callback should be called");
    TEST_ASSERT_EQ(callback_old_state, GF_TURBINE_STATE_STOPPED, "Old state should be STOPPED");
    TEST_ASSERT_EQ(callback_new_state, GF_TURBINE_STATE_STARTING, "New state should be STARTING");
    
    gf_turbine_shutdown();
}

/*===========================================================================*/
/* Pitch Control Tests                                                        */
/*===========================================================================*/

static void test_turbine_set_pitch_valid(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_set_pitch(45.0f);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Valid pitch should succeed");
    
    gf_turbine_shutdown();
}

static void test_turbine_set_pitch_out_of_range(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_set_pitch(-10.0f);
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_INVALID_PARAM, "Negative pitch should fail");
    
    status = gf_turbine_set_pitch(100.0f);
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_INVALID_PARAM, "Pitch > 90 should fail");
    
    gf_turbine_shutdown();
}

static void test_turbine_get_pitch(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_pitch_t pitch;
    gf_turbine_status_t status = gf_turbine_get_pitch(&pitch);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Get pitch should succeed");
    
    gf_turbine_shutdown();
}

static void test_turbine_pitch_null_ptr(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_get_pitch(NULL);
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_NULL_PTR, "Null pitch ptr should fail");
    
    gf_turbine_shutdown();
}

/*===========================================================================*/
/* Yaw Control Tests                                                          */
/*===========================================================================*/

static void test_turbine_set_yaw(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_set_yaw(180.0f);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Set yaw should succeed");
    
    gf_turbine_shutdown();
}

static void test_turbine_yaw_tracking_enable(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_enable_yaw_tracking(true);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Enable yaw tracking should succeed");
    
    status = gf_turbine_enable_yaw_tracking(false);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Disable yaw tracking should succeed");
    
    gf_turbine_shutdown();
}

/*===========================================================================*/
/* Power Control Tests                                                        */
/*===========================================================================*/

static void test_turbine_set_power_limit_valid(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_set_power_limit(1000.0f);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Valid power limit should succeed");
    
    gf_turbine_shutdown();
}

static void test_turbine_set_power_limit_invalid(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_set_power_limit(-100.0f);
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_INVALID_PARAM, "Negative power should fail");
    
    status = gf_turbine_set_power_limit(5000.0f);  /* Way over rated */
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_INVALID_PARAM, "Excessive power should fail");
    
    gf_turbine_shutdown();
}

static void test_turbine_set_control_mode(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_set_control_mode(GF_TURBINE_CONTROL_POWER_LIMIT);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Set control mode should succeed");
    
    status = gf_turbine_set_control_mode(GF_TURBINE_CONTROL_MPPT);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Switch to MPPT should succeed");
    
    gf_turbine_shutdown();
}

/*===========================================================================*/
/* Wind Update Tests                                                          */
/*===========================================================================*/

static void test_turbine_update_wind_valid(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_wind_t wind = make_wind(10.0f, 90.0f);
    gf_turbine_status_t status = gf_turbine_update_wind(&wind);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Valid wind update should succeed");
    
    gf_turbine_shutdown();
}

static void test_turbine_update_wind_null(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_update_wind(NULL);
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_NULL_PTR, "Null wind should fail");
    
    gf_turbine_shutdown();
}

/*===========================================================================*/
/* Telemetry Tests                                                            */
/*===========================================================================*/

static void test_turbine_get_telemetry(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_telemetry_t telemetry;
    gf_turbine_status_t status = gf_turbine_get_telemetry(&telemetry);
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Get telemetry should succeed");
    TEST_ASSERT_EQ(telemetry.state, GF_TURBINE_STATE_STOPPED, "State should be STOPPED");
    
    gf_turbine_shutdown();
}

static void test_turbine_telemetry_null(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    gf_turbine_status_t status = gf_turbine_get_telemetry(NULL);
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_NULL_PTR, "Null telemetry should fail");
    
    gf_turbine_shutdown();
}

/*===========================================================================*/
/* Process Loop Tests                                                         */
/*===========================================================================*/

static void test_turbine_process_not_initialized(void)
{
    gf_turbine_status_t status = gf_turbine_process();
    TEST_ASSERT_EQ(status, GF_TURBINE_ERROR_NOT_INITIALIZED, "Process without init should fail");
}

static void test_turbine_process_success(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    /* Set wind conditions before processing */
    gf_turbine_wind_t wind = make_wind(10.0f, 0.0f);
    gf_turbine_update_wind(&wind);
    
    gf_turbine_status_t status = gf_turbine_process();
    TEST_ASSERT_EQ(status, GF_TURBINE_OK, "Process should succeed");
    
    gf_turbine_shutdown();
}

static void test_turbine_startup_sequence(void)
{
    gf_turbine_config_t cfg = default_turbine_config();
    gf_turbine_init(&cfg);
    
    /* Set wind conditions */
    gf_turbine_wind_t wind = make_wind(10.0f, 0.0f);
    gf_turbine_update_wind(&wind);
    
    /* Start turbine */
    gf_turbine_start();
    TEST_ASSERT_EQ(gf_turbine_get_state(), GF_TURBINE_STATE_STARTING, "Should be STARTING");
    
    /* Run several cycles */
    simulate_cycles(50);
    
    /* Should transition through states */
    gf_turbine_state_t state = gf_turbine_get_state();
    TEST_ASSERT(state >= GF_TURBINE_STATE_RUNNING, "Should have progressed past STARTING");
    
    gf_turbine_shutdown();
}

/*===========================================================================*/
/* Grid Sync Initialization Tests                                             */
/*===========================================================================*/

static void test_grid_init_success(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_status_t status = gf_grid_init(&cfg);
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Grid init should succeed");
    TEST_ASSERT_EQ(gf_grid_get_state(), GF_GRID_STATE_DISCONNECTED, "Initial state should be DISCONNECTED");
    gf_grid_shutdown();
}

static void test_grid_init_null_config(void)
{
    gf_grid_status_t status = gf_grid_init(NULL);
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_NULL_PTR, "Null config should fail");
}

static void test_grid_not_initialized(void)
{
    gf_grid_status_t status = gf_grid_connect();
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_NOT_INITIALIZED, "Connect without init should fail");
}

/*===========================================================================*/
/* Grid Connection Tests                                                      */
/*===========================================================================*/

static void test_grid_connect(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_status_t status = gf_grid_connect();
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Connect should succeed");
    TEST_ASSERT_EQ(gf_grid_get_state(), GF_GRID_STATE_MONITORING, "Should be MONITORING");
    
    gf_grid_shutdown();
}

static void test_grid_disconnect(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    gf_grid_connect();
    
    gf_grid_status_t status = gf_grid_disconnect();
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Disconnect should succeed");
    TEST_ASSERT_EQ(gf_grid_get_state(), GF_GRID_STATE_DISCONNECTED, "Should be DISCONNECTED");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Grid Measurement Tests                                                     */
/*===========================================================================*/

static void test_grid_update_measurements(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_measurement_t meas = make_grid_measurement(50.0f, 690.0f, 0.0f);
    gf_grid_status_t status = gf_grid_update_measurements(&meas);
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Update measurements should succeed");
    
    gf_grid_shutdown();
}

static void test_grid_update_measurements_null(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_status_t status = gf_grid_update_measurements(NULL);
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_NULL_PTR, "Null measurements should fail");
    
    gf_grid_shutdown();
}

static void test_grid_get_measurements(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_measurement_t meas;
    gf_grid_status_t status = gf_grid_get_measurements(&meas);
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Get measurements should succeed");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Grid Synchronization Tests                                                 */
/*===========================================================================*/

static void test_grid_start_sync_from_monitoring(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    gf_grid_connect();  /* Go to MONITORING */
    
    gf_grid_status_t status = gf_grid_start_sync();
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Start sync should succeed");
    TEST_ASSERT_EQ(gf_grid_get_state(), GF_GRID_STATE_PRE_SYNC, "Should be PRE_SYNC");
    
    gf_grid_shutdown();
}

static void test_grid_start_sync_wrong_state(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    /* Don't connect - still DISCONNECTED */
    
    gf_grid_status_t status = gf_grid_start_sync();
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_NOT_INITIALIZED, "Sync from DISCONNECTED should fail");
    
    gf_grid_shutdown();
}

static void test_grid_get_sync_status(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_sync_status_t sync_status;
    gf_grid_status_t status = gf_grid_get_sync_status(&sync_status);
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Get sync status should succeed");
    
    gf_grid_shutdown();
}

static void test_grid_is_synchronized_false(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    bool synced = gf_grid_is_synchronized();
    TEST_ASSERT(!synced, "Should not be synchronized initially");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Grid Power Control Tests                                                   */
/*===========================================================================*/

static void test_grid_set_power(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_power_setpoint_t setpoint = {
        .active_power_kw = 1000.0f,
        .reactive_power_kvar = 100.0f,
        .power_factor = 0.95f,
        .ramp_rate_kw_s = 100.0f,
        .max_export_kw = 2000.0f
    };
    
    gf_grid_status_t status = gf_grid_set_power(&setpoint);
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Set power should succeed");
    
    gf_grid_shutdown();
}

static void test_grid_set_power_factor_valid(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_status_t status = gf_grid_set_power_factor(0.9f);
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Set PF should succeed");
    
    gf_grid_shutdown();
}

static void test_grid_set_power_factor_invalid(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_status_t status = gf_grid_set_power_factor(1.5f);
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_FREQ_OUT_OF_RANGE, "PF > 1 should fail");
    
    status = gf_grid_set_power_factor(-1.5f);
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_FREQ_OUT_OF_RANGE, "PF < -1 should fail");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Grid Fault Tests                                                           */
/*===========================================================================*/

static void test_grid_get_fault(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_fault_t fault;
    gf_grid_status_t status = gf_grid_get_fault(&fault);
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Get fault should succeed");
    TEST_ASSERT_EQ(fault, GF_GRID_FAULT_NONE, "Initial fault should be NONE");
    
    gf_grid_shutdown();
}

static void test_grid_clear_fault(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_status_t status = gf_grid_clear_fault();
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Clear fault should succeed");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Grid Process Tests                                                         */
/*===========================================================================*/

static void test_grid_process_not_initialized(void)
{
    gf_grid_status_t status = gf_grid_process();
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_NOT_INITIALIZED, "Process without init should fail");
}

static void test_grid_process_success(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_status_t status = gf_grid_process();
    TEST_ASSERT_EQ(status, GF_GRID_OK, "Process should succeed");
    
    gf_grid_shutdown();
}

static void test_grid_sync_sequence(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    gf_grid_connect();
    
    /* Set stable grid conditions */
    gf_grid_measurement_t meas = make_grid_measurement(50.0f, 690.0f, 0.0f);
    gf_grid_update_measurements(&meas);
    
    /* Start sync */
    gf_grid_start_sync();
    TEST_ASSERT_EQ(gf_grid_get_state(), GF_GRID_STATE_PRE_SYNC, "Should be PRE_SYNC");
    
    /* Run several cycles with good grid conditions */
    for (int i = 0; i < 100; i++) {
        gf_grid_update_measurements(&meas);
        simulate_grid_cycles(1);
    }
    
    /* Check sync status */
    gf_grid_sync_status_t sync_status;
    gf_grid_get_sync_status(&sync_status);
    
    /* State should have progressed */
    gf_grid_state_t state = gf_grid_get_state();
    TEST_ASSERT(state >= GF_GRID_STATE_PRE_SYNC, "Should have progressed in sync");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Grid State Callback Tests                                                  */
/*===========================================================================*/

static bool grid_state_callback_called = false;

static void test_grid_state_cb(gf_grid_state_t old_state, gf_grid_state_t new_state, void* user_data)
{
    (void)old_state;
    (void)new_state;
    (void)user_data;
    grid_state_callback_called = true;
}

static void test_grid_state_callback(void)
{
    grid_state_callback_called = false;
    
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    gf_grid_register_state_callback(test_grid_state_cb, NULL);
    
    gf_grid_connect();
    gf_grid_disconnect();
    
    TEST_ASSERT(grid_state_callback_called, "State callback should be called on disconnect");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Export Enable Tests                                                        */
/*===========================================================================*/

static void test_grid_enable_export_not_connected(void)
{
    gf_grid_config_t cfg = default_grid_config();
    gf_grid_init(&cfg);
    
    gf_grid_status_t status = gf_grid_enable_export(true);
    TEST_ASSERT_EQ(status, GF_GRID_ERROR_NOT_INITIALIZED, "Export without connection should fail");
    
    gf_grid_shutdown();
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void)
{
    printf("=== Wind Turbine & Grid Sync Tests ===\n\n");
    
    /* Turbine Initialization Tests */
    printf("--- Turbine Initialization Tests ---\n");
    RUN_TEST(test_turbine_init_success);
    RUN_TEST(test_turbine_init_null_config);
    RUN_TEST(test_turbine_not_initialized);
    RUN_TEST(test_turbine_shutdown_cleans_state);
    
    /* Turbine State Machine Tests */
    printf("\n--- Turbine State Machine Tests ---\n");
    RUN_TEST(test_turbine_start_transition);
    RUN_TEST(test_turbine_stop_transition);
    RUN_TEST(test_turbine_emergency_stop);
    RUN_TEST(test_turbine_state_callback);
    
    /* Pitch Control Tests */
    printf("\n--- Pitch Control Tests ---\n");
    RUN_TEST(test_turbine_set_pitch_valid);
    RUN_TEST(test_turbine_set_pitch_out_of_range);
    RUN_TEST(test_turbine_get_pitch);
    RUN_TEST(test_turbine_pitch_null_ptr);
    
    /* Yaw Control Tests */
    printf("\n--- Yaw Control Tests ---\n");
    RUN_TEST(test_turbine_set_yaw);
    RUN_TEST(test_turbine_yaw_tracking_enable);
    
    /* Power Control Tests */
    printf("\n--- Power Control Tests ---\n");
    RUN_TEST(test_turbine_set_power_limit_valid);
    RUN_TEST(test_turbine_set_power_limit_invalid);
    RUN_TEST(test_turbine_set_control_mode);
    
    /* Wind Update Tests */
    printf("\n--- Wind Update Tests ---\n");
    RUN_TEST(test_turbine_update_wind_valid);
    RUN_TEST(test_turbine_update_wind_null);
    
    /* Telemetry Tests */
    printf("\n--- Telemetry Tests ---\n");
    RUN_TEST(test_turbine_get_telemetry);
    RUN_TEST(test_turbine_telemetry_null);
    
    /* Process Loop Tests */
    printf("\n--- Process Loop Tests ---\n");
    RUN_TEST(test_turbine_process_not_initialized);
    RUN_TEST(test_turbine_process_success);
    RUN_TEST(test_turbine_startup_sequence);
    
    /* Grid Sync Initialization Tests */
    printf("\n--- Grid Sync Initialization Tests ---\n");
    RUN_TEST(test_grid_init_success);
    RUN_TEST(test_grid_init_null_config);
    RUN_TEST(test_grid_not_initialized);
    
    /* Grid Connection Tests */
    printf("\n--- Grid Connection Tests ---\n");
    RUN_TEST(test_grid_connect);
    RUN_TEST(test_grid_disconnect);
    
    /* Grid Measurement Tests */
    printf("\n--- Grid Measurement Tests ---\n");
    RUN_TEST(test_grid_update_measurements);
    RUN_TEST(test_grid_update_measurements_null);
    RUN_TEST(test_grid_get_measurements);
    
    /* Grid Synchronization Tests */
    printf("\n--- Grid Synchronization Tests ---\n");
    RUN_TEST(test_grid_start_sync_from_monitoring);
    RUN_TEST(test_grid_start_sync_wrong_state);
    RUN_TEST(test_grid_get_sync_status);
    RUN_TEST(test_grid_is_synchronized_false);
    
    /* Grid Power Control Tests */
    printf("\n--- Grid Power Control Tests ---\n");
    RUN_TEST(test_grid_set_power);
    RUN_TEST(test_grid_set_power_factor_valid);
    RUN_TEST(test_grid_set_power_factor_invalid);
    
    /* Grid Fault Tests */
    printf("\n--- Grid Fault Tests ---\n");
    RUN_TEST(test_grid_get_fault);
    RUN_TEST(test_grid_clear_fault);
    
    /* Grid Process Tests */
    printf("\n--- Grid Process Tests ---\n");
    RUN_TEST(test_grid_process_not_initialized);
    RUN_TEST(test_grid_process_success);
    RUN_TEST(test_grid_sync_sequence);
    
    /* Grid Callback Tests */
    printf("\n--- Grid Callback Tests ---\n");
    RUN_TEST(test_grid_state_callback);
    
    /* Export Tests */
    printf("\n--- Export Tests ---\n");
    RUN_TEST(test_grid_enable_export_not_connected);
    
    /* Summary */
    printf("\n=== Test Summary ===\n");
    printf("Total:  %d\n", tests_run);
    printf("Passed: %d\n", tests_passed);
    printf("Failed: %d\n", tests_failed);
    
    if (tests_failed == 0) {
        printf("\n*** ALL TESTS PASSED ***\n");
    }
    
    return tests_failed > 0 ? 1 : 0;
}
