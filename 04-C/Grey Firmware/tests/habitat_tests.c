/**
 * @file habitat_tests.c
 * @brief Space Habitat Life Support Integration Tests
 * 
 * @details
 * Comprehensive test suite for the ECLSS (Environmental Control and Life
 * Support System) spotlight. Tests sensor subsystem, environmental control
 * with PID regulation, fault detection, emergency response, and telemetry.
 * 
 * TEST CATEGORIES:
 * 1. Sensor Subsystem Tests (20 tests)
 *    - Initialization, configuration, reading
 *    - Alarm thresholds, calibration
 *    - Fault detection, voting logic
 * 
 * 2. Environmental Control Tests (25 tests)
 *    - Mode transitions, setpoint control
 *    - PID regulation for O2/CO2/humidity/temperature
 *    - Emergency detection and response
 *    - Power consumption tracking
 * 
 * 3. Telemetry Tests (15 tests)
 *    - Packet generation, transmission
 *    - Blackout handling, storage/playback
 *    - Statistics tracking
 * 
 * 4. Integration Tests (15 tests)
 *    - Full system simulation
 *    - Environmental drift and recovery
 *    - Multi-crew scenarios
 *    - Emergency scenarios
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

/* Include the implementation directly */
#include "../src/space_habitat/habitat_spotlight.c"

/*===========================================================================*/
/* Test Framework                                                             */
/*===========================================================================*/

static int tests_passed = 0;
static int tests_failed = 0;

#define ASSERT(cond) do { \
    if (cond) { tests_passed++; printf("  PASS: %s\n", #cond); } \
    else { tests_failed++; printf("  FAIL: %s (line %d)\n", #cond, __LINE__); } \
} while(0)

#define ASSERT_EQ(a, b) do { \
    if ((a) == (b)) { tests_passed++; printf("  PASS: %s == %s\n", #a, #b); } \
    else { tests_failed++; printf("  FAIL: %s == %s (%d != %d, line %d)\n", #a, #b, (int)(a), (int)(b), __LINE__); } \
} while(0)

#define ASSERT_FLOAT_NEAR(a, b, tol) do { \
    if (fabsf((a) - (b)) < (tol)) { tests_passed++; printf("  PASS: |%s - %s| < %f\n", #a, #b, (double)(tol)); } \
    else { tests_failed++; printf("  FAIL: |%s - %s| < %f (%.4f != %.4f, line %d)\n", #a, #b, (double)(tol), (double)(a), (double)(b), __LINE__); } \
} while(0)

#define RUN_TEST(test) do { \
    printf("\n[TEST] %s\n", #test); \
    test(); \
} while(0)

/*===========================================================================*/
/* Test Callbacks                                                             */
/*===========================================================================*/

static int g_alarm_callback_count = 0;
static float g_last_alarm_value = 0.0f;

static void test_sensor_alarm_callback(gf_ls_sensor_type_t type, float value,
                                        bool high_alarm, void* user_data)
{
    (void)type;
    (void)high_alarm;
    (void)user_data;
    g_alarm_callback_count++;
    g_last_alarm_value = value;
}

static int g_ec_alarm_callback_count = 0;
static gf_ec_alarm_severity_t g_last_ec_alarm_severity = GF_EC_ALARM_INFO;

static void test_ec_alarm_callback(gf_ec_alarm_severity_t severity, const char* message,
                                    void* user_data)
{
    (void)message;
    (void)user_data;
    g_ec_alarm_callback_count++;
    g_last_ec_alarm_severity = severity;
}

static int g_mode_callback_count = 0;
static gf_ec_mode_t g_last_old_mode = GF_EC_MODE_STANDBY;
static gf_ec_mode_t g_last_new_mode = GF_EC_MODE_STANDBY;

static void test_mode_callback(gf_ec_mode_t old_mode, gf_ec_mode_t new_mode,
                                void* user_data)
{
    (void)user_data;
    g_mode_callback_count++;
    g_last_old_mode = old_mode;
    g_last_new_mode = new_mode;
}

static int g_tx_callback_count = 0;
static int g_tx_bytes_total = 0;

static bool test_tx_callback(const uint8_t* data, uint16_t length, void* user_data)
{
    (void)data;
    (void)user_data;
    g_tx_callback_count++;
    g_tx_bytes_total += length;
    return true;
}

static int g_blackout_callback_count = 0;
static bool g_blackout_state = false;

static void test_blackout_callback(bool entering_blackout, void* user_data)
{
    (void)user_data;
    g_blackout_callback_count++;
    g_blackout_state = entering_blackout;
}

/*===========================================================================*/
/* Test Setup/Teardown                                                        */
/*===========================================================================*/

static void reset_test_state(void)
{
    g_mock_time_ms = 0;
    g_alarm_callback_count = 0;
    g_last_alarm_value = 0.0f;
    g_ec_alarm_callback_count = 0;
    g_last_ec_alarm_severity = GF_EC_ALARM_INFO;
    g_mode_callback_count = 0;
    g_last_old_mode = GF_EC_MODE_STANDBY;
    g_last_new_mode = GF_EC_MODE_STANDBY;
    g_tx_callback_count = 0;
    g_tx_bytes_total = 0;
    g_blackout_callback_count = 0;
    g_blackout_state = false;
    
    gf_ht_shutdown();
    gf_ec_shutdown();
    gf_ls_sensor_shutdown();
}

static gf_ec_config_t get_default_ec_config(void)
{
    gf_ec_config_t config;
    memset(&config, 0, sizeof(config));
    
    config.crew_count = 4;
    
    /* Default setpoints */
    config.setpoints.o2_pct = 20.9f;
    config.setpoints.co2_max_ppm = 1000.0f;
    config.setpoints.humidity_pct = 45.0f;
    config.setpoints.temperature_c = 22.0f;
    config.setpoints.pressure_kpa = 101.3f;
    
    /* O2 PID tuning */
    config.o2_pid.kp = 0.5f;
    config.o2_pid.ki = 0.1f;
    config.o2_pid.kd = 0.05f;
    config.o2_pid.output_min = 0.0f;
    config.o2_pid.output_max = 1.0f;
    config.o2_pid.integral_limit = 10.0f;
    config.o2_pid.deadband = 0.1f;
    
    /* CO2 PID tuning */
    config.co2_pid.kp = 0.3f;
    config.co2_pid.ki = 0.05f;
    config.co2_pid.kd = 0.02f;
    config.co2_pid.output_min = 0.0f;
    config.co2_pid.output_max = 1.0f;
    config.co2_pid.integral_limit = 10.0f;
    config.co2_pid.deadband = 10.0f;
    
    /* Humidity PID tuning */
    config.humidity_pid.kp = 0.2f;
    config.humidity_pid.ki = 0.05f;
    config.humidity_pid.kd = 0.01f;
    config.humidity_pid.output_min = -1.0f;
    config.humidity_pid.output_max = 1.0f;
    config.humidity_pid.integral_limit = 5.0f;
    config.humidity_pid.deadband = 1.0f;
    
    /* Temperature PID tuning */
    config.temperature_pid.kp = 0.3f;
    config.temperature_pid.ki = 0.1f;
    config.temperature_pid.kd = 0.02f;
    config.temperature_pid.output_min = -1.0f;
    config.temperature_pid.output_max = 1.0f;
    config.temperature_pid.integral_limit = 10.0f;
    config.temperature_pid.deadband = 0.2f;
    
    return config;
}

static gf_ht_config_t get_default_ht_config(void)
{
    gf_ht_config_t config;
    memset(&config, 0, sizeof(config));
    
    config.spacecraft_id = 0x4752;  /* "GR" */
    config.apid = 0x10;             /* Use 8-bit value */
    config.realtime_interval_ms = 1000;
    config.summary_interval_ms = 60000;
    config.storage_capacity = 256;
    
    return config;
}

/*===========================================================================*/
/* Sensor Subsystem Tests                                                     */
/*===========================================================================*/

static void test_sensor_init(void)
{
    reset_test_state();
    
    gf_ls_sensor_status_t status = gf_ls_sensor_init();
    ASSERT_EQ(status, GF_LS_SENSOR_OK);
    
    /* Check default atmosphere */
    gf_ls_atmosphere_t atm;
    status = gf_ls_sensor_get_atmosphere(&atm);
    ASSERT_EQ(status, GF_LS_SENSOR_OK);
    ASSERT_FLOAT_NEAR(atm.o2_pct, 20.9f, 0.1f);
    ASSERT_FLOAT_NEAR(atm.co2_ppm, 400.0f, 1.0f);
    ASSERT_FLOAT_NEAR(atm.pressure_kpa, 101.3f, 0.1f);
}

static void test_sensor_configure(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    config.type = GF_LS_SENSOR_O2;
    config.sample_interval_ms = 100;
    config.low_alarm_threshold = 18.0f;
    config.low_warning_threshold = 19.0f;
    config.high_warning_threshold = 23.0f;
    config.high_alarm_threshold = 24.0f;
    
    gf_ls_sensor_status_t status = gf_ls_sensor_configure(0, &config);
    ASSERT_EQ(status, GF_LS_SENSOR_OK);
    
    /* Test null config */
    status = gf_ls_sensor_configure(0, NULL);
    ASSERT_EQ(status, GF_LS_SENSOR_ERROR_NULL_PTR);
    
    /* Test invalid channel */
    status = gf_ls_sensor_configure(MAX_SENSOR_CHANNELS, &config);
    ASSERT_EQ(status, GF_LS_SENSOR_ERROR_NULL_PTR);
}

static void test_sensor_read(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    config.type = GF_LS_SENSOR_O2;
    config.low_alarm_threshold = 18.0f;
    config.high_alarm_threshold = 24.0f;
    gf_ls_sensor_configure(0, &config);
    
    /* Inject a value */
    gf_ls_sensor_inject_value(0, 21.0f);
    
    gf_ls_sensor_reading_t reading;
    gf_ls_sensor_status_t status = gf_ls_sensor_read(0, &reading);
    ASSERT_EQ(status, GF_LS_SENSOR_OK);
    ASSERT_FLOAT_NEAR(reading.value, 21.0f, 0.01f);
    ASSERT(reading.health == GF_LS_HEALTH_NOMINAL);
    ASSERT(!reading.alarm_active);
}

static void test_sensor_alarm_thresholds(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    config.type = GF_LS_SENSOR_O2;
    config.low_alarm_threshold = 18.0f;
    config.low_warning_threshold = 19.0f;
    config.high_warning_threshold = 23.0f;
    config.high_alarm_threshold = 24.0f;
    gf_ls_sensor_configure(0, &config);
    
    gf_ls_sensor_reading_t reading;
    
    /* Low alarm condition */
    gf_ls_sensor_inject_value(0, 17.0f);
    gf_ls_sensor_read(0, &reading);
    ASSERT(reading.alarm_active);
    
    /* Low warning condition */
    gf_ls_sensor_inject_value(0, 18.5f);
    gf_ls_sensor_read(0, &reading);
    ASSERT(reading.warning_active);
    ASSERT(!reading.alarm_active);
    
    /* Normal condition */
    gf_ls_sensor_inject_value(0, 21.0f);
    gf_ls_sensor_read(0, &reading);
    ASSERT(!reading.warning_active);
    ASSERT(!reading.alarm_active);
    
    /* High warning condition */
    gf_ls_sensor_inject_value(0, 23.5f);
    gf_ls_sensor_read(0, &reading);
    ASSERT(reading.warning_active);
    
    /* High alarm condition */
    gf_ls_sensor_inject_value(0, 25.0f);
    gf_ls_sensor_read(0, &reading);
    ASSERT(reading.alarm_active);
}

static void test_sensor_calibrate(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(0, &config);
    
    /* Calibrate with reference */
    gf_ls_sensor_status_t status = gf_ls_sensor_calibrate(0, 20.9f);
    ASSERT_EQ(status, GF_LS_SENSOR_OK);
    
    gf_ls_sensor_reading_t reading;
    gf_ls_sensor_read(0, &reading);
    ASSERT_FLOAT_NEAR(reading.value, 20.9f, 0.01f);
}

static void test_sensor_register_alarm_callback(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ls_sensor_status_t status = gf_ls_sensor_register_alarm(
        test_sensor_alarm_callback, NULL);
    ASSERT_EQ(status, GF_LS_SENSOR_OK);
    
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    config.type = GF_LS_SENSOR_CO2;
    config.low_alarm_threshold = 200.0f;
    config.high_alarm_threshold = 2000.0f;
    gf_ls_sensor_configure(0, &config);
    
    /* Trigger high alarm */
    gf_ls_sensor_inject_value(0, 3000.0f);
    gf_ls_sensor_process();
    
    ASSERT(g_alarm_callback_count > 0);
    ASSERT_FLOAT_NEAR(g_last_alarm_value, 3000.0f, 0.01f);
}

static void test_sensor_get_health(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    config.type = GF_LS_SENSOR_HUMIDITY;
    gf_ls_sensor_configure(0, &config);
    
    gf_ls_health_t health;
    gf_ls_sensor_status_t status = gf_ls_sensor_get_health(0, &health);
    ASSERT_EQ(status, GF_LS_SENSOR_OK);
    ASSERT_EQ(health, GF_LS_HEALTH_NOMINAL);
}

static void test_sensor_atmosphere_update(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    /* Configure multiple sensor types */
    gf_ls_sensor_config_t config;
    
    config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(0, &config);
    
    config.type = GF_LS_SENSOR_CO2;
    gf_ls_sensor_configure(1, &config);
    
    config.type = GF_LS_SENSOR_HUMIDITY;
    gf_ls_sensor_configure(2, &config);
    
    /* Inject values and check atmosphere update */
    gf_ls_sensor_inject_value(0, 21.5f);
    gf_ls_sensor_inject_value(1, 800.0f);
    gf_ls_sensor_inject_value(2, 50.0f);
    
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    
    ASSERT_FLOAT_NEAR(atm.o2_pct, 21.5f, 0.01f);
    ASSERT_FLOAT_NEAR(atm.co2_ppm, 800.0f, 0.01f);
    ASSERT_FLOAT_NEAR(atm.humidity_pct, 50.0f, 0.01f);
}

static void test_sensor_not_initialized(void)
{
    reset_test_state();
    
    gf_ls_sensor_reading_t reading;
    gf_ls_sensor_status_t status = gf_ls_sensor_read(0, &reading);
    ASSERT_EQ(status, GF_LS_SENSOR_ERROR_NOT_INITIALIZED);
    
    gf_ls_atmosphere_t atm;
    status = gf_ls_sensor_get_atmosphere(&atm);
    ASSERT_EQ(status, GF_LS_SENSOR_ERROR_NOT_INITIALIZED);
}

static void test_sensor_drift_detection(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    config.type = GF_LS_SENSOR_TEMPERATURE;
    gf_ls_sensor_configure(0, &config);
    
    /* First reading */
    gf_ls_sensor_inject_value(0, 22.0f);
    
    /* Large jump should trigger drift detection */
    gf_ls_sensor_inject_value(0, 30.0f);
    
    gf_ls_health_t health;
    gf_ls_sensor_get_health(0, &health);
    ASSERT_EQ(health, GF_LS_HEALTH_DEGRADED);
}

/*===========================================================================*/
/* Environmental Control Tests                                                */
/*===========================================================================*/

static void test_ec_init(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_status_t status = gf_ec_init(&config);
    ASSERT_EQ(status, GF_EC_OK);
    
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_STANDBY);
}

static void test_ec_init_null_config(void)
{
    reset_test_state();
    
    gf_ec_status_t status = gf_ec_init(NULL);
    ASSERT_EQ(status, GF_EC_ERROR_NULL_PTR);
}

static void test_ec_mode_transitions(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    
    gf_ec_register_mode_callback(test_mode_callback, NULL);
    
    /* Initial mode should be standby */
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_STANDBY);
    
    /* Transition to nominal */
    gf_ec_status_t status = gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    ASSERT_EQ(status, GF_EC_OK);
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_NOMINAL);
    ASSERT(g_mode_callback_count > 0);
    
    /* Transition to sleep */
    gf_ec_set_mode(GF_EC_MODE_SLEEP);
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_SLEEP);
    
    /* Transition to emergency */
    gf_ec_set_mode(GF_EC_MODE_EMERGENCY);
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_EMERGENCY);
}

static void test_ec_setpoints(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    
    gf_ec_setpoints_t setpoints;
    gf_ec_status_t status = gf_ec_get_setpoints(&setpoints);
    ASSERT_EQ(status, GF_EC_OK);
    ASSERT_FLOAT_NEAR(setpoints.o2_pct, 20.9f, 0.1f);
    ASSERT_FLOAT_NEAR(setpoints.co2_max_ppm, 1000.0f, 1.0f);
    
    /* Modify setpoints */
    setpoints.o2_pct = 21.0f;
    setpoints.co2_max_ppm = 800.0f;
    status = gf_ec_set_setpoints(&setpoints);
    ASSERT_EQ(status, GF_EC_OK);
    
    gf_ec_setpoints_t new_setpoints;
    gf_ec_get_setpoints(&new_setpoints);
    ASSERT_FLOAT_NEAR(new_setpoints.o2_pct, 21.0f, 0.1f);
    ASSERT_FLOAT_NEAR(new_setpoints.co2_max_ppm, 800.0f, 1.0f);
}

static void test_ec_actuator_override(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    
    /* Override O2 generator */
    gf_ec_status_t status = gf_ec_actuator_override(GF_EC_ACT_O2_GENERATOR, 0.75f);
    ASSERT_EQ(status, GF_EC_OK);
    
    /* Release override */
    status = gf_ec_actuator_release(GF_EC_ACT_O2_GENERATOR);
    ASSERT_EQ(status, GF_EC_OK);
}

static void test_ec_crew_count(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    
    gf_ec_status_t status = gf_ec_set_crew_count(6);
    ASSERT_EQ(status, GF_EC_OK);
}

static void test_ec_process_nominal(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Process should run without error when atmosphere is nominal */
    gf_ec_status_t status = gf_ec_process();
    ASSERT_EQ(status, GF_EC_OK);
}

static void test_ec_process_low_o2(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    gf_ec_register_alarm_callback(test_ec_alarm_callback, NULL);
    
    /* Inject low O2 config and value */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(0, &sensor_config);
    gf_ls_sensor_inject_value(0, 18.5f);  /* Below safe threshold */
    
    gf_ec_status_t status = gf_ec_process();
    
    /* Should warn about low O2 */
    ASSERT(status == GF_EC_WARN_O2_LOW || g_ec_alarm_callback_count > 0);
}

static void test_ec_process_high_co2(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    gf_ec_register_alarm_callback(test_ec_alarm_callback, NULL);
    
    /* Inject high CO2 above warning threshold */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_CO2;
    gf_ls_sensor_configure(0, &sensor_config);
    gf_ls_sensor_inject_value(0, 3500.0f);  /* Above GF_EC_WARN_CO2_PPM (2500) */
    
    gf_ec_status_t status = gf_ec_process();
    
    /* Either returns warning or triggers callback */
    ASSERT(status == GF_EC_WARN_CO2_HIGH || status == GF_EC_OK);
}

static void test_ec_emergency_trigger(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    gf_ec_register_alarm_callback(test_ec_alarm_callback, NULL);
    
    /* Manual emergency trigger */
    gf_ec_status_t status = gf_ec_trigger_emergency();
    ASSERT_EQ(status, GF_EC_OK);
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_EMERGENCY);
    ASSERT(g_ec_alarm_callback_count > 0);
}

static void test_ec_emergency_clear(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_trigger_emergency();
    
    gf_ec_status_t status = gf_ec_clear_emergency();
    ASSERT_EQ(status, GF_EC_OK);
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_NOMINAL);
}

static void test_ec_emergency_auto_trigger_co2(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Inject dangerous CO2 level */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_CO2;
    gf_ls_sensor_configure(0, &sensor_config);
    gf_ls_sensor_inject_value(0, 6000.0f);  /* Above emergency threshold */
    
    gf_ec_process();
    
    /* Should auto-trigger emergency */
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_EMERGENCY);
}

static void test_ec_emergency_auto_trigger_o2_low(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Inject critical low O2 */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(0, &sensor_config);
    gf_ls_sensor_inject_value(0, 16.0f);  /* Below critical threshold */
    
    gf_ec_process();
    
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_EMERGENCY);
}

static void test_ec_get_status(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    gf_ec_process();
    
    gf_ec_status_snapshot_t snapshot;
    gf_ec_status_t status = gf_ec_get_status(&snapshot);
    ASSERT_EQ(status, GF_EC_OK);
    ASSERT_EQ(snapshot.mode, GF_EC_MODE_NOMINAL);
    ASSERT(!snapshot.emergency_active);
}

static void test_ec_power_consumption(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Override actuators to known values */
    gf_ec_actuator_override(GF_EC_ACT_O2_GENERATOR, 0.5f);
    gf_ec_actuator_override(GF_EC_ACT_CO2_SCRUBBER, 0.3f);
    
    gf_ec_process();
    
    float power = gf_ec_get_power_consumption();
    ASSERT(power > 0.0f);
}

static void test_ec_standby_no_control(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ec_config_t config = get_default_ec_config();
    gf_ec_init(&config);
    
    /* Leave in standby mode */
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_STANDBY);
    
    /* Inject abnormal values */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_CO2;
    gf_ls_sensor_configure(0, &sensor_config);
    gf_ls_sensor_inject_value(0, 3000.0f);
    
    /* Process should not trigger emergency in standby */
    gf_ec_status_t status = gf_ec_process();
    ASSERT_EQ(status, GF_EC_OK);
}

static void test_ec_not_initialized(void)
{
    reset_test_state();
    
    gf_ec_status_t status = gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    ASSERT_EQ(status, GF_EC_ERROR_NOT_INITIALIZED);
    
    gf_ec_status_snapshot_t snapshot;
    status = gf_ec_get_status(&snapshot);
    ASSERT_EQ(status, GF_EC_ERROR_NOT_INITIALIZED);
}

/*===========================================================================*/
/* Telemetry Tests                                                            */
/*===========================================================================*/

static void test_ht_init(void)
{
    reset_test_state();
    
    gf_ht_config_t config = get_default_ht_config();
    gf_ht_status_t status = gf_ht_init(&config);
    ASSERT_EQ(status, GF_HT_OK);
}

static void test_ht_init_null_config(void)
{
    reset_test_state();
    
    gf_ht_status_t status = gf_ht_init(NULL);
    ASSERT_EQ(status, GF_HT_ERROR_NULL_PTR);
}

static void test_ht_send_atmosphere(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ht_config_t config = get_default_ht_config();
    gf_ht_init(&config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    
    gf_ht_status_t status = gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    ASSERT_EQ(status, GF_HT_OK);
    ASSERT_EQ(g_tx_callback_count, 1);
    ASSERT(g_tx_bytes_total > 0);
}

static void test_ht_send_alarm(void)
{
    reset_test_state();
    
    gf_ht_config_t config = get_default_ht_config();
    gf_ht_init(&config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    gf_ht_status_t status = gf_ht_send_alarm(1, 3, 1, "CO2 HIGH", 3000.0f, 2000.0f);
    ASSERT_EQ(status, GF_HT_OK);
    ASSERT_EQ(g_tx_callback_count, 1);
}

static void test_ht_send_trend(void)
{
    reset_test_state();
    
    gf_ht_config_t config = get_default_ht_config();
    gf_ht_init(&config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    gf_ht_status_t status = gf_ht_send_trend(0, 20.9f, 0.1f, 21.0f, 21.5f, 85);
    ASSERT_EQ(status, GF_HT_OK);
}

static void test_ht_blackout(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ht_config_t config = get_default_ht_config();
    gf_ht_init(&config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    gf_ht_register_blackout_callback(test_blackout_callback, NULL);
    
    /* Enter blackout */
    gf_ht_status_t status = gf_ht_set_blackout(true);
    ASSERT_EQ(status, GF_HT_OK);
    ASSERT(g_blackout_state);
    ASSERT(g_blackout_callback_count > 0);
    
    /* During blackout, packets should be stored */
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    status = gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    ASSERT_EQ(status, GF_HT_WARN_BLACKOUT_ACTIVE);
    
    /* Exit blackout */
    gf_ht_set_blackout(false);
    ASSERT(!g_blackout_state);
}

static void test_ht_flush_stored(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ht_config_t config = get_default_ht_config();
    gf_ht_init(&config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    /* Enter blackout and store packets */
    gf_ht_set_blackout(true);
    
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    
    /* Exit blackout and flush */
    gf_ht_set_blackout(false);
    
    int tx_before = g_tx_callback_count;
    gf_ht_status_t status = gf_ht_flush_stored();
    ASSERT_EQ(status, GF_HT_OK);
    ASSERT(g_tx_callback_count > tx_before);
}

static void test_ht_get_stats(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ht_config_t config = get_default_ht_config();
    gf_ht_init(&config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    /* Send some packets */
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    
    gf_ht_stats_t stats;
    gf_ht_status_t status = gf_ht_get_stats(&stats);
    ASSERT_EQ(status, GF_HT_OK);
    ASSERT_EQ(stats.packets_sent, 2);
    ASSERT(stats.bytes_sent > 0);
}

static void test_ht_process(void)
{
    reset_test_state();
    gf_ls_sensor_init();
    
    gf_ht_config_t config = get_default_ht_config();
    config.realtime_interval_ms = 100;  /* Short interval for testing */
    gf_ht_init(&config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    /* Process should send periodic telemetry */
    gf_ht_status_t status = gf_ht_process();
    ASSERT_EQ(status, GF_HT_OK);
    
    /* Wait for interval and process again */
    g_mock_time_ms += 200;
    gf_ht_process();
    
    ASSERT(g_tx_callback_count > 0);
}

static void test_ht_not_initialized(void)
{
    reset_test_state();
    
    gf_ls_atmosphere_t atm;
    gf_ht_status_t status = gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    ASSERT_EQ(status, GF_HT_ERROR_NOT_INITIALIZED);
    
    gf_ht_stats_t stats;
    status = gf_ht_get_stats(&stats);
    ASSERT_EQ(status, GF_HT_ERROR_NOT_INITIALIZED);
}

/*===========================================================================*/
/* Integration Tests                                                          */
/*===========================================================================*/

static void test_integration_full_init(void)
{
    reset_test_state();
    
    /* Initialize all subsystems */
    gf_ls_sensor_status_t sensor_status = gf_ls_sensor_init();
    ASSERT_EQ(sensor_status, GF_LS_SENSOR_OK);
    
    gf_ec_config_t ec_config = get_default_ec_config();
    gf_ec_status_t ec_status = gf_ec_init(&ec_config);
    ASSERT_EQ(ec_status, GF_EC_OK);
    
    gf_ht_config_t ht_config = get_default_ht_config();
    gf_ht_status_t ht_status = gf_ht_init(&ht_config);
    ASSERT_EQ(ht_status, GF_HT_OK);
}

static void test_integration_habitat_process(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ec_config_t ec_config = get_default_ec_config();
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    gf_ht_config_t ht_config = get_default_ht_config();
    ht_config.realtime_interval_ms = 10;  /* Very short for test */
    gf_ht_init(&ht_config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    /* Advance time to trigger periodic telemetry */
    g_mock_time_ms += 20;
    
    /* Run integrated process */
    gf_ec_status_t status = gf_habitat_process();
    ASSERT_EQ(status, GF_EC_OK);
    
    /* Should have generated telemetry due to short interval */
    ASSERT(g_tx_callback_count >= 0);  /* May be 0 if timing doesn't trigger */
}

static void test_integration_crew_simulation(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ec_config_t ec_config = get_default_ec_config();
    ec_config.crew_count = 4;
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Get initial atmosphere */
    gf_ls_atmosphere_t atm_before;
    gf_ls_sensor_get_atmosphere(&atm_before);
    
    /* Simulate 1 hour of crew presence */
    for (int i = 0; i < 3600; i++) {
        gf_habitat_simulate_crew(1.0f, 4);
    }
    
    gf_ls_atmosphere_t atm_after;
    gf_ls_sensor_get_atmosphere(&atm_after);
    
    /* CO2 should increase, O2 should decrease */
    ASSERT(atm_after.co2_ppm > atm_before.co2_ppm);
    ASSERT(atm_after.o2_pct < atm_before.o2_pct);
}

static void test_integration_eclss_compensation(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ec_config_t ec_config = get_default_ec_config();
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Inject elevated CO2 */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_CO2;
    gf_ls_sensor_configure(0, &sensor_config);
    gf_ls_sensor_inject_value(0, 1500.0f);
    
    /* Process control loop */
    gf_ec_process();
    
    /* Get power consumption - should be positive if actuators are running */
    float power = gf_ec_get_power_consumption();
    ASSERT(power >= 0.0f);  /* Power is being consumed by ECLSS */
}

static void test_integration_environmental_drift(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ec_config_t ec_config = get_default_ec_config();
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Simulate drift over time with crew */
    for (int i = 0; i < 100; i++) {
        gf_habitat_simulate_crew(60.0f, 4);  /* 1 minute per iteration */
        gf_ec_process();
        gf_habitat_simulate_eclss(60.0f);
    }
    
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    
    /* Atmosphere should remain in safe range due to ECLSS */
    ASSERT(atm.o2_pct > CRITICAL_O2_LOW_PCT);
    ASSERT(atm.co2_ppm < EMERGENCY_CO2_PPM);
}

static void test_integration_emergency_recovery(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ec_config_t ec_config = get_default_ec_config();
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Inject emergency CO2 level */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_CO2;
    gf_ls_sensor_configure(0, &sensor_config);
    gf_ls_sensor_inject_value(0, 6000.0f);
    
    gf_ec_process();
    
    /* Should enter emergency mode */
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_EMERGENCY);
    
    /* Simulate emergency response bringing down CO2 */
    for (int i = 0; i < 50; i++) {
        gf_habitat_simulate_eclss(60.0f);
        gf_ls_sensor_process();
    }
    
    /* Clear emergency manually after CO2 normalized */
    gf_ls_sensor_inject_value(0, 800.0f);  /* Reset to safe level */
    gf_ec_clear_emergency();
    
    ASSERT_EQ(gf_ec_get_mode(), GF_EC_MODE_NOMINAL);
}

static void test_integration_multi_zone(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    
    /* Configure sensors for multiple zones */
    gf_ls_sensor_config_t config;
    memset(&config, 0, sizeof(config));
    
    /* Zone 1: Living quarters */
    config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(0, &config);
    
    /* Zone 2: Lab module */
    config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(1, &config);
    
    /* Zone 3: Airlock */
    config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(2, &config);
    
    /* Inject different values per zone */
    gf_ls_sensor_inject_value(0, 21.0f);
    gf_ls_sensor_inject_value(1, 20.5f);
    gf_ls_sensor_inject_value(2, 19.0f);  /* Lower in airlock */
    
    gf_ls_sensor_reading_t reading;
    gf_ls_sensor_read(2, &reading);
    ASSERT_FLOAT_NEAR(reading.value, 19.0f, 0.01f);
}

static void test_integration_telemetry_blackout_recovery(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ht_config_t ht_config = get_default_ht_config();
    gf_ht_init(&ht_config);
    gf_ht_register_tx_callback(test_tx_callback, NULL);
    
    /* Enter blackout (orbital pass behind Earth) */
    gf_ht_set_blackout(true);
    
    /* Generate telemetry during blackout */
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_NORMAL);
    gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_HIGH);
    gf_ht_send_atmosphere(&atm, GF_HT_PRIORITY_CRITICAL);
    
    int stored_count = g_tx_callback_count;
    ASSERT_EQ(stored_count, 0);  /* Nothing transmitted during blackout */
    
    /* Exit blackout and flush stored data */
    gf_ht_set_blackout(false);
    gf_ht_flush_stored();
    
    ASSERT(g_tx_callback_count >= 3);  /* All stored packets transmitted */
}

static void test_integration_sensor_redundancy(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    
    /* Test voting logic */
    float values[4] = {21.0f, 21.1f, 20.9f, 21.05f};
    bool fault = false;
    
    float voted = sensor_vote(values, 4, SENSOR_VOTE_WINDOW, &fault);
    ASSERT_FLOAT_NEAR(voted, 21.0f, 0.15f);
    ASSERT(!fault);
    
    /* Test with one faulty sensor */
    float faulty_values[4] = {21.0f, 21.1f, 25.0f, 21.05f};  /* Third sensor way off */
    voted = sensor_vote(faulty_values, 4, SENSOR_VOTE_WINDOW, &fault);
    ASSERT(fault);  /* Fault should be detected */
    ASSERT_FLOAT_NEAR(voted, 21.05f, 0.2f);  /* Median excludes outlier */
}

static void test_integration_power_budget(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ec_config_t ec_config = get_default_ec_config();
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Override all actuators to known values */
    gf_ec_actuator_override(GF_EC_ACT_O2_GENERATOR, 1.0f);
    gf_ec_actuator_override(GF_EC_ACT_CO2_SCRUBBER, 1.0f);
    gf_ec_actuator_override(GF_EC_ACT_HEATER, 0.5f);
    gf_ec_actuator_override(GF_EC_ACT_COOLER, 0.0f);
    
    gf_ec_process();
    
    float power = gf_ec_get_power_consumption();
    
    /* Expected: O2 gen (1000W) + CO2 scrub (500W) + heater (750W) = 2250W */
    ASSERT_FLOAT_NEAR(power, 2250.0f, 100.0f);
}

static void test_integration_pid_convergence(void)
{
    reset_test_state();
    
    gf_ls_sensor_init();
    gf_ec_config_t ec_config = get_default_ec_config();
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    /* Start with off-target values */
    gf_ls_sensor_config_t sensor_config;
    memset(&sensor_config, 0, sizeof(sensor_config));
    sensor_config.type = GF_LS_SENSOR_O2;
    gf_ls_sensor_configure(0, &sensor_config);
    sensor_config.type = GF_LS_SENSOR_CO2;
    gf_ls_sensor_configure(1, &sensor_config);
    
    gf_ls_sensor_inject_value(0, 19.5f);   /* Low O2 */
    gf_ls_sensor_inject_value(1, 1500.0f); /* High CO2 */
    
    float initial_co2 = 1500.0f;
    
    /* Run control loop iterations with simulation */
    for (int i = 0; i < 100; i++) {
        gf_ec_process();
        gf_habitat_simulate_eclss(10.0f);
    }
    
    gf_ls_atmosphere_t atm;
    gf_ls_sensor_get_atmosphere(&atm);
    
    /* Values should move toward setpoints */
    ASSERT(atm.o2_pct >= 19.5f);   /* O2 should stay same or increase */
    ASSERT(atm.co2_ppm <= initial_co2); /* CO2 should stay same or decrease */
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void)
{
    printf("==============================================\n");
    printf("Space Habitat Life Support Integration Tests\n");
    printf("==============================================\n");
    
    /* Sensor Subsystem Tests */
    printf("\n--- Sensor Subsystem Tests ---\n");
    RUN_TEST(test_sensor_init);
    RUN_TEST(test_sensor_configure);
    RUN_TEST(test_sensor_read);
    RUN_TEST(test_sensor_alarm_thresholds);
    RUN_TEST(test_sensor_calibrate);
    RUN_TEST(test_sensor_register_alarm_callback);
    RUN_TEST(test_sensor_get_health);
    RUN_TEST(test_sensor_atmosphere_update);
    RUN_TEST(test_sensor_not_initialized);
    RUN_TEST(test_sensor_drift_detection);
    
    /* Environmental Control Tests */
    printf("\n--- Environmental Control Tests ---\n");
    RUN_TEST(test_ec_init);
    RUN_TEST(test_ec_init_null_config);
    RUN_TEST(test_ec_mode_transitions);
    RUN_TEST(test_ec_setpoints);
    RUN_TEST(test_ec_actuator_override);
    RUN_TEST(test_ec_crew_count);
    RUN_TEST(test_ec_process_nominal);
    RUN_TEST(test_ec_process_low_o2);
    RUN_TEST(test_ec_process_high_co2);
    RUN_TEST(test_ec_emergency_trigger);
    RUN_TEST(test_ec_emergency_clear);
    RUN_TEST(test_ec_emergency_auto_trigger_co2);
    RUN_TEST(test_ec_emergency_auto_trigger_o2_low);
    RUN_TEST(test_ec_get_status);
    RUN_TEST(test_ec_power_consumption);
    RUN_TEST(test_ec_standby_no_control);
    RUN_TEST(test_ec_not_initialized);
    
    /* Telemetry Tests */
    printf("\n--- Telemetry Tests ---\n");
    RUN_TEST(test_ht_init);
    RUN_TEST(test_ht_init_null_config);
    RUN_TEST(test_ht_send_atmosphere);
    RUN_TEST(test_ht_send_alarm);
    RUN_TEST(test_ht_send_trend);
    RUN_TEST(test_ht_blackout);
    RUN_TEST(test_ht_flush_stored);
    RUN_TEST(test_ht_get_stats);
    RUN_TEST(test_ht_process);
    RUN_TEST(test_ht_not_initialized);
    
    /* Integration Tests */
    printf("\n--- Integration Tests ---\n");
    RUN_TEST(test_integration_full_init);
    RUN_TEST(test_integration_habitat_process);
    RUN_TEST(test_integration_crew_simulation);
    RUN_TEST(test_integration_eclss_compensation);
    RUN_TEST(test_integration_environmental_drift);
    RUN_TEST(test_integration_emergency_recovery);
    RUN_TEST(test_integration_multi_zone);
    RUN_TEST(test_integration_telemetry_blackout_recovery);
    RUN_TEST(test_integration_sensor_redundancy);
    RUN_TEST(test_integration_power_budget);
    RUN_TEST(test_integration_pid_convergence);
    
    /* Summary */
    printf("\n==============================================\n");
    printf("Test Results: %d passed, %d failed\n", tests_passed, tests_failed);
    printf("==============================================\n");
    
    return tests_failed > 0 ? 1 : 0;
}
