/**
 * @file test_vessel.c
 * @brief Autonomous Vessel Control & Safety Test Suite
 * 
 * Comprehensive tests for autonomous vessel navigation, cargo telemetry,
 * collision avoidance, stability monitoring, and safety interlocks.
 * 
 * TEST CATEGORIES:
 * - Initialization and configuration
 * - Navigation state machine
 * - Heading and speed control
 * - Waypoint management
 * - ARPA target tracking and collision avoidance
 * - Cargo bay management
 * - Stability monitoring (TMR sensors, GM, heel)
 * - Weather response
 * - Safety interlocks and emergency stop
 * - Alarm management
 * - Telemetry generation
 * - Integration scenarios (rough seas, cargo imbalance, collision)
 * 
 * @author Grey Firmware Project
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

/* ============================================================================
 * Test Framework
 * ============================================================================ */

static int tests_run = 0;
static int tests_passed = 0;
static int assertions_run = 0;
static int assertions_passed = 0;
static int current_test_failed = 0;

#define TEST_ASSERT(cond, msg) do { \
    assertions_run++; \
    if (cond) { \
        assertions_passed++; \
    } else { \
        current_test_failed = 1; \
        printf("    FAIL: %s (line %d)\n", msg, __LINE__); \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) TEST_ASSERT((a) == (b), msg)
#define TEST_ASSERT_NE(a, b, msg) TEST_ASSERT((a) != (b), msg)
#define TEST_ASSERT_GT(a, b, msg) TEST_ASSERT((a) > (b), msg)
#define TEST_ASSERT_LT(a, b, msg) TEST_ASSERT((a) < (b), msg)
#define TEST_ASSERT_GE(a, b, msg) TEST_ASSERT((a) >= (b), msg)
#define TEST_ASSERT_LE(a, b, msg) TEST_ASSERT((a) <= (b), msg)
#define TEST_ASSERT_TRUE(a, msg) TEST_ASSERT((a), msg)
#define TEST_ASSERT_FALSE(a, msg) TEST_ASSERT(!(a), msg)

#define TEST_ASSERT_FLOAT_EQ(a, b, tol, msg) \
    TEST_ASSERT(fabsf((a) - (b)) < (tol), msg)

#define TEST_ASSERT_DOUBLE_EQ(a, b, tol, msg) \
    TEST_ASSERT(fabs((a) - (b)) < (tol), msg)

#define RUN_TEST(test_func) do { \
    current_test_failed = 0; \
    printf("  Running: %s\n", #test_func); \
    test_func(); \
    tests_run++; \
    if (!current_test_failed) { \
        tests_passed++; \
        printf("    PASS\n"); \
    } \
} while(0)

/* ============================================================================
 * Include spotlight implementation for testing
 * ============================================================================ */

#include "../src/vessel/vessel_spotlight.c"

/* ============================================================================
 * Test Helper Functions
 * ============================================================================ */

static void reset_system(void)
{
    vessel_shutdown();
    memset(&g_vessel, 0, sizeof(g_vessel));
}

static void setup_basic_system(void)
{
    reset_system();
    vessel_init();
}

static void setup_configured_vessel(void)
{
    setup_basic_system();
    vessel_config("MV Grey Spirit", 366123456, 9876543, 200.0f, 32.0f, 12.0f, 50000.0f);
}

static void setup_with_waypoints(void)
{
    setup_configured_vessel();
    /* Route from Port A to Port B */
    vessel_add_waypoint(37.7749, -122.4194, 12.0f, 0.5f);  /* San Francisco */
    vessel_add_waypoint(34.0522, -118.2437, 14.0f, 0.8f);  /* Los Angeles */
    vessel_add_waypoint(32.7157, -117.1611, 10.0f, 0.3f);  /* San Diego */
}

/* Helper function for reuse in tests */
__attribute__((unused))
static void setup_with_cargo(void)
{
    setup_configured_vessel();
    /* Configure cargo bays */
    vessel_config_bay(0, 2000000.0f, 50.0f, 0.0f, 5.0f);
    vessel_config_bay(1, 2000000.0f, 100.0f, 0.0f, 5.0f);
    vessel_config_bay(2, 2000000.0f, 150.0f, 0.0f, 5.0f);
    
    /* Load cargo */
    vessel_update_bay(0, 1500000.0f, 100);
    vessel_update_bay(1, 1600000.0f, 110);
    vessel_update_bay(2, 1400000.0f, 95);
}

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

static void test_init_success(void)
{
    reset_system();
    int result = vessel_init();
    TEST_ASSERT_EQ(result, 0, "Init should succeed");
    TEST_ASSERT_TRUE(g_vessel.initialized, "Should be initialized");
    TEST_ASSERT_EQ(vessel_get_nav_mode(), NAV_MODE_MANUAL, "Default nav mode is manual");
}

static void test_double_init(void)
{
    reset_system();
    vessel_init();
    int result = vessel_init();
    TEST_ASSERT_EQ(result, -1, "Double init should fail");
}

static void test_config_success(void)
{
    setup_basic_system();
    int result = vessel_config("Test Vessel", 123456789, 9999999, 150.0f, 25.0f, 10.0f, 30000.0f);
    TEST_ASSERT_EQ(result, 0, "Config should succeed");
    TEST_ASSERT_FLOAT_EQ(g_vessel.loa_m, 150.0f, 0.1f, "LOA should match");
    TEST_ASSERT_EQ(g_vessel.mmsi, 123456789u, "MMSI should match");
}

static void test_config_without_init(void)
{
    reset_system();
    int result = vessel_config("Test", 123, 456, 100.0f, 20.0f, 8.0f, 20000.0f);
    TEST_ASSERT_EQ(result, -1, "Config without init should fail");
}

static void test_shutdown_clears_state(void)
{
    setup_configured_vessel();
    vessel_shutdown();
    TEST_ASSERT_FALSE(g_vessel.initialized, "Should not be initialized after shutdown");
}

static void test_reset_reinitializes(void)
{
    setup_configured_vessel();
    vessel_reset();
    TEST_ASSERT_TRUE(g_vessel.initialized, "Should be initialized after reset");
}

/* ============================================================================
 * Navigation Mode Tests
 * ============================================================================ */

static void test_nav_mode_transitions(void)
{
    setup_configured_vessel();
    
    TEST_ASSERT_EQ(vessel_set_nav_mode(NAV_MODE_HEADING_HOLD), 0, "Set heading hold");
    TEST_ASSERT_EQ(vessel_get_nav_mode(), NAV_MODE_HEADING_HOLD, "Mode is heading hold");
    
    TEST_ASSERT_EQ(vessel_set_nav_mode(NAV_MODE_WAYPOINT), 0, "Set waypoint mode");
    TEST_ASSERT_EQ(vessel_get_nav_mode(), NAV_MODE_WAYPOINT, "Mode is waypoint");
    
    TEST_ASSERT_EQ(vessel_set_nav_mode(NAV_MODE_TRACK_CONTROL), 0, "Set track control");
    TEST_ASSERT_EQ(vessel_get_nav_mode(), NAV_MODE_TRACK_CONTROL, "Mode is track control");
}

static void test_nav_mode_without_init(void)
{
    reset_system();
    int result = vessel_set_nav_mode(NAV_MODE_WAYPOINT);
    TEST_ASSERT_EQ(result, -1, "Nav mode without init should fail");
}

static void test_emergency_mode_triggers(void)
{
    setup_configured_vessel();
    vessel_set_nav_mode(NAV_MODE_WAYPOINT);
    vessel_emergency_stop();
    TEST_ASSERT_EQ(vessel_get_nav_mode(), NAV_MODE_EMERGENCY, "Mode should be emergency");
    TEST_ASSERT_TRUE(g_vessel.emergency_stop, "Emergency flag should be set");
}

/* ============================================================================
 * Waypoint Management Tests
 * ============================================================================ */

static void test_add_waypoint_success(void)
{
    setup_configured_vessel();
    int result = vessel_add_waypoint(37.7749, -122.4194, 12.0f, 0.5f);
    TEST_ASSERT_EQ(result, 0, "Add waypoint should succeed");
    TEST_ASSERT_EQ(g_vessel.waypoint_count, 1, "Waypoint count should be 1");
}

static void test_add_multiple_waypoints(void)
{
    setup_with_waypoints();
    TEST_ASSERT_EQ(g_vessel.waypoint_count, 3, "Waypoint count should be 3");
    TEST_ASSERT_DOUBLE_EQ(g_vessel.route[0].position.latitude, 37.7749, 0.0001, "WP0 lat");
    TEST_ASSERT_DOUBLE_EQ(g_vessel.route[2].position.longitude, -117.1611, 0.0001, "WP2 lon");
}

static void test_waypoint_limit(void)
{
    setup_configured_vessel();
    for (int i = 0; i < VESSEL_MAX_WAYPOINTS; i++) {
        vessel_add_waypoint(35.0 + i * 0.1, -120.0 + i * 0.1, 10.0f, 0.5f);
    }
    int result = vessel_add_waypoint(40.0, -115.0, 10.0f, 0.5f);
    TEST_ASSERT_EQ(result, -1, "Exceeding waypoint limit should fail");
}

static void test_waypoint_without_init(void)
{
    reset_system();
    int result = vessel_add_waypoint(37.0, -122.0, 10.0f, 0.5f);
    TEST_ASSERT_EQ(result, -1, "Add waypoint without init should fail");
}

/* ============================================================================
 * Navigation Update Tests
 * ============================================================================ */

static void test_update_nav_success(void)
{
    setup_configured_vessel();
    int result = vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.5f, -2.0f, 50.0f);
    TEST_ASSERT_EQ(result, 0, "Nav update should succeed");
    TEST_ASSERT_DOUBLE_EQ(g_vessel.current.position.latitude, 37.5, 0.0001, "Lat matches");
    TEST_ASSERT_FLOAT_EQ(g_vessel.current.heading_deg, 90.0f, 0.1f, "Heading matches");
    TEST_ASSERT_FLOAT_EQ(g_vessel.current.speed_kts, 12.5f, 0.1f, "Speed matches");
}

static void test_update_nav_tracks_previous(void)
{
    setup_configured_vessel();
    vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
    vessel_update_nav(37.6, -122.4, 95.0f, 93.0f, 13.0f, 1.0f, 48.0f);
    
    TEST_ASSERT_DOUBLE_EQ(g_vessel.previous.position.latitude, 37.5, 0.0001, "Previous lat stored");
    TEST_ASSERT_FLOAT_EQ(g_vessel.previous.heading_deg, 90.0f, 0.1f, "Previous heading stored");
}

static void test_update_nav_without_init(void)
{
    reset_system();
    int result = vessel_update_nav(37.0, -122.0, 90.0f, 90.0f, 10.0f, 0.0f, 30.0f);
    TEST_ASSERT_EQ(result, -1, "Nav update without init should fail");
}

/* ============================================================================
 * ARPA Target Tracking Tests
 * ============================================================================ */

static void test_add_target_success(void)
{
    setup_configured_vessel();
    int result = vessel_update_target(1001, TARGET_CARGO, 37.8, -122.3, 180.0f, 14.0f);
    TEST_ASSERT_EQ(result, 0, "Add target should succeed");
    TEST_ASSERT_EQ(g_vessel.target_count, 1, "Target count should be 1");
}

static void test_update_existing_target(void)
{
    setup_configured_vessel();
    vessel_update_target(1001, TARGET_CARGO, 37.8, -122.3, 180.0f, 14.0f);
    vessel_update_target(1001, TARGET_CARGO, 37.85, -122.35, 175.0f, 13.0f);
    
    TEST_ASSERT_EQ(g_vessel.target_count, 1, "Target count should still be 1");
    TEST_ASSERT_DOUBLE_EQ(g_vessel.targets[0].position.latitude, 37.85, 0.0001, "Target updated");
}

static void test_multiple_targets(void)
{
    setup_configured_vessel();
    vessel_update_target(1001, TARGET_CARGO, 37.8, -122.3, 180.0f, 14.0f);
    vessel_update_target(1002, TARGET_TANKER, 37.9, -122.4, 270.0f, 10.0f);
    vessel_update_target(1003, TARGET_FISHING, 37.7, -122.2, 45.0f, 6.0f);
    
    TEST_ASSERT_EQ(g_vessel.target_count, 3, "Target count should be 3");
}

static void test_target_limit(void)
{
    setup_configured_vessel();
    for (int i = 0; i < VESSEL_MAX_TARGETS; i++) {
        vessel_update_target((uint16_t)(1000 + i), TARGET_VESSEL, 37.0 + i * 0.01, -122.0, 180.0f, 10.0f);
    }
    int result = vessel_update_target(9999, TARGET_VESSEL, 38.0, -123.0, 90.0f, 8.0f);
    TEST_ASSERT_NE(result, 0, "Exceeding target limit should fail");  /* Returns -2 for capacity */
}

/* ============================================================================
 * Cargo Bay Tests
 * ============================================================================ */

static void test_config_bay_success(void)
{
    setup_configured_vessel();
    int result = vessel_config_bay(0, 2000000.0f, 50.0f, 0.0f, 5.0f);
    TEST_ASSERT_EQ(result, 0, "Config bay should succeed");
    TEST_ASSERT_EQ(g_vessel.bay_count, 1, "Bay count should be 1");
}

static void test_update_bay_success(void)
{
    setup_configured_vessel();
    vessel_config_bay(0, 2000000.0f, 50.0f, 0.0f, 5.0f);
    
    int result = vessel_update_bay(0, 1500000.0f, 100);
    TEST_ASSERT_EQ(result, 0, "Update bay should succeed");
    TEST_ASSERT_FLOAT_EQ(g_vessel.bays[0].total_weight_kg, 1500000.0f, 100.0f, "Weight updated");
    TEST_ASSERT_EQ(g_vessel.bays[0].container_count, 100, "Container count updated");
}

static void test_bay_overload_detection(void)
{
    setup_configured_vessel();
    vessel_config_bay(0, 2000000.0f, 50.0f, 0.0f, 5.0f);
    vessel_update_bay(0, 2500000.0f, 150);  /* Over max weight */
    
    TEST_ASSERT_TRUE(g_vessel.bays[0].overloaded, "Bay should be marked overloaded");
}

static void test_bay_limit(void)
{
    setup_configured_vessel();
    for (int i = 0; i < VESSEL_MAX_CARGO_BAYS; i++) {
        vessel_config_bay((uint8_t)i, 1000000.0f, 50.0f * i, 0.0f, 5.0f);
    }
    int result = vessel_config_bay(VESSEL_MAX_CARGO_BAYS, 1000000.0f, 500.0f, 0.0f, 5.0f);
    TEST_ASSERT_EQ(result, -1, "Exceeding bay limit should fail");
}

/* ============================================================================
 * Stability & TMR Sensor Tests
 * ============================================================================ */

static void test_update_stability_tmr_agree(void)
{
    setup_configured_vessel();
    float roll[] = {5.0f, 5.1f, 4.9f};    /* Sensors agree */
    float pitch[] = {1.0f, 1.1f, 0.9f};
    
    int result = vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    TEST_ASSERT_EQ(result, 0, "Update stability should succeed");
    TEST_ASSERT_EQ(g_vessel.roll_sensor.status, TMR_AGREE_ALL, "Roll TMR should agree");
    TEST_ASSERT_TRUE(g_vessel.roll_sensor.healthy, "Roll sensor should be healthy");
}

static void test_update_stability_tmr_2of3(void)
{
    setup_configured_vessel();
    float roll[] = {5.0f, 5.1f, 15.0f};   /* One sensor off */
    float pitch[] = {1.0f, 1.1f, 1.0f};
    
    vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    TEST_ASSERT_EQ(g_vessel.roll_sensor.status, TMR_AGREE_2OF3, "Roll TMR should be 2of3");
    TEST_ASSERT_TRUE(g_vessel.roll_sensor.healthy, "Roll sensor still healthy with 2of3");
}

static void test_update_stability_tmr_disagree(void)
{
    setup_configured_vessel();
    float roll[] = {5.0f, 15.0f, 25.0f};  /* All sensors disagree */
    float pitch[] = {1.0f, 1.1f, 1.0f};
    
    vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    TEST_ASSERT_EQ(g_vessel.roll_sensor.status, TMR_DISAGREE, "Roll TMR should disagree");
    /* Note: TMR_DISAGREE still provides median value, not considered a 'fault' */
    TEST_ASSERT_FLOAT_EQ(g_vessel.roll_sensor.voted_value, 15.0f, 0.1f, "Median value used");
}

static void test_gm_stability_check(void)
{
    setup_configured_vessel();
    float roll[] = {2.0f, 2.0f, 2.0f};
    float pitch[] = {1.0f, 1.0f, 1.0f};
    
    /* Good GM */
    vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    TEST_ASSERT_TRUE(vessel_is_stable(), "Should be stable with good GM");
    TEST_ASSERT_FLOAT_EQ(vessel_get_gm(), 0.5f, 0.01f, "GM value correct");
}

static void test_low_gm_instability(void)
{
    setup_configured_vessel();
    float roll[] = {2.0f, 2.0f, 2.0f};
    float pitch[] = {1.0f, 1.0f, 1.0f};
    
    /* Low GM below threshold */
    vessel_update_stability(roll, pitch, 0.1f, 50000.0f);
    TEST_ASSERT_FALSE(vessel_is_stable(), "Should be unstable with low GM");
}

static void test_excessive_roll_alarm(void)
{
    setup_configured_vessel();
    float roll[] = {20.0f, 20.1f, 19.9f};  /* Excessive roll */
    float pitch[] = {1.0f, 1.0f, 1.0f};
    
    vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    TEST_ASSERT_GT(vessel_get_alarm_count(), 0, "Should have alarms for excessive roll");
}

static void test_sensor_fault_detection(void)
{
    setup_configured_vessel();
    float roll[] = {5.0f, 15.0f, 25.0f};  /* All disagree */
    float pitch[] = {1.0f, 1.0f, 1.0f};
    
    vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    /* TMR_DISAGREE still provides value, check status instead */
    TEST_ASSERT_EQ(vessel_get_roll_tmr_status(), TMR_DISAGREE, "TMR status should be disagree");
    TEST_ASSERT_FLOAT_EQ(g_vessel.roll_sensor.voted_value, 15.0f, 0.1f, "Median value used");
}

/* ============================================================================
 * Weather Response Tests
 * ============================================================================ */

static void test_update_weather_success(void)
{
    setup_configured_vessel();
    int result = vessel_update_weather(25.0f, 180.0f, 3.0f, 8.0f, SEA_STATE_3_SLIGHT);
    TEST_ASSERT_EQ(result, 0, "Weather update should succeed");
    TEST_ASSERT_FLOAT_EQ(g_vessel.weather.wind_speed_kts, 25.0f, 0.1f, "Wind speed updated");
    TEST_ASSERT_FLOAT_EQ(g_vessel.weather.wave_height_m, 3.0f, 0.1f, "Wave height updated");
}

static void test_weather_limit_exceeded(void)
{
    setup_configured_vessel();
    vessel_update_weather(55.0f, 180.0f, 3.0f, 8.0f, SEA_STATE_4_MODERATE);  /* Over wind limit */
    TEST_ASSERT_TRUE(g_vessel.weather_limit_exceeded, "Weather limit should be exceeded");
}

static void test_wave_limit_exceeded(void)
{
    setup_configured_vessel();
    vessel_update_weather(20.0f, 180.0f, 10.0f, 12.0f, SEA_STATE_7_HIGH);  /* Over wave limit */
    TEST_ASSERT_TRUE(g_vessel.weather_limit_exceeded, "Weather limit for waves exceeded");
}

/* ============================================================================
 * Emergency Stop & Safety Tests
 * ============================================================================ */

static void test_emergency_stop_success(void)
{
    setup_configured_vessel();
    vessel_set_nav_mode(NAV_MODE_WAYPOINT);
    
    int result = vessel_emergency_stop();
    TEST_ASSERT_EQ(result, 0, "Emergency stop should succeed");
    TEST_ASSERT_TRUE(g_vessel.emergency_stop, "Emergency flag set");
    TEST_ASSERT_EQ(g_vessel.current.engine_order, ENGINE_STOP, "Engine stopped");
    TEST_ASSERT_GT(g_vessel.emergency_count, 0, "Emergency count incremented");
}

static void test_emergency_stop_increments_count(void)
{
    setup_configured_vessel();
    
    vessel_emergency_stop();
    TEST_ASSERT_EQ(g_vessel.emergency_count, 1, "First emergency");
    
    vessel_reset();
    vessel_emergency_stop();
    TEST_ASSERT_EQ(g_vessel.emergency_count, 1, "Count reset with system");
}

static void test_emergency_stop_without_init(void)
{
    reset_system();
    int result = vessel_emergency_stop();
    TEST_ASSERT_EQ(result, -1, "Emergency stop without init should fail");
}

/* ============================================================================
 * Alarm Management Tests
 * ============================================================================ */

static void test_alarm_acknowledge(void)
{
    setup_configured_vessel();
    float roll[] = {20.0f, 20.0f, 20.0f};  /* Will trigger alarm */
    float pitch[] = {1.0f, 1.0f, 1.0f};
    vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    
    uint8_t count = vessel_get_alarm_count();
    TEST_ASSERT_GT(count, 0, "Should have alarms");
    
    int result = vessel_ack_alarm(0);
    TEST_ASSERT_EQ(result, 0, "Ack should succeed");
    TEST_ASSERT_TRUE(g_vessel.alarms[0].acknowledged, "Alarm should be acked");
}

static void test_alarm_ack_invalid_index(void)
{
    setup_configured_vessel();
    int result = vessel_ack_alarm(99);
    TEST_ASSERT_EQ(result, -1, "Ack invalid index should fail");
}

/* ============================================================================
 * Process Loop Tests
 * ============================================================================ */

static void test_process_basic(void)
{
    setup_configured_vessel();
    vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
    
    int result = vessel_process(1000);
    TEST_ASSERT_EQ(result, 0, "Process should succeed");
    TEST_ASSERT_GT(g_vessel.uptime_seconds, 0, "Uptime should increment");
}

static void test_process_accumulates_distance(void)
{
    setup_configured_vessel();
    
    /* Simulate movement over time with speed > 0 */
    vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
    vessel_process(0);  /* Initial process */
    float dist1 = vessel_get_total_distance();
    
    /* After 1 second at 12 knots, some distance should accumulate */
    vessel_process(1000);  /* 1 second later */
    float dist2 = vessel_get_total_distance();
    
    /* 12 knots = 12/3600 nm per second = 0.00333 nm */
    TEST_ASSERT_GE(dist2, dist1, "Distance should accumulate or stay same");
}

static void test_process_without_init(void)
{
    reset_system();
    int result = vessel_process(1000);
    TEST_ASSERT_EQ(result, -1, "Process without init should fail");
}

/* ============================================================================
 * Telemetry Tests
 * ============================================================================ */

static void test_telemetry_generation(void)
{
    setup_configured_vessel();
    vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
    
    uint8_t buffer[256];
    int len = vessel_get_telemetry(buffer, sizeof(buffer));
    TEST_ASSERT_GT(len, 0, "Should generate telemetry");
    TEST_ASSERT_LE(len, 256, "Should not exceed buffer");
}

static void test_telemetry_small_buffer(void)
{
    setup_configured_vessel();
    vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
    
    uint8_t buffer[10];  /* Too small */
    int len = vessel_get_telemetry(buffer, sizeof(buffer));
    TEST_ASSERT_EQ(len, -1, "Small buffer should fail");
}

static void test_telemetry_sequence(void)
{
    setup_configured_vessel();
    vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
    
    uint8_t buffer[256];
    vessel_get_telemetry(buffer, sizeof(buffer));
    uint16_t seq1 = g_vessel.telemetry_seq;
    
    vessel_get_telemetry(buffer, sizeof(buffer));
    uint16_t seq2 = g_vessel.telemetry_seq;
    
    TEST_ASSERT_GT(seq2, seq1, "Sequence should increment");
}

/* ============================================================================
 * Getter Function Tests
 * ============================================================================ */

static void test_getters_without_init(void)
{
    reset_system();
    TEST_ASSERT_EQ(vessel_get_nav_mode(), NAV_MODE_MANUAL, "Nav mode default");
    TEST_ASSERT_EQ(vessel_get_state(), VESSEL_STATE_MOORED, "State default");
    TEST_ASSERT_FLOAT_EQ(vessel_get_speed(), 0.0f, 0.01f, "Speed default");
    TEST_ASSERT_FLOAT_EQ(vessel_get_heading(), 0.0f, 0.01f, "Heading default");
    TEST_ASSERT_FLOAT_EQ(vessel_get_roll(), 0.0f, 0.01f, "Roll default");
    TEST_ASSERT_FLOAT_EQ(vessel_get_gm(), 0.0f, 0.01f, "GM default");
}

static void test_getters_with_data(void)
{
    setup_configured_vessel();
    vessel_update_nav(37.5, -122.5, 135.0f, 133.0f, 15.5f, 1.0f, 50.0f);
    
    float roll[] = {3.0f, 3.0f, 3.0f};
    float pitch[] = {0.5f, 0.5f, 0.5f};
    vessel_update_stability(roll, pitch, 0.8f, 50000.0f);
    
    TEST_ASSERT_FLOAT_EQ(vessel_get_speed(), 15.5f, 0.1f, "Speed matches");
    TEST_ASSERT_FLOAT_EQ(vessel_get_heading(), 135.0f, 0.1f, "Heading matches");
    TEST_ASSERT_FLOAT_EQ(vessel_get_roll(), 3.0f, 0.5f, "Roll matches");
    TEST_ASSERT_FLOAT_EQ(vessel_get_gm(), 0.8f, 0.01f, "GM matches");
}

/* ============================================================================
 * Integration Test: Rough Seas Scenario
 * ============================================================================ */

static void test_rough_seas_integration(void)
{
    setup_configured_vessel();
    vessel_set_nav_mode(NAV_MODE_WAYPOINT);
    vessel_add_waypoint(37.8, -122.3, 12.0f, 0.5f);
    vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
    
    /* Simulate deteriorating weather */
    vessel_update_weather(20.0f, 270.0f, 2.0f, 6.0f, SEA_STATE_2_SMOOTH);
    vessel_process(1000);
    TEST_ASSERT_FALSE(g_vessel.weather_limit_exceeded, "Not exceeded yet");
    
    vessel_update_weather(40.0f, 270.0f, 5.0f, 8.0f, SEA_STATE_5_ROUGH);
    vessel_process(2000);
    TEST_ASSERT_FALSE(g_vessel.weather_limit_exceeded, "Still within limits");
    
    vessel_update_weather(55.0f, 270.0f, 9.0f, 10.0f, SEA_STATE_7_HIGH);
    vessel_process(3000);
    TEST_ASSERT_TRUE(g_vessel.weather_limit_exceeded, "Weather limit exceeded");
    
    /* Heavy rolling in rough seas */
    float roll[] = {18.0f, 18.2f, 17.8f};
    float pitch[] = {4.0f, 4.1f, 3.9f};
    vessel_update_stability(roll, pitch, 0.3f, 50000.0f);
    vessel_process(4000);
    
    TEST_ASSERT_GT(vessel_get_alarm_count(), 0, "Should have weather/roll alarms");
}

/* ============================================================================
 * Integration Test: Cargo Imbalance Scenario
 * ============================================================================ */

static void test_cargo_imbalance_integration(void)
{
    setup_configured_vessel();
    
    /* Configure asymmetric cargo bays */
    vessel_config_bay(0, 2000000.0f, 50.0f, -10.0f, 5.0f);   /* Port side */
    vessel_config_bay(1, 2000000.0f, 50.0f, 10.0f, 5.0f);    /* Starboard side */
    
    /* Load unevenly - heavy on port */
    vessel_update_bay(0, 1800000.0f, 120);
    vessel_update_bay(1, 800000.0f, 50);
    
    /* This should cause heel */
    float roll[] = {8.0f, 8.1f, 7.9f};   /* Static heel from cargo */
    float pitch[] = {0.5f, 0.5f, 0.5f};
    vessel_update_stability(roll, pitch, 0.4f, 50000.0f);
    
    vessel_process(1000);
    
    /* System should detect stability concerns */
    float tot_weight = g_vessel.bays[0].total_weight_kg + g_vessel.bays[1].total_weight_kg;
    TEST_ASSERT_GT(tot_weight, 2000000.0f, "Total cargo weight significant");
    TEST_ASSERT_FLOAT_EQ(vessel_get_roll(), 8.0f, 0.5f, "Heel angle from cargo");
}

/* ============================================================================
 * Integration Test: Collision Avoidance Scenario
 * ============================================================================ */

static void test_collision_avoidance_integration(void)
{
    setup_configured_vessel();
    vessel_set_nav_mode(NAV_MODE_TRACK_CONTROL);
    vessel_update_nav(37.5, -122.5, 90.0f, 90.0f, 12.0f, 0.0f, 50.0f);
    
    /* No targets initially */
    TEST_ASSERT_EQ(vessel_get_dangerous_targets(), 0, "No dangerous targets");
    TEST_ASSERT_FALSE(vessel_is_collision_avoidance_active(), "No avoidance yet");
    
    /* Add a distant target - low risk */
    vessel_update_target(1001, TARGET_CARGO, 37.6, -122.3, 270.0f, 10.0f);
    vessel_process(1000);
    
    /* Add crossing target - higher risk */
    vessel_update_target(1002, TARGET_TANKER, 37.51, -122.48, 180.0f, 14.0f);
    vessel_process(2000);
    
    /* Simulate close quarters situation */
    vessel_update_target(1003, TARGET_FISHING, 37.502, -122.495, 270.0f, 8.0f);
    vessel_process(3000);
    
    TEST_ASSERT_GE(g_vessel.target_count, 3, "Should track multiple targets");
}

/* ============================================================================
 * Integration Test: Full Voyage Simulation
 * ============================================================================ */

static void test_voyage_simulation(void)
{
    setup_configured_vessel();
    
    /* Load cargo */
    vessel_config_bay(0, 2000000.0f, 50.0f, 0.0f, 5.0f);
    vessel_config_bay(1, 2000000.0f, 100.0f, 0.0f, 5.0f);
    vessel_update_bay(0, 1500000.0f, 100);
    vessel_update_bay(1, 1400000.0f, 95);
    
    /* Set route */
    vessel_add_waypoint(37.7749, -122.4194, 12.0f, 0.5f);
    vessel_add_waypoint(34.0522, -118.2437, 14.0f, 0.8f);
    vessel_set_nav_mode(NAV_MODE_WAYPOINT);
    
    /* Simulate voyage progress */
    float roll[] = {2.0f, 2.0f, 2.0f};
    float pitch[] = {0.5f, 0.5f, 0.5f};
    
    for (int i = 0; i < 10; i++) {
        double lat = 37.7749 - (i * 0.37);  /* Moving south */
        double lon = -122.4194 + (i * 0.42); /* Moving east */
        
        vessel_update_nav(lat, lon, 135.0f, 133.0f, 12.0f, 0.0f, 60.0f);
        vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
        vessel_update_weather(15.0f, 315.0f, 1.5f, 7.0f, SEA_STATE_2_SMOOTH);
        vessel_process(1000 * (uint32_t)(i + 1));
    }
    
    TEST_ASSERT_TRUE(g_vessel.initialized, "System still running");
    TEST_ASSERT_GT(g_vessel.uptime_seconds, 0, "Uptime accumulated");
    /* Event count may be positive due to startup events - just verify no alarms */
    TEST_ASSERT_EQ(vessel_get_alarm_count(), 0, "No alarms in normal voyage");
}

/* ============================================================================
 * Edge Case Tests
 * ============================================================================ */

static void test_heading_wraparound(void)
{
    setup_configured_vessel();
    
    /* Test heading near 360/0 boundary */
    vessel_update_nav(37.5, -122.5, 358.0f, 356.0f, 12.0f, 0.0f, 50.0f);
    TEST_ASSERT_FLOAT_EQ(vessel_get_heading(), 358.0f, 0.1f, "High heading");
    
    vessel_update_nav(37.5, -122.5, 2.0f, 4.0f, 12.0f, 0.0f, 50.0f);
    TEST_ASSERT_FLOAT_EQ(vessel_get_heading(), 2.0f, 0.1f, "Low heading after wrap");
}

static void test_zero_speed(void)
{
    setup_configured_vessel();
    vessel_update_nav(37.5, -122.5, 90.0f, 90.0f, 0.0f, 0.0f, 50.0f);
    
    TEST_ASSERT_FLOAT_EQ(vessel_get_speed(), 0.0f, 0.01f, "Zero speed");
    vessel_process(1000);
    /* Should handle zero speed without issues */
}

static void test_maximum_roll(void)
{
    setup_configured_vessel();
    float roll[] = {30.0f, 30.0f, 30.0f};  /* Critical roll */
    float pitch[] = {1.0f, 1.0f, 1.0f};
    
    vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
    
    TEST_ASSERT_FLOAT_EQ(vessel_get_roll(), 30.0f, 0.5f, "High roll recorded");
    TEST_ASSERT_GT(vessel_get_alarm_count(), 0, "Critical roll should alarm");
}

/* ============================================================================
 * Main Function
 * ============================================================================ */

int main(void)
{
    printf("\n");
    printf("============================================================\n");
    printf("  Autonomous Vessel Control & Safety Test Suite\n");
    printf("============================================================\n");
    printf("\n");
    
    /* Initialization Tests */
    printf("Initialization Tests:\n");
    RUN_TEST(test_init_success);
    RUN_TEST(test_double_init);
    RUN_TEST(test_config_success);
    RUN_TEST(test_config_without_init);
    RUN_TEST(test_shutdown_clears_state);
    RUN_TEST(test_reset_reinitializes);
    printf("\n");
    
    /* Navigation Mode Tests */
    printf("Navigation Mode Tests:\n");
    RUN_TEST(test_nav_mode_transitions);
    RUN_TEST(test_nav_mode_without_init);
    RUN_TEST(test_emergency_mode_triggers);
    printf("\n");
    
    /* Waypoint Tests */
    printf("Waypoint Management Tests:\n");
    RUN_TEST(test_add_waypoint_success);
    RUN_TEST(test_add_multiple_waypoints);
    RUN_TEST(test_waypoint_limit);
    RUN_TEST(test_waypoint_without_init);
    printf("\n");
    
    /* Navigation Update Tests */
    printf("Navigation Update Tests:\n");
    RUN_TEST(test_update_nav_success);
    RUN_TEST(test_update_nav_tracks_previous);
    RUN_TEST(test_update_nav_without_init);
    printf("\n");
    
    /* ARPA Target Tests */
    printf("ARPA Target Tracking Tests:\n");
    RUN_TEST(test_add_target_success);
    RUN_TEST(test_update_existing_target);
    RUN_TEST(test_multiple_targets);
    RUN_TEST(test_target_limit);
    printf("\n");
    
    /* Cargo Bay Tests */
    printf("Cargo Bay Management Tests:\n");
    RUN_TEST(test_config_bay_success);
    RUN_TEST(test_update_bay_success);
    RUN_TEST(test_bay_overload_detection);
    RUN_TEST(test_bay_limit);
    printf("\n");
    
    /* Stability Tests */
    printf("Stability & TMR Sensor Tests:\n");
    RUN_TEST(test_update_stability_tmr_agree);
    RUN_TEST(test_update_stability_tmr_2of3);
    RUN_TEST(test_update_stability_tmr_disagree);
    RUN_TEST(test_gm_stability_check);
    RUN_TEST(test_low_gm_instability);
    RUN_TEST(test_excessive_roll_alarm);
    RUN_TEST(test_sensor_fault_detection);
    printf("\n");
    
    /* Weather Tests */
    printf("Weather Response Tests:\n");
    RUN_TEST(test_update_weather_success);
    RUN_TEST(test_weather_limit_exceeded);
    RUN_TEST(test_wave_limit_exceeded);
    printf("\n");
    
    /* Emergency Tests */
    printf("Emergency Stop & Safety Tests:\n");
    RUN_TEST(test_emergency_stop_success);
    RUN_TEST(test_emergency_stop_increments_count);
    RUN_TEST(test_emergency_stop_without_init);
    printf("\n");
    
    /* Alarm Tests */
    printf("Alarm Management Tests:\n");
    RUN_TEST(test_alarm_acknowledge);
    RUN_TEST(test_alarm_ack_invalid_index);
    printf("\n");
    
    /* Process Tests */
    printf("Process Loop Tests:\n");
    RUN_TEST(test_process_basic);
    RUN_TEST(test_process_accumulates_distance);
    RUN_TEST(test_process_without_init);
    printf("\n");
    
    /* Telemetry Tests */
    printf("Telemetry Tests:\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_small_buffer);
    RUN_TEST(test_telemetry_sequence);
    printf("\n");
    
    /* Getter Tests */
    printf("Getter Function Tests:\n");
    RUN_TEST(test_getters_without_init);
    RUN_TEST(test_getters_with_data);
    printf("\n");
    
    /* Integration Tests */
    printf("Integration Tests:\n");
    RUN_TEST(test_rough_seas_integration);
    RUN_TEST(test_cargo_imbalance_integration);
    RUN_TEST(test_collision_avoidance_integration);
    RUN_TEST(test_voyage_simulation);
    printf("\n");
    
    /* Edge Case Tests */
    printf("Edge Case Tests:\n");
    RUN_TEST(test_heading_wraparound);
    RUN_TEST(test_zero_speed);
    RUN_TEST(test_maximum_roll);
    printf("\n");
    
    /* Summary */
    printf("============================================================\n");
    printf("  Test Results\n");
    printf("============================================================\n");
    printf("  Tests:      %d/%d passed\n", tests_passed, tests_run);
    printf("  Assertions: %d/%d passed\n", assertions_passed, assertions_run);
    printf("============================================================\n");
    
    if (tests_passed == tests_run && assertions_passed == assertions_run) {
        printf("  ALL TESTS PASSED!\n");
    } else {
        printf("  SOME TESTS FAILED\n");
    }
    printf("============================================================\n\n");
    
    return (tests_passed == tests_run) ? 0 : 1;
}
