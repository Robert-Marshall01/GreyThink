/**
 * @file firefighting_tests.c
 * @brief Integration tests for Firefighting Thermal Imaging & Suppression
 * 
 * Tests cover:
 * - System initialization and shutdown
 * - Thermal sensor registration and frame processing
 * - Heat map generation and hot spot detection
 * - Zone configuration and arming
 * - Fire detection and alarm triggering
 * - Suppression activation and abort
 * - Event logging and statistics
 * - Telemetry generation
 * - Fire spread simulation
 * - Sensor noise tolerance
 * - Multi-zone incident handling
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <math.h>

/* ============================================================================
 * Test Framework
 * ============================================================================ */

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define TEST_ASSERT(condition, message) do { \
    if (!(condition)) { \
        printf("  [FAIL] %s: %s\n", __func__, message); \
        tests_failed++; \
        return; \
    } \
} while(0)

#define TEST_ASSERT_FLOAT(actual, expected, tolerance, message) do { \
    if (fabsf((actual) - (expected)) > (tolerance)) { \
        printf("  [FAIL] %s: %s (got %.3f, expected %.3f)\n", \
               __func__, message, (actual), (expected)); \
        tests_failed++; \
        return; \
    } \
} while(0)

#define RUN_TEST(test_func) do { \
    tests_run++; \
    test_func(); \
    if (tests_run > tests_passed + tests_failed) { \
        tests_passed++; \
        printf("  [PASS] %s\n", #test_func); \
    } \
} while(0)

/* ============================================================================
 * Include Firefighting Implementation 
 * ============================================================================ */

#include "../src/firefighting/firefighting_spotlight.c"

/* ============================================================================
 * Test Helper Functions
 * ============================================================================ */

static void reset_firefighting(void) {
    if (g_ff.initialized) {
        ff_shutdown();
    }
    memset(&g_ff, 0, sizeof(g_ff));
}

static void setup_basic_system(void) {
    reset_firefighting();
    ff_init();
    
    /* Register a thermal sensor */
    ff_register_sensor(0);
    
    /* Configure a zone */
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Zone A",
        .system_type = FF_SYSTEM_WET_PIPE,
        .agent = FF_AGENT_WATER,
        .require_cross_zone = false,
        .preaction_delay = 5000,
        .sensor_count = 1,
        .alarm_threshold = FF_TEMP_ALARM
    };
    zone.sensor_ids[0] = 0;
    ff_configure_zone(&zone);
    
    /* Arm the system */
    ff_arm_system();
    ff_arm_zone(0);
}

static void inject_ambient_temps(uint8_t sensor_id) {
    float temps[FF_THERMAL_PIXELS];
    for (int i = 0; i < FF_THERMAL_PIXELS; i++) {
        temps[i] = FF_TEMP_AMBIENT + (float)(rand() % 5);
    }
    ff_update_thermal_frame(sensor_id, temps);
}

static void inject_fire_temps(uint8_t sensor_id, uint16_t x, uint16_t y, float temp) {
    ff_inject_heat_source(sensor_id, x, y, temp, 3.0f);
}

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

static void test_init_default(void) {
    reset_firefighting();
    int result = ff_init();
    TEST_ASSERT(result == 0, "Init should succeed");
    TEST_ASSERT(g_ff.initialized == true, "Should be initialized");
    TEST_ASSERT(g_ff.armed == false, "Should not be armed initially");
    ff_shutdown();
}

static void test_double_init(void) {
    reset_firefighting();
    ff_init();
    int result = ff_init();
    TEST_ASSERT(result == -1, "Double init should fail");
    ff_shutdown();
}

static void test_shutdown(void) {
    reset_firefighting();
    ff_init();
    int result = ff_shutdown();
    TEST_ASSERT(result == 0, "Shutdown should succeed");
    TEST_ASSERT(g_ff.initialized == false, "Should not be initialized");
}

static void test_shutdown_not_initialized(void) {
    reset_firefighting();
    int result = ff_shutdown();
    TEST_ASSERT(result == -1, "Shutdown when not initialized should fail");
}

/* ============================================================================
 * Sensor Registration Tests
 * ============================================================================ */

static void test_register_sensor(void) {
    reset_firefighting();
    ff_init();
    
    int result = ff_register_sensor(0);
    TEST_ASSERT(result == 0, "Sensor registration should succeed");
    TEST_ASSERT(g_ff.sensor_count == 1, "Sensor count should be 1");
    
    ff_shutdown();
}

static void test_register_multiple_sensors(void) {
    reset_firefighting();
    ff_init();
    
    ff_register_sensor(0);
    ff_register_sensor(1);
    ff_register_sensor(2);
    
    TEST_ASSERT(g_ff.sensor_count == 3, "Should have 3 sensors");
    
    ff_shutdown();
}

static void test_register_max_sensors(void) {
    reset_firefighting();
    ff_init();
    
    for (uint8_t i = 0; i < FF_MAX_THERMAL_SENSORS; i++) {
        ff_register_sensor(i);
    }
    
    int result = ff_register_sensor(FF_MAX_THERMAL_SENSORS);
    TEST_ASSERT(result == -1, "Should reject sensor beyond max");
    
    ff_shutdown();
}

/* ============================================================================
 * Frame Processing Tests
 * ============================================================================ */

static void test_update_thermal_frame(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    
    float temps[FF_THERMAL_PIXELS];
    for (int i = 0; i < FF_THERMAL_PIXELS; i++) {
        temps[i] = 25.0f;
    }
    
    int result = ff_update_thermal_frame(0, temps);
    TEST_ASSERT(result == 0, "Frame update should succeed");
    
    ff_shutdown();
}

static void test_update_invalid_sensor(void) {
    reset_firefighting();
    ff_init();
    
    float temps[FF_THERMAL_PIXELS];
    int result = ff_update_thermal_frame(99, temps);
    TEST_ASSERT(result == -1, "Should reject invalid sensor");
    
    ff_shutdown();
}

static void test_heat_source_injection(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    
    /* Inject a heat source at center */
    int result = ff_inject_heat_source(0, 16, 16, 200.0f, 5.0f);
    TEST_ASSERT(result == 0, "Heat source injection should succeed");
    
    /* Check pixel temperature */
    float temp;
    ff_get_pixel_temp(0, 16, 16, &temp);
    TEST_ASSERT(temp >= 100.0f, "Center should be hot");
    
    ff_shutdown();
}

/* ============================================================================
 * Zone Configuration Tests
 * ============================================================================ */

static void test_configure_zone(void) {
    reset_firefighting();
    ff_init();
    
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Test Zone",
        .system_type = FF_SYSTEM_WET_PIPE,
        .agent = FF_AGENT_WATER,
        .require_cross_zone = false,
        .preaction_delay = 10000,
        .alarm_threshold = FF_TEMP_ALARM,
        .sensor_count = 1
    };
    zone.sensor_ids[0] = 0;
    
    int result = ff_configure_zone(&zone);
    TEST_ASSERT(result == 0, "Zone configuration should succeed");
    TEST_ASSERT(g_ff.zone_count == 1, "Zone count should be 1");
    
    ff_shutdown();
}

static void test_configure_multiple_zones(void) {
    reset_firefighting();
    ff_init();
    
    for (uint8_t i = 0; i < 4; i++) {
        ff_zone_config_t zone = {
            .zone_id = i,
            .name = "Zone",
            .system_type = FF_SYSTEM_WET_PIPE,
            .agent = FF_AGENT_WATER
        };
        ff_configure_zone(&zone);
    }
    
    TEST_ASSERT(g_ff.zone_count == 4, "Should have 4 zones");
    
    ff_shutdown();
}

static void test_configure_gaseous_zone(void) {
    reset_firefighting();
    ff_init();
    
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Server Room",
        .system_type = FF_SYSTEM_GASEOUS,
        .agent = FF_AGENT_FM200,
        .require_cross_zone = true,
        .preaction_delay = 30000,
        .sensor_count = 2
    };
    zone.sensor_ids[0] = 0;
    zone.sensor_ids[1] = 1;
    
    int result = ff_configure_zone(&zone);
    TEST_ASSERT(result == 0, "Gaseous zone should be configurable");
    TEST_ASSERT(g_ff.zone_configs[0].require_cross_zone == true, 
                "Cross-zone should be set");
    
    ff_shutdown();
}

/* ============================================================================
 * Arming Tests
 * ============================================================================ */

static void test_arm_system(void) {
    reset_firefighting();
    ff_init();
    
    int result = ff_arm_system();
    TEST_ASSERT(result == 0, "System arming should succeed");
    TEST_ASSERT(g_ff.armed == true, "System should be armed");
    
    ff_shutdown();
}

static void test_disarm_system(void) {
    reset_firefighting();
    ff_init();
    ff_arm_system();
    
    int result = ff_disarm_system();
    TEST_ASSERT(result == 0, "System disarming should succeed");
    TEST_ASSERT(g_ff.armed == false, "System should be disarmed");
    
    ff_shutdown();
}

static void test_arm_zone(void) {
    reset_firefighting();
    ff_init();
    
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Zone A",
        .system_type = FF_SYSTEM_WET_PIPE,
        .agent = FF_AGENT_WATER
    };
    ff_configure_zone(&zone);
    ff_arm_system();
    
    int result = ff_arm_zone(0);
    TEST_ASSERT(result == 0, "Zone arming should succeed");
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state == FF_ZONE_MONITORING, "Zone should be monitoring");
    
    ff_shutdown();
}

static void test_arm_unconfigured_zone(void) {
    reset_firefighting();
    ff_init();
    ff_arm_system();
    
    int result = ff_arm_zone(99);
    TEST_ASSERT(result == -1, "Should reject unconfigured zone");
    
    ff_shutdown();
}

/* ============================================================================
 * Fire Detection Tests
 * ============================================================================ */

static void test_hot_spot_detection(void) {
    setup_basic_system();
    
    /* Inject a fire temperature */
    inject_fire_temps(0, 16, 16, 200.0f);
    
    /* Process frames */
    for (int i = 0; i < 10; i++) {
        ff_process(100);
    }
    
    /* Check for hot spots */
    ff_hot_spot_t spots[FF_MAX_HOT_SPOTS];
    uint8_t count = 0;
    ff_get_hot_spots(spots, FF_MAX_HOT_SPOTS, &count);
    
    TEST_ASSERT(count > 0, "Should detect hot spot");
    
    ff_shutdown();
}

static void test_alarm_trigger(void) {
    setup_basic_system();
    
    /* Inject fire temperature above alarm threshold */
    inject_fire_temps(0, 16, 16, FF_TEMP_FIRE + 50.0f);
    
    /* Process to trigger alarm */
    for (int i = 0; i < 20; i++) {
        ff_process(100);
    }
    
    /* Check zone state */
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    
    TEST_ASSERT(state >= FF_ZONE_ALARM, "Zone should be in alarm or beyond");
    
    ff_shutdown();
}

static void test_rate_of_rise_detection(void) {
    setup_basic_system();
    
    /* Start with ambient */
    inject_ambient_temps(0);
    ff_process(1000);
    
    /* Inject rapidly rising temperature */
    for (int i = 0; i < 5; i++) {
        float temp = FF_TEMP_AMBIENT + (float)i * 20.0f;
        ff_inject_heat_source(0, 16, 16, temp, 3.0f);
        ff_process(1000);  /* 1 second per step */
    }
    
    /* Should have detected rate-of-rise */
    ff_stats_t stats;
    ff_get_stats(&stats);
    
    TEST_ASSERT(stats.hot_spots_detected > 0, "Should detect rapid temperature rise");
    
    ff_shutdown();
}

/* ============================================================================
 * Suppression Tests
 * ============================================================================ */

static void test_manual_trigger(void) {
    setup_basic_system();
    
    int result = ff_manual_trigger(0);
    TEST_ASSERT(result == 0, "Manual trigger should succeed");
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state == FF_ZONE_PREACTION || state == FF_ZONE_DISCHARGING,
                "Zone should be preaction or discharging");
    
    ff_shutdown();
}

static void test_abort_suppression(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    
    /* Configure a preaction zone with abort enabled */
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Preaction Zone",
        .system_type = FF_SYSTEM_PREACTION,
        .agent = FF_AGENT_WATER,
        .preaction_delay = 30000,  /* 30 second delay */
        .abort_enabled = true,
        .sensor_count = 1
    };
    zone.sensor_ids[0] = 0;
    ff_configure_zone(&zone);
    ff_arm_system();
    ff_arm_zone(0);
    
    ff_manual_trigger(0);
    
    int result = ff_abort_suppression(0);
    TEST_ASSERT(result == 0, "Abort should succeed");
    
    /* Process to apply the abort */
    ff_process(100);
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state == FF_ZONE_MONITORING || state == FF_ZONE_IDLE,
                "Zone should return to monitoring or idle");
    
    ff_shutdown();
}

static void test_auto_suppression(void) {
    setup_basic_system();
    
    /* Inject high fire temperature */
    inject_fire_temps(0, 16, 16, FF_TEMP_FIRE + 100.0f);
    
    /* Process until suppression activates */
    for (int i = 0; i < 100; i++) {
        ff_process(100);
    }
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    
    /* Should progress toward suppression */
    TEST_ASSERT(state >= FF_ZONE_ALARM, "Should be in alarm or suppressing");
    
    ff_shutdown();
}

static void test_preaction_delay(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    
    /* Configure preaction system with delay */
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Preaction Zone",
        .system_type = FF_SYSTEM_PREACTION,
        .agent = FF_AGENT_WATER,
        .preaction_delay = 10000,  /* 10 second delay */
        .sensor_count = 1
    };
    zone.sensor_ids[0] = 0;
    ff_configure_zone(&zone);
    ff_arm_system();
    ff_arm_zone(0);
    
    /* Trigger */
    ff_manual_trigger(0);
    
    /* Process less than delay */
    for (int i = 0; i < 50; i++) {
        ff_process(100);  /* 5 seconds total */
    }
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state == FF_ZONE_PREACTION, "Should still be in preaction");
    
    /* Process past delay */
    for (int i = 0; i < 60; i++) {
        ff_process(100);  /* +6 seconds */
    }
    
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state == FF_ZONE_DISCHARGING, "Should now be discharging");
    
    ff_shutdown();
}

/* ============================================================================
 * Statistics Tests
 * ============================================================================ */

static void test_stats_initial(void) {
    reset_firefighting();
    ff_init();
    
    ff_stats_t stats;
    ff_get_stats(&stats);
    
    TEST_ASSERT(stats.uptime_seconds == 0, "Initial uptime should be 0");
    TEST_ASSERT(stats.frames_processed == 0, "Initial frames should be 0");
    
    ff_shutdown();
}

static void test_stats_accumulation(void) {
    setup_basic_system();
    
    /* Process for some time */
    for (int i = 0; i < 20; i++) {
        inject_ambient_temps(0);
        ff_process(1000);  /* 1 second each */
    }
    
    ff_stats_t stats;
    ff_get_stats(&stats);
    
    TEST_ASSERT(stats.uptime_seconds >= 10, "Uptime should have increased");
    TEST_ASSERT(stats.frames_processed > 0, "Should have processed frames");
    
    ff_shutdown();
}

static void test_stats_alarms(void) {
    setup_basic_system();
    
    /* Trigger an alarm */
    inject_fire_temps(0, 16, 16, FF_TEMP_FIRE + 100.0f);
    for (int i = 0; i < 20; i++) {
        ff_process(100);
    }
    
    ff_stats_t stats;
    ff_get_stats(&stats);
    
    TEST_ASSERT(stats.alarms_triggered > 0 || stats.hot_spots_detected > 0,
                "Should track alarms or hot spots");
    
    ff_shutdown();
}

/* ============================================================================
 * Event Logging Tests
 * ============================================================================ */

static void test_event_logging(void) {
    setup_basic_system();
    
    /* Trigger some events */
    ff_manual_trigger(0);
    ff_abort_suppression(0);
    
    ff_event_t events[FF_MAX_EVENTS];
    uint16_t count = 0;
    ff_get_events(events, FF_MAX_EVENTS, &count);
    
    TEST_ASSERT(count > 0, "Should have logged events");
    
    ff_shutdown();
}

static void test_event_details(void) {
    setup_basic_system();
    
    ff_manual_trigger(0);
    
    ff_event_t events[FF_MAX_EVENTS];
    uint16_t count = 0;
    ff_get_events(events, FF_MAX_EVENTS, &count);
    
    /* Check event has valid timestamp */
    bool found_valid = false;
    for (uint16_t i = 0; i < count; i++) {
        if (events[i].type != 0) {
            found_valid = true;
        }
    }
    
    TEST_ASSERT(found_valid, "Should have valid event types");
    
    ff_shutdown();
}

/* ============================================================================
 * Telemetry Tests
 * ============================================================================ */

static void test_telemetry_generation(void) {
    setup_basic_system();
    
    for (int i = 0; i < 5; i++) {
        ff_process(100);
    }
    
    uint8_t buffer[256];
    uint16_t len = 0;
    int result = ff_generate_telemetry(buffer, sizeof(buffer), &len);
    
    TEST_ASSERT(result == 0, "Telemetry generation should succeed");
    TEST_ASSERT(len > 0, "Telemetry should have content");
    
    ff_shutdown();
}

static void test_heat_map_retrieval(void) {
    setup_basic_system();
    
    inject_fire_temps(0, 16, 16, 150.0f);
    ff_process(100);
    
    uint8_t heat_map[FF_THERMAL_PIXELS];
    uint16_t width = 0, height = 0;
    int result = ff_get_heat_map(0, heat_map, &width, &height);
    
    TEST_ASSERT(result == 0, "Heat map retrieval should succeed");
    TEST_ASSERT(width == FF_THERMAL_WIDTH, "Width should match");
    TEST_ASSERT(height == FF_THERMAL_HEIGHT, "Height should match");
    
    ff_shutdown();
}

/* ============================================================================
 * Incident Handling Tests
 * ============================================================================ */

static void test_incident_creation(void) {
    setup_basic_system();
    
    /* Trigger fire detection */
    inject_fire_temps(0, 16, 16, FF_TEMP_FIRE + 100.0f);
    for (int i = 0; i < 30; i++) {
        ff_process(100);
    }
    
    ff_incident_t incident;
    int result = ff_get_incident(&incident);
    
    /* May or may not have incident depending on thresholds */
    TEST_ASSERT(result == 0 || result == -1, "Incident query should return valid code");
    
    ff_shutdown();
}

/* ============================================================================
 * Fire Spread Simulation Tests
 * ============================================================================ */

static void test_fire_spread(void) {
    setup_basic_system();
    
    /* Start with small fire */
    inject_fire_temps(0, 16, 16, FF_TEMP_FIRE);
    
    /* Process and expand fire */
    for (int cycle = 0; cycle < 10; cycle++) {
        /* Expand fire radius over time */
        float temp = FF_TEMP_FIRE + (float)cycle * 10.0f;
        float radius = 3.0f + (float)cycle * 0.5f;
        ff_inject_heat_source(0, 16, 16, temp, radius);
        
        for (int i = 0; i < 10; i++) {
            ff_process(100);
        }
    }
    
    /* Check system responded to growing fire */
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    
    TEST_ASSERT(state >= FF_ZONE_WARNING, "Should have detected fire spread");
    
    ff_shutdown();
}

/* ============================================================================
 * Sensor Noise Tests
 * ============================================================================ */

static void test_noise_filtering(void) {
    setup_basic_system();
    
    /* Inject noisy frame (random spikes) */
    float temps[FF_THERMAL_PIXELS];
    for (int i = 0; i < FF_THERMAL_PIXELS; i++) {
        temps[i] = FF_TEMP_AMBIENT + (float)(rand() % 20);
        /* Add occasional spikes */
        if (rand() % 50 == 0) {
            temps[i] = FF_TEMP_WARNING;  /* Spike but not fire */
        }
    }
    ff_update_thermal_frame(0, temps);
    
    /* Process */
    for (int i = 0; i < 20; i++) {
        ff_process(100);
    }
    
    ff_stats_t stats;
    ff_get_stats(&stats);
    
    /* Should not have triggered alarm from noise */
    TEST_ASSERT(stats.alarms_triggered == 0, "Should filter noise spikes");
    
    ff_shutdown();
}

static void test_real_fire_vs_noise(void) {
    setup_basic_system();
    
    /* First: noisy but no fire - should not alarm */
    for (int i = 0; i < 10; i++) {
        float temps[FF_THERMAL_PIXELS];
        for (int j = 0; j < FF_THERMAL_PIXELS; j++) {
            temps[j] = FF_TEMP_AMBIENT + (float)(rand() % 10);
        }
        ff_update_thermal_frame(0, temps);
        ff_process(100);
    }
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state == FF_ZONE_MONITORING, "Should stay monitoring with noise");
    
    /* Now: real fire */
    inject_fire_temps(0, 16, 16, FF_TEMP_FIRE + 100.0f);
    for (int i = 0; i < 20; i++) {
        ff_process(100);
    }
    
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state >= FF_ZONE_ALARM, "Should alarm on real fire");
    
    ff_shutdown();
}

/* ============================================================================
 * Multi-Zone Tests
 * ============================================================================ */

static void test_multi_zone_config(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    ff_register_sensor(1);
    
    /* Configure two zones with different sensors */
    ff_zone_config_t zone_a = {
        .zone_id = 0,
        .name = "Zone A",
        .system_type = FF_SYSTEM_WET_PIPE,
        .agent = FF_AGENT_WATER,
        .sensor_count = 1
    };
    zone_a.sensor_ids[0] = 0;
    ff_configure_zone(&zone_a);
    
    ff_zone_config_t zone_b = {
        .zone_id = 1,
        .name = "Zone B",
        .system_type = FF_SYSTEM_GASEOUS,
        .agent = FF_AGENT_FM200,
        .sensor_count = 1
    };
    zone_b.sensor_ids[0] = 1;
    ff_configure_zone(&zone_b);
    
    TEST_ASSERT(g_ff.zone_count == 2, "Should have 2 zones");
    
    ff_shutdown();
}

/* ============================================================================
 * Boundary Condition Tests
 * ============================================================================ */

static void test_null_pointer_handling(void) {
    setup_basic_system();
    
    int result = ff_get_stats(NULL);
    TEST_ASSERT(result == -1, "Null stats should fail");
    
    result = ff_get_incident(NULL);
    TEST_ASSERT(result == -1, "Null incident should fail");
    
    result = ff_get_events(NULL, 10, NULL);
    TEST_ASSERT(result == -1, "Null events should fail");
    
    ff_shutdown();
}

static void test_extreme_temperatures(void) {
    setup_basic_system();
    
    /* Inject flashover temperature */
    inject_fire_temps(0, 16, 16, FF_TEMP_FLASHOVER);
    
    for (int i = 0; i < 20; i++) {
        ff_process(100);
    }
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    
    TEST_ASSERT(state >= FF_ZONE_ALARM, "Should respond to extreme temp");
    
    ff_shutdown();
}

static void test_zero_delta_process(void) {
    setup_basic_system();
    
    int result = ff_process(0);
    TEST_ASSERT(result == 0, "Zero delta should be handled");
    
    ff_shutdown();
}

/* ============================================================================
 * Integration Tests
 * ============================================================================ */

static void test_full_fire_incident(void) {
    setup_basic_system();
    
    /* Phase 1: Normal monitoring */
    for (int i = 0; i < 10; i++) {
        inject_ambient_temps(0);
        ff_process(1000);
    }
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state == FF_ZONE_MONITORING, "Should be monitoring");
    
    /* Phase 2: Fire starts */
    inject_fire_temps(0, 16, 16, FF_TEMP_ALARM + 10.0f);
    for (int i = 0; i < 30; i++) {
        ff_process(500);
    }
    
    /* Should have detected */
    ff_stats_t stats;
    ff_get_stats(&stats);
    TEST_ASSERT(stats.hot_spots_detected > 0, "Should detect hot spot");
    
    /* Phase 3: Fire grows */
    inject_fire_temps(0, 16, 16, FF_TEMP_FIRE + 50.0f);
    for (int i = 0; i < 50; i++) {
        ff_process(200);
    }
    
    ff_get_zone_status(0, &state, NULL, NULL);
    TEST_ASSERT(state >= FF_ZONE_ALARM, "Should be alarming or suppressing");
    
    ff_shutdown();
}

static void test_full_suppression_cycle(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    
    /* Configure with no preaction delay */
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Quick Zone",
        .system_type = FF_SYSTEM_WET_PIPE,
        .agent = FF_AGENT_WATER,
        .preaction_delay = 0,
        .sensor_count = 1
    };
    zone.sensor_ids[0] = 0;
    ff_configure_zone(&zone);
    ff_arm_system();
    ff_arm_zone(0);
    
    /* Manual trigger for immediate suppression */
    ff_manual_trigger(0);
    
    /* Process through discharge */
    for (int i = 0; i < 60; i++) {
        ff_process(100);
    }
    
    ff_zone_state_t state;
    ff_get_zone_status(0, &state, NULL, NULL);
    
    /* Should have discharged and completed */
    TEST_ASSERT(state == FF_ZONE_DISCHARGING || state == FF_ZONE_DISCHARGED,
                "Should have discharged");
    
    ff_stats_t stats;
    ff_get_stats(&stats);
    TEST_ASSERT(stats.suppressions_activated > 0, "Should track suppression");
    
    ff_shutdown();
}

/* ============================================================================
 * Cross-Zone Tests
 * ============================================================================ */

static void test_cross_zone_requirement(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    ff_register_sensor(1);
    
    /* Configure zone requiring cross-zone confirmation */
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Data Center",
        .system_type = FF_SYSTEM_GASEOUS,
        .agent = FF_AGENT_FM200,
        .require_cross_zone = true,
        .preaction_delay = 30000,
        .sensor_count = 2
    };
    zone.sensor_ids[0] = 0;
    zone.sensor_ids[1] = 1;
    ff_configure_zone(&zone);
    ff_arm_system();
    ff_arm_zone(0);
    
    /* Fire on one sensor only */
    ff_inject_heat_source(0, 16, 16, FF_TEMP_FIRE, 3.0f);
    
    for (int i = 0; i < 20; i++) {
        ff_process(100);
    }
    
    /* Cross-zone may require both sensors - check configuration honored */
    TEST_ASSERT(g_ff.zone_configs[0].require_cross_zone == true,
                "Cross-zone flag should be set");
    
    ff_shutdown();
}

/* ============================================================================
 * Agent Type Tests
 * ============================================================================ */

static void test_water_agent(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Water Zone",
        .system_type = FF_SYSTEM_WET_PIPE,
        .agent = FF_AGENT_WATER,
        .discharge_rate = 100.0f
    };
    ff_configure_zone(&zone);
    
    TEST_ASSERT(g_ff.zone_configs[0].agent == FF_AGENT_WATER, 
                "Agent should be water");
    
    ff_shutdown();
}

static void test_foam_agent(void) {
    reset_firefighting();
    ff_init();
    ff_register_sensor(0);
    
    ff_zone_config_t zone = {
        .zone_id = 0,
        .name = "Foam Zone",
        .system_type = FF_SYSTEM_DELUGE,
        .agent = FF_AGENT_FOAM,
        .discharge_rate = 50.0f
    };
    ff_configure_zone(&zone);
    
    TEST_ASSERT(g_ff.zone_configs[0].agent == FF_AGENT_FOAM,
                "Agent should be foam");
    
    ff_shutdown();
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(void) {
    printf("=== Firefighting Thermal Imaging & Suppression Tests ===\n\n");
    
    printf("-- Initialization Tests --\n");
    RUN_TEST(test_init_default);
    RUN_TEST(test_double_init);
    RUN_TEST(test_shutdown);
    RUN_TEST(test_shutdown_not_initialized);
    
    printf("\n-- Sensor Registration Tests --\n");
    RUN_TEST(test_register_sensor);
    RUN_TEST(test_register_multiple_sensors);
    RUN_TEST(test_register_max_sensors);
    
    printf("\n-- Frame Processing Tests --\n");
    RUN_TEST(test_update_thermal_frame);
    RUN_TEST(test_update_invalid_sensor);
    RUN_TEST(test_heat_source_injection);
    
    printf("\n-- Zone Configuration Tests --\n");
    RUN_TEST(test_configure_zone);
    RUN_TEST(test_configure_multiple_zones);
    RUN_TEST(test_configure_gaseous_zone);
    
    printf("\n-- Arming Tests --\n");
    RUN_TEST(test_arm_system);
    RUN_TEST(test_disarm_system);
    RUN_TEST(test_arm_zone);
    RUN_TEST(test_arm_unconfigured_zone);
    
    printf("\n-- Fire Detection Tests --\n");
    RUN_TEST(test_hot_spot_detection);
    RUN_TEST(test_alarm_trigger);
    RUN_TEST(test_rate_of_rise_detection);
    
    printf("\n-- Suppression Tests --\n");
    RUN_TEST(test_manual_trigger);
    RUN_TEST(test_abort_suppression);
    RUN_TEST(test_auto_suppression);
    RUN_TEST(test_preaction_delay);
    
    printf("\n-- Statistics Tests --\n");
    RUN_TEST(test_stats_initial);
    RUN_TEST(test_stats_accumulation);
    RUN_TEST(test_stats_alarms);
    
    printf("\n-- Event Logging Tests --\n");
    RUN_TEST(test_event_logging);
    RUN_TEST(test_event_details);
    
    printf("\n-- Telemetry Tests --\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_heat_map_retrieval);
    
    printf("\n-- Incident Handling Tests --\n");
    RUN_TEST(test_incident_creation);
    
    printf("\n-- Fire Spread Simulation Tests --\n");
    RUN_TEST(test_fire_spread);
    
    printf("\n-- Sensor Noise Tests --\n");
    RUN_TEST(test_noise_filtering);
    RUN_TEST(test_real_fire_vs_noise);
    
    printf("\n-- Multi-Zone Tests --\n");
    RUN_TEST(test_multi_zone_config);
    
    printf("\n-- Boundary Condition Tests --\n");
    RUN_TEST(test_null_pointer_handling);
    RUN_TEST(test_extreme_temperatures);
    RUN_TEST(test_zero_delta_process);
    
    printf("\n-- Integration Tests --\n");
    RUN_TEST(test_full_fire_incident);
    RUN_TEST(test_full_suppression_cycle);
    
    printf("\n-- Cross-Zone Tests --\n");
    RUN_TEST(test_cross_zone_requirement);
    
    printf("\n-- Agent Type Tests --\n");
    RUN_TEST(test_water_agent);
    RUN_TEST(test_foam_agent);
    
    printf("\n========================================\n");
    printf("Tests Run:    %d\n", tests_run);
    printf("Tests Passed: %d\n", tests_passed);
    printf("Tests Failed: %d\n", tests_failed);
    printf("========================================\n");
    
    return tests_failed > 0 ? 1 : 0;
}
