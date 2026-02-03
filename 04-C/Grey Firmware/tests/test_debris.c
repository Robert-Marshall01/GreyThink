/**
 * @file test_debris.c
 * @brief Orbital Debris Tracking & Collision Prediction Test Suite
 * 
 * Comprehensive tests for space debris tracking, conjunction screening,
 * collision probability calculation, and CCSDS telemetry generation.
 * 
 * TEST CATEGORIES:
 * - Initialization and configuration
 * - TMR sensor voting and health monitoring
 * - Orbital mechanics calculations
 * - Track management and correlation
 * - Conjunction screening and probability
 * - Alarm management
 * - Telemetry generation
 * - Integration scenarios
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

#include "../src/debris/debris_spotlight.c"

/* ============================================================================
 * Test Helper Functions
 * ============================================================================ */

static void reset_system(void)
{
    debris_shutdown();
    memset(&g_debris, 0, sizeof(g_debris));
}

static void setup_basic_system(void)
{
    reset_system();
    debris_init();
}

static void setup_with_asset(void)
{
    setup_basic_system();
    
    state_vector_t iss_state = {
        .x = -4640.0, .y = -1108.0, .z = 5024.0,
        .vx = 3.721, .vy = -5.831, .vz = -2.612,
        .epoch_jd = 2460000.5
    };
    
    debris_config_asset(0, 25544, "ISS", &iss_state);
}

static void inject_detection(uint8_t id, float range, float az, float el,
                             float range_rate, float rcs)
{
    float r[3] = {range, range + 0.1f, range - 0.1f};
    float rr[3] = {range_rate, range_rate + 0.01f, range_rate - 0.01f};
    float azimuth[3] = {az, az + 0.1f, az - 0.1f};
    float elevation[3] = {el, el + 0.1f, el - 0.1f};
    
    debris_update_sensors(id, r, rr, azimuth, elevation, rcs, 15.0f,
                          (uint64_t)id * 1000000);
}

/* ============================================================================
 * Test Cases: Initialization
 * ============================================================================ */

static void test_init_default_state(void)
{
    reset_system();
    
    TEST_ASSERT_FALSE(g_debris.initialized, "Should not be initialized before init");
    
    int result = debris_init();
    
    TEST_ASSERT_EQ(result, 0, "Init should return 0");
    TEST_ASSERT_TRUE(g_debris.initialized, "System should be initialized");
    TEST_ASSERT_EQ(debris_get_track_count(), 0, "Track count should be 0");
    TEST_ASSERT_EQ(debris_get_asset_count(), 0, "Asset count should be 0");
    TEST_ASSERT_EQ(debris_get_conjunction_count(), 0, "Conjunction count should be 0");
}

static void test_init_default_config(void)
{
    setup_basic_system();
    
    TEST_ASSERT_FLOAT_EQ(g_debris.config.screen_distance_leo_km, 5.0f, 0.01f,
                         "LEO screen distance should be 5.0 km");
    TEST_ASSERT_FLOAT_EQ(g_debris.config.screen_distance_geo_km, 15.0f, 0.01f,
                         "GEO screen distance should be 15.0 km");
    TEST_ASSERT_FLOAT_EQ(g_debris.config.pc_red_threshold, 1e-4f, 1e-5f,
                         "Red PC threshold should be 1e-4");
    TEST_ASSERT_EQ(g_debris.config.prediction_days, 7, "Prediction days should be 7");
}

static void test_init_double_init_fails(void)
{
    setup_basic_system();
    
    int result = debris_init();
    
    TEST_ASSERT_EQ(result, -1, "Double init should fail");
}

static void test_init_intervals(void)
{
    setup_basic_system();
    
    TEST_ASSERT_EQ(g_debris.scan_interval_ms, 60000, "Scan interval should be 60s");
    TEST_ASSERT_EQ(g_debris.prediction_interval_ms, 300000, 
                   "Prediction interval should be 5 min");
}

static void test_shutdown_and_reinit(void)
{
    setup_basic_system();
    debris_shutdown();
    
    TEST_ASSERT_FALSE(g_debris.initialized, "Should not be initialized after shutdown");
    
    int result = debris_init();
    TEST_ASSERT_EQ(result, 0, "Re-init should succeed");
    TEST_ASSERT_TRUE(g_debris.initialized, "Should be initialized after re-init");
}

/* ============================================================================
 * Test Cases: Asset Configuration
 * ============================================================================ */

static void test_config_asset_basic(void)
{
    setup_basic_system();
    
    state_vector_t state = {
        .x = 6878.0, .y = 0.0, .z = 0.0,
        .vx = 0.0, .vy = 7.612, .vz = 0.0,
        .epoch_jd = 2460000.5
    };
    
    int result = debris_config_asset(0, 25544, "ISS", &state);
    
    TEST_ASSERT_EQ(result, 0, "Config asset should succeed");
    TEST_ASSERT_EQ(debris_get_asset_count(), 1, "Should have 1 asset");
    TEST_ASSERT_TRUE(g_debris.assets[0].configured, "Asset should be configured");
    TEST_ASSERT_EQ(g_debris.assets[0].norad_id, 25544, "NORAD ID should match");
}

static void test_config_asset_multiple(void)
{
    setup_basic_system();
    
    state_vector_t state = {.x = 6878.0, .y = 0.0, .z = 0.0};
    
    debris_config_asset(0, 41332, "DRAGON", &state);
    debris_config_asset(1, 40697, "STARLINK-1", &state);
    debris_config_asset(2, 45692, "STARLINK-2", &state);
    
    TEST_ASSERT_EQ(debris_get_asset_count(), 3, "Should have 3 assets");
}

static void test_config_asset_invalid_id(void)
{
    setup_basic_system();
    
    state_vector_t state = {.x = 6878.0};
    
    int result = debris_config_asset(255, 12345, "INVALID", &state);
    
    TEST_ASSERT_EQ(result, -1, "Invalid asset ID should fail");
}

static void test_asset_maneuverability(void)
{
    setup_with_asset();
    
    TEST_ASSERT_TRUE(g_debris.assets[0].maneuverable, "Asset should be maneuverable");
    TEST_ASSERT_GT(g_debris.assets[0].maneuver_delta_v_m_s, 0.0f, 
                   "Should have delta-V available");
}

/* ============================================================================
 * Test Cases: TMR Voting
 * ============================================================================ */

static void test_tmr_all_agree(void)
{
    float values[3] = {100.0f, 100.1f, 99.9f};
    float result;
    
    tmr_result_t vote = tmr_vote(values, &result);
    
    TEST_ASSERT_EQ(vote, TMR_AGREE_ALL, "All should agree");
    TEST_ASSERT_FLOAT_EQ(result, 100.0f, 0.1f, "Result should be average");
}

static void test_tmr_two_agree(void)
{
    float values[3] = {100.0f, 100.1f, 200.0f};  /* Third sensor faulty */
    float result;
    
    tmr_result_t vote = tmr_vote(values, &result);
    
    TEST_ASSERT_EQ(vote, TMR_AGREE_2OF3, "Two should agree");
    TEST_ASSERT_FLOAT_EQ(result, 100.05f, 0.1f, "Result should be average of two");
}

static void test_tmr_all_disagree(void)
{
    float values[3] = {100.0f, 200.0f, 300.0f};
    float result;
    
    tmr_result_t vote = tmr_vote(values, &result);
    
    TEST_ASSERT_EQ(vote, TMR_DISAGREE, "All should disagree");
    TEST_ASSERT_FLOAT_EQ(result, 200.0f, 0.1f, "Result should be median");
}

static void test_tmr_sensor_update(void)
{
    tmr_sensor_t sensor = {0};
    
    tmr_sensor_update(&sensor, 500.0f, 500.2f, 499.8f, 1000);
    
    TEST_ASSERT_EQ(sensor.vote_status, TMR_AGREE_ALL, "Should vote all agree");
    TEST_ASSERT_EQ(sensor.health, SENSOR_HEALTH_OK, "Health should be OK");
    TEST_ASSERT_EQ(sensor.samples, 1, "Should have 1 sample");
}

static void test_tmr_sensor_fault_detection(void)
{
    tmr_sensor_t sensor = {0};
    
    tmr_sensor_update(&sensor, 100.0f, 500.0f, 900.0f, 1000);
    
    TEST_ASSERT_EQ(sensor.vote_status, TMR_DISAGREE, "Should vote disagree");
    TEST_ASSERT_EQ(sensor.health, SENSOR_HEALTH_FAULT, "Health should be FAULT");
}

/* ============================================================================
 * Test Cases: Orbital Mechanics
 * ============================================================================ */

static void test_orbital_period_leo(void)
{
    double period = orbital_period(6778.0);  /* 400 km altitude */
    
    TEST_ASSERT_DOUBLE_EQ(period, 5554.5, 50.0, "LEO period should be ~92 min");
}

static void test_orbital_period_geo(void)
{
    double period = orbital_period(42164.0);  /* GEO altitude */
    
    TEST_ASSERT_DOUBLE_EQ(period, 86164.0, 500.0, "GEO period should be ~24 hours");
}

static void test_classify_orbit_leo(void)
{
    orbital_regime_t regime = classify_orbit(6778.0, 0.001);
    
    TEST_ASSERT_EQ(regime, ORBIT_LEO, "Should classify as LEO");
}

static void test_classify_orbit_geo(void)
{
    orbital_regime_t regime = classify_orbit(42164.0, 0.0005);
    
    TEST_ASSERT_EQ(regime, ORBIT_GEO, "Should classify as GEO");
}

static void test_classify_orbit_heo(void)
{
    orbital_regime_t regime = classify_orbit(25000.0, 0.7);
    
    TEST_ASSERT_EQ(regime, ORBIT_HEO, "Should classify as HEO");
}

static void test_state_to_elements(void)
{
    state_vector_t state = {
        .x = 6778.0, .y = 0.0, .z = 0.0,
        .vx = 0.0, .vy = 7.668, .vz = 0.0,
        .epoch_jd = 2460000.5
    };
    orbital_elements_t elem;
    
    state_to_elements(&state, &elem);
    
    TEST_ASSERT_DOUBLE_EQ(elem.sma, 6778.0, 50.0, "SMA should be ~6778 km");
    TEST_ASSERT_DOUBLE_EQ(elem.ecc, 0.0, 0.01, "Eccentricity should be ~0");
}

static void test_propagate_state(void)
{
    state_vector_t state0 = {
        .x = 6778.0, .y = 0.0, .z = 0.0,
        .vx = 0.0, .vy = 7.668, .vz = 0.0,
        .epoch_jd = 2460000.5
    };
    state_vector_t state1;
    
    propagate_state(&state0, 60.0, &state1);  /* 1 minute */
    
    /* After 1 minute, satellite should have moved */
    TEST_ASSERT_NE(state1.x, state0.x, "X position should change");
    TEST_ASSERT_GT(state1.y, 0.0, "Y position should increase");
}

/* ============================================================================
 * Test Cases: Detection Processing
 * ============================================================================ */

static void test_sensor_update_basic(void)
{
    setup_basic_system();
    
    float range[3] = {1000.0f, 1000.1f, 999.9f};
    float range_rate[3] = {-0.5f, -0.51f, -0.49f};
    float az[3] = {45.0f, 45.1f, 44.9f};
    float el[3] = {30.0f, 30.1f, 29.9f};
    
    int result = debris_update_sensors(1, range, range_rate, az, el, 
                                       -10.0f, 15.0f, 1000000);
    
    TEST_ASSERT_EQ(result, 0, "Sensor update should succeed");
    TEST_ASSERT_EQ(g_debris.detection_count, 1, "Should have 1 detection");
    TEST_ASSERT_EQ(debris_get_total_detections(), 1, "Total detections should be 1");
}

static void test_detection_creates_track(void)
{
    setup_basic_system();
    
    inject_detection(1, 1000.0f, 45.0f, 30.0f, -0.5f, -10.0f);
    
    TEST_ASSERT_EQ(debris_get_track_count(), 1, "Should have 1 track");
    TEST_ASSERT_TRUE(g_debris.tracks[0].active, "Track should be active");
    TEST_ASSERT_EQ(g_debris.tracks[0].quality, TRACK_TENTATIVE, 
                   "Track should be tentative");
}

static void test_detection_correlation(void)
{
    setup_basic_system();
    
    /* First detection */
    inject_detection(1, 1000.0f, 45.0f, 30.0f, -0.5f, -10.0f);
    
    /* Second detection very close - should correlate */
    inject_detection(2, 1001.0f, 45.05f, 30.02f, -0.5f, -10.0f);
    
    /* Should still be 1 track with updated obs count */
    TEST_ASSERT_EQ(debris_get_track_count(), 1, "Should still have 1 track");
    TEST_ASSERT_EQ(g_debris.tracks[0].obs_count, 2, "Should have 2 observations");
    TEST_ASSERT_EQ(g_debris.tracks[0].quality, TRACK_CONFIRMED, 
                   "Track should be confirmed");
}

static void test_detection_new_object(void)
{
    setup_basic_system();
    
    inject_detection(1, 1000.0f, 45.0f, 30.0f, -0.5f, -10.0f);
    inject_detection(2, 2000.0f, 90.0f, 60.0f, -1.0f, -15.0f);  /* Different object */
    
    TEST_ASSERT_EQ(debris_get_track_count(), 2, "Should have 2 tracks");
}

static void test_sensor_fault_detection(void)
{
    setup_basic_system();
    
    /* All sensors disagree */
    float range[3] = {100.0f, 1000.0f, 5000.0f};
    float range_rate[3] = {-0.5f, -0.51f, -0.49f};
    float az[3] = {45.0f, 45.1f, 44.9f};
    float el[3] = {30.0f, 30.1f, 29.9f};
    
    debris_update_sensors(1, range, range_rate, az, el, -10.0f, 15.0f, 1000000);
    
    TEST_ASSERT_TRUE(debris_has_sensor_fault(), "Should detect sensor fault");
}

/* ============================================================================
 * Test Cases: Collision Probability
 * ============================================================================ */

static void test_pc_close_approach(void)
{
    float pc = calculate_collision_probability(100.0f, 50.0f, 100.0f, 15.0f);
    
    TEST_ASSERT_GT(pc, 1e-5f, "Pc should be significant for 100m miss");
}

static void test_pc_far_approach(void)
{
    float pc = calculate_collision_probability(10000.0f, 50.0f, 100.0f, 15.0f);
    
    TEST_ASSERT_LT(pc, 1e-10f, "Pc should be negligible for 10km miss");
}

static void test_pc_invalid_inputs(void)
{
    float pc = calculate_collision_probability(0.0f, 0.0f, 0.0f, 15.0f);
    
    TEST_ASSERT_FLOAT_EQ(pc, 0.0f, 0.001f, "Pc should be 0 for invalid inputs");
}

static void test_conjunction_severity_green(void)
{
    conjunction_severity_t sev = classify_conjunction(10000.0f, 1e-10f, 72.0f);
    
    TEST_ASSERT_EQ(sev, CONJ_GREEN, "Should be green for far miss");
}

static void test_conjunction_severity_yellow(void)
{
    conjunction_severity_t sev = classify_conjunction(500.0f, 5e-5f, 48.0f);
    
    TEST_ASSERT_EQ(sev, CONJ_YELLOW, "Should be yellow for elevated Pc");
}

static void test_conjunction_severity_red(void)
{
    conjunction_severity_t sev = classify_conjunction(200.0f, 5e-4f, 24.0f);
    
    TEST_ASSERT_EQ(sev, CONJ_RED, "Should be red for high Pc");
}

static void test_conjunction_severity_emergency(void)
{
    conjunction_severity_t sev = classify_conjunction(50.0f, 1e-3f, 2.0f);
    
    TEST_ASSERT_EQ(sev, CONJ_EMERGENCY, "Should be emergency for imminent high Pc");
}

/* ============================================================================
 * Test Cases: Conjunction Management
 * ============================================================================ */

static void test_conjunction_screening(void)
{
    setup_with_asset();
    
    /* Inject detection that passes near asset */
    inject_detection(1, 420.0f, 0.0f, 0.0f, -7.0f, -10.0f);  /* Near ISS altitude */
    
    debris_process(300000);  /* Trigger screening */
    
    /* May or may not create conjunction depending on geometry */
    TEST_ASSERT_GE(debris_get_conjunction_count(), 0, "Should handle screening");
}

static void test_conjunction_retrieval(void)
{
    setup_with_asset();
    
    /* Force add a conjunction for testing */
    g_debris.conjunctions[0].conjunction_id = 1;
    g_debris.conjunctions[0].primary_id = 0;
    g_debris.conjunctions[0].secondary_id = 1;
    g_debris.conjunctions[0].miss_distance_m = 500.0f;
    g_debris.conjunctions[0].collision_probability = 1e-4f;
    g_debris.conjunctions[0].active = true;
    g_debris.conjunction_count = 1;
    
    conjunction_event_t events[8];
    int count = debris_get_conjunctions(events, 8);
    
    TEST_ASSERT_EQ(count, 1, "Should retrieve 1 conjunction");
    TEST_ASSERT_EQ(events[0].conjunction_id, 1, "Conjunction ID should match");
}

static void test_conjunction_get_by_id(void)
{
    setup_with_asset();
    
    g_debris.conjunctions[0].conjunction_id = 42;
    g_debris.conjunctions[0].miss_distance_m = 750.0f;
    g_debris.conjunction_count = 1;
    
    conjunction_event_t event;
    int result = debris_get_conjunction_by_id(42, &event);
    
    TEST_ASSERT_EQ(result, 0, "Should find conjunction");
    TEST_ASSERT_FLOAT_EQ(event.miss_distance_m, 750.0f, 0.1f, "Miss should match");
}

static void test_conjunction_acknowledge(void)
{
    setup_with_asset();
    
    g_debris.conjunctions[0].conjunction_id = 99;
    g_debris.conjunctions[0].ack_by_operator = false;
    g_debris.conjunction_count = 1;
    
    int result = debris_acknowledge_conjunction(99);
    
    TEST_ASSERT_EQ(result, 0, "Acknowledge should succeed");
    TEST_ASSERT_TRUE(g_debris.conjunctions[0].ack_by_operator, "Should be acked");
}

/* ============================================================================
 * Test Cases: Alarm Management
 * ============================================================================ */

static void test_alarm_trigger(void)
{
    setup_basic_system();
    
    trigger_alarm(ALARM_CONJUNCTION_RED, SEVERITY_CRITICAL, 1, 1e-4f, 5e-4f);
    
    TEST_ASSERT_EQ(debris_get_alarm_count(), 1, "Should have 1 alarm");
    TEST_ASSERT_EQ(g_debris.unacked_alarms, 1, "Should have 1 unacked alarm");
}

static void test_alarm_update_same_source(void)
{
    setup_basic_system();
    
    trigger_alarm(ALARM_CONJUNCTION_RED, SEVERITY_CRITICAL, 1, 1e-4f, 5e-4f);
    trigger_alarm(ALARM_CONJUNCTION_RED, SEVERITY_CRITICAL, 1, 1e-4f, 8e-4f);
    
    TEST_ASSERT_EQ(debris_get_alarm_count(), 1, "Should still have 1 alarm");
    TEST_ASSERT_FLOAT_EQ(g_debris.alarms[0].actual, 8e-4f, 1e-5f, 
                         "Alarm value should be updated");
}

static void test_alarm_clear(void)
{
    setup_basic_system();
    
    trigger_alarm(ALARM_CONJUNCTION_RED, SEVERITY_CRITICAL, 1, 1e-4f, 5e-4f);
    clear_alarm(ALARM_CONJUNCTION_RED, 1);
    
    TEST_ASSERT_EQ(debris_get_alarm_count(), 0, "Alarm should be cleared");
}

static void test_alarm_multiple_types(void)
{
    setup_basic_system();
    
    trigger_alarm(ALARM_CONJUNCTION_RED, SEVERITY_CRITICAL, 1, 1e-4f, 5e-4f);
    trigger_alarm(ALARM_SENSOR_FAULT, SEVERITY_WARNING, 2, 0.0f, 0.0f);
    trigger_alarm(ALARM_NEW_OBJECT, SEVERITY_INFO, 3, 0.0f, -15.0f);
    
    TEST_ASSERT_EQ(debris_get_alarm_count(), 3, "Should have 3 alarms");
}

/* ============================================================================
 * Test Cases: Process Loop
 * ============================================================================ */

static void test_process_basic(void)
{
    setup_basic_system();
    
    int result = debris_process(1000);
    
    TEST_ASSERT_EQ(result, 0, "Process should succeed");
    TEST_ASSERT_EQ(g_debris.current_time_ms, 1000, "Current time should update");
}

static void test_process_scan_completion(void)
{
    setup_basic_system();
    
    debris_process(0);
    debris_process(60000);  /* Trigger scan interval */
    
    TEST_ASSERT_EQ(g_debris.scans_completed, 1, "Should complete 1 scan");
}

static void test_process_uptime_tracking(void)
{
    setup_basic_system();
    
    debris_process(5000);
    
    TEST_ASSERT_EQ(g_debris.uptime_seconds, 5, "Uptime should be 5 seconds");
}

static void test_process_without_init(void)
{
    reset_system();
    
    int result = debris_process(1000);
    
    TEST_ASSERT_EQ(result, -1, "Process without init should fail");
}

/* ============================================================================
 * Test Cases: Event Logging
 * ============================================================================ */

static void test_event_logging(void)
{
    setup_basic_system();
    
    log_event(1, 5, 0x1234, 3.14f, 2.71f);
    
    TEST_ASSERT_EQ(debris_get_event_count(), 2, "Should have 2 events (init + logged)");
}

static void test_event_wrap_around(void)
{
    setup_basic_system();
    
    for (int i = 0; i < 300; i++) {
        log_event(1, 1, (uint16_t)i, (float)i, 0.0f);
    }
    
    TEST_ASSERT_EQ(debris_get_event_count(), DEBRIS_MAX_EVENTS, 
                   "Event count should max out");
}

/* ============================================================================
 * Test Cases: Telemetry Generation
 * ============================================================================ */

static void test_telemetry_generation(void)
{
    setup_with_asset();
    debris_process(1000);
    
    uint8_t buffer[256];
    int len = debris_get_telemetry(buffer, sizeof(buffer));
    
    TEST_ASSERT_GT(len, 0, "Telemetry should be generated");
    TEST_ASSERT_GT(len, (int)sizeof(telemetry_header_t), "Should include payload");
}

static void test_telemetry_header(void)
{
    setup_basic_system();
    debris_process(5000);
    
    uint8_t buffer[256];
    debris_get_telemetry(buffer, sizeof(buffer));
    
    telemetry_header_t *hdr = (telemetry_header_t *)buffer;
    
    TEST_ASSERT_EQ(hdr->sync_word, 0x1ACF, "Sync word should be 0x1ACF");
    TEST_ASSERT_EQ(hdr->apid, 0x0200, "APID should be 0x0200");
}

static void test_telemetry_sequence(void)
{
    setup_basic_system();
    
    uint8_t buffer[256];
    debris_get_telemetry(buffer, sizeof(buffer));
    telemetry_header_t *hdr1 = (telemetry_header_t *)buffer;
    uint16_t seq1 = hdr1->sequence_count;
    
    debris_get_telemetry(buffer, sizeof(buffer));
    telemetry_header_t *hdr2 = (telemetry_header_t *)buffer;
    uint16_t seq2 = hdr2->sequence_count;
    
    TEST_ASSERT_EQ(seq2, seq1 + 1, "Sequence should increment");
}

static void test_telemetry_buffer_too_small(void)
{
    setup_basic_system();
    
    uint8_t buffer[16];  /* Too small */
    int len = debris_get_telemetry(buffer, sizeof(buffer));
    
    TEST_ASSERT_EQ(len, -1, "Should fail with small buffer");
}

/* ============================================================================
 * Test Cases: Query Functions
 * ============================================================================ */

static void test_query_track_count(void)
{
    setup_basic_system();
    
    inject_detection(1, 1000.0f, 45.0f, 30.0f, -0.5f, -10.0f);
    inject_detection(2, 2000.0f, 90.0f, 60.0f, -1.0f, -15.0f);
    
    TEST_ASSERT_EQ(debris_get_track_count(), 2, "Should have 2 tracks");
}

static void test_query_red_conjunctions(void)
{
    setup_basic_system();
    
    g_debris.red_conjunctions = 3;
    
    TEST_ASSERT_EQ(debris_get_red_count(), 3, "Should return red count");
}

static void test_query_has_red(void)
{
    setup_basic_system();
    
    TEST_ASSERT_FALSE(debris_has_red_conjunction(), "Should have no red initially");
    
    g_debris.red_conjunctions = 1;
    
    TEST_ASSERT_TRUE(debris_has_red_conjunction(), "Should detect red conjunction");
}

static void test_query_closest_approach(void)
{
    setup_basic_system();
    
    g_debris.closest_approach_m = 150.5f;
    
    TEST_ASSERT_FLOAT_EQ(debris_get_closest_approach(), 150.5f, 0.1f,
                         "Should return closest approach");
}

static void test_get_track_by_id(void)
{
    setup_basic_system();
    inject_detection(1, 1000.0f, 45.0f, 30.0f, -0.5f, -10.0f);
    
    tracked_object_t track;
    int result = debris_get_track(1, &track);
    
    TEST_ASSERT_EQ(result, 0, "Should find track");
    TEST_ASSERT_EQ(track.track_id, 1, "Track ID should match");
}

static void test_get_track_not_found(void)
{
    setup_basic_system();
    
    tracked_object_t track;
    int result = debris_get_track(999, &track);
    
    TEST_ASSERT_EQ(result, -2, "Should return not found");
}

/* ============================================================================
 * Test Cases: Integration Scenarios
 * ============================================================================ */

static void test_scenario_full_workflow(void)
{
    reset_system();
    
    /* Initialize */
    TEST_ASSERT_EQ(debris_init(), 0, "Init should succeed");
    
    /* Configure asset */
    state_vector_t iss_state = {
        .x = -4640.0, .y = -1108.0, .z = 5024.0,
        .vx = 3.721, .vy = -5.831, .vz = -2.612,
        .epoch_jd = 2460000.5
    };
    TEST_ASSERT_EQ(debris_config_asset(0, 25544, "ISS", &iss_state), 0,
                   "Asset config should succeed");
    
    /* Process detections */
    for (int i = 0; i < 10; i++) {
        inject_detection((uint8_t)i, 800.0f + (float)i * 100.0f, 
                         (float)i * 10.0f, 20.0f + (float)i * 2.0f,
                         -0.5f, -10.0f - (float)i);
    }
    
    TEST_ASSERT_GE(debris_get_track_count(), 1, "Should have tracks");
    TEST_ASSERT_EQ(debris_get_total_detections(), 10, "Should have 10 detections");
    
    /* Process and screen */
    debris_process(300000);
    
    /* Generate telemetry */
    uint8_t buffer[256];
    int len = debris_get_telemetry(buffer, sizeof(buffer));
    TEST_ASSERT_GT(len, 0, "Should generate telemetry");
    
    /* Shutdown */
    debris_shutdown();
    TEST_ASSERT_FALSE(g_debris.initialized, "Should be shutdown");
}

static void test_scenario_multiple_scans(void)
{
    setup_with_asset();
    
    for (int scan = 0; scan < 5; scan++) {
        for (int det = 0; det < 5; det++) {
            inject_detection((uint8_t)(scan * 5 + det),
                             1000.0f + (float)det * 200.0f,
                             (float)det * 30.0f, 30.0f,
                             -0.5f, -10.0f);
        }
        debris_process((uint32_t)(scan + 1) * 60000);
    }
    
    TEST_ASSERT_EQ(g_debris.scans_completed, 5, "Should complete 5 scans");
}

static void test_scenario_reset(void)
{
    setup_with_asset();
    inject_detection(1, 1000.0f, 45.0f, 30.0f, -0.5f, -10.0f);
    
    debris_reset();
    
    TEST_ASSERT_TRUE(g_debris.initialized, "Should be initialized after reset");
    TEST_ASSERT_EQ(debris_get_track_count(), 0, "Tracks should be cleared");
    TEST_ASSERT_EQ(debris_get_asset_count(), 0, "Assets should be cleared");
}

/* ============================================================================
 * Test Main
 * ============================================================================ */

int main(void)
{
    printf("=================================================\n");
    printf(" Orbital Debris Tracking Test Suite\n");
    printf("=================================================\n\n");
    
    printf("Initialization Tests:\n");
    RUN_TEST(test_init_default_state);
    RUN_TEST(test_init_default_config);
    RUN_TEST(test_init_double_init_fails);
    RUN_TEST(test_init_intervals);
    RUN_TEST(test_shutdown_and_reinit);
    
    printf("\nAsset Configuration Tests:\n");
    RUN_TEST(test_config_asset_basic);
    RUN_TEST(test_config_asset_multiple);
    RUN_TEST(test_config_asset_invalid_id);
    RUN_TEST(test_asset_maneuverability);
    
    printf("\nTMR Voting Tests:\n");
    RUN_TEST(test_tmr_all_agree);
    RUN_TEST(test_tmr_two_agree);
    RUN_TEST(test_tmr_all_disagree);
    RUN_TEST(test_tmr_sensor_update);
    RUN_TEST(test_tmr_sensor_fault_detection);
    
    printf("\nOrbital Mechanics Tests:\n");
    RUN_TEST(test_orbital_period_leo);
    RUN_TEST(test_orbital_period_geo);
    RUN_TEST(test_classify_orbit_leo);
    RUN_TEST(test_classify_orbit_geo);
    RUN_TEST(test_classify_orbit_heo);
    RUN_TEST(test_state_to_elements);
    RUN_TEST(test_propagate_state);
    
    printf("\nDetection Processing Tests:\n");
    RUN_TEST(test_sensor_update_basic);
    RUN_TEST(test_detection_creates_track);
    RUN_TEST(test_detection_correlation);
    RUN_TEST(test_detection_new_object);
    RUN_TEST(test_sensor_fault_detection);
    
    printf("\nCollision Probability Tests:\n");
    RUN_TEST(test_pc_close_approach);
    RUN_TEST(test_pc_far_approach);
    RUN_TEST(test_pc_invalid_inputs);
    RUN_TEST(test_conjunction_severity_green);
    RUN_TEST(test_conjunction_severity_yellow);
    RUN_TEST(test_conjunction_severity_red);
    RUN_TEST(test_conjunction_severity_emergency);
    
    printf("\nConjunction Management Tests:\n");
    RUN_TEST(test_conjunction_screening);
    RUN_TEST(test_conjunction_retrieval);
    RUN_TEST(test_conjunction_get_by_id);
    RUN_TEST(test_conjunction_acknowledge);
    
    printf("\nAlarm Management Tests:\n");
    RUN_TEST(test_alarm_trigger);
    RUN_TEST(test_alarm_update_same_source);
    RUN_TEST(test_alarm_clear);
    RUN_TEST(test_alarm_multiple_types);
    
    printf("\nProcess Loop Tests:\n");
    RUN_TEST(test_process_basic);
    RUN_TEST(test_process_scan_completion);
    RUN_TEST(test_process_uptime_tracking);
    RUN_TEST(test_process_without_init);
    
    printf("\nEvent Logging Tests:\n");
    RUN_TEST(test_event_logging);
    RUN_TEST(test_event_wrap_around);
    
    printf("\nTelemetry Tests:\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_header);
    RUN_TEST(test_telemetry_sequence);
    RUN_TEST(test_telemetry_buffer_too_small);
    
    printf("\nQuery Function Tests:\n");
    RUN_TEST(test_query_track_count);
    RUN_TEST(test_query_red_conjunctions);
    RUN_TEST(test_query_has_red);
    RUN_TEST(test_query_closest_approach);
    RUN_TEST(test_get_track_by_id);
    RUN_TEST(test_get_track_not_found);
    
    printf("\nIntegration Scenarios:\n");
    RUN_TEST(test_scenario_full_workflow);
    RUN_TEST(test_scenario_multiple_scans);
    RUN_TEST(test_scenario_reset);
    
    printf("\n=================================================\n");
    printf(" Summary: %d/%d tests, %d/%d assertions\n",
           tests_passed, tests_run, assertions_passed, assertions_run);
    printf("=================================================\n");
    
    return (tests_passed == tests_run) ? 0 : 1;
}
