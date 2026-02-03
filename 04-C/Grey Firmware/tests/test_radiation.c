/**
 * @file test_radiation.c
 * @brief Unit tests for Radiation Shielding Control & Telemetry Spotlight
 * 
 * Tests cover:
 * - System initialization and configuration
 * - TMR sensor voting
 * - Radiation environment classification
 * - Shielding mode transitions
 * - Crew dose tracking
 * - Storm shelter activation
 * - Alarm generation
 * - Telemetry generation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* Include spotlight implementation */
#include "../src/radiation/radiation_spotlight.c"

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
    memset(&g_rad, 0, sizeof(g_rad));
    rad_init();
}

static void setup_basic_config(void)
{
    reset_system();
    
    /* Configure 4 sensors */
    rad_sensor_config(0, RAD_SENSOR_SILICON_DIODE, 1.0f, 0.0f);
    rad_sensor_config(1, RAD_SENSOR_SCINTILLATOR, 1.0f, 0.0f);
    rad_sensor_config(2, RAD_SENSOR_TEPC, 1.0f, 0.0f);
    rad_sensor_config(3, RAD_SENSOR_NEUTRON, 1.0f, 0.0f);
    
    /* Configure zones */
    rad_zone_config(0, SHIELD_ZONE_CREW_QUARTERS, SHIELD_TYPE_WATER_WALL, 10.0f, 25.0f);
    rad_zone_config(1, SHIELD_ZONE_COMMAND, SHIELD_TYPE_HYBRID, 8.0f, 20.0f);
    rad_zone_config(2, SHIELD_ZONE_STORM_SHELTER, SHIELD_TYPE_WATER_WALL, 20.0f, 40.0f);
    
    /* Register crew */
    rad_register_crew(0, "Commander", 1000.0f);
    rad_register_crew(1, "Pilot", 1000.0f);
    rad_register_crew(2, "Scientist", 600.0f);
}

static void simulate_nominal_readings(uint8_t sensor_id)
{
    float dose[3] = {40.0f, 40.0f, 40.0f};
    float flux[3] = {1000.0f, 1000.0f, 1000.0f};
    float let[3] = {5.0f, 5.0f, 5.0f};
    rad_update_sensors(sensor_id, dose, flux, let, g_rad.current_time_ms * 1000);
}

static void simulate_elevated_readings(uint8_t sensor_id)
{
    /* Target ~250 μSv/hr dose equivalent (between ELEVATED 200 and SPE_ONSET 500) */
    /* With LET=5.0 (Q~1.0), dose_equivalent ≈ dose_rate */
    float dose[3] = {250.0f, 250.0f, 250.0f};
    float flux[3] = {3000.0f, 3000.0f, 3000.0f};
    float let[3] = {5.0f, 5.0f, 5.0f};
    rad_update_sensors(sensor_id, dose, flux, let, g_rad.current_time_ms * 1000);
}

static void simulate_spe_readings(uint8_t sensor_id)
{
    float dose[3] = {3000.0f, 3000.0f, 3000.0f};
    float flux[3] = {50000.0f, 50000.0f, 50000.0f};
    float let[3] = {30.0f, 30.0f, 30.0f};
    rad_update_sensors(sensor_id, dose, flux, let, g_rad.current_time_ms * 1000);
}

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

static int test_init(void)
{
    reset_system();
    
    TEST_ASSERT(g_rad.initialized, "System should be initialized");
    TEST_ASSERT_EQ(g_rad.sensor_count, 0, "No sensors configured");
    TEST_ASSERT_EQ(g_rad.zone_count, 0, "No zones configured");
    TEST_ASSERT_EQ(g_rad.environment, RAD_ENV_NOMINAL, "Environment should be nominal");
    
    return 1;
}

static int test_double_init(void)
{
    reset_system();
    
    int result = rad_init();
    TEST_ASSERT_EQ(result, -1, "Double init should fail");
    
    return 1;
}

static int test_sensor_config(void)
{
    reset_system();
    
    int result = rad_sensor_config(0, RAD_SENSOR_SILICON_DIODE, 1.0f, 0.0f);
    TEST_ASSERT_EQ(result, 0, "Config should succeed");
    TEST_ASSERT_EQ(g_rad.sensor_count, 1, "Should have 1 sensor");
    TEST_ASSERT(g_rad.sensors[0].configured, "Sensor should be configured");
    TEST_ASSERT_EQ(g_rad.sensors[0].type, RAD_SENSOR_SILICON_DIODE, "Type should match");
    
    return 1;
}

static int test_zone_config(void)
{
    reset_system();
    
    int result = rad_zone_config(0, SHIELD_ZONE_STORM_SHELTER, SHIELD_TYPE_WATER_WALL,
                                  20.0f, 40.0f);
    TEST_ASSERT_EQ(result, 0, "Config should succeed");
    TEST_ASSERT_EQ(g_rad.zone_count, 1, "Should have 1 zone");
    TEST_ASSERT(g_rad.zones[0].configured, "Zone should be configured");
    TEST_ASSERT_FLOAT_EQ(g_rad.zones[0].nominal_density_gcm2, 20.0f, 0.1f, "Density should match");
    
    return 1;
}

static int test_crew_registration(void)
{
    reset_system();
    
    int result = rad_register_crew(0, "Commander", 1000.0f);
    TEST_ASSERT_EQ(result, 0, "Registration should succeed");
    TEST_ASSERT_EQ(g_rad.crew_count, 1, "Should have 1 crew");
    TEST_ASSERT(g_rad.crew[0].active, "Crew should be active");
    TEST_ASSERT_FLOAT_EQ(g_rad.crew[0].career_limit_msv, 1000.0f, 0.1f, "Limit should match");
    
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
    rad_sensor_config(0, RAD_SENSOR_SILICON_DIODE, 1.0f, 0.0f);
    
    float dose[3] = {50.0f, 50.0f, 50.0f};
    float flux[3] = {1000.0f, 1000.0f, 1000.0f};
    float let[3] = {5.0f, 5.0f, 5.0f};
    
    int result = rad_update_sensors(0, dose, flux, let, 1000);
    
    TEST_ASSERT_EQ(result, 0, "Update should succeed");
    TEST_ASSERT_EQ(g_rad.sensors[0].dose_rate.vote_status, TMR_AGREE_ALL, "Vote should agree");
    TEST_ASSERT_FLOAT_EQ(g_rad.sensors[0].dose_rate.voted_value, 50.0f, 0.1f, "Value should match");
    TEST_ASSERT_EQ(g_rad.sensors[0].dose_rate.health, SENSOR_HEALTH_OK, "Health should be OK");
    
    return 1;
}

static int test_tmr_sensor_degraded(void)
{
    reset_system();
    rad_sensor_config(0, RAD_SENSOR_SILICON_DIODE, 1.0f, 0.0f);
    
    /* One sensor off */
    float dose[3] = {50.0f, 50.0f, 100.0f};
    float flux[3] = {1000.0f, 1000.0f, 1000.0f};
    float let[3] = {5.0f, 5.0f, 5.0f};
    
    rad_update_sensors(0, dose, flux, let, 1000);
    
    TEST_ASSERT_EQ(g_rad.sensors[0].dose_rate.vote_status, TMR_AGREE_2OF3, "Should show 2-of-3");
    TEST_ASSERT_EQ(g_rad.sensors[0].dose_rate.health, SENSOR_HEALTH_DEGRADED, "Should be degraded");
    
    return 1;
}

/* ============================================================================
 * Environment Classification Tests
 * ============================================================================ */

static int test_environment_nominal(void)
{
    setup_basic_config();
    
    for (int i = 0; i < 4; i++) {
        simulate_nominal_readings(i);
    }
    
    rad_process(1000);
    
    TEST_ASSERT_EQ(g_rad.environment, RAD_ENV_NOMINAL, "Should be nominal");
    TEST_ASSERT(g_rad.ambient_dose_rate < DOSE_RATE_ELEVATED, "Dose rate should be low");
    
    return 1;
}

static int test_environment_elevated(void)
{
    setup_basic_config();
    
    for (int i = 0; i < 4; i++) {
        simulate_elevated_readings(i);
    }
    
    rad_process(1000);
    
    TEST_ASSERT_EQ(g_rad.environment, RAD_ENV_ELEVATED, "Should be elevated");
    
    return 1;
}

static int test_environment_spe_detection(void)
{
    setup_basic_config();
    
    /* Simulate SPE onset */
    for (int i = 0; i < 4; i++) {
        simulate_spe_readings(i);
    }
    
    rad_process(1000);
    
    TEST_ASSERT(g_rad.environment >= RAD_ENV_SPE_ONSET, "Should detect SPE");
    TEST_ASSERT(g_rad.storm_shelter_active, "Shelter should be active");
    
    return 1;
}

/* ============================================================================
 * Shielding Tests
 * ============================================================================ */

static int test_shield_mode_nominal(void)
{
    setup_basic_config();
    
    int result = rad_set_shield_mode(0, SHIELD_MODE_NOMINAL);
    
    TEST_ASSERT_EQ(result, 0, "Mode set should succeed");
    TEST_ASSERT_EQ(g_rad.zones[0].mode, SHIELD_MODE_NOMINAL, "Mode should be nominal");
    TEST_ASSERT_FLOAT_EQ(g_rad.zones[0].areal_density_gcm2, 10.0f, 0.1f, "Density should be nominal");
    
    return 1;
}

static int test_shield_mode_enhanced(void)
{
    setup_basic_config();
    
    int result = rad_set_shield_mode(0, SHIELD_MODE_ENHANCED);
    
    TEST_ASSERT_EQ(result, 0, "Mode set should succeed");
    TEST_ASSERT_EQ(g_rad.zones[0].mode, SHIELD_MODE_ENHANCED, "Mode should be enhanced");
    TEST_ASSERT(g_rad.zones[0].areal_density_gcm2 > 10.0f, "Density should increase");
    
    return 1;
}

static int test_shield_mode_storm_shelter(void)
{
    setup_basic_config();
    
    int result = rad_set_shield_mode(2, SHIELD_MODE_STORM_SHELTER);
    
    TEST_ASSERT_EQ(result, 0, "Mode set should succeed");
    TEST_ASSERT_EQ(g_rad.zones[2].mode, SHIELD_MODE_STORM_SHELTER, "Mode should be storm shelter");
    TEST_ASSERT_FLOAT_EQ(g_rad.zones[2].areal_density_gcm2, 40.0f, 0.1f, "Should be max density");
    TEST_ASSERT(g_rad.zones[2].water_pump_active, "Water pump should be active");
    
    return 1;
}

static int test_attenuation_calculation(void)
{
    float atten = calculate_attenuation(10.0f, RAD_PARTICLE_PROTON);
    
    TEST_ASSERT(atten > 0.0f && atten < 1.0f, "Attenuation should be between 0 and 1");
    TEST_ASSERT(atten < 0.9f, "10 g/cm2 should provide significant attenuation");
    
    return 1;
}

/* ============================================================================
 * Storm Shelter Tests
 * ============================================================================ */

static int test_shelter_activation(void)
{
    setup_basic_config();
    
    int result = rad_activate_storm_shelter();
    
    TEST_ASSERT_EQ(result, 0, "Activation should succeed");
    TEST_ASSERT(g_rad.storm_shelter_active, "Shelter should be active");
    TEST_ASSERT(g_rad.eva_suspended, "EVA should be suspended");
    TEST_ASSERT_EQ(g_rad.shelter_activations, 1, "Should count activation");
    
    return 1;
}

static int test_shelter_deactivation(void)
{
    setup_basic_config();
    rad_activate_storm_shelter();
    
    int result = rad_deactivate_storm_shelter();
    
    TEST_ASSERT_EQ(result, 0, "Deactivation should succeed");
    TEST_ASSERT(!g_rad.storm_shelter_active, "Shelter should be inactive");
    TEST_ASSERT(!g_rad.eva_suspended, "EVA should be allowed");
    
    return 1;
}

static int test_auto_shelter_on_spe(void)
{
    setup_basic_config();
    
    /* Simulate SPE */
    for (int i = 0; i < 4; i++) {
        simulate_spe_readings(i);
    }
    
    rad_process(1000);
    
    TEST_ASSERT(g_rad.storm_shelter_active, "Should auto-activate shelter");
    
    return 1;
}

static int test_shelter_response_time(void)
{
    setup_basic_config();
    
    rad_activate_storm_shelter();
    
    /* Response time should be tracked */
    TEST_ASSERT(g_rad.max_shelter_response_us > 0, "Response time should be recorded");
    
    return 1;
}

/* ============================================================================
 * Crew Dose Tests
 * ============================================================================ */

static int test_crew_dose_accumulation(void)
{
    setup_basic_config();
    
    g_rad.ambient_dose_rate = 100.0f; /* μSv/hr */
    
    int result = rad_update_crew(0, CREW_LOC_COMMAND, 10.0f); /* 10 μSv */
    
    TEST_ASSERT_EQ(result, 0, "Update should succeed");
    TEST_ASSERT(g_rad.crew[0].dose_today_usv > 0, "Daily dose should increase");
    TEST_ASSERT(g_rad.crew[0].dose_career_msv > 0, "Career dose should increase");
    
    return 1;
}

static int test_crew_location_tracking(void)
{
    setup_basic_config();
    
    rad_update_crew(0, CREW_LOC_EVA, 0.0f);
    
    TEST_ASSERT_EQ(g_rad.crew[0].location, CREW_LOC_EVA, "Location should update");
    
    return 1;
}

static int test_crew_dose_query(void)
{
    setup_basic_config();
    g_rad.crew[0].dose_career_msv = 100.0f;
    g_rad.crew[0].dose_rate_usv_hr = 50.0f;
    
    float career, rate;
    int result = rad_get_crew_dose(0, &career, &rate);
    
    TEST_ASSERT_EQ(result, 0, "Query should succeed");
    TEST_ASSERT_FLOAT_EQ(career, 100.0f, 0.1f, "Career dose should match");
    TEST_ASSERT_FLOAT_EQ(rate, 50.0f, 0.1f, "Rate should match");
    
    return 1;
}

/* ============================================================================
 * Alarm Tests
 * ============================================================================ */

static int test_dose_rate_alarm(void)
{
    setup_basic_config();
    
    trigger_alarm(ALARM_DOSE_RATE_HIGH, SEVERITY_WARNING, 0, 200.0f, 300.0f);
    
    TEST_ASSERT_EQ(g_rad.alarm_count, 1, "Should have 1 alarm");
    TEST_ASSERT_EQ(g_rad.unacked_alarms, 1, "Should have 1 unacked alarm");
    TEST_ASSERT_EQ(g_rad.alarms[0].type, ALARM_DOSE_RATE_HIGH, "Type should match");
    
    return 1;
}

static int test_alarm_acknowledgment(void)
{
    setup_basic_config();
    
    trigger_alarm(ALARM_DOSE_RATE_HIGH, SEVERITY_WARNING, 0, 200.0f, 300.0f);
    
    int result = rad_acknowledge_alarm(0);
    
    TEST_ASSERT_EQ(result, 0, "Ack should succeed");
    TEST_ASSERT_EQ(g_rad.unacked_alarms, 0, "No unacked alarms");
    TEST_ASSERT(g_rad.alarms[0].acknowledged, "Alarm should be acked");
    
    return 1;
}

static int test_alarm_clear(void)
{
    setup_basic_config();
    
    trigger_alarm(ALARM_DOSE_RATE_HIGH, SEVERITY_WARNING, 0, 200.0f, 300.0f);
    clear_alarm(ALARM_DOSE_RATE_HIGH, 0);
    
    TEST_ASSERT_EQ(g_rad.alarm_count, 0, "No alarms after clear");
    
    return 1;
}

static int test_no_duplicate_alarm(void)
{
    setup_basic_config();
    
    trigger_alarm(ALARM_DOSE_RATE_HIGH, SEVERITY_WARNING, 0, 200.0f, 300.0f);
    trigger_alarm(ALARM_DOSE_RATE_HIGH, SEVERITY_WARNING, 0, 200.0f, 350.0f);
    
    TEST_ASSERT_EQ(g_rad.alarm_count, 1, "Should not duplicate alarm");
    TEST_ASSERT_FLOAT_EQ(g_rad.alarms[0].actual_value, 350.0f, 0.1f, "Should update value");
    
    return 1;
}

/* ============================================================================
 * Telemetry Tests
 * ============================================================================ */

static int test_telemetry_generation(void)
{
    setup_basic_config();
    
    uint8_t buffer[256];
    int len = rad_get_telemetry(buffer, sizeof(buffer));
    
    TEST_ASSERT(len > 0, "Telemetry should be generated");
    TEST_ASSERT(len >= (int)sizeof(telemetry_header_t), "Should include header");
    
    telemetry_header_t *hdr = (telemetry_header_t *)buffer;
    TEST_ASSERT_EQ(hdr->sync_word, 0x1ACF, "Sync word should match");
    TEST_ASSERT_EQ(hdr->apid, 0x0200, "APID should match");
    
    return 1;
}

static int test_telemetry_sequence(void)
{
    setup_basic_config();
    
    uint8_t buffer[256];
    
    rad_get_telemetry(buffer, sizeof(buffer));
    telemetry_header_t *hdr1 = (telemetry_header_t *)buffer;
    uint16_t seq1 = hdr1->sequence_count;
    
    rad_get_telemetry(buffer, sizeof(buffer));
    telemetry_header_t *hdr2 = (telemetry_header_t *)buffer;
    uint16_t seq2 = hdr2->sequence_count;
    
    TEST_ASSERT_EQ(seq2, seq1 + 1, "Sequence should increment");
    
    return 1;
}

static int test_telemetry_buffer_too_small(void)
{
    setup_basic_config();
    
    uint8_t buffer[4]; /* Too small */
    int len = rad_get_telemetry(buffer, sizeof(buffer));
    
    TEST_ASSERT_EQ(len, -1, "Should fail with small buffer");
    
    return 1;
}

/* ============================================================================
 * Query Function Tests
 * ============================================================================ */

static int test_get_environment(void)
{
    setup_basic_config();
    g_rad.environment = RAD_ENV_ELEVATED;
    
    rad_environment_t env = rad_get_environment();
    TEST_ASSERT_EQ(env, RAD_ENV_ELEVATED, "Should return correct environment");
    
    return 1;
}

static int test_get_dose_rate(void)
{
    setup_basic_config();
    g_rad.ambient_dose_rate = 123.45f;
    
    float rate = rad_get_dose_rate();
    TEST_ASSERT_FLOAT_EQ(rate, 123.45f, 0.01f, "Should return correct rate");
    
    return 1;
}

static int test_is_eva_safe(void)
{
    setup_basic_config();
    g_rad.environment = RAD_ENV_NOMINAL;
    g_rad.ambient_dose_rate = 40.0f;
    g_rad.eva_suspended = false;
    
    bool safe = rad_is_eva_safe();
    TEST_ASSERT(safe, "EVA should be safe");
    
    g_rad.eva_suspended = true;
    safe = rad_is_eva_safe();
    TEST_ASSERT(!safe, "EVA should not be safe when suspended");
    
    return 1;
}

static int test_zone_status_query(void)
{
    setup_basic_config();
    g_rad.zones[0].areal_density_gcm2 = 15.0f;
    g_rad.zones[0].attenuation_factor = 0.7f;
    
    float density, atten;
    int result = rad_get_zone_status(0, &density, &atten);
    
    TEST_ASSERT_EQ(result, 0, "Query should succeed");
    TEST_ASSERT_FLOAT_EQ(density, 15.0f, 0.1f, "Density should match");
    TEST_ASSERT_FLOAT_EQ(atten, 0.7f, 0.01f, "Attenuation should match");
    
    return 1;
}

/* ============================================================================
 * Process Loop Tests
 * ============================================================================ */

static int test_process_not_initialized(void)
{
    memset(&g_rad, 0, sizeof(g_rad));
    
    int result = rad_process(1000);
    TEST_ASSERT_EQ(result, -1, "Should fail when not initialized");
    
    return 1;
}

static int test_process_normal(void)
{
    setup_basic_config();
    
    for (int i = 0; i < 4; i++) {
        simulate_nominal_readings(i);
    }
    
    int result = rad_process(1000);
    TEST_ASSERT_EQ(result, 0, "Process should succeed");
    TEST_ASSERT_EQ(g_rad.uptime_seconds, 1, "Uptime should update");
    
    return 1;
}

static int test_daily_reset(void)
{
    setup_basic_config();
    g_rad.crew[0].dose_today_usv = 100.0f;
    g_rad.last_process_ms = 86399000;
    
    /* Process at day boundary */
    rad_process(86400000);
    
    TEST_ASSERT_FLOAT_EQ(g_rad.crew[0].dose_today_usv, 0.0f, 0.01f, "Daily dose should reset");
    
    return 1;
}

/* ============================================================================
 * Sensor Fault Tests
 * ============================================================================ */

static int test_sensor_fault_detection(void)
{
    setup_basic_config();
    
    /* All sensors disagree */
    float dose[3] = {50.0f, 150.0f, 250.0f};
    float flux[3] = {1000.0f, 1000.0f, 1000.0f};
    float let[3] = {5.0f, 5.0f, 5.0f};
    rad_update_sensors(0, dose, flux, let, 1000);
    
    bool has_fault = rad_has_sensor_fault();
    TEST_ASSERT(has_fault, "Should detect sensor fault");
    
    return 1;
}

static int test_sensor_tmr_status(void)
{
    setup_basic_config();
    simulate_nominal_readings(0);
    
    tmr_result_t status = rad_get_sensor_tmr_status(0);
    TEST_ASSERT_EQ(status, TMR_AGREE_ALL, "TMR should agree");
    
    return 1;
}

/* ============================================================================
 * Reset and Shutdown Tests
 * ============================================================================ */

static int test_reset(void)
{
    setup_basic_config();
    g_rad.shelter_activations = 5;
    
    int result = rad_reset();
    
    TEST_ASSERT_EQ(result, 0, "Reset should succeed");
    TEST_ASSERT(g_rad.initialized, "Should be reinitialized");
    TEST_ASSERT_EQ(g_rad.sensor_count, 0, "Sensors should be cleared");
    TEST_ASSERT_EQ(g_rad.shelter_activations, 0, "Stats should be reset");
    
    return 1;
}

static int test_shutdown(void)
{
    setup_basic_config();
    rad_activate_storm_shelter();
    
    rad_shutdown();
    
    TEST_ASSERT(!g_rad.initialized, "Should not be initialized");
    TEST_ASSERT(!g_rad.storm_shelter_active, "Shelter should be deactivated");
    
    return 1;
}

/* ============================================================================
 * Integration Tests
 * ============================================================================ */

static int test_spe_response_sequence(void)
{
    setup_basic_config();
    
    /* Phase 1: Nominal conditions */
    for (int i = 0; i < 4; i++) {
        simulate_nominal_readings(i);
    }
    rad_process(1000);
    TEST_ASSERT_EQ(g_rad.environment, RAD_ENV_NOMINAL, "Should start nominal");
    TEST_ASSERT(!g_rad.storm_shelter_active, "Shelter should be inactive");
    
    /* Phase 2: SPE onset */
    for (int i = 0; i < 4; i++) {
        simulate_spe_readings(i);
    }
    rad_process(2000);
    TEST_ASSERT(g_rad.environment >= RAD_ENV_SPE_ONSET, "Should detect SPE");
    TEST_ASSERT(g_rad.storm_shelter_active, "Shelter should activate");
    TEST_ASSERT(g_rad.eva_suspended, "EVA should be suspended");
    TEST_ASSERT(g_rad.alarm_count > 0, "Should have alarms");
    
    /* Phase 3: SPE decay - back to nominal */
    for (int i = 0; i < 4; i++) {
        simulate_nominal_readings(i);
    }
    g_rad.environment = RAD_ENV_NOMINAL; /* Force environment for test */
    rad_process(3000);
    
    /* Should eventually deactivate shelter */
    TEST_ASSERT(!g_rad.storm_shelter_active, "Shelter should deactivate after SPE");
    
    return 1;
}

static int test_crew_protection_during_spe(void)
{
    setup_basic_config();
    
    /* Initial crew location */
    rad_update_crew(0, CREW_LOC_COMMAND, 0.0f);
    
    /* Simulate SPE */
    for (int i = 0; i < 4; i++) {
        simulate_spe_readings(i);
    }
    rad_process(1000);
    
    /* Shelter should be active */
    TEST_ASSERT(g_rad.storm_shelter_active, "Shelter should be active");
    
    /* All zones should be at max protection */
    for (uint8_t i = 0; i < g_rad.zone_count; i++) {
        TEST_ASSERT_EQ(g_rad.zones[i].mode, SHIELD_MODE_STORM_SHELTER, 
                       "All zones should be in storm shelter mode");
    }
    
    return 1;
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(void)
{
    printf("=== Radiation Shielding Control & Telemetry Tests ===\n\n");
    
    /* Initialization tests */
    RUN_TEST(test_init);
    RUN_TEST(test_double_init);
    RUN_TEST(test_sensor_config);
    RUN_TEST(test_zone_config);
    RUN_TEST(test_crew_registration);
    
    /* TMR voting tests */
    RUN_TEST(test_tmr_all_agree);
    RUN_TEST(test_tmr_two_of_three);
    RUN_TEST(test_tmr_all_disagree);
    RUN_TEST(test_tmr_sensor_update);
    RUN_TEST(test_tmr_sensor_degraded);
    
    /* Environment tests */
    RUN_TEST(test_environment_nominal);
    RUN_TEST(test_environment_elevated);
    RUN_TEST(test_environment_spe_detection);
    
    /* Shielding tests */
    RUN_TEST(test_shield_mode_nominal);
    RUN_TEST(test_shield_mode_enhanced);
    RUN_TEST(test_shield_mode_storm_shelter);
    RUN_TEST(test_attenuation_calculation);
    
    /* Storm shelter tests */
    RUN_TEST(test_shelter_activation);
    RUN_TEST(test_shelter_deactivation);
    RUN_TEST(test_auto_shelter_on_spe);
    RUN_TEST(test_shelter_response_time);
    
    /* Crew dose tests */
    RUN_TEST(test_crew_dose_accumulation);
    RUN_TEST(test_crew_location_tracking);
    RUN_TEST(test_crew_dose_query);
    
    /* Alarm tests */
    RUN_TEST(test_dose_rate_alarm);
    RUN_TEST(test_alarm_acknowledgment);
    RUN_TEST(test_alarm_clear);
    RUN_TEST(test_no_duplicate_alarm);
    
    /* Telemetry tests */
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_sequence);
    RUN_TEST(test_telemetry_buffer_too_small);
    
    /* Query tests */
    RUN_TEST(test_get_environment);
    RUN_TEST(test_get_dose_rate);
    RUN_TEST(test_is_eva_safe);
    RUN_TEST(test_zone_status_query);
    
    /* Process tests */
    RUN_TEST(test_process_not_initialized);
    RUN_TEST(test_process_normal);
    RUN_TEST(test_daily_reset);
    
    /* Sensor fault tests */
    RUN_TEST(test_sensor_fault_detection);
    RUN_TEST(test_sensor_tmr_status);
    
    /* Reset/shutdown tests */
    RUN_TEST(test_reset);
    RUN_TEST(test_shutdown);
    
    /* Integration tests */
    RUN_TEST(test_spe_response_sequence);
    RUN_TEST(test_crew_protection_during_spe);
    
    printf("\n=== Results: %d/%d tests passed ===\n", tests_passed, tests_run);
    
    return (tests_passed == tests_run) ? 0 : 1;
}
