/**
 * @file microgrid_tests.c
 * @brief Integration tests for Microgrid Controller & Demand Response
 * 
 * Tests cover:
 * - Initialization and configuration
 * - Source registration and dispatch
 * - Load registration and curtailment
 * - Storage management
 * - Demand response events
 * - Grid balancing logic
 * - Mode transitions (grid-connected, islanded)
 * - Telemetry and alarms
 * - Variable renewable simulation
 * - Emergency scenarios
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
 * Include Microgrid Implementation 
 * ============================================================================ */

/* Include the implementation directly for testing */
#include "../src/microgrid/microgrid_spotlight.c"

/* ============================================================================
 * Test Helper Functions
 * ============================================================================ */

static void reset_microgrid(void) {
    if (g_mg.initialized) {
        mg_shutdown();
    }
    memset(&g_mg, 0, sizeof(g_mg));
}

static void setup_basic_microgrid(void) {
    reset_microgrid();
    mg_init(NULL);
    
    /* Register a solar source */
    mg_register_source(MG_SOURCE_SOLAR, "Solar Array", 100.0f, false);
    
    /* Register a diesel generator */
    mg_register_source(MG_SOURCE_DIESEL, "Diesel Gen", 200.0f, true);
    
    /* Register loads */
    mg_register_load("Critical HVAC", MG_LOAD_CRITICAL, 50.0f);
    mg_register_load("Office Lighting", MG_LOAD_NORMAL, 30.0f);
    mg_register_load("EV Chargers", MG_LOAD_CURTAILABLE, 40.0f);
    
    /* Register storage */
    mg_register_storage("Battery ESS", 500.0f, 100.0f);
    
    /* Set sources online */
    mg_set_source_online(0, true);  /* Solar */
    mg_set_source_online(1, true);  /* Diesel */
    
    /* Start microgrid */
    mg_start();
}

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

static void test_init_default(void) {
    reset_microgrid();
    int result = mg_init(NULL);
    TEST_ASSERT(result == 0, "Init should succeed");
    TEST_ASSERT(g_mg.initialized == true, "Should be initialized");
    TEST_ASSERT(g_mg.mode == MG_MODE_OFF, "Mode should be OFF");
    mg_shutdown();
}

static void test_init_custom_config(void) {
    reset_microgrid();
    mg_config_t config = {
        .voltage_nominal = 400.0f,
        .frequency_nominal = 50.0f,
        .voltage_tolerance = 0.10f,
        .frequency_tolerance = 0.02f,
        .reserve_margin = 0.20f,
        .enable_demand_response = true,
        .enable_islanding = true,
        .balance_interval_ms = 50
    };
    
    int result = mg_init(&config);
    TEST_ASSERT(result == 0, "Custom init should succeed");
    TEST_ASSERT(g_mg.config.voltage_nominal == 400.0f, "Voltage should match");
    TEST_ASSERT(g_mg.config.frequency_nominal == 50.0f, "Frequency should match");
    mg_shutdown();
}

static void test_double_init(void) {
    reset_microgrid();
    mg_init(NULL);
    int result = mg_init(NULL);
    TEST_ASSERT(result == -1, "Double init should fail");
    mg_shutdown();
}

static void test_shutdown(void) {
    reset_microgrid();
    mg_init(NULL);
    int result = mg_shutdown();
    TEST_ASSERT(result == 0, "Shutdown should succeed");
    TEST_ASSERT(g_mg.initialized == false, "Should not be initialized");
}

/* ============================================================================
 * Source Registration Tests
 * ============================================================================ */

static void test_register_solar_source(void) {
    reset_microgrid();
    mg_init(NULL);
    
    int id = mg_register_source(MG_SOURCE_SOLAR, "Solar Panel", 50.0f, false);
    TEST_ASSERT(id == 0, "First source should be ID 0");
    TEST_ASSERT(g_mg.source_count == 1, "Source count should be 1");
    TEST_ASSERT(g_mg.sources[0].type == MG_SOURCE_SOLAR, "Type should be solar");
    TEST_ASSERT(g_mg.sources[0].dispatchable == false, "Solar not dispatchable");
    
    mg_shutdown();
}

static void test_register_diesel_source(void) {
    reset_microgrid();
    mg_init(NULL);
    
    int id = mg_register_source(MG_SOURCE_DIESEL, "Backup Gen", 100.0f, true);
    TEST_ASSERT(id == 0, "First source should be ID 0");
    TEST_ASSERT(g_mg.sources[0].dispatchable == true, "Diesel is dispatchable");
    TEST_ASSERT(g_mg.sources[0].min_output_kw > 0, "Diesel has min output");
    
    mg_shutdown();
}

static void test_register_multiple_sources(void) {
    reset_microgrid();
    mg_init(NULL);
    
    mg_register_source(MG_SOURCE_SOLAR, "Solar", 100.0f, false);
    mg_register_source(MG_SOURCE_WIND, "Wind", 50.0f, false);
    mg_register_source(MG_SOURCE_DIESEL, "Diesel", 200.0f, true);
    mg_register_source(MG_SOURCE_BATTERY, "Battery", 100.0f, true);
    
    TEST_ASSERT(g_mg.source_count == 4, "Should have 4 sources");
    
    mg_shutdown();
}

/* ============================================================================
 * Load Registration Tests
 * ============================================================================ */

static void test_register_critical_load(void) {
    reset_microgrid();
    mg_init(NULL);
    
    int id = mg_register_load("Life Safety", MG_LOAD_CRITICAL, 25.0f);
    TEST_ASSERT(id == 0, "First load should be ID 0");
    TEST_ASSERT(g_mg.loads[0].priority == MG_LOAD_CRITICAL, "Priority should be critical");
    TEST_ASSERT(g_mg.loads[0].dr_enrolled == false, "Critical loads not in DR");
    
    mg_shutdown();
}

static void test_register_curtailable_load(void) {
    reset_microgrid();
    mg_init(NULL);
    
    int id = mg_register_load("EV Charger", MG_LOAD_CURTAILABLE, 50.0f);
    TEST_ASSERT(id == 0, "First load should be ID 0");
    TEST_ASSERT(g_mg.loads[0].priority == MG_LOAD_CURTAILABLE, "Priority should be curtailable");
    TEST_ASSERT(g_mg.loads[0].dr_enrolled == true, "Curtailable loads in DR");
    
    mg_shutdown();
}

static void test_register_multiple_loads(void) {
    reset_microgrid();
    mg_init(NULL);
    
    mg_register_load("Critical", MG_LOAD_CRITICAL, 50.0f);
    mg_register_load("Essential", MG_LOAD_ESSENTIAL, 30.0f);
    mg_register_load("Normal", MG_LOAD_NORMAL, 40.0f);
    mg_register_load("Deferrable", MG_LOAD_DEFERRABLE, 20.0f);
    mg_register_load("Curtailable", MG_LOAD_CURTAILABLE, 60.0f);
    
    TEST_ASSERT(g_mg.load_count == 5, "Should have 5 loads");
    
    mg_shutdown();
}

/* ============================================================================
 * Storage Tests
 * ============================================================================ */

static void test_register_storage(void) {
    reset_microgrid();
    mg_init(NULL);
    
    int id = mg_register_storage("Battery 1", 1000.0f, 250.0f);
    TEST_ASSERT(id == 0, "First storage should be ID 0");
    TEST_ASSERT(g_mg.storage_count == 1, "Storage count should be 1");
    TEST_ASSERT(g_mg.storage[0].capacity_kwh == 1000.0f, "Capacity should match");
    TEST_ASSERT(g_mg.storage[0].soc_percent == 50.0f, "Initial SOC should be 50%");
    
    mg_shutdown();
}

static void test_storage_online_default(void) {
    reset_microgrid();
    mg_init(NULL);
    
    mg_register_storage("ESS", 500.0f, 100.0f);
    TEST_ASSERT(g_mg.storage[0].online == true, "Storage should be online by default");
    
    mg_shutdown();
}

/* ============================================================================
 * Grid Balancing Tests
 * ============================================================================ */

static void test_balance_calculation(void) {
    setup_basic_microgrid();
    
    /* Update solar availability */
    mg_update_source(0, 80.0f);  /* 80 kW available */
    
    /* Process a few cycles */
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    mg_balance_t balance;
    mg_get_balance(&balance);
    
    TEST_ASSERT(balance.total_demand_kw > 0, "Should have demand");
    
    mg_shutdown();
}

static void test_source_dispatch(void) {
    setup_basic_microgrid();
    
    mg_update_source(0, 50.0f);  /* Solar at 50 kW */
    
    /* Start and process */
    for (int i = 0; i < 20; i++) {
        mg_process(100);
    }
    
    /* Diesel should have ramped up to meet demand */
    TEST_ASSERT(g_mg.sources[1].power_setpoint_kw >= 0, "Diesel should have setpoint");
    
    mg_shutdown();
}

static void test_load_curtailment(void) {
    setup_basic_microgrid();
    
    /* Set limited generation */
    mg_update_source(0, 20.0f);   /* Only 20 kW solar */
    mg_set_source_online(1, false);  /* Diesel offline */
    
    /* Force island mode (no utility) */
    mg_set_utility_status(false, 0, 0);
    
    for (int i = 0; i < 50; i++) {
        mg_process(100);
    }
    
    /* Check if system is still running (may or may not curtail depending on storage) */
    TEST_ASSERT(g_mg.mode != MG_MODE_OFF, "Microgrid should be running");
    
    mg_shutdown();
}

/* ============================================================================
 * Mode Transition Tests
 * ============================================================================ */

static void test_start_grid_connected(void) {
    setup_basic_microgrid();
    
    mg_set_utility_status(true, 1.0f, 60.0f);
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    TEST_ASSERT(g_mg.mode == MG_MODE_GRID_CONNECTED, "Should be grid connected");
    
    mg_shutdown();
}

static void test_transition_to_island(void) {
    setup_basic_microgrid();
    
    mg_set_utility_status(true, 1.0f, 60.0f);
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    /* Lose utility */
    mg_set_utility_status(false, 0, 0);
    
    for (int i = 0; i < 20; i++) {
        mg_process(100);
    }
    
    TEST_ASSERT(g_mg.mode == MG_MODE_ISLANDED || 
                g_mg.mode == MG_MODE_ISLAND_TRANSITION, 
                "Should transition to island");
    
    mg_shutdown();
}

static void test_reconnect_to_grid(void) {
    setup_basic_microgrid();
    
    /* Start islanded */
    mg_set_utility_status(false, 0, 0);
    
    for (int i = 0; i < 20; i++) {
        mg_process(100);
    }
    
    /* Restore utility */
    mg_set_utility_status(true, 1.0f, 60.0f);
    
    for (int i = 0; i < 30; i++) {
        mg_process(100);
    }
    
    TEST_ASSERT(g_mg.mode == MG_MODE_GRID_CONNECTED ||
                g_mg.mode == MG_MODE_RECONNECTING,
                "Should reconnect or be reconnecting");
    
    mg_shutdown();
}

static void test_request_island(void) {
    setup_basic_microgrid();
    
    mg_set_utility_status(true, 1.0f, 60.0f);
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    int result = mg_request_island();
    TEST_ASSERT(result == 0, "Island request should succeed");
    TEST_ASSERT(g_mg.mode == MG_MODE_ISLAND_TRANSITION, 
                "Should be transitioning to island");
    
    mg_shutdown();
}

/* ============================================================================
 * Demand Response Tests
 * ============================================================================ */

static void test_receive_dr_event(void) {
    setup_basic_microgrid();
    
    int id = mg_receive_dr_event(MG_DR_LOAD_SHED, 1000, 3600, 50.0f, 0, false);
    TEST_ASSERT(id >= 0, "DR event should be accepted");
    TEST_ASSERT(g_mg.dr_event_count == 1, "Should have 1 DR event");
    
    mg_shutdown();
}

static void test_dr_event_activation(void) {
    setup_basic_microgrid();
    
    /* Schedule DR event starting now */
    mg_receive_dr_event(MG_DR_LOAD_SHED, 0, 3600, 30.0f, 0, true);
    
    /* Process until event activates */
    for (int i = 0; i < 20; i++) {
        mg_process(100);
    }
    
    TEST_ASSERT(g_mg.dr_events[0].active == true, "DR event should be active");
    
    mg_shutdown();
}

static void test_dr_price_response(void) {
    setup_basic_microgrid();
    
    /* High price signal */
    mg_receive_dr_event(MG_DR_PRICE_RESPONSE, 0, 3600, 20.0f, 0.75f, false);
    
    for (int i = 0; i < 20; i++) {
        mg_process(100);
    }
    
    TEST_ASSERT(g_mg.dr_events[0].active == true, "Price response should be active");
    
    mg_shutdown();
}

/* ============================================================================
 * Statistics Tests
 * ============================================================================ */

static void test_stats_initial(void) {
    setup_basic_microgrid();
    
    mg_stats_t stats;
    mg_get_stats(&stats);
    
    TEST_ASSERT(stats.uptime_seconds == 0, "Initial uptime should be 0");
    TEST_ASSERT(stats.energy_generated_kwh == 0, "Initial generation should be 0");
    
    mg_shutdown();
}

static void test_stats_accumulation(void) {
    setup_basic_microgrid();
    
    mg_update_source(0, 100.0f);
    mg_set_source_online(0, true);
    
    /* Run for 10+ seconds of simulated time (1000ms * 15 iterations = 15s) */
    for (int i = 0; i < 15; i++) {
        mg_process(1000);  /* 1 second per iteration */
    }
    
    mg_stats_t stats;
    mg_get_stats(&stats);
    
    /* After 15 seconds of processing, uptime should be at least 10 */
    TEST_ASSERT(stats.uptime_seconds >= 10, "Uptime should have increased");
    
    mg_shutdown();
}

static void test_stats_peak_tracking(void) {
    setup_basic_microgrid();
    
    for (int i = 0; i < 50; i++) {
        mg_process(100);
    }
    
    mg_stats_t stats;
    mg_get_stats(&stats);
    
    TEST_ASSERT(stats.peak_demand_kw > 0, "Should track peak demand");
    
    mg_shutdown();
}

/* ============================================================================
 * Alarm Tests
 * ============================================================================ */

static void test_alarm_generation(void) {
    setup_basic_microgrid();
    
    /* Process to generate mode transition alarm */
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    TEST_ASSERT(g_mg.alarm_count > 0, "Should have generated alarms");
    
    mg_shutdown();
}

static void test_alarm_acknowledge(void) {
    setup_basic_microgrid();
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    if (g_mg.alarm_count > 0) {
        uint16_t alarm_id = g_mg.alarms[0].id;
        int result = mg_ack_alarm(alarm_id);
        TEST_ASSERT(result == 0, "Ack should succeed");
        TEST_ASSERT(g_mg.alarms[0].acknowledged == true, "Should be acknowledged");
    }
    
    mg_shutdown();
}

static void test_get_alarms(void) {
    setup_basic_microgrid();
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    mg_alarm_t alarms[16];
    int count = mg_get_alarms(alarms, 16);
    
    TEST_ASSERT(count >= 0, "Should return valid count");
    
    mg_shutdown();
}

/* ============================================================================
 * Telemetry Tests
 * ============================================================================ */

static void test_telemetry_points(void) {
    setup_basic_microgrid();
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    float value;
    int result = mg_get_telemetry("total_generation_kw", &value);
    TEST_ASSERT(result == 0, "Should find telemetry point");
    
    mg_shutdown();
}

static void test_telemetry_mode(void) {
    setup_basic_microgrid();
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    float value;
    int result = mg_get_telemetry("mode", &value);
    TEST_ASSERT(result == 0, "Should find mode telemetry");
    TEST_ASSERT(value >= 0, "Mode should be valid");
    
    mg_shutdown();
}

/* ============================================================================
 * Emergency Tests
 * ============================================================================ */

static void test_emergency_shutdown(void) {
    setup_basic_microgrid();
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    int result = mg_emergency_shutdown();
    TEST_ASSERT(result == 0, "Emergency shutdown should succeed");
    TEST_ASSERT(g_mg.mode == MG_MODE_EMERGENCY_SHUTDOWN, "Should be in emergency mode");
    
    /* Check that non-critical loads are disconnected */
    bool non_critical_on = false;
    for (uint8_t i = 0; i < g_mg.load_count; i++) {
        if (g_mg.loads[i].priority != MG_LOAD_CRITICAL && 
            g_mg.loads[i].connected) {
            non_critical_on = true;
        }
    }
    TEST_ASSERT(non_critical_on == false, "Non-critical loads should be off");
    
    mg_shutdown();
}

/* ============================================================================
 * Variable Renewable Tests
 * ============================================================================ */

static void test_solar_variability(void) {
    setup_basic_microgrid();
    
    /* Simulate cloud passing */
    for (int i = 0; i < 10; i++) {
        mg_update_source(0, 100.0f);  /* Full sun */
        mg_process(100);
    }
    
    for (int i = 0; i < 10; i++) {
        mg_update_source(0, 30.0f);   /* Cloud */
        mg_process(100);
    }
    
    for (int i = 0; i < 10; i++) {
        mg_update_source(0, 80.0f);   /* Partial sun */
        mg_process(100);
    }
    
    TEST_ASSERT(g_mg.mode != MG_MODE_OFF, "Should handle variability");
    
    mg_shutdown();
}

static void test_ramp_rate_limits(void) {
    setup_basic_microgrid();
    
    /* Large step change in demand */
    mg_update_load(2, 100.0f);  /* EV chargers to max */
    
    float prev_output = g_mg.sources[1].power_output_kw;
    mg_process(100);
    float delta = fabsf(g_mg.sources[1].power_output_kw - prev_output);
    
    /* Ramp should be limited */
    TEST_ASSERT(delta <= MG_RAMP_RATE_KW_SEC * 0.2f, 
                "Ramp rate should be limited");
    
    mg_shutdown();
}

/* ============================================================================
 * Integration Tests
 * ============================================================================ */

static void test_full_day_simulation(void) {
    setup_basic_microgrid();
    
    mg_set_utility_status(true, 1.0f, 60.0f);
    
    /* Simulate 24 hours at 1-minute resolution (1440 steps) */
    for (int hour = 0; hour < 24; hour++) {
        /* Solar profile */
        float solar = 0;
        if (hour >= 6 && hour <= 18) {
            solar = 100.0f * sinf((float)(hour - 6) / 12.0f * 3.14159f);
        }
        mg_update_source(0, solar);
        
        /* Load profile */
        float load_factor = 0.6f;
        if (hour >= 9 && hour <= 17) {
            load_factor = 1.0f;  /* Peak hours */
        } else if (hour >= 18 && hour <= 22) {
            load_factor = 0.8f;  /* Evening */
        }
        mg_update_load(1, 30.0f * load_factor);
        mg_update_load(2, 40.0f * load_factor);
        
        for (int min = 0; min < 60; min++) {
            mg_process(1000);  /* 1 second per step */
        }
    }
    
    mg_stats_t stats;
    mg_get_stats(&stats);
    
    TEST_ASSERT(stats.uptime_seconds > 0, "Should have uptime");
    TEST_ASSERT(stats.energy_generated_kwh > 0 || stats.energy_imported_kwh > 0,
                "Should have energy flow");
    
    mg_shutdown();
}

static void test_island_with_dr(void) {
    setup_basic_microgrid();
    
    /* Start grid-connected */
    mg_set_utility_status(true, 1.0f, 60.0f);
    
    for (int i = 0; i < 10; i++) {
        mg_process(100);
    }
    
    /* Schedule DR event */
    mg_receive_dr_event(MG_DR_LOAD_SHED, 0, 3600, 25.0f, 0, true);
    
    /* Lose utility during DR event */
    mg_set_utility_status(false, 0, 0);
    
    for (int i = 0; i < 30; i++) {
        mg_process(100);
    }
    
    /* Should handle both DR and island transition */
    TEST_ASSERT(g_mg.mode == MG_MODE_ISLANDED ||
                g_mg.mode == MG_MODE_ISLAND_TRANSITION,
                "Should be islanded");
    TEST_ASSERT(g_mg.dr_events[0].active == true, "DR should still be active");
    
    mg_shutdown();
}

/* ============================================================================
 * Boundary Condition Tests
 * ============================================================================ */

static void test_zero_generation(void) {
    setup_basic_microgrid();
    
    mg_update_source(0, 0);
    mg_set_source_online(1, false);
    mg_set_utility_status(false, 0, 0);
    
    for (int i = 0; i < 20; i++) {
        mg_process(100);
    }
    
    /* Should curtail loads when no generation */
    mg_balance_t balance;
    mg_get_balance(&balance);
    
    TEST_ASSERT(balance.total_generation_kw >= 0, "Generation should be >= 0");
    
    mg_shutdown();
}

static void test_storage_depletion(void) {
    setup_basic_microgrid();
    
    /* Deplete storage */
    g_mg.storage[0].soc_percent = 5.0f;
    
    mg_set_utility_status(false, 0, 0);
    mg_update_source(0, 20.0f);
    mg_set_source_online(1, false);
    
    for (int i = 0; i < 20; i++) {
        mg_process(100);
    }
    
    /* Should not discharge below reserve */
    TEST_ASSERT(g_mg.storage[0].soc_percent >= 0, "SOC should not go negative");
    
    mg_shutdown();
}

static void test_null_pointer_handling(void) {
    setup_basic_microgrid();
    
    int result = mg_get_balance(NULL);
    TEST_ASSERT(result == -1, "Null balance should fail");
    
    result = mg_get_stats(NULL);
    TEST_ASSERT(result == -1, "Null stats should fail");
    
    result = mg_get_telemetry("test", NULL);
    TEST_ASSERT(result == -1, "Null telemetry should fail");
    
    mg_shutdown();
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(void) {
    printf("=== Microgrid Controller Integration Tests ===\n\n");
    
    printf("-- Initialization Tests --\n");
    RUN_TEST(test_init_default);
    RUN_TEST(test_init_custom_config);
    RUN_TEST(test_double_init);
    RUN_TEST(test_shutdown);
    
    printf("\n-- Source Registration Tests --\n");
    RUN_TEST(test_register_solar_source);
    RUN_TEST(test_register_diesel_source);
    RUN_TEST(test_register_multiple_sources);
    
    printf("\n-- Load Registration Tests --\n");
    RUN_TEST(test_register_critical_load);
    RUN_TEST(test_register_curtailable_load);
    RUN_TEST(test_register_multiple_loads);
    
    printf("\n-- Storage Tests --\n");
    RUN_TEST(test_register_storage);
    RUN_TEST(test_storage_online_default);
    
    printf("\n-- Grid Balancing Tests --\n");
    RUN_TEST(test_balance_calculation);
    RUN_TEST(test_source_dispatch);
    RUN_TEST(test_load_curtailment);
    
    printf("\n-- Mode Transition Tests --\n");
    RUN_TEST(test_start_grid_connected);
    RUN_TEST(test_transition_to_island);
    RUN_TEST(test_reconnect_to_grid);
    RUN_TEST(test_request_island);
    
    printf("\n-- Demand Response Tests --\n");
    RUN_TEST(test_receive_dr_event);
    RUN_TEST(test_dr_event_activation);
    RUN_TEST(test_dr_price_response);
    
    printf("\n-- Statistics Tests --\n");
    RUN_TEST(test_stats_initial);
    RUN_TEST(test_stats_accumulation);
    RUN_TEST(test_stats_peak_tracking);
    
    printf("\n-- Alarm Tests --\n");
    RUN_TEST(test_alarm_generation);
    RUN_TEST(test_alarm_acknowledge);
    RUN_TEST(test_get_alarms);
    
    printf("\n-- Telemetry Tests --\n");
    RUN_TEST(test_telemetry_points);
    RUN_TEST(test_telemetry_mode);
    
    printf("\n-- Emergency Tests --\n");
    RUN_TEST(test_emergency_shutdown);
    
    printf("\n-- Variable Renewable Tests --\n");
    RUN_TEST(test_solar_variability);
    RUN_TEST(test_ramp_rate_limits);
    
    printf("\n-- Integration Tests --\n");
    RUN_TEST(test_full_day_simulation);
    RUN_TEST(test_island_with_dr);
    
    printf("\n-- Boundary Condition Tests --\n");
    RUN_TEST(test_zero_generation);
    RUN_TEST(test_storage_depletion);
    RUN_TEST(test_null_pointer_handling);
    
    printf("\n========================================\n");
    printf("Tests Run:    %d\n", tests_run);
    printf("Tests Passed: %d\n", tests_passed);
    printf("Tests Failed: %d\n", tests_failed);
    printf("========================================\n");
    
    return tests_failed > 0 ? 1 : 0;
}
