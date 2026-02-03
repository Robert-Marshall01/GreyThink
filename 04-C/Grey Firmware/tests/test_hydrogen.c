/**
 * @file test_hydrogen.c
 * @brief Integration Tests for Hydrogen Fuel Cell Control & Telemetry Spotlight
 * 
 * Test coverage includes:
 * - Fuel cell initialization and configuration
 * - Electrolyzer initialization and configuration
 * - Power setpoint control
 * - Temperature management
 * - Efficiency calculations
 * - Safety alarm generation
 * - Emergency shutdown
 * - Telemetry generation
 * - Fault injection and recovery
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <math.h>
#include <stdint.h>
#include <stdbool.h>

/* Test framework macros */
#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        printf("  FAIL: %s (line %d)\n", msg, __LINE__); \
        return 1; \
    } \
} while(0)

#define TEST_ASSERT_EQ_INT(expected, actual, msg) do { \
    if ((expected) != (actual)) { \
        printf("  FAIL: %s - expected %d, got %d (line %d)\n", \
               msg, (int)(expected), (int)(actual), __LINE__); \
        return 1; \
    } \
} while(0)

#define TEST_ASSERT_FLOAT_NEAR(expected, actual, tol, msg) do { \
    float diff = fabsf((float)(expected) - (float)(actual)); \
    if (diff > (float)(tol)) { \
        printf("  FAIL: %s - expected %.4f, got %.4f (line %d)\n", \
               msg, (float)(expected), (float)(actual), __LINE__); \
        return 1; \
    } \
} while(0)

#define RUN_TEST(name) do { \
    printf("  Running %s...\n", #name); \
    int result = name(); \
    if (result == 0) { \
        tests_passed++; \
        printf("  PASS: %s\n", #name); \
    } else { \
        tests_failed++; \
    } \
    tests_run++; \
} while(0)

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

/* ============================================================================
 * Test Stubs - Forward declarations from hydrogen_spotlight.c
 * ============================================================================ */

/* Mode and state enums */
typedef enum {
    H2_MODE_OFF,
    H2_MODE_STANDBY,
    H2_MODE_STARTUP,
    H2_MODE_RUNNING,
    H2_MODE_LOAD_FOLLOW,
    H2_MODE_SHUTDOWN,
    H2_MODE_EMERGENCY,
    H2_MODE_MAINTENANCE,
    H2_MODE_FAULT
} h2_mode_t;

typedef enum {
    FC_STATE_OFF,
    FC_STATE_PURGING,
    FC_STATE_WARMUP,
    FC_STATE_RUNNING,
    FC_STATE_COOLDOWN,
    FC_STATE_FAULT
} fc_state_t;

typedef enum {
    EL_STATE_OFF,
    EL_STATE_STANDBY,
    EL_STATE_WARMUP,
    EL_STATE_PRODUCTION,
    EL_STATE_COOLDOWN,
    EL_STATE_FAULT
} el_state_t;

typedef enum {
    ALARM_NONE,
    ALARM_H2_LEAK,
    ALARM_OVER_TEMP,
    ALARM_UNDER_TEMP,
    ALARM_LOW_CELL_VOLTAGE,
    ALARM_HIGH_CURRENT,
    ALARM_LOW_PRESSURE,
    ALARM_HIGH_PRESSURE,
    ALARM_PURITY_LOW,
    ALARM_COOLANT_FLOW,
    ALARM_VENT_FAILURE,
    ALARM_WATER_QUALITY,
    ALARM_STACK_IMBALANCE,
    ALARM_COMM_FAULT
} h2_alarm_type_t;

typedef enum {
    SEVERITY_INFO,
    SEVERITY_WARNING,
    SEVERITY_ALARM,
    SEVERITY_CRITICAL
} h2_severity_t;

/* Status structures */
#define H2_MAX_CELLS 120

typedef struct {
    fc_state_t state;
    float stack_voltage;
    float stack_current;
    float power_output_kw;
    float cell_voltages[H2_MAX_CELLS];
    float min_cell_voltage;
    float max_cell_voltage;
    float cell_spread;
    float stack_temp_c;
    float coolant_temp_in_c;
    float coolant_temp_out_c;
    float h2_flow_slpm;
    float air_flow_slpm;
    float efficiency_pct;
    uint32_t run_hours;
    uint32_t start_cycles;
} fc_status_t;

typedef struct {
    el_state_t state;
    float voltage;
    float current;
    float power_kw;
    float h2_rate_kg_h;
    float h2_rate_nm3_h;
    float o2_rate_kg_h;
    float h2_purity_pct;
    float h2_pressure_bar;
    float stack_temp_c;
    float water_resistivity;
    float specific_energy;
    float efficiency_pct;
    uint32_t run_hours;
} el_status_t;

typedef struct {
    bool e_stop_active;
    bool h2_leak_detected;
    float h2_concentration_ppm;
    bool vent_active;
    float vent_flow_rate;
    bool coolant_ok;
    bool water_ok;
    uint8_t active_alarms;
} h2_safety_t;

typedef struct {
    float h2_produced_kg;
    float h2_consumed_kg;
    float energy_produced_kwh;
    float energy_consumed_kwh;
    float water_consumed_l;
    float overall_efficiency;
    uint32_t fc_run_seconds;
    uint32_t el_run_seconds;
    uint32_t uptime_seconds;
} h2_stats_t;

typedef struct {
    uint32_t alarm_id;
    h2_alarm_type_t type;
    h2_severity_t severity;
    uint32_t timestamp;
    float value;
    float threshold;
    int acknowledged;
    int active;
} h2_alarm_t;

/* Function declarations (from hydrogen_spotlight.c) */
extern int h2_init(void);
extern int h2_shutdown(void);
extern int h2_configure_fuelcell(uint8_t cell_count, float rated_power_kw);
extern int h2_configure_electrolyzer(uint8_t cell_count, float rated_power_kw, float rated_h2_kg_h);
extern int h2_set_mode(h2_mode_t mode);
extern int h2_set_fc_power(float power_kw);
extern int h2_set_el_power(float power_kw);
extern int h2_set_el_production(float h2_kg_h);
extern int h2_emergency_stop(void);
extern int h2_clear_emergency(void);
extern int h2_process(uint32_t delta_ms);
extern h2_mode_t h2_get_mode(void);
extern int h2_get_fc_status(fc_status_t* status);
extern int h2_get_el_status(el_status_t* status);
extern int h2_get_safety(h2_safety_t* safety);
extern int h2_get_stats(h2_stats_t* stats);
extern int h2_get_alarms(h2_alarm_t* alarms, uint8_t max_alarms, uint8_t* count);
extern int h2_acknowledge_alarm(uint32_t alarm_id);
extern int h2_generate_telemetry(uint8_t* buffer, uint16_t max_len, uint16_t* len);
extern int h2_inject_leak(float concentration_ppm);
extern int h2_inject_temperature(int fuel_cell, float temp_c);
extern int h2_inject_water_quality(float resistivity);

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

static int test_init_success(void) {
    TEST_ASSERT_EQ_INT(0, h2_init(), "Init should succeed");
    TEST_ASSERT_EQ_INT(H2_MODE_OFF, h2_get_mode(), "Mode should be OFF");
    
    h2_shutdown();
    return 0;
}

static int test_init_double_init(void) {
    TEST_ASSERT_EQ_INT(0, h2_init(), "First init should succeed");
    TEST_ASSERT_EQ_INT(-1, h2_init(), "Second init should fail");
    
    h2_shutdown();
    return 0;
}

static int test_shutdown_not_initialized(void) {
    /* Ensure clean state */
    TEST_ASSERT_EQ_INT(-1, h2_shutdown(), "Shutdown before init should fail");
    return 0;
}

static int test_initial_state(void) {
    h2_init();
    
    fc_status_t fc_status;
    el_status_t el_status;
    h2_safety_t safety;
    
    TEST_ASSERT_EQ_INT(0, h2_get_fc_status(&fc_status), "FC status query OK");
    TEST_ASSERT_EQ_INT(FC_STATE_OFF, fc_status.state, "FC state OFF");
    TEST_ASSERT_FLOAT_NEAR(0.0f, fc_status.power_output_kw, 0.1f, "FC power zero");
    
    TEST_ASSERT_EQ_INT(0, h2_get_el_status(&el_status), "EL status query OK");
    TEST_ASSERT_EQ_INT(EL_STATE_OFF, el_status.state, "EL state OFF");
    TEST_ASSERT_FLOAT_NEAR(0.0f, el_status.power_kw, 0.1f, "EL power zero");
    
    TEST_ASSERT_EQ_INT(0, h2_get_safety(&safety), "Safety query OK");
    TEST_ASSERT_EQ_INT(0, safety.e_stop_active, "E-stop not active");
    TEST_ASSERT_EQ_INT(0, safety.h2_leak_detected, "No leak detected");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Configuration Tests
 * ============================================================================ */

static int test_configure_fuelcell(void) {
    h2_init();
    
    TEST_ASSERT_EQ_INT(0, h2_configure_fuelcell(80, 50.0f), "Config FC OK");
    
    h2_shutdown();
    return 0;
}

static int test_configure_fuelcell_max_cells(void) {
    h2_init();
    
    TEST_ASSERT_EQ_INT(0, h2_configure_fuelcell(120, 200.0f), "120 cells OK");
    TEST_ASSERT_EQ_INT(-2, h2_configure_fuelcell(130, 200.0f), "130 cells should fail");
    
    h2_shutdown();
    return 0;
}

static int test_configure_electrolyzer(void) {
    h2_init();
    
    TEST_ASSERT_EQ_INT(0, h2_configure_electrolyzer(50, 100.0f, 2.0f), "Config EL OK");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Mode Control Tests
 * ============================================================================ */

static int test_mode_transitions(void) {
    h2_init();
    
    TEST_ASSERT_EQ_INT(0, h2_set_mode(H2_MODE_STANDBY), "Set standby OK");
    TEST_ASSERT_EQ_INT(H2_MODE_STANDBY, h2_get_mode(), "Mode is standby");
    
    TEST_ASSERT_EQ_INT(0, h2_set_mode(H2_MODE_STARTUP), "Set startup OK");
    TEST_ASSERT_EQ_INT(H2_MODE_STARTUP, h2_get_mode(), "Mode is startup");
    
    TEST_ASSERT_EQ_INT(0, h2_set_mode(H2_MODE_RUNNING), "Set running OK");
    TEST_ASSERT_EQ_INT(H2_MODE_RUNNING, h2_get_mode(), "Mode is running");
    
    TEST_ASSERT_EQ_INT(0, h2_set_mode(H2_MODE_SHUTDOWN), "Set shutdown OK");
    TEST_ASSERT_EQ_INT(H2_MODE_SHUTDOWN, h2_get_mode(), "Mode is shutdown");
    
    h2_shutdown();
    return 0;
}

static int test_emergency_mode(void) {
    h2_init();
    
    h2_set_mode(H2_MODE_RUNNING);
    
    TEST_ASSERT_EQ_INT(0, h2_emergency_stop(), "E-stop OK");
    TEST_ASSERT_EQ_INT(H2_MODE_EMERGENCY, h2_get_mode(), "Mode is emergency");
    
    fc_status_t fc_status;
    h2_get_fc_status(&fc_status);
    TEST_ASSERT_EQ_INT(FC_STATE_FAULT, fc_status.state, "FC in fault");
    
    h2_shutdown();
    return 0;
}

static int test_emergency_clear(void) {
    h2_init();
    
    h2_emergency_stop();
    TEST_ASSERT_EQ_INT(0, h2_clear_emergency(), "Clear emergency OK");
    TEST_ASSERT_EQ_INT(H2_MODE_OFF, h2_get_mode(), "Mode is OFF");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Fuel Cell Operation Tests
 * ============================================================================ */

static int test_fc_startup_sequence(void) {
    h2_init();
    h2_inject_temperature(1, 45.0f);  /* Pre-heat to min operating temp */
    
    h2_set_mode(H2_MODE_STARTUP);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    TEST_ASSERT(status.state == FC_STATE_PURGING || status.state == FC_STATE_WARMUP,
                "FC in startup sequence");
    
    /* Process to complete warmup */
    h2_process(1000);
    h2_get_fc_status(&status);
    TEST_ASSERT_EQ_INT(FC_STATE_RUNNING, status.state, "FC now running");
    
    h2_shutdown();
    return 0;
}

static int test_fc_power_setpoint(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);  /* Complete startup */
    
    h2_set_fc_power(50.0f);
    h2_process(100);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    TEST_ASSERT_FLOAT_NEAR(50.0f, status.power_output_kw, 1.0f, "FC power at 50 kW");
    TEST_ASSERT(status.stack_voltage > 0, "Stack voltage positive");
    TEST_ASSERT(status.stack_current > 0, "Stack current positive");
    
    h2_shutdown();
    return 0;
}

static int test_fc_power_clamped(void) {
    h2_init();
    
    /* Set power above rated - should clamp */
    h2_set_fc_power(500.0f);  /* Default rated is 100 kW */
    
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    TEST_ASSERT(status.power_output_kw <= 100.0f, "Power clamped to rated");
    
    h2_shutdown();
    return 0;
}

static int test_fc_efficiency(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_fc_power(50.0f);
    h2_process(100);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    TEST_ASSERT(status.efficiency_pct > 0, "Efficiency calculated");
    TEST_ASSERT(status.efficiency_pct <= 100.0f, "Efficiency reasonable");
    TEST_ASSERT(status.h2_flow_slpm > 0, "H2 flow positive");
    
    h2_shutdown();
    return 0;
}

static int test_fc_cell_voltage_alarm(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    /* High load causes voltage drop */
    h2_set_fc_power(100.0f);
    h2_process(100);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    /* At high load, cell voltage should drop */
    TEST_ASSERT(status.min_cell_voltage < 1.0f, "Cell voltage drops at load");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Electrolyzer Operation Tests
 * ============================================================================ */

static int test_el_startup_sequence(void) {
    h2_init();
    h2_inject_temperature(0, 55.0f);  /* Pre-heat electrolyzer */
    
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    el_status_t status;
    h2_get_el_status(&status);
    TEST_ASSERT_EQ_INT(EL_STATE_PRODUCTION, status.state, "EL in production");
    
    h2_shutdown();
    return 0;
}

static int test_el_power_mode(void) {
    h2_init();
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_el_power(50.0f);
    h2_process(100);
    
    el_status_t status;
    h2_get_el_status(&status);
    TEST_ASSERT_FLOAT_NEAR(50.0f, status.power_kw, 1.0f, "EL power at 50 kW");
    TEST_ASSERT(status.h2_rate_kg_h > 0, "H2 production positive");
    
    h2_shutdown();
    return 0;
}

static int test_el_production_mode(void) {
    h2_init();
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_el_production(1.0f);  /* 1 kg/h target */
    h2_process(100);
    
    el_status_t status;
    h2_get_el_status(&status);
    TEST_ASSERT(status.h2_rate_kg_h > 0, "H2 production positive");
    TEST_ASSERT(status.power_kw > 0, "Power consumption positive");
    
    h2_shutdown();
    return 0;
}

static int test_el_efficiency(void) {
    h2_init();
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_el_power(50.0f);
    h2_process(100);
    
    el_status_t status;
    h2_get_el_status(&status);
    TEST_ASSERT(status.efficiency_pct > 0, "Efficiency calculated");
    TEST_ASSERT(status.specific_energy > 0, "Specific energy calculated");
    /* Typical PEM: 50-70 kWh/kg, so efficiency 55-80% of theoretical */
    TEST_ASSERT(status.efficiency_pct <= 100.0f, "Efficiency reasonable");
    
    h2_shutdown();
    return 0;
}

static int test_el_h2_purity(void) {
    h2_init();
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_el_power(50.0f);
    h2_process(100);
    
    el_status_t status;
    h2_get_el_status(&status);
    TEST_ASSERT(status.h2_purity_pct >= 99.5f, "H2 purity high (PEM)");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Safety Tests
 * ============================================================================ */

static int test_h2_leak_warning(void) {
    h2_init();
    h2_set_mode(H2_MODE_RUNNING);
    
    h2_inject_leak(1500.0f);  /* Above warning threshold */
    h2_process(100);
    
    h2_safety_t safety;
    h2_get_safety(&safety);
    TEST_ASSERT_EQ_INT(0, safety.h2_leak_detected, "Not critical leak");
    TEST_ASSERT(safety.active_alarms > 0, "Warning alarm active");
    
    h2_shutdown();
    return 0;
}

static int test_h2_leak_critical(void) {
    h2_init();
    h2_set_mode(H2_MODE_RUNNING);
    
    h2_inject_leak(5000.0f);  /* Above critical threshold */
    h2_process(100);
    
    h2_safety_t safety;
    h2_get_safety(&safety);
    TEST_ASSERT_EQ_INT(1, safety.h2_leak_detected, "Critical leak detected");
    TEST_ASSERT_EQ_INT(H2_MODE_EMERGENCY, h2_get_mode(), "Emergency mode");
    
    h2_shutdown();
    return 0;
}

static int test_over_temperature_alarm(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_inject_temperature(1, 85.0f);  /* Above max operating */
    h2_set_fc_power(50.0f);
    h2_process(100);
    
    h2_alarm_t alarms[8];
    uint8_t count;
    h2_get_alarms(alarms, 8, &count);
    
    int found = 0;
    for (uint8_t i = 0; i < count; i++) {
        if (alarms[i].type == ALARM_OVER_TEMP) {
            found = 1;
            break;
        }
    }
    TEST_ASSERT(found, "Over-temp alarm generated");
    
    h2_shutdown();
    return 0;
}

static int test_e_stop_triggers_fault(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_emergency_stop();
    h2_process(100);
    
    fc_status_t fc_status;
    el_status_t el_status;
    h2_get_fc_status(&fc_status);
    h2_get_el_status(&el_status);
    
    TEST_ASSERT_EQ_INT(FC_STATE_FAULT, fc_status.state, "FC in fault");
    TEST_ASSERT_EQ_INT(EL_STATE_FAULT, el_status.state, "EL in fault");
    
    h2_shutdown();
    return 0;
}

static int test_alarm_acknowledge(void) {
    h2_init();
    h2_set_mode(H2_MODE_RUNNING);
    
    h2_inject_leak(1500.0f);
    h2_process(100);
    
    h2_alarm_t alarms[8];
    uint8_t count;
    h2_get_alarms(alarms, 8, &count);
    TEST_ASSERT(count > 0, "Alarm active");
    
    uint32_t alarm_id = alarms[0].alarm_id;
    TEST_ASSERT_EQ_INT(0, h2_acknowledge_alarm(alarm_id), "Ack OK");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Statistics Tests
 * ============================================================================ */

static int test_statistics_accumulation(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_fc_power(50.0f);
    h2_set_el_power(50.0f);
    
    /* Run for 10 seconds simulated time */
    for (int i = 0; i < 10; i++) {
        h2_process(1000);
    }
    
    h2_stats_t stats;
    h2_get_stats(&stats);
    
    TEST_ASSERT(stats.energy_produced_kwh > 0, "Energy produced tracked");
    TEST_ASSERT(stats.energy_consumed_kwh > 0, "Energy consumed tracked");
    TEST_ASSERT(stats.h2_produced_kg > 0, "H2 production tracked");
    TEST_ASSERT(stats.h2_consumed_kg > 0, "H2 consumption tracked");
    TEST_ASSERT(stats.fc_run_seconds >= 10, "FC run time tracked");
    TEST_ASSERT(stats.el_run_seconds >= 10, "EL run time tracked");
    
    h2_shutdown();
    return 0;
}

static int test_water_consumption(void) {
    h2_init();
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_el_power(100.0f);
    
    /* Run for 10 seconds */
    for (int i = 0; i < 10; i++) {
        h2_process(1000);
    }
    
    h2_stats_t stats;
    h2_get_stats(&stats);
    TEST_ASSERT(stats.water_consumed_l > 0, "Water consumption tracked");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Telemetry Tests
 * ============================================================================ */

static int test_telemetry_generation(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    h2_set_fc_power(50.0f);
    h2_process(100);
    
    uint8_t buffer[256];
    uint16_t len;
    
    TEST_ASSERT_EQ_INT(0, h2_generate_telemetry(buffer, 256, &len), "Telemetry gen OK");
    TEST_ASSERT(len > 0, "Telemetry has data");
    TEST_ASSERT(buffer[0] == 'H' && buffer[1] == '2', "Telemetry header correct");
    
    h2_shutdown();
    return 0;
}

static int test_telemetry_buffer_too_small(void) {
    h2_init();
    
    uint8_t buffer[32];
    uint16_t len;
    
    TEST_ASSERT_EQ_INT(-2, h2_generate_telemetry(buffer, 32, &len), "Buffer too small");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Variable Load Tests
 * ============================================================================ */

static int test_fc_load_variation(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    float powers[] = {10.0f, 25.0f, 50.0f, 75.0f, 100.0f};
    fc_status_t status;
    
    for (int i = 0; i < 5; i++) {
        h2_set_fc_power(powers[i]);
        h2_process(100);
        h2_get_fc_status(&status);
        
        TEST_ASSERT_FLOAT_NEAR(powers[i], status.power_output_kw, 1.0f, "Power tracks setpoint");
    }
    
    h2_shutdown();
    return 0;
}

static int test_el_load_variation(void) {
    h2_init();
    h2_inject_temperature(0, 55.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    float powers[] = {20.0f, 40.0f, 60.0f, 80.0f, 100.0f};
    el_status_t status;
    
    for (int i = 0; i < 5; i++) {
        h2_set_el_power(powers[i]);
        h2_process(100);
        h2_get_el_status(&status);
        
        TEST_ASSERT_FLOAT_NEAR(powers[i], status.power_kw, 1.0f, "Power tracks setpoint");
    }
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Fault Injection Tests
 * ============================================================================ */

static int test_temperature_injection(void) {
    h2_init();
    
    h2_inject_temperature(1, 75.0f);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    TEST_ASSERT_FLOAT_NEAR(75.0f, status.stack_temp_c, 1.0f, "FC temp injected");
    
    h2_inject_temperature(0, 55.0f);
    
    el_status_t el_status;
    h2_get_el_status(&el_status);
    TEST_ASSERT_FLOAT_NEAR(55.0f, el_status.stack_temp_c, 1.0f, "EL temp injected");
    
    h2_shutdown();
    return 0;
}

static int test_water_quality_injection(void) {
    h2_init();
    
    h2_inject_water_quality(10.0f);  /* Below ideal 18 MΩ·cm */
    
    el_status_t status;
    h2_get_el_status(&status);
    TEST_ASSERT_FLOAT_NEAR(10.0f, status.water_resistivity, 0.1f, "Water quality injected");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Process Loop Tests
 * ============================================================================ */

static int test_process_increments_uptime(void) {
    h2_init();
    
    h2_process(5000);
    
    h2_stats_t stats;
    h2_get_stats(&stats);
    TEST_ASSERT(stats.uptime_seconds >= 5, "Uptime incremented");
    
    h2_shutdown();
    return 0;
}

static int test_process_not_initialized(void) {
    /* Not initialized */
    TEST_ASSERT_EQ_INT(-1, h2_process(100), "Process fails if not init");
    return 0;
}

/* ============================================================================
 * Edge Case Tests
 * ============================================================================ */

static int test_zero_power_setpoint(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_fc_power(0.0f);
    h2_process(100);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    TEST_ASSERT_FLOAT_NEAR(0.0f, status.power_output_kw, 0.1f, "Zero power accepted");
    
    h2_shutdown();
    return 0;
}

static int test_negative_power_clamped(void) {
    h2_init();
    
    h2_set_fc_power(-10.0f);
    h2_set_el_power(-10.0f);
    
    /* Powers should be clamped to zero internally */
    h2_shutdown();
    return 0;  /* No crash = pass */
}

static int test_null_pointer_handling(void) {
    h2_init();
    
    TEST_ASSERT_EQ_INT(-1, h2_get_fc_status(NULL), "Null FC status rejected");
    TEST_ASSERT_EQ_INT(-1, h2_get_el_status(NULL), "Null EL status rejected");
    TEST_ASSERT_EQ_INT(-1, h2_get_safety(NULL), "Null safety rejected");
    TEST_ASSERT_EQ_INT(-1, h2_get_stats(NULL), "Null stats rejected");
    
    h2_shutdown();
    return 0;
}

static int test_cooldown_sequence(void) {
    h2_init();
    h2_inject_temperature(1, 65.0f);
    h2_set_mode(H2_MODE_STARTUP);
    h2_process(1000);
    
    h2_set_mode(H2_MODE_SHUTDOWN);
    
    fc_status_t status;
    h2_get_fc_status(&status);
    TEST_ASSERT_EQ_INT(FC_STATE_COOLDOWN, status.state, "FC in cooldown");
    
    h2_shutdown();
    return 0;
}

/* ============================================================================
 * Test Suite Runner
 * ============================================================================ */

int main(void) {
    printf("================================================\n");
    printf("Hydrogen Fuel Cell Control Test Suite\n");
    printf("================================================\n\n");
    
    printf("--- Initialization Tests ---\n");
    RUN_TEST(test_init_success);
    RUN_TEST(test_init_double_init);
    RUN_TEST(test_shutdown_not_initialized);
    RUN_TEST(test_initial_state);
    
    printf("\n--- Configuration Tests ---\n");
    RUN_TEST(test_configure_fuelcell);
    RUN_TEST(test_configure_fuelcell_max_cells);
    RUN_TEST(test_configure_electrolyzer);
    
    printf("\n--- Mode Control Tests ---\n");
    RUN_TEST(test_mode_transitions);
    RUN_TEST(test_emergency_mode);
    RUN_TEST(test_emergency_clear);
    
    printf("\n--- Fuel Cell Operation Tests ---\n");
    RUN_TEST(test_fc_startup_sequence);
    RUN_TEST(test_fc_power_setpoint);
    RUN_TEST(test_fc_power_clamped);
    RUN_TEST(test_fc_efficiency);
    RUN_TEST(test_fc_cell_voltage_alarm);
    
    printf("\n--- Electrolyzer Operation Tests ---\n");
    RUN_TEST(test_el_startup_sequence);
    RUN_TEST(test_el_power_mode);
    RUN_TEST(test_el_production_mode);
    RUN_TEST(test_el_efficiency);
    RUN_TEST(test_el_h2_purity);
    
    printf("\n--- Safety Tests ---\n");
    RUN_TEST(test_h2_leak_warning);
    RUN_TEST(test_h2_leak_critical);
    RUN_TEST(test_over_temperature_alarm);
    RUN_TEST(test_e_stop_triggers_fault);
    RUN_TEST(test_alarm_acknowledge);
    
    printf("\n--- Statistics Tests ---\n");
    RUN_TEST(test_statistics_accumulation);
    RUN_TEST(test_water_consumption);
    
    printf("\n--- Telemetry Tests ---\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_buffer_too_small);
    
    printf("\n--- Variable Load Tests ---\n");
    RUN_TEST(test_fc_load_variation);
    RUN_TEST(test_el_load_variation);
    
    printf("\n--- Fault Injection Tests ---\n");
    RUN_TEST(test_temperature_injection);
    RUN_TEST(test_water_quality_injection);
    
    printf("\n--- Process Loop Tests ---\n");
    RUN_TEST(test_process_increments_uptime);
    RUN_TEST(test_process_not_initialized);
    
    printf("\n--- Edge Case Tests ---\n");
    RUN_TEST(test_zero_power_setpoint);
    RUN_TEST(test_negative_power_clamped);
    RUN_TEST(test_null_pointer_handling);
    RUN_TEST(test_cooldown_sequence);
    
    printf("\n================================================\n");
    printf("Test Results: %d/%d passed, %d failed\n", 
           tests_passed, tests_run, tests_failed);
    printf("================================================\n");
    
    return (tests_failed > 0) ? 1 : 0;
}
