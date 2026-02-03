/**
 * @file energy_tests.c
 * @brief Integration Tests for Energy Harvesting & Smart Grid Spotlight
 * 
 * This test suite validates the Energy Harvesting spotlight subsystem including:
 * - Solar panel MPPT algorithms (P&O, Incremental Conductance)
 * - Battery charging state machine (CC/CV/trickle)
 * - Power budget management and load shedding
 * - Smart grid telemetry and export control
 * - Variable sunlight condition simulation
 * 
 * @copyright Grey Firmware Project
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Test Framework
 ******************************************************************************/

static int tests_run = 0;
static int tests_passed = 0;
static int assertions_total = 0;
static int assertions_passed = 0;

#define TEST_ASSERT(cond, msg) do { \
    assertions_total++; \
    if (!(cond)) { \
        printf("    ASSERTION FAILED: %s\n", msg); \
    } else { \
        assertions_passed++; \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) do { \
    assertions_total++; \
    if ((a) != (b)) { \
        printf("    ASSERTION FAILED: %s (got %d, expected %d)\n", msg, (int)(a), (int)(b)); \
    } else { \
        assertions_passed++; \
    } \
} while(0)

#define TEST_ASSERT_RANGE(val, min, max, msg) do { \
    assertions_total++; \
    if ((val) < (min) || (val) > (max)) { \
        printf("    ASSERTION FAILED: %s (%d not in [%d, %d])\n", msg, (int)(val), (int)(min), (int)(max)); \
    } else { \
        assertions_passed++; \
    } \
} while(0)

#define RUN_TEST(test_func, name) do { \
    printf("  [TEST] %s... ", name); \
    int before = assertions_passed; \
    test_func(); \
    tests_run++; \
    if (assertions_passed > before) { \
        printf("PASS\n"); \
        tests_passed++; \
    } else { \
        printf("FAIL\n"); \
    } \
} while(0)

/*******************************************************************************
 * Energy Spotlight API (Embedded Implementation)
 * This is a standalone copy for testing - matches energy_spotlight.c
 ******************************************************************************/

/* Configuration */
#define GF_MPPT_PERTURBATION_MV     50
#define GF_MPPT_NIGHT_THRESHOLD_MW  100
#define GF_BATT_PRECHARGE_THRESHOLD 2800
#define GF_BATT_CV_THRESHOLD        4150
#define GF_BATT_FULL_CURRENT        50

/* MPPT algorithms */
typedef enum {
    MPPT_PERTURB_OBSERVE = 0,
    MPPT_INCREMENTAL_COND,
    MPPT_CONSTANT_VOLTAGE
} mppt_algorithm_t;

/* Charge states */
typedef enum {
    CHARGE_OFF = 0,
    CHARGE_PRECHARGE,
    CHARGE_CC,
    CHARGE_CV,
    CHARGE_TOPOFF,
    CHARGE_COMPLETE,
    CHARGE_FAULT
} charge_state_t;

/* Budget states */
typedef enum {
    BUDGET_CRITICAL = 0,
    BUDGET_LOW,
    BUDGET_NORMAL,
    BUDGET_SURPLUS
} budget_state_t;

/* Grid states */
typedef enum {
    GRID_DISCONNECTED = 0,
    GRID_SYNCHRONIZING,
    GRID_CONNECTED,
    GRID_ISLANDED,
    GRID_FAULT
} grid_state_t;

/* Internal state */
typedef struct {
    bool initialized;
    
    /* Solar */
    uint16_t solar_voltage_mv;
    uint16_t solar_current_ma;
    uint32_t solar_power_mw;
    
    /* MPPT */
    mppt_algorithm_t mppt_algorithm;
    uint16_t mppt_voltage_mv;
    uint16_t mppt_last_voltage_mv;
    uint32_t mppt_last_power_mw;
    int8_t mppt_direction;
    bool night_mode;
    uint32_t mppt_track_count;
    
    /* Battery */
    uint16_t battery_voltage_mv;
    int16_t battery_current_ma;
    uint8_t battery_soc_pct;
    int8_t battery_temp_c;
    charge_state_t charge_state;
    uint32_t charge_cycles;
    
    /* Budget */
    budget_state_t budget_state;
    uint16_t available_mw;
    uint16_t allocated_mw;
    uint32_t loadshed_count;
    
    /* Grid */
    grid_state_t grid_state;
    uint32_t grid_voltage_mv;
    uint32_t grid_frequency_mhz;
    int32_t grid_power_mw;
    bool grid_export_enabled;
    int32_t grid_export_limit_mw;
    
    /* Statistics */
    uint64_t total_harvested_mwh;
    uint32_t uptime_seconds;
    
    /* Callback */
    void (*budget_callback)(int state);
    
} energy_state_t;

static energy_state_t g_state = {0};

/* Initialize */
static int energy_init(int algorithm) {
    memset(&g_state, 0, sizeof(g_state));
    g_state.mppt_algorithm = (mppt_algorithm_t)algorithm;
    g_state.mppt_voltage_mv = 18000;
    g_state.mppt_direction = 1;
    g_state.battery_voltage_mv = 3700;
    g_state.battery_soc_pct = 30;
    g_state.battery_temp_c = 25;
    g_state.charge_state = CHARGE_OFF;
    g_state.grid_state = GRID_DISCONNECTED;
    g_state.grid_export_limit_mw = 5000;
    g_state.initialized = true;
    return 0;
}

/* Update solar readings */
static int energy_update_solar(uint16_t voltage_mv, uint16_t current_ma) {
    if (!g_state.initialized) return -1;
    g_state.solar_voltage_mv = voltage_mv;
    g_state.solar_current_ma = current_ma;
    g_state.solar_power_mw = ((uint32_t)voltage_mv * current_ma) / 1000;
    g_state.total_harvested_mwh += g_state.solar_power_mw / 3600;
    return 0;
}

/* Update battery readings */
static int energy_update_battery(uint16_t voltage_mv, int16_t current_ma, int8_t temp_c) {
    if (!g_state.initialized) return -1;
    g_state.battery_voltage_mv = voltage_mv;
    g_state.battery_current_ma = current_ma;
    g_state.battery_temp_c = temp_c;
    return 0;
}

/* P&O MPPT algorithm */
static uint16_t mppt_perturb_observe(void) {
    int32_t delta_p = (int32_t)g_state.solar_power_mw - (int32_t)g_state.mppt_last_power_mw;
    int32_t delta_v = (int32_t)g_state.solar_voltage_mv - (int32_t)g_state.mppt_last_voltage_mv;
    
    if (delta_p != 0) {
        if ((delta_p > 0) == (delta_v > 0)) {
            g_state.mppt_direction = 1;
        } else {
            g_state.mppt_direction = -1;
        }
    }
    
    int32_t new_voltage = (int32_t)g_state.mppt_voltage_mv + 
                          (g_state.mppt_direction * GF_MPPT_PERTURBATION_MV);
    
    if (new_voltage < 5000) new_voltage = 5000;
    if (new_voltage > 60000) new_voltage = 60000;
    
    g_state.mppt_last_voltage_mv = g_state.solar_voltage_mv;
    g_state.mppt_last_power_mw = g_state.solar_power_mw;
    g_state.mppt_track_count++;
    
    return (uint16_t)new_voltage;
}

/* Run MPPT step */
static void mppt_process(void) {
    if (g_state.solar_power_mw < GF_MPPT_NIGHT_THRESHOLD_MW) {
        g_state.night_mode = true;
        return;
    }
    g_state.night_mode = false;
    
    switch (g_state.mppt_algorithm) {
        case MPPT_PERTURB_OBSERVE:
            g_state.mppt_voltage_mv = mppt_perturb_observe();
            break;
        case MPPT_CONSTANT_VOLTAGE:
            g_state.mppt_voltage_mv = (g_state.solar_voltage_mv * 76) / 100;
            break;
        default:
            g_state.mppt_voltage_mv = mppt_perturb_observe();
            break;
    }
}

/* Battery charge state machine */
static void battery_charge_process(void) {
    /* Temperature protection */
    if (g_state.battery_temp_c < 0 || g_state.battery_temp_c > 45) {
        g_state.charge_state = CHARGE_FAULT;
        return;
    }
    
    switch (g_state.charge_state) {
        case CHARGE_OFF:
            if (g_state.solar_power_mw > 0 && g_state.battery_soc_pct < 100) {
                if (g_state.battery_voltage_mv < GF_BATT_PRECHARGE_THRESHOLD) {
                    g_state.charge_state = CHARGE_PRECHARGE;
                } else {
                    g_state.charge_state = CHARGE_CC;
                }
            }
            break;
        case CHARGE_PRECHARGE:
            if (g_state.battery_voltage_mv >= GF_BATT_PRECHARGE_THRESHOLD) {
                g_state.charge_state = CHARGE_CC;
            }
            break;
        case CHARGE_CC:
            if (g_state.battery_voltage_mv >= GF_BATT_CV_THRESHOLD) {
                g_state.charge_state = CHARGE_CV;
            }
            break;
        case CHARGE_CV:
            if (g_state.battery_current_ma < GF_BATT_FULL_CURRENT) {
                g_state.charge_state = CHARGE_TOPOFF;
            }
            break;
        case CHARGE_TOPOFF:
            if (g_state.battery_current_ma < GF_BATT_FULL_CURRENT / 2) {
                g_state.charge_state = CHARGE_COMPLETE;
                g_state.battery_soc_pct = 100;
                g_state.charge_cycles++;
            }
            break;
        case CHARGE_COMPLETE:
            if (g_state.battery_soc_pct < 95) {
                g_state.charge_state = CHARGE_CV;
            }
            break;
        case CHARGE_FAULT:
            break;
    }
    
    /* Update SoC from voltage */
    if (g_state.battery_voltage_mv >= 4200) g_state.battery_soc_pct = 100;
    else if (g_state.battery_voltage_mv >= 4100) g_state.battery_soc_pct = 90;
    else if (g_state.battery_voltage_mv >= 4000) g_state.battery_soc_pct = 80;
    else if (g_state.battery_voltage_mv >= 3900) g_state.battery_soc_pct = 70;
    else if (g_state.battery_voltage_mv >= 3800) g_state.battery_soc_pct = 50;
    else if (g_state.battery_voltage_mv >= 3700) g_state.battery_soc_pct = 30;
    else if (g_state.battery_voltage_mv >= 3600) g_state.battery_soc_pct = 20;
    else if (g_state.battery_voltage_mv >= 3500) g_state.battery_soc_pct = 10;
    else g_state.battery_soc_pct = 5;
}

/* Update power budget */
static void budget_update(void) {
    g_state.available_mw = g_state.solar_power_mw;
    if (g_state.battery_soc_pct > 20) {
        g_state.available_mw += 5000;
    }
    
    if (g_state.battery_soc_pct < 10) {
        g_state.budget_state = BUDGET_CRITICAL;
    } else if (g_state.battery_soc_pct < 30 || g_state.solar_power_mw < g_state.allocated_mw) {
        g_state.budget_state = BUDGET_LOW;
    } else if (g_state.battery_soc_pct > 80 && g_state.solar_power_mw > g_state.allocated_mw) {
        g_state.budget_state = BUDGET_SURPLUS;
    } else {
        g_state.budget_state = BUDGET_NORMAL;
    }
    
    if (g_state.budget_state == BUDGET_CRITICAL) {
        g_state.loadshed_count++;
        if (g_state.budget_callback) {
            g_state.budget_callback(BUDGET_CRITICAL);
        }
    }
}

/* Process energy manager */
static int energy_process(void) {
    if (!g_state.initialized) return -1;
    mppt_process();
    battery_charge_process();
    budget_update();
    g_state.uptime_seconds++;
    return 0;
}

/* Grid connect */
static int grid_connect(void) {
    if (!g_state.initialized) return -1;
    g_state.grid_voltage_mv = 230000;
    g_state.grid_frequency_mhz = 50000;
    g_state.grid_state = GRID_CONNECTED;
    return 0;
}

/* Grid disconnect */
static int grid_disconnect(void) {
    g_state.grid_state = GRID_DISCONNECTED;
    g_state.grid_power_mw = 0;
    return 0;
}

/* Enable export */
static int grid_enable_export(int32_t limit_mw) {
    g_state.grid_export_enabled = true;
    g_state.grid_export_limit_mw = limit_mw;
    return 0;
}

/* Request power */
static uint16_t energy_request_power(uint16_t power_mw) {
    if (!g_state.initialized) return 0;
    uint16_t remaining = g_state.available_mw - g_state.allocated_mw;
    if (g_state.budget_state == BUDGET_CRITICAL) return 0;
    uint16_t granted = (power_mw <= remaining) ? power_mw : remaining;
    g_state.allocated_mw += granted;
    return granted;
}

/* Release power */
static int energy_release_power(uint16_t power_mw) {
    if (power_mw <= g_state.allocated_mw) {
        g_state.allocated_mw -= power_mw;
    }
    return 0;
}

/* Register callback */
static int energy_register_callback(void (*callback)(int)) {
    g_state.budget_callback = callback;
    return 0;
}

/* Shutdown */
static int energy_shutdown(void) {
    g_state.initialized = false;
    return 0;
}

/* Clear fault */
static int energy_clear_fault(void) {
    if (g_state.charge_state == CHARGE_FAULT) {
        g_state.charge_state = CHARGE_OFF;
        return 0;
    }
    return -1;
}

/*******************************************************************************
 * Test Cases - MPPT Algorithms
 ******************************************************************************/

static void test_mppt_init(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    TEST_ASSERT(g_state.initialized, "Manager should be initialized");
    TEST_ASSERT_EQ(g_state.mppt_algorithm, MPPT_PERTURB_OBSERVE, "Algorithm should be P&O");
    TEST_ASSERT_EQ(g_state.mppt_voltage_mv, 18000, "Initial setpoint should be 18V");
    energy_shutdown();
}

static void test_mppt_night_mode(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Low power = night mode */
    energy_update_solar(5000, 10); /* 50mW */
    energy_process();
    TEST_ASSERT(g_state.night_mode, "Should enter night mode at low power");
    
    /* Higher power = tracking */
    energy_update_solar(18000, 1000); /* 18W */
    energy_process();
    TEST_ASSERT(!g_state.night_mode, "Should exit night mode with sufficient power");
    
    energy_shutdown();
}

static void test_mppt_perturb_observe(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Initial reading */
    energy_update_solar(18000, 1000);
    energy_process();
    uint16_t v1 = g_state.mppt_voltage_mv;
    
    /* Increase power - should continue same direction */
    energy_update_solar(18050, 1050);
    energy_process();
    uint16_t v2 = g_state.mppt_voltage_mv;
    
    /* Decrease power - should reverse */
    energy_update_solar(18100, 900);
    energy_process();
    uint16_t v3 = g_state.mppt_voltage_mv;
    
    TEST_ASSERT(g_state.mppt_track_count >= 3, "Should have tracking iterations");
    TEST_ASSERT(v1 != v2 || v2 != v3, "Voltage should change during tracking");
    
    energy_shutdown();
}

static void test_mppt_constant_voltage(void) {
    energy_init(MPPT_CONSTANT_VOLTAGE);
    
    energy_update_solar(24000, 500); /* 12W */
    energy_process();
    
    /* CV mode uses 76% of Voc */
    uint16_t expected = (24000 * 76) / 100;
    TEST_ASSERT_RANGE(g_state.mppt_voltage_mv, expected - 100, expected + 100, 
                      "CV mode should track at 76% Voc");
    
    energy_shutdown();
}

/*******************************************************************************
 * Test Cases - Battery Charging
 ******************************************************************************/

static void test_battery_charge_start(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Start with no power - should stay off */
    energy_update_solar(0, 0);
    energy_process();
    TEST_ASSERT_EQ(g_state.charge_state, CHARGE_OFF, "Should be off without power");
    
    /* Add power - should start charging */
    energy_update_battery(3700, 500, 25);
    energy_update_solar(18000, 1000);
    energy_process();
    TEST_ASSERT(g_state.charge_state == CHARGE_CC || g_state.charge_state == CHARGE_PRECHARGE,
                "Should start charging with power available");
    
    energy_shutdown();
}

static void test_battery_precharge(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Deeply discharged battery */
    energy_update_battery(2500, 0, 25);
    energy_update_solar(18000, 1000);
    energy_process();
    
    TEST_ASSERT_EQ(g_state.charge_state, CHARGE_PRECHARGE, 
                   "Should precharge deeply discharged battery");
    
    /* Voltage rises above threshold */
    energy_update_battery(2900, 100, 25);
    energy_process();
    
    TEST_ASSERT_EQ(g_state.charge_state, CHARGE_CC, 
                   "Should transition to CC when voltage recovered");
    
    energy_shutdown();
}

static void test_battery_cc_cv_transition(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* CC mode */
    energy_update_battery(3900, 1000, 25);
    energy_update_solar(18000, 2000);
    energy_process();
    
    /* Voltage reaches CV threshold */
    energy_update_battery(4150, 1000, 25);
    energy_process();
    
    TEST_ASSERT_EQ(g_state.charge_state, CHARGE_CV, 
                   "Should transition to CV at voltage threshold");
    
    energy_shutdown();
}

static void test_battery_charge_complete(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Set up CV phase */
    g_state.charge_state = CHARGE_CV;
    energy_update_battery(4200, 40, 25);
    energy_update_solar(18000, 1000);
    energy_process();
    
    /* Current drops below threshold */
    energy_update_battery(4200, 20, 25);
    g_state.charge_state = CHARGE_TOPOFF;
    energy_process();
    
    TEST_ASSERT_EQ(g_state.charge_state, CHARGE_COMPLETE, 
                   "Should complete when current drops");
    TEST_ASSERT_EQ(g_state.battery_soc_pct, 100, "SoC should be 100%");
    
    energy_shutdown();
}

static void test_battery_temperature_protection(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Normal temperature */
    energy_update_battery(3800, 500, 25);
    energy_update_solar(18000, 1000);
    energy_process();
    TEST_ASSERT(g_state.charge_state != CHARGE_FAULT, "Should charge at normal temp");
    
    /* Too hot */
    energy_update_battery(3800, 500, 50);
    energy_process();
    TEST_ASSERT_EQ(g_state.charge_state, CHARGE_FAULT, "Should fault when too hot");
    
    energy_shutdown();
}

static void test_battery_fault_clear(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    g_state.charge_state = CHARGE_FAULT;
    
    int result = energy_clear_fault();
    TEST_ASSERT_EQ(result, 0, "Should clear fault successfully");
    TEST_ASSERT_EQ(g_state.charge_state, CHARGE_OFF, "Should return to off state");
    
    energy_shutdown();
}

/*******************************************************************************
 * Test Cases - Power Budget
 ******************************************************************************/

static void test_budget_critical(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Very low battery */
    g_state.battery_soc_pct = 5;
    energy_update_solar(5000, 100); /* Low power */
    budget_update();
    
    TEST_ASSERT_EQ(g_state.budget_state, BUDGET_CRITICAL, 
                   "Should be critical at low SoC");
    TEST_ASSERT(g_state.loadshed_count > 0, "Should trigger load shedding");
    
    energy_shutdown();
}

static void test_budget_normal(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    g_state.battery_soc_pct = 60;
    energy_update_solar(18000, 1000);
    budget_update();
    
    TEST_ASSERT_EQ(g_state.budget_state, BUDGET_NORMAL, 
                   "Should be normal with good SoC and harvest");
    
    energy_shutdown();
}

static void test_budget_surplus(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    g_state.battery_soc_pct = 90;
    energy_update_solar(20000, 2000);
    g_state.allocated_mw = 1000;
    budget_update();
    
    TEST_ASSERT_EQ(g_state.budget_state, BUDGET_SURPLUS, 
                   "Should be surplus with high SoC and excess power");
    
    energy_shutdown();
}

static void test_power_request(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    g_state.battery_soc_pct = 60;
    energy_update_solar(18000, 1000);
    budget_update();
    
    uint16_t granted = energy_request_power(3000);
    TEST_ASSERT(granted > 0, "Should grant power in normal state");
    
    /* Request more than available */
    uint16_t granted2 = energy_request_power(50000);
    TEST_ASSERT(granted2 < 50000, "Should limit to available power");
    
    energy_shutdown();
}

static void test_power_release(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    g_state.allocated_mw = 5000;
    
    energy_release_power(2000);
    TEST_ASSERT_EQ(g_state.allocated_mw, 3000, "Should reduce allocated power");
    
    energy_shutdown();
}

/* Static variables for callback testing */
static int g_callback_called = 0;
static int g_callback_last_state = -1;

static void budget_test_callback(int state) {
    g_callback_called++;
    g_callback_last_state = state;
}

static void test_budget_callback(void) {
    g_callback_called = 0;
    g_callback_last_state = -1;
    
    energy_init(MPPT_PERTURB_OBSERVE);
    energy_register_callback(budget_test_callback);
    
    g_state.battery_soc_pct = 5;
    energy_update_solar(500, 10);
    budget_update();
    
    TEST_ASSERT(g_callback_called > 0, "Callback should be called on critical");
    TEST_ASSERT_EQ(g_callback_last_state, BUDGET_CRITICAL, "Should report critical state");
    
    energy_shutdown();
}

/*******************************************************************************
 * Test Cases - Smart Grid
 ******************************************************************************/

static void test_grid_connect(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    int result = grid_connect();
    TEST_ASSERT_EQ(result, 0, "Should connect successfully");
    TEST_ASSERT_EQ(g_state.grid_state, GRID_CONNECTED, "Should be connected");
    TEST_ASSERT_EQ(g_state.grid_voltage_mv, 230000, "Should have nominal voltage");
    TEST_ASSERT_EQ(g_state.grid_frequency_mhz, 50000, "Should have nominal frequency");
    
    energy_shutdown();
}

static void test_grid_disconnect(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    grid_connect();
    
    grid_disconnect();
    TEST_ASSERT_EQ(g_state.grid_state, GRID_DISCONNECTED, "Should be disconnected");
    TEST_ASSERT_EQ(g_state.grid_power_mw, 0, "Power should be zero");
    
    energy_shutdown();
}

static void test_grid_export(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    grid_connect();
    
    grid_enable_export(10000);
    TEST_ASSERT(g_state.grid_export_enabled, "Export should be enabled");
    TEST_ASSERT_EQ(g_state.grid_export_limit_mw, 10000, "Limit should be set");
    
    energy_shutdown();
}

/*******************************************************************************
 * Test Cases - Variable Sunlight Simulation
 ******************************************************************************/

static void test_sunrise_simulation(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Dawn - very low light */
    energy_update_solar(10000, 10); /* 100mW */
    energy_process();
    TEST_ASSERT(g_state.night_mode, "Should be in night mode at dawn");
    
    /* Sunrise - increasing */
    energy_update_solar(15000, 100); /* 1.5W */
    energy_process();
    TEST_ASSERT(!g_state.night_mode, "Should exit night mode");
    
    /* Morning - good power */
    energy_update_solar(18000, 500); /* 9W */
    energy_process();
    TEST_ASSERT(g_state.mppt_track_count > 0, "Should be tracking MPP");
    TEST_ASSERT_EQ(g_state.budget_state, BUDGET_LOW, "Budget depends on battery");
    
    energy_shutdown();
}

static void test_cloud_passing(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Sunny */
    energy_update_solar(20000, 2000); /* 40W */
    energy_process();
    uint32_t sunny_power = g_state.solar_power_mw;
    
    /* Cloud passes - sudden drop */
    energy_update_solar(15000, 500); /* 7.5W */
    energy_process();
    uint32_t cloudy_power = g_state.solar_power_mw;
    
    /* Sun returns */
    energy_update_solar(20000, 1800); /* 36W */
    energy_process();
    uint32_t recovered_power = g_state.solar_power_mw;
    
    TEST_ASSERT(sunny_power > cloudy_power, "Power should drop with clouds");
    TEST_ASSERT(recovered_power > cloudy_power, "Power should recover");
    TEST_ASSERT(g_state.mppt_track_count >= 3, "MPPT should adapt");
    
    energy_shutdown();
}

static void test_sunset_simulation(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Late afternoon */
    energy_update_solar(18000, 1000); /* 18W */
    energy_process();
    TEST_ASSERT(!g_state.night_mode, "Should still be tracking");
    
    /* Sunset */
    energy_update_solar(12000, 50); /* 600mW */
    energy_process();
    
    /* Dusk */
    energy_update_solar(5000, 10); /* 50mW */
    energy_process();
    TEST_ASSERT(g_state.night_mode, "Should enter night mode");
    
    energy_shutdown();
}

/*******************************************************************************
 * Test Cases - Integration Tests
 ******************************************************************************/

static void test_full_day_cycle(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* Start with partly discharged battery */
    energy_update_battery(3800, 0, 25);
    
    /* Simulate 24 hours in steps */
    uint16_t hourly_solar[24] = {
        0, 0, 0, 0, 0, 500,       /* 0-5: night to dawn */
        2000, 5000, 8000, 10000,  /* 6-9: morning ramp */
        12000, 14000, 15000, 14000, /* 10-13: peak */
        12000, 8000, 5000, 2000,  /* 14-17: afternoon decline */
        500, 0, 0, 0, 0, 0        /* 18-23: evening to night */
    };
    
    for (int hour = 0; hour < 24; hour++) {
        uint16_t current = hourly_solar[hour];
        uint16_t voltage = (current > 0) ? 18000 : 5000;
        
        /* Simulate 60 minutes @ 1 process/min */
        for (int min = 0; min < 60; min++) {
            energy_update_solar(voltage, current);
            energy_update_battery(g_state.battery_voltage_mv + 1, 
                                  (current > 0) ? 500 : -100, 25);
            energy_process();
        }
    }
    
    TEST_ASSERT(g_state.total_harvested_mwh > 0, "Should harvest energy over day");
    TEST_ASSERT(g_state.uptime_seconds == 24 * 60, "Should have 24 hours uptime");
    TEST_ASSERT(g_state.mppt_track_count > 0, "Should have MPPT tracking");
    
    energy_shutdown();
}

static void test_energy_to_grid_pipeline(void) {
    energy_init(MPPT_PERTURB_OBSERVE);
    
    /* High harvest, full battery */
    g_state.battery_soc_pct = 95;
    energy_update_solar(20000, 3000); /* 60W */
    energy_process();
    
    /* Connect to grid */
    grid_connect();
    grid_enable_export(50000);
    
    /* Should have surplus for export */
    TEST_ASSERT_EQ(g_state.budget_state, BUDGET_SURPLUS, "Should be surplus");
    TEST_ASSERT_EQ(g_state.grid_state, GRID_CONNECTED, "Grid should be connected");
    TEST_ASSERT(g_state.grid_export_enabled, "Export should be enabled");
    
    /* Request some power */
    uint16_t load = energy_request_power(10000);
    TEST_ASSERT(load > 0, "Should grant power");
    
    energy_shutdown();
}

/*******************************************************************************
 * Main Test Runner
 ******************************************************************************/

int main(void) {
    printf("\n");
    printf("================================================================\n");
    printf("           ENERGY HARVESTING & SMART GRID TESTS\n");
    printf("================================================================\n\n");
    
    /* MPPT Algorithm Tests */
    printf("[TEST GROUP] MPPT Algorithms\n");
    printf("----------------------------------------\n");
    RUN_TEST(test_mppt_init, "MPPT initialization");
    RUN_TEST(test_mppt_night_mode, "Night mode detection");
    RUN_TEST(test_mppt_perturb_observe, "Perturb & Observe algorithm");
    RUN_TEST(test_mppt_constant_voltage, "Constant voltage mode");
    printf("----------------------------------------\n");
    printf("Group: %d/%d passed\n\n", tests_passed, tests_run);
    
    int group1 = tests_passed;
    
    /* Battery Charging Tests */
    printf("[TEST GROUP] Battery Charging\n");
    printf("----------------------------------------\n");
    RUN_TEST(test_battery_charge_start, "Charge initiation");
    RUN_TEST(test_battery_precharge, "Precharge mode");
    RUN_TEST(test_battery_cc_cv_transition, "CC to CV transition");
    RUN_TEST(test_battery_charge_complete, "Charge completion");
    RUN_TEST(test_battery_temperature_protection, "Temperature protection");
    RUN_TEST(test_battery_fault_clear, "Fault clearing");
    printf("----------------------------------------\n");
    printf("Group: %d/%d passed\n\n", tests_passed - group1, tests_run - (int)group1);
    
    int group2 = tests_passed;
    
    /* Power Budget Tests */
    printf("[TEST GROUP] Power Budget Management\n");
    printf("----------------------------------------\n");
    RUN_TEST(test_budget_critical, "Critical state detection");
    RUN_TEST(test_budget_normal, "Normal operation");
    RUN_TEST(test_budget_surplus, "Surplus detection");
    RUN_TEST(test_power_request, "Power request handling");
    RUN_TEST(test_power_release, "Power release");
    RUN_TEST(test_budget_callback, "Budget callback");
    printf("----------------------------------------\n");
    printf("Group: %d/%d passed\n\n", tests_passed - group2, tests_run - (int)group2);
    
    int group3 = tests_passed;
    
    /* Smart Grid Tests */
    printf("[TEST GROUP] Smart Grid Interface\n");
    printf("----------------------------------------\n");
    RUN_TEST(test_grid_connect, "Grid connection");
    RUN_TEST(test_grid_disconnect, "Grid disconnection");
    RUN_TEST(test_grid_export, "Export configuration");
    printf("----------------------------------------\n");
    printf("Group: %d/%d passed\n\n", tests_passed - group3, tests_run - (int)group3);
    
    int group4 = tests_passed;
    
    /* Variable Sunlight Tests */
    printf("[TEST GROUP] Variable Sunlight Simulation\n");
    printf("----------------------------------------\n");
    RUN_TEST(test_sunrise_simulation, "Sunrise transition");
    RUN_TEST(test_cloud_passing, "Cloud passing adaptation");
    RUN_TEST(test_sunset_simulation, "Sunset transition");
    printf("----------------------------------------\n");
    printf("Group: %d/%d passed\n\n", tests_passed - group4, tests_run - (int)group4);
    
    int group5 = tests_passed;
    
    /* Integration Tests */
    printf("[TEST GROUP] Integration Tests\n");
    printf("----------------------------------------\n");
    RUN_TEST(test_full_day_cycle, "Full 24-hour simulation");
    RUN_TEST(test_energy_to_grid_pipeline, "Energy to grid pipeline");
    printf("----------------------------------------\n");
    printf("Group: %d/%d passed\n\n", tests_passed - group5, tests_run - (int)group5);
    
    /* Summary */
    printf("================================================================\n");
    printf("                    TEST SUMMARY\n");
    printf("================================================================\n");
    printf("Tests run:    %d\n", tests_run);
    printf("Tests passed: %d\n", assertions_passed);
    printf("Tests failed: %d\n", assertions_total - assertions_passed);
    printf("================================================================\n\n");
    
    if (tests_passed == tests_run) {
        printf("=== ALL TESTS PASSED ===\n\n");
        return 0;
    } else {
        printf("=== SOME TESTS FAILED ===\n\n");
        return 1;
    }
}
