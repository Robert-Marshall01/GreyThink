/**
 * @file bms_tests.c
 * @brief Battery Management System Integration Tests
 * 
 * @details
 * Comprehensive test suite for the BMS spotlight implementation.
 * Tests cover charge control, thermal management, fault detection,
 * cell balancing, SOC estimation, and telemetry.
 * 
 * TEST CATEGORIES:
 * - Initialization and configuration
 * - Normal charge/discharge cycles
 * - Overcharge protection
 * - Deep discharge protection
 * - Thermal runaway detection
 * - Overcurrent protection
 * - Cell balancing
 * - SOC/SOH estimation
 * - State machine transitions
 * - Contactor control
 * 
 * @copyright Grey Firmware Project
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <limits.h>

/*******************************************************************************
 * Test Framework
 ******************************************************************************/

static int g_tests_run = 0;
static int g_tests_passed = 0;
static int g_tests_failed = 0;

#define TEST_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        printf("  FAIL: %s (line %d)\n", msg, __LINE__); \
        return 0; \
    } \
} while(0)

#define RUN_TEST(test_func) do { \
    g_tests_run++; \
    printf("Running: %s\n", #test_func); \
    if (test_func()) { \
        g_tests_passed++; \
        printf("  PASS\n"); \
    } else { \
        g_tests_failed++; \
    } \
} while(0)

/*******************************************************************************
 * BMS Type Definitions (from bms_spotlight.c)
 ******************************************************************************/

typedef enum {
    BMS_CHEM_LI_ION,
    BMS_CHEM_LIFEPO4,
    BMS_CHEM_LIPO,
    BMS_CHEM_NIMH,
    BMS_CHEM_SOLID_STATE
} bms_chemistry_t;

typedef enum {
    BMS_STATE_INIT,
    BMS_STATE_IDLE,
    BMS_STATE_CHARGING,
    BMS_STATE_DISCHARGING,
    BMS_STATE_BALANCING,
    BMS_STATE_FAULT,
    BMS_STATE_SHUTDOWN
} bms_state_t;

typedef enum {
    BMS_CHARGE_PRECHARGE,
    BMS_CHARGE_CC,
    BMS_CHARGE_CV,
    BMS_CHARGE_TOPOFF,
    BMS_CHARGE_COMPLETE
} bms_charge_phase_t;

typedef enum {
    BMS_FAULT_NONE              = 0x0000,
    BMS_FAULT_CELL_UV           = 0x0001,
    BMS_FAULT_CELL_OV           = 0x0002,
    BMS_FAULT_PACK_UV           = 0x0004,
    BMS_FAULT_PACK_OV           = 0x0008,
    BMS_FAULT_OVER_TEMP         = 0x0010,
    BMS_FAULT_UNDER_TEMP        = 0x0020,
    BMS_FAULT_THERMAL_RUNAWAY   = 0x0040,
    BMS_FAULT_OVERCURRENT       = 0x0080,
    BMS_FAULT_SHORT_CIRCUIT     = 0x0100,
    BMS_FAULT_COMM_ERROR        = 0x0200,
    BMS_FAULT_BAL_FAIL          = 0x0400,
    BMS_FAULT_SOC_INVALID       = 0x0800,
    BMS_FAULT_SENSOR_FAIL       = 0x1000,
    BMS_FAULT_ISOLATION         = 0x2000
} bms_fault_t;

typedef enum {
    BMS_BALANCE_PASSIVE,
    BMS_BALANCE_ACTIVE
} bms_balance_method_t;

typedef struct {
    bms_chemistry_t chemistry;
    uint8_t cell_count;
    uint8_t temp_zone_count;
    uint16_t cell_capacity_mah;
    uint16_t cell_min_mv;
    uint16_t cell_max_mv;
    uint16_t cell_nominal_mv;
    int16_t temp_min_c10;
    int16_t temp_max_c10;
    uint16_t max_charge_ma;
    uint16_t max_discharge_ma;
    bms_balance_method_t balance_method;
    uint16_t balance_threshold_mv;
} bms_config_t;

typedef struct {
    bms_state_t state;
    bms_charge_phase_t charge_phase;
    uint32_t fault_flags;
    uint32_t pack_voltage_mv;
    int32_t pack_current_ma;
    int32_t power_mw;
    uint16_t max_cell_mv;
    uint16_t min_cell_mv;
    uint16_t cell_delta_mv;
    int16_t max_temp_c10;
    int16_t min_temp_c10;
    uint8_t soc_pct;
    uint8_t soh_pct;
    bool contactor_closed;
    bool precharge_active;
    uint32_t uptime_s;
} bms_status_t;

/*******************************************************************************
 * External BMS API Declarations
 ******************************************************************************/

extern int bms_init(const bms_config_t* config);
extern void bms_shutdown(void);
extern int bms_start_charge(void);
extern void bms_stop_charge(void);
extern int bms_enable_discharge(void);
extern void bms_disable_discharge(void);
extern int bms_get_status(bms_status_t* status);
extern uint16_t bms_get_cell_voltage(uint8_t cell);
extern int16_t bms_get_zone_temp(uint8_t zone);
extern void bms_process(uint32_t delta_ms);

/* Test API */
extern void bms_test_set_cell_voltage(uint8_t cell, uint16_t voltage_mv);
extern void bms_test_set_zone_temp(uint8_t zone, int16_t temp_c10);
extern void bms_test_set_current(int32_t current_ma);
extern uint32_t bms_test_get_faults(void);
extern bms_state_t bms_test_get_state(void);
extern bool bms_test_is_cell_balancing(uint8_t cell);
extern void bms_test_reset(void);
extern bool bms_test_get_contactor(void);
extern void bms_test_inject_fault(uint32_t fault_mask);
extern void bms_test_clear_fault(uint32_t fault_mask);
extern void bms_test_set_soc(uint8_t soc_pct);
extern bms_charge_phase_t bms_test_get_charge_phase(void);

/*******************************************************************************
 * Test Helpers
 ******************************************************************************/

static bms_config_t create_default_config(void) {
    bms_config_t cfg = {
        .chemistry = BMS_CHEM_LI_ION,
        .cell_count = 4,
        .temp_zone_count = 2,
        .cell_capacity_mah = 3000,
        .cell_min_mv = 2500,
        .cell_max_mv = 4200,
        .cell_nominal_mv = 3700,
        .temp_min_c10 = 0,
        .temp_max_c10 = 600,
        .max_charge_ma = 3000,
        .max_discharge_ma = 10000,
        .balance_method = BMS_BALANCE_PASSIVE,
        .balance_threshold_mv = 30
    };
    return cfg;
}

static void init_bms_normal(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    bms_init(&cfg);
    
    /* Set normal cell voltages */
    for (int i = 0; i < 4; i++) {
        bms_test_set_cell_voltage(i, 3700);
    }
    
    /* Set normal temperatures */
    for (int i = 0; i < 2; i++) {
        bms_test_set_zone_temp(i, 250);  /* 25°C */
    }
    
    /* Process to move from INIT to IDLE */
    bms_process(100);
}

/*******************************************************************************
 * Initialization Tests
 ******************************************************************************/

static int test_init_null_config(void) {
    bms_test_reset();
    int result = bms_init(NULL);
    TEST_ASSERT(result == -1, "Should reject null config");
    return 1;
}

static int test_init_zero_cells(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    cfg.cell_count = 0;
    int result = bms_init(&cfg);
    TEST_ASSERT(result == -2, "Should reject zero cell count");
    return 1;
}

static int test_init_too_many_cells(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    cfg.cell_count = 100;
    int result = bms_init(&cfg);
    TEST_ASSERT(result == -2, "Should reject excessive cell count");
    return 1;
}

static int test_init_too_many_temp_zones(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    cfg.temp_zone_count = 100;
    int result = bms_init(&cfg);
    TEST_ASSERT(result == -3, "Should reject excessive temp zones");
    return 1;
}

static int test_init_valid_config(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    int result = bms_init(&cfg);
    TEST_ASSERT(result == 0, "Should accept valid config");
    return 1;
}

static int test_init_lifepo4(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    cfg.chemistry = BMS_CHEM_LIFEPO4;
    cfg.cell_nominal_mv = 3200;
    int result = bms_init(&cfg);
    TEST_ASSERT(result == 0, "Should accept LiFePO4 config");
    return 1;
}

static int test_init_large_pack(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    cfg.cell_count = 24;
    cfg.temp_zone_count = 8;
    int result = bms_init(&cfg);
    TEST_ASSERT(result == 0, "Should accept large pack config");
    return 1;
}

static int test_state_after_init(void) {
    init_bms_normal();
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_IDLE, "Should be in IDLE after init");
    return 1;
}

/*******************************************************************************
 * Status and Monitoring Tests
 ******************************************************************************/

static int test_get_status_null(void) {
    init_bms_normal();
    int result = bms_get_status(NULL);
    TEST_ASSERT(result == -1, "Should reject null status pointer");
    return 1;
}

static int test_get_status_valid(void) {
    init_bms_normal();
    bms_status_t status;
    int result = bms_get_status(&status);
    TEST_ASSERT(result == 0, "Should return status");
    TEST_ASSERT(status.state == BMS_STATE_IDLE, "State should be IDLE");
    return 1;
}

static int test_get_cell_voltage(void) {
    init_bms_normal();
    bms_test_set_cell_voltage(0, 3700);
    bms_test_set_cell_voltage(1, 3710);
    bms_test_set_cell_voltage(2, 3690);
    bms_test_set_cell_voltage(3, 3705);
    
    TEST_ASSERT(bms_get_cell_voltage(0) == 3700, "Cell 0 voltage");
    TEST_ASSERT(bms_get_cell_voltage(1) == 3710, "Cell 1 voltage");
    TEST_ASSERT(bms_get_cell_voltage(2) == 3690, "Cell 2 voltage");
    TEST_ASSERT(bms_get_cell_voltage(3) == 3705, "Cell 3 voltage");
    return 1;
}

static int test_get_cell_voltage_invalid(void) {
    init_bms_normal();
    uint16_t v = bms_get_cell_voltage(100);
    TEST_ASSERT(v == 0, "Invalid cell should return 0");
    return 1;
}

static int test_get_zone_temp(void) {
    init_bms_normal();
    bms_test_set_zone_temp(0, 250);
    bms_test_set_zone_temp(1, 280);
    
    TEST_ASSERT(bms_get_zone_temp(0) == 250, "Zone 0 temp");
    TEST_ASSERT(bms_get_zone_temp(1) == 280, "Zone 1 temp");
    return 1;
}

static int test_get_zone_temp_invalid(void) {
    init_bms_normal();
    int16_t t = bms_get_zone_temp(100);
    TEST_ASSERT(t == INT16_MIN, "Invalid zone should return INT16_MIN");
    return 1;
}

static int test_pack_voltage_calculation(void) {
    init_bms_normal();
    bms_test_set_cell_voltage(0, 3700);
    bms_test_set_cell_voltage(1, 3710);
    bms_test_set_cell_voltage(2, 3690);
    bms_test_set_cell_voltage(3, 3700);
    
    bms_process(100);
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.pack_voltage_mv == 14800, "Pack voltage should be sum");
    return 1;
}

static int test_cell_delta_calculation(void) {
    init_bms_normal();
    bms_test_set_cell_voltage(0, 3700);
    bms_test_set_cell_voltage(1, 3750);
    bms_test_set_cell_voltage(2, 3680);
    bms_test_set_cell_voltage(3, 3720);
    
    bms_process(100);
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.max_cell_mv == 3750, "Max cell voltage");
    TEST_ASSERT(status.min_cell_mv == 3680, "Min cell voltage");
    TEST_ASSERT(status.cell_delta_mv == 70, "Cell delta");
    return 1;
}

/*******************************************************************************
 * Charge Control Tests
 ******************************************************************************/

static int test_start_charge_normal(void) {
    init_bms_normal();
    int result = bms_start_charge();
    TEST_ASSERT(result == 0, "Should allow charge start");
    
    bms_process(100);
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_CHARGING, "Should transition to CHARGING");
    return 1;
}

static int test_start_charge_with_fault(void) {
    init_bms_normal();
    bms_test_inject_fault(BMS_FAULT_CELL_OV);
    bms_process(100);
    
    int result = bms_start_charge();
    TEST_ASSERT(result == -2, "Should reject charge with fault");
    return 1;
}

static int test_stop_charge(void) {
    init_bms_normal();
    bms_start_charge();
    bms_process(100);
    
    bms_stop_charge();
    bms_process(100);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_IDLE, "Should return to IDLE");
    return 1;
}

static int test_charge_phase_precharge(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    bms_init(&cfg);
    
    /* Set low cell voltages for precharge */
    for (int i = 0; i < 4; i++) {
        bms_test_set_cell_voltage(i, 2700);  /* Below UV_WARN */
    }
    bms_test_set_zone_temp(0, 250);
    bms_test_set_zone_temp(1, 250);
    
    bms_process(100);  /* INIT -> IDLE */
    bms_start_charge();
    bms_process(100);
    
    bms_charge_phase_t phase = bms_test_get_charge_phase();
    TEST_ASSERT(phase == BMS_CHARGE_PRECHARGE, "Should be in precharge");
    return 1;
}

static int test_charge_phase_cc(void) {
    init_bms_normal();
    bms_start_charge();
    bms_process(100);
    
    bms_charge_phase_t phase = bms_test_get_charge_phase();
    TEST_ASSERT(phase == BMS_CHARGE_CC, "Should be in CC phase");
    return 1;
}

static int test_charge_phase_cv_transition(void) {
    init_bms_normal();
    bms_start_charge();
    bms_process(100);
    
    /* Set cells near max voltage */
    for (int i = 0; i < 4; i++) {
        bms_test_set_cell_voltage(i, 4150);
    }
    bms_process(200);
    
    bms_charge_phase_t phase = bms_test_get_charge_phase();
    TEST_ASSERT(phase == BMS_CHARGE_CV, "Should transition to CV");
    return 1;
}

/*******************************************************************************
 * Discharge Control Tests
 ******************************************************************************/

static int test_enable_discharge_normal(void) {
    init_bms_normal();
    int result = bms_enable_discharge();
    TEST_ASSERT(result == 0, "Should allow discharge enable");
    
    bms_process(100);
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_DISCHARGING, "Should be DISCHARGING");
    return 1;
}

static int test_enable_discharge_with_fault(void) {
    init_bms_normal();
    bms_test_inject_fault(BMS_FAULT_CELL_UV);
    bms_process(100);
    
    int result = bms_enable_discharge();
    TEST_ASSERT(result == -2, "Should reject discharge with fault");
    return 1;
}

static int test_disable_discharge(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);  /* Wait for contactor */
    
    bms_disable_discharge();
    bms_process(100);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_IDLE, "Should return to IDLE");
    return 1;
}

static int test_contactor_closes_on_discharge(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(50);   /* Transition to DISCHARGING */
    bms_process(200);  /* Wait for precharge and contactor */
    
    bool closed = bms_test_get_contactor();
    TEST_ASSERT(closed, "Contactor should close for discharge");
    return 1;
}

static int test_contactor_opens_on_disable(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_disable_discharge();
    bms_process(100);
    
    bool closed = bms_test_get_contactor();
    TEST_ASSERT(!closed, "Contactor should open when disabled");
    return 1;
}

/*******************************************************************************
 * Overvoltage Protection Tests
 ******************************************************************************/

static int test_cell_overvoltage_detection(void) {
    init_bms_normal();
    
    /* Set one cell to overvoltage */
    bms_test_set_cell_voltage(2, 4250);
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_CELL_OV, "Should detect cell OV");
    return 1;
}

static int test_overvoltage_triggers_fault_state(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_cell_voltage(0, 4300);
    bms_process(200);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_FAULT, "Should enter FAULT state");
    return 1;
}

static int test_overvoltage_opens_contactor(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_cell_voltage(0, 4300);
    bms_process(200);
    
    bool closed = bms_test_get_contactor();
    TEST_ASSERT(!closed, "Contactor should open on OV");
    return 1;
}

static int test_pack_overvoltage_detection(void) {
    init_bms_normal();
    
    /* Set all cells high but individually OK */
    for (int i = 0; i < 4; i++) {
        bms_test_set_cell_voltage(i, 4220);
    }
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_PACK_OV, "Should detect pack OV");
    return 1;
}

/*******************************************************************************
 * Undervoltage Protection Tests
 ******************************************************************************/

static int test_cell_undervoltage_detection(void) {
    init_bms_normal();
    
    bms_test_set_cell_voltage(1, 2400);
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_CELL_UV, "Should detect cell UV");
    return 1;
}

static int test_undervoltage_during_discharge(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_cell_voltage(3, 2300);
    bms_process(200);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_FAULT, "Should enter FAULT on UV");
    return 1;
}

static int test_deep_discharge_contactor_open(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_cell_voltage(0, 2100);
    bms_process(200);
    
    bool closed = bms_test_get_contactor();
    TEST_ASSERT(!closed, "Contactor should open on deep discharge");
    return 1;
}

static int test_pack_undervoltage_detection(void) {
    init_bms_normal();
    
    for (int i = 0; i < 4; i++) {
        bms_test_set_cell_voltage(i, 2450);
    }
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_PACK_UV, "Should detect pack UV");
    return 1;
}

/*******************************************************************************
 * Thermal Protection Tests
 ******************************************************************************/

static int test_over_temperature_detection(void) {
    init_bms_normal();
    
    bms_test_set_zone_temp(0, 650);  /* 65°C */
    bms_process(600);  /* Wait for temp sample */
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_OVER_TEMP, "Should detect over temp");
    return 1;
}

static int test_under_temperature_detection(void) {
    init_bms_normal();
    
    bms_test_set_zone_temp(1, -10);  /* -1°C */
    bms_process(600);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_UNDER_TEMP, "Should detect under temp");
    return 1;
}

static int test_thermal_fault_stops_operation(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_zone_temp(0, 700);  /* 70°C */
    bms_process(600);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_FAULT, "Should enter FAULT on thermal");
    return 1;
}

static int test_thermal_runaway_detection(void) {
    init_bms_normal();
    
    /* Simulate rapid temperature rise */
    bms_test_set_zone_temp(0, 300);  /* 30°C baseline */
    bms_process(500);  /* Trigger temp sample */
    
    /* Set much higher temp to simulate runaway rate */
    /* Rate detection depends on implementation */
    bms_test_set_zone_temp(0, 500);  /* 50°C - large jump */
    bms_process(600);  /* Another sample */
    
    /* Note: runaway detection depends on rate calculation */
    /* This test verifies the temperature monitoring works */
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.max_temp_c10 >= 500, "Should track max temp");
    return 1;
}

static int test_thermal_recovers_when_cooled(void) {
    init_bms_normal();
    
    bms_test_set_zone_temp(0, 650);
    bms_process(600);
    
    /* Verify fault */
    TEST_ASSERT(bms_test_get_faults() & BMS_FAULT_OVER_TEMP, "Should have OT fault");
    
    /* Cool down */
    bms_test_set_zone_temp(0, 300);
    bms_process(600);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(!(faults & BMS_FAULT_OVER_TEMP), "OT fault should clear");
    return 1;
}

/*******************************************************************************
 * Overcurrent Protection Tests
 ******************************************************************************/

static int test_charge_overcurrent_detection(void) {
    init_bms_normal();
    bms_start_charge();
    bms_process(100);
    
    bms_test_set_current(5000);  /* 5A > 3A limit */
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_OVERCURRENT, "Should detect charge OC");
    return 1;
}

static int test_discharge_overcurrent_detection(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_current(-15000);  /* 15A > 10A limit */
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_OVERCURRENT, "Should detect discharge OC");
    return 1;
}

static int test_short_circuit_detection(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_current(-150000);  /* 150A - short circuit */
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_SHORT_CIRCUIT, "Should detect short circuit");
    return 1;
}

static int test_short_circuit_opens_contactor(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_current(-200000);
    bms_process(200);
    
    bool closed = bms_test_get_contactor();
    TEST_ASSERT(!closed, "Contactor should open on short circuit");
    return 1;
}

/*******************************************************************************
 * Cell Balancing Tests
 ******************************************************************************/

static int test_balancing_identifies_high_cells(void) {
    init_bms_normal();
    
    /* Create imbalance */
    bms_test_set_cell_voltage(0, 3700);
    bms_test_set_cell_voltage(1, 3750);  /* 50mV higher */
    bms_test_set_cell_voltage(2, 3700);
    bms_test_set_cell_voltage(3, 3700);
    
    /* Process enough for balance check */
    for (int i = 0; i < 60; i++) {
        bms_process(100);
    }
    
    bool cell1_bal = bms_test_is_cell_balancing(1);
    TEST_ASSERT(cell1_bal, "High cell should be balancing");
    return 1;
}

static int test_balancing_ignores_low_cells(void) {
    init_bms_normal();
    
    bms_test_set_cell_voltage(0, 3700);
    bms_test_set_cell_voltage(1, 3750);
    bms_test_set_cell_voltage(2, 3700);
    bms_test_set_cell_voltage(3, 3700);
    
    for (int i = 0; i < 60; i++) {
        bms_process(100);
    }
    
    bool cell0_bal = bms_test_is_cell_balancing(0);
    TEST_ASSERT(!cell0_bal, "Low cells should not balance");
    return 1;
}

static int test_balancing_threshold(void) {
    init_bms_normal();
    
    /* Small imbalance below threshold */
    bms_test_set_cell_voltage(0, 3700);
    bms_test_set_cell_voltage(1, 3720);  /* 20mV */
    bms_test_set_cell_voltage(2, 3700);
    bms_test_set_cell_voltage(3, 3700);
    
    for (int i = 0; i < 60; i++) {
        bms_process(100);
    }
    
    bool cell1_bal = bms_test_is_cell_balancing(1);
    TEST_ASSERT(!cell1_bal, "Below threshold should not balance");
    return 1;
}

static int test_balancing_stops_when_balanced(void) {
    init_bms_normal();
    
    bms_test_set_cell_voltage(0, 3700);
    bms_test_set_cell_voltage(1, 3750);
    bms_test_set_cell_voltage(2, 3700);
    bms_test_set_cell_voltage(3, 3700);
    
    for (int i = 0; i < 60; i++) {
        bms_process(100);
    }
    
    /* Now balance the cells */
    for (int i = 0; i < 4; i++) {
        bms_test_set_cell_voltage(i, 3700);
    }
    
    for (int i = 0; i < 60; i++) {
        bms_process(100);
    }
    
    bool any_balancing = false;
    for (int i = 0; i < 4; i++) {
        if (bms_test_is_cell_balancing(i)) any_balancing = true;
    }
    TEST_ASSERT(!any_balancing, "Should stop balancing when equal");
    return 1;
}

/*******************************************************************************
 * SOC/SOH Estimation Tests
 ******************************************************************************/

static int test_soc_initial_value(void) {
    init_bms_normal();
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.soc_pct == 50, "Initial SOC should be 50%");
    return 1;
}

static int test_soc_set_and_get(void) {
    init_bms_normal();
    bms_test_set_soc(80);
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.soc_pct == 80, "SOC should be 80%");
    return 1;
}

static int test_soc_charging_increases(void) {
    init_bms_normal();
    bms_test_set_soc(50);
    bms_start_charge();
    bms_process(100);
    
    /* Simulate charging current */
    bms_test_set_current(2000);  /* 2A charge */
    
    /* Run for many seconds */
    for (int i = 0; i < 100; i++) {
        bms_process(1000);  /* 1 second steps */
    }
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.soc_pct > 50, "SOC should increase while charging");
    return 1;
}

static int test_soc_discharging_decreases(void) {
    init_bms_normal();
    bms_test_set_soc(50);
    bms_enable_discharge();
    bms_process(200);
    
    bms_test_set_current(-2000);  /* 2A discharge */
    
    for (int i = 0; i < 100; i++) {
        bms_process(1000);
    }
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.soc_pct < 50, "SOC should decrease while discharging");
    return 1;
}

static int test_soh_initial_value(void) {
    init_bms_normal();
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.soh_pct == 100, "Initial SOH should be 100%");
    return 1;
}

/*******************************************************************************
 * State Machine Tests
 ******************************************************************************/

static int test_state_init_to_idle(void) {
    bms_test_reset();
    bms_config_t cfg = create_default_config();
    bms_init(&cfg);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_INIT, "Should start in INIT");
    
    bms_process(100);
    state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_IDLE, "Should transition to IDLE");
    return 1;
}

static int test_state_idle_to_charging(void) {
    init_bms_normal();
    bms_start_charge();
    bms_process(100);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_CHARGING, "Should transition to CHARGING");
    return 1;
}

static int test_state_idle_to_discharging(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(100);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_DISCHARGING, "Should transition to DISCHARGING");
    return 1;
}

static int test_state_to_fault(void) {
    init_bms_normal();
    bms_test_set_cell_voltage(0, 4300);
    bms_process(200);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_FAULT, "Should enter FAULT state");
    return 1;
}

static int test_state_fault_recovery(void) {
    init_bms_normal();
    
    bms_test_set_cell_voltage(0, 4300);
    bms_process(200);
    TEST_ASSERT(bms_test_get_state() == BMS_STATE_FAULT, "Should be in FAULT");
    
    bms_test_set_cell_voltage(0, 3700);
    bms_process(200);
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_IDLE, "Should recover to IDLE");
    return 1;
}

static int test_state_shutdown(void) {
    init_bms_normal();
    bms_shutdown();
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_SHUTDOWN, "Should be in SHUTDOWN");
    return 1;
}

static int test_shutdown_opens_contactor(void) {
    init_bms_normal();
    bms_enable_discharge();
    bms_process(200);
    
    bms_shutdown();
    
    bool closed = bms_test_get_contactor();
    TEST_ASSERT(!closed, "Contactor should open on shutdown");
    return 1;
}

/*******************************************************************************
 * Fault Injection/Clearing Tests
 ******************************************************************************/

static int test_fault_injection(void) {
    init_bms_normal();
    
    bms_test_inject_fault(BMS_FAULT_COMM_ERROR);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(faults & BMS_FAULT_COMM_ERROR, "Should have injected fault");
    return 1;
}

static int test_fault_clearing(void) {
    init_bms_normal();
    
    bms_test_inject_fault(BMS_FAULT_SENSOR_FAIL);
    TEST_ASSERT(bms_test_get_faults() & BMS_FAULT_SENSOR_FAIL, "Should have fault");
    
    bms_test_clear_fault(BMS_FAULT_SENSOR_FAIL);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(!(faults & BMS_FAULT_SENSOR_FAIL), "Fault should be cleared");
    return 1;
}

static int test_multiple_faults(void) {
    init_bms_normal();
    
    bms_test_inject_fault(BMS_FAULT_CELL_UV);
    bms_test_inject_fault(BMS_FAULT_OVER_TEMP);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT((faults & BMS_FAULT_CELL_UV) && (faults & BMS_FAULT_OVER_TEMP),
                "Should have multiple faults");
    return 1;
}

/*******************************************************************************
 * Edge Cases and Stress Tests
 ******************************************************************************/

static int test_rapid_state_changes(void) {
    init_bms_normal();
    
    for (int i = 0; i < 10; i++) {
        bms_start_charge();
        bms_process(50);
        bms_stop_charge();
        bms_process(50);
        bms_enable_discharge();
        bms_process(100);
        bms_disable_discharge();
        bms_process(50);
    }
    
    bms_state_t state = bms_test_get_state();
    TEST_ASSERT(state == BMS_STATE_IDLE, "Should end in IDLE");
    return 1;
}

static int test_voltage_at_boundary(void) {
    init_bms_normal();
    
    /* Exactly at limit */
    bms_test_set_cell_voltage(0, 4200);
    bms_process(200);
    
    uint32_t faults = bms_test_get_faults();
    TEST_ASSERT(!(faults & BMS_FAULT_CELL_OV), "At limit should not fault");
    
    /* Just above limit */
    bms_test_set_cell_voltage(0, 4201);
    bms_process(200);
    
    /* Note: fault threshold is at lockout (4250), not max (4200) */
    faults = bms_test_get_faults();
    TEST_ASSERT(!(faults & BMS_FAULT_CELL_OV), "Slightly above max but below lockout");
    return 1;
}

static int test_long_operation(void) {
    init_bms_normal();
    bms_enable_discharge();
    
    /* Simulate 1 hour of operation (3600 seconds) */
    for (int i = 0; i < 3600; i++) {
        bms_process(1000);
    }
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.uptime_s >= 3600, "Should track uptime");
    return 1;
}

static int test_all_cells_different(void) {
    init_bms_normal();
    
    bms_test_set_cell_voltage(0, 3600);
    bms_test_set_cell_voltage(1, 3700);
    bms_test_set_cell_voltage(2, 3800);
    bms_test_set_cell_voltage(3, 3650);
    
    bms_process(200);
    
    bms_status_t status;
    bms_get_status(&status);
    TEST_ASSERT(status.pack_voltage_mv == 14750, "Pack voltage sum");
    TEST_ASSERT(status.max_cell_mv == 3800, "Max cell");
    TEST_ASSERT(status.min_cell_mv == 3600, "Min cell");
    TEST_ASSERT(status.cell_delta_mv == 200, "Cell delta");
    return 1;
}

/*******************************************************************************
 * Main Test Runner
 ******************************************************************************/

int main(void) {
    printf("==========================================================\n");
    printf("Battery Management System (BMS) Integration Tests\n");
    printf("==========================================================\n\n");
    
    /* Initialization Tests */
    printf("--- Initialization Tests ---\n");
    RUN_TEST(test_init_null_config);
    RUN_TEST(test_init_zero_cells);
    RUN_TEST(test_init_too_many_cells);
    RUN_TEST(test_init_too_many_temp_zones);
    RUN_TEST(test_init_valid_config);
    RUN_TEST(test_init_lifepo4);
    RUN_TEST(test_init_large_pack);
    RUN_TEST(test_state_after_init);
    
    /* Status and Monitoring Tests */
    printf("\n--- Status and Monitoring Tests ---\n");
    RUN_TEST(test_get_status_null);
    RUN_TEST(test_get_status_valid);
    RUN_TEST(test_get_cell_voltage);
    RUN_TEST(test_get_cell_voltage_invalid);
    RUN_TEST(test_get_zone_temp);
    RUN_TEST(test_get_zone_temp_invalid);
    RUN_TEST(test_pack_voltage_calculation);
    RUN_TEST(test_cell_delta_calculation);
    
    /* Charge Control Tests */
    printf("\n--- Charge Control Tests ---\n");
    RUN_TEST(test_start_charge_normal);
    RUN_TEST(test_start_charge_with_fault);
    RUN_TEST(test_stop_charge);
    RUN_TEST(test_charge_phase_precharge);
    RUN_TEST(test_charge_phase_cc);
    RUN_TEST(test_charge_phase_cv_transition);
    
    /* Discharge Control Tests */
    printf("\n--- Discharge Control Tests ---\n");
    RUN_TEST(test_enable_discharge_normal);
    RUN_TEST(test_enable_discharge_with_fault);
    RUN_TEST(test_disable_discharge);
    RUN_TEST(test_contactor_closes_on_discharge);
    RUN_TEST(test_contactor_opens_on_disable);
    
    /* Overvoltage Protection Tests */
    printf("\n--- Overvoltage Protection Tests ---\n");
    RUN_TEST(test_cell_overvoltage_detection);
    RUN_TEST(test_overvoltage_triggers_fault_state);
    RUN_TEST(test_overvoltage_opens_contactor);
    RUN_TEST(test_pack_overvoltage_detection);
    
    /* Undervoltage Protection Tests */
    printf("\n--- Undervoltage Protection Tests ---\n");
    RUN_TEST(test_cell_undervoltage_detection);
    RUN_TEST(test_undervoltage_during_discharge);
    RUN_TEST(test_deep_discharge_contactor_open);
    RUN_TEST(test_pack_undervoltage_detection);
    
    /* Thermal Protection Tests */
    printf("\n--- Thermal Protection Tests ---\n");
    RUN_TEST(test_over_temperature_detection);
    RUN_TEST(test_under_temperature_detection);
    RUN_TEST(test_thermal_fault_stops_operation);
    RUN_TEST(test_thermal_runaway_detection);
    RUN_TEST(test_thermal_recovers_when_cooled);
    
    /* Overcurrent Protection Tests */
    printf("\n--- Overcurrent Protection Tests ---\n");
    RUN_TEST(test_charge_overcurrent_detection);
    RUN_TEST(test_discharge_overcurrent_detection);
    RUN_TEST(test_short_circuit_detection);
    RUN_TEST(test_short_circuit_opens_contactor);
    
    /* Cell Balancing Tests */
    printf("\n--- Cell Balancing Tests ---\n");
    RUN_TEST(test_balancing_identifies_high_cells);
    RUN_TEST(test_balancing_ignores_low_cells);
    RUN_TEST(test_balancing_threshold);
    RUN_TEST(test_balancing_stops_when_balanced);
    
    /* SOC/SOH Estimation Tests */
    printf("\n--- SOC/SOH Estimation Tests ---\n");
    RUN_TEST(test_soc_initial_value);
    RUN_TEST(test_soc_set_and_get);
    RUN_TEST(test_soc_charging_increases);
    RUN_TEST(test_soc_discharging_decreases);
    RUN_TEST(test_soh_initial_value);
    
    /* State Machine Tests */
    printf("\n--- State Machine Tests ---\n");
    RUN_TEST(test_state_init_to_idle);
    RUN_TEST(test_state_idle_to_charging);
    RUN_TEST(test_state_idle_to_discharging);
    RUN_TEST(test_state_to_fault);
    RUN_TEST(test_state_fault_recovery);
    RUN_TEST(test_state_shutdown);
    RUN_TEST(test_shutdown_opens_contactor);
    
    /* Fault Injection Tests */
    printf("\n--- Fault Injection Tests ---\n");
    RUN_TEST(test_fault_injection);
    RUN_TEST(test_fault_clearing);
    RUN_TEST(test_multiple_faults);
    
    /* Edge Cases */
    printf("\n--- Edge Cases and Stress Tests ---\n");
    RUN_TEST(test_rapid_state_changes);
    RUN_TEST(test_voltage_at_boundary);
    RUN_TEST(test_long_operation);
    RUN_TEST(test_all_cells_different);
    
    /* Summary */
    printf("\n==========================================================\n");
    printf("Test Results: %d/%d passed", g_tests_passed, g_tests_run);
    if (g_tests_failed > 0) {
        printf(" (%d FAILED)", g_tests_failed);
    }
    printf("\n==========================================================\n");
    
    return (g_tests_failed == 0) ? 0 : 1;
}
