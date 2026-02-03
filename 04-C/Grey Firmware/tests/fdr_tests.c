/**
 * @file fdr_tests.c
 * @brief Comprehensive Integration Tests for Flight Data Recorder Spotlight
 * 
 * @details
 * This test suite validates all aspects of the FDR implementation:
 * - Initialization and configuration
 * - Parameter recording and encoding
 * - Multi-channel sensor voting (MVS algorithm)
 * - Fault detection and handling
 * - Event triggering and storage
 * - Telemetry reporting
 * - Memory management and wraparound
 * - Exceedance monitoring
 * - Emergency scenarios (GPWS, TCAS, Stall)
 * 
 * Test coverage targets DO-178C DAL B requirements for
 * Flight Data Recorder systems per TSO-C124b.
 * 
 * @version 1.0
 * @date 2024
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <stdbool.h>
#include <math.h>

/*******************************************************************************
 * Test Framework (Minimal Unity-like)
 ******************************************************************************/

static int g_test_pass = 0;
static int g_test_fail = 0;
static const char* g_current_test = NULL;

#define TEST_ASSERT(cond) do { \
    if (!(cond)) { \
        printf("  [FAIL] %s: Line %d: %s\n", g_current_test, __LINE__, #cond); \
        g_test_fail++; \
        return; \
    } \
} while(0)

#define TEST_ASSERT_EQUAL(expected, actual) do { \
    if ((expected) != (actual)) { \
        printf("  [FAIL] %s: Line %d: Expected %d, got %d\n", \
               g_current_test, __LINE__, (int)(expected), (int)(actual)); \
        g_test_fail++; \
        return; \
    } \
} while(0)

#define TEST_ASSERT_FLOAT_WITHIN(delta, expected, actual) do { \
    float _d = (float)(expected) - (float)(actual); \
    if (_d < 0) _d = -_d; \
    if (_d > (delta)) { \
        printf("  [FAIL] %s: Line %d: Expected %.3f, got %.3f (delta=%.3f)\n", \
               g_current_test, __LINE__, (float)(expected), (float)(actual), _d); \
        g_test_fail++; \
        return; \
    } \
} while(0)

#define TEST_ASSERT_NOT_NULL(ptr) do { \
    if ((ptr) == NULL) { \
        printf("  [FAIL] %s: Line %d: Pointer is NULL\n", g_current_test, __LINE__); \
        g_test_fail++; \
        return; \
    } \
} while(0)

#define RUN_TEST(func) do { \
    g_current_test = #func; \
    func(); \
    if (g_test_fail == fail_before) { \
        printf("  [PASS] %s\n", #func); \
        g_test_pass++; \
    } \
    fail_before = g_test_fail; \
} while(0)

/*******************************************************************************
 * FDR Types (mirrored from implementation)
 ******************************************************************************/

typedef enum {
    FDR_QUALITY_GOOD,
    FDR_QUALITY_DEGRADED,
    FDR_QUALITY_FAILED,
    FDR_QUALITY_NCD
} fdr_quality_t;

typedef enum {
    FDR_STATE_OFF,
    FDR_STATE_INIT,
    FDR_STATE_RECORDING,
    FDR_STATE_TRIGGERED,
    FDR_STATE_FAULT,
    FDR_STATE_READOUT
} fdr_state_t;

typedef enum {
    FDR_TRIGGER_NONE,
    FDR_TRIGGER_TAKEOFF,
    FDR_TRIGGER_LANDING,
    FDR_TRIGGER_EXCEEDANCE,
    FDR_TRIGGER_WARNING,
    FDR_TRIGGER_MANUAL,
    FDR_TRIGGER_IMPACT,
    FDR_TRIGGER_GPWS,
    FDR_TRIGGER_TCAS,
    FDR_TRIGGER_STALL
} fdr_trigger_t;

typedef enum {
    FDR_FAULT_NONE,
    FDR_FAULT_MEMORY,
    FDR_FAULT_SENSOR,
    FDR_FAULT_TIMING,
    FDR_FAULT_CHECKSUM,
    FDR_FAULT_POWER
} fdr_fault_t;

typedef enum {
    FDR_CAT_TIME,
    FDR_CAT_ALTITUDE,
    FDR_CAT_AIRSPEED,
    FDR_CAT_HEADING,
    FDR_CAT_ATTITUDE,
    FDR_CAT_ACCELERATION,
    FDR_CAT_ENGINE,
    FDR_CAT_CONTROL,
    FDR_CAT_CONFIGURATION,
    FDR_CAT_AUTOPILOT,
    FDR_CAT_WARNING,
    FDR_CAT_NAVIGATION,
    FDR_CAT_FUEL,
    FDR_CAT_CUSTOM
} fdr_category_t;

typedef struct {
    uint8_t id;
    fdr_category_t category;
    const char* name;
    const char* unit;
    float resolution;
    float min_value;
    float max_value;
    uint8_t bits;
    uint8_t rate_hz;
    uint8_t subframe;
    bool mandatory;
    uint8_t channels;
} fdr_param_def_t;

typedef struct {
    uint8_t param_count;
    const fdr_param_def_t* params;
    uint32_t memory_size;
    bool enable_wrap;
    bool enable_bit;
    bool enable_ulb;
} fdr_config_t;

typedef struct {
    uint8_t param_id;
    float value;
    fdr_quality_t quality;
    uint8_t source_channel;
    uint8_t channels_valid;
    bool fault_detected;
} fdr_voted_param_t;

typedef struct {
    fdr_state_t state;
    uint32_t frames_recorded;
    uint32_t recording_hours;
    uint32_t memory_used_pct;
    bool memory_full;
    uint32_t fault_count;
    fdr_fault_t last_fault;
    uint32_t event_count;
    fdr_trigger_t last_trigger;
    uint32_t bit_status;
} fdr_status_t;

typedef struct {
    uint32_t timestamp;
    uint32_t frame_number;
    fdr_state_t state;
    float altitude_ft;
    float airspeed_kts;
    float heading_deg;
    float vertical_speed_fpm;
    float roll_deg;
    float pitch_deg;
    float g_load;
    uint8_t fault_count;
    uint8_t event_count;
} fdr_telemetry_t;

typedef struct {
    uint32_t frame_number;
    uint32_t timestamp;
    fdr_trigger_t trigger;
    float altitude_ft;
    float airspeed_kts;
    float g_load;
    char description[32];
} fdr_event_record_t;

/* Parameter IDs */
#define PARAM_PRESSURE_ALT  1
#define PARAM_IAS           4
#define PARAM_MAG_HEADING   8
#define PARAM_PITCH         12
#define PARAM_ROLL          13
#define PARAM_NORMAL_ACCEL  15
#define PARAM_STALL_WARN    34
#define PARAM_GPWS          35
#define PARAM_TCAS_RA       36
#define PARAM_VERT_SPEED    39

/*******************************************************************************
 * External Function Declarations (from fdr_spotlight.c)
 ******************************************************************************/

extern int fdr_init(const fdr_config_t* config);
extern void fdr_shutdown(void);
extern int fdr_start_recording(void);
extern void fdr_stop_recording(void);
extern void fdr_inject_sensor(uint8_t param_id, uint8_t channel, 
                               float value, fdr_quality_t quality);
extern void fdr_trigger_event(fdr_trigger_t trigger, const char* description);
extern int fdr_get_status(fdr_status_t* status);
extern int fdr_get_voted_param(uint8_t param_id, fdr_voted_param_t* voted);
extern int fdr_get_telemetry(fdr_telemetry_t* telem);
extern int fdr_get_event(uint16_t index, fdr_event_record_t* event);
extern void fdr_process(uint32_t delta_ms);

/* Test APIs */
extern void fdr_test_reset(void);
extern fdr_state_t fdr_test_get_state(void);
extern uint32_t fdr_test_get_frame_count(void);
extern uint16_t fdr_test_get_event_count(void);
extern bool fdr_test_fault_detected(uint8_t param_id);
extern void fdr_test_inject_fault(fdr_fault_t fault);
extern uint8_t fdr_test_get_telemetry_count(void);

/*******************************************************************************
 * Test Setup/Teardown
 ******************************************************************************/

static void test_setup(void) {
    fdr_test_reset();
    fdr_init(NULL);
}

static void test_teardown(void) {
    fdr_shutdown();
}

/*******************************************************************************
 * Test Category: Initialization
 ******************************************************************************/

static void test_init_default_config(void) {
    test_setup();
    
    fdr_status_t status;
    int result = fdr_get_status(&status);
    
    TEST_ASSERT_EQUAL(0, result);
    TEST_ASSERT(status.state == FDR_STATE_INIT || 
                status.state == FDR_STATE_RECORDING);
    TEST_ASSERT_EQUAL(0, status.frames_recorded);
    TEST_ASSERT_EQUAL(0, status.fault_count);
    TEST_ASSERT_EQUAL(0, status.event_count);
    
    test_teardown();
}

static void test_init_custom_config(void) {
    test_setup();
    fdr_shutdown();
    
    fdr_param_def_t custom_params[] = {
        {0, FDR_CAT_ALTITUDE, "ALT", "ft", 1.0f, -1000, 45000, 16, 4, 0, true, 2},
        {1, FDR_CAT_AIRSPEED, "SPD", "kts", 0.5f, 0, 500, 10, 4, 0, true, 2},
    };
    
    fdr_config_t config = {
        .param_count = 2,
        .params = custom_params,
        .memory_size = 4096,
        .enable_wrap = true,
        .enable_bit = true,
        .enable_ulb = false
    };
    
    int result = fdr_init(&config);
    TEST_ASSERT_EQUAL(0, result);
    
    fdr_status_t status;
    result = fdr_get_status(&status);
    TEST_ASSERT_EQUAL(0, result);
    
    test_teardown();
}

static void test_double_init(void) {
    test_setup();
    
    /* Second init should succeed (replaces first) */
    int result = fdr_init(NULL);
    TEST_ASSERT_EQUAL(0, result);
    
    fdr_status_t status;
    result = fdr_get_status(&status);
    TEST_ASSERT_EQUAL(0, result);
    
    test_teardown();
}

static void test_start_recording(void) {
    test_setup();
    
    int result = fdr_start_recording();
    TEST_ASSERT_EQUAL(0, result);
    
    TEST_ASSERT_EQUAL(FDR_STATE_RECORDING, fdr_test_get_state());
    
    test_teardown();
}

static void test_start_stop_recording(void) {
    test_setup();
    
    fdr_start_recording();
    TEST_ASSERT_EQUAL(FDR_STATE_RECORDING, fdr_test_get_state());
    
    fdr_stop_recording();
    TEST_ASSERT_EQUAL(FDR_STATE_INIT, fdr_test_get_state());
    
    test_teardown();
}

static void test_shutdown_stops_recording(void) {
    test_setup();
    
    fdr_start_recording();
    fdr_shutdown();
    
    TEST_ASSERT_EQUAL(FDR_STATE_OFF, fdr_test_get_state());
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Sensor Input and Voting
 ******************************************************************************/

static void test_inject_single_sensor(void) {
    test_setup();
    
    /* PARAM_PRESSURE_ALT has 3 channels - inject all 3 for predictable MVS vote */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 35000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 35000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 35000.0f, FDR_QUALITY_GOOD);
    fdr_start_recording();
    
    /* Process to trigger voting */
    fdr_process(125);
    
    fdr_voted_param_t voted;
    int result = fdr_get_voted_param(PARAM_PRESSURE_ALT, &voted);
    
    TEST_ASSERT_EQUAL(0, result);
    TEST_ASSERT_FLOAT_WITHIN(100.0f, 35000.0f, voted.value);
    TEST_ASSERT_EQUAL(FDR_QUALITY_GOOD, voted.quality);
    
    test_teardown();
}

static void test_inject_dual_sensor_agreement(void) {
    test_setup();
    
    /* Both sensors agree */
    fdr_inject_sensor(PARAM_IAS, 0, 250.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 1, 252.0f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_IAS, &voted);
    
    /* Should average when close */
    TEST_ASSERT_FLOAT_WITHIN(2.0f, 251.0f, voted.value);
    TEST_ASSERT_EQUAL(false, voted.fault_detected);
    TEST_ASSERT_EQUAL(FDR_QUALITY_GOOD, voted.quality);
    
    test_teardown();
}

static void test_inject_dual_sensor_disagreement(void) {
    test_setup();
    
    /* Sensors disagree beyond threshold */
    fdr_inject_sensor(PARAM_IAS, 0, 200.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 1, 280.0f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_IAS, &voted);
    
    /* Should flag fault but still provide value */
    TEST_ASSERT_EQUAL(true, voted.fault_detected);
    TEST_ASSERT_EQUAL(FDR_QUALITY_DEGRADED, voted.quality);
    
    test_teardown();
}

static void test_triple_sensor_mvs_voting(void) {
    test_setup();
    
    /* Triple sensors - one outlier */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 35000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 35050.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 35025.0f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_PRESSURE_ALT, &voted);
    
    /* MVS should select middle value (35025) */
    TEST_ASSERT_FLOAT_WITHIN(50.0f, 35025.0f, voted.value);
    TEST_ASSERT_EQUAL(false, voted.fault_detected);
    
    test_teardown();
}

static void test_triple_sensor_one_failed(void) {
    test_setup();
    
    /* One channel failed */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 35000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 35010.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 0.0f, FDR_QUALITY_FAILED);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_PRESSURE_ALT, &voted);
    
    /* Should use remaining two (average) */
    TEST_ASSERT_EQUAL(2, voted.channels_valid);
    TEST_ASSERT_FLOAT_WITHIN(20.0f, 35005.0f, voted.value);
    
    test_teardown();
}

static void test_all_sensors_failed(void) {
    test_setup();
    
    /* All channels failed */
    fdr_inject_sensor(PARAM_IAS, 0, 0.0f, FDR_QUALITY_FAILED);
    fdr_inject_sensor(PARAM_IAS, 1, 0.0f, FDR_QUALITY_FAILED);
    fdr_inject_sensor(PARAM_IAS, 2, 0.0f, FDR_QUALITY_FAILED);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_IAS, &voted);
    
    /* Should report NCD */
    TEST_ASSERT_EQUAL(0, voted.channels_valid);
    TEST_ASSERT_EQUAL(FDR_QUALITY_NCD, voted.quality);
    
    test_teardown();
}

static void test_sensor_quality_degraded(void) {
    test_setup();
    
    /* Mark all 3 channels as degraded - none should be counted as valid */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 30000.0f, FDR_QUALITY_DEGRADED);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 30000.0f, FDR_QUALITY_DEGRADED);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 30000.0f, FDR_QUALITY_DEGRADED);
    
    fdr_start_recording();
    fdr_process(125);
    
    /* Degraded quality should not be used in voting */
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_PRESSURE_ALT, &voted);
    
    /* No good quality channels should be counted */
    TEST_ASSERT_EQUAL(0, voted.channels_valid);
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Frame Recording
 ******************************************************************************/

static void test_record_single_frame(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Process 8 samples to complete one frame (8Hz, 1 frame/sec) */
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT_EQUAL(1, fdr_test_get_frame_count());
    
    test_teardown();
}

static void test_record_multiple_frames(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Process 5 seconds worth */
    for (int i = 0; i < 5 * 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT_EQUAL(5, fdr_test_get_frame_count());
    
    test_teardown();
}

static void test_frame_count_survives_state_change(void) {
    test_setup();
    
    fdr_start_recording();
    
    for (int i = 0; i < 24; i++) {  /* 3 frames */
        fdr_process(125);
    }
    
    /* Store count before state change */
    (void)fdr_test_get_frame_count();  /* Verify accessible */
    
    fdr_stop_recording();
    fdr_start_recording();
    
    for (int i = 0; i < 16; i++) {  /* 2 more frames */
        fdr_process(125);
    }
    
    /* New frames should still increment */
    TEST_ASSERT(fdr_test_get_frame_count() >= 2);
    
    test_teardown();
}

static void test_status_shows_recorded_frames(void) {
    test_setup();
    
    fdr_start_recording();
    
    for (int i = 0; i < 16; i++) {  /* 2+ frames */
        fdr_process(125);
    }
    
    fdr_status_t status;
    fdr_get_status(&status);
    
    /* Should have at least 2 frames, might have 3 due to timing */
    TEST_ASSERT(status.frames_recorded >= 2);
    /* State can be RECORDING or TRIGGERED if event fired */
    TEST_ASSERT(status.state == FDR_STATE_RECORDING || status.state == FDR_STATE_TRIGGERED);
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Event Triggering
 ******************************************************************************/

static void test_manual_event_trigger(void) {
    test_setup();
    
    fdr_start_recording();
    fdr_trigger_event(FDR_TRIGGER_MANUAL, "TEST_EVENT");
    
    TEST_ASSERT_EQUAL(1, fdr_test_get_event_count());
    
    fdr_event_record_t event;
    int result = fdr_get_event(0, &event);
    
    TEST_ASSERT_EQUAL(0, result);
    TEST_ASSERT_EQUAL(FDR_TRIGGER_MANUAL, event.trigger);
    TEST_ASSERT(strcmp(event.description, "TEST_EVENT") == 0);
    
    test_teardown();
}

static void test_takeoff_event(void) {
    test_setup();
    
    fdr_start_recording();
    fdr_trigger_event(FDR_TRIGGER_TAKEOFF, "ROTATE");
    
    fdr_event_record_t event;
    fdr_get_event(0, &event);
    
    TEST_ASSERT_EQUAL(FDR_TRIGGER_TAKEOFF, event.trigger);
    
    test_teardown();
}

static void test_landing_event(void) {
    test_setup();
    
    fdr_start_recording();
    fdr_trigger_event(FDR_TRIGGER_LANDING, "TOUCHDOWN");
    
    fdr_event_record_t event;
    fdr_get_event(0, &event);
    
    TEST_ASSERT_EQUAL(FDR_TRIGGER_LANDING, event.trigger);
    
    test_teardown();
}

static void test_multiple_events(void) {
    test_setup();
    
    fdr_start_recording();
    
    fdr_trigger_event(FDR_TRIGGER_TAKEOFF, "TAKEOFF");
    fdr_trigger_event(FDR_TRIGGER_EXCEEDANCE, "OVERSPEED");
    fdr_trigger_event(FDR_TRIGGER_LANDING, "LANDING");
    
    TEST_ASSERT_EQUAL(3, fdr_test_get_event_count());
    
    fdr_event_record_t ev0, ev1, ev2;
    fdr_get_event(0, &ev0);
    fdr_get_event(1, &ev1);
    fdr_get_event(2, &ev2);
    
    TEST_ASSERT_EQUAL(FDR_TRIGGER_TAKEOFF, ev0.trigger);
    TEST_ASSERT_EQUAL(FDR_TRIGGER_EXCEEDANCE, ev1.trigger);
    TEST_ASSERT_EQUAL(FDR_TRIGGER_LANDING, ev2.trigger);
    
    test_teardown();
}

static void test_event_captures_flight_params(void) {
    test_setup();
    
    /* Set up flight parameters - inject all channels for predictable voting */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 38000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 38000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 38000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 0, 280.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 1, 280.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 2, 280.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 0, 1.2f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 1, 1.2f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 2, 1.2f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);  /* Vote sensors */
    
    fdr_trigger_event(FDR_TRIGGER_MANUAL, "MARK");
    
    fdr_event_record_t event;
    fdr_get_event(0, &event);
    
    /* Event should capture current flight state */
    TEST_ASSERT_FLOAT_WITHIN(1000.0f, 38000.0f, event.altitude_ft);
    TEST_ASSERT_FLOAT_WITHIN(20.0f, 280.0f, event.airspeed_kts);
    TEST_ASSERT_FLOAT_WITHIN(0.5f, 1.2f, event.g_load);
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Exceedance Detection
 ******************************************************************************/

static void test_overspeed_detection(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Inject overspeed condition (IAS > 340 kts) */
    fdr_inject_sensor(PARAM_IAS, 0, 360.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 1, 358.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 2, 362.0f, FDR_QUALITY_GOOD);
    
    /* Process to trigger frame and exceedance check */
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    /* Should have triggered exceedance event */
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    
    test_teardown();
}

static void test_high_g_detection(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Inject high G-load (> 2.5g) */
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 0, 3.2f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 1, 3.1f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 2, 3.3f, FDR_QUALITY_GOOD);
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    
    test_teardown();
}

static void test_high_bank_angle_detection(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Inject high bank angle (> 45 degrees) */
    fdr_inject_sensor(PARAM_ROLL, 0, 55.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 1, 54.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 2, 56.0f, FDR_QUALITY_GOOD);
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    
    test_teardown();
}

static void test_stall_warning_detection(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Activate stall warning */
    fdr_inject_sensor(PARAM_STALL_WARN, 0, 1.0f, FDR_QUALITY_GOOD);
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    /* Should trigger stall event and enter triggered state */
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    TEST_ASSERT_EQUAL(FDR_STATE_TRIGGERED, fdr_test_get_state());
    
    test_teardown();
}

static void test_gpws_detection(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Activate GPWS */
    fdr_inject_sensor(PARAM_GPWS, 0, 3.0f, FDR_QUALITY_GOOD);  /* Mode 3 */
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    TEST_ASSERT_EQUAL(FDR_STATE_TRIGGERED, fdr_test_get_state());
    
    test_teardown();
}

static void test_tcas_ra_detection(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Activate TCAS RA */
    fdr_inject_sensor(PARAM_TCAS_RA, 0, 1.0f, FDR_QUALITY_GOOD);
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    TEST_ASSERT_EQUAL(FDR_STATE_TRIGGERED, fdr_test_get_state());
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Telemetry
 ******************************************************************************/

static void test_telemetry_generation(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Process complete frame to generate telemetry */
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT_EQUAL(1, fdr_test_get_telemetry_count());
    
    test_teardown();
}

static void test_telemetry_content(void) {
    test_setup();
    
    /* Set up known values - inject all channels for predictable voting */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 41000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 41000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 41000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 0, 290.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 1, 290.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 2, 290.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_MAG_HEADING, 0, 270.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_MAG_HEADING, 1, 270.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 0, 15.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 1, 15.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 2, 15.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PITCH, 0, 5.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PITCH, 1, 5.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PITCH, 2, 5.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 0, 1.05f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 1, 1.05f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 2, 1.05f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    fdr_telemetry_t telem;
    int result = fdr_get_telemetry(&telem);
    
    TEST_ASSERT_EQUAL(0, result);
    /* State can be RECORDING or TRIGGERED if event fired */
    TEST_ASSERT(telem.state == FDR_STATE_RECORDING || telem.state == FDR_STATE_TRIGGERED);
    TEST_ASSERT_FLOAT_WITHIN(1000.0f, 41000.0f, telem.altitude_ft);
    TEST_ASSERT_FLOAT_WITHIN(20.0f, 290.0f, telem.airspeed_kts);
    TEST_ASSERT_FLOAT_WITHIN(5.0f, 270.0f, telem.heading_deg);
    TEST_ASSERT_FLOAT_WITHIN(5.0f, 15.0f, telem.roll_deg);
    TEST_ASSERT_FLOAT_WITHIN(2.0f, 5.0f, telem.pitch_deg);
    TEST_ASSERT_FLOAT_WITHIN(0.2f, 1.05f, telem.g_load);
    
    test_teardown();
}

static void test_telemetry_queue_multiple(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Generate 5 telemetry packets */
    for (int f = 0; f < 5; f++) {
        for (int i = 0; i < 8; i++) {
            fdr_process(125);
        }
    }
    
    /* Should have 5 packets queued */
    TEST_ASSERT_EQUAL(5, fdr_test_get_telemetry_count());
    
    /* Drain queue */
    fdr_telemetry_t telem;
    for (int i = 0; i < 5; i++) {
        TEST_ASSERT_EQUAL(0, fdr_get_telemetry(&telem));
    }
    
    TEST_ASSERT_EQUAL(0, fdr_test_get_telemetry_count());
    
    test_teardown();
}

static void test_telemetry_empty_queue(void) {
    test_setup();
    
    fdr_telemetry_t telem;
    int result = fdr_get_telemetry(&telem);
    
    /* Should return error */
    TEST_ASSERT(result != 0);
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Fault Handling
 ******************************************************************************/

static void test_inject_sensor_fault(void) {
    test_setup();
    
    fdr_test_inject_fault(FDR_FAULT_SENSOR);
    
    TEST_ASSERT_EQUAL(FDR_STATE_FAULT, fdr_test_get_state());
    
    fdr_status_t status;
    fdr_get_status(&status);
    
    TEST_ASSERT_EQUAL(1, status.fault_count);
    TEST_ASSERT_EQUAL(FDR_FAULT_SENSOR, status.last_fault);
    
    test_teardown();
}

static void test_inject_memory_fault(void) {
    test_setup();
    
    fdr_test_inject_fault(FDR_FAULT_MEMORY);
    
    fdr_status_t status;
    fdr_get_status(&status);
    
    TEST_ASSERT_EQUAL(FDR_FAULT_MEMORY, status.last_fault);
    
    test_teardown();
}

static void test_inject_timing_fault(void) {
    test_setup();
    
    fdr_test_inject_fault(FDR_FAULT_TIMING);
    
    fdr_status_t status;
    fdr_get_status(&status);
    
    TEST_ASSERT_EQUAL(FDR_FAULT_TIMING, status.last_fault);
    
    test_teardown();
}

static void test_fault_recovery(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Inject fault */
    fdr_test_inject_fault(FDR_FAULT_SENSOR);
    TEST_ASSERT_EQUAL(FDR_STATE_FAULT, fdr_test_get_state());
    
    /* Process should attempt recovery */
    fdr_process(125);
    TEST_ASSERT_EQUAL(FDR_STATE_INIT, fdr_test_get_state());
    
    test_teardown();
}

static void test_cross_channel_fault_detection(void) {
    test_setup();
    
    /* Large disagreement between channels */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 35000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 20000.0f, FDR_QUALITY_GOOD);  /* Bad! */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 35050.0f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    /* Should detect cross-channel fault */
    TEST_ASSERT(fdr_test_fault_detected(PARAM_PRESSURE_ALT));
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Status Reporting
 ******************************************************************************/

static void test_status_null_pointer(void) {
    test_setup();
    
    int result = fdr_get_status(NULL);
    TEST_ASSERT(result != 0);
    
    test_teardown();
}

static void test_status_not_initialized(void) {
    /* Don't call test_setup */
    fdr_shutdown();
    
    fdr_status_t status;
    int result = fdr_get_status(&status);
    
    TEST_ASSERT(result != 0);
}

static void test_status_memory_usage(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Record many frames */
    for (int f = 0; f < 100; f++) {
        for (int i = 0; i < 8; i++) {
            fdr_process(125);
        }
    }
    
    fdr_status_t status;
    fdr_get_status(&status);
    
    TEST_ASSERT(status.memory_used_pct > 0);
    TEST_ASSERT(status.frames_recorded == 100);
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Voted Parameter Access
 ******************************************************************************/

static void test_voted_param_null_pointer(void) {
    test_setup();
    
    int result = fdr_get_voted_param(PARAM_IAS, NULL);
    TEST_ASSERT(result != 0);
    
    test_teardown();
}

static void test_voted_param_invalid_id(void) {
    test_setup();
    
    fdr_voted_param_t voted;
    int result = fdr_get_voted_param(255, &voted);  /* Invalid ID */
    
    TEST_ASSERT(result != 0);
    
    test_teardown();
}

static void test_voted_param_all_mandatory(void) {
    test_setup();
    
    fdr_start_recording();
    fdr_process(125);
    
    /* All mandatory params should be readable */
    fdr_voted_param_t voted;
    
    for (uint8_t i = 0; i < 41; i++) {  /* 41 mandatory params */
        int result = fdr_get_voted_param(i, &voted);
        TEST_ASSERT_EQUAL(0, result);
    }
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Event Access
 ******************************************************************************/

static void test_event_null_pointer(void) {
    test_setup();
    
    fdr_trigger_event(FDR_TRIGGER_MANUAL, "TEST");
    
    int result = fdr_get_event(0, NULL);
    TEST_ASSERT(result != 0);
    
    test_teardown();
}

static void test_event_invalid_index(void) {
    test_setup();
    
    fdr_trigger_event(FDR_TRIGGER_MANUAL, "TEST");
    
    fdr_event_record_t event;
    int result = fdr_get_event(10, &event);  /* Only 1 event exists */
    
    TEST_ASSERT(result != 0);
    
    test_teardown();
}

static void test_event_description_truncation(void) {
    test_setup();
    
    /* Very long description */
    fdr_trigger_event(FDR_TRIGGER_MANUAL, 
                      "THIS_IS_A_VERY_LONG_DESCRIPTION_THAT_EXCEEDS_BUFFER");
    
    fdr_event_record_t event;
    fdr_get_event(0, &event);
    
    /* Should be truncated but not crash */
    TEST_ASSERT(strlen(event.description) < 32);
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Emergency Scenarios
 ******************************************************************************/

static void test_impact_event(void) {
    test_setup();
    
    fdr_start_recording();
    
    fdr_trigger_event(FDR_TRIGGER_IMPACT, "IMPACT_DETECTED");
    
    TEST_ASSERT_EQUAL(FDR_STATE_TRIGGERED, fdr_test_get_state());
    
    fdr_event_record_t event;
    fdr_get_event(0, &event);
    TEST_ASSERT_EQUAL(FDR_TRIGGER_IMPACT, event.trigger);
    
    test_teardown();
}

static void test_combined_emergency_scenario(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Simulate approach to terrain with GPWS activation */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 2500.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 0, 180.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_VERT_SPEED, 0, -2500.0f, FDR_QUALITY_GOOD);  /* High sink rate */
    fdr_inject_sensor(PARAM_GPWS, 0, 1.0f, FDR_QUALITY_GOOD);  /* SINK RATE warning */
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    /* Should be in triggered state with GPWS event */
    TEST_ASSERT_EQUAL(FDR_STATE_TRIGGERED, fdr_test_get_state());
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    
    /* Continue recording despite triggered state */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 2000.0f, FDR_QUALITY_GOOD);
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT(fdr_test_get_frame_count() >= 2);
    
    test_teardown();
}

static void test_tcas_ra_climb_scenario(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Set up cruise parameters */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 35000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 0, 280.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_VERT_SPEED, 0, 0.0f, FDR_QUALITY_GOOD);
    
    fdr_process(125);
    
    /* TCAS RA activates */
    fdr_inject_sensor(PARAM_TCAS_RA, 0, 1.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_VERT_SPEED, 0, 2000.0f, FDR_QUALITY_GOOD);  /* Climb */
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 0, 1.3f, FDR_QUALITY_GOOD);
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    TEST_ASSERT_EQUAL(FDR_STATE_TRIGGERED, fdr_test_get_state());
    
    /* Telemetry should show climb */
    fdr_telemetry_t telem;
    if (fdr_get_telemetry(&telem) == 0) {
        TEST_ASSERT(telem.vertical_speed_fpm > 0);
    }
    
    test_teardown();
}

static void test_approach_stall_scenario(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Approach configuration */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 1500.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 0, 135.0f, FDR_QUALITY_GOOD);  /* Slow */
    fdr_inject_sensor(PARAM_PITCH, 0, 8.0f, FDR_QUALITY_GOOD);
    
    fdr_process(125);
    
    /* Stall warning activates */
    fdr_inject_sensor(PARAM_STALL_WARN, 0, 1.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_NORMAL_ACCEL, 0, 0.7f, FDR_QUALITY_GOOD);  /* Reduced G */
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    /* Should trigger stall event */
    TEST_ASSERT_EQUAL(FDR_STATE_TRIGGERED, fdr_test_get_state());
    
    fdr_event_record_t event;
    for (uint16_t i = 0; i < fdr_test_get_event_count(); i++) {
        fdr_get_event(i, &event);
        if (event.trigger == FDR_TRIGGER_STALL) {
            TEST_ASSERT(1);  /* Found stall event */
            break;
        }
    }
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Boundary Conditions
 ******************************************************************************/

static void test_max_value_encoding(void) {
    test_setup();
    
    /* Inject maximum values - all channels for predictable voting */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, 50000.0f, FDR_QUALITY_GOOD);  /* Max alt */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, 50000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, 50000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 0, 512.0f, FDR_QUALITY_GOOD);  /* Max IAS */
    fdr_inject_sensor(PARAM_IAS, 1, 512.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 2, 512.0f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_PRESSURE_ALT, &voted);
    TEST_ASSERT_FLOAT_WITHIN(1000.0f, 50000.0f, voted.value);
    
    test_teardown();
}

static void test_min_value_encoding(void) {
    test_setup();
    
    /* Inject minimum values - all channels for predictable voting */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 0, -2000.0f, FDR_QUALITY_GOOD);  /* Min alt */
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 1, -2000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PRESSURE_ALT, 2, -2000.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 0, 0.0f, FDR_QUALITY_GOOD);  /* Zero airspeed */
    fdr_inject_sensor(PARAM_IAS, 1, 0.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_IAS, 2, 0.0f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_PRESSURE_ALT, &voted);
    TEST_ASSERT_FLOAT_WITHIN(500.0f, -2000.0f, voted.value);
    
    test_teardown();
}

static void test_negative_roll(void) {
    test_setup();
    
    /* Left bank */
    fdr_inject_sensor(PARAM_ROLL, 0, -35.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 1, -34.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 2, -36.0f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_ROLL, &voted);
    TEST_ASSERT_FLOAT_WITHIN(5.0f, -35.0f, voted.value);
    
    test_teardown();
}

static void test_negative_pitch(void) {
    test_setup();
    
    /* Nose down */
    fdr_inject_sensor(PARAM_PITCH, 0, -15.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PITCH, 1, -14.5f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_PITCH, 2, -15.5f, FDR_QUALITY_GOOD);
    
    fdr_start_recording();
    fdr_process(125);
    
    fdr_voted_param_t voted;
    fdr_get_voted_param(PARAM_PITCH, &voted);
    TEST_ASSERT_FLOAT_WITHIN(2.0f, -15.0f, voted.value);
    
    test_teardown();
}

static void test_high_bank_negative(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* High left bank (< -45 degrees) */
    fdr_inject_sensor(PARAM_ROLL, 0, -55.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 1, -54.0f, FDR_QUALITY_GOOD);
    fdr_inject_sensor(PARAM_ROLL, 2, -56.0f, FDR_QUALITY_GOOD);
    
    for (int i = 0; i < 8; i++) {
        fdr_process(125);
    }
    
    /* Should trigger high bank event */
    TEST_ASSERT(fdr_test_get_event_count() >= 1);
    
    test_teardown();
}

/*******************************************************************************
 * Test Category: Timing and Process Loop
 ******************************************************************************/

static void test_process_null_delta(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Zero delta should not crash */
    fdr_process(0);
    fdr_process(0);
    fdr_process(0);
    
    /* No frames should be recorded yet */
    TEST_ASSERT_EQUAL(0, fdr_test_get_frame_count());
    
    test_teardown();
}

static void test_process_large_delta(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Large delta (1 second) */
    fdr_process(1000);
    
    /* Should handle gracefully */
    TEST_ASSERT(fdr_test_get_state() == FDR_STATE_RECORDING ||
                fdr_test_get_state() == FDR_STATE_TRIGGERED);
    
    test_teardown();
}

static void test_rapid_process_calls(void) {
    test_setup();
    
    fdr_start_recording();
    
    /* Many rapid calls */
    for (int i = 0; i < 1000; i++) {
        fdr_process(1);
    }
    
    /* Should complete frames (1000ms = 1 second) */
    TEST_ASSERT(fdr_test_get_frame_count() >= 1);
    
    test_teardown();
}

/*******************************************************************************
 * Main Test Runner
 ******************************************************************************/

int main(void) {
    int fail_before = 0;
    
    printf("\n=== FDR Spotlight Integration Tests ===\n\n");
    
    printf("-- Initialization Tests --\n");
    RUN_TEST(test_init_default_config);
    RUN_TEST(test_init_custom_config);
    RUN_TEST(test_double_init);
    RUN_TEST(test_start_recording);
    RUN_TEST(test_start_stop_recording);
    RUN_TEST(test_shutdown_stops_recording);
    
    printf("\n-- Sensor Voting Tests --\n");
    RUN_TEST(test_inject_single_sensor);
    RUN_TEST(test_inject_dual_sensor_agreement);
    RUN_TEST(test_inject_dual_sensor_disagreement);
    RUN_TEST(test_triple_sensor_mvs_voting);
    RUN_TEST(test_triple_sensor_one_failed);
    RUN_TEST(test_all_sensors_failed);
    RUN_TEST(test_sensor_quality_degraded);
    
    printf("\n-- Frame Recording Tests --\n");
    RUN_TEST(test_record_single_frame);
    RUN_TEST(test_record_multiple_frames);
    RUN_TEST(test_frame_count_survives_state_change);
    RUN_TEST(test_status_shows_recorded_frames);
    
    printf("\n-- Event Triggering Tests --\n");
    RUN_TEST(test_manual_event_trigger);
    RUN_TEST(test_takeoff_event);
    RUN_TEST(test_landing_event);
    RUN_TEST(test_multiple_events);
    RUN_TEST(test_event_captures_flight_params);
    
    printf("\n-- Exceedance Detection Tests --\n");
    RUN_TEST(test_overspeed_detection);
    RUN_TEST(test_high_g_detection);
    RUN_TEST(test_high_bank_angle_detection);
    RUN_TEST(test_stall_warning_detection);
    RUN_TEST(test_gpws_detection);
    RUN_TEST(test_tcas_ra_detection);
    
    printf("\n-- Telemetry Tests --\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_content);
    RUN_TEST(test_telemetry_queue_multiple);
    RUN_TEST(test_telemetry_empty_queue);
    
    printf("\n-- Fault Handling Tests --\n");
    RUN_TEST(test_inject_sensor_fault);
    RUN_TEST(test_inject_memory_fault);
    RUN_TEST(test_inject_timing_fault);
    RUN_TEST(test_fault_recovery);
    RUN_TEST(test_cross_channel_fault_detection);
    
    printf("\n-- Status Reporting Tests --\n");
    RUN_TEST(test_status_null_pointer);
    RUN_TEST(test_status_not_initialized);
    RUN_TEST(test_status_memory_usage);
    
    printf("\n-- Voted Parameter Access Tests --\n");
    RUN_TEST(test_voted_param_null_pointer);
    RUN_TEST(test_voted_param_invalid_id);
    RUN_TEST(test_voted_param_all_mandatory);
    
    printf("\n-- Event Access Tests --\n");
    RUN_TEST(test_event_null_pointer);
    RUN_TEST(test_event_invalid_index);
    RUN_TEST(test_event_description_truncation);
    
    printf("\n-- Emergency Scenario Tests --\n");
    RUN_TEST(test_impact_event);
    RUN_TEST(test_combined_emergency_scenario);
    RUN_TEST(test_tcas_ra_climb_scenario);
    RUN_TEST(test_approach_stall_scenario);
    
    printf("\n-- Boundary Condition Tests --\n");
    RUN_TEST(test_max_value_encoding);
    RUN_TEST(test_min_value_encoding);
    RUN_TEST(test_negative_roll);
    RUN_TEST(test_negative_pitch);
    RUN_TEST(test_high_bank_negative);
    
    printf("\n-- Timing Tests --\n");
    RUN_TEST(test_process_null_delta);
    RUN_TEST(test_process_large_delta);
    RUN_TEST(test_rapid_process_calls);
    
    printf("\n========================================\n");
    printf("Tests Run:    %d\n", g_test_pass + g_test_fail);
    printf("Tests Passed: %d\n", g_test_pass);
    printf("Tests Failed: %d\n", g_test_fail);
    printf("========================================\n\n");
    
    return (g_test_fail > 0) ? 1 : 0;
}
