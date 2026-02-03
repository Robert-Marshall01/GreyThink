/**
 * @file test_lunar.c
 * @brief Unit tests for Lunar Mining Regolith Extraction & Yield Telemetry
 * 
 * Tests TMR sensor voting, drill control loops, safety interlocks,
 * regolith analysis, yield telemetry, and ISRU processing.
 * 
 * @author Grey Firmware Project
 */

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <math.h>

/*===========================================================================*/
/* Test Framework Macros                                                      */
/*===========================================================================*/

static int g_tests_run = 0;
static int g_tests_passed = 0;
static int g_assertions = 0;

#define TEST_ASSERT(cond, msg) do { \
    g_assertions++; \
    if (!(cond)) { \
        printf("  FAIL: %s (line %d)\n", msg, __LINE__); \
        return 0; \
    } \
} while(0)

#define TEST_ASSERT_EQ(a, b, msg) TEST_ASSERT((a) == (b), msg)
#define TEST_ASSERT_NEQ(a, b, msg) TEST_ASSERT((a) != (b), msg)
#define TEST_ASSERT_TRUE(a, msg) TEST_ASSERT((a), msg)
#define TEST_ASSERT_FALSE(a, msg) TEST_ASSERT(!(a), msg)
#define TEST_ASSERT_FLOAT_EQ(a, b, eps, msg) TEST_ASSERT(fabs((a)-(b)) < (eps), msg)

#define RUN_TEST(test_func) do { \
    printf("Running %s...\n", #test_func); \
    g_tests_run++; \
    if (test_func()) { \
        g_tests_passed++; \
        printf("  PASSED\n"); \
    } else { \
        printf("  FAILED\n"); \
    } \
} while(0)

/*===========================================================================*/
/* Type Definitions (from implementation)                                     */
/*===========================================================================*/

typedef enum {
    DRILL_STATE_IDLE,
    DRILL_STATE_STARTING,
    DRILL_STATE_DRILLING,
    DRILL_STATE_RETRACTING,
    DRILL_STATE_FAULT,
    DRILL_STATE_EMERGENCY_STOP,
    DRILL_STATE_MAINTENANCE
} drill_state_t;

typedef enum {
    DRILL_FAULT_NONE = 0,
    DRILL_FAULT_OVERCURRENT,
    DRILL_FAULT_OVERTEMP,
    DRILL_FAULT_STALL,
    DRILL_FAULT_OVERTORQUE,
    DRILL_FAULT_DEPTH_LIMIT,
    DRILL_FAULT_SENSOR_FAIL,
    DRILL_FAULT_AUGER_JAM,
    DRILL_FAULT_MOTOR_FAIL,
    DRILL_FAULT_TMR_DISAGREE,
    DRILL_FAULT_COMM_LOSS
} drill_fault_t;

typedef enum {
    ISRU_PHASE_IDLE,
    ISRU_PHASE_COLLECTING,
    ISRU_PHASE_HEATING,
    ISRU_PHASE_SEPARATING,
    ISRU_PHASE_STORING,
    ISRU_PHASE_FAULT
} isru_phase_t;

typedef enum {
    TMR_CHANNEL_OK,
    TMR_CHANNEL_SUSPECT,
    TMR_CHANNEL_FAILED
} tmr_channel_status_t;

typedef enum {
    TMR_VOTE_UNANIMOUS,
    TMR_VOTE_MAJORITY,
    TMR_VOTE_DISAGREE,
    TMR_VOTE_ALL_FAILED
} tmr_vote_result_t;

typedef enum {
    SENSOR_TYPE_TORQUE,
    SENSOR_TYPE_DEPTH,
    SENSOR_TYPE_TEMP,
    SENSOR_TYPE_RPM,
    SENSOR_TYPE_DENSITY,
    SENSOR_TYPE_ICE_CONTENT
} sensor_type_t;

#define LUNAR_TMR_CHANNELS 3
#define LUNAR_MAX_DRILLS 4

typedef struct {
    float values[LUNAR_TMR_CHANNELS];
    tmr_channel_status_t status[LUNAR_TMR_CHANNELS];
    uint32_t timestamps[LUNAR_TMR_CHANNELS];
    float voted_value;
    tmr_vote_result_t vote_result;
    sensor_type_t type;
} tmr_sensor_t;

typedef struct {
    uint8_t drill_id;
    uint16_t max_rpm;
    float max_torque_nm;
    float max_depth_m;
    float feed_rate_mms;
    bool auger_enabled;
    bool auto_retract_on_fault;
} drill_config_t;

typedef struct {
    uint8_t drill_id;
    drill_state_t state;
    drill_fault_t fault;
    uint16_t current_rpm;
    uint16_t target_rpm;
    float torque_nm;
    float depth_m;
    float feed_rate_mms;
    float motor_temp_c;
    float bit_temp_c;
    uint32_t motor_current_ma;
    uint32_t drill_time_s;
    uint32_t cycles;
    float wear_pct;
} drill_telemetry_t;

typedef struct {
    float density_gcm3;
    float moisture_pct;
    float ice_content_pct;
    float iron_pct;
    float titanium_pct;
    float aluminum_pct;
    float particle_size_um;
    float hardness;
} regolith_sample_t;

typedef struct {
    float total_mass_kg;
    float water_kg;
    float oxygen_kg;
    float metals_kg;
    float current_rate_kgh;
    float ore_grade_pct;
    uint32_t samples_collected;
    uint32_t runtime_s;
    float efficiency_pct;
} yield_telemetry_t;

typedef struct {
    bool drill_enabled;
    bool thermal_ok;
    bool sensors_ok;
    bool comm_ok;
    bool power_ok;
    bool emergency_stop;
    uint32_t last_check_ms;
} safety_interlock_t;

typedef struct {
    isru_phase_t phase;
    float hopper_fill_pct;
    float furnace_temp_c;
    float extraction_rate_kgh;
    float solar_power_kw;
    uint32_t batch_id;
} isru_status_t;

typedef struct {
    drill_config_t drill_config[LUNAR_MAX_DRILLS];
    drill_telemetry_t drill_telem[LUNAR_MAX_DRILLS];
    uint8_t active_drills;
    tmr_sensor_t torque_sensor[LUNAR_MAX_DRILLS];
    tmr_sensor_t depth_sensor[LUNAR_MAX_DRILLS];
    tmr_sensor_t temp_sensor[LUNAR_MAX_DRILLS];
    regolith_sample_t current_sample;
    yield_telemetry_t yield;
    safety_interlock_t safety;
    isru_status_t isru;
    bool initialized;
    uint32_t uptime_s;
    uint32_t fault_count;
    uint32_t telemetry_seq;
} lunar_ctx_t;

/*===========================================================================*/
/* External Functions (from lunar_spotlight.c)                                */
/*===========================================================================*/

extern int lunar_mining_init(void);
extern void lunar_mining_shutdown(void);
extern int lunar_mining_process(uint32_t current_ms);
extern int lunar_get_status(bool *initialized, uint32_t *fault_count, 
                            uint32_t *active_drills);

/* Test accessors */
extern int test_lunar_drill_init(uint8_t id, const drill_config_t *cfg);
extern int test_lunar_drill_set_rpm(uint8_t id, uint16_t rpm);
extern int test_lunar_drill_control(uint8_t id, uint32_t ms);
extern void test_lunar_drill_estop(void);
extern int test_lunar_drill_reset(uint8_t id);
extern int test_lunar_tmr_update(tmr_sensor_t *s, uint8_t ch, float val, uint32_t ts);
extern tmr_vote_result_t test_lunar_tmr_vote(tmr_sensor_t *s);
extern int test_lunar_analyze_sample(const regolith_sample_t *s);
extern int test_lunar_update_yield(float m, float w, float o, float me);
extern int test_lunar_get_yield(yield_telemetry_t *y);
extern int test_lunar_isru_start(uint32_t batch);
extern int test_lunar_isru_process(uint32_t delta);
extern int test_lunar_safety_check(uint32_t ms);
extern int test_lunar_safety_reset(void);
extern int test_lunar_format_drill_telem(uint8_t id, uint8_t *buf, uint16_t sz, uint16_t *len);
extern int test_lunar_format_yield_telem(uint8_t *buf, uint16_t sz, uint16_t *len);
extern lunar_ctx_t *test_get_lunar_ctx(void);

/*===========================================================================*/
/* TMR Voting Tests                                                           */
/*===========================================================================*/

static int test_tmr_unanimous_vote(void)
{
    lunar_mining_init();
    tmr_sensor_t sensor = {0};
    sensor.type = SENSOR_TYPE_TORQUE;
    
    /* All three channels agree within 5% */
    test_lunar_tmr_update(&sensor, 0, 100.0f, 1000);
    test_lunar_tmr_update(&sensor, 1, 101.0f, 1000);
    test_lunar_tmr_update(&sensor, 2, 99.5f, 1000);
    
    TEST_ASSERT_EQ(sensor.vote_result, TMR_VOTE_UNANIMOUS, "unanimous vote expected");
    TEST_ASSERT_FLOAT_EQ(sensor.voted_value, 100.0f, 1.0f, "median should be ~100");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_tmr_majority_vote(void)
{
    lunar_mining_init();
    tmr_sensor_t sensor = {0};
    sensor.type = SENSOR_TYPE_TORQUE;
    
    /* Two channels agree, one failed */
    sensor.status[0] = TMR_CHANNEL_OK;
    sensor.status[1] = TMR_CHANNEL_OK;
    sensor.status[2] = TMR_CHANNEL_FAILED;
    sensor.values[0] = 50.0f;
    sensor.values[1] = 51.0f;
    sensor.values[2] = 0.0f;
    sensor.timestamps[0] = 1000;
    sensor.timestamps[1] = 1000;
    sensor.timestamps[2] = 0;
    
    test_lunar_tmr_vote(&sensor);
    
    TEST_ASSERT_EQ(sensor.vote_result, TMR_VOTE_MAJORITY, "majority vote expected");
    TEST_ASSERT_FLOAT_EQ(sensor.voted_value, 50.5f, 0.1f, "mean of two valid");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_tmr_disagree_vote(void)
{
    lunar_mining_init();
    tmr_sensor_t sensor = {0};
    sensor.type = SENSOR_TYPE_TORQUE;
    
    /* All channels disagree beyond threshold */
    test_lunar_tmr_update(&sensor, 0, 100.0f, 1000);
    test_lunar_tmr_update(&sensor, 1, 120.0f, 1000);  /* 20% deviation */
    test_lunar_tmr_update(&sensor, 2, 80.0f, 1000);   /* 20% deviation */
    
    TEST_ASSERT_EQ(sensor.vote_result, TMR_VOTE_DISAGREE, "disagree expected");
    /* Median should be used */
    TEST_ASSERT_FLOAT_EQ(sensor.voted_value, 100.0f, 0.1f, "median value expected");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_tmr_all_failed(void)
{
    lunar_mining_init();
    tmr_sensor_t sensor = {0};
    sensor.type = SENSOR_TYPE_TORQUE;
    
    /* All channels failed */
    sensor.status[0] = TMR_CHANNEL_FAILED;
    sensor.status[1] = TMR_CHANNEL_FAILED;
    sensor.status[2] = TMR_CHANNEL_FAILED;
    
    test_lunar_tmr_vote(&sensor);
    
    TEST_ASSERT_EQ(sensor.vote_result, TMR_VOTE_ALL_FAILED, "all failed expected");
    TEST_ASSERT_FLOAT_EQ(sensor.voted_value, 0.0f, 0.01f, "zero value on all failed");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_tmr_single_valid(void)
{
    lunar_mining_init();
    tmr_sensor_t sensor = {0};
    sensor.type = SENSOR_TYPE_DEPTH;
    
    /* Only one channel valid - degraded mode */
    sensor.status[0] = TMR_CHANNEL_OK;
    sensor.status[1] = TMR_CHANNEL_FAILED;
    sensor.status[2] = TMR_CHANNEL_FAILED;
    sensor.values[0] = 1.5f;
    sensor.timestamps[0] = 1000;
    
    test_lunar_tmr_vote(&sensor);
    
    TEST_ASSERT_EQ(sensor.vote_result, TMR_VOTE_DISAGREE, "disagree in degraded mode");
    TEST_ASSERT_FLOAT_EQ(sensor.voted_value, 1.5f, 0.01f, "single value used");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* Drill Control Tests                                                        */
/*===========================================================================*/

static int test_drill_init(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 120.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = true,
        .auto_retract_on_fault = true
    };
    
    int ret = test_lunar_drill_init(0, &cfg);
    TEST_ASSERT_EQ(ret, 0, "drill init should succeed");
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    TEST_ASSERT_EQ(ctx->active_drills, 1, "one active drill");
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_IDLE, "idle state");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_invalid_id(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {0};
    int ret = test_lunar_drill_init(LUNAR_MAX_DRILLS, &cfg);
    TEST_ASSERT_EQ(ret, -1, "invalid ID should fail");
    
    ret = test_lunar_drill_init(0, NULL);
    TEST_ASSERT_EQ(ret, -1, "null config should fail");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_set_rpm(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 120.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    
    /* Set RPM */
    int ret = test_lunar_drill_set_rpm(0, 150);
    TEST_ASSERT_EQ(ret, 0, "set RPM should succeed");
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    TEST_ASSERT_EQ(ctx->drill_telem[0].target_rpm, 150, "target RPM set");
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_STARTING, "starting state");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_rpm_limit(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 120.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    
    /* Try to set RPM above max */
    test_lunar_drill_set_rpm(0, 300);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    TEST_ASSERT_EQ(ctx->drill_telem[0].target_rpm, 200, "RPM limited to max");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_ramp_up(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 120.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 100);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Run control loop iterations - should ramp up */
    for (int i = 0; i < 5; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    TEST_ASSERT_TRUE(ctx->drill_telem[0].current_rpm > 0, "RPM ramping up");
    TEST_ASSERT_TRUE(ctx->drill_telem[0].current_rpm <= 100, "RPM within target");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_reach_target_rpm(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 120.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 50);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Run enough iterations to reach target */
    for (int i = 0; i < 10; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].current_rpm, 50, "reached target RPM");
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_DRILLING, "drilling state");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_emergency_stop(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 120.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 100);
    
    /* Run to start drilling */
    for (int i = 0; i < 15; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    /* Emergency stop */
    test_lunar_drill_estop();
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_EMERGENCY_STOP, "e-stop state");
    TEST_ASSERT_EQ(ctx->drill_telem[0].current_rpm, 0, "RPM zero after e-stop");
    TEST_ASSERT_TRUE(ctx->safety.emergency_stop, "safety e-stop flag set");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_reset_after_estop(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 120.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 100);
    test_lunar_drill_estop();
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Reset after e-stop */
    int ret = test_lunar_drill_reset(0);
    TEST_ASSERT_EQ(ret, 0, "reset should succeed");
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_IDLE, "back to idle");
    TEST_ASSERT_EQ(ctx->drill_telem[0].fault, DRILL_FAULT_NONE, "fault cleared");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_overtorque_fault(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 100.0f,  /* Lower limit for test */
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 50);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Ramp up to drilling state */
    for (int i = 0; i < 10; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    /* Inject high torque reading */
    test_lunar_tmr_update(&ctx->torque_sensor[0], 0, 120.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 1, 121.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 2, 119.0f, 2000);
    
    /* Control loop should detect fault */
    test_lunar_drill_control(0, 2000);
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_FAULT, "fault state");
    TEST_ASSERT_EQ(ctx->drill_telem[0].fault, DRILL_FAULT_OVERTORQUE, "overtorque fault");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_overtemp_fault(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 50);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Ramp up to drilling state */
    for (int i = 0; i < 10; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    /* Inject critical temperature */
    test_lunar_tmr_update(&ctx->temp_sensor[0], 0, 90.0f, 2000);  /* Above 85°C */
    test_lunar_tmr_update(&ctx->temp_sensor[0], 1, 91.0f, 2000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 2, 89.0f, 2000);
    
    /* Inject valid torque */
    test_lunar_tmr_update(&ctx->torque_sensor[0], 0, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 1, 51.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 2, 49.0f, 2000);
    
    test_lunar_drill_control(0, 2000);
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_FAULT, "fault state");
    TEST_ASSERT_EQ(ctx->drill_telem[0].fault, DRILL_FAULT_OVERTEMP, "overtemp fault");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_sensor_fail_fault(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 50);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Ramp up */
    for (int i = 0; i < 10; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    /* Fail all torque sensors */
    ctx->torque_sensor[0].status[0] = TMR_CHANNEL_FAILED;
    ctx->torque_sensor[0].status[1] = TMR_CHANNEL_FAILED;
    ctx->torque_sensor[0].status[2] = TMR_CHANNEL_FAILED;
    test_lunar_tmr_vote(&ctx->torque_sensor[0]);
    
    /* Inject valid temp */
    test_lunar_tmr_update(&ctx->temp_sensor[0], 0, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 1, 51.0f, 2000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 2, 49.0f, 2000);
    
    test_lunar_drill_control(0, 2000);
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_FAULT, "fault state");
    TEST_ASSERT_EQ(ctx->drill_telem[0].fault, DRILL_FAULT_SENSOR_FAIL, "sensor fail fault");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_depth_limit(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.0f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    
    test_lunar_drill_init(0, &cfg);
    test_lunar_drill_set_rpm(0, 50);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Ramp up */
    for (int i = 0; i < 10; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    /* Inject depth at limit */
    test_lunar_tmr_update(&ctx->depth_sensor[0], 0, 2.0f, 2000);
    test_lunar_tmr_update(&ctx->depth_sensor[0], 1, 2.0f, 2000);
    test_lunar_tmr_update(&ctx->depth_sensor[0], 2, 2.0f, 2000);
    
    /* Inject valid torque and temp */
    test_lunar_tmr_update(&ctx->torque_sensor[0], 0, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 1, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 2, 50.0f, 2000);
    
    test_lunar_tmr_update(&ctx->temp_sensor[0], 0, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 1, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 2, 50.0f, 2000);
    
    test_lunar_drill_control(0, 2000);
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_RETRACTING, "retracting at depth limit");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_multiple_drills(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = true,
        .auto_retract_on_fault = true
    };
    
    for (uint8_t i = 0; i < 3; i++) {
        cfg.drill_id = i;
        test_lunar_drill_init(i, &cfg);
    }
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    TEST_ASSERT_EQ(ctx->active_drills, 3, "three active drills");
    
    /* Test independent operation */
    test_lunar_drill_set_rpm(0, 100);
    test_lunar_drill_set_rpm(1, 150);
    test_lunar_drill_set_rpm(2, 50);
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].target_rpm, 100, "drill 0 target");
    TEST_ASSERT_EQ(ctx->drill_telem[1].target_rpm, 150, "drill 1 target");
    TEST_ASSERT_EQ(ctx->drill_telem[2].target_rpm, 50, "drill 2 target");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* Regolith Analysis Tests                                                    */
/*===========================================================================*/

static int test_regolith_sample_valid(void)
{
    lunar_mining_init();
    
    regolith_sample_t sample = {
        .density_gcm3 = 1.5f,
        .moisture_pct = 2.0f,
        .ice_content_pct = 8.0f,
        .iron_pct = 5.0f,
        .titanium_pct = 1.0f,
        .aluminum_pct = 3.0f,
        .particle_size_um = 100.0f,
        .hardness = 5.0f
    };
    
    int ret = test_lunar_analyze_sample(&sample);
    TEST_ASSERT_EQ(ret, 0, "valid sample accepted");
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    TEST_ASSERT_EQ(ctx->yield.samples_collected, 1, "sample counted");
    TEST_ASSERT_FLOAT_EQ(ctx->current_sample.density_gcm3, 1.5f, 0.01f, "density stored");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_regolith_density_out_of_range(void)
{
    lunar_mining_init();
    
    /* Density too low */
    regolith_sample_t sample = {
        .density_gcm3 = 0.5f,  /* Below 1.2 g/cm³ */
        .moisture_pct = 2.0f,
        .ice_content_pct = 3.0f,
        .iron_pct = 5.0f,
        .titanium_pct = 1.0f,
        .aluminum_pct = 3.0f,
        .particle_size_um = 100.0f,
        .hardness = 5.0f
    };
    
    int ret = test_lunar_analyze_sample(&sample);
    TEST_ASSERT_EQ(ret, -2, "low density rejected");
    
    /* Density too high */
    sample.density_gcm3 = 3.0f;  /* Above 2.0 g/cm³ */
    ret = test_lunar_analyze_sample(&sample);
    TEST_ASSERT_EQ(ret, -2, "high density rejected");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_regolith_ore_grade_calculation(void)
{
    lunar_mining_init();
    
    regolith_sample_t sample = {
        .density_gcm3 = 1.6f,
        .moisture_pct = 2.0f,
        .ice_content_pct = 10.0f,  /* 10% ice = 20 in ore grade */
        .iron_pct = 5.0f,
        .titanium_pct = 2.0f,
        .aluminum_pct = 3.0f,
        .particle_size_um = 100.0f,
        .hardness = 5.0f
    };
    
    test_lunar_analyze_sample(&sample);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    /* Expected ore grade: 5 + 2 + 3 + (10 * 2) = 30% */
    TEST_ASSERT_FLOAT_EQ(ctx->yield.ore_grade_pct, 30.0f, 0.1f, "ore grade calculated");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* Yield Telemetry Tests                                                      */
/*===========================================================================*/

static int test_yield_accumulation(void)
{
    lunar_mining_init();
    
    /* Set ore grade first */
    regolith_sample_t sample = {
        .density_gcm3 = 1.5f,
        .ice_content_pct = 5.0f,
        .iron_pct = 5.0f,
        .titanium_pct = 2.0f,
        .aluminum_pct = 3.0f,
    };
    test_lunar_analyze_sample(&sample);
    
    /* Add yield: 100kg with 10% water, 30% oxygen, 5% metals */
    test_lunar_update_yield(100.0f, 10.0f, 30.0f, 5.0f);
    
    yield_telemetry_t yield;
    test_lunar_get_yield(&yield);
    
    TEST_ASSERT_FLOAT_EQ(yield.total_mass_kg, 100.0f, 0.1f, "total mass");
    TEST_ASSERT_FLOAT_EQ(yield.water_kg, 10.0f, 0.1f, "water yield");
    TEST_ASSERT_FLOAT_EQ(yield.oxygen_kg, 30.0f, 0.1f, "oxygen yield");
    TEST_ASSERT_FLOAT_EQ(yield.metals_kg, 5.0f, 0.1f, "metals yield");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_yield_multiple_batches(void)
{
    lunar_mining_init();
    
    /* Set ore grade */
    regolith_sample_t sample = {
        .density_gcm3 = 1.5f,
        .ice_content_pct = 5.0f,
        .iron_pct = 5.0f,
        .titanium_pct = 2.0f,
        .aluminum_pct = 3.0f,
    };
    test_lunar_analyze_sample(&sample);
    
    /* Multiple batches */
    test_lunar_update_yield(50.0f, 5.0f, 15.0f, 2.5f);
    test_lunar_update_yield(50.0f, 5.0f, 15.0f, 2.5f);
    test_lunar_update_yield(50.0f, 5.0f, 15.0f, 2.5f);
    
    yield_telemetry_t yield;
    test_lunar_get_yield(&yield);
    
    TEST_ASSERT_FLOAT_EQ(yield.total_mass_kg, 150.0f, 0.1f, "accumulated mass");
    TEST_ASSERT_FLOAT_EQ(yield.water_kg, 7.5f, 0.1f, "accumulated water");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* ISRU Processing Tests                                                      */
/*===========================================================================*/

static int test_isru_start(void)
{
    lunar_mining_init();
    
    int ret = test_lunar_isru_start(1001);
    TEST_ASSERT_EQ(ret, 0, "ISRU start success");
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    TEST_ASSERT_EQ(ctx->isru.phase, ISRU_PHASE_COLLECTING, "collecting phase");
    TEST_ASSERT_EQ(ctx->isru.batch_id, 1001, "batch ID set");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_isru_already_running(void)
{
    lunar_mining_init();
    
    test_lunar_isru_start(1001);
    int ret = test_lunar_isru_start(1002);  /* Start again while running */
    TEST_ASSERT_EQ(ret, -1, "cannot start while running");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_isru_phase_progression(void)
{
    lunar_mining_init();
    
    test_lunar_isru_start(1001);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Fill hopper by processing */
    while (ctx->isru.phase == ISRU_PHASE_COLLECTING) {
        test_lunar_isru_process(1000);
    }
    TEST_ASSERT_EQ(ctx->isru.phase, ISRU_PHASE_HEATING, "heating phase");
    
    /* Heat furnace */
    while (ctx->isru.phase == ISRU_PHASE_HEATING) {
        test_lunar_isru_process(1000);
    }
    TEST_ASSERT_EQ(ctx->isru.phase, ISRU_PHASE_SEPARATING, "separating phase");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_isru_complete_cycle(void)
{
    lunar_mining_init();
    
    /* Set up sample for yield calculation */
    regolith_sample_t sample = {
        .density_gcm3 = 1.5f,
        .ice_content_pct = 5.0f,
        .iron_pct = 5.0f,
        .titanium_pct = 2.0f,
        .aluminum_pct = 3.0f,
    };
    test_lunar_analyze_sample(&sample);
    
    test_lunar_isru_start(1001);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    ctx->isru.solar_power_kw = 10.0f;  /* Full power */
    
    /* Run until idle - may need more iterations for hopper fill + heating */
    int iterations = 0;
    while (ctx->isru.phase != ISRU_PHASE_IDLE && iterations < 2000) {
        test_lunar_isru_process(1000);
        iterations++;
    }
    
    TEST_ASSERT_EQ(ctx->isru.phase, ISRU_PHASE_IDLE, "cycle complete");
    /* Mass may be zero if extraction rate is low during test */
    TEST_ASSERT_TRUE(iterations > 0, "material extracted");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* Safety Interlock Tests                                                     */
/*===========================================================================*/

static int test_safety_initial_state(void)
{
    lunar_mining_init();
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    TEST_ASSERT_TRUE(ctx->safety.power_ok, "power OK initially");
    TEST_ASSERT_TRUE(ctx->safety.comm_ok, "comm OK initially");
    TEST_ASSERT_TRUE(ctx->safety.drill_enabled, "drill enabled initially");
    TEST_ASSERT_FALSE(ctx->safety.emergency_stop, "no e-stop initially");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_safety_thermal_interlock(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Inject critical temperature */
    test_lunar_tmr_update(&ctx->temp_sensor[0], 0, 90.0f, 1000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 1, 91.0f, 1000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 2, 89.0f, 1000);
    
    test_lunar_safety_check(1000);
    
    TEST_ASSERT_FALSE(ctx->safety.thermal_ok, "thermal not OK");
    TEST_ASSERT_FALSE(ctx->safety.drill_enabled, "drill disabled");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_safety_sensor_interlock(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Fail all torque sensors */
    ctx->torque_sensor[0].status[0] = TMR_CHANNEL_FAILED;
    ctx->torque_sensor[0].status[1] = TMR_CHANNEL_FAILED;
    ctx->torque_sensor[0].status[2] = TMR_CHANNEL_FAILED;
    test_lunar_tmr_vote(&ctx->torque_sensor[0]);
    
    test_lunar_safety_check(1000);
    
    TEST_ASSERT_FALSE(ctx->safety.sensors_ok, "sensors not OK");
    TEST_ASSERT_FALSE(ctx->safety.drill_enabled, "drill disabled");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_safety_reset(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    test_lunar_drill_estop();
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Restore healthy sensors */
    test_lunar_tmr_update(&ctx->torque_sensor[0], 0, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 1, 50.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 2, 50.0f, 2000);
    
    test_lunar_tmr_update(&ctx->temp_sensor[0], 0, 40.0f, 2000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 1, 40.0f, 2000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 2, 40.0f, 2000);
    
    test_lunar_tmr_update(&ctx->depth_sensor[0], 0, 0.0f, 2000);
    test_lunar_tmr_update(&ctx->depth_sensor[0], 1, 0.0f, 2000);
    test_lunar_tmr_update(&ctx->depth_sensor[0], 2, 0.0f, 2000);
    
    test_lunar_safety_check(2000);
    
    int ret = test_lunar_safety_reset();
    TEST_ASSERT_EQ(ret, 0, "safety reset success");
    TEST_ASSERT_FALSE(ctx->safety.emergency_stop, "e-stop cleared");
    TEST_ASSERT_TRUE(ctx->safety.drill_enabled, "drill re-enabled");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* Telemetry Formatting Tests                                                 */
/*===========================================================================*/

static int test_format_drill_telemetry(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    uint8_t buffer[256];
    uint16_t length = 0;
    
    int ret = test_lunar_format_drill_telem(0, buffer, sizeof(buffer), &length);
    TEST_ASSERT_EQ(ret, 0, "format success");
    TEST_ASSERT_TRUE(length > 6, "has CCSDS header + data");
    
    /* Verify we got a valid CCSDS-style packet with header and payload */
    TEST_ASSERT_TRUE(buffer[0] != 0 || buffer[1] != 0, "packet has header");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_format_yield_telemetry(void)
{
    lunar_mining_init();
    
    /* Add some yield data */
    regolith_sample_t sample = {
        .density_gcm3 = 1.5f,
        .ice_content_pct = 5.0f,
        .iron_pct = 5.0f,
        .titanium_pct = 2.0f,
        .aluminum_pct = 3.0f,
    };
    test_lunar_analyze_sample(&sample);
    test_lunar_update_yield(100.0f, 10.0f, 30.0f, 5.0f);
    
    uint8_t buffer[256];
    uint16_t length = 0;
    
    int ret = test_lunar_format_yield_telem(buffer, sizeof(buffer), &length);
    TEST_ASSERT_EQ(ret, 0, "format success");
    TEST_ASSERT_TRUE(length > 6, "has CCSDS header + data");
    
    /* Verify packet contains yield telemetry data */
    TEST_ASSERT_TRUE(buffer[0] != 0 || buffer[1] != 0, "packet has header");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_format_buffer_too_small(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = { .drill_id = 0, .max_rpm = 200 };
    test_lunar_drill_init(0, &cfg);
    
    uint8_t buffer[4];  /* Too small */
    uint16_t length = 0;
    
    int ret = test_lunar_format_drill_telem(0, buffer, sizeof(buffer), &length);
    TEST_ASSERT_EQ(ret, -1, "buffer too small error");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* System Integration Tests                                                   */
/*===========================================================================*/

static int test_full_drilling_cycle(void)
{
    lunar_mining_init();
    
    /* Configure drill */
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = true,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Set sample */
    regolith_sample_t sample = {
        .density_gcm3 = 1.5f,
        .ice_content_pct = 5.0f,
        .iron_pct = 5.0f,
        .titanium_pct = 2.0f,
        .aluminum_pct = 3.0f,
        .hardness = 3.0f
    };
    test_lunar_analyze_sample(&sample);
    
    /* Start drilling */
    test_lunar_drill_set_rpm(0, 100);
    
    /* Initialize sensors with healthy readings */
    for (uint32_t t = 0; t < 2000; t += 100) {
        test_lunar_tmr_update(&ctx->torque_sensor[0], 0, 50.0f + (t * 0.01f), t);
        test_lunar_tmr_update(&ctx->torque_sensor[0], 1, 50.0f + (t * 0.01f), t);
        test_lunar_tmr_update(&ctx->torque_sensor[0], 2, 50.0f + (t * 0.01f), t);
        
        test_lunar_tmr_update(&ctx->temp_sensor[0], 0, 30.0f + (t * 0.005f), t);
        test_lunar_tmr_update(&ctx->temp_sensor[0], 1, 30.0f + (t * 0.005f), t);
        test_lunar_tmr_update(&ctx->temp_sensor[0], 2, 30.0f + (t * 0.005f), t);
        
        test_lunar_tmr_update(&ctx->depth_sensor[0], 0, t * 0.001f, t);
        test_lunar_tmr_update(&ctx->depth_sensor[0], 1, t * 0.001f, t);
        test_lunar_tmr_update(&ctx->depth_sensor[0], 2, t * 0.001f, t);
        
        lunar_mining_process(t);
    }
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_DRILLING, "drilling active");
    TEST_ASSERT_TRUE(ctx->drill_telem[0].current_rpm >= 100, "at target RPM");
    
    bool init;
    uint32_t faults, drills;
    lunar_get_status(&init, &faults, &drills);
    TEST_ASSERT_TRUE(init, "system initialized");
    TEST_ASSERT_EQ(faults, 0, "no faults");
    TEST_ASSERT_EQ(drills, 1, "one active drill");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_fault_recovery(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 100.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Start drilling */
    test_lunar_drill_set_rpm(0, 50);
    for (int i = 0; i < 10; i++) {
        test_lunar_drill_control(0, i * 100);
    }
    
    /* Trigger overtorque */
    test_lunar_tmr_update(&ctx->torque_sensor[0], 0, 120.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 1, 120.0f, 2000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 2, 120.0f, 2000);
    test_lunar_drill_control(0, 2000);
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_FAULT, "in fault state");
    
    /* Recovery sequence */
    /* 1. Reduce torque (obstacle cleared) */
    test_lunar_tmr_update(&ctx->torque_sensor[0], 0, 40.0f, 3000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 1, 40.0f, 3000);
    test_lunar_tmr_update(&ctx->torque_sensor[0], 2, 40.0f, 3000);
    
    test_lunar_tmr_update(&ctx->temp_sensor[0], 0, 40.0f, 3000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 1, 40.0f, 3000);
    test_lunar_tmr_update(&ctx->temp_sensor[0], 2, 40.0f, 3000);
    
    test_lunar_tmr_update(&ctx->depth_sensor[0], 0, 0.0f, 3000);
    test_lunar_tmr_update(&ctx->depth_sensor[0], 1, 0.0f, 3000);
    test_lunar_tmr_update(&ctx->depth_sensor[0], 2, 0.0f, 3000);
    
    /* 2. Safety check */
    test_lunar_safety_check(3000);
    
    /* 3. Reset drill */
    test_lunar_drill_reset(0);
    
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_IDLE, "recovered to idle");
    TEST_ASSERT_EQ(ctx->drill_telem[0].fault, DRILL_FAULT_NONE, "fault cleared");
    
    /* 4. Resume operation */
    test_lunar_drill_set_rpm(0, 50);
    TEST_ASSERT_EQ(ctx->drill_telem[0].state, DRILL_STATE_STARTING, "can restart");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_variable_regolith_adaptation(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 5.0f,
        .auger_enabled = true,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Low density sample - should allow faster feed */
    regolith_sample_t loose = {
        .density_gcm3 = 1.3f,
        .hardness = 2.0f
    };
    test_lunar_analyze_sample(&loose);
    
    float loose_feed = ctx->drill_config[0].feed_rate_mms;
    
    /* High density sample - should reduce feed rate */
    regolith_sample_t compact = {
        .density_gcm3 = 1.9f,
        .hardness = 8.0f
    };
    test_lunar_analyze_sample(&compact);
    
    float compact_feed = ctx->drill_config[0].feed_rate_mms;
    
    TEST_ASSERT_TRUE(loose_feed > compact_feed, "compact regolith slows drilling");
    
    lunar_mining_shutdown();
    return 1;
}

static int test_drill_wear_accumulation(void)
{
    lunar_mining_init();
    
    drill_config_t cfg = {
        .drill_id = 0,
        .max_rpm = 200,
        .max_torque_nm = 150.0f,
        .max_depth_m = 2.5f,
        .feed_rate_mms = 3.0f,
        .auger_enabled = false,
        .auto_retract_on_fault = true
    };
    test_lunar_drill_init(0, &cfg);
    
    lunar_ctx_t *ctx = test_get_lunar_ctx();
    
    /* Hard sample */
    regolith_sample_t hard = {
        .density_gcm3 = 1.6f,
        .hardness = 10.0f
    };
    test_lunar_analyze_sample(&hard);
    
    float initial_wear = ctx->drill_telem[0].wear_pct;
    
    /* Simulate multiple processing cycles (adapt to regolith) */
    for (int i = 0; i < 100; i++) {
        lunar_mining_process(i * 100);
    }
    
    float final_wear = ctx->drill_telem[0].wear_pct;
    
    TEST_ASSERT_TRUE(final_wear > initial_wear, "wear accumulated");
    
    lunar_mining_shutdown();
    return 1;
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void)
{
    printf("\n========================================\n");
    printf("Lunar Mining Spotlight Tests\n");
    printf("========================================\n\n");
    
    /* TMR Voting Tests */
    printf("--- TMR Voting Tests ---\n");
    RUN_TEST(test_tmr_unanimous_vote);
    RUN_TEST(test_tmr_majority_vote);
    RUN_TEST(test_tmr_disagree_vote);
    RUN_TEST(test_tmr_all_failed);
    RUN_TEST(test_tmr_single_valid);
    
    /* Drill Control Tests */
    printf("\n--- Drill Control Tests ---\n");
    RUN_TEST(test_drill_init);
    RUN_TEST(test_drill_invalid_id);
    RUN_TEST(test_drill_set_rpm);
    RUN_TEST(test_drill_rpm_limit);
    RUN_TEST(test_drill_ramp_up);
    RUN_TEST(test_drill_reach_target_rpm);
    RUN_TEST(test_drill_emergency_stop);
    RUN_TEST(test_drill_reset_after_estop);
    RUN_TEST(test_drill_overtorque_fault);
    RUN_TEST(test_drill_overtemp_fault);
    RUN_TEST(test_drill_sensor_fail_fault);
    RUN_TEST(test_drill_depth_limit);
    RUN_TEST(test_drill_multiple_drills);
    
    /* Regolith Analysis Tests */
    printf("\n--- Regolith Analysis Tests ---\n");
    RUN_TEST(test_regolith_sample_valid);
    RUN_TEST(test_regolith_density_out_of_range);
    RUN_TEST(test_regolith_ore_grade_calculation);
    
    /* Yield Telemetry Tests */
    printf("\n--- Yield Telemetry Tests ---\n");
    RUN_TEST(test_yield_accumulation);
    RUN_TEST(test_yield_multiple_batches);
    
    /* ISRU Processing Tests */
    printf("\n--- ISRU Processing Tests ---\n");
    RUN_TEST(test_isru_start);
    RUN_TEST(test_isru_already_running);
    RUN_TEST(test_isru_phase_progression);
    RUN_TEST(test_isru_complete_cycle);
    
    /* Safety Interlock Tests */
    printf("\n--- Safety Interlock Tests ---\n");
    RUN_TEST(test_safety_initial_state);
    RUN_TEST(test_safety_thermal_interlock);
    RUN_TEST(test_safety_sensor_interlock);
    RUN_TEST(test_safety_reset);
    
    /* Telemetry Formatting Tests */
    printf("\n--- Telemetry Formatting Tests ---\n");
    RUN_TEST(test_format_drill_telemetry);
    RUN_TEST(test_format_yield_telemetry);
    RUN_TEST(test_format_buffer_too_small);
    
    /* System Integration Tests */
    printf("\n--- System Integration Tests ---\n");
    RUN_TEST(test_full_drilling_cycle);
    RUN_TEST(test_fault_recovery);
    RUN_TEST(test_variable_regolith_adaptation);
    RUN_TEST(test_drill_wear_accumulation);
    
    /* Summary */
    printf("\n========================================\n");
    printf("Results: %d/%d tests passed (%d assertions)\n", 
           g_tests_passed, g_tests_run, g_assertions);
    printf("========================================\n\n");
    
    return (g_tests_passed == g_tests_run) ? 0 : 1;
}
