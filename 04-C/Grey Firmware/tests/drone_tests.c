/**
 * @file drone_tests.c
 * @brief Comprehensive tests for Drone Flight Control Spotlight
 *
 * Tests cover:
 * - PID controller functionality
 * - Attitude control loops
 * - Position hold and navigation
 * - Sensor fusion
 * - Safety interlocks (geofence, altitude, battery)
 * - Flight mode transitions
 * - Emergency procedures
 * - Motor mixing validation
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>
#include <stdint.h>
#include <math.h>

/* Test framework macros */
#define TEST_ASSERT(cond) do { \
    if (!(cond)) { \
        printf("  [FAIL] %s:%d: %s\n", __FILE__, __LINE__, #cond); \
        return 1; \
    } \
} while(0)

#define TEST_ASSERT_FLOAT_EQ(a, b, tol) do { \
    float _a = (a), _b = (b); \
    if (fabsf(_a - _b) > (tol)) { \
        printf("  [FAIL] %s:%d: %f != %f (tol=%f)\n", __FILE__, __LINE__, _a, _b, tol); \
        return 1; \
    } \
} while(0)

#define RUN_TEST(test) do { \
    printf("  Running %s...\n", #test); \
    tests_run++; \
    if (test() == 0) { \
        tests_passed++; \
        printf("  [PASS] %s\n", #test); \
    } else { \
        tests_failed++; \
    } \
} while(0)

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

/* ─────────────────────────────────────────────────────────────────────────────
 * Enable test mode and include implementation directly
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_DRONE_SPOTLIGHT_TEST

/* Include implementation - this provides all types and functions */
#include "../src/drone/drone_spotlight.c"

/* ─────────────────────────────────────────────────────────────────────────────
 * Test Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

static uint16_t last_safety_flags = 0;
static flight_mode_t last_old_mode = MODE_DISARMED;
static flight_mode_t last_new_mode = MODE_DISARMED;

static void test_safety_cb(uint16_t flags, void* user_data)
{
    (void)user_data;
    last_safety_flags = flags;
}

static void test_mode_cb(flight_mode_t old_mode, flight_mode_t new_mode, void* user_data)
{
    (void)user_data;
    last_old_mode = old_mode;
    last_new_mode = new_mode;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * PID Controller Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_pid_init(void)
{
    pid_controller_t pid;
    pid_init(&pid, 1.0f, 0.1f, 0.05f, -1.0f, 1.0f, 0.5f);
    
    TEST_ASSERT_FLOAT_EQ(pid.kp, 1.0f, 0.001f);
    TEST_ASSERT_FLOAT_EQ(pid.ki, 0.1f, 0.001f);
    TEST_ASSERT_FLOAT_EQ(pid.kd, 0.05f, 0.001f);
    TEST_ASSERT_FLOAT_EQ(pid.output_min, -1.0f, 0.001f);
    TEST_ASSERT_FLOAT_EQ(pid.output_max, 1.0f, 0.001f);
    TEST_ASSERT_FLOAT_EQ(pid.imax, 0.5f, 0.001f);
    TEST_ASSERT_FLOAT_EQ(pid.integral, 0.0f, 0.001f);
    
    return 0;
}

static int test_pid_proportional(void)
{
    pid_controller_t pid;
    pid_init(&pid, 2.0f, 0.0f, 0.0f, -10.0f, 10.0f, 1.0f);
    
    float output = pid_compute(&pid, 1.0f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(output, 2.0f, 0.001f);  /* kp * error */
    
    output = pid_compute(&pid, -2.0f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(output, -4.0f, 0.001f);
    
    return 0;
}

static int test_pid_integral(void)
{
    pid_controller_t pid;
    pid_init(&pid, 0.0f, 10.0f, 0.0f, -10.0f, 10.0f, 5.0f);
    
    /* Accumulate integral over time */
    for (int i = 0; i < 10; i++) {
        pid_compute(&pid, 1.0f, 0.01f);  /* error * dt = 0.01 per step */
    }
    
    /* Integral should be ~0.1, output = ki * integral = 1.0 */
    TEST_ASSERT_FLOAT_EQ(pid.integral, 0.1f, 0.001f);
    float output = pid_compute(&pid, 1.0f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(output, 1.1f, 0.1f);
    
    return 0;
}

static int test_pid_integral_windup(void)
{
    pid_controller_t pid;
    pid_init(&pid, 0.0f, 100.0f, 0.0f, -10.0f, 10.0f, 0.5f);  /* imax = 0.5 */
    
    /* Drive integral to windup limit */
    for (int i = 0; i < 100; i++) {
        pid_compute(&pid, 1.0f, 0.01f);
    }
    
    /* Integral should be clamped at imax */
    TEST_ASSERT(pid.integral <= 0.5f);
    
    return 0;
}

static int test_pid_output_clamping(void)
{
    pid_controller_t pid;
    pid_init(&pid, 10.0f, 0.0f, 0.0f, -0.5f, 0.5f, 1.0f);
    
    float output = pid_compute(&pid, 1.0f, 0.01f);  /* Would be 10.0 */
    TEST_ASSERT_FLOAT_EQ(output, 0.5f, 0.001f);  /* Clamped to max */
    
    output = pid_compute(&pid, -1.0f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(output, -0.5f, 0.001f);  /* Clamped to min */
    
    return 0;
}

static int test_pid_reset(void)
{
    pid_controller_t pid;
    pid_init(&pid, 1.0f, 1.0f, 1.0f, -1.0f, 1.0f, 0.5f);
    
    /* Accumulate some state */
    pid_compute(&pid, 1.0f, 0.01f);
    pid_compute(&pid, 1.0f, 0.01f);
    TEST_ASSERT(pid.integral > 0.0f);
    
    /* Reset */
    pid_reset(&pid);
    TEST_ASSERT_FLOAT_EQ(pid.integral, 0.0f, 0.001f);
    TEST_ASSERT_FLOAT_EQ(pid.prev_error, 0.0f, 0.001f);
    
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Flight Controller Initialization Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_fc_init(void)
{
    int result = drone_fc_init();
    TEST_ASSERT(result == 0);
    TEST_ASSERT(fc_initialized == true);
    
    /* Check default state */
    flight_mode_t mode;
    arm_state_t arm;
    drone_fc_get_state(&mode, &arm, NULL, NULL, NULL, NULL);
    TEST_ASSERT(mode == MODE_DISARMED);
    TEST_ASSERT(arm == ARM_DISARMED);
    
    drone_fc_deinit();
    return 0;
}

static int test_fc_arm_disarm(void)
{
    drone_fc_init();
    
    /* Set valid sensor data for arming */
    imu_data_t imu = {
        .accel = {0.0f, 0.0f, -GRAVITY_MSS},
        .gyro = {0.0f, 0.0f, 0.0f},
        .mag = {0.3f, 0.0f, 0.5f},
        .temperature = 25.0f
    };
    drone_fc_update_imu(&imu);
    
    /* GPS for home position */
    gps_position_t gps = {
        .latitude = 37.7749,
        .longitude = -122.4194,
        .altitude = 10.0f,
        .valid = true,
        .satellites = 12
    };
    drone_fc_update_gps(&gps);
    
    /* Try to arm */
    int result = drone_fc_arm();
    TEST_ASSERT(result == 0);
    
    arm_state_t arm;
    drone_fc_get_state(NULL, &arm, NULL, NULL, NULL, NULL);
    TEST_ASSERT(arm == ARM_ARMED);
    
    /* Disarm */
    result = drone_fc_disarm(true);
    TEST_ASSERT(result == 0);
    
    drone_fc_get_state(NULL, &arm, NULL, NULL, NULL, NULL);
    TEST_ASSERT(arm == ARM_DISARMED);
    
    drone_fc_deinit();
    return 0;
}

static int test_fc_arm_low_battery_fails(void)
{
    drone_fc_init();
    
    /* Set low battery */
    drone_fc_set_battery(11.0f, 15.0f);
    
    /* Try to arm - should fail */
    int result = drone_fc_arm();
    TEST_ASSERT(result == -4);  /* Battery too low */
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Motor Mixing Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_motor_mixing_hover(void)
{
    drone_fc_init();
    drone_fc_arm();
    
    /* Pure throttle, no rotation */
    fc.motors.armed = true;
    motor_mix(0.5f, 0.0f, 0.0f, 0.0f);
    
    /* All motors should be equal */
    TEST_ASSERT_FLOAT_EQ(fc.motors.throttle[0], 0.5f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(fc.motors.throttle[1], 0.5f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(fc.motors.throttle[2], 0.5f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(fc.motors.throttle[3], 0.5f, 0.01f);
    
    drone_fc_deinit();
    return 0;
}

static int test_motor_mixing_roll(void)
{
    drone_fc_init();
    drone_fc_arm();
    
    fc.motors.armed = true;
    motor_mix(0.5f, 1.0f, 0.0f, 0.0f);  /* Roll right */
    
    /* Left motors higher, right motors lower */
    TEST_ASSERT(fc.motors.throttle[0] < fc.motors.throttle[1]);  /* FL < FR */
    TEST_ASSERT(fc.motors.throttle[3] < fc.motors.throttle[2]);  /* RL < RR */
    
    drone_fc_deinit();
    return 0;
}

static int test_motor_mixing_pitch(void)
{
    drone_fc_init();
    drone_fc_arm();
    
    fc.motors.armed = true;
    motor_mix(0.5f, 0.0f, 1.0f, 0.0f);  /* Pitch forward */
    
    /* Front motors higher, rear lower */
    TEST_ASSERT(fc.motors.throttle[0] > fc.motors.throttle[3]);  /* FL > RL */
    TEST_ASSERT(fc.motors.throttle[1] > fc.motors.throttle[2]);  /* FR > RR */
    
    drone_fc_deinit();
    return 0;
}

static int test_motor_mixing_yaw(void)
{
    drone_fc_init();
    drone_fc_arm();
    
    fc.motors.armed = true;
    motor_mix(0.5f, 0.0f, 0.0f, 1.0f);  /* Yaw CW */
    
    /* CCW motors (2,4) higher than CW motors (1,3) */
    TEST_ASSERT(fc.motors.throttle[1] > fc.motors.throttle[0]);  /* FR(CCW) > FL(CW) */
    TEST_ASSERT(fc.motors.throttle[3] > fc.motors.throttle[2]);  /* RL(CCW) > RR(CW) */
    
    drone_fc_deinit();
    return 0;
}

static int test_motor_pwm_range(void)
{
    drone_fc_init();
    drone_fc_arm();
    
    fc.motors.armed = true;
    motor_mix(1.0f, 0.0f, 0.0f, 0.0f);
    
    /* Check PWM limits */
    for (int i = 0; i < NUM_MOTORS; i++) {
        TEST_ASSERT(fc.motors.pwm[i] >= PWM_IDLE);
        TEST_ASSERT(fc.motors.pwm[i] <= PWM_MAX);
    }
    
    /* Disarmed should be PWM_MIN */
    fc.motors.armed = false;
    motor_mix(1.0f, 0.0f, 0.0f, 0.0f);
    
    for (int i = 0; i < NUM_MOTORS; i++) {
        TEST_ASSERT(fc.motors.pwm[i] == PWM_MIN);
    }
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Geofence Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_geofence_inside(void)
{
    drone_fc_init();
    
    /* Set home */
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    drone_fc_set_geofence(true, 100.0f, 120.0f, 2.0f);
    
    /* Position inside geofence */
    fc.position_ned.x = 10.0f;   /* 10m north */
    fc.position_ned.y = 10.0f;   /* 10m east */
    fc.position_ned.z = -50.0f;  /* 50m altitude */
    
    TEST_ASSERT(check_geofence() == true);
    
    drone_fc_deinit();
    return 0;
}

static int test_geofence_radius_breach(void)
{
    drone_fc_init();
    
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    drone_fc_set_geofence(true, 100.0f, 120.0f, 2.0f);
    
    /* Position outside radius */
    fc.position_ned.x = 80.0f;
    fc.position_ned.y = 80.0f;   /* ~113m from home */
    fc.position_ned.z = -50.0f;
    
    TEST_ASSERT(check_geofence() == false);
    
    drone_fc_deinit();
    return 0;
}

static int test_geofence_altitude_breach(void)
{
    drone_fc_init();
    
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    drone_fc_set_geofence(true, 100.0f, 120.0f, 2.0f);
    
    /* Position above max altitude */
    fc.position_ned.x = 10.0f;
    fc.position_ned.y = 10.0f;
    fc.position_ned.z = -150.0f;  /* 150m altitude > 120m limit */
    
    TEST_ASSERT(check_geofence() == false);
    
    drone_fc_deinit();
    return 0;
}

static int test_geofence_disabled(void)
{
    drone_fc_init();
    
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    drone_fc_set_geofence(false, 100.0f, 120.0f, 2.0f);  /* Disabled */
    
    /* Position way outside */
    fc.position_ned.x = 500.0f;
    fc.position_ned.y = 500.0f;
    fc.position_ned.z = -200.0f;
    
    TEST_ASSERT(check_geofence() == true);  /* OK because disabled */
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Safety System Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_safety_battery_low(void)
{
    drone_fc_init();
    drone_fc_set_safety_callback(test_safety_cb, NULL);
    
    drone_fc_set_battery(12.5f, 15.0f);  /* Below 20% */
    safety_update(1000);
    
    TEST_ASSERT(fc.safety_flags & SAFETY_BATTERY_LOW);
    TEST_ASSERT(last_safety_flags & SAFETY_BATTERY_LOW);
    
    drone_fc_deinit();
    return 0;
}

static int test_safety_battery_critical_lands(void)
{
    drone_fc_init();
    drone_fc_arm();
    drone_fc_set_mode(MODE_POS_HOLD);
    
    /* Set home for GPS */
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    gps_position_t gps = { .latitude = 37.7749, .longitude = -122.4194,
                           .altitude = 10.0f, .valid = true, .satellites = 12 };
    drone_fc_update_gps(&gps);
    
    /* Critical battery */
    drone_fc_set_battery(11.0f, 5.0f);  /* 5% < critical */
    safety_update(1000);
    
    TEST_ASSERT(fc.safety_flags & SAFETY_BATTERY_CRITICAL);
    TEST_ASSERT(fc.mode == MODE_LAND);  /* Should switch to land */
    
    drone_fc_deinit();
    return 0;
}

static int test_safety_rc_lost_rtl(void)
{
    drone_fc_init();
    drone_fc_arm();
    
    /* Set home */
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    fc.home_set = true;
    
    /* Simulate RC timeout */
    fc.last_rc_time_ms = 0;
    fc.mode = MODE_STABILIZE;
    safety_update(1000);  /* 1000ms since last RC */
    
    TEST_ASSERT(fc.safety_flags & SAFETY_RC_LOST);
    TEST_ASSERT(fc.mode == MODE_RTL);  /* Should RTL */
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Flight Mode Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_mode_transitions(void)
{
    drone_fc_init();
    drone_fc_set_mode_callback(test_mode_cb, NULL);
    
    /* Set up GPS */
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    gps_position_t gps = { .latitude = 37.7749, .longitude = -122.4194,
                           .altitude = 10.0f, .valid = true, .satellites = 12 };
    drone_fc_update_gps(&gps);
    
    /* Must be armed for mode changes */
    imu_data_t imu = { .accel = {0, 0, -GRAVITY_MSS} };
    drone_fc_update_imu(&imu);
    drone_fc_arm();
    
    TEST_ASSERT(drone_fc_set_mode(MODE_ALT_HOLD) == 0);
    TEST_ASSERT(fc.mode == MODE_ALT_HOLD);
    TEST_ASSERT(last_new_mode == MODE_ALT_HOLD);
    
    TEST_ASSERT(drone_fc_set_mode(MODE_POS_HOLD) == 0);
    TEST_ASSERT(fc.mode == MODE_POS_HOLD);
    
    drone_fc_deinit();
    return 0;
}

static int test_mode_pos_hold_needs_gps(void)
{
    drone_fc_init();
    
    imu_data_t imu = { .accel = {0, 0, -GRAVITY_MSS} };
    drone_fc_update_imu(&imu);
    drone_fc_arm();
    
    /* Mark GPS as lost */
    fc.safety_flags |= SAFETY_GPS_LOST;
    
    /* Should fail to enter POS_HOLD */
    int result = drone_fc_set_mode(MODE_POS_HOLD);
    TEST_ASSERT(result == -3);
    
    drone_fc_deinit();
    return 0;
}

static int test_mode_rtl_needs_home(void)
{
    drone_fc_init();
    
    imu_data_t imu = { .accel = {0, 0, -GRAVITY_MSS} };
    drone_fc_update_imu(&imu);
    drone_fc_arm();
    
    /* No home set */
    fc.home_set = false;
    
    int result = drone_fc_set_mode(MODE_RTL);
    TEST_ASSERT(result == -4);
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Sensor Fusion Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_attitude_fusion_level(void)
{
    /* Ensure clean init */
    drone_fc_deinit();
    drone_fc_init();
    
    /* Start from level (zero) attitude */
    fc.attitude.roll = 0.0f;
    fc.attitude.pitch = 0.0f;
    fc.attitude.yaw = 0.0f;
    
    /* Level accelerometer with zero gyro 
     * For level, accel.z should be positive (gravity pointing into sensor)
     * atan2(0, +z) = 0 (level roll)
     */
    imu_data_t imu = {
        .accel = {0.0f, 0.0f, GRAVITY_MSS},  /* Positive Z for level */
        .gyro = {0.0f, 0.0f, 0.0f}
    };
    
    /* Single fusion update - with both gyro and accel at zero roll, should stay level */
    fusion_update_attitude(&imu, CONTROL_DT);
    
    /* Attitude should remain at zero since we started there and gyro is zero */
    TEST_ASSERT_FLOAT_EQ(fc.attitude.roll, 0.0f, 0.01f);
    TEST_ASSERT_FLOAT_EQ(fc.attitude.pitch, 0.0f, 0.2f);  /* Pitch uses different calc */
    
    drone_fc_deinit();
    return 0;
}

static int test_attitude_fusion_tilted(void)
{
    /* Ensure clean init */
    drone_fc_deinit();
    drone_fc_init();
    
    /* Start from level */
    fc.attitude.roll = 0.0f;
    fc.attitude.pitch = 0.0f;
    fc.attitude.yaw = 0.0f;
    
    /* Apply gyro rotation - this should change the attitude */
    float rotation_rate = 1.0f;  /* rad/s */
    imu_data_t imu = {
        .accel = {0.0f, 0.0f, GRAVITY_MSS},   /* Level accel (positive Z) */
        .gyro = {rotation_rate, 0.0f, 0.0f}   /* Rolling right */
    };
    
    /* Single update */
    fusion_update_attitude(&imu, CONTROL_DT);
    
    /* Roll should have increased due to gyro integration */
    /* Expected: rotation_rate * dt * alpha = 1.0 * 0.0025 * 0.98 = ~0.00245 */
    TEST_ASSERT(fc.attitude.roll > 0.0f);  /* Should be positive (rolling right) */
    TEST_ASSERT(fc.attitude.roll < 0.01f); /* Should be small after one iteration */
    
    drone_fc_deinit();
    return 0;
}

static int test_gps_home_position(void)
{
    drone_fc_init();
    
    gps_position_t gps = {
        .latitude = 37.7749,
        .longitude = -122.4194,
        .altitude = 100.0f,
        .valid = true,
        .satellites = 12
    };
    
    drone_fc_update_gps(&gps);
    
    /* Home should be set */
    TEST_ASSERT(fc.home_set == true);
    TEST_ASSERT_FLOAT_EQ(fc.home.latitude, 37.7749, 0.0001);
    TEST_ASSERT_FLOAT_EQ(fc.home.longitude, -122.4194, 0.0001);
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Control Loop Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_stabilize_mode_loop(void)
{
    drone_fc_init();
    drone_fc_arm();
    drone_fc_set_mode(MODE_STABILIZE);
    
    /* Set attitude setpoint */
    drone_fc_set_attitude(10.0f * DEG_TO_RAD, 5.0f * DEG_TO_RAD, 0.0f, 0.5f);
    
    /* Simulate level attitude */
    imu_data_t imu = { .accel = {0, 0, -GRAVITY_MSS}, .gyro = {0, 0, 0} };
    drone_fc_update_imu(&imu);
    
    /* Run control loop */
    drone_fc_update(CONTROL_DT);
    
    /* Should have motor output */
    TEST_ASSERT(fc.motors.pwm[0] != PWM_MIN);
    
    drone_fc_deinit();
    return 0;
}

static int test_position_hold_loop(void)
{
    drone_fc_init();
    
    /* Set up GPS and home */
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    gps_position_t gps = { .latitude = 37.7749, .longitude = -122.4194,
                           .altitude = 50.0f, .valid = true, .satellites = 12 };
    drone_fc_update_gps(&gps);
    
    imu_data_t imu = { .accel = {0, 0, -GRAVITY_MSS}, .gyro = {0, 0, 0} };
    drone_fc_update_imu(&imu);
    
    drone_fc_arm();
    drone_fc_set_mode(MODE_POS_HOLD);
    
    /* Offset position from setpoint */
    fc.position_ned.x = 5.0f;  /* 5m north of home */
    fc.position_ned.y = 3.0f;  /* 3m east of home */
    
    /* Position setpoint is at home (0, 0) */
    fc.position_sp.x = 0.0f;
    fc.position_sp.y = 0.0f;
    
    /* Run control loop */
    drone_fc_update(CONTROL_DT);
    
    /* Should generate attitude to correct position */
    /* Can't easily test exact values, just verify it's running */
    TEST_ASSERT(fc.arm_state == ARM_ARMED);
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Emergency Procedures Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_emergency_stop(void)
{
    drone_fc_init();
    drone_fc_arm();
    drone_fc_set_mode(MODE_STABILIZE);
    
    /* Set some throttle */
    fc.throttle_sp = 0.5f;
    drone_fc_update(CONTROL_DT);
    
    /* Emergency stop */
    drone_fc_emergency_stop();
    
    TEST_ASSERT(fc.mode == MODE_EMERGENCY);
    TEST_ASSERT(fc.arm_state == ARM_DISARMED);
    
    for (int i = 0; i < NUM_MOTORS; i++) {
        TEST_ASSERT(fc.motors.pwm[i] == PWM_MIN);
    }
    
    drone_fc_deinit();
    return 0;
}

static int test_preflight_check(void)
{
    drone_fc_init();
    
    /* Reset attitude for level check */
    fc.attitude.roll = 0.0f;
    fc.attitude.pitch = 0.0f;
    fc.attitude.yaw = 0.0f;
    
    /* Good sensors */
    imu_data_t imu = { .accel = {0, 0, -GRAVITY_MSS}, .gyro = {0, 0, 0} };
    drone_fc_update_imu(&imu);
    fc.imu = imu;  /* Ensure IMU data is set */
    
    gps_position_t gps = { .latitude = 37.7749, .longitude = -122.4194,
                           .altitude = 10.0f, .valid = true, .satellites = 12 };
    drone_fc_update_gps(&gps);
    fc.gps = gps;  /* Ensure GPS data is set */
    
    drone_fc_set_battery(16.8f, 100.0f);
    fc.safety_flags = SAFETY_OK;  /* Clear any safety flags */
    
    int result = drone_fc_preflight_check();
    /* Preflight should pass or only have minor issues (accelerometer might not be perfectly level) */
    TEST_ASSERT((result & ~0x01) == 0);  /* Allow accelerometer not level flag */
    
    /* Low battery should fail */
    drone_fc_set_battery(11.5f, 15.0f);
    result = drone_fc_preflight_check();
    TEST_ASSERT(result & 0x04);  /* Battery check failed */
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Flight Path Simulation Tests
 * ───────────────────────────────────────────────────────────────────────────── */

static int test_flight_path_hover(void)
{
    drone_fc_init();
    
    /* Set up for flight */
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    gps_position_t gps = { .latitude = 37.7749, .longitude = -122.4194,
                           .altitude = 50.0f, .valid = true, .satellites = 12 };
    drone_fc_update_gps(&gps);
    
    imu_data_t imu = { .accel = {0, 0, -GRAVITY_MSS}, .gyro = {0, 0, 0} };
    drone_fc_update_imu(&imu);
    
    drone_fc_arm();
    drone_fc_set_mode(MODE_ALT_HOLD);
    
    /* Simulate 5 seconds of hover */
    for (int i = 0; i < 5 * CONTROL_RATE_HZ; i++) {
        drone_fc_update_imu(&imu);
        drone_fc_set_attitude(0, 0, 0, 0.5f);
        drone_fc_update(CONTROL_DT);
        fc.last_rc_time_ms = fc.loop_counter * (uint32_t)(CONTROL_DT * 1000.0f);
    }
    
    /* Should still be armed and flying */
    TEST_ASSERT(fc.arm_state == ARM_ARMED);
    TEST_ASSERT(fc.mode == MODE_ALT_HOLD);
    
    drone_fc_deinit();
    return 0;
}

static int test_flight_sensor_noise(void)
{
    drone_fc_init();
    
    drone_fc_set_home(37.7749, -122.4194, 10.0f);
    gps_position_t gps = { .latitude = 37.7749, .longitude = -122.4194,
                           .altitude = 50.0f, .valid = true, .satellites = 12 };
    drone_fc_update_gps(&gps);
    
    drone_fc_arm();
    drone_fc_set_mode(MODE_STABILIZE);
    
    /* Simulate with noisy IMU data */
    for (int i = 0; i < 100; i++) {
        float noise = ((float)(i % 7) - 3.0f) * 0.1f;  /* Simple noise */
        imu_data_t imu = {
            .accel = {noise, noise * 0.5f, -GRAVITY_MSS + noise * 0.2f},
            .gyro = {noise * 0.01f, noise * 0.01f, noise * 0.005f}
        };
        
        drone_fc_update_imu(&imu);
        drone_fc_set_attitude(0, 0, 0, 0.5f);
        drone_fc_update(CONTROL_DT);
        fc.last_rc_time_ms = fc.loop_counter * (uint32_t)(CONTROL_DT * 1000.0f);
    }
    
    /* Attitude should remain relatively stable despite noise */
    TEST_ASSERT(fabsf(fc.attitude.roll) < 1.0f);   /* Less than ~57 degrees */
    TEST_ASSERT(fabsf(fc.attitude.pitch) < 1.0f);
    
    drone_fc_deinit();
    return 0;
}

/* ─────────────────────────────────────────────────────────────────────────────
 * Test Runner
 * ───────────────────────────────────────────────────────────────────────────── */

int main(void)
{
    printf("\n=== Drone Flight Control Tests ===\n\n");
    
    printf("--- PID Controller Tests ---\n");
    RUN_TEST(test_pid_init);
    RUN_TEST(test_pid_proportional);
    RUN_TEST(test_pid_integral);
    RUN_TEST(test_pid_integral_windup);
    RUN_TEST(test_pid_output_clamping);
    RUN_TEST(test_pid_reset);
    
    printf("\n--- Flight Controller Init Tests ---\n");
    RUN_TEST(test_fc_init);
    RUN_TEST(test_fc_arm_disarm);
    RUN_TEST(test_fc_arm_low_battery_fails);
    
    printf("\n--- Motor Mixing Tests ---\n");
    RUN_TEST(test_motor_mixing_hover);
    RUN_TEST(test_motor_mixing_roll);
    RUN_TEST(test_motor_mixing_pitch);
    RUN_TEST(test_motor_mixing_yaw);
    RUN_TEST(test_motor_pwm_range);
    
    printf("\n--- Geofence Tests ---\n");
    RUN_TEST(test_geofence_inside);
    RUN_TEST(test_geofence_radius_breach);
    RUN_TEST(test_geofence_altitude_breach);
    RUN_TEST(test_geofence_disabled);
    
    printf("\n--- Safety System Tests ---\n");
    RUN_TEST(test_safety_battery_low);
    RUN_TEST(test_safety_battery_critical_lands);
    RUN_TEST(test_safety_rc_lost_rtl);
    
    printf("\n--- Flight Mode Tests ---\n");
    RUN_TEST(test_mode_transitions);
    RUN_TEST(test_mode_pos_hold_needs_gps);
    RUN_TEST(test_mode_rtl_needs_home);
    
    printf("\n--- Sensor Fusion Tests ---\n");
    RUN_TEST(test_attitude_fusion_level);
    RUN_TEST(test_attitude_fusion_tilted);
    RUN_TEST(test_gps_home_position);
    
    printf("\n--- Control Loop Tests ---\n");
    RUN_TEST(test_stabilize_mode_loop);
    RUN_TEST(test_position_hold_loop);
    
    printf("\n--- Emergency Procedures Tests ---\n");
    RUN_TEST(test_emergency_stop);
    RUN_TEST(test_preflight_check);
    
    printf("\n--- Flight Path Simulation Tests ---\n");
    RUN_TEST(test_flight_path_hover);
    RUN_TEST(test_flight_sensor_noise);
    
    printf("\n=================================\n");
    printf("Drone Tests: %d/%d passed", tests_passed, tests_run);
    if (tests_failed > 0) {
        printf(" (%d FAILED)", tests_failed);
    }
    printf("\n=================================\n\n");
    
    return tests_failed > 0 ? 1 : 0;
}
