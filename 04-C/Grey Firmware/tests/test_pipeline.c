/**
 * @file test_pipeline.c
 * @brief Unit Tests for Pipeline Leak Detection & Safety Spotlight
 * 
 * Tests cover:
 * - Initialization and configuration
 * - Sensor registration and updates
 * - Segment configuration
 * - Valve control
 * - Mass balance leak detection
 * - Pressure point analysis
 * - Negative pressure wave detection
 * - Rate of change monitoring
 * - Blockage detection
 * - Alert management
 * - Emergency shutdown
 * - Telemetry generation
 * - Leak and drift injection (simulation)
 * 
 * @author Grey Firmware Project
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <assert.h>

/* ============================================================================
 * Include the implementation directly for testing
 * ============================================================================ */
#include "../src/pipeline/pipeline_spotlight.c"

/* ============================================================================
 * Test Utilities
 * ============================================================================ */

static int tests_run = 0;
static int tests_passed = 0;

#define TEST_ASSERT(cond) do { \
    if (!(cond)) { \
        printf("  FAIL: %s (line %d)\n", #cond, __LINE__); \
        return 0; \
    } \
} while(0)

#define RUN_TEST(test_func) do { \
    printf("Running %s...\n", #test_func); \
    tests_run++; \
    memset(&g_pipe, 0, sizeof(g_pipe)); \
    if (test_func()) { \
        printf("  PASS\n"); \
        tests_passed++; \
    } else { \
        printf("  FAILED\n"); \
    } \
} while(0)

/* Helper to create a basic pipeline configuration */
static void setup_basic_pipeline(void) {
    pipe_init(1001, PRODUCT_CRUDE_OIL);
    
    /* Register flow sensors */
    pipe_sensor_config_t flow1 = {
        .sensor_id = 1,
        .type = SENSOR_FLOW,
        .location_km = 0.0f,
        .segment_id = 1,
        .low_limit = 0,
        .high_limit = 1000,
        .deadband = 0.5f,
        .sample_rate_hz = 1,
        .enabled = true,
        .calibrated = true
    };
    pipe_register_sensor(&flow1);
    
    pipe_sensor_config_t flow2 = {
        .sensor_id = 2,
        .type = SENSOR_FLOW,
        .location_km = 10.0f,
        .segment_id = 1,
        .low_limit = 0,
        .high_limit = 1000,
        .deadband = 0.5f,
        .sample_rate_hz = 1,
        .enabled = true,
        .calibrated = true
    };
    pipe_register_sensor(&flow2);
    
    /* Register pressure sensors */
    pipe_sensor_config_t press1 = {
        .sensor_id = 3,
        .type = SENSOR_PRESSURE,
        .location_km = 0.0f,
        .segment_id = 1,
        .low_limit = 1.0f,
        .high_limit = 100.0f,
        .deadband = 0.1f,
        .sample_rate_hz = 10,
        .enabled = true,
        .calibrated = true
    };
    pipe_register_sensor(&press1);
    
    pipe_sensor_config_t press2 = {
        .sensor_id = 4,
        .type = SENSOR_PRESSURE,
        .location_km = 5.0f,
        .segment_id = 1,
        .low_limit = 1.0f,
        .high_limit = 100.0f,
        .deadband = 0.1f,
        .sample_rate_hz = 10,
        .enabled = true,
        .calibrated = true
    };
    pipe_register_sensor(&press2);
    
    pipe_sensor_config_t press3 = {
        .sensor_id = 5,
        .type = SENSOR_PRESSURE,
        .location_km = 10.0f,
        .segment_id = 1,
        .low_limit = 1.0f,
        .high_limit = 100.0f,
        .deadband = 0.1f,
        .sample_rate_hz = 10,
        .enabled = true,
        .calibrated = true
    };
    pipe_register_sensor(&press3);
    
    /* Configure segment */
    pipe_segment_config_t seg1 = {
        .segment_id = 1,
        .name = "MainLine-1",
        .start_km = 0.0f,
        .end_km = 10.0f,
        .diameter_mm = 600,
        .wall_thickness_mm = 12.0f,
        .product = PRODUCT_CRUDE_OIL,
        .upstream_flow_sensor = 1,
        .downstream_flow_sensor = 2,
        .pressure_sensors = {3, 4, 5, 0},
        .pressure_sensor_count = 3,
        .imbalance_threshold = 5.0f,
        .settling_time_ms = 1000,
        .leak_detection_enabled = true
    };
    pipe_configure_segment(&seg1);
    
    /* Configure valve */
    pipe_valve_config_t valve1 = {
        .valve_id = 1,
        .name = "ESV-1",
        .location_km = 0.0f,
        .segment_up = 0,
        .segment_down = 1,
        .esv = true,
        .stroke_time_ms = 1000
    };
    pipe_configure_valve(&valve1);
    
    pipe_valve_config_t valve2 = {
        .valve_id = 2,
        .name = "ESV-2",
        .location_km = 10.0f,
        .segment_up = 1,
        .segment_down = 0,
        .esv = true,
        .stroke_time_ms = 1000
    };
    pipe_configure_valve(&valve2);
}

/* ============================================================================
 * Initialization Tests
 * ============================================================================ */

static int test_init_basic(void) {
    int result = pipe_init(1001, PRODUCT_CRUDE_OIL);
    TEST_ASSERT(result == 0);
    TEST_ASSERT(g_pipe.initialized == true);
    TEST_ASSERT(g_pipe.pipeline_id == 1001);
    TEST_ASSERT(g_pipe.product == PRODUCT_CRUDE_OIL);
    TEST_ASSERT(g_pipe.monitoring_active == false);
    pipe_shutdown();
    return 1;
}

static int test_init_double_fails(void) {
    TEST_ASSERT(pipe_init(1001, PRODUCT_CRUDE_OIL) == 0);
    TEST_ASSERT(pipe_init(1002, PRODUCT_NATURAL_GAS) == -1);
    pipe_shutdown();
    return 1;
}

static int test_shutdown(void) {
    TEST_ASSERT(pipe_init(1001, PRODUCT_CRUDE_OIL) == 0);
    TEST_ASSERT(pipe_shutdown() == 0);
    TEST_ASSERT(g_pipe.initialized == false);
    return 1;
}

static int test_shutdown_without_init(void) {
    TEST_ASSERT(pipe_shutdown() == -1);
    return 1;
}

/* ============================================================================
 * Sensor Tests
 * ============================================================================ */

static int test_register_sensor(void) {
    pipe_init(1001, PRODUCT_CRUDE_OIL);
    
    pipe_sensor_config_t config = {
        .sensor_id = 1,
        .type = SENSOR_FLOW,
        .location_km = 0.0f,
        .segment_id = 1,
        .low_limit = 0,
        .high_limit = 1000,
        .deadband = 0.5f,
        .sample_rate_hz = 1,
        .enabled = true,
        .calibrated = true
    };
    
    TEST_ASSERT(pipe_register_sensor(&config) == 0);
    TEST_ASSERT(g_pipe.sensor_count == 1);
    TEST_ASSERT(g_pipe.sensors[0].config.sensor_id == 1);
    TEST_ASSERT(g_pipe.sensors[0].online == true);
    
    pipe_shutdown();
    return 1;
}

static int test_sensor_max_limit(void) {
    pipe_init(1001, PRODUCT_CRUDE_OIL);
    
    for (uint8_t i = 0; i < PIPE_MAX_SENSORS; i++) {
        pipe_sensor_config_t config = {
            .sensor_id = i + 1,
            .type = SENSOR_PRESSURE,
            .enabled = true
        };
        TEST_ASSERT(pipe_register_sensor(&config) == 0);
    }
    
    pipe_sensor_config_t extra = { .sensor_id = 100 };
    TEST_ASSERT(pipe_register_sensor(&extra) == -2);
    
    pipe_shutdown();
    return 1;
}

static int test_update_sensor(void) {
    setup_basic_pipeline();
    
    TEST_ASSERT(pipe_update_sensor(1, 500.0f, 1.0f) == 0);
    
    pipe_sensor_t* sensor = pipe_find_sensor(1);
    TEST_ASSERT(sensor != NULL);
    TEST_ASSERT(sensor->last_reading.value == 500.0f);
    TEST_ASSERT(sensor->last_reading.quality == 1.0f);
    TEST_ASSERT(sensor->last_reading.valid == true);
    
    pipe_shutdown();
    return 1;
}

static int test_update_invalid_sensor(void) {
    setup_basic_pipeline();
    TEST_ASSERT(pipe_update_sensor(99, 100.0f, 1.0f) == -2);
    pipe_shutdown();
    return 1;
}

static int test_sensor_online_offline(void) {
    setup_basic_pipeline();
    
    pipe_sensor_t* sensor = pipe_find_sensor(1);
    TEST_ASSERT(sensor->online == true);
    
    TEST_ASSERT(pipe_set_sensor_online(1, false) == 0);
    TEST_ASSERT(sensor->online == false);
    
    TEST_ASSERT(pipe_set_sensor_online(1, true) == 0);
    TEST_ASSERT(sensor->online == true);
    
    pipe_shutdown();
    return 1;
}

static int test_sensor_history(void) {
    setup_basic_pipeline();
    
    /* Add multiple readings */
    for (int i = 0; i < 20; i++) {
        pipe_update_sensor(1, 100.0f + i, 1.0f);
    }
    
    pipe_sensor_t* sensor = pipe_find_sensor(1);
    TEST_ASSERT(sensor->history.count == 20);
    TEST_ASSERT(sensor->history.mean > 100.0f);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Segment Tests
 * ============================================================================ */

static int test_configure_segment(void) {
    pipe_init(1001, PRODUCT_CRUDE_OIL);
    
    pipe_segment_config_t config = {
        .segment_id = 1,
        .name = "TestSegment",
        .start_km = 0.0f,
        .end_km = 10.0f,
        .diameter_mm = 600,
        .product = PRODUCT_CRUDE_OIL,
        .imbalance_threshold = 5.0f
    };
    
    TEST_ASSERT(pipe_configure_segment(&config) == 0);
    TEST_ASSERT(g_pipe.segment_count == 1);
    TEST_ASSERT(g_pipe.total_length_km == 10.0f);
    
    pipe_shutdown();
    return 1;
}

static int test_segment_max_limit(void) {
    pipe_init(1001, PRODUCT_CRUDE_OIL);
    
    for (uint8_t i = 0; i < PIPE_MAX_SEGMENTS; i++) {
        pipe_segment_config_t config = {
            .segment_id = i + 1,
            .end_km = (float)(i + 1) * 10
        };
        TEST_ASSERT(pipe_configure_segment(&config) == 0);
    }
    
    pipe_segment_config_t extra = { .segment_id = 100 };
    TEST_ASSERT(pipe_configure_segment(&extra) == -2);
    
    pipe_shutdown();
    return 1;
}

static int test_segment_status_query(void) {
    setup_basic_pipeline();
    
    pipe_seg_status_t status;
    float imbalance, leak_rate;
    
    TEST_ASSERT(pipe_get_segment_status(1, &status, &imbalance, &leak_rate) == 0);
    TEST_ASSERT(status == SEG_STATUS_NORMAL);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Valve Tests
 * ============================================================================ */

static int test_configure_valve(void) {
    pipe_init(1001, PRODUCT_CRUDE_OIL);
    
    pipe_valve_config_t config = {
        .valve_id = 1,
        .name = "ESV-1",
        .location_km = 5.0f,
        .esv = true,
        .stroke_time_ms = 2000
    };
    
    TEST_ASSERT(pipe_configure_valve(&config) == 0);
    TEST_ASSERT(g_pipe.valve_count == 1);
    TEST_ASSERT(g_pipe.valves[0].state == VALVE_OPEN);
    
    pipe_shutdown();
    return 1;
}

static int test_valve_control(void) {
    setup_basic_pipeline();
    
    TEST_ASSERT(pipe_control_valve(1, false) == 0);
    
    pipe_valve_t* valve = pipe_find_valve(1);
    TEST_ASSERT(valve->state == VALVE_CLOSING);
    TEST_ASSERT(valve->commanded == VALVE_CLOSED);
    
    /* Simulate time passing for valve to close */
    pipe_process(1500);
    
    TEST_ASSERT(valve->state == VALVE_CLOSED);
    
    pipe_shutdown();
    return 1;
}

static int test_valve_open(void) {
    setup_basic_pipeline();
    
    /* Close valve first */
    pipe_control_valve(1, false);
    pipe_process(1500);
    
    pipe_valve_t* valve = pipe_find_valve(1);
    TEST_ASSERT(valve->state == VALVE_CLOSED);
    
    /* Now open */
    TEST_ASSERT(pipe_control_valve(1, true) == 0);
    TEST_ASSERT(valve->state == VALVE_OPENING);
    
    pipe_process(1500);
    TEST_ASSERT(valve->state == VALVE_OPEN);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Monitoring Tests
 * ============================================================================ */

static int test_start_monitoring(void) {
    setup_basic_pipeline();
    
    TEST_ASSERT(pipe_start_monitoring() == 0);
    TEST_ASSERT(g_pipe.monitoring_active == true);
    
    pipe_shutdown();
    return 1;
}

static int test_stop_monitoring(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    TEST_ASSERT(pipe_stop_monitoring() == 0);
    TEST_ASSERT(g_pipe.monitoring_active == false);
    
    pipe_shutdown();
    return 1;
}

static int test_process_updates_time(void) {
    setup_basic_pipeline();
    
    uint32_t before = g_pipe.current_time_ms;
    pipe_process(1000);
    
    TEST_ASSERT(g_pipe.current_time_ms == before + 1000);
    TEST_ASSERT(g_pipe.stats.uptime_seconds == 1);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Mass Balance Detection Tests
 * ============================================================================ */

static int test_mass_balance_normal(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Normal flow - input equals output */
    pipe_update_sensor(1, 500.0f, 1.0f);  /* Flow in */
    pipe_update_sensor(2, 500.0f, 1.0f);  /* Flow out */
    
    /* Wait for settling time */
    pipe_process(2000);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->status == SEG_STATUS_NORMAL);
    TEST_ASSERT(fabsf(seg->imbalance_pct) < 1.0f);
    
    pipe_shutdown();
    return 1;
}

static int test_mass_balance_minor_leak(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Small imbalance - 2% loss */
    pipe_update_sensor(1, 500.0f, 1.0f);  /* Flow in */
    pipe_update_sensor(2, 490.0f, 1.0f);  /* Flow out */
    
    pipe_process(2000);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->imbalance_pct > 1.0f && seg->imbalance_pct < 5.0f);
    
    pipe_shutdown();
    return 1;
}

static int test_mass_balance_major_leak(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Large imbalance - 10% loss */
    pipe_update_sensor(1, 500.0f, 1.0f);  /* Flow in */
    pipe_update_sensor(2, 450.0f, 1.0f);  /* Flow out */
    
    pipe_process(2000);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->leak_suspected == true);
    TEST_ASSERT(seg->detection_method == DETECT_MASS_BALANCE);
    TEST_ASSERT(seg->leak_severity == LEAK_MAJOR);
    
    pipe_shutdown();
    return 1;
}

static int test_mass_balance_rupture(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Massive imbalance - 30% loss */
    pipe_update_sensor(1, 500.0f, 1.0f);  /* Flow in */
    pipe_update_sensor(2, 350.0f, 1.0f);  /* Flow out */
    
    pipe_process(2000);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->leak_suspected == true);
    TEST_ASSERT(seg->leak_severity == LEAK_RUPTURE);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Pressure Analysis Tests
 * ============================================================================ */

static int test_pressure_gradient_normal(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Normal pressure gradient along pipe */
    pipe_update_sensor(3, 50.0f, 1.0f);  /* Pressure at 0km */
    pipe_update_sensor(4, 48.0f, 1.0f);  /* Pressure at 5km */
    pipe_update_sensor(5, 46.0f, 1.0f);  /* Pressure at 10km */
    
    pipe_update_sensor(1, 500.0f, 1.0f);
    pipe_update_sensor(2, 500.0f, 1.0f);
    
    pipe_process(2000);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->pressure_gradient > 0);
    TEST_ASSERT(seg->avg_pressure > 40.0f);
    
    pipe_shutdown();
    return 1;
}

static int test_pressure_high_warning(void) {
    setup_basic_pipeline();
    
    /* High pressure reading */
    pipe_update_sensor(3, 95.0f, 1.0f);
    
    /* Check event was logged */
    TEST_ASSERT(g_pipe.event_count > 0);
    
    pipe_shutdown();
    return 1;
}

static int test_pressure_low_warning(void) {
    setup_basic_pipeline();
    
    /* Low pressure reading - use the actual low_limit from config */
    pipe_update_sensor(3, 0.5f, 1.0f);  /* Below low_limit of 1.0 */
    
    /* Check event was logged */
    bool found_low_pressure = false;
    for (uint16_t i = 0; i < g_pipe.event_count; i++) {
        if (g_pipe.events[i].type == EVENT_PRESSURE_LOW) {
            found_low_pressure = true;
            break;
        }
    }
    TEST_ASSERT(found_low_pressure);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * NPW Detection Tests
 * ============================================================================ */

static int test_npw_detection(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Build history with stable pressure */
    for (int i = 0; i < 20; i++) {
        pipe_update_sensor(3, 50.0f, 1.0f);
        pipe_update_sensor(1, 500.0f, 1.0f);
        pipe_update_sensor(2, 500.0f, 1.0f);
        pipe_process(1000);
    }
    
    /* Sudden pressure drop (simulating rupture) */
    for (int i = 0; i < 12; i++) {
        pipe_update_sensor(3, 50.0f - (i * 2.0f), 1.0f);  /* Rapid drop */
        pipe_process(1000);
    }
    
    pipe_sensor_t* sensor = pipe_find_sensor(3);
    /* Check that trend is negative (dropping) */
    TEST_ASSERT(sensor->history.trend < 0);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Alert Tests
 * ============================================================================ */

static int test_alert_creation(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Trigger a leak condition */
    pipe_update_sensor(1, 500.0f, 1.0f);
    pipe_update_sensor(2, 400.0f, 1.0f);  /* 20% loss */
    
    pipe_process(2000);
    
    TEST_ASSERT(g_pipe.stats.alerts_generated > 0);
    
    pipe_alert_t alerts[8];
    uint8_t count;
    TEST_ASSERT(pipe_get_alerts(alerts, 8, &count) == 0);
    TEST_ASSERT(count > 0);
    TEST_ASSERT(alerts[0].segment_id == 1);
    TEST_ASSERT(alerts[0].active == true);
    
    pipe_shutdown();
    return 1;
}

static int test_alert_acknowledge(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    pipe_update_sensor(1, 500.0f, 1.0f);
    pipe_update_sensor(2, 400.0f, 1.0f);
    pipe_process(2000);
    
    pipe_alert_t alerts[8];
    uint8_t count;
    pipe_get_alerts(alerts, 8, &count);
    TEST_ASSERT(count > 0);
    
    TEST_ASSERT(pipe_acknowledge_alert(alerts[0].alert_id) == 0);
    
    /* Verify acknowledged */
    pipe_get_alerts(alerts, 8, &count);
    TEST_ASSERT(alerts[0].acknowledged == true);
    
    pipe_shutdown();
    return 1;
}

static int test_alert_clear_on_recovery(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Create leak condition */
    pipe_update_sensor(1, 500.0f, 1.0f);
    pipe_update_sensor(2, 400.0f, 1.0f);
    pipe_process(2000);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->leak_suspected == true);
    
    /* Restore normal flow */
    pipe_update_sensor(2, 500.0f, 1.0f);
    pipe_process(1000);
    
    TEST_ASSERT(seg->leak_suspected == false);
    TEST_ASSERT(seg->status == SEG_STATUS_NORMAL);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Emergency Shutdown Tests
 * ============================================================================ */

static int test_emergency_shutdown(void) {
    setup_basic_pipeline();
    
    TEST_ASSERT(pipe_trigger_shutdown(1) == 0);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->status == SEG_STATUS_SHUTDOWN);
    TEST_ASSERT(g_pipe.stats.shutdowns == 1);
    
    /* Check valves are closing */
    pipe_valve_t* valve1 = pipe_find_valve(1);
    pipe_valve_t* valve2 = pipe_find_valve(2);
    TEST_ASSERT(valve1->state == VALVE_CLOSING || valve1->commanded == VALVE_CLOSED);
    TEST_ASSERT(valve2->state == VALVE_CLOSING || valve2->commanded == VALVE_CLOSED);
    
    pipe_shutdown();
    return 1;
}

static int test_auto_shutdown_on_major_leak(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Create confirmed major leak */
    pipe_update_sensor(1, 500.0f, 1.0f);
    pipe_update_sensor(2, 350.0f, 1.0f);  /* 30% loss */
    
    pipe_process(2000);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    /* Major leak should trigger segment to alarm/shutdown */
    TEST_ASSERT(seg->status == SEG_STATUS_WARNING || 
                seg->status == SEG_STATUS_ALARM ||
                seg->status == SEG_STATUS_SHUTDOWN);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Maintenance Tests
 * ============================================================================ */

static int test_maintenance_mode(void) {
    setup_basic_pipeline();
    
    TEST_ASSERT(pipe_set_maintenance(1, true) == 0);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->status == SEG_STATUS_MAINTENANCE);
    TEST_ASSERT(seg->config.leak_detection_enabled == false);
    
    pipe_shutdown();
    return 1;
}

static int test_exit_maintenance(void) {
    setup_basic_pipeline();
    
    pipe_set_maintenance(1, true);
    TEST_ASSERT(pipe_set_maintenance(1, false) == 0);
    
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->status == SEG_STATUS_NORMAL);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Statistics Tests
 * ============================================================================ */

static int test_get_stats(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    pipe_update_sensor(1, 500.0f, 1.0f);
    pipe_update_sensor(2, 500.0f, 1.0f);
    pipe_process(5000);
    
    pipe_stats_t stats;
    TEST_ASSERT(pipe_get_stats(&stats) == 0);
    TEST_ASSERT(stats.uptime_seconds == 5);
    TEST_ASSERT(stats.readings_processed >= 2);
    
    pipe_shutdown();
    return 1;
}

static int test_throughput_tracking(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Flow 500 m³/h for 1 hour (3600 seconds) */
    for (int i = 0; i < 3600; i++) {
        pipe_update_sensor(1, 500.0f, 1.0f);
        pipe_update_sensor(2, 500.0f, 1.0f);
        pipe_process(1000);
    }
    
    pipe_stats_t stats;
    pipe_get_stats(&stats);
    
    /* Should have ~500 m³ throughput */
    TEST_ASSERT(stats.total_product_through > 400.0f);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Event Log Tests
 * ============================================================================ */

static int test_event_logging(void) {
    setup_basic_pipeline();
    
    uint16_t initial_count = g_pipe.event_count;
    
    /* Actions that generate events */
    pipe_set_sensor_online(1, false);
    pipe_set_sensor_online(1, true);
    
    TEST_ASSERT(g_pipe.event_count > initial_count);
    
    pipe_shutdown();
    return 1;
}

static int test_get_events(void) {
    setup_basic_pipeline();
    
    pipe_event_t events[64];
    uint16_t count;
    
    TEST_ASSERT(pipe_get_events(events, 64, &count) == 0);
    TEST_ASSERT(count > 0);  /* At least init events */
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Telemetry Tests
 * ============================================================================ */

static int test_generate_telemetry(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    uint8_t buffer[256];
    uint16_t len;
    
    TEST_ASSERT(pipe_generate_telemetry(buffer, 256, &len) == 0);
    TEST_ASSERT(len > 12);  /* At least header */
    
    /* Check header */
    TEST_ASSERT(buffer[0] == 'P');
    TEST_ASSERT(buffer[1] == 'I');
    TEST_ASSERT(buffer[2] == 'P');
    TEST_ASSERT(buffer[3] == 'E');
    
    pipe_shutdown();
    return 1;
}

static int test_telemetry_buffer_too_small(void) {
    setup_basic_pipeline();
    
    uint8_t buffer[64];
    uint16_t len;
    
    TEST_ASSERT(pipe_generate_telemetry(buffer, 64, &len) == -2);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Injection Tests (Simulation)
 * ============================================================================ */

static int test_inject_leak(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* Set initial flow */
    pipe_update_sensor(1, 500.0f, 1.0f);
    pipe_update_sensor(2, 500.0f, 1.0f);
    
    /* Inject leak */
    TEST_ASSERT(pipe_inject_leak(1, 50.0f, 5.0f) == 0);
    
    /* Check downstream flow reduced */
    pipe_sensor_t* flow_out = pipe_find_sensor(2);
    TEST_ASSERT(flow_out->last_reading.value < 500.0f);
    
    pipe_shutdown();
    return 1;
}

static int test_inject_drift(void) {
    setup_basic_pipeline();
    
    pipe_update_sensor(1, 500.0f, 1.0f);
    
    TEST_ASSERT(pipe_inject_drift(1, 10.0f) == 0);
    
    pipe_sensor_t* sensor = pipe_find_sensor(1);
    TEST_ASSERT(sensor->last_reading.value == 510.0f);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Blockage Detection Tests
 * ============================================================================ */

static int test_blockage_detection(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    /* High pressure with flow reduction suggests blockage */
    pipe_update_sensor(3, 95.0f, 1.0f);  /* High pressure */
    pipe_update_sensor(4, 90.0f, 1.0f);
    pipe_update_sensor(5, 85.0f, 1.0f);
    
    pipe_update_sensor(1, 500.0f, 1.0f);  /* Flow in */
    pipe_update_sensor(2, 300.0f, 1.0f);  /* Flow out reduced */
    
    pipe_process(2000);
    
    /* Should detect blockage condition */
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg->status != SEG_STATUS_NORMAL);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Edge Cases
 * ============================================================================ */

static int test_zero_flow(void) {
    setup_basic_pipeline();
    pipe_start_monitoring();
    
    pipe_update_sensor(1, 0.0f, 1.0f);
    pipe_update_sensor(2, 0.0f, 1.0f);
    
    pipe_process(2000);
    
    /* Should not crash with zero flow */
    pipe_segment_t* seg = pipe_find_segment(1);
    TEST_ASSERT(seg != NULL);
    
    pipe_shutdown();
    return 1;
}

static int test_negative_values(void) {
    setup_basic_pipeline();
    
    /* Negative flow (reverse flow condition) */
    TEST_ASSERT(pipe_update_sensor(1, -50.0f, 1.0f) == 0);
    
    pipe_shutdown();
    return 1;
}

static int test_low_quality_readings(void) {
    setup_basic_pipeline();
    
    /* Low quality reading should be marked invalid */
    TEST_ASSERT(pipe_update_sensor(1, 500.0f, 0.3f) == 0);
    
    pipe_sensor_t* sensor = pipe_find_sensor(1);
    TEST_ASSERT(sensor->last_reading.valid == false);
    
    pipe_shutdown();
    return 1;
}

static int test_high_quality_readings(void) {
    setup_basic_pipeline();
    
    TEST_ASSERT(pipe_update_sensor(1, 500.0f, 0.9f) == 0);
    
    pipe_sensor_t* sensor = pipe_find_sensor(1);
    TEST_ASSERT(sensor->last_reading.valid == true);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Multi-Segment Tests
 * ============================================================================ */

static int test_multi_segment_pipeline(void) {
    pipe_init(1001, PRODUCT_CRUDE_OIL);
    
    /* Configure 3 segments */
    for (uint8_t i = 1; i <= 3; i++) {
        pipe_segment_config_t seg = {
            .segment_id = i,
            .start_km = (float)(i - 1) * 10,
            .end_km = (float)i * 10,
            .imbalance_threshold = 5.0f,
            .settling_time_ms = 1000
        };
        strncpy(seg.name, "Segment", sizeof(seg.name));
        TEST_ASSERT(pipe_configure_segment(&seg) == 0);
    }
    
    TEST_ASSERT(g_pipe.segment_count == 3);
    TEST_ASSERT(g_pipe.total_length_km == 30.0f);
    
    pipe_shutdown();
    return 1;
}

/* ============================================================================
 * Main Test Runner
 * ============================================================================ */

int main(void) {
    printf("========================================\n");
    printf("Pipeline Leak Detection Test Suite\n");
    printf("========================================\n\n");
    
    /* Initialization Tests */
    printf("--- Initialization Tests ---\n");
    RUN_TEST(test_init_basic);
    RUN_TEST(test_init_double_fails);
    RUN_TEST(test_shutdown);
    RUN_TEST(test_shutdown_without_init);
    
    /* Sensor Tests */
    printf("\n--- Sensor Tests ---\n");
    RUN_TEST(test_register_sensor);
    RUN_TEST(test_sensor_max_limit);
    RUN_TEST(test_update_sensor);
    RUN_TEST(test_update_invalid_sensor);
    RUN_TEST(test_sensor_online_offline);
    RUN_TEST(test_sensor_history);
    
    /* Segment Tests */
    printf("\n--- Segment Tests ---\n");
    RUN_TEST(test_configure_segment);
    RUN_TEST(test_segment_max_limit);
    RUN_TEST(test_segment_status_query);
    
    /* Valve Tests */
    printf("\n--- Valve Tests ---\n");
    RUN_TEST(test_configure_valve);
    RUN_TEST(test_valve_control);
    RUN_TEST(test_valve_open);
    
    /* Monitoring Tests */
    printf("\n--- Monitoring Tests ---\n");
    RUN_TEST(test_start_monitoring);
    RUN_TEST(test_stop_monitoring);
    RUN_TEST(test_process_updates_time);
    
    /* Mass Balance Tests */
    printf("\n--- Mass Balance Detection Tests ---\n");
    RUN_TEST(test_mass_balance_normal);
    RUN_TEST(test_mass_balance_minor_leak);
    RUN_TEST(test_mass_balance_major_leak);
    RUN_TEST(test_mass_balance_rupture);
    
    /* Pressure Tests */
    printf("\n--- Pressure Analysis Tests ---\n");
    RUN_TEST(test_pressure_gradient_normal);
    RUN_TEST(test_pressure_high_warning);
    RUN_TEST(test_pressure_low_warning);
    
    /* NPW Tests */
    printf("\n--- NPW Detection Tests ---\n");
    RUN_TEST(test_npw_detection);
    
    /* Alert Tests */
    printf("\n--- Alert Tests ---\n");
    RUN_TEST(test_alert_creation);
    RUN_TEST(test_alert_acknowledge);
    RUN_TEST(test_alert_clear_on_recovery);
    
    /* Shutdown Tests */
    printf("\n--- Emergency Shutdown Tests ---\n");
    RUN_TEST(test_emergency_shutdown);
    RUN_TEST(test_auto_shutdown_on_major_leak);
    
    /* Maintenance Tests */
    printf("\n--- Maintenance Tests ---\n");
    RUN_TEST(test_maintenance_mode);
    RUN_TEST(test_exit_maintenance);
    
    /* Statistics Tests */
    printf("\n--- Statistics Tests ---\n");
    RUN_TEST(test_get_stats);
    RUN_TEST(test_throughput_tracking);
    
    /* Event Tests */
    printf("\n--- Event Log Tests ---\n");
    RUN_TEST(test_event_logging);
    RUN_TEST(test_get_events);
    
    /* Telemetry Tests */
    printf("\n--- Telemetry Tests ---\n");
    RUN_TEST(test_generate_telemetry);
    RUN_TEST(test_telemetry_buffer_too_small);
    
    /* Injection Tests */
    printf("\n--- Injection Tests ---\n");
    RUN_TEST(test_inject_leak);
    RUN_TEST(test_inject_drift);
    
    /* Blockage Tests */
    printf("\n--- Blockage Detection Tests ---\n");
    RUN_TEST(test_blockage_detection);
    
    /* Edge Cases */
    printf("\n--- Edge Case Tests ---\n");
    RUN_TEST(test_zero_flow);
    RUN_TEST(test_negative_values);
    RUN_TEST(test_low_quality_readings);
    RUN_TEST(test_high_quality_readings);
    
    /* Multi-Segment Tests */
    printf("\n--- Multi-Segment Tests ---\n");
    RUN_TEST(test_multi_segment_pipeline);
    
    /* Summary */
    printf("\n========================================\n");
    printf("Test Results: %d/%d passed\n", tests_passed, tests_run);
    printf("========================================\n");
    
    return (tests_passed == tests_run) ? 0 : 1;
}
