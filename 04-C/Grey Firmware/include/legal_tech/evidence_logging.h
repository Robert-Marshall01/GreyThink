/**
 * @file evidence_logging.h
 * @brief Secure Evidence Logging for Legal Technology
 *
 * INDUSTRY RELEVANCE:
 * Digital evidence must maintain chain of custody and integrity for court
 * admissibility. This module demonstrates expertise in forensically-sound
 * data capture used by law enforcement body cameras, legal discovery
 * systems, and compliance recording platforms.
 *
 * Key capabilities demonstrated:
 * - Cryptographic integrity (SHA-256 hash chains)
 * - Tamper-evident storage
 * - Chain of custody tracking
 * - FRE 901(b)(9) authentication compliance
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_EVIDENCE_LOGGING_H
#define GF_EVIDENCE_LOGGING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Evidence types */
typedef enum {
    GF_EVIDENCE_VIDEO,
    GF_EVIDENCE_AUDIO,
    GF_EVIDENCE_DOCUMENT,
    GF_EVIDENCE_SENSOR_DATA,
    GF_EVIDENCE_METADATA
} gf_evidence_type_t;

/** Chain of custody event */
typedef enum {
    GF_CUSTODY_CAPTURE,             /**< Initial recording */
    GF_CUSTODY_TRANSFER,            /**< Transferred to custodian */
    GF_CUSTODY_ACCESS,              /**< Accessed for review */
    GF_CUSTODY_EXPORT,              /**< Exported for proceedings */
    GF_CUSTODY_SEAL                 /**< Sealed for storage */
} gf_custody_event_t;

/** Evidence record */
typedef struct {
    uint64_t evidence_id;
    gf_evidence_type_t type;
    uint64_t timestamp_capture;
    char case_number[32];
    char officer_badge[16];
    uint8_t sha256_hash[32];
    uint32_t size_bytes;
    bool integrity_verified;
} gf_evidence_record_t;

/** Custody log entry */
typedef struct {
    uint64_t evidence_id;
    gf_custody_event_t event;
    uint64_t timestamp;
    char custodian_id[32];
    char notes[128];
    uint8_t signature[64];
} gf_custody_entry_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_evidence_init(void);
int gf_evidence_begin_capture(gf_evidence_type_t type, const char* case_number);
int gf_evidence_append_data(const void* data, size_t len);
int gf_evidence_finalize(gf_evidence_record_t* record);
int gf_evidence_log_custody(const gf_custody_entry_t* entry);
int gf_evidence_verify(uint64_t evidence_id);
void gf_evidence_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EVIDENCE_LOGGING_H */

    ABORT_THRUST_HIGH,
    ABORT_THRUST_ASYMMETRY,
    ABORT_CHAMBER_OVERPRESSURE,
    ABORT_CHAMBER_OVERTEMP,
    ABORT_TURBOPUMP_OVERTEMP,
    ABORT_VIBRATION_EXCESSIVE,
    ABORT_PROPELLANT_LEAK,
    ABORT_IGNITION_FAILURE,
    ABORT_GUIDANCE_FAILURE,
    ABORT_SENSOR_FAILURE,
    ABORT_RANGE_SAFETY,
    ABORT_MANUAL
} abort_type_t;

typedef enum {
    SEVERITY_INFO,
    SEVERITY_CAUTION,
    SEVERITY_WARNING,
    SEVERITY_ABORT
} alarm_severity_t;

typedef enum {
    SENSOR_THRUST_MAIN,
    SENSOR_CHAMBER_PRESSURE,
    SENSOR_CHAMBER_TEMP,
    SENSOR_NOZZLE_TEMP,
    SENSOR_TURBOPUMP_TEMP,
    SENSOR_INJECTOR_PRESSURE,
    SENSOR_OXIDIZER_FLOW,
    SENSOR_FUEL_FLOW,
    SENSOR_VIBRATION_AXIAL,
    SENSOR_VIBRATION_LATERAL,
    SENSOR_PROPELLANT_TANK_PRESS,
    SENSOR_PROPELLANT_TANK_TEMP,
    SENSOR_TYPE_COUNT
} sensor_type_t;

typedef struct {
    uint8_t engine_id;
    engine_state_t state;
    float thrust_kn;
    float thrust_pct;
    float chamber_pressure_psi;
    float chamber_temp_c;
    float nozzle_temp_c;
    float turbopump_temp_c;
    float injector_dp_pct;
    float oxidizer_flow_kg_s;
    float fuel_flow_kg_s;
    float mixture_ratio;
    float vibration_g;
    float isp_s;
    uint32_t run_time_ms;
    bool healthy;
} engine_status_t;

typedef struct {
    bool armed;
    bool abort_triggered;
    abort_type_t abort_reason;
    uint8_t abort_engine_id;
    uint64_t abort_timestamp_ns;
    uint32_t abort_latency_us;
    uint8_t tmr_health[3];
    bool all_channels_healthy;
    bool fts_armed;
    uint32_t safety_checks_per_sec;
} safety_status_t;

typedef struct {
    uint64_t frames_generated;
    uint64_t frames_transmitted;
    uint64_t frames_dropped;
    uint32_t current_rate_hz;
    float buffer_utilization;
} tlm_stats_t;

typedef struct {
    uint32_t alarm_id;
    abort_type_t type;
    alarm_severity_t severity;
    uint8_t engine_id;
    uint64_t timestamp_ns;
    float value;
    float threshold;
    bool active;
    bool acknowledged;
} alarm_t;

typedef struct {
    uint32_t event_id;
    uint64_t timestamp_ns;
    const char* message;
    uint8_t engine_id;
    float value;
} event_t;

/*===========================================================================*/
/* External Function Declarations                                             */
/*===========================================================================*/

extern int rocket_init(void);
extern int rocket_configure_vehicle(uint16_t vehicle_id, const char* name,
                                    uint8_t engine_count, bool human_rated);
extern int rocket_configure_engine(uint8_t engine_id, const char* name,
                                   float nominal_thrust_kn, float max_thrust_kn);
extern int rocket_safety_arm(void);
extern int rocket_get_safety_status(safety_status_t* status);
extern int rocket_get_engine_status(uint8_t engine_id, engine_status_t* status);
extern launch_phase_t rocket_get_phase(void);
extern int rocket_start_ignition(void);
extern int rocket_abort(abort_type_t reason);
extern int rocket_set_phase(launch_phase_t phase);
extern int rocket_inject_sensor(uint8_t engine_id, sensor_type_t type,
                                uint8_t channel, float value);
extern int rocket_inject_engine_state(uint8_t engine_id, engine_state_t state,
                                      float thrust_pct, float chamber_temp_c,
                                      float vibration_g);
extern int rocket_get_tlm_stats(tlm_stats_t* stats);
extern int rocket_generate_telemetry(void* buffer, size_t buffer_len, size_t* frame_len);
extern int rocket_get_alarms(alarm_t* alarms, uint8_t max_alarms, uint8_t* count);
extern int rocket_get_events(event_t* events, uint16_t max_events, uint16_t* count);
extern int rocket_check_readiness(bool* ready, char* reason, size_t reason_len);
extern int rocket_process(uint32_t delta_ms);
extern int rocket_shutdown(void);

/*===========================================================================*/
/* Initialization Tests                                                       */
/*===========================================================================*/

static int test_init_success(void) {
    TEST_ASSERT_EQ_INT(0, rocket_init(), "Init should succeed");
    TEST_ASSERT_EQ_INT(PHASE_PRELAUNCH, rocket_get_phase(), "Phase should be PRELAUNCH");
    TEST_ASSERT_EQ_INT(0, rocket_shutdown(), "Shutdown should succeed");
    return 0;
}

static int test_double_init_fails(void) {
    TEST_ASSERT_EQ_INT(0, rocket_init(), "First init should succeed");
    TEST_ASSERT_EQ_INT(-1, rocket_init(), "Second init should fail");
    rocket_shutdown();
    return 0;
}

static int test_shutdown_before_init_fails(void) {
    TEST_ASSERT_EQ_INT(-1, rocket_shutdown(), "Shutdown before init should fail");
    return 0;
}

static int test_vehicle_configuration(void) {
    rocket_init();
    
    TEST_ASSERT_EQ_INT(0, rocket_configure_vehicle(1001, "Falcon Heavy", 9, false),
                       "Vehicle config OK");
    
    /* Configure engines */
    for (int i = 0; i < 9; i++) {
        TEST_ASSERT_EQ_INT(0, rocket_configure_engine(i, "Merlin 1D", 845.0f, 914.0f),
                           "Engine config OK");
    }
    
    rocket_shutdown();
    return 0;
}

static int test_too_many_engines(void) {
    rocket_init();
    
    TEST_ASSERT_EQ_INT(-2, rocket_configure_vehicle(1, "Test", 99, false),
                       "Should reject >9 engines");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Safety System Tests                                                        */
/*===========================================================================*/

static int test_safety_arm(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    
    TEST_ASSERT_EQ_INT(0, rocket_safety_arm(), "Safety arm OK");
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.armed, "Safety should be armed");
    TEST_ASSERT(!status.abort_triggered, "No abort yet");
    
    rocket_shutdown();
    return 0;
}

static int test_safety_checks_run(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Process multiple times */
    for (int i = 0; i < 100; i++) {
        rocket_process(10);
    }
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.safety_checks_per_sec > 0, "Safety checks ran");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Abort Trigger Tests                                                        */
/*===========================================================================*/

static int test_low_thrust_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Inject low thrust (80% when min is 85%) */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 80.0f, 3000.0f, 5.0f);
    rocket_process(10);
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.abort_triggered, "Abort should trigger");
    TEST_ASSERT_EQ_INT(ABORT_THRUST_LOW, status.abort_reason, "Abort reason");
    TEST_ASSERT_EQ_INT(PHASE_ABORT, rocket_get_phase(), "Phase should be ABORT");
    
    rocket_shutdown();
    return 0;
}

static int test_high_thrust_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Inject high thrust (110% when max is 105%) */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 110.0f, 3000.0f, 5.0f);
    rocket_process(10);
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.abort_triggered, "Abort should trigger");
    TEST_ASSERT_EQ_INT(ABORT_THRUST_HIGH, status.abort_reason, "Abort reason");
    
    rocket_shutdown();
    return 0;
}

static int test_overtemperature_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Inject overtemperature (3700°C when abort is 3600°C) */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 100.0f, 3700.0f, 5.0f);
    rocket_process(10);
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.abort_triggered, "Abort should trigger");
    TEST_ASSERT_EQ_INT(ABORT_CHAMBER_OVERTEMP, status.abort_reason, "Abort reason");
    
    rocket_shutdown();
    return 0;
}

static int test_vibration_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Inject excessive vibration repeatedly to fill history */
    for (int i = 0; i < 15; i++) {
        rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 100.0f, 3000.0f, 25.0f);
        rocket_process(10);
    }
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.abort_triggered, "Abort should trigger");
    TEST_ASSERT_EQ_INT(ABORT_VIBRATION_EXCESSIVE, status.abort_reason, "Abort reason");
    
    rocket_shutdown();
    return 0;
}

static int test_manual_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    
    TEST_ASSERT_EQ_INT(0, rocket_abort(ABORT_MANUAL), "Manual abort OK");
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.abort_triggered, "Abort should trigger");
    TEST_ASSERT_EQ_INT(ABORT_MANUAL, status.abort_reason, "Abort reason");
    
    rocket_shutdown();
    return 0;
}

static int test_thrust_asymmetry_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 3, false);  /* 3 engines */
    rocket_configure_engine(0, "E0", 1000.0f, 1100.0f);
    rocket_configure_engine(1, "E1", 1000.0f, 1100.0f);
    rocket_configure_engine(2, "E2", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Engine 0 at 100%, Engine 1 at 100%, Engine 2 at 75% (>10% deviation) */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 100.0f, 3000.0f, 5.0f);
    rocket_inject_engine_state(1, ENGINE_STATE_MAINSTAGE, 100.0f, 3000.0f, 5.0f);
    rocket_inject_engine_state(2, ENGINE_STATE_MAINSTAGE, 75.0f, 3000.0f, 5.0f);
    rocket_process(10);
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(status.abort_triggered, "Abort should trigger");
    TEST_ASSERT_EQ_INT(ABORT_THRUST_ASYMMETRY, status.abort_reason, "Abort reason");
    
    rocket_shutdown();
    return 0;
}

static int test_nominal_no_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Inject nominal values - should not abort */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 100.0f, 3000.0f, 8.0f);
    rocket_process(10);
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(!status.abort_triggered, "Should not abort on nominal");
    
    rocket_shutdown();
    return 0;
}

static int test_abort_response_time(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Trigger abort */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 80.0f, 3000.0f, 5.0f);
    rocket_process(10);
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    
    /* Abort latency should be recorded and <10ms */
    TEST_ASSERT(status.abort_latency_us < 10000, "Abort latency under 10ms");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Engine Status Tests                                                        */
/*===========================================================================*/

static int test_engine_status_query(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    
    engine_status_t status;
    TEST_ASSERT_EQ_INT(0, rocket_get_engine_status(0, &status), "Status query OK");
    TEST_ASSERT_EQ_INT(ENGINE_STATE_OFF, status.state, "State OFF initial");
    TEST_ASSERT_EQ_INT(0, status.engine_id, "Engine ID");
    
    rocket_shutdown();
    return 0;
}

static int test_invalid_engine_id(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    
    engine_status_t status;
    TEST_ASSERT_EQ_INT(-2, rocket_get_engine_status(5, &status), 
                       "Invalid engine should fail");
    
    rocket_shutdown();
    return 0;
}

static int test_engine_state_after_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 2, false);
    rocket_configure_engine(0, "E0", 1000.0f, 1100.0f);
    rocket_configure_engine(1, "E1", 1000.0f, 1100.0f);
    rocket_safety_arm();
    
    rocket_abort(ABORT_MANUAL);
    
    engine_status_t status0, status1;
    rocket_get_engine_status(0, &status0);
    rocket_get_engine_status(1, &status1);
    
    TEST_ASSERT_EQ_INT(ENGINE_STATE_ABORT, status0.state, "Engine 0 in ABORT");
    TEST_ASSERT_EQ_INT(ENGINE_STATE_ABORT, status1.state, "Engine 1 in ABORT");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* TMR Sensor Voting Tests                                                    */
/*===========================================================================*/

static int test_tmr_three_channel_voting(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    
    /* Inject 3 channels for thrust with slight variation */
    rocket_inject_sensor(0, SENSOR_THRUST_MAIN, 0, 1000.0f);
    rocket_inject_sensor(0, SENSOR_THRUST_MAIN, 1, 1005.0f);
    rocket_inject_sensor(0, SENSOR_THRUST_MAIN, 2, 1003.0f);
    
    rocket_process(10);
    
    engine_status_t status;
    rocket_get_engine_status(0, &status);
    
    /* Voted value should be median: 1003 */
    TEST_ASSERT_FLOAT_NEAR(1003.0f, status.thrust_kn, 1.0f, "TMR voting median");
    
    rocket_shutdown();
    return 0;
}

static int test_tmr_outlier_rejection(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    
    /* Inject 3 channels with one outlier */
    rocket_inject_sensor(0, SENSOR_THRUST_MAIN, 0, 1000.0f);
    rocket_inject_sensor(0, SENSOR_THRUST_MAIN, 1, 1002.0f);
    rocket_inject_sensor(0, SENSOR_THRUST_MAIN, 2, 500.0f);  /* Faulty sensor */
    
    rocket_process(10);
    
    engine_status_t status;
    rocket_get_engine_status(0, &status);
    
    /* Voted value should still be reasonable (median or mean of agreeing) */
    TEST_ASSERT(status.thrust_kn > 800.0f, "Outlier filtered");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Telemetry Tests                                                            */
/*===========================================================================*/

static int test_telemetry_generation(void) {
    rocket_init();
    rocket_configure_vehicle(1001, "Test", 2, false);
    rocket_configure_engine(0, "E0", 1000.0f, 1100.0f);
    rocket_configure_engine(1, "E1", 1000.0f, 1100.0f);
    
    uint8_t buffer[2048];
    size_t frame_len;
    
    TEST_ASSERT_EQ_INT(0, rocket_generate_telemetry(buffer, sizeof(buffer), &frame_len),
                       "Telemetry generation OK");
    TEST_ASSERT(frame_len > 0, "Frame has content");
    TEST_ASSERT(frame_len <= 1024, "Frame size reasonable");
    
    rocket_shutdown();
    return 0;
}

static int test_telemetry_buffer_too_small(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    
    uint8_t buffer[32];  /* Too small */
    size_t frame_len;
    
    TEST_ASSERT_EQ_INT(-2, rocket_generate_telemetry(buffer, sizeof(buffer), &frame_len),
                       "Should fail with small buffer");
    
    rocket_shutdown();
    return 0;
}

static int test_telemetry_stats(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    
    uint8_t buffer[2048];
    size_t frame_len;
    
    /* Generate multiple frames */
    for (int i = 0; i < 5; i++) {
        rocket_generate_telemetry(buffer, sizeof(buffer), &frame_len);
    }
    
    tlm_stats_t stats;
    TEST_ASSERT_EQ_INT(0, rocket_get_tlm_stats(&stats), "Stats query OK");
    TEST_ASSERT_EQ_INT(5, stats.frames_generated, "Frame count");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Alarm Tests                                                                */
/*===========================================================================*/

static int test_alarm_generated_on_fault(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Trigger low thrust */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 80.0f, 3000.0f, 5.0f);
    rocket_process(10);
    
    alarm_t alarms[10];
    uint8_t count;
    TEST_ASSERT_EQ_INT(0, rocket_get_alarms(alarms, 10, &count), "Alarm query OK");
    TEST_ASSERT(count > 0, "Alarm generated");
    TEST_ASSERT_EQ_INT(ABORT_THRUST_LOW, alarms[0].type, "Alarm type");
    TEST_ASSERT_EQ_INT(SEVERITY_ABORT, alarms[0].severity, "Alarm severity");
    
    rocket_shutdown();
    return 0;
}

static int test_warning_alarm_no_abort(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Inject temperature just above warning but below abort */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 100.0f, 3550.0f, 5.0f);
    rocket_process(10);
    
    safety_status_t status;
    rocket_get_safety_status(&status);
    TEST_ASSERT(!status.abort_triggered, "Warning should not abort");
    
    alarm_t alarms[10];
    uint8_t count;
    rocket_get_alarms(alarms, 10, &count);
    
    /* Should have warning alarm */
    bool found_warning = false;
    for (uint8_t i = 0; i < count; i++) {
        if (alarms[i].type == ABORT_CHAMBER_OVERTEMP && 
            alarms[i].severity == SEVERITY_WARNING) {
            found_warning = true;
        }
    }
    TEST_ASSERT(found_warning, "Warning alarm generated");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Launch Readiness Tests                                                     */
/*===========================================================================*/

static int test_readiness_safety_not_armed(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    
    bool ready;
    char reason[128];
    TEST_ASSERT_EQ_INT(0, rocket_check_readiness(&ready, reason, sizeof(reason)),
                       "Readiness check OK");
    TEST_ASSERT(!ready, "Should not be ready");
    
    rocket_shutdown();
    return 0;
}

static int test_readiness_go_for_launch(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    
    /* Inject valid sensor data on all channels */
    for (int ch = 0; ch < 3; ch++) {
        rocket_inject_sensor(0, SENSOR_THRUST_MAIN, ch, 1000.0f);
        rocket_inject_sensor(0, SENSOR_CHAMBER_PRESSURE, ch, 3000.0f);
        rocket_inject_sensor(0, SENSOR_CHAMBER_TEMP, ch, 3000.0f);
    }
    rocket_process(10);
    
    bool ready;
    char reason[128];
    rocket_check_readiness(&ready, reason, sizeof(reason));
    TEST_ASSERT(ready, "Should be ready");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Event Logging Tests                                                        */
/*===========================================================================*/

static int test_events_logged(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 2, false);
    rocket_configure_engine(0, "E0", 1000.0f, 1100.0f);
    rocket_configure_engine(1, "E1", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_abort(ABORT_MANUAL);
    
    event_t events[32];
    uint16_t count;
    TEST_ASSERT_EQ_INT(0, rocket_get_events(events, 32, &count), "Event query OK");
    TEST_ASSERT(count > 0, "Events logged");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Phase Transition Tests                                                     */
/*===========================================================================*/

static int test_phase_transitions(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    
    TEST_ASSERT_EQ_INT(PHASE_PRELAUNCH, rocket_get_phase(), "Initial phase");
    
    rocket_safety_arm();
    TEST_ASSERT_EQ_INT(0, rocket_set_phase(PHASE_TERMINAL_COUNT), "Set terminal count");
    TEST_ASSERT_EQ_INT(PHASE_TERMINAL_COUNT, rocket_get_phase(), "Terminal count");
    
    TEST_ASSERT_EQ_INT(0, rocket_set_phase(PHASE_LIFTOFF), "Set liftoff");
    TEST_ASSERT_EQ_INT(PHASE_LIFTOFF, rocket_get_phase(), "Liftoff");
    
    rocket_shutdown();
    return 0;
}

static int test_ignition_requires_terminal_count(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    
    /* Should fail if not in terminal count */
    TEST_ASSERT_EQ_INT(-3, rocket_start_ignition(), "Ignition requires T/C");
    
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    TEST_ASSERT_EQ_INT(0, rocket_start_ignition(), "Ignition from T/C OK");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Edge Cases and Error Handling                                              */
/*===========================================================================*/

static int test_null_pointer_handling(void) {
    rocket_init();
    
    TEST_ASSERT_EQ_INT(-1, rocket_get_engine_status(0, NULL), "Null engine status");
    TEST_ASSERT_EQ_INT(-1, rocket_get_safety_status(NULL), "Null safety status");
    TEST_ASSERT_EQ_INT(-1, rocket_get_tlm_stats(NULL), "Null telemetry stats");
    TEST_ASSERT_EQ_INT(-1, rocket_check_readiness(NULL, NULL, 0), "Null readiness");
    
    rocket_shutdown();
    return 0;
}

static int test_process_not_initialized(void) {
    TEST_ASSERT_EQ_INT(-1, rocket_process(10), "Process before init fails");
    return 0;
}

static int test_run_time_accumulation(void) {
    rocket_init();
    rocket_configure_vehicle(1, "Test", 1, false);
    rocket_configure_engine(0, "Engine", 1000.0f, 1100.0f);
    rocket_safety_arm();
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    /* Set engine to mainstage */
    rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 100.0f, 3000.0f, 5.0f);
    
    /* Process for 1 second */
    for (int i = 0; i < 100; i++) {
        rocket_inject_engine_state(0, ENGINE_STATE_MAINSTAGE, 100.0f, 3000.0f, 5.0f);
        rocket_process(10);
    }
    
    engine_status_t status;
    rocket_get_engine_status(0, &status);
    TEST_ASSERT(status.run_time_ms >= 1000, "Run time accumulated");
    
    rocket_shutdown();
    return 0;
}

/*===========================================================================*/
/* Main Test Runner                                                           */
/*===========================================================================*/

int main(void) {
    printf("\n");
    printf("================================================\n");
    printf("=== Rocket Engine Telemetry & Safety Tests ===\n");
    printf("================================================\n\n");
    
    printf("--- Initialization Tests ---\n");
    RUN_TEST(test_init_success);
    RUN_TEST(test_double_init_fails);
    RUN_TEST(test_shutdown_before_init_fails);
    RUN_TEST(test_vehicle_configuration);
    RUN_TEST(test_too_many_engines);
    
    printf("\n--- Safety System Tests ---\n");
    RUN_TEST(test_safety_arm);
    RUN_TEST(test_safety_checks_run);
    
    printf("\n--- Abort Trigger Tests ---\n");
    RUN_TEST(test_low_thrust_abort);
    RUN_TEST(test_high_thrust_abort);
    RUN_TEST(test_overtemperature_abort);
    RUN_TEST(test_vibration_abort);
    RUN_TEST(test_manual_abort);
    RUN_TEST(test_thrust_asymmetry_abort);
    RUN_TEST(test_nominal_no_abort);
    RUN_TEST(test_abort_response_time);
    
    printf("\n--- Engine Status Tests ---\n");
    RUN_TEST(test_engine_status_query);
    RUN_TEST(test_invalid_engine_id);
    RUN_TEST(test_engine_state_after_abort);
    
    printf("\n--- TMR Sensor Voting Tests ---\n");
    RUN_TEST(test_tmr_three_channel_voting);
    RUN_TEST(test_tmr_outlier_rejection);
    
    printf("\n--- Telemetry Tests ---\n");
    RUN_TEST(test_telemetry_generation);
    RUN_TEST(test_telemetry_buffer_too_small);
    RUN_TEST(test_telemetry_stats);
    
    printf("\n--- Alarm Tests ---\n");
    RUN_TEST(test_alarm_generated_on_fault);
    RUN_TEST(test_warning_alarm_no_abort);
    
    printf("\n--- Launch Readiness Tests ---\n");
    RUN_TEST(test_readiness_safety_not_armed);
    RUN_TEST(test_readiness_go_for_launch);
    
    printf("\n--- Event Logging Tests ---\n");
    RUN_TEST(test_events_logged);
    
    printf("\n--- Phase Transition Tests ---\n");
    RUN_TEST(test_phase_transitions);
    RUN_TEST(test_ignition_requires_terminal_count);
    
    printf("\n--- Edge Case Tests ---\n");
    RUN_TEST(test_null_pointer_handling);
    RUN_TEST(test_process_not_initialized);
    RUN_TEST(test_run_time_accumulation);
    
    printf("\n================================================\n");
    printf("Test Results: %d/%d passed, %d failed\n",
           g_tests_passed, g_tests_run, g_tests_run - g_tests_passed);
    printf("================================================\n");
    
    return (g_tests_passed == g_tests_run) ? 0 : 1;
}
