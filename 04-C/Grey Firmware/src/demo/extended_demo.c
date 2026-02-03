/**
 * @file extended_demo.c
 * @brief Extended Integration Pipeline Demo
 * 
 * DEMONSTRATION: End-to-End IoT Data Pipeline
 * =============================================
 * 
 * This demo showcases the complete integration of all Grey Firmware modules:
 * 
 *   Sensor → Power Manager → CAN Bus → Telemetry → MQTT → Cloud
 *   
 * Key integration points demonstrated:
 *   1. Sensor data acquisition with power-aware sampling
 *   2. Power state machine managing sampling rate and radio duty cycle
 *   3. CAN bus for local vehicle/industrial network communication
 *   4. Telemetry aggregation and batching
 *   5. MQTT publish to cloud backend
 *   6. Diagnostics and health monitoring throughout
 * 
 * This represents the cohesion goal: showing how all modules work together
 * in a realistic IoT/automotive/industrial application.
 */

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

/* Core Framework */
#include "core/scheduler.h"
#include "core/message_bus.h"
#include "core/error_handler.h"
/* Note: logging.h would be included in production builds */

/* Automotive Domain */
#include "automotive/can_bus.h"

/* Power Management */
#include "power/power_state.h"

/* Note: These headers are stubs defined in Phase 3 */
/* #include "diagnostics/metrics.h" */
/* #include "diagnostics/health.h" */

/*===========================================================================*/
/* Stub Types for Demo (normally from diagnostics headers)                    */
/*===========================================================================*/

typedef uint32_t gf_metric_handle_t;
typedef uint32_t gf_health_handle_t;

typedef enum {
    GF_METRIC_COUNTER = 0,
    GF_METRIC_GAUGE,
    GF_METRIC_HISTOGRAM
} gf_metric_type_t;

typedef struct {
    char                name[32];
    gf_metric_type_t    type;
    const char         *description;
} gf_metric_def_t;

typedef enum {
    GF_HEALTH_OK = 0,
    GF_HEALTH_DEGRADED,
    GF_HEALTH_UNHEALTHY,
    GF_HEALTH_CRITICAL
} gf_health_status_t;

typedef enum {
    GF_CHECK_HEARTBEAT = 0,
    GF_CHECK_SELFTEST,
    GF_CHECK_THRESHOLD,
    GF_CHECK_WATCHDOG
} gf_check_type_t;

typedef gf_health_status_t (*gf_health_check_fn)(void *ctx);

typedef struct {
    char                name[24];
    gf_check_type_t     type;
    gf_health_check_fn  check_fn;
    void               *ctx;
    uint32_t            interval_ms;
    uint32_t            timeout_ms;
} gf_health_check_t;

typedef struct {
    gf_health_status_t  overall;
    uint8_t             ok_count;
    uint8_t             degraded_count;
    uint8_t             unhealthy_count;
    uint8_t             critical_count;
    uint32_t            total_checks;
    uint32_t            last_check_time;
    uint32_t            uptime_sec;
} gf_health_summary_t;

/* Stub function declarations */
static int gf_metrics_init(void) { return 0; }
static gf_metric_handle_t gf_metric_register(const gf_metric_def_t *def) {
    static gf_metric_handle_t next_handle = 1;
    (void)def;
    return next_handle++;
}
static void gf_metric_counter_inc(gf_metric_handle_t handle) { (void)handle; }
static int gf_health_init(void) { return 0; }
static gf_health_handle_t gf_health_register(const gf_health_check_t *check) {
    static gf_health_handle_t next_handle = 1;
    (void)check;
    return next_handle++;
}
static void gf_health_get_summary(gf_health_summary_t *summary) {
    if (summary) {
        summary->overall = GF_HEALTH_OK;
        summary->ok_count = 3;
        summary->degraded_count = 0;
        summary->unhealthy_count = 0;
        summary->critical_count = 0;
        summary->total_checks = 3;
    }
}

/*===========================================================================*/
/* Demo Configuration                                                         */
/*===========================================================================*/

#define DEMO_SAMPLE_INTERVAL_ACTIVE_MS      1000    /* 1 Hz when active */
#define DEMO_SAMPLE_INTERVAL_SLEEP_MS       60000   /* 1/min in low power */
#define DEMO_MQTT_BATCH_SIZE                10      /* Samples per publish */
#define DEMO_CAN_BROADCAST_ID               0x200   /* CAN message ID */

/*===========================================================================*/
/* Demo State                                                                 */
/*===========================================================================*/

typedef struct {
    /* Power management */
    gf_power_state_t    current_power_mode;
    uint32_t            power_lock_handle;
    uint32_t            last_activity_time;
    
    /* Sensor data */
    int32_t             temperature;        /* °C * 100 */
    uint32_t            pressure;           /* Pa */
    uint16_t            humidity;           /* % * 100 */
    
    /* Telemetry batch */
    uint8_t             batch_count;
    struct {
        uint32_t        timestamp;
        int32_t         temperature;
        uint32_t        pressure;
    } batch[DEMO_MQTT_BATCH_SIZE];
    
    /* Metrics handles */
    gf_metric_handle_t  metric_samples;
    gf_metric_handle_t  metric_can_tx;
    gf_metric_handle_t  metric_mqtt_pub;
    gf_metric_handle_t  metric_power_transitions;
    
    /* Health handles */
    gf_health_handle_t  health_sensor;
    gf_health_handle_t  health_can;
    gf_health_handle_t  health_mqtt;
    
    /* Statistics */
    uint32_t            total_samples;
    uint32_t            can_messages_sent;
    uint32_t            mqtt_publishes;
    uint32_t            errors;
    
} demo_state_t;

static demo_state_t g_demo = {0};

/*===========================================================================*/
/* Simulated Tick                                                             */
/*===========================================================================*/

static uint32_t demo_get_tick_ms(void) {
    static uint32_t tick = 0;
    return tick += 10;  /* Simulate time passing */
}

/*===========================================================================*/
/* Power State Callbacks                                                      */
/*===========================================================================*/

static void on_power_transition(gf_power_state_t from, gf_power_state_t to, 
                                 void *ctx) {
    (void)ctx;
    
    g_demo.current_power_mode = to;
    
    const char *state_names[] = {"RUN", "IDLE", "SLEEP", "DEEP_SLEEP"};
    printf("[POWER] Transition: %s -> %s\n", state_names[from], state_names[to]);
    
    gf_metric_counter_inc(g_demo.metric_power_transitions);
}

static void on_wake(gf_wake_source_t source, void *ctx) {
    (void)ctx;
    
    printf("[POWER] Wake from ");
    if (source & GF_WAKE_GPIO) printf("GPIO ");
    if (source & GF_WAKE_TIMER) printf("TIMER ");
    if (source & GF_WAKE_RTC) printf("RTC ");
    printf("\n");
    
    g_demo.last_activity_time = demo_get_tick_ms();
}

/*===========================================================================*/
/* Sensor Pipeline Stage                                                      */
/*===========================================================================*/

static void simulate_sensor_read(void) {
    /* Simulate sensor data with some variation */
    static int32_t base_temp = 2500;     /* 25.00 °C */
    static uint32_t base_pressure = 101325;  /* 1 atm */
    
    base_temp += (demo_get_tick_ms() % 10) - 5;  /* ±0.05°C drift */
    base_pressure += (demo_get_tick_ms() % 100) - 50;  /* ±50 Pa drift */
    
    g_demo.temperature = base_temp;
    g_demo.pressure = base_pressure;
    g_demo.humidity = 5000;  /* 50.00% */
    
    g_demo.total_samples++;
    gf_metric_counter_inc(g_demo.metric_samples);
    
    printf("[SENSOR] T=%d.%02d°C, P=%u Pa, H=%u.%02u%%\n",
           g_demo.temperature / 100, g_demo.temperature % 100,
           g_demo.pressure,
           g_demo.humidity / 100, g_demo.humidity % 100);
}

/*===========================================================================*/
/* CAN Bus Pipeline Stage                                                     */
/*===========================================================================*/

static void broadcast_to_can(void) {
    gf_can_frame_t frame = {
        .id = DEMO_CAN_BROADCAST_ID,
        .extended = false,
        .rtr = false,
        .dlc = 8,
        .data = {0}
    };
    
    /* Pack sensor data into CAN frame */
    frame.data[0] = (g_demo.temperature >> 8) & 0xFF;
    frame.data[1] = g_demo.temperature & 0xFF;
    frame.data[2] = (g_demo.pressure >> 24) & 0xFF;
    frame.data[3] = (g_demo.pressure >> 16) & 0xFF;
    frame.data[4] = (g_demo.pressure >> 8) & 0xFF;
    frame.data[5] = g_demo.pressure & 0xFF;
    frame.data[6] = (g_demo.humidity >> 8) & 0xFF;
    frame.data[7] = g_demo.humidity & 0xFF;
    
    int result = gf_can_transmit(&frame, 100);
    if (result == 0) {
        g_demo.can_messages_sent++;
        gf_metric_counter_inc(g_demo.metric_can_tx);
        printf("[CAN] Sent frame ID=0x%03X\n", frame.id);
    } else {
        g_demo.errors++;
        printf("[CAN] TX failed: %d\n", result);
    }
}

/*===========================================================================*/
/* Telemetry Batching Stage                                                   */
/*===========================================================================*/

static void add_to_batch(void) {
    if (g_demo.batch_count >= DEMO_MQTT_BATCH_SIZE) {
        return;  /* Batch full, will be flushed separately */
    }
    
    g_demo.batch[g_demo.batch_count].timestamp = demo_get_tick_ms();
    g_demo.batch[g_demo.batch_count].temperature = g_demo.temperature;
    g_demo.batch[g_demo.batch_count].pressure = g_demo.pressure;
    g_demo.batch_count++;
    
    printf("[TELEMETRY] Batch: %d/%d samples\n", 
           g_demo.batch_count, DEMO_MQTT_BATCH_SIZE);
}

/*===========================================================================*/
/* MQTT Cloud Stage                                                           */
/*===========================================================================*/

static void publish_to_cloud(void) {
    if (g_demo.batch_count == 0) {
        return;
    }
    
    /* Lock power to RUN during radio operation */
    g_demo.power_lock_handle = gf_power_lock(GF_POWER_STATE_RUN);
    
    /* Simulate MQTT publish with JSON payload */
    printf("[MQTT] Publishing batch of %d samples...\n", g_demo.batch_count);
    
    /* Build JSON payload (simplified) */
    char payload[512];
    int offset = snprintf(payload, sizeof(payload), 
                          "{\"device\":\"grey_demo\",\"samples\":[");
    
    for (int i = 0; i < g_demo.batch_count && offset < (int)sizeof(payload) - 50; i++) {
        offset += snprintf(payload + offset, sizeof(payload) - offset,
                          "%s{\"t\":%u,\"temp\":%d,\"pres\":%u}",
                          i > 0 ? "," : "",
                          g_demo.batch[i].timestamp,
                          g_demo.batch[i].temperature,
                          g_demo.batch[i].pressure);
    }
    snprintf(payload + offset, sizeof(payload) - offset, "]}");
    
    /* Simulate MQTT publish */
    printf("[MQTT] Payload: %s\n", payload);
    printf("[MQTT] Published to topic 'sensors/grey_demo/batch'\n");
    
    g_demo.mqtt_publishes++;
    gf_metric_counter_inc(g_demo.metric_mqtt_pub);
    g_demo.batch_count = 0;
    
    /* Release power lock */
    if (g_demo.power_lock_handle) {
        gf_power_unlock(g_demo.power_lock_handle);
        g_demo.power_lock_handle = 0;
    }
}

/*===========================================================================*/
/* Health Checks                                                              */
/*===========================================================================*/

static gf_health_status_t check_sensor_health(void *ctx) {
    (void)ctx;
    
    /* Check if sensor readings are within valid range */
    if (g_demo.temperature < -4000 || g_demo.temperature > 8500) {
        return GF_HEALTH_UNHEALTHY;
    }
    if (g_demo.pressure < 80000 || g_demo.pressure > 120000) {
        return GF_HEALTH_DEGRADED;
    }
    return GF_HEALTH_OK;
}

static gf_health_status_t check_can_health(void *ctx) {
    (void)ctx;
    
    /* Check CAN error rate */
    if (g_demo.can_messages_sent == 0) {
        return GF_HEALTH_DEGRADED;
    }
    return GF_HEALTH_OK;
}

static gf_health_status_t check_mqtt_health(void *ctx) {
    (void)ctx;
    
    /* Check MQTT publish success */
    if (g_demo.errors > g_demo.mqtt_publishes / 10) {
        return GF_HEALTH_UNHEALTHY;
    }
    return GF_HEALTH_OK;
}

/*===========================================================================*/
/* Demo Initialization                                                        */
/*===========================================================================*/

static void demo_init_power(void) {
    printf("\n=== Initializing Power Management ===\n");
    
    gf_power_config_t power_config = {
        .min_state = GF_POWER_STATE_DEEP_SLEEP,
        .idle_timeout_ms = 100,
        .sleep_timeout_ms = 5000,
        .deep_sleep_timeout_ms = 30000,
        .ram_retention = true,
        .enable_profiling = true
    };
    
    gf_power_init(&power_config);
    gf_power_set_transition_callback(on_power_transition, NULL);
    gf_power_set_wake_callback(on_wake, NULL);
    
    /* Configure timer wake for periodic sampling */
    gf_wake_timer_config_t timer_cfg = {
        .timeout_ms = DEMO_SAMPLE_INTERVAL_ACTIVE_MS,
        .periodic = true
    };
    gf_power_configure_wake_timer(&timer_cfg);
    gf_power_enable_wake_source(GF_WAKE_TIMER, true);
    
    printf("Power manager initialized\n");
}

static void demo_init_metrics(void) {
    printf("\n=== Initializing Metrics ===\n");
    
    gf_metrics_init();
    
    gf_metric_def_t def_samples = {
        .name = "sensor_samples",
        .type = GF_METRIC_COUNTER,
        .description = "Total sensor readings"
    };
    g_demo.metric_samples = gf_metric_register(&def_samples);
    
    gf_metric_def_t def_can = {
        .name = "can_messages_tx",
        .type = GF_METRIC_COUNTER,
        .description = "CAN messages transmitted"
    };
    g_demo.metric_can_tx = gf_metric_register(&def_can);
    
    gf_metric_def_t def_mqtt = {
        .name = "mqtt_publishes",
        .type = GF_METRIC_COUNTER,
        .description = "MQTT publishes"
    };
    g_demo.metric_mqtt_pub = gf_metric_register(&def_mqtt);
    
    gf_metric_def_t def_power = {
        .name = "power_transitions",
        .type = GF_METRIC_COUNTER,
        .description = "Power state transitions"
    };
    g_demo.metric_power_transitions = gf_metric_register(&def_power);
    
    printf("Metrics initialized: 4 counters registered\n");
}

static void demo_init_health(void) {
    printf("\n=== Initializing Health Monitor ===\n");
    
    gf_health_init();
    
    gf_health_check_t check_sensor = {
        .name = "sensor",
        .type = GF_CHECK_HEARTBEAT,
        .check_fn = check_sensor_health,
        .interval_ms = 5000
    };
    g_demo.health_sensor = gf_health_register(&check_sensor);
    
    gf_health_check_t check_can = {
        .name = "can_bus",
        .type = GF_CHECK_HEARTBEAT,
        .check_fn = check_can_health,
        .interval_ms = 5000
    };
    g_demo.health_can = gf_health_register(&check_can);
    
    gf_health_check_t check_mqtt = {
        .name = "mqtt",
        .type = GF_CHECK_HEARTBEAT,
        .check_fn = check_mqtt_health,
        .interval_ms = 10000
    };
    g_demo.health_mqtt = gf_health_register(&check_mqtt);
    
    printf("Health monitor initialized: 3 checks registered\n");
}

/*===========================================================================*/
/* Main Pipeline Loop                                                         */
/*===========================================================================*/

static void run_pipeline_cycle(void) {
    uint32_t now = demo_get_tick_ms();
    
    printf("\n--- Pipeline Cycle @ %u ms ---\n", now);
    
    /* Stage 1: Sensor acquisition */
    simulate_sensor_read();
    
    /* Stage 2: CAN broadcast (for local network) */
    broadcast_to_can();
    
    /* Stage 3: Add to telemetry batch */
    add_to_batch();
    
    /* Stage 4: Publish batch if full */
    if (g_demo.batch_count >= DEMO_MQTT_BATCH_SIZE) {
        publish_to_cloud();
    }
    
    /* Update last activity for power management */
    g_demo.last_activity_time = now;
}

static void check_power_state(void) {
    uint32_t now = demo_get_tick_ms();
    uint32_t idle_time = now - g_demo.last_activity_time;
    
    /* Transition to lower power states based on idle time */
    gf_power_state_t current = gf_power_get_state();
    
    if (idle_time > 30000 && current < GF_POWER_STATE_DEEP_SLEEP) {
        printf("[POWER] Long idle detected, requesting DEEP_SLEEP\n");
        gf_power_request_state(GF_POWER_STATE_DEEP_SLEEP);
    } else if (idle_time > 5000 && current < GF_POWER_STATE_SLEEP) {
        printf("[POWER] Short idle detected, requesting SLEEP\n");
        gf_power_request_state(GF_POWER_STATE_SLEEP);
    } else if (idle_time > 100 && current < GF_POWER_STATE_IDLE) {
        gf_power_request_state(GF_POWER_STATE_IDLE);
    }
}

/*===========================================================================*/
/* Demo Entry Point                                                           */
/*===========================================================================*/

void extended_demo_run(void) {
    printf("\n");
    printf("╔══════════════════════════════════════════════════════════════╗\n");
    printf("║        GREY FIRMWARE - Extended Integration Demo             ║\n");
    printf("║                                                              ║\n");
    printf("║  Pipeline: Sensor → Power → CAN → Telemetry → MQTT → Cloud  ║\n");
    printf("╚══════════════════════════════════════════════════════════════╝\n");
    
    /* Initialize all subsystems */
    demo_init_power();
    demo_init_metrics();
    demo_init_health();
    
    /* Initialize CAN bus (from existing implementation) */
    printf("\n=== Initializing CAN Bus ===\n");
    gf_can_config_t can_cfg = {
        .baud_rate = 500000,        /* 500 kbps */
        .timing = {0},              /* Auto-calculate */
        .loopback = false,
        .silent = false,
        .auto_retransmit = true,
        .bus_off_recovery_ms = 2000
    };
    gf_can_init(&can_cfg);
    printf("CAN bus initialized at 500 kbps\n");
    
    printf("\n=== Starting Pipeline ===\n");
    g_demo.last_activity_time = demo_get_tick_ms();
    
    /* Run demo cycles */
    for (int cycle = 0; cycle < 15; cycle++) {
        run_pipeline_cycle();
        check_power_state();
        
        /* Simulate time passing */
        for (int i = 0; i < 100; i++) {
            demo_get_tick_ms();
        }
    }
    
    /* Print final statistics */
    printf("\n");
    printf("╔══════════════════════════════════════════════════════════════╗\n");
    printf("║                    Demo Statistics                           ║\n");
    printf("╠══════════════════════════════════════════════════════════════╣\n");
    printf("║  Total samples:        %-5u                                 ║\n", g_demo.total_samples);
    printf("║  CAN messages sent:    %-5u                                 ║\n", g_demo.can_messages_sent);
    printf("║  MQTT publishes:       %-5u                                 ║\n", g_demo.mqtt_publishes);
    printf("║  Errors:               %-5u                                 ║\n", g_demo.errors);
    printf("╚══════════════════════════════════════════════════════════════╝\n");
    
    /* Print power statistics */
    gf_power_status_t power_status;
    gf_power_get_status(&power_status);
    printf("\n=== Power Statistics ===\n");
    printf("Time in RUN:        %u ms\n", power_status.time_in_run_ms);
    printf("Time in IDLE:       %u ms\n", power_status.time_in_idle_ms);
    printf("Time in SLEEP:      %u ms\n", power_status.time_in_sleep_ms);
    printf("Time in DEEP_SLEEP: %u ms\n", power_status.time_in_deep_sleep_ms);
    printf("Wake count:         %u\n", power_status.wake_count);
    printf("Transitions:        %u\n", power_status.transition_count);
    
    /* Print health summary */
    printf("\n=== Health Summary ===\n");
    gf_health_summary_t health;
    gf_health_get_summary(&health);
    printf("Overall: %s\n", 
           health.overall == GF_HEALTH_OK ? "OK" :
           health.overall == GF_HEALTH_DEGRADED ? "DEGRADED" : "UNHEALTHY");
    printf("OK: %d, Degraded: %d, Unhealthy: %d, Critical: %d\n",
           health.ok_count, health.degraded_count, 
           health.unhealthy_count, health.critical_count);
    
    printf("\n=== Demo Complete ===\n");
}

/*===========================================================================*/
/* Main Entry (if built standalone)                                           */
/*===========================================================================*/

#ifdef EXTENDED_DEMO_STANDALONE
int main(void) {
    extended_demo_run();
    return 0;
}
#endif
