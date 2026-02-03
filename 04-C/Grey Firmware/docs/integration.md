# Integration Guide

This guide explains how to integrate Grey Firmware components into your embedded projects.

## Quick Start

### 1. Include the Master Header

```c
#include "grey_firmware.h"
```

This provides access to all modules and type definitions.

### 2. Initialize Core Services

```c
int main(void)
{
    /* Initialize error handling first */
    gf_error_init(NULL);
    
    /* Initialize message bus */
    gf_msg_init();
    
    /* Initialize scheduler */
    gf_sched_init(deadline_callback);
    
    /* Your initialization code... */
    
    /* Main loop */
    while (1) {
        /* Process scheduled tasks */
        gf_sched_run();
        
        /* Dispatch queued messages */
        gf_msg_process(0);
    }
}
```

## Using the Message Bus

### Publishing Messages

```c
/* Simple publish */
float temperature = 25.5f;
gf_msg_publish("sensor/temperature", &temperature, sizeof(temperature),
               GF_MSG_QOS_FIRE_FORGET, GF_MSG_PRIO_NORMAL);

/* With typed payload */
gf_msg_publish_sensor("sensor/temp", 0, raw_adc, scaled_value, 0);
```

### Subscribing to Messages

```c
/* Callback function */
void on_sensor_data(const gf_message_t *msg, void *ctx)
{
    if (msg->payload_len >= sizeof(float)) {
        float value = *(float *)msg->payload;
        printf("Temperature: %.1f\n", value);
    }
}

/* Subscribe */
int handle = gf_msg_subscribe("sensor/*", on_sensor_data, NULL);

/* Later: unsubscribe if needed */
gf_msg_unsubscribe(handle);
```

## Using the Scheduler

### Creating Tasks

```c
void sensor_task(void *arg)
{
    /* Read sensor, publish data */
    float reading = read_sensor();
    gf_msg_publish("sensor/data", &reading, sizeof(reading),
                   GF_MSG_QOS_FIRE_FORGET, GF_MSG_PRIO_NORMAL);
}

/* Register task */
gf_task_t sensor = {
    .name = "sensor_task",
    .entry = sensor_task,
    .arg = NULL,
    .priority = GF_PRIORITY_NORMAL,
    .period_ms = 100,       /* Run every 100ms */
    .deadline_ms = 50       /* Must complete within 50ms */
};

uint8_t task_id;
gf_sched_task_create(&sensor, &task_id);
```

### Task Priority Levels

```c
GF_PRIORITY_IDLE     /* Background tasks */
GF_PRIORITY_LOW      /* Non-critical operations */
GF_PRIORITY_NORMAL   /* Standard priority */
GF_PRIORITY_HIGH     /* Time-sensitive tasks */
GF_PRIORITY_CRITICAL /* Real-time requirements */
```

## Using the CAN Driver

### Initialization

```c
gf_can_config_t can_config = {
    .baud_rate = GF_CAN_BAUD_500KBIT,
    .loopback = false,
    .silent = false,
    .auto_retransmit = true,
    .bus_off_recovery_ms = 1000
};

gf_can_init(&can_config);
gf_can_start();
```

### Sending Frames

```c
gf_can_frame_t frame = {
    .id = 0x123,
    .dlc = 8,
    .extended = false,
    .rtr = false,
    .data = {0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08}
};

gf_can_transmit(&frame, 100);  /* 100ms timeout */
```

### Receiving Frames

```c
/* Callback for received frames */
void on_can_rx(const gf_can_frame_t *frame, void *ctx)
{
    printf("CAN ID: 0x%03X, DLC: %d\n", frame->id, frame->dlc);
}

gf_can_set_rx_callback(on_can_rx, NULL);

/* Configure acceptance filter */
gf_can_filter_t filter = {
    .id = 0x100,
    .mask = 0x7F0,     /* Accept 0x100-0x10F */
    .fifo = 0,
    .extended = false
};
gf_can_set_filter(0, &filter);
```

### Error Handling

```c
void on_can_error(gf_can_err_type_t error, gf_can_err_state_t state, void *ctx)
{
    if (state == GF_CAN_ERR_BUSOFF) {
        /* Handle bus-off condition */
        gf_error_report(GF_SUBSYS_CAN, 0x01, GF_SEV_ERROR, "CAN bus-off");
    }
}

gf_can_set_error_callback(on_can_error, NULL);
```

## Using the Secure Bootloader

### From Application Code

```c
/* Confirm successful boot (call early in startup) */
gf_boot_init();
gf_boot_confirm();

/* Query boot information */
gf_boot_info_t info;
gf_boot_get_info(&info);
printf("Active slot: %d\n", info.active_slot);
printf("Boot count: %u\n", info.boot_count);
```

### OTA Update Flow

```c
/* 1. Download image to inactive slot */
/* 2. Verify image */
gf_boot_result_t result = gf_boot_verify_slot(1);  /* Verify slot B */
if (result != GF_BOOT_OK) {
    /* Handle verification failure */
    return;
}

/* 3. Set pending slot */
gf_boot_set_active_slot(1);

/* 4. Request reboot */
/* System will boot from new slot */
/* If boot fails, bootloader reverts to old slot */
```

## Error Handling Best Practices

### Reporting Errors

```c
/* Report with automatic recovery suggestion */
gf_recovery_t recovery = gf_error_report(
    GF_SUBSYS_CAN,      /* Subsystem */
    0x10,               /* Error code */
    GF_SEV_ERROR,       /* Severity */
    "TX timeout"        /* Message */
);

/* Take recommended action */
switch (recovery) {
    case GF_RECOVERY_RETRY:
        /* Retry the operation */
        break;
    case GF_RECOVERY_REINIT:
        /* Reinitialize the subsystem */
        gf_can_init(&config);
        break;
    case GF_RECOVERY_DEGRADE:
        /* Disable feature, continue */
        break;
    case GF_RECOVERY_RESET:
        /* System reset required */
        break;
}
```

### Registering Error Callbacks

```c
void my_error_handler(const gf_error_entry_t *entry, void *ctx)
{
    /* Log to SD card, send notification, etc. */
    if (entry->severity >= GF_SEV_ERROR) {
        led_set_error(true);
    }
}

gf_error_register_callback(my_error_handler, NULL);
```

## Logging

### Basic Logging

```c
GF_LOG_DEBUG("my_module", "Value: %d", value);
GF_LOG_INFO("my_module", "Initialization complete");
GF_LOG_WARN("my_module", "Low battery: %d%%", percent);
GF_LOG_ERROR("my_module", "Connection failed");
GF_LOG_FATAL("my_module", "Critical hardware fault");
```

### Conditional Logging

```c
GF_LOG_IF(temperature > 80, GF_LOG_LEVEL_WARN, "sensor",
          "High temperature: %.1f", temperature);
```

### Custom Log Sinks

```c
void uart_log_sink(const gf_log_entry_t *entry, void *ctx)
{
    char buffer[128];
    snprintf(buffer, sizeof(buffer), "[%s] %s: %s\n",
             gf_log_level_str(entry->level),
             entry->module,
             entry->message);
    uart_send(buffer);
}

gf_log_register_sink(uart_log_sink, NULL);
```

## Platform Porting

### HAL Functions to Implement

```c
/* Required by scheduler */
uint32_t hal_get_tick_ms(void);

/* Required by CAN driver */
void hal_can_write_reg(uint32_t addr, uint32_t value);
uint32_t hal_can_read_reg(uint32_t addr);

/* Required by secure boot */
void hal_flash_read(uint32_t addr, void *data, uint32_t len);
void hal_flash_write(uint32_t addr, const void *data, uint32_t len);

/* Required by watchdog */
void hal_wdt_kick(void);
void hal_wdt_init(uint32_t timeout_ms);
```

### Simulation Mode

For desktop development, stubs are provided:

```c
/* Desktop simulation (already included) */
#ifndef EMBEDDED_TARGET
uint32_t hal_get_tick_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return ts.tv_sec * 1000 + ts.tv_nsec / 1000000;
}
#endif
```

## Testing Your Integration

### Run Unit Tests
```bash
make test-unit
```

### Run Integration Tests
```bash
make test-integ
```

### Add Custom Tests

```c
/* In tests/unit_tests.c */
static void test_my_feature(void)
{
    /* Setup */
    my_init();
    
    /* Test */
    int result = my_function();
    GF_TEST_ASSERT_EQ(result, GF_OK);
    
    /* Verify */
    GF_TEST_ASSERT(my_state_valid());
}

/* Register in test suite */
static gf_test_suite_t my_suite = {
    .name = "My_Feature_Tests",
    .tests = {
        {"test_my_feature", test_my_feature},
    },
    .test_count = 1
};
```

## Common Integration Patterns

### Sensor → CAN → Cloud Pipeline

```c
/* Sensor task publishes readings */
void sensor_task(void *arg) {
    float value = read_sensor();
    gf_msg_publish("sensor/data", &value, sizeof(value),
                   GF_MSG_QOS_AT_LEAST_ONCE, GF_MSG_PRIO_NORMAL);
}

/* CAN gateway subscribes and forwards */
void can_gateway(const gf_message_t *msg, void *ctx) {
    gf_can_frame_t frame = {
        .id = 0x100,
        .dlc = 4
    };
    memcpy(frame.data, msg->payload, 4);
    gf_can_transmit(&frame, 100);
}
gf_msg_subscribe("sensor/*", can_gateway, NULL);

/* MQTT bridge for cloud upload */
void mqtt_bridge(const gf_message_t *msg, void *ctx) {
    gf_mqtt_publish(msg->topic, msg->payload, msg->payload_len);
}
gf_msg_subscribe("sensor/*", mqtt_bridge, NULL);
```

## Reliability Features (Phase 4)

### Task Watchdog Supervision

Monitor individual tasks with configurable timeouts and recovery actions.

```c
#include "core/reliability.h"

/* Initialize reliability subsystem */
gf_rel_init();

/* Register task for monitoring */
gf_task_wdt_config_t wdt_config = {
    .task_name = "sensor_task",
    .timeout_ms = 1000,
    .mode = GF_WDT_MODE_SIMPLE,
    .critical = true    /* Reset if timeout */
};
gf_task_wdt_handle_t wdt = gf_rel_wdt_register(&wdt_config);

/* Kick watchdog in task loop */
void sensor_task(void *arg) {
    while (1) {
        gf_rel_wdt_kick(wdt);
        
        /* ... task work ... */
        
        sleep_ms(100);
    }
}

/* For deadline-critical operations */
gf_rel_wdt_start_deadline(wdt);
do_critical_operation();
gf_rel_wdt_complete_deadline(wdt);
```

### Persistent Fault Logging

Record faults to survive reboot for post-mortem analysis.

```c
/* Record a fault */
gf_rel_fault_record(
    GF_FAULT_COMM_TIMEOUT,      /* Fault type */
    GF_FAULT_SEV_MAJOR,         /* Severity */
    GF_SUBSYS_CAN,              /* Subsystem */
    "CAN bus timeout"           /* Description */
);

/* Retrieve recent faults */
gf_persistent_fault_t faults[10];
int count = gf_rel_fault_get_recent(faults, 10);

/* Get statistics */
gf_fault_stats_t stats;
gf_rel_fault_get_stats(&stats);
printf("Total faults: %u, Critical: %u\n", 
       stats.total_faults, stats.critical_count);

/* Acknowledge and clear */
gf_rel_fault_acknowledge(fault_timestamp);
gf_rel_fault_clear_acknowledged();
```

### Recovery Strategies

Configure automatic recovery based on fault type.

```c
/* Set recovery policy */
gf_recovery_policy_t policy = {
    .fault_type = GF_FAULT_COMM_TIMEOUT,
    .strategy = GF_RECOVERY_STRATEGY_RETRY,
    .max_retries = 3,
    .retry_delay_ms = 100,
    .escalate_on_fail = true    /* Escalate to next strategy */
};
gf_rel_set_recovery_policy(&policy);

/* Register custom recovery hook */
bool my_recovery_hook(gf_fault_type_t fault, void *ctx) {
    if (fault == GF_FAULT_COMM_TIMEOUT) {
        reinit_communication();
        return true;    /* Recovery succeeded */
    }
    return false;
}
gf_rel_register_recovery_hook(GF_FAULT_COMM_TIMEOUT, 
                               my_recovery_hook, NULL);

/* Safe mode operations */
if (critical_failure_detected) {
    gf_rel_enter_safe_mode("Critical sensor failure");
}

/* Feature degradation */
gf_rel_disable_feature(FEATURE_LOGGING);  /* Disable non-essential */
if (gf_rel_is_feature_disabled(FEATURE_LOGGING)) {
    /* Skip logging operations */
}
```

### Fault Injection for Testing

Test recovery paths with controlled fault injection.

```c
/* Enable with GF_ENABLE_FAULT_INJECTION define */
#ifdef GF_ENABLE_FAULT_INJECTION

/* Configure fault injection */
gf_inject_config_t inject = {
    .type = GF_INJECT_COMM_FAIL,
    .probability = 50,       /* 50% chance */
    .subsystem = 0,          /* All subsystems */
    .one_shot = false,
    .trigger_after = 10      /* After 10 operations */
};
gf_rel_inject_configure(&inject);
gf_rel_inject_enable(GF_INJECT_COMM_FAIL);

/* In code under test */
if (GF_CHECK_INJECT(GF_INJECT_COMM_FAIL, GF_SUBSYS_CAN)) {
    /* Simulate communication failure */
    return GF_ERR_TIMEOUT;
}

/* Check injection status */
gf_inject_status_t status;
gf_rel_inject_get_status(&status);
printf("Injections triggered: %u\n", status.fault_triggered);

#endif
```

## Metrics Collection

### Counter Metrics

Track cumulative values (events, bytes, errors).

```c
#include "core/metrics.h"

gf_metrics_init();

/* Create counter */
gf_counter_handle_t rx_counter = 
    gf_metrics_counter_create("can_rx_frames");

/* Increment on event */
gf_metrics_counter_inc(rx_counter);
gf_metrics_counter_add(rx_counter, frame_count);

/* Read value */
uint64_t total = gf_metrics_counter_get(rx_counter);
```

### Gauge Metrics

Track point-in-time values with min/max/avg statistics.

```c
/* Create gauge */
gf_gauge_handle_t temp_gauge = 
    gf_metrics_gauge_create("cpu_temperature");

/* Update value */
gf_metrics_gauge_set(temp_gauge, read_temperature());

/* Get statistics */
int32_t min, max, avg;
gf_metrics_gauge_get_stats(temp_gauge, &min, &max, &avg);
```

### Histogram Metrics

Track value distributions (latency, sizes).

```c
/* Define latency buckets (microseconds) */
uint32_t latency_buckets[] = {10, 50, 100, 500, 1000, 5000};
gf_histogram_handle_t latency = 
    gf_metrics_histogram_create("task_latency", latency_buckets, 6);

/* Record observations */
uint32_t start = gf_metrics_get_tick();
do_work();
gf_metrics_histogram_observe(latency, gf_metrics_get_tick() - start);

/* Get percentiles */
uint32_t p50 = gf_metrics_histogram_percentile(latency, 50);
uint32_t p99 = gf_metrics_histogram_percentile(latency, 99);
```

### Export Metrics

```c
/* Export in Prometheus format */
char buffer[2048];
int len = gf_metrics_export_text(buffer, sizeof(buffer));
/* Send buffer over HTTP, UART, etc. */
```

## Health Monitoring

### Register Health Checks

```c
#include "core/health.h"

gf_health_init();

/* Define health check callback */
gf_health_status_t check_sensor(void *ctx, char *msg, size_t len) {
    if (!sensor_connected()) {
        snprintf(msg, len, "Sensor disconnected");
        return GF_HEALTH_FAILED;
    }
    if (sensor_temp() > 85) {
        snprintf(msg, len, "Temp: %d C", sensor_temp());
        return GF_HEALTH_WARNING;
    }
    return GF_HEALTH_OK;
}

/* Register check (runs every 5 seconds) */
int sensor_id = gf_health_register("temperature_sensor", 
                                    check_sensor, NULL, 5000);

/* Run checks in main loop */
void main_loop(void) {
    gf_health_check_all();  /* Call periodically */
    gf_health_publish_heartbeat();
}

/* Query health status */
gf_health_summary_t summary;
gf_health_get_summary(&summary);
if (summary.overall_status >= GF_HEALTH_CRITICAL) {
    /* Take protective action */
}
```

### Remote Logging

```c
/* Initialize and configure */
gf_remote_log_init();
gf_remote_log_set_level(GF_LOG_LEVEL_INFO);
gf_remote_log_enable(true);

/* Optional: set transport callback */
void send_to_cloud(const gf_remote_log_entry_t *entry, void *ctx) {
    mqtt_publish("device/logs", entry, sizeof(*entry));
}
gf_remote_log_config_t log_cfg = {
    .min_level = GF_LOG_LEVEL_WARN,
    .enabled = true,
    .transport = send_to_cloud,
    .transport_ctx = NULL
};
gf_remote_log_configure(&log_cfg);

/* Log messages */
GF_RLOG_INFO(GF_SUBSYS_CORE, "System started, version %s", VERSION);
GF_RLOG_WARN(GF_SUBSYS_SENSOR, "High temperature: %d", temp);
GF_RLOG_ERROR(GF_SUBSYS_CAN, "Bus-off detected");

/* If no transport, logs queue up */
/* Flush when transport available */
gf_remote_log_flush();
```

---

*For more details, see the source code comments and the API documentation in header files.*
