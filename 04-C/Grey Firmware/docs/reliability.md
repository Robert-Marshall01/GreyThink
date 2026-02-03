# Reliability Engineering Guide

This guide covers Grey Firmware's reliability features for building robust, fault-tolerant embedded systems.

## Overview

Modern embedded systems require production-grade reliability infrastructure:

- **Watchdog Supervision**: Detect and recover from task hangs
- **Fault Logging**: Persistent records for post-mortem analysis
- **Recovery Strategies**: Automated fault response and graceful degradation
- **Fault Injection**: Chaos engineering for embedded systems
- **Health Monitoring**: Proactive failure detection
- **Telemetry**: Metrics collection and remote logging

## Industry Standards Context

These features support compliance with:

| Standard | Domain | Reliability Requirement |
|----------|--------|------------------------|
| ISO 26262 | Automotive | ASIL A-D fault detection and handling |
| IEC 62304 | Medical | Risk management and fault tolerance |
| IEC 61508 | Industrial | SIL 1-4 safety integrity |
| DO-178C | Aerospace | DAL A-E software assurance |

---

## Task Watchdog Supervision

### Concept

Unlike hardware watchdogs that monitor the entire system, task-level watchdog supervision monitors individual tasks. This allows:

- Identifying which specific task hung
- Recovering individual tasks without system reset
- Different timeout policies per task criticality

### Implementation

```c
#include "core/reliability.h"

/* Initialize reliability subsystem */
gf_rel_init();

/* Configure task watchdog */
gf_task_wdt_config_t config = {
    .task_name = "sensor_task",
    .timeout_ms = 1000,
    .mode = GF_WDT_MODE_SIMPLE,
    .critical = true
};

/* Register for monitoring */
gf_task_wdt_handle_t wdt = gf_rel_wdt_register(&config);

/* Task must kick watchdog before timeout */
void sensor_task(void *arg)
{
    while (1) {
        gf_rel_wdt_kick(wdt);
        
        /* Do work */
        read_sensors();
        
        delay_ms(100);
    }
}
```

### Watchdog Modes

| Mode | Use Case | Behavior |
|------|----------|----------|
| `GF_WDT_MODE_SIMPLE` | Periodic tasks | Must kick before timeout |
| `GF_WDT_MODE_WINDOW` | Timing-critical | Must kick within time window |
| `GF_WDT_MODE_DEADLINE` | Operations with deadlines | Start/complete bracketing |

### Window Mode

Prevents both too-early and too-late kicks:

```c
gf_task_wdt_config_t config = {
    .task_name = "timing_critical",
    .timeout_ms = 100,
    .window_min_ms = 40,    /* Must wait at least 40ms */
    .mode = GF_WDT_MODE_WINDOW,
    .critical = true
};
```

### Deadline Mode

For operations that must complete within a time limit:

```c
gf_rel_wdt_start_deadline(wdt);

/* Critical operation - must complete before deadline */
result = flash_write(addr, data, len);

gf_rel_wdt_complete_deadline(wdt);
```

### Periodic Check

Call from a timer interrupt or high-priority task:

```c
void timer_isr(void)
{
    gf_rel_wdt_check_all();  /* Check all registered watchdogs */
}
```

---

## Persistent Fault Logging

### Why Persistent?

Embedded systems often operate unattended. When failures occur:

- The user may not be present
- Traditional logs are lost on reset
- Root cause analysis requires history
- Fleet-wide trends need data collection

### Fault Types

```c
typedef enum {
    GF_FAULT_NONE = 0,
    GF_FAULT_WATCHDOG_TIMEOUT,
    GF_FAULT_STACK_OVERFLOW,
    GF_FAULT_HEAP_EXHAUSTED,
    GF_FAULT_HARDFAULT,
    GF_FAULT_BUSFAULT,
    GF_FAULT_MEMFAULT,
    GF_FAULT_USAGEFAULT,
    GF_FAULT_ASSERT_FAIL,
    GF_FAULT_COMM_TIMEOUT,
    GF_FAULT_CRC_ERROR,
    GF_FAULT_CONFIG_CORRUPT,
    GF_FAULT_SENSOR_FAIL,
    GF_FAULT_POWER_FAIL,
    GF_FAULT_USER_DEFINED = 0x80
} gf_fault_type_t;
```

### Severity Levels

```c
typedef enum {
    GF_FAULT_SEV_INFO = 0,      /* Informational */
    GF_FAULT_SEV_MINOR,         /* Minor, recoverable */
    GF_FAULT_SEV_MAJOR,         /* Significant, degraded op */
    GF_FAULT_SEV_CRITICAL,      /* Needs immediate attention */
    GF_FAULT_SEV_FATAL          /* Caused system reset */
} gf_fault_severity_t;
```

### Recording Faults

```c
/* Simple recording */
gf_rel_fault_record(
    GF_FAULT_COMM_TIMEOUT,
    GF_FAULT_SEV_MAJOR,
    GF_SUBSYS_CAN,
    "CAN bus timeout on node 3"
);

/* Extended recording with more context */
gf_persistent_fault_t fault = {
    .type = GF_FAULT_HARDFAULT,
    .severity = GF_FAULT_SEV_FATAL,
    .subsystem = GF_SUBSYS_CORE,
    .fault_address = (uint32_t)__builtin_return_address(0),
    .extra_data = SCB->CFSR,  /* Fault status register */
};
snprintf(fault.message, sizeof(fault.message), 
         "HardFault at 0x%08X", fault.fault_address);
gf_rel_fault_record_ex(&fault);
```

### Retrieving Fault History

```c
/* Get recent faults */
gf_persistent_fault_t recent[10];
int count = gf_rel_fault_get_recent(recent, 10);

for (int i = 0; i < count; i++) {
    printf("[%u] Type: %d, Sev: %d, Subsys: %d\n",
           recent[i].timestamp,
           recent[i].type,
           recent[i].severity,
           recent[i].subsystem);
    printf("    Message: %s\n", recent[i].message);
}

/* Get statistics */
gf_fault_stats_t stats;
gf_rel_fault_get_stats(&stats);

printf("Total faults: %u\n", stats.total_faults);
printf("Fatal: %u, Critical: %u, Minor: %u\n",
       stats.fatal_count, stats.critical_count, stats.minor_count);
printf("Reboots: %u, Log rollovers: %u\n",
       stats.reboot_count, stats.log_rollovers);
```

### Fault Acknowledgment

For field service workflows:

```c
/* Mark fault as acknowledged */
gf_rel_fault_acknowledge(fault_timestamp);

/* Clear acknowledged faults */
int cleared = gf_rel_fault_clear_acknowledged();
printf("Cleared %d acknowledged faults\n", cleared);

/* Sync to flash for persistence */
gf_rel_fault_sync();
```

---

## Recovery Strategies

### Strategy Types

| Strategy | Description | Use Case |
|----------|-------------|----------|
| `NONE` | No automatic action | Logging-only faults |
| `RETRY` | Retry failed operation | Transient errors |
| `REINIT` | Reinitialize subsystem | State corruption |
| `FALLBACK` | Use alternative | Sensor redundancy |
| `DEGRADE` | Disable feature | Non-critical failures |
| `SAFE_MODE` | Minimal operation | Partial system failure |
| `RESET` | System reset | Unrecoverable state |

### Configuring Policies

```c
/* Retry communication errors up to 3 times */
gf_recovery_policy_t comm_policy = {
    .fault_type = GF_FAULT_COMM_TIMEOUT,
    .strategy = GF_RECOVERY_STRATEGY_RETRY,
    .max_retries = 3,
    .retry_delay_ms = 100,
    .escalate_on_fail = true  /* Escalate to next strategy */
};
gf_rel_set_recovery_policy(&comm_policy);

/* Enter safe mode on sensor failure */
gf_recovery_policy_t sensor_policy = {
    .fault_type = GF_FAULT_SENSOR_FAIL,
    .strategy = GF_RECOVERY_STRATEGY_SAFE_MODE,
    .max_retries = 0,
    .escalate_on_fail = false
};
gf_rel_set_recovery_policy(&sensor_policy);
```

### Custom Recovery Hooks

```c
bool can_recovery_hook(gf_fault_type_t fault, void *ctx)
{
    if (fault == GF_FAULT_COMM_TIMEOUT) {
        /* Attempt to recover CAN bus */
        gf_can_stop();
        delay_ms(100);
        gf_can_start();
        
        return true;  /* Recovery succeeded */
    }
    return false;  /* Let default handler run */
}

gf_rel_register_recovery_hook(GF_FAULT_COMM_TIMEOUT, 
                               can_recovery_hook, NULL);
```

### Executing Recovery

```c
/* Automatic: called when watchdog timeout occurs */
gf_recovery_strategy_t strategy = 
    gf_rel_execute_recovery(GF_FAULT_WATCHDOG_TIMEOUT);

/* Manual: respond to detected fault */
if (sensor_read_failed) {
    gf_rel_fault_record(GF_FAULT_SENSOR_FAIL, GF_FAULT_SEV_MAJOR,
                       GF_SUBSYS_SENSOR, "Primary sensor failed");
    gf_rel_execute_recovery(GF_FAULT_SENSOR_FAIL);
}
```

### Safe Mode Operation

```c
/* Enter safe mode */
gf_rel_enter_safe_mode("Critical sensor failure");

/* Check if in safe mode */
gf_recovery_status_t status;
gf_rel_get_recovery_status(&status);

if (status.in_safe_mode) {
    /* Run with reduced functionality */
    run_safe_mode_loop();
}

/* Exit safe mode when conditions allow */
if (all_sensors_operational()) {
    gf_rel_exit_safe_mode();
}
```

### Graceful Degradation

Selectively disable features instead of full failure:

```c
#define FEATURE_LOGGING     0x01
#define FEATURE_TELEMETRY   0x02
#define FEATURE_DIAGNOSTICS 0x04

/* Disable non-essential feature */
if (memory_low) {
    gf_rel_disable_feature(FEATURE_TELEMETRY);
}

/* Check before using feature */
if (!gf_rel_is_feature_disabled(FEATURE_LOGGING)) {
    write_log_entry(...);
}

/* Re-enable when resources available */
gf_rel_enable_feature(FEATURE_TELEMETRY);
```

---

## Fault Injection Testing

### Why Fault Injection?

> "Hope is not a strategy" - Testing recovery paths requires triggering them.

Fault injection (chaos engineering for embedded) allows:

- Testing recovery code that rarely executes
- Validating watchdog timeouts
- Simulating hardware failures
- Stress testing error handlers

### Enabling Fault Injection

Compile with `-DGF_ENABLE_FAULT_INJECTION`:

```makefile
CFLAGS += -DGF_ENABLE_FAULT_INJECTION
```

**WARNING**: Never enable in production builds!

### Injection Types

```c
typedef enum {
    GF_INJECT_NONE = 0,
    GF_INJECT_MEMORY_CORRUPT,   /* Memory corruption */
    GF_INJECT_COMM_FAIL,        /* Communication failure */
    GF_INJECT_SENSOR_FAIL,      /* Sensor read failure */
    GF_INJECT_POWER_GLITCH,     /* Power brownout */
    GF_INJECT_FLASH_ERROR,      /* Flash write failure */
    GF_INJECT_CRC_FAIL,         /* CRC mismatch */
    GF_INJECT_TIMEOUT,          /* Operation timeout */
    GF_INJECT_OVERFLOW,         /* Buffer overflow */
    GF_INJECT_RANDOM            /* Random injection */
} gf_inject_type_t;
```

### Configuring Injection

```c
gf_inject_config_t config = {
    .type = GF_INJECT_COMM_FAIL,
    .probability = 10,      /* 10% chance per check */
    .subsystem = 0,         /* All subsystems (0 = any) */
    .trigger_after = 100,   /* After 100 operations */
    .one_shot = false       /* Continuous injection */
};

gf_rel_inject_configure(&config);
gf_rel_inject_enable(GF_INJECT_COMM_FAIL);
```

### Instrumenting Code

Add injection points to code under test:

```c
int can_transmit(const gf_can_frame_t *frame)
{
    /* Check for injected fault */
    if (GF_CHECK_INJECT(GF_INJECT_COMM_FAIL, GF_SUBSYS_CAN)) {
        return GF_ERR_TIMEOUT;  /* Simulate failure */
    }
    
    /* Normal operation */
    return hardware_transmit(frame);
}
```

### One-Shot Testing

Test recovery from single fault:

```c
/* Configure for single injection */
gf_inject_config_t config = {
    .type = GF_INJECT_FLASH_ERROR,
    .probability = 100,  /* Guaranteed trigger */
    .one_shot = true     /* Only once */
};

gf_rel_inject_configure(&config);
gf_rel_inject_enable(GF_INJECT_FLASH_ERROR);

/* Next flash write will fail */
result = flash_write(addr, data, len);
assert(result == GF_ERR_FLASH);

/* Verify recovery */
result = flash_write(addr, data, len);  /* Should succeed */
assert(result == GF_OK);
```

### Monitoring Injection

```c
gf_inject_status_t status;
gf_rel_inject_get_status(&status);

printf("Injection active: %s\n", status.active ? "yes" : "no");
printf("Type: %d\n", status.current_type);
printf("Injections: %u\n", status.injection_count);
printf("Faults triggered: %u\n", status.fault_triggered);

/* Reset counters */
gf_rel_inject_reset();
```

---

## Health Monitoring

### Architecture

```
┌──────────────┐   ┌──────────────┐   ┌──────────────┐
│   Sensor     │   │     CAN      │   │    Power     │
│   Health     │   │    Health    │   │   Health     │
│   Check      │   │    Check     │   │    Check     │
└──────┬───────┘   └──────┬───────┘   └──────┬───────┘
       │                  │                   │
       └──────────────────┼───────────────────┘
                          ▼
              ┌───────────────────────┐
              │    Health Monitor     │
              │  - Periodic checks    │
              │  - Status aggregation │
              │  - Heartbeat publish  │
              └───────────┬───────────┘
                          │
                          ▼
              ┌───────────────────────┐
              │     Message Bus       │
              │  "health/heartbeat"   │
              └───────────────────────┘
```

### Health Status Levels

| Status | Description | Action |
|--------|-------------|--------|
| `OK` | Operating normally | None |
| `DEGRADED` | Reduced capability | Monitor closely |
| `WARNING` | Attention needed | Schedule maintenance |
| `CRITICAL` | Immediate attention | Alert operator |
| `FAILED` | Component non-functional | Initiate recovery |

### Registering Health Checks

```c
gf_health_init();

/* Define health check function */
gf_health_status_t check_sensor(void *ctx, char *msg, size_t len)
{
    if (!sensor_connected()) {
        snprintf(msg, len, "Disconnected");
        return GF_HEALTH_FAILED;
    }
    
    int temp = sensor_temperature();
    if (temp > 80) {
        snprintf(msg, len, "High temp: %d C", temp);
        return GF_HEALTH_WARNING;
    }
    
    return GF_HEALTH_OK;
}

/* Register (checks every 5 seconds) */
int id = gf_health_register("temperature", check_sensor, NULL, 5000);
```

### Running Health Checks

```c
/* Call periodically from main loop */
void main_loop(void)
{
    gf_health_check_all();
    
    /* Publish heartbeat for monitoring systems */
    gf_health_publish_heartbeat();
}
```

### Querying Health Status

```c
/* Overall health */
gf_health_summary_t summary;
gf_health_get_summary(&summary);

printf("Overall: %d\n", summary.overall_status);
printf("Healthy: %d, Degraded: %d\n", 
       summary.healthy_count, summary.degraded_count);
printf("Warning: %d, Critical: %d, Failed: %d\n",
       summary.warning_count, summary.critical_count, 
       summary.failed_count);

/* Individual component */
gf_component_health_t health;
gf_health_get_component(id, &health);

printf("Component: %s\n", health.name);
printf("Status: %d\n", health.status);
printf("Message: %s\n", health.message);
printf("Fail count: %u\n", health.fail_count);
```

---

## Remote Logging

### Configuration

```c
gf_remote_log_init();

/* Set minimum level for transmission */
gf_remote_log_set_level(GF_LOG_LEVEL_WARN);

/* Enable remote logging */
gf_remote_log_enable(true);
```

### With Transport Callback

```c
void mqtt_log_transport(const gf_remote_log_entry_t *entry, void *ctx)
{
    char topic[64];
    snprintf(topic, sizeof(topic), "device/%s/logs", device_id);
    mqtt_publish(topic, entry->message, strlen(entry->message));
}

gf_remote_log_config_t config = {
    .min_level = GF_LOG_LEVEL_INFO,
    .enabled = true,
    .transport = mqtt_log_transport,
    .transport_ctx = NULL
};

gf_remote_log_configure(&config);
```

### Logging Messages

```c
/* Using convenience macros */
GF_RLOG_INFO(GF_SUBSYS_CORE, "System booted, version %s", VERSION);
GF_RLOG_WARN(GF_SUBSYS_SENSOR, "High temperature: %d", temp);
GF_RLOG_ERROR(GF_SUBSYS_CAN, "Bus-off detected");

/* Using function directly */
gf_remote_log(GF_LOG_LEVEL_ERROR, GF_SUBSYS_POWER, 
              "Voltage below threshold: %.2fV", voltage);
```

### Offline Queueing

When transport is unavailable, messages queue locally:

```c
/* Log some messages (queued if no transport) */
GF_RLOG_ERROR(GF_SUBSYS_CAN, "Error 1");
GF_RLOG_ERROR(GF_SUBSYS_CAN, "Error 2");

/* Later, when transport available */
int flushed = gf_remote_log_flush();
printf("Sent %d queued messages\n", flushed);

/* Check statistics */
uint32_t queued, sent, dropped;
gf_remote_log_get_stats(&queued, &sent, &dropped);
```

---

## Metrics Collection

See [integration.md](integration.md#metrics-collection) for detailed metrics API usage.

### Quick Reference

```c
/* Counters - monotonically increasing */
gf_counter_handle_t errors = gf_metrics_counter_create("error_count");
gf_metrics_counter_inc(errors);

/* Gauges - point-in-time values */
gf_gauge_handle_t temp = gf_metrics_gauge_create("temperature");
gf_metrics_gauge_set(temp, current_temp);

/* Histograms - distributions */
uint32_t buckets[] = {10, 50, 100, 500};
gf_histogram_handle_t lat = gf_metrics_histogram_create("latency", buckets, 4);
gf_metrics_histogram_observe(lat, measured_latency);

/* Export */
char buffer[2048];
gf_metrics_export_text(buffer, sizeof(buffer));
```

---

## Best Practices

### 1. Defense in Depth

```c
/* Multiple layers of protection */
gf_rel_wdt_kick(wdt);           /* Task watchdog */
gf_wdt_kick();                   /* System watchdog */
gf_health_check_all();           /* Health monitoring */
gf_rel_fault_sync();             /* Persist fault log */
```

### 2. Fail Fast, Recover Gracefully

```c
/* Detect failures early */
if (!validate_config()) {
    gf_rel_fault_record(GF_FAULT_CONFIG_CORRUPT, GF_FAULT_SEV_CRITICAL,
                       GF_SUBSYS_CORE, "Config validation failed");
    gf_rel_enter_safe_mode("Invalid configuration");
    return;
}
```

### 3. Log Meaningful Context

```c
/* Not this */
gf_rel_fault_record(GF_FAULT_SENSOR_FAIL, GF_FAULT_SEV_MAJOR,
                   0, "fail");

/* This */
gf_rel_fault_record(GF_FAULT_SENSOR_FAIL, GF_FAULT_SEV_MAJOR,
                   GF_SUBSYS_SENSOR,
                   "Temp sensor #3 read error: I2C NACK at 0x48");
```

### 4. Test Recovery Paths

```c
/* In test builds */
#ifdef GF_ENABLE_FAULT_INJECTION
void test_watchdog_recovery(void)
{
    /* Inject timeout */
    gf_inject_config_t cfg = {.type = GF_INJECT_TIMEOUT, .probability = 100};
    gf_rel_inject_configure(&cfg);
    gf_rel_inject_enable(GF_INJECT_TIMEOUT);
    
    /* Trigger and verify recovery */
    run_task();
    
    gf_recovery_status_t status;
    gf_rel_get_recovery_status(&status);
    assert(status.recovery_count > 0);
}
#endif
```

### 5. Monitor Trends

```c
/* Periodic metrics check */
void check_trends(void)
{
    gf_fault_stats_t stats;
    gf_rel_fault_get_stats(&stats);
    
    /* Alert if fault rate increasing */
    static uint32_t last_faults = 0;
    uint32_t new_faults = stats.total_faults - last_faults;
    last_faults = stats.total_faults;
    
    if (new_faults > FAULT_RATE_THRESHOLD) {
        GF_RLOG_WARN(GF_SUBSYS_CORE, 
                     "High fault rate: %u in last period", new_faults);
    }
}
```

---

## Testing

### Running Reliability Tests

```bash
# All tests
make test

# Fault injection tests specifically
make test-fault
```

### Test Coverage

The fault injection test suite covers:
- Task watchdog registration and timeout
- Persistent fault logging and rollover
- Recovery strategy execution
- Fault injection mechanics
- Integrated resilience scenarios

See `tests/fault_injection_tests.c` for examples.

---

*For API details, see header files in `include/core/`.*
