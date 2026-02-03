# Pipeline Leak Detection & Safety System

## Overview

The Pipeline Leak Detection & Safety spotlight provides production-grade infrastructure monitoring for oil, gas, and liquid transmission pipelines. This module implements multiple industry-standard leak detection algorithms, emergency shutdown valve control, and SCADA-compatible telemetry.

**Industry Relevance:** Critical infrastructure protection systems for energy sector - relevant to roles at Emerson, Honeywell, Schneider Electric, ABB, and pipeline operators like Enterprise Products, Kinder Morgan, Williams Companies.

## Features

### Flow Monitoring
- Ultrasonic flow measurement support
- Coriolis mass flow integration
- Turbine and vortex flow sensors
- Flow rate trending and anomaly detection
- Configurable sampling rates (1-100 Hz)

### Leak Detection Methods

#### 1. Mass Balance Analysis
Compares upstream and downstream flow rates to detect product loss:
- Imbalance threshold: 5% triggers alarm
- Warning at 2% imbalance
- Leak classification: Minor (<1%), Moderate (1-5%), Major (5-20%), Rupture (>20%)

#### 2. Pressure Point Analysis (PPA)
Monitors pressure gradients between sensors:
- Detects abnormal pressure drops between adjacent sensors
- Gradient analysis identifies approximate leak location
- Multiple pressure sensor support per segment

#### 3. Negative Pressure Wave (NPW)
Detects rapid pressure drops indicating pipe rupture:
- High-confidence rupture detection
- Threshold: 0.5 bar/second pressure drop
- Immediate alarm generation
- Location estimation from sensor position

#### 4. Statistical Pattern Recognition (SPR)
Learning-based anomaly detection:
- Historical baseline comparison
- Z-score analysis for flow deviations
- Detects subtle, gradual leaks
- Requires 60+ samples for baseline

#### 5. Rate of Change (ROC)
Monitors rapid parameter changes:
- Flow rate change threshold: 5.0 m³/h/sec
- Pressure rate change monitoring
- Early warning for developing issues

### Blockage Detection
Identifies flow restrictions:
- Flow ratio monitoring (output/input)
- Pressure buildup correlation
- Threshold: Output < 70% of input with high pressure

### Emergency Response

#### Valve Control
- Emergency Shutdown Valve (ESV) management
- Automatic isolation on confirmed major leak
- Configurable stroke times
- State tracking (Open, Closed, Opening, Closing, Fault)

#### Segment Management
- Up to 16 pipeline segments
- Individual segment status tracking
- Maintenance mode for safe operations
- Isolation status for leak containment

### Telemetry & Logging

#### Real-time Telemetry
- SCADA-compatible packet format
- Per-segment status reporting
- Flow and pressure data
- Alert summary

#### Event Logging
- Circular buffer (256 events)
- Timestamped entries
- Event types: System, Sensor, Leak, Valve, Shutdown, Maintenance
- Compliance audit trail support

## Data Structures

### Sensor Types
```c
typedef enum {
    SENSOR_FLOW,           // Flow meter
    SENSOR_PRESSURE,       // Pressure transducer
    SENSOR_TEMPERATURE,    // Temperature sensor
    SENSOR_DENSITY,        // Densitometer
    SENSOR_PIG_DETECTOR    // Pig passage detector
} pipe_sensor_type_t;
```

### Product Types
```c
typedef enum {
    PRODUCT_CRUDE_OIL,
    PRODUCT_REFINED_FUEL,
    PRODUCT_NATURAL_GAS,
    PRODUCT_LPG,
    PRODUCT_WATER,
    PRODUCT_CHEMICAL
} pipe_product_t;
```

### Leak Severity
```c
typedef enum {
    LEAK_NONE,
    LEAK_MINOR,      // <1% of flow
    LEAK_MODERATE,   // 1-5% of flow
    LEAK_MAJOR,      // 5-20% of flow
    LEAK_RUPTURE     // >20% or sudden event
} pipe_leak_severity_t;
```

## API Reference

### Initialization
```c
int pipe_init(uint32_t pipeline_id, pipe_product_t product);
int pipe_shutdown(void);
```

### Configuration
```c
int pipe_register_sensor(const pipe_sensor_config_t* config);
int pipe_configure_segment(const pipe_segment_config_t* config);
int pipe_configure_valve(const pipe_valve_config_t* config);
```

### Sensor Updates
```c
int pipe_update_sensor(uint8_t sensor_id, float value, float quality);
int pipe_set_sensor_online(uint8_t sensor_id, bool online);
```

### Monitoring Control
```c
int pipe_start_monitoring(void);
int pipe_stop_monitoring(void);
int pipe_process(uint32_t delta_ms);  // Call periodically
```

### Status Queries
```c
int pipe_get_segment_status(uint8_t segment_id, pipe_seg_status_t* status,
                            float* imbalance, float* leak_rate);
int pipe_get_alerts(pipe_alert_t* alerts, uint8_t max_alerts, uint8_t* count);
int pipe_acknowledge_alert(uint32_t alert_id);
int pipe_get_stats(pipe_stats_t* stats);
int pipe_get_events(pipe_event_t* events, uint16_t max_events, uint16_t* count);
```

### Control Operations
```c
int pipe_trigger_shutdown(uint8_t segment_id);
int pipe_control_valve(uint8_t valve_id, bool open);
int pipe_set_maintenance(uint8_t segment_id, bool maintenance);
```

### Telemetry
```c
int pipe_generate_telemetry(uint8_t* buffer, uint16_t max_len, uint16_t* len);
```

### Testing/Simulation
```c
int pipe_inject_leak(uint8_t segment_id, float leak_rate, float location_km);
int pipe_inject_drift(uint8_t sensor_id, float drift_rate);
```

## Configuration Example

```c
// Initialize pipeline system
pipe_init(1001, PRODUCT_CRUDE_OIL);

// Register flow sensors
pipe_sensor_config_t flow_in = {
    .sensor_id = 1,
    .type = SENSOR_FLOW,
    .location_km = 0.0f,
    .segment_id = 1,
    .low_limit = 0,
    .high_limit = 1000,
    .sample_rate_hz = 1,
    .enabled = true
};
pipe_register_sensor(&flow_in);

// Configure segment
pipe_segment_config_t segment = {
    .segment_id = 1,
    .name = "MainLine-1",
    .start_km = 0.0f,
    .end_km = 10.0f,
    .diameter_mm = 600,
    .product = PRODUCT_CRUDE_OIL,
    .upstream_flow_sensor = 1,
    .downstream_flow_sensor = 2,
    .pressure_sensors = {3, 4, 5},
    .pressure_sensor_count = 3,
    .imbalance_threshold = 5.0f,
    .settling_time_ms = 30000,
    .leak_detection_enabled = true
};
pipe_configure_segment(&segment);

// Configure ESV
pipe_valve_config_t esv = {
    .valve_id = 1,
    .name = "ESV-1",
    .location_km = 0.0f,
    .segment_down = 1,
    .esv = true,
    .stroke_time_ms = 2000
};
pipe_configure_valve(&esv);

// Start monitoring
pipe_start_monitoring();
```

## Message Bus Topics

| Topic | Description |
|-------|-------------|
| `pipeline/flow` | Flow rate updates |
| `pipeline/pressure` | Pressure readings |
| `pipeline/leak/alert` | Leak detected notification |
| `pipeline/leak/confirmed` | Confirmed leak event |
| `pipeline/pressure/anomaly` | Pressure anomaly detected |
| `pipeline/segment` | Segment status change |
| `pipeline/valve` | Valve state change |
| `pipeline/shutdown` | Emergency shutdown event |
| `pipeline/telemetry` | Periodic telemetry packet |
| `pipeline/event` | Generic pipeline event |

## Compliance Standards

### API 1130 - Computational Pipeline Monitoring
- Real-time leak detection algorithms
- Multiple detection method support
- Alarm and event logging

### API 1160 - Managing System Integrity
- Segment-based integrity management
- Condition monitoring and trending
- Maintenance scheduling support

### PHMSA 49 CFR 192 (Natural Gas)
- Leak detection requirements
- Emergency response procedures
- Documentation and record-keeping

### PHMSA 49 CFR 195 (Hazardous Liquids)
- Computational Pipeline Monitoring requirements
- Rupture detection capability
- Automatic/remote valve closure

## Performance Characteristics

| Parameter | Value |
|-----------|-------|
| Max Segments | 16 |
| Max Sensors | 32 |
| Max Valves | 16 |
| Max Active Alerts | 64 |
| Event Log Size | 256 entries |
| History Buffer | 120 samples (2 min @ 1Hz) |
| Detection Latency | <1 second for NPW |
| Mass Balance Response | 30 seconds (settling) |

## Safety Features

1. **Fail-Safe Design**
   - ESVs default to closing on loss of control
   - Sensor redundancy for critical measurements
   - Automatic isolation on confirmed major leak

2. **Alarm Philosophy**
   - Warning (2% imbalance): Operator attention
   - Alarm (5% imbalance): Investigation required
   - Emergency (>20% or NPW): Automatic shutdown

3. **Maintenance Mode**
   - Suspends leak detection for safe operations
   - Manual valve control preserved
   - Automatic return to normal monitoring

## Test Coverage

The test suite (`tests/test_pipeline.c`) includes 44 tests covering:
- Initialization and configuration
- Sensor registration and updates
- Segment and valve management
- Mass balance leak detection
- Pressure point analysis
- NPW rupture detection
- Alert creation and acknowledgment
- Emergency shutdown procedures
- Maintenance mode operations
- Telemetry generation
- Leak and drift injection simulation
- Edge cases (zero flow, negative values, quality filtering)

Run tests with: `make test-pipeline`

## Files

| File | Description |
|------|-------------|
| `src/pipeline/pipeline_spotlight.c` | Main implementation (~1300 lines) |
| `tests/test_pipeline.c` | Test suite (44 tests) |
| `docs/pipeline_systems.md` | This documentation |

## Integration Notes

1. **Scheduler Integration**
   - Call `pipe_process()` at regular intervals (100-1000ms typical)
   - Use scheduler periodic tasks for consistent timing

2. **Message Bus Integration**
   - Subscribe to pipeline topics for SCADA integration
   - Publish alerts to message bus for external systems

3. **Telemetry Export**
   - Generate telemetry packets for SCADA systems
   - 128-byte minimum buffer recommended
   - Binary format for efficient transmission
