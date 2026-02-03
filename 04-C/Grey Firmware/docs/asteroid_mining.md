# Asteroid Drill Control & Resource Telemetry

## Overview

Phase 27 spotlight subsystem implementing automated drill control for asteroid resource extraction. The system provides comprehensive drill state management, Triple Modular Redundancy (TMR) sensor voting for critical measurements, safety interlocks, and CCSDS-compatible telemetry generation.

## Architecture

### Subsystem Components

```
┌─────────────────────────────────────────────────────────────────┐
│                    Asteroid Mining System                        │
├─────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────────────────┐ │
│  │ Drill Unit 0│  │ Drill Unit 1│  │ ... up to 4 units      │ │
│  └──────┬──────┘  └──────┬──────┘  └───────────┬─────────────┘ │
│         │                │                      │               │
│         └────────────────┴──────────────────────┘               │
│                          │                                       │
│         ┌────────────────┴────────────────┐                     │
│         │        State Machine            │                     │
│         │  IDLE → PRE_CHECK → ANCHORING  │                     │
│         │    → SURFACE_SURVEY → DRILLING │                     │
│         │    → SAMPLE_COLLECT → RETRACT  │                     │
│         └────────────────┬────────────────┘                     │
│                          │                                       │
│  ┌───────────────────────┼───────────────────────────┐          │
│  │                       │                           │          │
│  ▼                       ▼                           ▼          │
│  ┌─────────┐      ┌─────────────┐           ┌──────────────┐   │
│  │ TMR     │      │ Safety      │           │ Telemetry    │   │
│  │ Sensors │      │ Interlocks  │           │ Generator    │   │
│  └─────────┘      └─────────────┘           └──────────────┘   │
└─────────────────────────────────────────────────────────────────┘
```

## Drill States

| State | Description |
|-------|-------------|
| `DRILL_STATE_IDLE` | No operation, awaiting start command |
| `DRILL_STATE_PRE_CHECK` | System health verification before drilling |
| `DRILL_STATE_ANCHORING` | Securing drill to asteroid surface |
| `DRILL_STATE_SURFACE_SURVEY` | Analyzing surface composition |
| `DRILL_STATE_DRILLING` | Active drilling operation |
| `DRILL_STATE_SAMPLE_COLLECT` | Extracting material sample |
| `DRILL_STATE_RETRACTING` | Withdrawing drill from surface |
| `DRILL_STATE_FAULT` | Error condition requiring intervention |
| `DRILL_STATE_MAINTENANCE` | Scheduled maintenance mode |

## Safety Interlocks

The system implements multiple safety interlocks:

### Fault Types

| Fault | Trigger Condition | Safety Action |
|-------|-------------------|---------------|
| `DRILL_FAULT_OVERHEAT` | Temperature > 150°C | Immediate retract |
| `DRILL_FAULT_OVERTORQUE` | Torque > max limit | Stop rotation |
| `DRILL_FAULT_STALL` | RPM = 0 with power applied | Reduce depth rate |
| `DRILL_FAULT_BIT_BREAK` | Resonance detected | Emergency stop |
| `DRILL_FAULT_POWER_LOSS` | Voltage < threshold | Safe shutdown |
| `DRILL_FAULT_VIBRATION` | Excessive vibration | Reduce speed |
| `DRILL_FAULT_ANCHOR_SLIP` | Anchor force loss | Abort operation |
| `DRILL_FAULT_SENSOR_FAIL` | TMR voting failure | Degraded mode |
| `DRILL_FAULT_DEPTH_LIMIT` | Max depth reached | Stop advance |

## TMR Sensor Voting

Critical measurements use Triple Modular Redundancy:

- **Temperature**: 3 independent thermocouples per drill
- **Torque**: 3 strain gauge channels
- **Vibration**: 3 accelerometer axes
- **Depth**: 3 linear encoders

### Voting Algorithm

```
1. Read all 3 sensor channels
2. Check for failed sensors (out-of-range values)
3. If 3 sensors healthy:
   - Use median value
4. If 2 sensors healthy:
   - Use average of 2 valid readings
5. If 1 sensor healthy:
   - Enter degraded mode, use single value with caution flag
6. If 0 sensors healthy:
   - Set DRILL_FAULT_SENSOR_FAIL
```

## Material Classification

The system identifies asteroid composition:

| Material | Characteristics | Resource Value |
|----------|-----------------|----------------|
| `MATERIAL_REGOLITH` | Loose surface layer | Low |
| `MATERIAL_SILICATE` | Rocky composition | Medium |
| `MATERIAL_CARBONACEOUS` | Carbon-rich (C-type) | High (water ice) |
| `MATERIAL_METALLIC` | Iron-nickel (M-type) | Very High |
| `MATERIAL_ICE` | Water ice deposits | Critical |

## Yield Tracking

The system maintains extraction statistics:

```c
typedef struct {
    uint32_t total_samples;      // Total extraction attempts
    uint32_t successful_samples; // Successful extractions
    float total_mass_kg;         // Total extracted mass
    float ice_mass_kg;           // Water ice extracted
    float metal_mass_kg;         // Metal ore extracted
    float silicate_mass_kg;      // Silicate material
    float avg_purity;            // Average sample purity
    float extraction_rate;       // kg/hour
    uint32_t drilling_time_s;    // Total drilling time
} yield_stats_t;
```

## Telemetry Format

CCSDS-compatible telemetry frames:

| Field | Offset | Size | Description |
|-------|--------|------|-------------|
| SYNC | 0 | 4 | Sync marker 0x1ACFFC1D |
| APID | 4 | 2 | Application ID |
| SEQ | 6 | 2 | Sequence counter |
| LENGTH | 8 | 2 | Payload length |
| UNIT_ID | 10 | 1 | Drill unit identifier |
| STATE | 11 | 1 | Current drill state |
| DEPTH | 12 | 4 | Current depth (mm) |
| TEMP | 16 | 4 | Temperature (°C × 100) |
| TORQUE | 20 | 4 | Torque (Nm × 100) |
| RPM | 24 | 2 | Current rotation speed |
| FAULT | 26 | 2 | Fault code |
| YIELD | 28 | 4 | Cumulative yield (g) |
| CRC | 32 | 2 | CRC-16 checksum |

## API Reference

### Initialization

```c
int asteroid_drill_init(void);
void asteroid_drill_shutdown(void);
```

### Configuration

```c
typedef struct {
    uint8_t unit_id;
    float max_torque_nm;
    float target_rpm;
    float max_depth_m;
    float overheat_temp_c;
    bool percussion_enabled;
} drill_config_t;

int asteroid_drill_configure(const drill_config_t *config);
```

### Operations

```c
int asteroid_drill_start(uint8_t unit_id);
int asteroid_drill_abort(uint8_t unit_id);
int asteroid_drill_reset(uint8_t unit_id);
void asteroid_drill_update(uint32_t elapsed_ms);
```

### Status Queries

```c
int asteroid_drill_get_state(uint8_t unit_id, drill_state_t *state);
int asteroid_drill_get_fault(uint8_t unit_id, drill_fault_t *fault);
int asteroid_drill_get_yield(uint8_t unit_id, yield_stats_t *stats);
int asteroid_drill_get_sample(uint8_t unit_id, sample_record_t *sample);
int asteroid_drill_get_telemetry(uint8_t unit_id, uint8_t *buffer, size_t max_len);
```

### Test Support

```c
int asteroid_drill_set_environment(uint8_t unit_id, const char *param, float value);
int asteroid_drill_inject_fault(uint8_t unit_id, sensor_type_t sensor, uint8_t channel);
```

## Message Bus Topics

| Topic | Description |
|-------|-------------|
| `asteroid/yield` | Extraction yield updates |
| `asteroid/drill/fault` | Drill fault notifications |
| `asteroid/drill/status` | Current drill state |
| `asteroid/drill/torque` | Real-time torque readings |
| `asteroid/drill/temperature` | Temperature updates |
| `asteroid/drill/depth` | Depth progress |
| `asteroid/sample` | Sample collection events |
| `asteroid/material` | Material classification |
| `asteroid/anchor` | Anchor status |
| `asteroid/tmr/status` | TMR sensor health |
| `asteroid/telemetry` | Full telemetry frames |
| `asteroid/alarm` | Safety alarms |
| `asteroid/safety` | Safety interlock events |

## Test Coverage

The test suite covers:

1. **Initialization** - Module startup/shutdown
2. **Configuration** - Drill parameter setup
3. **State Machine** - Full drilling cycle progression
4. **TMR Voting** - Sensor fault tolerance
5. **Safety Interlocks** - All fault conditions
6. **Yield Tracking** - Statistics accuracy
7. **Telemetry** - Frame generation
8. **Multi-Drill** - Independent unit operation
9. **Error Handling** - Invalid input handling

Run tests:
```bash
make test-asteroid
```

## References

- NASA Near-Earth Object Survey Program
- ESA Asteroid Prospection Explorer (APEX) Mission
- ISRU (In-Situ Resource Utilization) Standards
- CCSDS 133.0-B-2 (Space Packet Protocol)
- TMR Design Guidelines for Space Applications
