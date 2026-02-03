# Zero-Gravity Fabrication Control Spotlight

## Overview

The Orbital Manufacturing Spotlight provides precision control for microgravity fabrication processes aboard orbital platforms. This subsystem manages chamber environments, material processing, and quality assurance for space-based manufacturing operations.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Orbital Fabrication Control                       │
├─────────────────────────────────────────────────────────────────────┤
│  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
│  │  Chamber 0  │  │  Chamber 1  │  │  Chamber 2  │  │  Chamber 3  │ │
│  │  Vapor Dep  │  │  Crystal    │  │  Alloy      │  │  Fiber      │ │
│  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘ │
│         │                │                │                │        │
│  ┌──────┴────────────────┴────────────────┴────────────────┴──────┐ │
│  │                    TMR Sensor Network                          │ │
│  │  Temperature (3x)  |  Pressure (3x)  |  Vibration (3x)        │ │
│  └──────────────────────────────────────────────────────────────┘  │
│         │                                                          │
│  ┌──────┴──────────────────────────────────────────────────────┐   │
│  │                  Safety Interlock System                     │   │
│  │  Thermal Runaway | Vacuum Loss | Vibration | Attitude Loss  │   │
│  └──────────────────────────────────────────────────────────────┘   │
│         │                                                          │
│  ┌──────┴──────────────────────────────────────────────────────┐   │
│  │                   CCSDS Telemetry                            │   │
│  │  Process Yield | Fault Status | Environment | Quality       │   │
│  └──────────────────────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────────────┘
```

## Supported Processes

| Process Type | Description | Typical Duration | Key Parameters |
|-------------|-------------|------------------|----------------|
| Crystal Growth | Semiconductor crystal growth in microgravity | Hours to days | Temperature (±0.5°C), g-level (<1e-5) |
| Vapor Deposition | Thin film deposition | Minutes to hours | Pressure (1e-6 torr), uniformity |
| Alloy Melting | High-purity alloy formation | Hours | Temperature (up to 2500°C) |
| Fiber Drawing | Optical fiber production | Continuous | Temperature stability, vibration |
| 3D Printing | Additive manufacturing | Hours | Layer accuracy, thermal control |
| Containerless | Levitation processing | Variable | Attitude control, contamination-free |

## State Machine

```
    ┌──────────────────────────────────────────────────────────────┐
    │                                                              │
    ▼                                                              │
┌──────┐    ┌────────────┐    ┌─────────┐    ┌───────────┐        │
│ IDLE │───▶│VACUUM_PUMP │───▶│ HEATING │───▶│DEPOSITION │        │
└──────┘    └────────────┘    └─────────┘    └───────────┘        │
    ▲                              │              │                │
    │                              │              ▼                │
    │                              │         ┌────────┐            │
    │                              └────────▶│ GROWTH │            │
    │                                        └────────┘            │
    │                                             │                │
    │       ┌───────────┐   ┌──────────────┐     ▼                │
    │◀──────│ UNLOADING │◀──│QUALITY_CHECK │◀───┬──────┐          │
    │       └───────────┘   └──────────────┘   │       │          │
    │                                          │  ┌────────┐      │
    │                                          └──│COOLING │      │
    │                                             └────────┘      │
    │                                                              │
    │       ┌─────────┐     Fault/Emergency                       │
    └───────┤  ABORT  │◀───────────────────────────────────────────┘
            └─────────┘
```

## Safety Interlocks

The system implements comprehensive safety monitoring:

### Thermal Protection
- **Overtemperature**: Absolute limit at 2500°C
- **Thermal Runaway**: Rate-of-change detection (>100°C/s)
- **Setpoint Deviation**: Alarm when >100°C above setpoint

### Vacuum Integrity
- **Target Vacuum**: 1×10⁻⁶ torr
- **Warning Threshold**: 1×10⁻⁴ torr
- **Pump Timeout**: 60 seconds

### Environmental Monitoring
- **Vibration Limit**: 10 mg during crystal growth
- **Attitude Stability**: <0.1° for precision processes
- **Microgravity Quality**: <1×10⁻⁵ g

## Triple Modular Redundancy (TMR)

All critical sensors use TMR voting:

```c
typedef struct {
    float values[3];           /* Readings from 3 channels */
    uint32_t timestamps[3];    /* Reading timestamps */
    tmr_status_t status[3];    /* Channel health status */
    tmr_vote_t vote_result;    /* Voting outcome */
} tmr_sensor_t;
```

TMR States:
- `TMR_ALL_AGREE`: All channels within tolerance
- `TMR_MAJORITY`: 2/3 agreement, failed channel identified
- `TMR_DISAGREE`: No consensus, operator intervention required
- `TMR_ALL_FAILED`: Catastrophic sensor failure

## PID Temperature Control

High-precision PID controllers maintain process temperatures:

```c
/* Default PID gains for temperature control */
kP = 0.5    /* Proportional gain */
kI = 0.1    /* Integral gain with anti-windup */
kD = 0.05   /* Derivative gain */
```

Features:
- Output clamping (0-100% power)
- Anti-windup protection
- Rate limiting for heater protection

## Telemetry Format

CCSDS-compatible telemetry packets:

| Field | Size | Description |
|-------|------|-------------|
| Version | 3 bits | CCSDS version (0) |
| Type | 1 bit | Telemetry (0) |
| Sec Header | 1 bit | Secondary header present |
| APID | 11 bits | Application Process ID |
| Sequence | 16 bits | Packet sequence number |
| Length | 16 bits | Packet data length |
| State | 8 bits | Current process state |
| Process | 8 bits | Process type |
| Fault | 8 bits | Active fault code |
| Temperature | 32 bits | Chamber temperature (float) |
| Pressure | 32 bits | Chamber pressure (float) |
| Thickness | 32 bits | Sample thickness (float) |
| Uniformity | 32 bits | Sample uniformity (float) |
| Checksum | 8 bits | XOR checksum |

## API Reference

### Initialization
```c
int orbital_fab_init(void);
void orbital_fab_shutdown(void);
```

### Configuration
```c
int orbital_fab_configure(const fab_chamber_config_t *config);
```

### Operations
```c
int orbital_fab_start(uint8_t chamber_id);
int orbital_fab_abort(uint8_t chamber_id);
int orbital_fab_reset(uint8_t chamber_id);
void orbital_fab_update(uint32_t elapsed_ms);
```

### Monitoring
```c
int orbital_fab_get_state(uint8_t chamber_id, fab_state_t *state);
int orbital_fab_get_fault(uint8_t chamber_id, fab_fault_t *fault);
int orbital_fab_get_environment(uint8_t chamber_id, fab_environment_t *env);
int orbital_fab_get_yield(uint8_t chamber_id, yield_stats_t *yield);
int orbital_fab_get_sample(uint8_t chamber_id, fab_sample_t *sample);
int orbital_fab_get_telemetry(uint8_t chamber_id, uint8_t *buffer, size_t max_len);
```

### Testing
```c
int orbital_fab_set_environment(uint8_t chamber_id, const char *param, float value);
int orbital_fab_inject_sensor_fault(uint8_t chamber_id, fab_sensor_type_t type, uint8_t channel);
```

## Fault Codes

| Code | Name | Description |
|------|------|-------------|
| 0 | NONE | No active fault |
| 1 | VACUUM_LOSS | Vacuum pressure above threshold |
| 2 | OVERTEMP | Temperature exceeds 2500°C |
| 3 | UNDERTEMP | Temperature below safe limit |
| 4 | THERMAL_RUNAWAY | Excessive heating rate |
| 5 | PRESSURE_SPIKE | Sudden pressure increase |
| 6 | SENSOR_FAIL | TMR sensor failure |
| 7 | VIBRATION | Excessive vibration detected |
| 8 | ATTITUDE_LOSS | Spacecraft attitude error |
| 9 | POWER_FAIL | Power supply fault |
| 10 | PRECURSOR_EMPTY | Material feedstock depleted |
| 11 | TMR_DISAGREE | Sensor voting disagreement |
| 12 | GROWTH_DEFECT | Crystal defect detected |
| 13 | CONTAMINATION | Process contamination |
| 14 | COMM_LOSS | Ground communication lost |

## Standards Compliance

- **ISS National Lab**: Protocol compatibility for ISS integration
- **NASA MSC**: Marshall Space Center guidelines for space manufacturing
- **ESA Columbus**: European Columbus laboratory specifications
- **CCSDS 133.0-B**: Space Packet Protocol for telemetry

## Test Coverage

The orbital manufacturing spotlight includes 38 unit tests covering:

- Module initialization and shutdown
- Chamber configuration and startup
- Process cycle completion (vapor deposition, crystal growth)
- Safety interlock detection (overtemp, vacuum loss, vibration)
- TMR sensor voting and fault injection
- Multi-chamber independence
- Telemetry generation
- Error handling and edge cases

Run tests with:
```bash
make test-orbital
```

## Integration Notes

### Message Bus Topics
```c
GF_TOPIC_FAB_YIELD      /* Production yield updates */
GF_TOPIC_PROCESS_FAULT  /* Process fault notifications */
```

### Scheduler Requirements
- Update rate: 10 Hz minimum (100ms cycles)
- Priority: High (real-time process control)

### Memory Usage
- Per-chamber state: ~512 bytes
- Telemetry buffer: 64 bytes per packet
- Total: ~2.5 KB for 4 chambers
