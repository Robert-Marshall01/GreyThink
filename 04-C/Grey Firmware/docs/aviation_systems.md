# Aviation Systems Documentation

## Overview

Grey Firmware's aviation domain provides safety-critical embedded software modules for avionics systems, with a spotlight on the **Flight Data Recorder (FDR)** implementation. This documentation covers the FDR architecture, regulatory compliance considerations, and demonstrates embedded aviation expertise.

## Flight Data Recorder (FDR) Spotlight

### Architecture

The FDR Spotlight (`src/aviation/fdr_spotlight.c`) implements a production-grade flight data recording system with the following key components:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                        FDR Spotlight Architecture                       │
├─────────────────────────────────────────────────────────────────────────┤
│                                                                         │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐                 │
│  │ Sensor Ch 1 │    │ Sensor Ch 2 │    │ Sensor Ch 3 │                 │
│  │  (Primary)  │    │ (Secondary) │    │ (Tertiary)  │                 │
│  └──────┬──────┘    └──────┬──────┘    └──────┬──────┘                 │
│         │                  │                  │                         │
│         └─────────────┬────┴────────────┬─────┘                         │
│                       ▼                 │                               │
│              ┌────────────────┐         │                               │
│              │  Cross-Channel │         │                               │
│              │  Voting (MVS)  │         │                               │
│              └───────┬────────┘         │                               │
│                      ▼                  │                               │
│  ┌───────────────────────────────────────────────────────────┐         │
│  │                Parameter Encoding Engine                   │         │
│  │  • Fixed-point encoding with configurable resolution      │         │
│  │  • Range clamping per TSO-C124b                           │         │
│  │  • CRC-16-CCITT frame integrity                           │         │
│  └───────────────────────────┬───────────────────────────────┘         │
│                              ▼                                          │
│  ┌───────────────────────────────────────────────────────────┐         │
│  │                 Frame Recording Engine                     │         │
│  │  • 8 Hz sampling rate                                      │         │
│  │  • 4 subframes per second (256 words each)                │         │
│  │  • ARINC 717/767 compatible format                         │         │
│  └───────────────────────────┬───────────────────────────────┘         │
│                              ▼                                          │
│  ┌───────────────────────────┐  ┌────────────────────────────┐         │
│  │   Circular Recording      │  │   Exceedance Detection     │         │
│  │      Memory (25hr)        │  │   • Overspeed (>340 kts)   │         │
│  │   • Crash-survivable     │  │   • High G (>2.5g)         │         │
│  │   • Wraparound logic     │  │   • Bank angle (>45°)      │         │
│  └───────────────────────────┘  │   • Stall/GPWS/TCAS        │         │
│                                 └────────────────────────────┘         │
│                                             │                          │
│                                             ▼                          │
│  ┌───────────────────────────────────────────────────────────┐         │
│  │                   Telemetry Output                        │         │
│  │  • Real-time flight state reporting                       │         │
│  │  • Event logging with context                             │         │
│  │  • Fault status broadcasting                              │         │
│  └───────────────────────────────────────────────────────────┘         │
│                                                                         │
└─────────────────────────────────────────────────────────────────────────┘
```

### Key Components

#### 1. Parameter Recording (88+ Parameters)

The FDR records mandatory parameters per TSO-C124b and ED-112A:

| Category | Example Parameters | Sample Rate | Channels |
|----------|-------------------|-------------|----------|
| Time | UTC time | 1 Hz | 1 |
| Altitude | Pressure, baro, radio altitude | 1-4 Hz | 2-3 |
| Airspeed | IAS, TAS, Mach, ground speed | 1-4 Hz | 1-3 |
| Heading | Magnetic, true, track, drift | 1-4 Hz | 1-2 |
| Attitude | Pitch, roll, yaw | 4-8 Hz | 2-3 |
| Acceleration | Normal, lateral, longitudinal | 4-8 Hz | 2-3 |
| Engine | N1, N2, EGT, fuel flow | 1-4 Hz | 2 |
| Control Surfaces | Elevator, aileron, rudder | 4 Hz | 2 |
| Configuration | Flaps, slats, gear, speed brake | 1-2 Hz | 2-3 |
| Autopilot | AP/AT engaged, modes | 1 Hz | 1-2 |
| Warnings | Master, stall, GPWS, TCAS | 8 Hz | 1 |
| Navigation | Lat/lon, vertical speed | 1-4 Hz | 2 |
| Fuel | Total fuel quantity | 1 Hz | 1 |

#### 2. Sensor Voting Algorithm

The FDR implements **Mid-Value Select (MVS)** voting for triple-redundant sensors:

```c
// MVS Voting Algorithm Pseudocode
function vote_value(sensor_channels[3]):
    // Filter valid channels
    valid_values = filter(channels, quality == GOOD)
    
    if valid_values.count == 0:
        return NCD  // No Computed Data
    
    if valid_values.count == 1:
        return valid_values[0]  // Single source
    
    if valid_values.count == 2:
        // Dual-redundant: average if close
        if abs(values[0] - values[1]) < threshold:
            return (values[0] + values[1]) / 2
        else:
            flag_fault()
            return (values[0] + values[1]) / 2
    
    // Triple-redundant: select middle value
    sort(valid_values)
    middle = valid_values[1]
    
    // Check for outliers
    if deviation_exceeds_threshold(middle, values):
        flag_fault()
    
    return middle
```

**Benefits of MVS:**
- Tolerates single sensor failure silently
- Detects and flags disagreeing sensors
- Maintains recording even with degraded data

#### 3. Event Triggering

The FDR automatically records events for:

| Trigger Type | Detection Criteria | FDR Action |
|--------------|-------------------|------------|
| Takeoff | Weight-on-wheels → airborne | Log event, note V-speeds |
| Landing | Touchdown detection | Log event, note runway |
| Overspeed | IAS > Vmo/Mmo | Log exceedance, trigger state |
| High G | Normal accel > 2.5g or < 0g | Log exceedance |
| High Bank | Roll > 45° | Log exceedance |
| Stall Warning | Stick shaker active | Enter triggered state |
| GPWS | Any mode active | Enter triggered state |
| TCAS RA | Resolution advisory | Enter triggered state |
| Impact | Vertical accel > 3.5g | Enter triggered state |
| Manual | Crew trigger | Log marker |

#### 4. Memory Management

```
Recording Memory Structure (Simulated 64KB, represents 25-hour buffer):

┌──────────────────────────────────────────────────────────────┐
│                     Frame 0 (1 second)                       │
├──────────────────────────────────────────────────────────────┤
│ Sync  │ Frame# │ Timestamp │ Subframe │ CRC │ Data words... │
│ 0x247 │ 32-bit │ UTC secs  │   0-3    │     │ up to 256     │
├──────────────────────────────────────────────────────────────┤
│                     Frame 1 (1 second)                       │
├──────────────────────────────────────────────────────────────┤
│ ...                                                          │
├──────────────────────────────────────────────────────────────┤
│                     Frame N (wraparound)                     │
└──────────────────────────────────────────────────────────────┘

• Oldest data overwritten when full (circular buffer)
• CRC-16-CCITT for frame integrity validation
• Sync word 0x247 per ED-112A specification
```

### Regulatory Compliance

The FDR implementation demonstrates knowledge of key aviation regulations:

#### TSO-C124b (Flight Data Recorder Systems)

- **88+ mandatory parameters** recorded
- **Configurable sample rates** (1-8 Hz per parameter)
- **25-hour minimum duration** (simulated)
- **Crash-survivable memory** interface (abstracted)

#### EUROCAE ED-112A

- **Frame format** with sync word, timestamp, subframes
- **CRC integrity** checking
- **Parameter encoding** with defined resolution
- **Underwater locator beacon** (ULB) interface stub

#### DO-178C (Software Considerations)

- **Modular design** with clear interfaces
- **Defensive coding** (null checks, range validation)
- **Testability** (comprehensive test hooks)
- **Documentation** (extensive comments)

### API Reference

```c
// Initialization
int fdr_init(const fdr_config_t* config);  // NULL for defaults
void fdr_shutdown(void);

// Recording Control
int fdr_start_recording(void);
void fdr_stop_recording(void);

// Sensor Input (for HAL integration)
void fdr_inject_sensor(uint8_t param_id, uint8_t channel, 
                       float value, fdr_quality_t quality);

// Manual Event Trigger
void fdr_trigger_event(fdr_trigger_t trigger, const char* description);

// Status and Data Access
int fdr_get_status(fdr_status_t* status);
int fdr_get_voted_param(uint8_t param_id, fdr_voted_param_t* voted);
int fdr_get_telemetry(fdr_telemetry_t* telem);
int fdr_get_event(uint16_t index, fdr_event_record_t* event);

// Processing (call at 8Hz minimum)
void fdr_process(uint32_t delta_ms);
```

### Message Bus Topics

The FDR publishes to these aviation topics:

| Topic | Description |
|-------|-------------|
| `GF_TOPIC_FLIGHT_LOG` | Flight parameter log entries |
| `GF_TOPIC_FLIGHT_FRAME` | Complete recorded frames |
| `GF_TOPIC_FLIGHT_EVENT` | Event triggers (takeoff, landing, etc.) |
| `GF_TOPIC_AVIONICS_ALERT` | Safety alerts (GPWS, TCAS, stall) |
| `GF_TOPIC_SENSOR_FAULT` | Cross-channel sensor disagreements |
| `GF_TOPIC_SENSOR_VOTE` | Voted parameter values |
| `GF_TOPIC_EMERGENCY_EVENT` | High-priority emergency conditions |
| `GF_TOPIC_FLIGHT_TELEMETRY` | Real-time FDR telemetry |
| `GF_TOPIC_FDR_STATUS` | FDR system status |

### Test Coverage

The FDR test suite (`tests/fdr_tests.c`) includes 65+ tests covering:

1. **Initialization Tests** (6 tests)
   - Default and custom configuration
   - Double init handling
   - Start/stop recording

2. **Sensor Voting Tests** (7 tests)
   - Single, dual, triple redundancy
   - Agreement and disagreement detection
   - Failed channel handling

3. **Frame Recording Tests** (4 tests)
   - Single and multiple frame recording
   - Frame count persistence
   - Status reporting

4. **Event Triggering Tests** (5 tests)
   - Manual, takeoff, landing events
   - Multiple event storage
   - Flight parameter capture

5. **Exceedance Detection Tests** (6 tests)
   - Overspeed, high-G, bank angle
   - Stall warning, GPWS, TCAS RA

6. **Telemetry Tests** (4 tests)
   - Generation and content validation
   - Queue management

7. **Fault Handling Tests** (5 tests)
   - Sensor, memory, timing faults
   - Recovery mechanisms

8. **Emergency Scenario Tests** (4 tests)
   - Combined emergency scenarios
   - TCAS RA climb
   - Approach stall

9. **Boundary Condition Tests** (5 tests)
   - Max/min value encoding
   - Negative attitude handling

Run tests with:
```bash
make test-fdr
```

## Aviation Domain Stubs

In addition to the FDR spotlight, Grey Firmware provides stub interfaces for:

### Avionics Sensor Interface
`include/aviation/avionics_sensor.h`
- ARINC 429, MIL-STD-1553 data buses
- GPS, ADC, AHRS, EFIS, VOR, ILS, TCAS sensors
- Triple-modular redundancy (TMR)

### Safety Compliance Telemetry
`include/aviation/safety_compliance.h`
- DO-178C/DO-254 DAL levels (A-E)
- Certification artifact tracking
- Failure mode analysis

## Industry Applications

The aviation domain implementation demonstrates expertise relevant to:

| Industry | Application |
|----------|-------------|
| Commercial Aviation | Flight data monitoring, FOQA programs |
| General Aviation | Lightweight FDR for small aircraft |
| Defense | Mission data recording |
| UAV/drones | Black box for autonomous systems |
| Spaceflight | Telemetry recording |
| Ground Vehicles | Event data recorders |

## Safety Considerations

This implementation is intended for **demonstration and educational purposes**. Production aviation systems require:

- DO-178C certified software development
- Hardware integration with crash-survivable modules
- Extensive structural coverage testing (MC/DC)
- Independent verification and validation
- DAL B software development lifecycle

## Files

| File | Description |
|------|-------------|
| `src/aviation/fdr_spotlight.c` | FDR implementation (~1200 lines) |
| `tests/fdr_tests.c` | Integration tests (65+ tests) |
| `include/aviation/avionics_sensor.h` | Sensor interface stub |
| `include/aviation/flight_data_recorder.h` | FDR types and constants |
| `include/aviation/safety_compliance.h` | Compliance logging stub |

## Building and Testing

```bash
# Build FDR tests
make test-fdr

# Run all tests including FDR
make test
```

---

*This documentation is part of Grey Firmware Phase 15. The FDR Spotlight demonstrates production-quality embedded aviation software patterns while maintaining the educational focus of the project.*
