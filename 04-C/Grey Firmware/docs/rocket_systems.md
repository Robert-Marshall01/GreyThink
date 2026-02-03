# Rocket Engine Telemetry & Safety System

## Overview

The Rocket Engine Telemetry & Safety spotlight implements a production-grade control system for rocket engine monitoring, safety interlock logic, and launch telemetry - demonstrating expertise in aerospace embedded systems critical for space launch vehicles.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                    ROCKET LAUNCH CONTROL SYSTEM                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────────────────────────────────────────────────────────────┐   │
│  │                       MULTI-ENGINE ARRAY                            │   │
│  │  ┌─────────┐ ┌─────────┐ ┌─────────┐ ┌─────────┐      ┌─────────┐  │   │
│  │  │Engine 0 │ │Engine 1 │ │Engine 2 │ │Engine 3 │ ...  │Engine N │  │   │
│  │  │ Merlin  │ │ Merlin  │ │ Merlin  │ │ Merlin  │      │ Merlin  │  │   │
│  │  └────┬────┘ └────┬────┘ └────┬────┘ └────┬────┘      └────┬────┘  │   │
│  │       │           │           │           │                │       │   │
│  └───────┼───────────┼───────────┼───────────┼────────────────┼───────┘   │
│          │           │           │           │                │           │
│  ┌───────▼───────────▼───────────▼───────────▼────────────────▼───────┐   │
│  │                    SENSOR DATA COLLECTION                          │   │
│  │  • Thrust transducers (1kHz)      • Chamber pressure (500Hz)      │   │
│  │  • Chamber temperature (100Hz)     • Vibration (5kHz)             │   │
│  │  • Turbopump temp (100Hz)          • Propellant flow (200Hz)      │   │
│  └─────────────────────────────────┬─────────────────────────────────┘   │
│                                    │                                      │
│  ┌─────────────────────────────────▼─────────────────────────────────┐   │
│  │              TRIPLE MODULAR REDUNDANCY (TMR) VOTING               │   │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐                │   │
│  │  │ Channel A   │  │ Channel B   │  │ Channel C   │                │   │
│  │  │ Sensors     │  │ Sensors     │  │ Sensors     │                │   │
│  │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘                │   │
│  │         └────────────────┼────────────────┘                        │   │
│  │                    ┌─────▼─────┐                                   │   │
│  │                    │  VOTER    │◄─ 2-of-3 Agreement               │   │
│  │                    │  Median   │   Outlier Rejection              │   │
│  │                    └─────┬─────┘                                   │   │
│  └──────────────────────────┼────────────────────────────────────────┘   │
│                             │                                             │
│  ┌──────────────────────────▼────────────────────────────────────────┐   │
│  │                   SAFETY INTERLOCK SYSTEM                          │   │
│  │  ┌─────────────────────────────────────────────────────────────┐  │   │
│  │  │                    ABORT CONDITIONS                          │  │   │
│  │  │                                                              │  │   │
│  │  │  Thrust       │ <85% or >105% nominal ──────────► ABORT     │  │   │
│  │  │  Asymmetry    │ >10% inter-engine deviation ────► ABORT     │  │   │
│  │  │  Temperature  │ >3600°C chamber ────────────────► ABORT     │  │   │
│  │  │  Pressure     │ >4000 PSI chamber ──────────────► ABORT     │  │   │
│  │  │  Vibration    │ >20g structural ────────────────► ABORT     │  │   │
│  │  │  Sensors      │ All TMR channels failed ────────► ABORT     │  │   │
│  │  │                                                              │  │   │
│  │  └─────────────────────────────────────────────────────────────┘  │   │
│  │                                                                    │   │
│  │  ⚡ ABORT RESPONSE: <10ms latency (deterministic)                 │   │
│  └────────────────────────────────────────────────────────────────────┘   │
│                                                                             │
├─────────────────────────────────────────────────────────────────────────────┤
│                        TELEMETRY & DOWNLINK                                 │
│  ┌─────────────────────────────────────────────────────────────────────────┐│
│  │ • CCSDS-compatible telemetry frame generation                          ││
│  │ • Real-time engine health monitoring                                    ││
│  │ • Launch readiness status                                               ││
│  │ • Event logging for flight reconstruction                               ││
│  │ • Alarm management with severity levels                                 ││
│  └─────────────────────────────────────────────────────────────────────────┘│
└─────────────────────────────────────────────────────────────────────────────┘
```

## Key Features

### 1. Multi-Engine Support
- Up to 9 engines per vehicle (e.g., Falcon 9/Heavy configuration)
- Individual engine state machines (OFF → PRESTART → IGNITION → MAINSTAGE → SHUTDOWN)
- Per-engine health monitoring and diagnostics

### 2. Triple Modular Redundancy (TMR)
- Three redundant sensor channels per measurement
- 2-of-3 voting with outlier rejection
- Automatic failover on channel loss
- Agreement tolerance: 2% between channels

### 3. Safety Interlock Logic
Production-grade abort conditions:

| Condition | Threshold | Response |
|-----------|-----------|----------|
| Low Thrust | <85% nominal | Immediate abort |
| High Thrust | >105% nominal | Immediate abort |
| Thrust Asymmetry | >10% deviation | Immediate abort |
| Chamber Overtemp | >3600°C | Immediate abort |
| Chamber Overpressure | >4000 PSI | Immediate abort |
| Excessive Vibration | >20g sustained | Immediate abort |
| Sensor Failure | All TMR channels failed | Immediate abort |

### 4. Launch Sequence Phases
```
PRELAUNCH → TERMINAL_COUNT → IGNITION_SEQUENCE → LIFTOFF →
PITCH_PROGRAM → MAX_Q → THROTTLE_BUCKET → MAX_THRUST →
MECO → STAGE_SEPARATION → MISSION_COMPLETE
                    ↓ (any anomaly)
                  ABORT
```

### 5. Telemetry Generation
- CCSDS Blue Book compatible frame format
- Configurable virtual channels
- Mission time stamping (nanosecond resolution)
- Buffer management for downlink scheduling

## API Reference

### Initialization

```c
// Initialize rocket system
int rocket_init(void);

// Configure launch vehicle
int rocket_configure_vehicle(uint16_t vehicle_id, const char* name,
                             uint8_t engine_count, bool human_rated);

// Configure individual engine
int rocket_configure_engine(uint8_t engine_id, const char* name,
                            float nominal_thrust_kn, float max_thrust_kn);
```

### Safety System

```c
// Arm safety interlock system
int rocket_safety_arm(void);

// Get safety status
int rocket_get_safety_status(safety_status_t* status);

// Trigger manual abort
int rocket_abort(abort_type_t reason);
```

### Launch Operations

```c
// Set launch phase
int rocket_set_phase(launch_phase_t phase);

// Start ignition sequence
int rocket_start_ignition(void);

// Check launch readiness
int rocket_check_readiness(bool* ready, char* reason, size_t reason_len);
```

### Telemetry

```c
// Generate telemetry frame
int rocket_generate_telemetry(void* buffer, size_t buffer_len, size_t* frame_len);

// Get telemetry statistics
int rocket_get_tlm_stats(tlm_stats_t* stats);
```

### Testing & Simulation

```c
// Inject sensor data for testing
int rocket_inject_sensor(uint8_t engine_id, sensor_type_t type,
                         uint8_t channel, float value);

// Inject complete engine state
int rocket_inject_engine_state(uint8_t engine_id, engine_state_t state,
                               float thrust_pct, float chamber_temp_c,
                               float vibration_g);
```

## Integration Example

```c
#include "rocket_spotlight.h"

int main(void) {
    // Initialize
    rocket_init();
    
    // Configure Falcon 9-like vehicle
    rocket_configure_vehicle(1001, "Falcon 9", 9, false);
    
    // Configure 9x Merlin 1D engines
    for (int i = 0; i < 9; i++) {
        rocket_configure_engine(i, "Merlin 1D", 845.0f, 914.0f);
    }
    
    // Arm safety system
    rocket_safety_arm();
    
    // Enter terminal count
    rocket_set_phase(PHASE_TERMINAL_COUNT);
    
    // Process loop (typically 5ms interval)
    while (1) {
        rocket_process(5);
        
        // Check for abort
        safety_status_t status;
        rocket_get_safety_status(&status);
        
        if (status.abort_triggered) {
            printf("ABORT: reason=%d, latency=%u us\n",
                   status.abort_reason, status.abort_latency_us);
            break;
        }
        
        // Generate and transmit telemetry
        uint8_t frame[1024];
        size_t len;
        rocket_generate_telemetry(frame, sizeof(frame), &len);
        transmit_telemetry(frame, len);
    }
    
    rocket_shutdown();
    return 0;
}
```

## Aerospace Standards Alignment

### NASA-STD-8719 (Software Safety)
- Hazard analysis-driven safety requirements
- Fault tree analysis for abort conditions
- Single-fault tolerant design via TMR

### DO-178C (Airborne Systems)
- Design assurance level mapping
- Verification requirements for safety-critical code
- Deterministic timing guarantees

### MIL-STD-1553 / SpaceWire
- Data bus compatible framing
- Protocol-agnostic telemetry encoding
- Multi-drop sensor topology support

### CCSDS Blue Book
- Telemetry channel coding standards
- Space Packet Protocol compatibility
- Virtual channel multiplexing

## Test Coverage

The rocket subsystem includes 35 comprehensive tests:

### Initialization Tests (5)
- Basic init/shutdown
- Double init prevention
- Vehicle configuration
- Engine configuration limits

### Safety System Tests (2)
- Safety arming
- Periodic safety checks

### Abort Trigger Tests (8)
- Low thrust abort
- High thrust abort
- Over-temperature abort
- Excessive vibration abort
- Manual abort
- Thrust asymmetry abort
- Nominal operation (no false abort)
- Abort response time verification

### Engine Status Tests (3)
- Status query
- Invalid engine handling
- State after abort

### TMR Voting Tests (2)
- Three-channel median voting
- Outlier rejection

### Telemetry Tests (3)
- Frame generation
- Buffer size validation
- Statistics tracking

### Alarm Tests (2)
- Alarm generation on fault
- Warning vs abort severity

### Launch Readiness Tests (2)
- Safety not armed check
- GO for launch verification

### Phase Transition Tests (2)
- Phase state machine
- Ignition sequence gating

### Edge Cases (3)
- Null pointer handling
- Pre-init failures
- Run time accumulation

## Running Tests

```bash
# Run rocket engine tests only
make test-rocket

# Run all tests including rocket
make test
```

## Message Bus Integration

The rocket subsystem publishes to these topics:

| Topic | Description |
|-------|-------------|
| `GF_TOPIC_ENGINE_STATUS` | Per-engine status updates |
| `GF_TOPIC_ENGINE_THRUST` | Thrust readings |
| `GF_TOPIC_ENGINE_TEMP` | Temperature readings |
| `GF_TOPIC_ENGINE_VIBRATION` | Vibration data |
| `GF_TOPIC_LAUNCH_PHASE` | Launch phase transitions |
| `GF_TOPIC_LAUNCH_ABORT` | Abort notifications |
| `GF_TOPIC_LAUNCH_COUNTDOWN` | T-minus countdown |
| `GF_TOPIC_SAFETY_INTERLOCK` | Interlock status |
| `GF_TOPIC_SAFETY_ALARM` | Alarm notifications |
| `GF_TOPIC_ROCKET_TELEMETRY` | Telemetry frames |
| `GF_TOPIC_TMR_STATUS` | TMR voting status |
| `GF_TOPIC_FTS_STATUS` | Flight Termination System |

## Industry Relevance

This spotlight demonstrates expertise valuable to:

- **Commercial Launch Providers**: SpaceX, Blue Origin, Rocket Lab, Relativity Space
- **Traditional Aerospace**: Lockheed Martin, Northrop Grumman, Boeing
- **Space Agencies**: NASA, ESA, JAXA
- **Propulsion Systems**: Aerojet Rocketdyne, Ursa Major
- **Defense Contractors**: Raytheon, L3Harris

Key skills demonstrated:
- Real-time safety-critical embedded systems
- Redundancy and fault tolerance design
- Aerospace standards compliance
- High-frequency sensor data processing
- Deterministic response time systems
