# Lunar Mining: Regolith Extraction & Yield Telemetry

## Overview

The Lunar Mining spotlight demonstrates embedded firmware for In-Situ Resource Utilization (ISRU) operations on the Moon. This subsystem provides comprehensive drill control, TMR sensor voting, safety interlocks, and CCSDS-compatible telemetry—essential for sustainable lunar exploration.

## Industry Context

### Why Lunar Mining Matters

Lunar regolith contains valuable resources that enable sustainable space exploration:

| Resource | Source | Application |
|----------|--------|-------------|
| Water Ice | Permanently shadowed craters | Propellant (H₂/O₂), life support |
| Oxygen | Ilmenite reduction (FeTiO₃) | Breathing, propellant oxidizer |
| Iron | Native lunar regolith | Construction, manufacturing |
| Titanium | Ilmenite (FeTiO₃) | Structural components |
| Helium-3 | Solar wind implantation | Future fusion power |
| Aluminum | Anorthite (CaAl₂Si₂O₈) | Structures, conductors |

### Key Missions & Programs

- **NASA Artemis Program**: Return humans to lunar surface with ISRU infrastructure
- **CLPS (Commercial Lunar Payload Services)**: Private lunar landers carrying ISRU experiments
- **ESA PROSPECT**: Polar drill and mini-lab for volatile extraction
- **JAXA SLIM/LUPEX**: Japanese-Indian lunar polar exploration

### Embedded Systems Challenges

| Challenge | Impact | Grey Firmware Solution |
|-----------|--------|------------------------|
| 1/6g drilling dynamics | Different penetration forces | Adaptive feed rate control |
| Temperature extremes | -173°C to +127°C surface | Thermal management interlocks |
| Vacuum operation | No convective cooling | Motor thermal modeling |
| Communication delays | 1.3s Earth-Moon latency | Autonomous fault handling |
| Radiation environment | SEUs, latch-up | TMR sensor voting |
| Dust contamination | Regolith is abrasive | Wear tracking, maintenance |

## Architecture

### System Components

```
┌─────────────────────────────────────────────────────────────────┐
│                    Lunar Mining Controller                       │
├─────────────────────────────────────────────────────────────────┤
│                                                                   │
│  ┌───────────────┐  ┌───────────────┐  ┌───────────────┐        │
│  │   Drill 0     │  │   Drill 1     │  │   Drill N     │        │
│  │  ┌─────────┐  │  │  ┌─────────┐  │  │  ┌─────────┐  │        │
│  │  │ Torque  │  │  │  │ Torque  │  │  │  │ Torque  │  │        │
│  │  │ TMR x3  │  │  │  │ TMR x3  │  │  │  │ TMR x3  │  │        │
│  │  └─────────┘  │  │  └─────────┘  │  │  └─────────┘  │        │
│  │  ┌─────────┐  │  │  ┌─────────┐  │  │  ┌─────────┐  │        │
│  │  │ Depth   │  │  │  │ Depth   │  │  │  │ Depth   │  │        │
│  │  │ TMR x3  │  │  │  │ TMR x3  │  │  │  │ TMR x3  │  │        │
│  │  └─────────┘  │  │  └─────────┘  │  │  └─────────┘  │        │
│  │  ┌─────────┐  │  │  ┌─────────┐  │  │  ┌─────────┐  │        │
│  │  │  Temp   │  │  │  │  Temp   │  │  │  │  Temp   │  │        │
│  │  │ TMR x3  │  │  │  │ TMR x3  │  │  │  │ TMR x3  │  │        │
│  │  └─────────┘  │  │  └─────────┘  │  │  └─────────┘  │        │
│  └───────────────┘  └───────────────┘  └───────────────┘        │
│                              │                                    │
│  ┌───────────────────────────▼───────────────────────────┐      │
│  │              Safety Interlock Manager                  │      │
│  │  • Thermal limits  • Sensor health  • E-stop control  │      │
│  └───────────────────────────┬───────────────────────────┘      │
│                              │                                    │
│  ┌───────────────────────────▼───────────────────────────┐      │
│  │                 ISRU Processing Unit                   │      │
│  │  COLLECTING → HEATING → SEPARATING → STORING → IDLE   │      │
│  └───────────────────────────┬───────────────────────────┘      │
│                              │                                    │
│  ┌───────────────────────────▼───────────────────────────┐      │
│  │              Yield Telemetry Collector                 │      │
│  │  Water | Oxygen | Metals | Ore Grade | Efficiency     │      │
│  └───────────────────────────────────────────────────────┘      │
└─────────────────────────────────────────────────────────────────┘
```

### TMR (Triple Modular Redundancy) Voting

Each critical sensor has three independent channels for radiation tolerance:

```c
typedef enum {
    TMR_VOTE_UNANIMOUS,  /* All 3 agree within 5% */
    TMR_VOTE_MAJORITY,   /* 2 of 3 agree (1 failed) */
    TMR_VOTE_DISAGREE,   /* Channels diverge >5% */
    TMR_VOTE_ALL_FAILED  /* No valid channels */
} tmr_vote_result_t;
```

**Voting Algorithm:**
1. Collect readings from all 3 channels
2. Mark timed-out channels as failed
3. For 3 valid: use median (outlier rejection)
4. For 2 valid: use mean
5. For 1 valid: use value but flag degraded
6. For 0 valid: return ALL_FAILED, trigger fault

### Drill State Machine

```
        ┌─────────────────────────────────────┐
        │                                     │
        ▼                                     │
   ┌─────────┐     set_rpm > 0     ┌──────────────┐
   │  IDLE   │────────────────────►│   STARTING   │
   └─────────┘                     └──────────────┘
        ▲                                │
        │                                │ RPM reached
        │                                ▼
        │                          ┌──────────────┐
        │          depth limit     │   DRILLING   │◄──────┐
        │◄─────────────────────────┤              │       │
   ┌─────────────┐                 └──────────────┘       │
   │ RETRACTING  │                       │                │
   └─────────────┘                       │ fault          │
        ▲                                ▼                │
        │ auto_retract            ┌──────────────┐        │
        └─────────────────────────│    FAULT     │────────┘
                                  └──────────────┘  reset
                                         │
                                         │ e-stop
                                         ▼
                                  ┌──────────────┐
                                  │ EMERGENCY    │
                                  │    STOP      │
                                  └──────────────┘
```

### Fault Detection

| Fault | Detection | Response |
|-------|-----------|----------|
| Overtorque | Torque > max_torque_nm | Stop drill, FAULT state |
| Overtemp | Motor temp > 85°C | Stop drill, FAULT state |
| Sensor Fail | TMR_VOTE_ALL_FAILED | Stop drill, FAULT state |
| Depth Limit | Depth >= max_depth_m | Auto-retract |
| Stall | RPM = 0 while drilling | Stop drill, FAULT state |

## ISRU Processing Phases

The In-Situ Resource Utilization pipeline:

1. **COLLECTING**: Drills extract regolith into hopper
2. **HEATING**: Furnace ramps to 900°C for volatiles extraction
3. **SEPARATING**: Oxygen, water, metals separated
4. **STORING**: Products transferred to storage tanks
5. **IDLE**: Ready for next batch

## Message Bus Topics

Phase 25 adds 15 new topics for lunar mining:

```c
GF_TOPIC_REGOLITH_YIELD     "lunar/regolith/yield"
GF_TOPIC_REGOLITH_DENSITY   "lunar/regolith/density"
GF_TOPIC_REGOLITH_GRADE     "lunar/regolith/grade"
GF_TOPIC_DRILL_STATUS       "lunar/drill/status"
GF_TOPIC_DRILL_TORQUE       "lunar/drill/torque"
GF_TOPIC_DRILL_DEPTH        "lunar/drill/depth"
GF_TOPIC_DRILL_SPEED        "lunar/drill/speed"
GF_TOPIC_DRILL_FAULT        "lunar/drill/fault"
GF_TOPIC_DRILL_TEMP         "lunar/drill/temperature"
GF_TOPIC_AUGER_STATUS       "lunar/auger/status"
GF_TOPIC_ISRU_STATUS        "lunar/isru/status"
GF_TOPIC_ISRU_PRODUCTION    "lunar/isru/production"
GF_TOPIC_LUNAR_TELEMETRY    "lunar/telemetry"
GF_TOPIC_LUNAR_ALARM        "lunar/alarm"
GF_TOPIC_LUNAR_SAFETY       "lunar/safety"
```

## Telemetry Format

Telemetry follows CCSDS (Consultative Committee for Space Data Systems) packet structure:

```
┌──────────────────────────────────────────────────┐
│                 CCSDS Primary Header              │
├──────────────┬──────────────┬────────────────────┤
│  Packet ID   │ Sequence Ctrl │     Length        │
│   (16 bits)  │   (16 bits)   │    (16 bits)      │
├──────────────┴──────────────┴────────────────────┤
│                  Payload (variable)               │
│   drill_telemetry_t or yield_telemetry_t         │
└──────────────────────────────────────────────────┘
```

## Test Coverage

38 tests with 89 assertions covering:

| Category | Tests | Assertions |
|----------|-------|------------|
| TMR Voting | 5 | 10 |
| Drill Control | 13 | 30 |
| Regolith Analysis | 3 | 7 |
| Yield Telemetry | 2 | 8 |
| ISRU Processing | 4 | 8 |
| Safety Interlocks | 4 | 12 |
| Telemetry Formatting | 3 | 9 |
| System Integration | 4 | 15 |

Run tests:
```bash
make test-lunar
```

## API Reference

### Initialization

```c
int lunar_mining_init(void);
void lunar_mining_shutdown(void);
```

### Drill Control

```c
int lunar_drill_init(uint8_t id, const drill_config_t *cfg);
int lunar_drill_set_rpm(uint8_t id, uint16_t rpm);
void lunar_drill_emergency_stop(void);
int lunar_drill_reset(uint8_t id);
```

### Sample Analysis

```c
int lunar_analyze_sample(const regolith_sample_t *sample);
int lunar_get_yield(yield_telemetry_t *yield);
```

### Safety

```c
int lunar_safety_check(uint32_t current_ms);
int lunar_safety_reset(void);
```

### ISRU Processing

```c
int lunar_isru_start(uint32_t batch_id);
int lunar_isru_process(uint32_t delta_ms);
```

## Configuration Parameters

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| DRILL_MAX_RPM | 300 | 10-500 | Maximum drill speed |
| DRILL_MAX_TORQUE_NM | 150 | 50-300 | Torque limit |
| DRILL_MAX_DEPTH_M | 3.0 | 0.5-10 | Depth limit |
| MOTOR_TEMP_CRITICAL_C | 85 | 60-100 | Thermal cutoff |
| TMR_DISAGREEMENT_THRESHOLD | 5% | 1-20% | Voting tolerance |
| REGOLITH_DENSITY_MIN | 1.2 | - | Valid density range |
| REGOLITH_DENSITY_MAX | 2.0 | - | Valid density range |

## Related Headers

### Lunar Mining Domain
- [regolith_sensor.h](../include/lunar_mining/regolith_sensor.h) - Density, moisture, ice detection
- [drill_control.h](../include/lunar_mining/drill_control.h) - Drill loop interface
- [yield_telemetry.h](../include/lunar_mining/yield_telemetry.h) - ISRU yield tracking

### Other Phase 25 Domains
- Smart Fashion: [textile_sensor.h](../include/smart_fashion/textile_sensor.h)
- Emergency Medicine: [diagnostic_device.h](../include/emergency_medicine/diagnostic_device.h)
- Smart Logistics: [warehouse_robot.h](../include/smart_logistics/warehouse_robot.h)
- Cultural Events: [venue_sensor.h](../include/cultural_events/venue_sensor.h)

## References

1. NASA ISRU Technology Development: https://www.nasa.gov/isru
2. ESA PROSPECT Mission: https://www.esa.int/prospect
3. CCSDS Packet Telemetry Standard: CCSDS 102.0-B-5
4. Lunar Surface Innovation Initiative: https://www.nasa.gov/lsii
5. "Drilling and Excavation for Construction on the Moon and Mars" - Space Resources Roundtable
