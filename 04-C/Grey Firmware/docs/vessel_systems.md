# Autonomous Vessel Control & Safety System

## Overview

The Autonomous Vessel Control & Safety spotlight (Phase 24) implements a production-grade maritime autonomous surface ship (MASS) control system. This module demonstrates autonomous navigation, cargo management, collision avoidance, and safety interlocks conforming to international maritime standards.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Vessel Control System                        │
├─────────────────────────────────────────────────────────────────┤
│                                                                 │
│  ┌──────────────┐  ┌──────────────┐  ┌──────────────┐          │
│  │  Navigation  │  │    Cargo     │  │   Safety     │          │
│  │   Control    │  │  Management  │  │  System      │          │
│  └──────┬───────┘  └──────┬───────┘  └──────┬───────┘          │
│         │                 │                 │                   │
│  ┌──────▼───────┐  ┌──────▼───────┐  ┌──────▼───────┐          │
│  │   ARPA/AIS   │  │   Stability  │  │  Collision   │          │
│  │   Tracking   │  │   Monitor    │  │  Avoidance   │          │
│  └──────────────┘  └──────────────┘  └──────────────┘          │
│                                                                 │
│  ┌────────────────────────────────────────────────────┐        │
│  │                 TMR Sensor Voting                   │        │
│  │       (Roll, Pitch, Navigation Sensors)             │        │
│  └────────────────────────────────────────────────────┘        │
│                                                                 │
│  ┌────────────────────────────────────────────────────┐        │
│  │              NMEA/AIS Telemetry Output              │        │
│  └────────────────────────────────────────────────────┘        │
│                                                                 │
└─────────────────────────────────────────────────────────────────┘
```

## Key Components

### Navigation Control

The navigation subsystem provides:

- **Navigation Modes**: Manual, Heading Hold, Track Control, Waypoint, Collision Avoidance, Emergency, Drift
- **Vessel States**: Moored, Anchored, Maneuvering, Underway, Restricted, Not Under Command, Emergency
- **Heading/Speed Control**: PID-based autopilot with weather compensation
- **Waypoint Navigation**: Up to 64 waypoints with turn radius and ETA calculation
- **Route Management**: Total distance tracking and remaining distance calculation

### ARPA Target Tracking

Automatic Radar Plotting Aid (ARPA) functionality:

- **Target Types**: Vessel, Fishing, Tug, Passenger, Cargo, Tanker, HSC, Buoy, Land
- **CPA/TCPA Calculation**: Closest Point of Approach and Time to CPA
- **Collision Risk Levels**: None, Low, Medium, High, Critical
- **Capacity**: Up to 128 tracked targets
- **Target Updates**: Position, course, speed with track history

### Cargo Management

Container and bulk cargo monitoring:

- **Cargo Types**: Container, Bulk, Tanker, Breakbulk, RoRo
- **Bay Management**: Up to 20 cargo bays with weight limits
- **Container Tracking**: Up to 1000 containers with status
- **Weight Distribution**: LCG, TCG, VCG monitoring
- **Overload Detection**: Automatic bay overload alerts

### Stability Monitoring

Ship stability calculations:

- **TMR Sensor Voting**: Triple Modular Redundancy for roll and pitch sensors
- **Metacentric Height (GM)**: Stability calculation with minimum threshold
- **Heel Angle Monitoring**: Static heel detection from cargo imbalance
- **Draft Monitoring**: Forward, aft, and midship draft tracking
- **Roll/Pitch Limits**: Warning and critical thresholds

### Safety Systems

Maritime safety interlocks:

- **Emergency Stop**: Immediate engine shutdown with alarm
- **Weather Limits**: Wind speed (50 kts) and wave height (8 m) limits
- **Collision Avoidance**: Automatic mode transition for close targets
- **Alarm Management**: Up to 32 active alarms with acknowledgment
- **Event Logging**: Up to 256 events with timestamps

## Message Bus Topics

The vessel system publishes on these topics:

| Topic | Description |
|-------|-------------|
| `GF_TOPIC_VESSEL_STATUS` | Overall vessel state |
| `GF_TOPIC_VESSEL_NAV` | Navigation data |
| `GF_TOPIC_VESSEL_HEADING` | Current heading |
| `GF_TOPIC_VESSEL_SPEED` | Speed over ground |
| `GF_TOPIC_VESSEL_POSITION` | GPS position |
| `GF_TOPIC_VESSEL_MODE` | Navigation mode |
| `GF_TOPIC_VESSEL_WAYPOINT` | Current waypoint |
| `GF_TOPIC_VESSEL_ROUTE` | Route information |
| `GF_TOPIC_CARGO_STATUS` | Cargo bay status |
| `GF_TOPIC_CARGO_ALERT` | Cargo alarms |
| `GF_TOPIC_CARGO_STABILITY` | Stability data |
| `GF_TOPIC_CARGO_WEIGHT` | Total cargo weight |
| `GF_TOPIC_COLLISION_RISK` | Collision risk level |
| `GF_TOPIC_ARPA_TARGET` | ARPA target updates |
| `GF_TOPIC_AIS_VESSEL` | AIS vessel data |
| `GF_TOPIC_NAV_SAFETY` | Navigation safety |
| `GF_TOPIC_VESSEL_STABILITY` | Stability status |
| `GF_TOPIC_VESSEL_WEATHER` | Weather conditions |
| `GF_TOPIC_VESSEL_ALARM` | Active alarms |
| `GF_TOPIC_VESSEL_TELEMETRY` | NMEA telemetry |
| `GF_TOPIC_VESSEL_EMERGENCY` | Emergency events |
| `GF_TOPIC_COLREGS_VIOLATION` | COLREGS violations |

## API Reference

### Initialization

```c
int vessel_init(void);
int vessel_config(const char *name, uint32_t mmsi, uint32_t imo,
                  float loa, float beam, float draft, float dwt);
int vessel_reset(void);
void vessel_shutdown(void);
```

### Navigation Control

```c
int vessel_set_nav_mode(nav_mode_t mode);
int vessel_add_waypoint(double lat, double lon, float speed, float turn_radius);
int vessel_update_nav(double lat, double lon, float heading, float course,
                      float speed, float rot, float depth);
```

### Target Tracking

```c
int vessel_update_target(uint16_t id, target_type_t type, double lat, double lon,
                         float course, float speed);
```

### Cargo Management

```c
int vessel_config_bay(uint8_t bay_id, float max_weight, float lcg, float tcg, float vcg);
int vessel_update_bay(uint8_t bay_id, float weight, uint16_t containers);
```

### Stability & Weather

```c
int vessel_update_stability(const float roll[3], const float pitch[3],
                            float gm, float displacement);
int vessel_update_weather(float wind_speed, float wind_dir, float wave_height,
                          float wave_period, sea_state_t sea_state);
```

### Safety & Telemetry

```c
int vessel_emergency_stop(void);
int vessel_get_telemetry(uint8_t *buffer, size_t max_len);
int vessel_ack_alarm(uint8_t alarm_idx);
int vessel_process(uint32_t time_ms);
```

### Getters

```c
nav_mode_t vessel_get_nav_mode(void);
vessel_state_t vessel_get_state(void);
float vessel_get_speed(void);
float vessel_get_heading(void);
float vessel_get_roll(void);
float vessel_get_gm(void);
uint8_t vessel_get_dangerous_targets(void);
bool vessel_is_collision_avoidance_active(void);
bool vessel_is_stable(void);
uint8_t vessel_get_alarm_count(void);
uint16_t vessel_get_event_count(void);
float vessel_get_total_distance(void);
bool vessel_has_sensor_fault(void);
tmr_result_t vessel_get_roll_tmr_status(void);
```

## Safety Thresholds

| Parameter | Warning | Critical |
|-----------|---------|----------|
| CPA (nm) | 2.0 | 0.5 |
| TCPA (min) | 30 | 10 |
| Roll (deg) | 15 | 25 |
| Pitch (deg) | 5 | - |
| Heel (deg) | - | 2 |
| GM (m) | - | 0.15 |
| Wind (kts) | - | 50 |
| Waves (m) | - | 8 |

## Standards Compliance

The implementation follows these maritime standards:

- **IMO SOLAS**: Safety of Life at Sea conventions
- **COLREGS**: International Regulations for Preventing Collisions at Sea
- **ISM Code**: International Safety Management
- **MARPOL**: Marine pollution prevention
- **IMO MSC.1/Circ.1604**: MASS regulatory scoping
- **ISO 19847**: Ship data management
- **NMEA 2000**: Marine electronics standard

## Test Coverage

The test suite ([test_vessel.c](../tests/test_vessel.c)) provides 54 tests with 110 assertions covering:

1. **Initialization Tests** (6 tests)
   - Init, double init, config, shutdown, reset

2. **Navigation Mode Tests** (3 tests)
   - Mode transitions, emergency mode

3. **Waypoint Tests** (4 tests)
   - Add, multiple, limits

4. **Navigation Update Tests** (3 tests)
   - Position, heading, speed tracking

5. **ARPA Target Tests** (4 tests)
   - Add, update, multiple, limits

6. **Cargo Bay Tests** (4 tests)
   - Config, update, overload detection

7. **Stability Tests** (7 tests)
   - TMR voting, GM checks, roll alarms

8. **Weather Tests** (3 tests)
   - Updates, wind/wave limits

9. **Emergency Tests** (3 tests)
   - Emergency stop, state changes

10. **Alarm Tests** (2 tests)
    - Acknowledge, invalid index

11. **Process Loop Tests** (3 tests)
    - Basic processing, uptime

12. **Telemetry Tests** (3 tests)
    - Generation, buffer limits, sequence

13. **Getter Tests** (2 tests)
    - Default values, data access

14. **Integration Tests** (4 tests)
    - Rough seas, cargo imbalance, collision, voyage simulation

15. **Edge Case Tests** (3 tests)
    - Heading wraparound, zero speed, max roll

## Usage Example

```c
#include "vessel/vessel_spotlight.c"

int main(void) {
    /* Initialize vessel system */
    vessel_init();
    vessel_config("MV Grey Spirit", 366123456, 9876543,
                  200.0f, 32.0f, 12.0f, 50000.0f);
    
    /* Configure cargo bays */
    vessel_config_bay(0, 2000000.0f, 50.0f, 0.0f, 5.0f);
    vessel_config_bay(1, 2000000.0f, 100.0f, 0.0f, 5.0f);
    vessel_update_bay(0, 1500000.0f, 100);
    vessel_update_bay(1, 1400000.0f, 95);
    
    /* Set route */
    vessel_add_waypoint(37.7749, -122.4194, 12.0f, 0.5f);
    vessel_add_waypoint(34.0522, -118.2437, 14.0f, 0.8f);
    vessel_set_nav_mode(NAV_MODE_WAYPOINT);
    
    /* Main loop */
    while (1) {
        /* Update sensors */
        float roll[] = {2.0f, 2.1f, 1.9f};
        float pitch[] = {0.5f, 0.5f, 0.5f};
        vessel_update_stability(roll, pitch, 0.5f, 50000.0f);
        
        /* Update navigation */
        vessel_update_nav(37.5, -122.5, 90.0f, 88.0f, 12.0f, 0.0f, 50.0f);
        
        /* Update weather */
        vessel_update_weather(15.0f, 270.0f, 1.5f, 7.0f, SEA_STATE_2_SMOOTH);
        
        /* Process */
        vessel_process(get_time_ms());
        
        /* Check for alarms */
        if (vessel_get_alarm_count() > 0) {
            handle_alarms();
        }
        
        /* Generate telemetry */
        uint8_t buffer[256];
        int len = vessel_get_telemetry(buffer, sizeof(buffer));
        if (len > 0) {
            send_telemetry(buffer, len);
        }
        
        delay_ms(100);
    }
    
    vessel_shutdown();
    return 0;
}
```

## Related Documentation

- [Overview](overview.md) - Complete Grey Firmware overview
- [Message Bus](message_bus.md) - Inter-module communication
- [Marine Systems](marine.md) - Marine sonar and depth monitoring
