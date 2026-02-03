# Space Habitat Life Support Spotlight

## Overview

The Space Habitat Life Support spotlight demonstrates Grey Firmware's capability for aerospace and closed-loop life support systems. This implementation provides a production-grade Environmental Control and Life Support System (ECLSS) suitable for:

- NASA/ESA/JAXA space station life support (ISS ECLSS patterns)
- Commercial space stations (Axiom, Orbital Reef, Starlab)
- Lunar Gateway life support systems
- Mars habitat systems
- Submarine atmosphere management

## Architecture

The spotlight consists of three tightly integrated subsystems:

### 1. Life Support Sensor Subsystem (`life_support_sensor.h`)

Provides drivers for critical environmental sensors:

- **O2 Sensor**: Electrochemical/optical oxygen monitoring
- **CO2 Sensor**: NDIR carbon dioxide measurement
- **Humidity Sensor**: Relative humidity tracking
- **Temperature Sensor**: Cabin temperature
- **Pressure Sensor**: Total cabin pressure
- **Trace Contaminant Analyzer**: CO, NH3, H2S, VOC detection

**Key Features:**
- Quad-redundant sensor support (3-of-4 voting)
- Sensor drift detection and compensation
- Automatic calibration scheduling
- Health monitoring with degraded state tracking

### 2. Environmental Control System (`env_control.h`)

Closed-loop atmospheric control with PID regulation:

- **O2 Generation**: Electrolysis/MOXIE control
- **CO2 Scrubbing**: CDRA-style carbon dioxide removal
- **Humidity Control**: Humidifier/dehumidifier balance
- **Thermal Regulation**: Heater/cooler management

**Operating Modes:**
| Mode | Description |
|------|-------------|
| STANDBY | Monitoring only, no active control |
| NOMINAL | Normal closed-loop operation |
| ECONOMY | Power-saving with wider bands |
| SLEEP | Optimized for crew rest periods |
| EVA | Extra-vehicular activity support |
| EMERGENCY | Rapid atmosphere correction |

**Emergency Detection:**
- Auto-triggers at CO2 > 5000 ppm
- Auto-triggers at O2 < 18% or O2 > 25%
- Callbacks for ground notification

### 3. Habitat Telemetry (`habitat_telemetry.h`)

CCSDS-compatible telemetry for aerospace applications:

- **Packet Types**: Atmosphere, thermal, power, alarms, trends
- **Priority Levels**: Low, Normal, High, Critical
- **Blackout Handling**: Store-and-forward during comm gaps
- **Statistics**: Packet counts, bytes sent, blackout metrics

## Control Theory

### PID Implementation

Each environmental parameter uses a dedicated PID controller:

```c
typedef struct {
    float kp;              // Proportional gain
    float ki;              // Integral gain
    float kd;              // Derivative gain
    float integral_limit;  // Anti-windup limit
    float deadband;        // Control tolerance
} pid_controller_t;
```

**Tuning Recommendations:**
| Parameter | Kp | Ki | Kd | Deadband |
|-----------|----|----|----| ---------|
| O2 | 0.5 | 0.1 | 0.05 | 0.1% |
| CO2 | 0.3 | 0.05 | 0.02 | 10 ppm |
| Humidity | 0.2 | 0.05 | 0.01 | 1% |
| Temperature | 0.3 | 0.1 | 0.02 | 0.2°C |

### Sensor Voting Logic

For redundant sensors, median voting isolates faulty readings:

```c
float sensor_vote(const float* values, uint8_t count, 
                  float tolerance, bool* fault_detected);
```

- **Dual (2-of-2)**: Average with deviation check
- **Triple (2-of-3)**: Median selection
- **Quad (3-of-4)**: Median with outlier detection

## API Quick Reference

### Sensor Subsystem

```c
gf_ls_sensor_init(void);
gf_ls_sensor_configure(uint8_t channel, const gf_ls_sensor_config_t* config);
gf_ls_sensor_read(uint8_t channel, gf_ls_sensor_reading_t* reading);
gf_ls_sensor_get_atmosphere(gf_ls_atmosphere_t* atmosphere);
gf_ls_sensor_calibrate(uint8_t channel, float reference_value);
gf_ls_sensor_register_alarm(gf_ls_alarm_cb_t callback, void* user_data);
```

### Environmental Control

```c
gf_ec_init(const gf_ec_config_t* config);
gf_ec_set_mode(gf_ec_mode_t mode);
gf_ec_set_setpoints(const gf_ec_setpoints_t* setpoints);
gf_ec_actuator_override(gf_ec_actuator_t actuator, float output);
gf_ec_trigger_emergency(void);
gf_ec_process(void);  // Call periodically
```

### Telemetry

```c
gf_ht_init(const gf_ht_config_t* config);
gf_ht_send_atmosphere(const gf_ls_atmosphere_t* atmosphere, gf_ht_priority_t priority);
gf_ht_send_alarm(uint16_t alarm_id, uint8_t severity, ...);
gf_ht_set_blackout(bool blackout_active);
gf_ht_flush_stored(void);
gf_ht_process(void);  // Call periodically
```

## Integration Example

```c
#include "space_habitat/life_support_sensor.h"
#include "space_habitat/env_control.h"
#include "space_habitat/habitat_telemetry.h"

int main(void) {
    // Initialize subsystems
    gf_ls_sensor_init();
    
    gf_ec_config_t ec_config = {
        .crew_count = 4,
        .setpoints = {
            .o2_pct = 20.9f,
            .co2_max_ppm = 1000.0f,
            .humidity_pct = 45.0f,
            .temperature_c = 22.0f
        },
        // PID configs...
    };
    gf_ec_init(&ec_config);
    gf_ec_set_mode(GF_EC_MODE_NOMINAL);
    
    gf_ht_config_t ht_config = {
        .spacecraft_id = 0x4752,
        .realtime_interval_ms = 1000
    };
    gf_ht_init(&ht_config);
    
    // Main loop
    while (1) {
        gf_habitat_process();  // Runs sensor, EC, and telemetry
        delay_ms(100);
    }
}
```

## Test Coverage

The spotlight includes 112 integration tests covering:

| Category | Tests | Coverage |
|----------|-------|----------|
| Sensor Subsystem | 32 | Initialization, configuration, reading, alarms, calibration, drift detection |
| Environmental Control | 40 | Mode transitions, setpoints, actuator control, emergency handling |
| Telemetry | 24 | Packet generation, blackout handling, statistics |
| Integration | 16 | Full system simulation, crew effects, emergency recovery |

Run tests with:
```bash
make test-habitat
```

## Safety Considerations

1. **Redundancy**: Use at least triple redundancy for O2/CO2 sensors
2. **Watchdog**: Implement hardware watchdog for control loop
3. **Fallback**: Emergency mode should trigger external backup systems
4. **Logging**: All mode transitions and alarms should be persisted
5. **Ground Override**: Provide manual actuator override capability

## Performance

| Metric | Value |
|--------|-------|
| Control Loop | < 1ms per cycle |
| Memory Usage | ~8KB static |
| Stack Usage | ~2KB peak |
| Power Model | Accurate to ±5% |

## References

- NASA ECLSS Design Documentation
- ISS Environmental Control System Architecture
- CCSDS Space Packet Protocol (133.0-B-1)
- IEC 61508 for safety-critical systems
