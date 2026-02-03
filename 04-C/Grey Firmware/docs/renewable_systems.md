# Renewable Energy Systems Module

## Overview

The Renewable Energy Systems module provides comprehensive wind turbine control and grid synchronization functionality for utility-scale wind farm applications. This spotlight demonstrates embedded firmware techniques used in the renewable energy industry.

## Industry Context

Wind energy is the fastest-growing renewable energy source globally. Firmware engineers in this domain work at companies like:
- **Vestas** - World's largest wind turbine manufacturer
- **GE Renewable Energy** - Major turbine supplier
- **Siemens Gamesa** - Leading offshore wind technology
- **Goldwind** - Largest Chinese turbine manufacturer
- **Envision Energy** - Smart wind solutions

Key challenges in wind turbine firmware:
- Real-time pitch/yaw control for power optimization
- Maximum Power Point Tracking (MPPT)
- Grid code compliance (frequency/voltage ride-through)
- Safety interlocks and emergency shutdown
- Harsh environment operation (-40°C to +50°C)

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Wind Farm SCADA System                       │
└─────────────────────────────────────────────────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   Turbine Control Unit                          │
├─────────────┬─────────────┬─────────────┬─────────────────────┤
│   Pitch     │    Yaw      │   Rotor     │    Generator        │
│  Control    │  Control    │   Speed     │    Control          │
│    PID      │    PID      │    MPPT     │    Power/PF         │
└─────────────┴─────────────┴─────────────┴─────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                   Grid Synchronization                          │
├─────────────┬─────────────┬─────────────┬─────────────────────┤
│  Phase-     │  Frequency  │   Voltage   │    Anti-            │
│  Locked     │   Match     │   Match     │   Islanding         │
│   Loop      │             │             │                     │
└─────────────┴─────────────┴─────────────┴─────────────────────┘
                              │
                              ▼
┌─────────────────────────────────────────────────────────────────┐
│                      Utility Grid                               │
└─────────────────────────────────────────────────────────────────┘
```

## Key Components

### 1. Wind Turbine Control (`wind_turbine.h`)

**Pitch Control:**
- Blade pitch angle adjustment (0-90°)
- Rate-limited actuation (10°/s max)
- Individual blade control capability
- Emergency feathering (90°)

**Yaw Control:**
- Nacelle rotation for wind alignment
- Wind direction tracking with deadband
- Cable unwinding logic
- Rate-limited rotation (0.5°/s)

**Operating States:**
```c
typedef enum {
    GF_TURBINE_STATE_STOPPED,
    GF_TURBINE_STATE_STARTING,
    GF_TURBINE_STATE_RUNNING,
    GF_TURBINE_STATE_POWER_PRODUCTION,
    GF_TURBINE_STATE_STOPPING,
    GF_TURBINE_STATE_EMERGENCY_STOP,
    GF_TURBINE_STATE_MAINTENANCE,
    GF_TURBINE_STATE_FAULT
} gf_turbine_state_t;
```

### 2. Grid Synchronization (`grid_sync.h`)

**Phase-Locked Loop (PLL):**
- Tracks grid phase angle
- PI-controlled frequency adjustment
- Phase lock detection (±5°)

**Synchronization Sequence:**
1. DISCONNECTED → MONITORING (measure grid)
2. MONITORING → PRE_SYNC (check conditions)
3. PRE_SYNC → SYNCHRONIZING (PLL active)
4. SYNCHRONIZING → CONNECTED (phase locked)
5. CONNECTED → EXPORTING (power enabled)

**Protection Features:**
- Under/over voltage protection
- Under/over frequency protection
- Anti-islanding detection
- Low voltage ride-through (LVRT)

### 3. Energy Telemetry (`energy_telemetry.h`)

**Data Points:**
- Active/reactive power
- Voltage, current, frequency
- Power factor
- Rotor speed, blade pitch
- Wind speed, direction
- Generator temperature

**Protocols Supported:**
- Modbus TCP
- DNP3
- IEC 61850
- OPC-UA

## Aerodynamics Model

The implementation includes a simplified Betz-limit power coefficient model:

```c
// Cp = f(λ, β) - Power coefficient as function of tip speed ratio and pitch
float cp = c1 * (c2/λi - c3*β - c4) * exp(-c5/λi) + c6*λ

// Mechanical power from wind
P = 0.5 * ρ * A * v³ * Cp
```

Where:
- λ = Tip speed ratio (ωR/v)
- β = Blade pitch angle
- ρ = Air density (1.225 kg/m³ at sea level)
- A = Rotor swept area
- v = Wind velocity

## Control Modes

### MPPT (Maximum Power Point Tracking)
- Optimal tip speed ratio: ~7 for 3-blade turbines
- Below rated wind: pitch = 0°, vary speed
- Above rated wind: pitch control, constant speed

### Power Limiting
- Curtailment for grid stability
- Pitch control to reduce Cp
- Ramp rate control (100 kW/s typical)

### Frequency Support
- Droop control for grid frequency
- Synthetic inertia response
- Fast frequency response

## Safety Systems

| Safety Feature | Trigger | Action |
|----------------|---------|--------|
| Overspeed | Rotor > 25 RPM | Emergency stop, feather blades |
| High Wind | v > 25 m/s | Normal shutdown |
| Low Wind | v < 3 m/s | Idle, wait for wind |
| Grid Fault | f, V out of range | Disconnect, LVRT |
| Generator Overtemp | T > 120°C | Derate or stop |
| Vibration | Accelerometer | Emergency stop |

## API Usage

### Basic Turbine Operation

```c
#include "renewable/wind_turbine.h"

// Configure turbine
gf_turbine_config_t config = {
    .rated_power_kw = 2000.0f,
    .cut_in_wind_mps = 3.0f,
    .cut_out_wind_mps = 25.0f,
    .rated_wind_mps = 12.0f,
    .rotor_diameter_m = 80.0f,
    .num_blades = 3,
    .gear_ratio = 100.0f,
    .generator_poles = 4,
    .mode = GF_TURBINE_CONTROL_MPPT
};

gf_turbine_init(&config);

// Register callbacks
gf_turbine_register_state_callback(on_state_change, NULL);
gf_turbine_register_alarm_callback(on_alarm, NULL);

// Start turbine
gf_turbine_start();

// Main loop
while (running) {
    // Update wind measurement from anemometer
    gf_turbine_wind_t wind = {
        .speed_mps = read_anemometer_speed(),
        .direction_deg = read_anemometer_direction(),
        .density_kgm3 = 1.225f
    };
    gf_turbine_update_wind(&wind);
    
    // Process control loops
    gf_turbine_process();
    
    // Get telemetry
    gf_turbine_telemetry_t telem;
    gf_turbine_get_telemetry(&telem);
    
    delay_ms(100);  // 10 Hz
}
```

### Grid Synchronization

```c
#include "renewable/grid_sync.h"

// Configure grid interface
gf_grid_config_t config = {
    .nominal_frequency_hz = 50.0f,
    .nominal_voltage_v = 690.0f,
    .freq_tolerance_hz = 0.5f,
    .voltage_tolerance_pct = 10.0f,
    .anti_islanding_enabled = true,
    .lvrt_enabled = true,
    .sync_timeout_ms = 10000
};

gf_grid_init(&config);

// Connect and synchronize
gf_grid_connect();
gf_grid_start_sync();

// Main loop
while (running) {
    // Update grid measurements from CTs/PTs
    gf_grid_measurement_t meas = {
        .voltage_v = {690.0f, 690.0f, 690.0f},
        .frequency_hz = 50.0f,
        .phase_angle_deg = read_grid_phase()
    };
    gf_grid_update_measurements(&meas);
    
    // Process PLL and state machine
    gf_grid_process();
    
    // Check if synchronized
    if (gf_grid_is_synchronized()) {
        gf_grid_enable_export(true);
    }
}
```

## Message Bus Topics

| Topic | Description |
|-------|-------------|
| `renewable/wind/output` | Power output data |
| `renewable/wind/status` | Turbine state |
| `renewable/wind/pitch` | Blade pitch position |
| `renewable/wind/yaw` | Nacelle yaw position |
| `renewable/wind/rotor` | Rotor speed/torque |
| `renewable/wind/alarm` | Alarm events |
| `renewable/grid/sync` | Sync status |
| `renewable/grid/fault` | Grid fault events |
| `renewable/grid/export` | Export power status |
| `renewable/turbine/telemetry` | Full telemetry package |

## Testing

Run renewable tests:
```bash
make test-renewable
```

Test coverage includes:
- Turbine initialization and configuration
- State machine transitions
- Pitch/yaw control limits
- Power control and MPPT
- Grid synchronization sequence
- Fault handling and callbacks
- 51 tests total

## Standards Compliance

The implementation follows:
- **IEC 61400-25**: Wind turbine communication
- **IEC 61400-27**: Electrical simulation models
- **IEEE 1547**: Grid interconnection
- **Grid codes**: ENTSO-E (EU), NERC (US)

## Performance Metrics

| Metric | Value |
|--------|-------|
| Control loop rate | 10-20 Hz |
| Pitch response | < 100 ms |
| Grid sync time | < 10 s |
| PLL bandwidth | 10 Hz |
| Phase accuracy | ±1° |
| Power ramp rate | 100 kW/s |

## File Structure

```
include/renewable/
├── wind_turbine.h       # Turbine control API
├── grid_sync.h          # Grid sync API
└── energy_telemetry.h   # Telemetry collection

src/renewable/
└── renewable_spotlight.c  # Full implementation

tests/
└── renewable_tests.c      # Integration tests
```

## Future Enhancements

- [ ] Blade ice detection
- [ ] Predictive maintenance (ML-based)
- [ ] Wake steering optimization
- [ ] Hydrogen electrolyzer integration
- [ ] Battery energy storage coupling
