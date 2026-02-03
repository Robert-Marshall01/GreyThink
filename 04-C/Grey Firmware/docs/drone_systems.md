# Drone Flight Control Systems

## Overview

Grey Firmware Phase 9 introduces a comprehensive **Drone Flight Control Spotlight**, demonstrating production-grade UAV flight control firmware. This spotlight showcases skills directly applicable to major drone manufacturers, open-source flight stacks, and emerging eVTOL platforms.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                      Flight Control System                          │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│    ┌──────────┐    ┌──────────┐    ┌──────────┐    ┌──────────┐   │
│    │ Position │───►│ Velocity │───►│ Attitude │───►│   Rate   │   │
│    │   Loop   │    │   Loop   │    │   Loop   │    │   Loop   │   │
│    │  (50Hz)  │    │  (50Hz)  │    │ (400Hz)  │    │ (400Hz)  │   │
│    └──────────┘    └──────────┘    └──────────┘    └──────────┘   │
│         ▲               ▲               ▲               │          │
│         │               │               │               ▼          │
│    ┌────────────────────────────────┐   │        ┌──────────┐     │
│    │       Sensor Fusion (EKF)      │───┘        │  Motor   │     │
│    │     GPS + IMU + Baro + Mag     │            │  Mixer   │     │
│    └────────────────────────────────┘            └──────────┘     │
│                   ▲                                    │          │
│                   │                                    ▼          │
│    ┌──────────────────────────┐           ┌────────────────────┐  │
│    │     Safety Interlocks    │           │    Motor Outputs   │  │
│    │ • Geofence   • Altitude  │           │   (PWM 1000-2000)  │  │
│    │ • Battery    • RC Loss   │           └────────────────────┘  │
│    └──────────────────────────┘                                   │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Core Components

### 1. Cascaded PID Control Loops

The flight control system uses a cascaded PID architecture with four nested loops:

| Loop | Rate | Input | Output |
|------|------|-------|--------|
| **Position** | 50 Hz | Position error (NED) | Velocity setpoint |
| **Velocity** | 50 Hz | Velocity error | Attitude setpoint |
| **Attitude** | 400 Hz | Attitude error | Rate setpoint |
| **Rate** | 400 Hz | Angular rate error | Motor commands |

**PID Controller Features:**
- Proportional, Integral, Derivative terms
- Anti-windup on integral accumulator
- Low-pass filtered derivative
- Output clamping
- Reset functionality for mode transitions

### 2. Sensor Fusion

The system fuses multiple sensors using a complementary filter (simplified EKF):

| Sensor | Update Rate | Contribution |
|--------|-------------|--------------|
| **IMU Accelerometer** | 400 Hz | Low-frequency attitude reference |
| **IMU Gyroscope** | 400 Hz | High-frequency attitude estimation |
| **GPS** | 10 Hz | Position and velocity |
| **Barometer** | 50 Hz | Altitude (future) |
| **Magnetometer** | 50 Hz | Heading reference (future) |

**Fusion Algorithm:**
```c
// Complementary filter: 98% gyro, 2% accelerometer
attitude = α × (attitude + gyro × dt) + (1-α) × accel_attitude
```

### 3. Flight Modes

| Mode | Description | Requirements |
|------|-------------|--------------|
| `DISARMED` | Motors off, safe state | - |
| `STABILIZE` | Self-leveling, manual throttle | Armed |
| `ALT_HOLD` | Automatic altitude maintenance | Armed + Baro |
| `POS_HOLD` | GPS position hold | Armed + GPS |
| `RTL` | Return to launch position | Armed + GPS + Home |
| `LAND` | Automatic landing | Armed |
| `EMERGENCY` | Immediate motor stop | - |

### 4. Safety Interlocks

The safety system continuously monitors for hazardous conditions:

| Condition | Flag | Action |
|-----------|------|--------|
| Geofence breach | `SAFETY_GEOFENCE` | Switch to RTL |
| Max altitude exceeded | `SAFETY_ALTITUDE_HIGH` | Descend |
| Min altitude breach | `SAFETY_ALTITUDE_LOW` | Alert |
| Battery < 20% | `SAFETY_BATTERY_LOW` | Alert |
| Battery < 10% | `SAFETY_BATTERY_CRITICAL` | Force landing |
| RC signal lost | `SAFETY_RC_LOST` | RTL |
| GPS signal lost | `SAFETY_GPS_LOST` | Altitude hold |
| Sensor fault | `SAFETY_SENSOR_FAULT` | Emergency |

**Geofence Configuration:**
- Cylindrical fence (radius + altitude limits)
- Home position as center
- Configurable breach action (RTL, Land, Hover)

### 5. Motor Mixing

The motor mixer translates control commands to individual motor outputs:

**Quadcopter X Configuration:**
```
        Front
      1       2
        \ ^ /
         \|/
          X
         /|\
        / | \
      4       3
        Rear

Motor 1: Front-Left  (CW)
Motor 2: Front-Right (CCW)
Motor 3: Rear-Right  (CW)
Motor 4: Rear-Left   (CCW)
```

**Mixing Matrix:**
```c
motor[i] = throttle × M[i][0] + roll × M[i][1] + pitch × M[i][2] + yaw × M[i][3]
```

| Motor | Throttle | Roll | Pitch | Yaw |
|-------|----------|------|-------|-----|
| 1 (FL) | +1 | -1 | +1 | -1 |
| 2 (FR) | +1 | +1 | +1 | +1 |
| 3 (RR) | +1 | +1 | -1 | -1 |
| 4 (RL) | +1 | -1 | -1 | +1 |

## API Reference

### Initialization

```c
// Initialize flight controller
int drone_fc_init(void);

// Shutdown flight controller
void drone_fc_deinit(void);
```

### Arming/Disarming

```c
// Arm motors (performs pre-arm checks)
int drone_fc_arm(void);

// Disarm motors (force=true bypasses altitude check)
int drone_fc_disarm(bool force);
```

### Mode Control

```c
// Set flight mode
int drone_fc_set_mode(flight_mode_t new_mode);

// Mode enum values:
// MODE_DISARMED, MODE_STABILIZE, MODE_ALT_HOLD
// MODE_POS_HOLD, MODE_RTL, MODE_AUTO, MODE_LAND, MODE_EMERGENCY
```

### Control Loop

```c
// Run control loop (call at 400 Hz)
void drone_fc_update(float dt);

// Update IMU sensor data
void drone_fc_update_imu(const imu_data_t* imu);

// Update GPS position
void drone_fc_update_gps(const gps_position_t* gps);
```

### Setpoints

```c
// Set attitude and throttle (stabilize mode)
void drone_fc_set_attitude(float roll, float pitch, float yaw, float throttle);

// Set position target (position hold mode)
void drone_fc_set_position(float north, float east, float down, float yaw);
```

### Safety Configuration

```c
// Configure geofence
void drone_fc_set_geofence(bool enabled, float radius_m, float max_alt_m, float min_alt_m);

// Set home position
void drone_fc_set_home(double lat, double lon, float alt);

// Update battery status
void drone_fc_set_battery(float voltage, float percent);

// Emergency stop (immediate motor shutdown)
void drone_fc_emergency_stop(void);
```

### Status Queries

```c
// Get current state
void drone_fc_get_state(flight_mode_t* mode, arm_state_t* arm, 
                         uint16_t* safety, euler_t* attitude,
                         vec3_t* position, float* battery_pct);

// Get motor PWM outputs
void drone_fc_get_motors(uint16_t pwm[4]);

// Run pre-flight checks
int drone_fc_preflight_check(void);
```

### Callbacks

```c
// Safety status change callback
typedef void (*safety_callback_t)(uint16_t flags, void* user_data);
void drone_fc_set_safety_callback(safety_callback_t cb, void* user_data);

// Mode change callback
typedef void (*mode_callback_t)(flight_mode_t old_mode, flight_mode_t new_mode, void* user_data);
void drone_fc_set_mode_callback(mode_callback_t cb, void* user_data);
```

## Message Bus Integration

The drone system publishes and subscribes to standardized message bus topics:

### Topics

| Topic | Direction | Description |
|-------|-----------|-------------|
| `drone/cmd` | Subscribe | Flight commands (arm, mode, setpoints) |
| `drone/state` | Publish | Current flight state |
| `drone/telemetry` | Publish | Position, velocity, acceleration |
| `drone/gps` | Publish | GPS fix information |
| `drone/motor` | Publish | Motor outputs and status |
| `drone/fault` | Publish | Safety violations |
| `drone/geofence` | Publish | Geofence breach events |
| `drone/waypoint` | Subscribe | Waypoint commands |

### Payload Types

- `GF_PAYLOAD_DRONE_CMD` - Flight command payload
- `GF_PAYLOAD_DRONE_STATE` - State payload
- `GF_PAYLOAD_DRONE_TELEM` - Telemetry payload
- `GF_PAYLOAD_DRONE_GPS` - GPS payload
- `GF_PAYLOAD_DRONE_MOTOR` - Motor output payload

## Testing

The test suite validates all critical flight control functionality:

```bash
make test-drone
```

### Test Categories

| Category | Tests | Coverage |
|----------|-------|----------|
| PID Controller | 6 | Gains, windup, clamping |
| Initialization | 3 | Init, arm/disarm, safety |
| Motor Mixing | 5 | Hover, roll, pitch, yaw, PWM range |
| Geofence | 4 | Inside, radius breach, altitude, disabled |
| Safety System | 3 | Battery, RC loss, auto-RTL |
| Flight Modes | 3 | Transitions, GPS/home requirements |
| Sensor Fusion | 3 | Level, tilt, GPS home |
| Control Loops | 2 | Stabilize, position hold |
| Emergency | 2 | Emergency stop, preflight |
| Simulation | 2 | Hover, sensor noise |

**Total: 33 tests**

## Industry Relevance

This spotlight demonstrates skills applicable to:

| Company/Platform | Relevance |
|------------------|-----------|
| **DJI** | Consumer/enterprise drone firmware |
| **Skydio** | Autonomous obstacle avoidance |
| **PX4** | Open-source autopilot stack |
| **ArduPilot** | Open-source flight controller |
| **Betaflight** | Racing drone firmware |
| **Joby Aviation** | eVTOL air taxi |
| **Lilium** | Electric air taxi |
| **Wing (Google)** | Delivery drones |

## Future Enhancements

Planned improvements for future phases:

| Feature | Description |
|---------|-------------|
| **Extended Kalman Filter** | Full EKF for optimal sensor fusion |
| **Obstacle Avoidance** | Proximity sensor integration |
| **Autonomous Waypoints** | Mission planning and execution |
| **MAVLink Protocol** | Standard telemetry protocol |
| **HIL Simulation** | Hardware-in-the-loop testing |
| **Multi-rotor Support** | Hex, octo, custom configurations |

## Files

| File | Description |
|------|-------------|
| `src/drone/drone_spotlight.c` | Main flight control implementation |
| `include/drone/flight_control.h` | Flight control API headers |
| `include/drone/sensor_fusion.h` | Sensor fusion headers |
| `include/drone/telemetry.h` | Telemetry protocol headers |
| `tests/drone_tests.c` | Comprehensive test suite |
| `docs/drone_systems.md` | This documentation |

## References

- PX4 Developer Guide: https://dev.px4.io/
- ArduPilot Documentation: https://ardupilot.org/dev/
- MAVLink Protocol: https://mavlink.io/
- Control Theory for Quadrotors: MIT OpenCourseWare
