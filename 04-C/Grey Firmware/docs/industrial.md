# Industrial & Robotics Module

## Overview

The Industrial module provides firmware building blocks for robotics,
CNC machines, conveyor systems, and industrial automation applications.

## Module Components

```
┌─────────────────────────────────────────────────────────────────────┐
│                    Industrial / Robotics Module                     │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────┐   ┌──────────────┐   ┌──────────────────────┐    │
│  │ Motor Control│   │   Encoder    │   │  Safety Interlock    │    │
│  │              │   │              │   │                      │    │
│  │ • PWM drive  │   │ • Quadrature │   │ • Dual-channel       │    │
│  │ • Profiles   │   │ • X1/X2/X4   │   │ • E-stop             │    │
│  │ • Limits     │   │ • Velocity   │   │ • Guard monitoring   │    │
│  │ • Sync       │   │ • Index      │   │ • Safe Torque Off    │    │
│  └──────────────┘   └──────────────┘   └──────────────────────┘    │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## Motor Control

### Configuration

```c
#include "industrial/motor_control.h"

gf_motor_config_t motor_cfg = {
    .pwm_freq_hz = 20000,       // 20 kHz PWM
    .pwm_resolution = 12,       // 12-bit (0-4095)
    .encoder_channel = 0,       // Feedback encoder
    .max_speed_rpm = 3000,
    .current_limit_ma = 5000,
    .enable_soft_start = true,
    .enable_soft_stop = true
};

gf_motor_init(0, &motor_cfg);
```

### Motion Profiles

```c
// Trapezoidal profile for smooth motion
gf_motor_profile_t profile = {
    .type = GF_MOTOR_PROFILE_TRAPEZOIDAL,
    .accel_rpm_per_sec = 500,
    .decel_rpm_per_sec = 500,
    .max_speed_rpm = 1500
};

gf_motor_set_profile(0, &profile);
gf_motor_move_to(0, 10000);  // Move to position 10000
```

### Multi-Axis Coordination

```c
// Synchronized start for coordinated motion
gf_motor_group_t group = {
    .motors = {0, 1, 2},
    .count = 3
};

gf_motor_sync_start(&group);
```

## Quadrature Encoder

### Configuration

```c
#include "industrial/encoder.h"

gf_encoder_config_t enc_cfg = {
    .mode = GF_ENC_MODE_X4,     // 4x counting
    .counts_per_rev = 4000,    // 1000 PPR encoder
    .invert_direction = false,
    .enable_index = true,
    .filter_ns = 100           // 100 ns glitch filter
};

gf_encoder_init(0, &enc_cfg);
```

### Reading Position and Velocity

```c
// Get current position
int32_t position = gf_encoder_get_position(0);

// Get velocity (counts/second)
int32_t velocity = gf_encoder_get_velocity(0);

// Convert to angle
int32_t angle_centideg = gf_encoder_to_angle(0, position);

// Full status
gf_encoder_status_t status;
gf_encoder_get_status(0, &status);
printf("Rev: %u, Errors: %u\n", 
       status.revolutions, status.error_count);
```

### Index Pulse Homing

```c
void on_index(uint8_t channel, int32_t position, void *ctx) {
    // Home position found
    gf_encoder_set_position(channel, 0);
    printf("Homed at pulse count %d\n", position);
}

gf_encoder_set_index_callback(on_index, NULL);
```

## Safety Interlock System

### Dual-Channel Safety Input

```c
#include "industrial/safety_interlock.h"

gf_safety_init();

// Configure E-stop (dual-channel, NC contacts)
gf_interlock_config_t estop_cfg = {
    .type = GF_INTERLOCK_ESTOP,
    .gpio_ch1 = 10,
    .gpio_ch2 = 11,
    .normally_closed = true,
    .require_pulse_test = false
};

gf_safety_add_interlock(0, &estop_cfg);

// Configure guard door
gf_interlock_config_t guard_cfg = {
    .type = GF_INTERLOCK_GUARD,
    .gpio_ch1 = 12,
    .gpio_ch2 = 13,
    .normally_closed = true
};

gf_safety_add_interlock(1, &guard_cfg);
```

### Safety State Monitoring

```c
// Check if machine can run
if (gf_safety_can_run()) {
    gf_motor_start(0);
} else {
    gf_safety_status_t status;
    gf_safety_get_status(&status);
    
    switch (status.state) {
        case GF_SAFETY_STATE_ESTOP:
            // E-stop pressed
            break;
        case GF_SAFETY_STATE_GUARD_OPEN:
            // Guard door open
            break;
        case GF_SAFETY_STATE_DISCREPANCY:
            // Dual-channel mismatch - wiring fault!
            break;
    }
}
```

### Safety Callbacks

```c
void on_safety_change(gf_safety_state_t state, 
                       uint8_t channel, void *ctx) {
    if (state != GF_SAFETY_STATE_OK) {
        // Immediate stop
        gf_motor_stop(0);
        gf_motor_stop(1);
    }
}

gf_safety_set_callback(on_safety_change, NULL);
```

## Integration Example: CNC Axis

```c
// Complete axis control with safety
typedef struct {
    uint8_t motor_ch;
    uint8_t encoder_ch;
    int32_t home_offset;
    bool    homed;
} axis_t;

void axis_init(axis_t *axis) {
    // Motor
    gf_motor_config_t m = {
        .pwm_freq_hz = 20000,
        .encoder_channel = axis->encoder_ch,
        .max_speed_rpm = 3000
    };
    gf_motor_init(axis->motor_ch, &m);
    
    // Encoder
    gf_encoder_config_t e = {
        .mode = GF_ENC_MODE_X4,
        .counts_per_rev = 4000,
        .enable_index = true
    };
    gf_encoder_init(axis->encoder_ch, &e);
}

int axis_move(axis_t *axis, int32_t target) {
    if (!gf_safety_can_run()) {
        return -1;  // Safety interlock active
    }
    
    if (!axis->homed) {
        return -2;  // Must home first
    }
    
    return gf_motor_move_to(axis->motor_ch, 
                            target + axis->home_offset);
}
```

## Safety Standards Reference

This module is designed with awareness of:

- **IEC 61508** - Functional Safety
- **ISO 13849** - Safety of Machinery
- **IEC 62443** - Industrial Cybersecurity
- **OSHA 1910** - Machine Guarding

> **Note**: This is a demonstration implementation. Production safety-critical
> systems require certified hardware, formal verification, and third-party
> assessment to applicable standards.

## Industry Applications

| Industry | Application | Components Used |
|----------|-------------|-----------------|
| Robotics | Joint control | Motor + Encoder |
| CNC | Axis positioning | All three |
| Packaging | Conveyor control | Motor + Safety |
| Automotive | Assembly fixtures | Safety + Motor |
| Semiconductor | Wafer handling | All three |

## Related Modules

- [CAN Bus](can_bus.md) - Industrial communication
- [Power Management](power_management.md) - Motor power control
- [Diagnostics](diagnostics.md) - Machine health monitoring
