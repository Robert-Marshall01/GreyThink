# Firefighting Thermal Imaging & Suppression Systems

## Overview

The Grey Firmware Firefighting Spotlight implements production-grade fire detection and
suppression control for embedded systems. It provides thermal imaging preprocessing,
intelligent hot spot detection, zone-based suppression management, and comprehensive
telemetry compliant with NFPA standards (13, 72, 2001) and IEC 61508 functional safety.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────────────┐
│                      FIREFIGHTING SYSTEM ARCHITECTURE                       │
├─────────────────────────────────────────────────────────────────────────────┤
│                                                                             │
│  ┌─────────────┐    ┌─────────────┐    ┌─────────────┐    ┌─────────────┐  │
│  │  Thermal    │    │  Thermal    │    │  Thermal    │    │  Thermal    │  │
│  │  Sensor 0   │    │  Sensor 1   │    │  Sensor 2   │    │  Sensor N   │  │
│  │  (32×32)    │    │  (32×32)    │    │  (32×32)    │    │  (32×32)    │  │
│  └──────┬──────┘    └──────┬──────┘    └──────┬──────┘    └──────┬──────┘  │
│         │                  │                  │                  │         │
│         └──────────────────┼──────────────────┼──────────────────┘         │
│                            ▼                                               │
│         ┌──────────────────────────────────────────────────────────┐       │
│         │              THERMAL FRAME PROCESSING                     │       │
│         │  ┌────────────┐ ┌────────────┐ ┌────────────────────┐   │       │
│         │  │  Gaussian  │ │  Gradient  │ │  Local Maxima      │   │       │
│         │  │   Blur     │→│ Detection  │→│  Detection         │   │       │
│         │  └────────────┘ └────────────┘ └────────────────────┘   │       │
│         └──────────────────────────────────────────────────────────┘       │
│                            │                                               │
│                            ▼                                               │
│         ┌──────────────────────────────────────────────────────────┐       │
│         │              HOT SPOT TRACKING                           │       │
│         │  • Temperature Tracking    • Rate-of-Rise Detection     │       │
│         │  • Fire Confirmation       • Hot Spot Aging             │       │
│         └──────────────────────────────────────────────────────────┘       │
│                            │                                               │
│                            ▼                                               │
│  ┌───────────┐    ┌───────────┐    ┌───────────┐    ┌───────────┐         │
│  │  Zone 0   │    │  Zone 1   │    │  Zone 2   │    │  Zone N   │         │
│  │ WET_PIPE  │    │ PREACTION │    │  GASEOUS  │    │  DELUGE   │         │
│  │  Water    │    │   Foam    │    │   FM-200  │    │   Water   │         │
│  └─────┬─────┘    └─────┬─────┘    └─────┬─────┘    └─────┬─────┘         │
│        │                │                │                │               │
│        ▼                ▼                ▼                ▼               │
│  ┌───────────────────────────────────────────────────────────────────┐    │
│  │                    VALVE CONTROL SUBSYSTEM                        │    │
│  │  • Valve State Machine    • Agent Level Tracking                 │    │
│  │  • Fail-Safe Design       • Flow Rate Control                    │    │
│  └───────────────────────────────────────────────────────────────────┘    │
│                                                                            │
│  ┌───────────────────────────────────────────────────────────────────┐    │
│  │                    INCIDENT MANAGEMENT                            │    │
│  │  • Fire Stage Tracking    • Zone Correlation                     │    │
│  │  • Event Logging          • Telemetry Generation                 │    │
│  └───────────────────────────────────────────────────────────────────┘    │
│                                                                            │
└────────────────────────────────────────────────────────────────────────────┘
```

## Key Features

### Thermal Imaging

- **32×32 pixel thermal frame processing** per sensor
- **Gaussian blur preprocessing** for noise reduction
- **Gradient calculation** for edge detection
- **Support for up to 8 thermal sensors**

### Temperature Thresholds

| Threshold | Temperature | Purpose |
|-----------|-------------|---------|
| Ambient | 25°C | Normal operating baseline |
| Warning | 60°C | Early detection alert |
| Alarm | 85°C | Fire panel alarm trigger |
| Fire | 150°C | Confirmed fire detection |
| Flashover | 600°C | Critical emergency stage |

### Hot Spot Detection

- **Local maxima detection** algorithm identifies temperature peaks
- **Rate-of-rise detection** (>15°C/min triggers alert)
- **Fire confirmation** when spot exceeds fire threshold
- **Tracking persistence** with 30-second timeout for aging
- **Up to 32 simultaneous hot spots** tracked

### Zone-Based Suppression

Each suppression zone supports:

- **Zone ID and naming** for facility management
- **System type selection**: WET_PIPE, DRY_PIPE, PREACTION, DELUGE, GASEOUS
- **Agent selection**: WATER, FOAM, CO2, FM-200, WATER_MIST
- **Multi-sensor assignment** (up to 4 sensors per zone)
- **Cross-zone confirmation** for critical areas
- **Configurable pre-action delay** for evacuation
- **Manual trigger and abort** capabilities

### Zone State Machine

```
     ┌──────────┐
     │   IDLE   │
     └────┬─────┘
          │ arm
          ▼
    ┌──────────────┐
    │  MONITORING  │◄──────────────────┐
    └──────┬───────┘                   │
           │ temp >= warning           │ abort/clear
           ▼                           │
    ┌──────────────┐                   │
    │   WARNING    │───────────────────┤
    └──────┬───────┘                   │
           │ temp >= alarm             │
           ▼                           │
    ┌──────────────┐                   │
    │    ALARM     │───────────────────┘
    └──────┬───────┘
           │ temp >= fire OR manual
           ▼
    ┌──────────────┐
    │  PREACTION   │ (if system requires delay)
    └──────┬───────┘
           │ delay elapsed
           ▼
    ┌──────────────┐
    │ DISCHARGING  │
    └──────┬───────┘
           │ complete
           ▼
    ┌──────────────┐
    │  DISCHARGED  │
    └──────────────┘
```

## API Reference

### System Control

```c
int ff_init(void);
int ff_shutdown(void);
int ff_arm_system(void);
int ff_disarm_system(void);
int ff_process(uint32_t delta_ms);
```

### Sensor Management

```c
int ff_register_sensor(uint8_t sensor_id);
int ff_update_thermal_frame(uint8_t sensor_id, const float* temps);
int ff_inject_heat_source(uint8_t sensor_id, uint16_t x, uint16_t y, 
                          float temp, float radius);
int ff_get_pixel_temp(uint8_t sensor_id, uint16_t x, uint16_t y, float* temp);
int ff_get_heat_map(uint8_t sensor_id, uint8_t* heat_map, 
                    uint16_t* width, uint16_t* height);
```

### Zone Configuration

```c
int ff_configure_zone(const ff_zone_config_t* config);
int ff_arm_zone(uint8_t zone_id);
int ff_disarm_zone(uint8_t zone_id);
int ff_get_zone_status(uint8_t zone_id, ff_zone_state_t* state_out,
                       ff_valve_state_t* valve_out, float* temp_out);
```

### Suppression Control

```c
int ff_manual_trigger(uint8_t zone_id);
int ff_abort_suppression(uint8_t zone_id);
```

### Telemetry & Diagnostics

```c
int ff_get_stats(ff_stats_t* stats);
int ff_get_hot_spots(ff_hot_spot_t* spots, uint8_t max_spots, uint8_t* count);
int ff_get_incident(ff_incident_t* incident);
int ff_get_events(ff_event_t* events, uint16_t max_events, uint16_t* count);
int ff_generate_telemetry(uint8_t* buffer, uint16_t max_len, uint16_t* len);
```

## Data Structures

### Zone Configuration

```c
typedef struct {
    uint8_t zone_id;
    char name[24];
    ff_system_type_t system_type;
    ff_agent_t agent;
    float area_m2;
    uint8_t sensor_ids[4];
    uint8_t sensor_count;
    float alarm_threshold;
    float discharge_rate;
    uint32_t preaction_delay;
    bool require_cross_zone;
    bool abort_enabled;
} ff_zone_config_t;
```

### Hot Spot Information

```c
typedef struct {
    uint16_t x;
    uint16_t y;
    float temperature;
    float peak_temperature;
    float rate_of_rise;
    float area_m2;
    bool growing;
    bool confirmed_fire;
    uint32_t first_detect_ms;
    uint32_t last_update_ms;
    uint8_t zone_id;
} ff_hot_spot_t;
```

### Incident Report

```c
typedef struct {
    uint32_t incident_id;
    uint32_t start_time;
    ff_fire_stage_t stage;
    uint8_t zones_affected;
    uint8_t zones_discharged;
    float max_temperature;
    float spread_rate_m2_min;
    bool evacuation_ordered;
    bool fire_dept_notified;
    bool suppression_effective;
    uint32_t resolution_time;
} ff_incident_t;
```

## Message Bus Integration

The firefighting system integrates with Grey Firmware's message bus using these topics:

| Topic | Purpose |
|-------|---------|
| `GF_TOPIC_FIRE_ALERT` | Fire detection notifications |
| `GF_TOPIC_FIRE_ZONE_STATUS` | Zone state changes |
| `GF_TOPIC_SUPPRESSION_STATUS` | Suppression system updates |
| `GF_TOPIC_SUPPRESSION_TRIGGER` | Suppression activation commands |
| `GF_TOPIC_THERMAL_SCAN` | Thermal frame data |
| `GF_TOPIC_THERMAL_ALERT` | High temperature alerts |
| `GF_TOPIC_FIRE_INCIDENT` | Incident status reports |
| `GF_TOPIC_FIRE_TELEMETRY` | Continuous telemetry stream |

## Standards Compliance

### NFPA Compliance

- **NFPA 13**: Sprinkler system installation standards
- **NFPA 72**: Fire alarm code requirements
- **NFPA 2001**: Clean agent suppression systems

### IEC 61508 Safety

- Fail-safe valve design
- Redundant sensor cross-zone verification
- Comprehensive event logging for audit trails

## Usage Example

```c
#include "firefighting/thermal_sensor.h"
#include "firefighting/suppression_actuator.h"
#include "firefighting/fire_telemetry.h"

/* Include spotlight implementation */
#include "src/firefighting/firefighting_spotlight.c"

int main(void) {
    /* Initialize system */
    ff_init();
    
    /* Register thermal sensors */
    ff_register_sensor(0);
    ff_register_sensor(1);
    
    /* Configure zones */
    ff_zone_config_t warehouse = {
        .zone_id = 0,
        .name = "Warehouse A",
        .system_type = FF_SYSTEM_WET_PIPE,
        .agent = FF_AGENT_WATER,
        .alarm_threshold = FF_TEMP_ALARM,
        .sensor_count = 1
    };
    warehouse.sensor_ids[0] = 0;
    ff_configure_zone(&warehouse);
    
    ff_zone_config_t server_room = {
        .zone_id = 1,
        .name = "Server Room",
        .system_type = FF_SYSTEM_GASEOUS,
        .agent = FF_AGENT_FM200,
        .require_cross_zone = true,
        .preaction_delay = 30000,
        .abort_enabled = true,
        .sensor_count = 2
    };
    server_room.sensor_ids[0] = 0;
    server_room.sensor_ids[1] = 1;
    ff_configure_zone(&server_room);
    
    /* Arm system */
    ff_arm_system();
    
    /* Main loop */
    while (running) {
        /* Update thermal frames from hardware */
        float temps[FF_THERMAL_PIXELS];
        read_thermal_sensor(0, temps);
        ff_update_thermal_frame(0, temps);
        
        /* Process fire detection and suppression */
        ff_process(100);  /* 100ms tick */
        
        /* Check for incidents */
        ff_incident_t incident;
        if (ff_get_incident(&incident) == 0) {
            report_fire_incident(&incident);
        }
        
        delay_ms(100);
    }
    
    ff_shutdown();
    return 0;
}
```

## Test Coverage

The firefighting spotlight includes 44 comprehensive tests covering:

- System initialization/shutdown
- Sensor registration and frame processing
- Hot spot detection and tracking
- Zone configuration (all system types)
- Arming/disarming operations
- Fire detection and alarm triggering
- Rate-of-rise detection
- Manual trigger and abort
- Preaction delay timing
- Statistics and event logging
- Telemetry generation
- Fire spread simulation
- Sensor noise filtering
- Multi-zone coordination
- Cross-zone verification
- Boundary conditions

Run tests with:
```bash
make test-firefighting
```

## File Structure

```
include/firefighting/
├── fire_telemetry.h      # Telemetry structures and functions
├── suppression_actuator.h # Valve and actuator control
└── thermal_sensor.h       # Thermal imaging interface

src/firefighting/
└── firefighting_spotlight.c  # Complete implementation (~1300 lines)

tests/
└── firefighting_tests.c      # 44 integration tests
```

## See Also

- [Overview](overview.md) - Grey Firmware architecture overview
- [Disaster Relief Systems](disaster_relief_systems.md) - Emergency response integration
- [Microgrid Systems](microgrid_systems.md) - Power management during incidents
