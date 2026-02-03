# Orbital Debris Tracking & Collision Prediction

Grey Firmware Phase 23 spotlight implementation for space situational awareness and collision avoidance.

## Overview

The Debris Tracking module provides a production-grade framework for monitoring space debris, predicting collisions, and managing conjunction events. It demonstrates embedded systems expertise in safety-critical aerospace applications where split-second decisions protect multi-billion dollar assets.

## Space Debris Environment

### The Challenge

- **LEO (200-2000 km)**: ~23,000 tracked objects, millions of sub-cm fragments
- **MEO (2000-35786 km)**: Navigation constellation traffic
- **GEO (35786 km)**: Premium orbital real estate for communications
- **Hypervelocity impacts**: 7-15 km/s relative velocities
- **Kessler Syndrome risk**: Cascading fragmentation events

### Object Classification

| Type | Description | Example |
|------|-------------|---------|
| Payload | Active/inactive spacecraft | ISS, Starlink |
| Rocket Body | Upper stages, boosters | Falcon 9 second stage |
| Debris | Breakup fragments | Cosmos-2251 debris |
| Analyst Object | Manually tracked items | Unknown objects |

## Architecture

### Core Components

```
┌─────────────────────────────────────────────────────────────────┐
│                    Debris Tracking System                       │
├─────────────────┬─────────────────┬─────────────────────────────┤
│   Radar Input   │   Track Manager │   Conjunction Assessment    │
│  ┌──────────┐   │   ┌──────────┐  │   ┌────────────────────┐   │
│  │ TMR Vote │   │   │ Correlate│  │   │ Screen Conjunctions│   │
│  │ Validate │   │   │ Propagate│  │   │ Calculate Pc       │   │
│  │ Filter   │───┼──▶│ Update   │──┼──▶│ Classify Severity  │   │
│  └──────────┘   │   └──────────┘  │   └────────────────────┘   │
└─────────────────┴─────────────────┴─────────────────────────────┘
                                               │
                          ┌────────────────────┴────────────────────┐
                          ▼                                         ▼
               ┌──────────────────┐                    ┌──────────────────┐
               │ Maneuver Planner │                    │ Telemetry Gen    │
               │ (CDM Generation) │                    │ (CCSDS Format)   │
               └──────────────────┘                    └──────────────────┘
```

### Data Flow

1. **Radar Detection**: S-band/X-band returns with TMR voting
2. **Track Processing**: Correlation with catalog or new object creation
3. **Orbit Determination**: State vector and covariance estimation
4. **Conjunction Screening**: Protected asset proximity checks
5. **Probability Calculation**: Collision probability via covariance methods
6. **Alert Generation**: Severity-based alarm triggering
7. **Telemetry Output**: CCSDS-compliant data products

## Key Features

### TMR Sensor Voting

Triple modular redundancy for radar measurements:

```c
typedef struct {
    float readings[3];
    float voted_value;
    tmr_result_t vote_status;  // TMR_AGREE_ALL, TMR_AGREE_2OF3, TMR_DISAGREE
    sensor_health_t health;
} tmr_sensor_t;
```

Voting logic:
- **All agree**: Average of three readings
- **2-of-3 agree**: Average of agreeing pair, flag degraded
- **All disagree**: Use median, flag fault condition

### Orbital Mechanics

State vector propagation with J2 perturbation:

```c
// Two-body motion with oblateness correction
double j2_factor = 1.5 * J2_COEFFICIENT * (RE_KM²) / r²;

// Acceleration including J2
ax = -μ * x / r³ * (1 + j2_factor * (1 - 5*z²/r²));
ay = -μ * y / r³ * (1 + j2_factor * (1 - 5*z²/r²));
az = -μ * z / r³ * (1 + j2_factor * (3 - 5*z²/r²));
```

### Collision Probability

Simplified Foster method implementation:

```c
float sigma_combined = sqrt(σr² + σt²);
float u² = miss² / (2 * sigma_combined²);
float r² = hbr² / sigma_combined²;
float Pc = (r² / 2) * exp(-u²);
```

### Conjunction Severity Classification

| Level | Criteria | Response |
|-------|----------|----------|
| GREEN | Miss > 5 km | No action |
| WATCH | Miss 1-5 km | Monitor |
| YELLOW | Pc > 1e-5 | Elevated tracking |
| RED | Pc > 1e-4 | Maneuver planning |
| EMERGENCY | Pc > 1e-4, TCA < 6 hours | Execute maneuver |

## API Reference

### Initialization

```c
// Initialize debris tracking system
int debris_init(void);

// Configure protected asset (spacecraft to protect)
int debris_config_asset(uint8_t asset_id, uint32_t norad_id, 
                        const char *name, const state_vector_t *state);
```

### Sensor Input

```c
// Update with radar detection (TMR channels)
int debris_update_sensors(uint8_t detection_id,
                          float range[3], float range_rate[3],
                          float azimuth[3], float elevation[3],
                          float rcs_dbsm, float snr_db,
                          uint64_t timestamp_us);
```

### Main Processing

```c
// Run periodic processing (scans, predictions, screening)
int debris_process(uint32_t time_ms);
```

### Conjunction Queries

```c
// Get active conjunctions
int debris_get_conjunctions(conjunction_event_t *events, uint8_t max);

// Get specific conjunction
int debris_get_conjunction_by_id(uint32_t conj_id, conjunction_event_t *event);

// Acknowledge conjunction (operator action)
int debris_acknowledge_conjunction(uint32_t conj_id);
```

### Status Queries

```c
uint16_t debris_get_track_count(void);
uint8_t debris_get_asset_count(void);
uint8_t debris_get_conjunction_count(void);
uint8_t debris_get_red_count(void);
uint8_t debris_get_yellow_count(void);
float debris_get_closest_approach(void);
bool debris_has_red_conjunction(void);
bool debris_has_sensor_fault(void);
```

### Telemetry

```c
// Generate CCSDS telemetry packet
int debris_get_telemetry(uint8_t *buffer, size_t max_len);
```

## Message Bus Topics

Phase 23 adds debris-related topics:

| Topic | Description |
|-------|-------------|
| `debris/detected` | New object detection |
| `debris/catalog` | Catalog correlation event |
| `debris/track` | Track update |
| `debris/collision/alert` | Collision alert notification |
| `debris/collision/probability` | Pc assessment update |
| `debris/conjunction/event` | New conjunction detected |
| `debris/conjunction/update` | Conjunction status change |
| `debris/maneuver/recommendation` | Maneuver recommendation |
| `debris/maneuver/execute` | Maneuver execution command |
| `debris/tracking/update` | Tracking status update |
| `debris/telemetry` | Telemetry packet |
| `debris/alarm` | System alarm |
| `debris/radar/detection` | Raw radar detection |
| `debris/asset/status` | Protected asset status |

## Standards Compliance

The implementation follows aerospace industry standards:

- **CCSDS 502.0-B**: Orbit Data Messages (ODM)
- **CCSDS 508.0-B**: Conjunction Data Messages (CDM)
- **NASA-STD-8719.14**: Process for Limiting Orbital Debris
- **ISO 24113**: Space debris mitigation requirements
- **ESA Space Debris Guidelines**: European debris mitigation practices

## Configuration

Default system configuration:

```c
debris_config_t config = {
    .screen_distance_leo_km = 5.0f,    // LEO screening threshold
    .screen_distance_geo_km = 15.0f,   // GEO screening threshold
    .pc_red_threshold = 1e-4f,         // Red alert Pc threshold
    .pc_yellow_threshold = 1e-5f,      // Yellow alert Pc threshold
    .prediction_days = 7,               // Forecast horizon
    .auto_maneuver_enabled = false,    // Require operator approval
    .cdm_auto_generation = true        // Generate CDMs automatically
};
```

## Test Coverage

The test suite (`tests/test_debris.c`) covers:

- **60 test cases** across 11 categories
- **106 assertions** validating behavior
- Initialization and configuration
- TMR sensor voting logic
- Orbital mechanics calculations
- Detection processing and tracking
- Collision probability algorithms
- Conjunction management
- Alarm triggering and clearing
- Telemetry generation
- Integration scenarios

Run tests: `make test-debris`

## Example Usage

```c
// Initialize system
debris_init();

// Configure protected asset (ISS)
state_vector_t iss_state = {
    .x = -4640.0, .y = -1108.0, .z = 5024.0,
    .vx = 3.721, .vy = -5.831, .vz = -2.612,
    .epoch_jd = 2460000.5
};
debris_config_asset(0, 25544, "ISS", &iss_state);

// Main loop
while (running) {
    // Feed radar detections
    float range[3] = {get_radar_range(0), get_radar_range(1), get_radar_range(2)};
    float range_rate[3] = {...};
    float azimuth[3] = {...};
    float elevation[3] = {...};
    
    debris_update_sensors(det_id++, range, range_rate, azimuth, elevation,
                          rcs, snr, timestamp);
    
    // Process tracking and conjunction screening
    debris_process(get_time_ms());
    
    // Check for alerts
    if (debris_has_red_conjunction()) {
        conjunction_event_t events[8];
        int count = debris_get_conjunctions(events, 8);
        // Handle red conjunctions...
    }
    
    // Generate telemetry
    uint8_t telem[256];
    int len = debris_get_telemetry(telem, sizeof(telem));
    transmit_telemetry(telem, len);
}
```

## Related Documentation

- [Radiation Systems](radiation_systems.md) - Space radiation protection
- [Rocket Engine Telemetry](rocket_systems.md) - Launch vehicle monitoring
- [Satellite Communication](satellite_comm.md) - Space-ground links
- [Overview](overview.md) - Project overview

## File Locations

| File | Description |
|------|-------------|
| `src/debris/debris_spotlight.c` | Implementation (~1,100 lines) |
| `tests/test_debris.c` | Test suite (60 tests) |
| `include/space_debris/orbital_radar.h` | Radar sensor stub |
| `include/space_debris/collision_prediction.h` | Prediction stub |
| `include/space_debris/debris_telemetry.h` | Telemetry stub |
