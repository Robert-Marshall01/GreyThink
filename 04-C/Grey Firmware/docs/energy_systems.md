# Energy Harvesting & Smart Grid

## Overview

The Energy Harvesting subsystem provides a production-grade implementation for:
- **Solar/renewable energy harvesting** with Maximum Power Point Tracking (MPPT)
- **Battery management** with CC/CV charging and protection
- **Power budget management** for critical and normal operations
- **Smart grid integration** for bidirectional power flow

This spotlight demonstrates expertise in renewable energy systems, power electronics firmware, and grid-tied inverter control.

## Architecture

```
┌─────────────────────────────────────────────────────────────────────┐
│                     Energy Harvesting System                        │
├─────────────────────────────────────────────────────────────────────┤
│                                                                     │
│  ┌──────────────┐    ┌──────────────┐    ┌──────────────┐          │
│  │ Solar Panel  │───►│    MPPT      │───►│   Battery    │          │
│  │   Driver     │    │  Controller  │    │   Manager    │          │
│  └──────────────┘    └──────────────┘    └──────────────┘          │
│         │                   │                   │                   │
│         ▼                   ▼                   ▼                   │
│  ┌──────────────────────────────────────────────────────┐          │
│  │              Power Budget Manager                     │          │
│  │   ┌─────────┐  ┌─────────┐  ┌─────────────────┐     │          │
│  │   │CRITICAL │  │ NORMAL  │  │    SURPLUS      │     │          │
│  │   │ <20%    │  │ 20-80%  │  │     >80%        │     │          │
│  │   └─────────┘  └─────────┘  └─────────────────┘     │          │
│  └──────────────────────────────────────────────────────┘          │
│                              │                                      │
│                              ▼                                      │
│                   ┌──────────────────┐                              │
│                   │   Smart Grid     │                              │
│                   │   Interface      │                              │
│                   │  • Export Power  │                              │
│                   │  • Demand Resp.  │                              │
│                   │  • Freq Support  │                              │
│                   └──────────────────┘                              │
│                                                                     │
└─────────────────────────────────────────────────────────────────────┘
```

## MPPT Algorithms

Maximum Power Point Tracking (MPPT) extracts optimal power from solar panels as conditions change.

### Algorithm Modes

| Mode | Description | Use Case |
|------|-------------|----------|
| `MPPT_PERTURB_OBSERVE` | Steps voltage up/down, measures power change | Fast-changing conditions |
| `MPPT_INCREMENTAL_COND` | Uses dI/dV slope to find MPP | Steady-state optimization |
| `MPPT_CONSTANT_VOLTAGE` | Fixed voltage ratio to Voc | Stable irradiance |

### Perturb & Observe Algorithm

```c
// Simplified P&O algorithm
if (delta_power > 0) {
    // Power increased - keep going same direction
    if (delta_voltage > 0) {
        operating_voltage += step_size;  // Increase voltage
    } else {
        operating_voltage -= step_size;  // Decrease voltage
    }
} else {
    // Power decreased - reverse direction
    if (delta_voltage > 0) {
        operating_voltage -= step_size;  // Decrease voltage
    } else {
        operating_voltage += step_size;  // Increase voltage
    }
}
```

### Incremental Conductance Algorithm

The incremental conductance algorithm uses the slope of the I-V curve:

- At MPP: $\frac{dI}{dV} = -\frac{I}{V}$
- Left of MPP: $\frac{dI}{dV} > -\frac{I}{V}$ → Increase voltage
- Right of MPP: $\frac{dI}{dV} < -\frac{I}{V}$ → Decrease voltage

This provides more accurate tracking under rapidly changing conditions.

### API Usage

```c
#include "energy/solar_panel.h"

// Initialize with panel specifications
gf_solar_panel_config_t config = {
    .voc = 42.0f,           // Open circuit voltage
    .isc = 10.5f,           // Short circuit current
    .vmp = 35.0f,           // Voltage at max power
    .imp = 9.8f,            // Current at max power
    .temp_coefficient = -0.35f  // %/°C
};
gf_solar_panel_init(&config);

// Set MPPT mode
gf_mppt_set_mode(MPPT_PERTURB_OBSERVE);

// Update tracking (call periodically, e.g., every 100ms)
gf_mppt_update();

// Get current operating point
float voltage = gf_mppt_get_voltage();
float current = gf_mppt_get_current();
float power = gf_mppt_get_power();
```

## Battery Management

### Charging States

The battery manager implements a state machine for safe charging:

```
┌────────────┐     Vbat < Vprecharge    ┌────────────┐
│ DISCHARGED │─────────────────────────►│  PRECHARGE │
└────────────┘                          └────────────┘
                                               │
                                    Vbat > Vprecharge
                                               ▼
                                        ┌─────────────┐
      ┌────────────────────────────────►│     CC      │
      │                                 │ (Const. Cur)│
      │                                 └─────────────┘
      │                                        │
      │                             Vbat >= Vmax (4.2V)
      │                                        ▼
      │                                 ┌─────────────┐
      │                                 │     CV      │
      │                                 │ (Const. Volt)│
      │                                 └─────────────┘
      │                                        │
      │                             Ibat < Iterm (C/20)
      │                                        ▼
      │                                 ┌─────────────┐
      │         Vbat < Vrecharge       │   TRICKLE   │
      └────────────────────────────────│  (Full)     │
                                       └─────────────┘
```

### Temperature Protection

| Temperature Range | Action |
|-------------------|--------|
| < 0°C | Charging disabled (cold) |
| 0-10°C | Reduced charge current (50%) |
| 10-45°C | Normal charging |
| 45-60°C | Reduced charge current (50%) |
| > 60°C | Charging disabled (hot) |

### Fault Detection

- **Overvoltage**: Cell voltage > 4.25V
- **Undervoltage**: Cell voltage < 2.5V
- **Overcurrent**: Charge/discharge exceeds limits
- **Overtemperature**: Outside safe operating range
- **Cell imbalance**: Cell voltage difference > threshold

### API Usage

```c
#include "energy/energy_harvesting.h"

// Initialize battery manager
gf_battery_config_t batt_config = {
    .chemistry = BATTERY_LIION,
    .capacity_mah = 5000,
    .cells_series = 3,
    .cells_parallel = 2,
    .charge_current_ma = 2000,
    .max_voltage_mv = 12600  // 4.2V × 3 cells
};
gf_battery_init(&batt_config);

// Start charging
gf_battery_start_charging();

// Monitor state
gf_battery_state_t state = gf_battery_get_state();
uint8_t soc = gf_battery_get_soc();          // 0-100%
float temp = gf_battery_get_temperature();    // °C
uint32_t cycles = gf_battery_get_cycles();
```

## Power Budget Management

### Budget States

The power budget manager tracks available power and allocates to consumers:

| State | Available Power | Actions |
|-------|-----------------|---------|
| `BUDGET_CRITICAL` | < 20% | Essential loads only, disable non-critical |
| `BUDGET_NORMAL` | 20-80% | Standard operation |
| `BUDGET_SURPLUS` | > 80% | Allow grid export, charge storage |

### Power Registration

Consumers register power requirements:

```c
// Register a power consumer
gf_power_consumer_t display = {
    .name = "Display",
    .priority = POWER_PRIORITY_NORMAL,
    .min_power_mw = 50,
    .max_power_mw = 500
};
gf_power_budget_register(&display);

// Request power allocation
int32_t granted = gf_power_budget_request(&display, 300);  // Request 300mW
if (granted >= display.min_power_mw) {
    // Power granted, operate display
}

// Release power when done
gf_power_budget_release(&display, granted);
```

### Budget Callbacks

```c
void on_budget_change(gf_budget_state_t old_state, gf_budget_state_t new_state) {
    if (new_state == BUDGET_CRITICAL) {
        // Reduce power consumption
        disable_backlight();
        reduce_sensor_rate();
    }
}

gf_power_budget_set_callback(on_budget_change);
```

### Priority-Based Allocation

| Priority | Description | Examples |
|----------|-------------|----------|
| `CRITICAL` | Safety-essential, never shed | Watchdog, safety sensors |
| `HIGH` | Important functions | Communication, data logging |
| `NORMAL` | Standard operations | Display, user interface |
| `LOW` | Non-essential | Background tasks, LED indicators |

## Smart Grid Interface

### Grid Connection

```c
#include "energy/smart_grid.h"

// Initialize grid interface
gf_grid_config_t grid_config = {
    .nominal_voltage = 240,      // VAC
    .nominal_frequency = 50,     // Hz
    .max_export_power = 3000,    // W
    .power_factor_limit = 0.95
};
gf_grid_init(&grid_config);

// Connect to grid
gf_grid_connect();

// Monitor grid status
gf_grid_status_t status = gf_grid_get_status();
float voltage = gf_grid_get_voltage();
float frequency = gf_grid_get_frequency();
```

### Power Export

```c
// Enable power export
gf_grid_set_export_enabled(true);
gf_grid_set_export_limit(2000);  // Max 2kW export

// Check export status
int32_t export_w = gf_grid_get_export_power();
```

### Demand Response

The grid interface supports utility demand response signals:

```c
typedef enum {
    DEMAND_NORMAL,        // Normal operation
    DEMAND_MODERATE,      // Reduce non-essential loads
    DEMAND_HIGH,          // Minimize consumption
    DEMAND_CRITICAL       // Emergency reduction
} gf_demand_level_t;

void on_demand_response(gf_demand_level_t level) {
    switch (level) {
        case DEMAND_HIGH:
            // Shift deferrable loads
            pause_ev_charging();
            reduce_hvac();
            break;
        case DEMAND_CRITICAL:
            // Emergency load shedding
            shed_non_critical_loads();
            break;
    }
}

gf_grid_set_demand_callback(on_demand_response);
```

### Frequency Support

Grid-forming inverter support for frequency regulation:

```c
// Enable frequency droop response
gf_grid_set_frequency_support(true);
gf_grid_set_droop_coefficient(0.05);  // 5% droop

// At 49.5Hz (1% under 50Hz): Export increased by 20%
// At 50.5Hz (1% over 50Hz): Export decreased by 20%
```

## Message Bus Integration

### Energy Topics

The energy subsystem publishes to these message bus topics:

| Topic | Payload | Description |
|-------|---------|-------------|
| `GF_TOPIC_ENERGY_UPDATE` | `gf_energy_update_payload_t` | Periodic energy status |
| `GF_TOPIC_ENERGY_HARVEST` | Solar/wind harvest events | Power generation updates |
| `GF_TOPIC_ENERGY_BATTERY` | `gf_energy_battery_payload_t` | Battery state changes |
| `GF_TOPIC_ENERGY_GRID` | `gf_grid_telemetry_payload_t` | Grid measurements |
| `GF_TOPIC_ENERGY_BUDGET` | Budget state changes | Power allocation events |
| `GF_TOPIC_ENERGY_FAULT` | `gf_energy_alert_payload_t` | Fault notifications |

### Example Subscription

```c
void on_energy_update(const gf_message_t* msg) {
    gf_energy_update_payload_t* payload = msg->data;
    printf("Source: %s, Power: %.1fW, SoC: %d%%\n",
           get_source_name(payload->source),
           payload->power_w,
           payload->soc_percent);
}

gf_msg_subscribe(GF_TOPIC_ENERGY_UPDATE, on_energy_update);
```

## Statistics and Monitoring

### Energy Statistics

```c
gf_energy_stats_t stats;
gf_energy_get_stats(&stats);

// Solar statistics
printf("Solar: Today: %.2f kWh, Lifetime: %.2f MWh\n",
       stats.solar_today_wh / 1000.0f,
       stats.solar_lifetime_wh / 1000000.0f);

// Battery statistics
printf("Battery: Cycles: %d, Health: %d%%\n",
       stats.battery_cycles,
       stats.battery_health_percent);

// Grid statistics
printf("Grid: Imported: %.2f kWh, Exported: %.2f kWh\n",
       stats.grid_import_wh / 1000.0f,
       stats.grid_export_wh / 1000.0f);
```

### Diagnostic Commands

```c
// Run self-diagnostics
gf_energy_diagnostic_result_t result;
gf_energy_run_diagnostics(&result);

if (result.mppt_ok && result.battery_ok && result.grid_ok) {
    printf("All systems nominal\n");
} else {
    // Handle faults
}
```

## Testing

The energy subsystem includes 24 automated tests:

```bash
make test-energy   # Run energy tests only
```

### Test Categories

| Category | Tests | Description |
|----------|-------|-------------|
| MPPT Algorithms | 4 | P&O tracking, incremental conductance |
| Battery Charging | 6 | CC/CV, precharge, temperature |
| Power Budget | 6 | State machine, allocations |
| Smart Grid | 3 | Connection, export, demand response |
| Variable Sunlight | 3 | Irradiance simulation |
| Integration | 2 | End-to-end pipelines |

## Configuration

### Compile-Time Options

```c
// In grey_firmware.h or project config
#define GF_ENERGY_MAX_POWER_SOURCES    8
#define GF_ENERGY_MAX_CONSUMERS       16
#define GF_ENERGY_MPPT_PERIOD_MS     100
#define GF_ENERGY_STATS_INTERVAL_MS 1000
```

### Runtime Configuration

```c
gf_energy_config_t config = {
    .mppt_mode = MPPT_PERTURB_OBSERVE,
    .battery_low_threshold = 20,     // %
    .battery_high_threshold = 80,    // %
    .grid_export_enabled = true,
    .demand_response_enabled = true
};
gf_energy_configure(&config);
```

## Related Documentation

- [Overview](overview.md) - Project structure and architecture
- [Module Map](module_map.md) - Module dependencies
- [Power Management](../include/power/power_management.h) - System power states

---

*Energy Harvesting & Smart Grid - Demonstrating renewable energy firmware for solar, battery, and grid-tied systems*
