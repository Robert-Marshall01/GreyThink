# Battery Management System (BMS) Spotlight

This document describes the Battery Management System spotlight implementation in Grey Firmware, demonstrating expertise in embedded energy storage systems.

## Overview

The BMS spotlight provides a production-quality implementation of battery management for multi-cell lithium-ion packs. It demonstrates key competencies required for modern energy storage applications in electric vehicles, grid storage, UPS systems, and consumer electronics.

## Industry Relevance

Battery Management Systems are critical components in:

- **Electric Vehicles**: Tesla, BMW, Rivian, Ford vehicles require sophisticated BMS for safety and performance
- **Grid-Scale Storage**: Tesla Megapack, Fluence, LG Energy systems rely on BMS for grid stability
- **Data Center UPS**: Ensuring uninterrupted power with real-time battery monitoring
- **Consumer Electronics**: Laptops, smartphones, power tools with lithium batteries
- **Aerospace**: Satellite and aircraft battery systems with stringent safety requirements

## Key Features

### 1. Cell Voltage Monitoring

The system monitors individual cell voltages in packs up to 24 cells:

```c
// Per-cell voltage tracking with fault detection
typedef struct {
    uint16_t voltage_mv;      // Cell voltage (mV)
    int16_t current_ma;       // Cell current (mA)
    bool balancing;           // Balancing active
    bool ov_fault;            // Overvoltage fault
    bool uv_fault;            // Undervoltage fault
} bms_cell_t;
```

**Protection Thresholds:**
- Undervoltage lockout: 2500 mV
- Undervoltage warning: 2800 mV  
- Overvoltage warning: 4150 mV
- Overvoltage lockout: 4250 mV

### 2. Thermal Management

Multi-zone temperature monitoring with thermal runaway prevention:

```c
typedef struct {
    int16_t temp_c10;         // Temperature (0.1°C)
    int16_t temp_rate_c10_s;  // Rate of change
    int16_t max_temp_c10;     // Maximum recorded
    bool warning;             // Warning active
    bool fault;               // Fault active
} bms_temp_zone_t;
```

**Thermal Thresholds:**
- Under temperature: 0°C
- Over temperature warning: 45°C
- Over temperature lockout: 60°C
- Thermal runaway rate: 2°C/second

### 3. Charge Control State Machine

The system implements industry-standard CC-CV charging:

```
INIT → IDLE ←→ CHARGING ←→ DISCHARGING
         ↓         ↓            ↓
       FAULT ← (any fault detected)
         ↓
      SHUTDOWN
```

**Charge Phases:**
1. **Precharge**: Trickle charge for deeply depleted cells
2. **Constant Current (CC)**: Maximum safe charging current
3. **Constant Voltage (CV)**: Taper current as cells fill
4. **Top-off**: Final saturation phase
5. **Complete**: Charging finished

### 4. SOC/SOH Estimation

State of Charge and State of Health estimation using:

- **Coulomb Counting**: Real-time current integration
- **OCV Correlation**: Open circuit voltage lookup (idle state)
- **Cycle Counting**: Full equivalent cycle tracking
- **Capacity Fade**: SOH estimation based on cycle count

```c
typedef struct {
    uint8_t soc_pct;          // State of charge (%)
    uint8_t soh_pct;          // State of health (%)
    int32_t coulomb_count_mas; // Coulomb counter (mAs)
    uint32_t capacity_mah;    // Pack capacity (mAh)
    uint32_t remaining_mah;   // Remaining capacity
    uint32_t full_cycles;     // Full cycle count
    uint16_t internal_res_mohm; // Internal resistance
    bool valid;               // Estimation valid
} bms_soc_estimator_t;
```

### 5. Cell Balancing

Passive balancing support with configurable threshold:

- Identifies cells above minimum voltage + threshold
- Activates balancing resistors on high cells
- Operates during charging and idle states
- Configurable balance threshold (default: 30mV)

### 6. Fault Detection

Comprehensive fault handling with fail-safe design:

| Fault | Description | Action |
|-------|-------------|--------|
| CELL_UV | Cell undervoltage | Open contactor, disable discharge |
| CELL_OV | Cell overvoltage | Open contactor, stop charging |
| PACK_UV | Pack undervoltage | Emergency shutdown |
| PACK_OV | Pack overvoltage | Emergency shutdown |
| OVER_TEMP | Temperature too high | Reduce current, open contactor |
| UNDER_TEMP | Temperature too low | Disable charging |
| THERMAL_RUNAWAY | Rapid temperature rise | Emergency shutdown |
| OVERCURRENT | Excessive current | Open contactor |
| SHORT_CIRCUIT | Short circuit detected | Immediate shutdown |
| COMM_ERROR | Communication failure | Enter safe state |
| SENSOR_FAIL | Sensor malfunction | Enter safe state |
| ISOLATION | Isolation fault | Enter safe state |

### 7. Contactor Control

Safe contactors sequencing with precharge:

1. Verify no faults present
2. Activate precharge relay
3. Wait for capacitor charge (100ms)
4. Close main contactor
5. Disable precharge relay

## API Reference

### Initialization

```c
int bms_init(const bms_config_t* config);
void bms_shutdown(void);
```

### Charge/Discharge Control

```c
int bms_start_charge(void);
void bms_stop_charge(void);
int bms_enable_discharge(void);
void bms_disable_discharge(void);
```

### Status Monitoring

```c
int bms_get_status(bms_status_t* status);
uint16_t bms_get_cell_voltage(uint8_t cell);
int16_t bms_get_zone_temp(uint8_t zone);
```

### Main Processing

```c
void bms_process(uint32_t delta_ms);
```

## Configuration

The BMS supports multiple cell chemistries:

| Chemistry | Nominal Voltage | Min Voltage | Max Voltage |
|-----------|-----------------|-------------|-------------|
| Li-ion | 3.7V | 2.5V | 4.2V |
| LiFePO4 | 3.2V | 2.5V | 3.65V |
| LiPo | 3.7V | 3.0V | 4.2V |
| NiMH | 1.2V | 0.9V | 1.4V |
| Solid-state | 4.0V | 2.5V | 4.4V |

## Integration with Grey Firmware

### Message Bus Topics

The BMS publishes telemetry on standardized topics:

```c
#define GF_TOPIC_BATTERY_STATUS     "bms/status"
#define GF_TOPIC_BATTERY_VOLTAGE    "bms/voltage"
#define GF_TOPIC_BATTERY_CURRENT    "bms/current"
#define GF_TOPIC_BATTERY_SOC        "bms/soc"
#define GF_TOPIC_BATTERY_TEMP       "bms/temperature"
#define GF_TOPIC_THERMAL_ALERT      "bms/thermal/alert"
#define GF_TOPIC_BMS_FAULT          "bms/fault"
#define GF_TOPIC_BMS_BALANCE        "bms/balance"
#define GF_TOPIC_BMS_CONTACTOR      "bms/contactor"
#define GF_TOPIC_BMS_CHARGE_PHASE   "bms/charge/phase"
```

### Scheduler Integration

The BMS process function can be registered as a periodic task:

```c
gf_sched_create_task("bms", bms_task_handler, NULL, 100, GF_SCHED_PRIO_HIGH);
```

## Test Coverage

The BMS implementation includes 67 integration tests covering:

- Initialization edge cases
- Status monitoring accuracy
- Charge control transitions
- Discharge control behavior
- Overvoltage protection
- Undervoltage protection
- Thermal protection
- Overcurrent/short circuit protection
- Cell balancing logic
- SOC/SOH estimation
- State machine transitions
- Fault injection and recovery

Run tests with:
```bash
make test-bms
```

## Future Enhancements

Potential additions for production deployment:

1. **Active Balancing**: Charge transfer between cells for faster balancing
2. **Predictive SOH**: Machine learning-based capacity fade prediction
3. **CAN Bus Integration**: J1939/UDS diagnostic protocol support
4. **Cloud Telemetry**: Remote monitoring and fleet analytics
5. **Thermal Preconditioning**: Pre-heating/cooling for optimal performance

## References

- IEC 62619: Lithium secondary cells for industrial applications
- ISO 12405: Electrically propelled vehicles - Battery systems
- SAE J2464: Electric and Hybrid Electric Vehicle Battery Systems
- UL 2580: Batteries for Use In Electric Vehicles
