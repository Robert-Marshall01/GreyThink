# Module Dependency Map

This document describes the relationships between Grey Firmware modules and their dependencies.

## Dependency Graph

```
                              ┌─────────────────┐
                              │  Application    │
                              │   (main.c)      │
                              └────────┬────────┘
                                       │
                    ┌──────────────────┼──────────────────┐
                    │                  │                  │
                    ▼                  ▼                  ▼
            ┌───────────┐      ┌───────────┐      ┌───────────┐
            │  Message  │      │ Scheduler │      │   Error   │
            │   Bus     │◄────►│           │◄────►│  Handler  │
            └─────┬─────┘      └─────┬─────┘      └─────┬─────┘
                  │                  │                  │
                  │                  │                  │
         ┌────────┴────────┬────────┴────────┬────────┴────────┐
         │                 │                 │                 │
         ▼                 ▼                 ▼                 ▼
   ┌───────────┐    ┌───────────┐    ┌───────────┐    ┌───────────┐
   │  CAN Bus  │    │   MQTT    │    │  Secure   │    │  Sensor   │
   │  Driver   │    │  Client   │    │   Boot    │    │ Acquire   │
   └───────────┘    └───────────┘    └───────────┘    └───────────┘
         │                 │                 │                 │
         ▼                 ▼                 ▼                 ▼
   ┌───────────┐    ┌───────────┐    ┌───────────┐    ┌───────────┐
   │  Driver   │    │  Network  │    │  Flash    │    │ ADC/SPI   │
   │ Registry  │    │   HAL     │    │   HAL     │    │   HAL     │
   └───────────┘    └───────────┘    └───────────┘    └───────────┘
```

## Module Descriptions

### Core Layer

| Module | Header | Source | Description |
|--------|--------|--------|-------------|
| **Scheduler** | `core/scheduler.h` | `core/scheduler.c` | Priority-based cooperative task scheduler |
| **Message Bus** | `core/message_bus.h` | `core/message_bus.c` | Pub/sub inter-module communication |
| **Error Handler** | `core/error_handler.h` | `core/error_handler.c` | Centralized error management |
| **Driver Registry** | `core/driver_registry.h` | `core/driver_registry.c` | Hardware driver abstraction |
| **Logging** | `core/logging.h` | `core/logging.c` | Logging with fault injection |

### Automotive Layer

| Module | Header | Source | Description |
|--------|--------|--------|-------------|
| **CAN Bus** | `automotive/can_bus.h` | `automotive/can_bus.c` | CAN 2.0B driver (SPOTLIGHT) |
| **Watchdog** | `automotive/watchdog.h` | `automotive/watchdog.c` | Hardware watchdog interface |

### IoT Layer

| Module | Header | Source | Description |
|--------|--------|--------|-------------|
| **MQTT Client** | `iot/mqtt_client.h` | `iot/mqtt_client.c` | MQTT protocol handler |
| **OTA Update** | `iot/ota_update.h` | `iot/ota_update.c` | Over-the-air firmware updates |

### Security Layer

| Module | Header | Source | Description |
|--------|--------|--------|-------------|
| **Secure Boot** | `security/secure_boot.h` | `security/secure_boot.c` | Secure bootloader (SPOTLIGHT) |
| **Firmware Signing** | `security/firmware_signing.h` | `security/firmware_signing.c` | Image signing utilities |

### Consumer Layer

| Module | Header | Source | Description |
|--------|--------|--------|-------------|
| **USB HID** | `consumer/usb_hid.h` | `consumer/usb_hid.c` | USB Human Interface Device |
| **Display Driver** | `consumer/display_driver.h` | `consumer/display_driver.c` | Display controller |

### Medical Layer

| Module | Header | Source | Description |
|--------|--------|--------|-------------|
| **Sensor Acquisition** | `medical/sensor_acquisition.h` | `medical/sensor_acquisition.c` | Multi-channel sensor interface |
| **Calibration** | `medical/calibration.h` | `medical/calibration.c` | Calibration management |

## Dependency Rules

### Layer Dependencies

1. **Application** can depend on any layer
2. **Domain Layers** (Automotive, IoT, etc.) depend on Core
3. **Core** depends only on HAL abstractions
4. **HAL** is platform-specific, no upward dependencies

### Module Dependencies

```
core/scheduler.h
└── Dependencies: <stdint.h>, <stdbool.h>

core/message_bus.h
└── Dependencies: <stdint.h>, <stdbool.h>

core/error_handler.h
└── Dependencies: <stdint.h>, <stdbool.h>
└── Uses: message_bus (for error notifications)

automotive/can_bus.h
└── Dependencies: <stdint.h>, <stdbool.h>
└── Uses: message_bus, error_handler, driver_registry

security/secure_boot.h
└── Dependencies: <stdint.h>, <stdbool.h>, <stddef.h>
└── Uses: error_handler
└── Provides: crypto primitives (SHA-256, ECDSA)
```

## Initialization Order

Components should be initialized in this order:

```c
int main(void)
{
    /* 1. Low-level hardware */
    hal_init();
    
    /* 2. Core services */
    gf_error_init(NULL);        /* Error handler first */
    gf_msg_init();              /* Message bus */
    gf_sched_init(NULL);        /* Scheduler */
    gf_drv_init();              /* Driver registry */
    
    /* 3. Platform drivers */
    gf_can_init(&can_config);
    gf_mqtt_init(&mqtt_config);
    
    /* 4. Application modules */
    gf_boot_init();             /* Confirm boot success */
    
    /* 5. Start scheduler */
    while (1) {
        gf_sched_run();
        gf_msg_process(0);
    }
}
```

## Message Topics

Standard topics used for inter-module communication:

| Topic | Publisher | Subscriber | Payload |
|-------|-----------|------------|---------|
| `sensor/data` | Sensor Acq | MQTT, CAN | gf_sensor_payload_t |
| `sensor/alarm` | Sensor Acq | Error Handler | gf_error_payload_t |
| `can/rx` | CAN Driver | Application | gf_can_frame_t |
| `can/tx` | Application | CAN Driver | gf_can_frame_t |
| `mqtt/status` | MQTT Client | Application | gf_status_payload_t |
| `ota/progress` | OTA Module | Application | gf_status_payload_t |
| `system/error` | Error Handler | All | gf_error_payload_t |
| `boot/status` | Secure Boot | Application | gf_status_payload_t |

## Build Targets

Each module can be compiled independently or as part of the full system:

```makefile
# Individual modules
$(OBJ_DIR)/core/scheduler.o: src/core/scheduler.c
$(OBJ_DIR)/automotive/can_bus.o: src/automotive/can_bus.c

# Full system
$(TARGET): $(OBJS)
    $(CC) $(LDFLAGS) -o $@ $^
```

## Testing Dependencies

| Test Suite | Tests Module | Dependencies |
|------------|--------------|--------------|
| CAN_Bus_Tests | can_bus.c | error_handler, message_bus |
| Message_Bus_Tests | message_bus.c | (none) |
| Scheduler_Tests | scheduler.c | (none) |
| Error_Handler_Tests | error_handler.c | (none) |
| Pipeline_Tests | Integration | All modules |
| Secure_Boot_Tests | secure_boot.c | error_handler |

---

*Module dependencies follow clean architecture principles - dependencies point inward toward core services*
