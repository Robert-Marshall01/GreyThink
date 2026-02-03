# Grey Firmware

**A Modular Firmware Framework Demonstration**

⚠️ WARNING: Grey Firmware is a compilation of firmware demos, not a production-ready product. Because these modules are designed as learning artifacts without access to full hardware environments, stability and completeness are not guaranteed.

Grey Firmware is a comprehensive embedded systems project showcasing deep firmware expertise across IoT, automotive, medical devices, consumer electronics, and security domains. The architecture demonstrates production-grade design patterns with **two spotlight implementations** that show full protocol-level detail.

---

## Architecture Overview

```
┌────────────────────────────────────────────────────────────────────┐
│                         APPLICATION LAYER                          │
│                   ┌─────────────────────────┐                      │
│                   │   Integration Demo      │                      │
│                   │   Sensor→CAN→MQTT→Cloud │                      │
│                   └─────────────────────────┘                      │
├────────────────────────────────────────────────────────────────────┤
│                        DOMAIN MODULES                              │
│  ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐ ┌──────────┐ │
│  │   IoT    │ │Automotive│ │ Consumer │ │ Medical  │ │ Security │ │
│  ├──────────┤ ├──────────┤ ├──────────┤ ├──────────┤ ├──────────┤ │
│  │ MQTT     │ │ CAN ★    │ │ USB HID  │ │ Sensors  │ │SecureBoot★│
│  │ OTA      │ │ Watchdog │ │ Display  │ │ Calibr.  │ │ Signing  │ │
│  └──────────┘ └──────────┘ └──────────┘ └──────────┘ └──────────┘ │
├────────────────────────────────────────────────────────────────────┤
│                        CORE FRAMEWORK                              │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐ ┌────────────┐│
│  │  Scheduler   │ │ Driver Reg.  │ │ Message Bus  │ │Error Handle││
│  │  Priority    │ │     HAL      │ │  Pub/Sub     │ │  Watchdog  ││
│  │  Deadline    │ │  Lifecycle   │ │   Events     │ │   Logging  ││
│  └──────────────┘ └──────────────┘ └──────────────┘ └────────────┘│
├────────────────────────────────────────────────────────────────────┤
│                         HARDWARE                                   │
│        MCU │ CAN Controller │ USB │ ADC │ Flash │ Crypto          │
└────────────────────────────────────────────────────────────────────┘

★ = Spotlight: Production-grade implementation
```

---

## Directory Structure

```
Grey Firmware/
├── include/                    # Header files
│   ├── grey_firmware.h        # Master header
│   ├── core/                  # Core framework
│   │   ├── scheduler.h        # RTOS-style scheduler
│   │   ├── driver_registry.h  # Dynamic HAL
│   │   ├── message_bus.h      # Pub/sub messaging
│   │   └── error_handler.h    # Error management
│   ├── iot/                   # IoT domain
│   │   ├── mqtt_client.h      # MQTT 3.1.1 client
│   │   └── ota_update.h       # A/B OTA updates
│   ├── automotive/            # Automotive domain
│   │   ├── can_bus.h          # ★ CAN 2.0B driver
│   │   └── watchdog.h         # Window watchdog
│   ├── consumer/              # Consumer devices
│   │   ├── usb_hid.h          # USB HID class
│   │   └── display_driver.h   # Display abstraction
│   ├── medical/               # Medical devices
│   │   ├── sensor_acquisition.h  # 16-bit ADC
│   │   └── calibration.h      # Calibration system
│   ├── security/              # Security domain
│   │   ├── secure_boot.h      # ★ Secure bootloader
│   │   └── firmware_signing.h # Firmware signing
│   └── demo/                  # Demo application
│       └── demo.h
├── src/                       # Implementation files
│   ├── main.c                 # Entry point
│   ├── core/                  # Core implementations
│   ├── iot/                   # IoT implementations
│   ├── automotive/            # Automotive implementations
│   ├── consumer/              # Consumer implementations
│   ├── medical/               # Medical implementations
│   ├── security/              # Security implementations
│   └── demo/                  # Demo implementation
├── Makefile                   # GNU Make build
├── CMakeLists.txt            # CMake build
└── README.md                  # This file
```

---

## Spotlight Implementations

### 1. CAN Bus Driver (`automotive/can_bus.c`)

**~650 lines** of production-grade CAN 2.0B implementation:

- **Protocol Compliance**: Standard (11-bit) and extended (29-bit) identifiers
- **Hardware Abstraction**: 8-channel support with configurable baud rates
- **Error Handling**: TEC/REC counters, bus-off detection, automatic recovery
- **Acceptance Filtering**: Hardware filter configuration with mask/match
- **Callback Architecture**: Async frame reception with priority queuing
- **Timestamp Support**: Microsecond-precision frame timing

```c
/* Example: Initialize CAN at 500 kbit/s with filtering */
gf_can_config_t config = {
    .baud_rate = GF_CAN_BAUD_500KBIT,
    .auto_retransmit = true,
    .bus_off_recovery_ms = 1000
};
gf_can_init(&config);

gf_can_filter_t filter = {
    .id = 0x100,
    .mask = 0x7F0,  /* Match 0x100-0x10F */
    .extended = false
};
gf_can_set_filter(&filter);
```

### 2. Secure Bootloader (`security/secure_boot.c`)

**~600 lines** of cryptographic boot verification:

- **ECDSA-P256 Signatures**: Hardware-accelerated verification
- **Chain of Trust**: Boot ROM → Bootloader → Application
- **A/B Partitioning**: Dual-bank update support
- **Anti-Rollback**: Monotonic version counters
- **Secure State Machine**: Boot stages with defined transitions
- **Debug Protection**: Secure/non-secure world separation

```c
/* Example: Verify firmware before execution */
gf_boot_init();

gf_boot_info_t info;
gf_boot_get_info(&info);

if (info.boot_confirmed) {
    /* Application verified and confirmed */
    gf_boot_mark_slot_confirmed(info.active_slot);
}
```

---

## Building

### Prerequisites

- GCC or Clang (C11 support)
- GNU Make or CMake 3.13+

### Using Make

```bash
# Debug build (default)
make

# Release build
make release

# Clean
make clean

# Cross-compile for ARM
make CROSS_COMPILE=arm-none-eabi-
```

### Using CMake

```bash
mkdir build && cd build
cmake ..
cmake --build .

# Release build
cmake -DCMAKE_BUILD_TYPE=Release ..
cmake --build .
```

---

## Running the Demo

```bash
./build/bin/grey_firmware
```

Output:
```
╔════════════════════════════════════════════════════════════╗
║                     GREY FIRMWARE v1.0.0                    ║
╠════════════════════════════════════════════════════════════╣
║  Modular Firmware Framework Demonstration                  ║
║                                                            ║
║  Modules:                                                  ║
║    [CORE] Scheduler, Driver Registry, Message Bus          ║
║    [IOT]  MQTT Client, OTA Updates                         ║
║    [AUTO] CAN 2.0B Driver*, Watchdog                       ║
║    [MED]  Sensor Acquisition, Calibration                  ║
║    [SEC]  Secure Bootloader*, Firmware Signing             ║
╚════════════════════════════════════════════════════════════╝

[1/4] Verifying secure boot...
      Boot slot: A
      Version:   1.0.0
      Confirmed: YES

[2/4] Initializing demo...
      Scheduler:  initialized
      Message Bus: initialized
      CAN Bus:     500 kbit/s
      MQTT:        localhost:1883

[3/4] Starting demo...
      Demo running: Sensor → CAN → MQTT

[4/4] Entering main loop...
  [100] Samples: 100, CAN: 100, MQTT: 100, Errors: 0
  [200] Samples: 200, CAN: 200, MQTT: 200, Errors: 0
  ...
```

---

## Integration Demo Data Flow

```
┌─────────────┐    Message     ┌─────────────┐    CAN      ┌─────────────┐
│   Sensor    │──────Bus──────▶│CAN Gateway  │────Frame───▶│  CAN Bus    │
│  Task       │   SENSOR_DATA  │  Callback   │             │  Hardware   │
└─────────────┘                └──────┬──────┘             └─────────────┘
                                      │
                                      │ Message Bus
                                      │ CAN_TX
                                      ▼
                               ┌─────────────┐   TCP/IP    ┌─────────────┐
                               │MQTT Gateway │────────────▶│   Cloud     │
                               │  Callback   │    JSON     │  Broker     │
                               └─────────────┘             └─────────────┘
```

---

## Core Framework Details

### Scheduler

- **Cooperative multitasking** with priority levels (0-3)
- **Period/deadline** support for real-time tasks
- **Up to 32 tasks** with configurable stack sizes
- **Statistics tracking**: execution time, deadline misses

### Driver Registry

- **Dynamic HAL** with runtime driver registration
- **Lifecycle management**: init/deinit/suspend/resume
- **Dependency resolution** for driver ordering
- **Up to 32 registered drivers**

### Message Bus

- **Publish/subscribe** pattern for loose coupling
- **16 topics** with 8 subscribers each
- **32-message queue** with priority levels
- **QoS levels**: fire-and-forget, at-least-once

### Error Handler

- **256-entry** error history ring buffer
- **Severity classification**: debug, info, warning, error, critical
- **Watchdog integration** for fault recovery
- **Callback hooks** for custom error handling

---

## Design Philosophy

1. **Modularity**: Each domain is self-contained with minimal dependencies
2. **Portability**: Platform-agnostic core with HAL abstraction
3. **Safety**: Defensive programming with comprehensive error handling
4. **Testability**: Clean interfaces for unit testing
5. **Documentation**: Every function documented with purpose and usage

---

## License

This is a demonstration project showcasing firmware development expertise.

---

## Author

Grey Firmware demonstrates comprehensive embedded systems knowledge across:
- Real-time operating system concepts
- Industrial communication protocols (CAN, MQTT)
- Safety-critical systems (medical, automotive)
- Security (secure boot, cryptographic verification)
- Consumer electronics (USB, displays)
