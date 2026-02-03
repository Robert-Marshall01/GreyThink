# Smart Home Stack - Zigbee/Matter Spotlight

## Overview

The Grey Firmware Smart Home Stack implements a production-ready Zigbee 3.0 and Matter protocol handler for embedded IoT devices. This spotlight demonstrates protocol-level device management, secure mesh networking, and home automation capabilities in resource-constrained environments.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Application Layer                             │
├─────────────────────────────────────────────────────────────────┤
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────────────┐ │
│  │  Scene   │  │  Group   │  │ Device   │  │    Discovery     │ │
│  │ Manager  │  │ Manager  │  │ Registry │  │     Engine       │ │
│  └────┬─────┘  └────┬─────┘  └────┬─────┘  └────────┬─────────┘ │
│       │             │             │                  │           │
├───────┴─────────────┴─────────────┴──────────────────┴───────────┤
│                    ZCL Command Handler                           │
│        ┌─────────┬─────────┬─────────┬──────────┐               │
│        │ On/Off  │  Level  │  Color  │ Identify │               │
│        │ Cluster │ Cluster │ Cluster │ Cluster  │               │
│        └────┬────┴────┬────┴────┬────┴────┬─────┘               │
├─────────────┴─────────┴─────────┴─────────┴─────────────────────┤
│                    Network Layer                                 │
│   ┌────────────────┐  ┌────────────────┐  ┌─────────────────┐   │
│   │  Coordinator   │  │  Join Control  │  │ Frame Counter   │   │
│   │   (PAN ID)     │  │  (Permit Join) │  │   Security      │   │
│   └────────┬───────┘  └────────┬───────┘  └────────┬────────┘   │
├────────────┴───────────────────┴───────────────────┴────────────┤
│                    Physical Layer (802.15.4)                     │
│              Channel 11-26 (2.4 GHz ISM Band)                    │
└─────────────────────────────────────────────────────────────────┘
```

## Key Features

### Network Formation & Management
- **PAN ID Configuration**: 16-bit network identifier
- **Channel Selection**: IEEE 802.15.4 channels 11-26
- **Coordinator Mode**: Network formation and management
- **Permit Join Control**: Time-limited device admission

### Device Discovery & Pairing
- **Capacity**: Up to 8 devices per network
- **Addressing**: IEEE 64-bit extended + 16-bit network addresses
- **Commissioning States**: Discovered → Joining → Authenticating → Paired → Configured
- **Automatic Timeout**: 30-second commissioning window

### ZCL Cluster Support

| Cluster | ID | Description |
|---------|------|-------------|
| On/Off | 0x0006 | Binary device control |
| Level | 0x0008 | Brightness/dimming 0-254 |
| Color | 0x0300 | Hue (0-254), Saturation (0-254) |
| Identify | 0x0003 | Device identification |

### Scene Management
- **Capacity**: 8 scenes × 8 devices per scene
- **Scene Data**: Device, cluster, command, parameters
- **Recall**: Single command activates all scene devices

### Group Management
- **Capacity**: 8 groups × 4 devices per group
- **Multicast**: Single command to all group members
- **ZCL Forwarding**: Commands relayed to matching devices

### Security
- **Network Key**: 128-bit AES encryption key
- **Frame Counter**: Monotonic counter prevents replay attacks
- **Key Rotation**: Automatic re-keying support
- **Counter Validation**: 256K window for out-of-order frames

## API Reference

### Network Management

```c
// Initialize with PAN ID and channel
gf_error_t gf_smarthome_init(uint16_t pan_id, uint8_t channel);

// Start network formation
gf_error_t gf_smarthome_network_start(void);

// Control device admission
gf_error_t gf_smarthome_permit_join(uint8_t duration_sec);

// Set network security key
gf_error_t gf_smarthome_set_network_key(const uint8_t key[16]);

// Shutdown network
gf_error_t gf_smarthome_shutdown(void);
```

### Device Discovery

```c
// Start discovering devices
gf_error_t gf_smarthome_discover_devices(void);

// Commission discovered device by network address
gf_error_t gf_smarthome_commission_device(uint16_t nwk_addr);

// Remove device from network
gf_error_t gf_smarthome_remove_device(uint16_t nwk_addr);

// Get device count
size_t gf_smarthome_get_device_count(void);

// Get network statistics
gf_smarthome_stats_t gf_smarthome_get_stats(void);
```

### ZCL Commands

```c
// Send command to device cluster
gf_error_t gf_smarthome_send_zcl_command(
    uint16_t device_addr,
    uint16_t cluster,
    uint8_t command,
    const uint8_t *params,
    size_t param_len
);

// Convenience wrappers
gf_error_t gf_smarthome_device_on(uint16_t addr);
gf_error_t gf_smarthome_device_off(uint16_t addr);
gf_error_t gf_smarthome_device_toggle(uint16_t addr);
gf_error_t gf_smarthome_device_set_level(uint16_t addr, uint8_t level);
gf_error_t gf_smarthome_device_set_color(uint16_t addr, uint8_t hue, uint8_t sat);
```

### Scene Management

```c
// Create scene with ID and name
gf_error_t gf_smarthome_create_scene(uint8_t scene_id, const char *name);

// Add device action to scene
gf_error_t gf_smarthome_scene_add_device(
    uint8_t scene_id,
    uint16_t device_addr,
    uint16_t cluster,
    uint8_t command,
    const uint8_t *params,
    size_t param_len
);

// Recall (activate) scene
gf_error_t gf_smarthome_recall_scene(uint8_t scene_id);

// Delete scene
gf_error_t gf_smarthome_delete_scene(uint8_t scene_id);
```

### Group Management

```c
// Create device group
gf_error_t gf_smarthome_create_group(uint16_t group_id, const char *name);

// Add device to group
gf_error_t gf_smarthome_group_add_device(uint16_t group_id, uint16_t device_addr);

// Remove device from group
gf_error_t gf_smarthome_group_remove_device(uint16_t group_id, uint16_t device_addr);

// Send command to all devices in group
gf_error_t gf_smarthome_group_send_zcl(
    uint16_t group_id,
    uint16_t cluster,
    uint8_t command,
    const uint8_t *params,
    size_t param_len
);
```

## Message Bus Integration

The Smart Home stack integrates with the Grey Firmware message bus for event-driven architecture:

### Topics

| Topic | Description |
|-------|-------------|
| `GF_TOPIC_SMARTHOME_CMD` | Device commands (on/off/level/color) |
| `GF_TOPIC_SMARTHOME_EVENT` | Device events (motion, contact, etc.) |
| `GF_TOPIC_SMARTHOME_DISCOVERY` | Device discovery notifications |
| `GF_TOPIC_SMARTHOME_PAIR` | Pairing status updates |
| `GF_TOPIC_SMARTHOME_SCENE` | Scene recall events |

### Payload Types

```c
// Command payload
typedef struct {
    uint16_t device_id;
    uint16_t cluster;
    uint8_t command;
    uint8_t params[8];
} gf_smarthome_cmd_payload_t;

// Event payload  
typedef struct {
    uint16_t device;
    uint8_t event_type;    // GF_SMARTHOME_EVENT_*
    uint8_t zone;
    uint32_t trigger;
} gf_smarthome_event_payload_t;
```

### Event Types

| Event | Code | Description |
|-------|------|-------------|
| `GF_SMARTHOME_EVENT_MOTION` | 0x01 | PIR motion detected |
| `GF_SMARTHOME_EVENT_CONTACT` | 0x02 | Door/window contact |
| `GF_SMARTHOME_EVENT_TEMP` | 0x03 | Temperature change |
| `GF_SMARTHOME_EVENT_HUMIDITY` | 0x04 | Humidity change |
| `GF_SMARTHOME_EVENT_BUTTON` | 0x05 | Button press |

## Statistics

The stack maintains comprehensive operational statistics:

```c
typedef struct {
    uint32_t devices_discovered;
    uint32_t devices_paired;
    uint32_t commands_sent;
    uint32_t commands_failed;
    uint32_t scenes_recalled;
    uint32_t groups_commanded;
    uint32_t discovery_cycles;
} gf_smarthome_stats_t;
```

## Configuration Constants

| Constant | Value | Description |
|----------|-------|-------------|
| `GF_SH_MAX_DEVICES` | 8 | Maximum devices per network |
| `GF_SH_MAX_SCENES` | 8 | Maximum scenes |
| `GF_SH_MAX_GROUPS` | 8 | Maximum groups |
| `GF_SH_MAX_SCENE_DEVICES` | 8 | Devices per scene |
| `GF_SH_MAX_GROUP_DEVICES` | 4 | Devices per group |
| `GF_SH_MAX_NAME_LEN` | 32 | Maximum name length |
| `GF_SH_CHANNEL_MIN` | 11 | Minimum 802.15.4 channel |
| `GF_SH_CHANNEL_MAX` | 26 | Maximum 802.15.4 channel |

## Test Coverage

The Smart Home stack includes 22 test cases with 54 assertions:

| Test Group | Tests | Description |
|------------|-------|-------------|
| Network Formation | 3 | Init, start, permit join |
| Device Discovery | 3 | Discover, count, capacity |
| Device Commissioning | 2 | Commission flow, timeout |
| ZCL Commands | 4 | On/Off, Level, Color, Toggle |
| Scene Management | 3 | Create, add devices, recall |
| Group Management | 3 | Create, add devices, multicast |
| Security | 2 | Key setting, frame validation |
| Integration | 2 | Full workflows |

Run tests with:
```bash
make test-smarthome
```

## Example Usage

### Basic Device Control

```c
#include "smarthome/smarthome_spotlight.h"

int main(void) {
    // Initialize network on channel 11
    gf_smarthome_init(0x1234, 11);
    gf_smarthome_network_start();
    
    // Set security key
    uint8_t key[16] = {0x01, 0x02, /* ... */ 0x10};
    gf_smarthome_set_network_key(key);
    
    // Allow devices to join for 60 seconds
    gf_smarthome_permit_join(60);
    
    // Discover and commission devices
    gf_smarthome_discover_devices();
    gf_smarthome_commission_device(0x0001);
    
    // Control devices
    gf_smarthome_device_on(0x0001);
    gf_smarthome_device_set_level(0x0001, 128);
    gf_smarthome_device_set_color(0x0001, 128, 200);
    
    return 0;
}
```

### Scene Automation

```c
// Create "Movie Mode" scene
gf_smarthome_create_scene(1, "Movie Mode");

// Add devices with specific states
uint8_t dim_params[] = {25, 0, 0};  // 10% brightness
gf_smarthome_scene_add_device(1, 0x0001, 0x0008, 0x04, dim_params, 3);

uint8_t off_cmd = 0x00;
gf_smarthome_scene_add_device(1, 0x0002, 0x0006, 0x00, &off_cmd, 1);

// Later: recall scene with one command
gf_smarthome_recall_scene(1);
```

### Group Control

```c
// Create living room group
gf_smarthome_create_group(1, "Living Room");
gf_smarthome_group_add_device(1, 0x0001);
gf_smarthome_group_add_device(1, 0x0002);
gf_smarthome_group_add_device(1, 0x0003);

// Control all lights at once
uint8_t level = 200;
gf_smarthome_group_send_zcl(1, 0x0008, 0x04, &level, 1);
```

## Matter Compatibility

The implementation follows ZCL patterns compatible with Matter (formerly Project CHIP):

- **Data Model**: Clusters map to Matter device types
- **Command Format**: ZCL commands align with Matter interaction model
- **Security**: Frame counter approach mirrors Matter message counters
- **Commissioning**: State machine adapts to Matter's commissioning flow

## Memory Footprint

| Component | RAM | Flash |
|-----------|-----|-------|
| Network State | ~64 bytes | - |
| Device Table | ~256 bytes | - |
| Scene Table | ~512 bytes | - |
| Group Table | ~128 bytes | - |
| Code | - | ~4KB |
| **Total** | **~960 bytes** | **~4KB** |

## See Also

- [Overview](overview.md) - Project overview and architecture
- [Message Bus](../include/core/message_bus.h) - Event system integration
- [Scheduler](../include/core/scheduler.h) - Task scheduling
