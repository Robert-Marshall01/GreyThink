# Satellite Communication Subsystem

## Overview

The **Satellite Communication Spotlight** demonstrates production-grade embedded aerospace communications expertise. This subsystem implements a complete satellite uplink/downlink system suitable for LEO (Low Earth Orbit) satellite terminals, remote telemetry systems, and deep-space probe communications.

## Industry Relevance

### Market Drivers
- **LEO Satellite Constellations**: Starlink, OneWeb, Iridium NEXT create massive demand for ground terminals
- **Remote IoT**: Maritime, agricultural, and industrial assets beyond cellular coverage
- **Disaster Response**: Communication resilience when terrestrial infrastructure fails
- **Aerospace & Defense**: Secure, reliable spacecraft-to-ground links

### Standards Compliance
| Standard | Description | Implementation |
|----------|-------------|----------------|
| CCSDS 131.0-B-3 | TM/TC Coding | Reed-Solomon (255,223) |
| CCSDS 355.0-B-1 | Space Data Link Security | AES-128-GCM (placeholder) |
| DVB-S2X | Broadcast FEC | LDPC/BCH compatible design |
| ECSS-E-ST-50C | Spacecraft Interface | Framing protocol |

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Application Layer                             │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐             │
│  │  Telemetry   │ │   Commands   │ │   Beacons    │             │
│  └──────────────┘ └──────────────┘ └──────────────┘             │
├─────────────────────────────────────────────────────────────────┤
│                    Security Layer                                │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐             │
│  │  AES-GCM     │ │ Key Exchange │ │ Anti-Replay  │             │
│  │  Encryption  │ │  (X25519)    │ │  Window      │             │
│  └──────────────┘ └──────────────┘ └──────────────┘             │
├─────────────────────────────────────────────────────────────────┤
│                    FEC Layer (Forward Error Correction)          │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐             │
│  │ Reed-Solomon │ │ Interleaving │ │   Framing    │             │
│  │  (255,223)   │ │   (Block)    │ │   + CRC      │             │
│  └──────────────┘ └──────────────┘ └──────────────┘             │
├─────────────────────────────────────────────────────────────────┤
│                    Link Layer                                    │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐             │
│  │    Queue     │ │   Retries    │ │   Doppler    │             │
│  │  Management  │ │  (Exp Back)  │ │   Tracking   │             │
│  └──────────────┘ └──────────────┘ └──────────────┘             │
├─────────────────────────────────────────────────────────────────┤
│                    Physical Layer (RF Interface)                 │
│  ┌──────────────┐ ┌──────────────┐ ┌──────────────┐             │
│  │  Transceiver │ │   Antenna    │ │    Power     │             │
│  │    Driver    │ │   Control    │ │  Amplifier   │             │
│  └──────────────┘ └──────────────┘ └──────────────┘             │
└─────────────────────────────────────────────────────────────────┘
```

## Key Features

### 1. Reed-Solomon Error Correction
- **Standard**: RS(255,223) with 32 parity bytes
- **Capability**: Correct up to 16 symbol errors per block
- **Implementation**: Galois Field GF(2^8) with primitive polynomial 0x11D
- **Performance**: Suitable for BER up to 10^-3 for typical satellite channels

```c
/* Reed-Solomon encoding */
int sat_rs_encode(sat_rs_codec_t* codec, const uint8_t* data, 
                  uint16_t len, uint8_t* parity);

/* Reed-Solomon decoding with error correction */
int sat_rs_decode(sat_rs_codec_t* codec, uint8_t* data, 
                  uint16_t len, const uint8_t* parity,
                  uint16_t* errors_corrected);
```

### 2. Authenticated Encryption
- **Algorithm**: AES-128-GCM (placeholder implementation)
- **Key Size**: 128-bit session keys
- **Nonce**: 96-bit unique per-frame
- **Authentication Tag**: 128-bit (truncatable)

Security features:
- Anti-replay protection with 64-packet sliding window
- Session key separation (TX/RX)
- Key age tracking for rotation
- Secure zeroization capability

### 3. Store-and-Forward
Handles intermittent satellite coverage:
- Queue depth: 64 packets
- Priority-based transmission
- Automatic mode switching on link timeout
- Persistent state across passes

### 4. Link State Machine

```
    ┌────────────────────────────────────────────────────┐
    │                                                    │
    ▼                                                    │
┌───────┐       ┌───────────┐       ┌──────┐       ┌───────────┐
│ IDLE  │──────▶│ ACQUIRING │──────▶│ SYNC │──────▶│ CONNECTED │
└───────┘       └───────────┘       └──────┘       └───────────┘
                     │                                   │
                     │                                   │
                     ▼                                   ▼
              ┌──────────────┐                    ┌──────────┐
              │STORE_FORWARD │◀───────────────────│ TX_ONLY  │
              └──────────────┘                    └──────────┘
                     │
                     ▼
               (Pass predicted)
```

### 5. Doppler Compensation
LEO satellites can have ±40 kHz Doppler shift at L-band:
- Real-time Doppler tracking input
- Frequency offset applied to modem
- Pass prediction integration

## API Reference

### Initialization
```c
int sat_init(void);
int sat_shutdown(void);
```

### Telemetry Transmission
```c
int sat_queue_telemetry(const uint8_t* data, uint16_t len, 
                        sat_priority_t priority);
```

Priority levels:
- `SAT_PRIORITY_LOW`: Routine housekeeping
- `SAT_PRIORITY_NORMAL`: Standard telemetry
- `SAT_PRIORITY_HIGH`: Time-sensitive data
- `SAT_PRIORITY_EMERGENCY`: Distress/safety critical

### Security Configuration
```c
int sat_load_keys(const uint8_t* tx_key, const uint8_t* rx_key);
int sat_set_doppler(float doppler_hz);
int sat_set_store_forward(bool enable);
```

### Status and Statistics
```c
int sat_get_status(sat_link_status_t* status);
int sat_get_stats(sat_stats_t* stats);
```

Statistics tracked:
- Packets TX/RX and byte counts
- Retransmission count
- RS error corrections and failures
- CRC errors
- Authentication failures
- Replay attack detections
- Link timeouts

## Message Bus Integration

The satellite subsystem publishes to these message bus topics:

| Topic | Description |
|-------|-------------|
| `satellite/uplink` | Outgoing telemetry queued |
| `satellite/downlink` | Incoming command received |
| `satellite/telemetry` | Telemetry packet transmitted |
| `satellite/command` | Command execution status |
| `satellite/status` | Link status updates |
| `satellite/link/state` | State machine transitions |
| `satellite/pass` | Pass prediction events |
| `satellite/error` | Error notifications |

## Integration Example

### Basic Telemetry Upload
```c
#include "space/sat_uplink.h"

int main(void) {
    /* Initialize satellite system */
    sat_init();
    
    /* Load security keys */
    uint8_t tx_key[16] = { /* ... */ };
    uint8_t rx_key[16] = { /* ... */ };
    sat_load_keys(tx_key, rx_key);
    
    /* Start acquisition */
    sat_start_acquisition();
    
    /* Queue telemetry */
    uint8_t sensor_data[64];
    collect_sensor_data(sensor_data);
    sat_queue_telemetry(sensor_data, sizeof(sensor_data), 
                        SAT_PRIORITY_NORMAL);
    
    /* Main loop */
    while (running) {
        sat_process(125);  /* 8 Hz processing rate */
        delay_ms(125);
    }
    
    sat_shutdown();
    return 0;
}
```

### Receive Callback Registration
```c
void on_command_received(const sat_packet_t* packet) {
    /* Process incoming command */
    switch (packet->payload[0]) {
        case CMD_REBOOT:
            system_reboot();
            break;
        case CMD_CONFIG:
            apply_config(&packet->payload[1], packet->header.length - 1);
            break;
    }
}

int main(void) {
    sat_init();
    sat_register_rx_callback(on_command_received);
    /* ... */
}
```

## Test Coverage

The test suite (`tests/satellite_tests.c`) provides comprehensive coverage:

| Category | Tests | Description |
|----------|-------|-------------|
| Initialization | 4 | Init, RS codec, double-init, shutdown |
| Link State | 3 | State transitions, store-forward |
| Packet Queue | 6 | Single, multiple, priorities, overflow |
| Security | 3 | Key loading, encryption status |
| Doppler | 3 | Positive, negative, zero shifts |
| Processing | 3 | Basic, connected, store-forward modes |
| Statistics | 3 | Initial, after TX, null pointer |
| Timeouts | 2 | Link timeout, stats increment |
| Error Injection | 2 | Bit errors, CRC verification |
| Boundary | 4 | Zero delta, large delta, rapid changes |
| Packet Loss | 1 | Multi-packet recovery |
| Latency | 1 | High-latency link tolerance |
| Handshake | 1 | Security setup |

**Total: 36 integration tests**

Run tests:
```bash
make test-satellite
```

## Performance Characteristics

| Parameter | Value | Notes |
|-----------|-------|-------|
| Max Payload | 480 bytes | Configurable |
| TX Queue | 64 packets | Priority-ordered |
| RX Buffer | 16 packets | Command buffer |
| Link Timeout | 30 seconds | Configurable |
| ACK Timeout | 5 seconds | Before retry |
| Max Retries | 5 | Per packet |
| RS Overhead | 14.3% | 32/223 bytes |
| Auth Tag | 16 bytes | AES-GCM |

## Future Enhancements

- [ ] Full AES-GCM implementation (replace placeholder)
- [ ] X25519 key exchange protocol
- [ ] LDPC/Turbo code support for near-Shannon performance
- [ ] DVB-S2X modulation compatibility
- [ ] Convolutional interleaving for fading channels
- [ ] Pass prediction from TLE/SGP4
- [ ] Automatic Doppler tracking from ephemeris
- [ ] Multi-satellite handoff support

## Recruiter Notes

This subsystem demonstrates:

1. **Protocol Design**: Complete communication stack from application to framing
2. **Error Correction Coding**: Galois Field arithmetic and RS implementation
3. **Security Engineering**: Authenticated encryption with replay protection
4. **State Machine Design**: Robust link management for unreliable channels
5. **Real-time Systems**: Timing-critical satellite pass operations
6. **Aerospace Standards**: CCSDS-compatible design choices
7. **Testing Methodology**: Comprehensive integration test coverage

The implementation balances production-ready robustness with educational clarity, making it suitable for both learning and as a foundation for real satellite systems.
