# Grey Firmware Networking Stack

## Overview

The Grey Firmware networking stack provides a lightweight, embedded-friendly implementation of core networking protocols. It demonstrates production-quality patterns while remaining portable across different hardware platforms.

## Architecture

```
┌─────────────────────────────────────────────────────────┐
│                    Application Layer                     │
│                   (HTTP Client, MQTT)                    │
├─────────────────────────────────────────────────────────┤
│                     DNS Resolver                         │
│              (Caching, Multiple Servers)                 │
├─────────────────────────────────────────────────────────┤
│                    TCP/IP Stack                          │
│           (BSD Socket API, State Machine)                │
├─────────────────────────────────────────────────────────┤
│                  Network Interface                       │
│              (Ethernet, WiFi, Cellular)                  │
└─────────────────────────────────────────────────────────┘
```

## Components

### TCP/IP Stack (`net/tcp_ip.h`, `net/tcp_ip.c`)

BSD-style socket API for embedded systems (~600 lines).

#### Features

- **Socket Types**: TCP (SOCK_STREAM) and UDP (SOCK_DGRAM)
- **Socket API**: `gf_socket()`, `gf_bind()`, `gf_listen()`, `gf_accept()`, `gf_connect()`, `gf_send()`, `gf_recv()`, `gf_close()`
- **TCP State Machine**: Full RFC 793 state transitions
- **Socket Options**: Timeouts, keep-alive, reuse address, no delay
- **Network Utilities**: `gf_htons()`, `gf_ntohs()`, `gf_htonl()`, `gf_ntohl()`, `gf_inet_aton()`, `gf_inet_ntoa()`

#### TCP State Machine

```
      CLOSED
         │
    ┌────┴────┐
    │ passive │ active
    │  open   │ open
    ▼         ▼
  LISTEN   SYN_SENT
    │         │
    └────┬────┘
         ▼
   SYN_RECEIVED
         │
         ▼
    ESTABLISHED ◄─────────────────┐
         │                        │
    ┌────┴────┐                   │
    │  close  │ recv FIN          │
    ▼         ▼                   │
FIN_WAIT_1  CLOSE_WAIT            │
    │         │                   │
    ▼         ▼                   │
FIN_WAIT_2  LAST_ACK              │
    │         │                   │
    ▼         │                   │
TIME_WAIT ────┴───────────────────┘
    │
    ▼
  CLOSED
```

#### Usage Example

```c
#include "net/tcp_ip.h"

// Initialize the network stack
gf_net_config_t config = GF_NET_CONFIG_DEFAULT;
gf_net_init(&config);

// Create a TCP socket
int sock = gf_socket(GF_AF_INET, GF_SOCK_STREAM, 0);

// Connect to a server
gf_sockaddr_in_t addr = {
    .family = GF_AF_INET,
    .port = gf_htons(80),
    .addr = {{192, 168, 1, 100}}
};
gf_connect(sock, (gf_sockaddr_t*)&addr, sizeof(addr));

// Send data
const char* request = "GET / HTTP/1.1\r\nHost: example.com\r\n\r\n";
gf_send(sock, request, strlen(request), 0);

// Receive response
char buffer[1024];
ssize_t received = gf_recv(sock, buffer, sizeof(buffer), 0);

// Close socket
gf_close(sock);
```

### DNS Resolver (`net/dns.h`, `net/dns.c`)

DNS resolution with caching (~400 lines).

#### Features

- **Record Types**: A (IPv4), AAAA (IPv6), CNAME, MX, TXT
- **Caching**: TTL-based cache with configurable size
- **Multiple Servers**: Primary and backup DNS server support
- **Async Resolution**: Non-blocking resolution with callbacks
- **Query Building**: RFC 1035 compliant message construction

#### Configuration

```c
typedef struct {
    gf_ipv4_addr_t primary_server;      // Primary DNS server
    gf_ipv4_addr_t secondary_server;    // Fallback server
    uint32_t cache_size;                // Cache entries (default: 32)
    uint32_t default_ttl;               // Default TTL in seconds
    uint32_t query_timeout_ms;          // Timeout per query
    uint8_t max_retries;                // Retry count
} gf_dns_config_t;
```

#### Usage Example

```c
#include "net/dns.h"

// Initialize DNS resolver
gf_dns_config_t config = {
    .primary_server = {{8, 8, 8, 8}},     // Google DNS
    .secondary_server = {{1, 1, 1, 1}},   // Cloudflare
    .cache_size = 64,
    .default_ttl = 300,
    .query_timeout_ms = 5000,
    .max_retries = 3
};
gf_dns_init(&config);

// Resolve a hostname
gf_ipv4_addr_t resolved_ip;
int result = gf_dns_resolve("api.example.com", &resolved_ip, 5000);

if (result == 0) {
    printf("Resolved to: %u.%u.%u.%u\n",
           resolved_ip.octets[0], resolved_ip.octets[1],
           resolved_ip.octets[2], resolved_ip.octets[3]);
}

// Check cache statistics
gf_dns_cache_stats_t stats;
gf_dns_cache_get_stats(&stats);
printf("Cache hits: %u, misses: %u\n", stats.hits, stats.misses);
```

### HTTP Client (`net/http_client.h`, `net/http_client.c`)

HTTP/1.1 client with retry and timeout logic (~500 lines).

#### Features

- **HTTP Methods**: GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS
- **Retry Logic**: Exponential backoff with configurable limits
- **Timeouts**: Connection, read, and overall request timeouts
- **Redirects**: Configurable redirect following
- **Headers**: Custom header support
- **Connection Pooling**: Socket reuse for performance

#### Configuration

```c
typedef struct {
    uint32_t connect_timeout_ms;    // Connection timeout
    uint32_t read_timeout_ms;       // Read timeout
    uint32_t timeout_ms;            // Overall request timeout
    uint8_t retry_count;            // Number of retries
    uint32_t retry_delay_ms;        // Base retry delay
    bool follow_redirects;          // Follow HTTP redirects
    uint8_t max_redirects;          // Max redirect hops
} gf_http_config_t;
```

#### Retry Strategy

The HTTP client implements exponential backoff:

```
Attempt 1: Immediate
Attempt 2: base_delay * 2^0 = 1000ms
Attempt 3: base_delay * 2^1 = 2000ms
Attempt 4: base_delay * 2^2 = 4000ms
```

#### Usage Example

```c
#include "net/http_client.h"

// Initialize HTTP client
gf_http_config_t config = {
    .connect_timeout_ms = 5000,
    .read_timeout_ms = 10000,
    .timeout_ms = 30000,
    .retry_count = 3,
    .retry_delay_ms = 1000,
    .follow_redirects = true,
    .max_redirects = 5
};
gf_http_client_init(&config);

// Make a GET request
gf_http_response_t response;
int result = gf_http_get("http://api.example.com/data", NULL, &response);

if (result == 0 && response.status_code == 200) {
    printf("Response: %.*s\n", (int)response.body_length, response.body);
}

// Make a POST request with JSON body
gf_http_header_t headers[] = {
    {"Content-Type", "application/json"},
    {"Authorization", "Bearer token123"}
};
const char* body = "{\"sensor_id\": 1, \"value\": 42.5}";
result = gf_http_post("http://api.example.com/telemetry",
                      headers, 2, body, strlen(body), &response);
```

## Message Bus Integration

The networking stack integrates with the Grey Firmware message bus for event-driven architectures:

### Network Topics

| Topic | Description |
|-------|-------------|
| `GF_TOPIC_NET_CONNECTED` | Network interface connected |
| `GF_TOPIC_NET_DISCONNECTED` | Network interface disconnected |
| `GF_TOPIC_NET_PACKET_RX` | Packet received |
| `GF_TOPIC_NET_PACKET_TX` | Packet transmitted |
| `GF_TOPIC_NET_ERROR` | Network error occurred |
| `GF_TOPIC_DNS_RESOLVED` | DNS resolution completed |
| `GF_TOPIC_HTTP_RESPONSE` | HTTP response received |
| `GF_TOPIC_SOCKET_EVENT` | Socket state changed |

### Example: Event-Driven HTTP

```c
#include "core/message_bus.h"
#include "net/http_client.h"

void on_http_response(gf_topic_t topic, const void* payload, size_t len)
{
    const gf_http_result_payload_t* result = payload;
    printf("HTTP %s complete: %d\n", result->url, result->status_code);
}

void init_networking(void)
{
    gf_msgbus_subscribe(GF_TOPIC_HTTP_RESPONSE, on_http_response, NULL);
}
```

## Testing

The networking stack includes 31 automated tests:

```bash
make test-net
```

### Test Categories

**TCP/IP Tests (13)**:
- Socket creation for TCP and UDP
- Bind, listen, connect operations
- Socket options configuration
- Network utility functions

**Network Interface Tests (3)**:
- Interface add/remove
- Address configuration
- Default gateway management

**DNS Tests (6)**:
- Direct IP resolution
- Hostname resolution
- Cache operations and TTL
- Server configuration

**HTTP Tests (9)**:
- URL parsing and encoding
- Status code helpers
- Retry delay calculations
- Connection pool management

## Performance Considerations

### Memory Usage

| Component | RAM (bytes) | Flash (bytes) |
|-----------|-------------|---------------|
| TCP/IP Stack | ~8KB | ~15KB |
| DNS Resolver | ~4KB | ~8KB |
| HTTP Client | ~6KB | ~12KB |

### Tuning Parameters

```c
// Reduce memory for constrained devices
#define GF_MAX_SOCKETS       8      // Default: 16
#define GF_DNS_CACHE_SIZE    16     // Default: 32
#define GF_HTTP_POOL_SIZE    2      // Default: 4
```

## Error Handling

All networking functions return standard error codes:

| Code | Meaning |
|------|---------|
| 0 | Success |
| -1 | Invalid parameter |
| -2 | Resource unavailable |
| -3 | Timeout |
| -4 | Connection refused |
| -5 | Host unreachable |
| -6 | Network down |

## Future Enhancements

Planned additions for future phases:

- **TLS/SSL**: Secure connections via mbedTLS integration
- **WebSocket**: Full-duplex communication
- **mDNS**: Zero-configuration networking
- **SNTP**: Precise time synchronization
- **IPv6**: Full IPv6 stack support

---

*See [overview.md](overview.md) for complete project documentation.*
