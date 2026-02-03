/**
 * @file tcp_ip.h
 * @brief Lightweight TCP/IP Stack Interface
 * 
 * INDUSTRY RELEVANCE:
 *   Networking is fundamental to modern embedded systems:
 *   - Industrial IoT: MQTT, Modbus TCP, OPC-UA connectivity
 *   - Automotive: Ethernet AVB/TSN, DoIP diagnostics
 *   - Consumer: WiFi/BLE gateways, smart home devices
 *   - Medical: Remote monitoring, DICOM imaging
 *   - Building Automation: BACnet/IP, cloud connectivity
 * 
 * This implementation provides:
 *   - BSD-style socket abstraction
 *   - TCP and UDP protocol support
 *   - Connection state machine management
 *   - Zero-copy buffer options for performance
 *   - Network interface abstraction
 * 
 * DESIGN PHILOSOPHY:
 *   Inspired by lwIP, uIP, and CycloneTCP but designed for
 *   demonstration of embedded networking concepts with clear,
 *   educational code structure.
 */

#ifndef GF_TCP_IP_H
#define GF_TCP_IP_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Constants and Limits                                                       */
/*===========================================================================*/

#define GF_NET_MAX_SOCKETS          8
#define GF_NET_MAX_INTERFACES       2
#define GF_NET_MAX_DNS_SERVERS      2
#define GF_NET_MTU                  1500
#define GF_NET_TCP_MSS              (GF_NET_MTU - 40)   /* MTU - IP/TCP headers */
#define GF_NET_TCP_WINDOW_SIZE      4096
#define GF_NET_TCP_MAX_RETRIES      5
#define GF_NET_TCP_RETRY_TIMEOUT_MS 1000
#define GF_NET_ARP_CACHE_SIZE       16
#define GF_NET_EPHEMERAL_PORT_START 49152
#define GF_NET_EPHEMERAL_PORT_END   65535

/*===========================================================================*/
/* IP Address Types                                                           */
/*===========================================================================*/

typedef struct {
    uint8_t addr[4];                /* IPv4 address in network byte order */
} gf_ipv4_addr_t;

typedef struct {
    uint8_t addr[16];               /* IPv6 address */
} gf_ipv6_addr_t;

typedef enum {
    GF_NET_AF_INET = 2,             /* IPv4 */
    GF_NET_AF_INET6 = 10            /* IPv6 (future) */
} gf_net_af_t;

/* Common IPv4 addresses */
#define GF_IPV4_ANY         ((gf_ipv4_addr_t){{0, 0, 0, 0}})
#define GF_IPV4_LOOPBACK    ((gf_ipv4_addr_t){{127, 0, 0, 1}})
#define GF_IPV4_BROADCAST   ((gf_ipv4_addr_t){{255, 255, 255, 255}})

/*===========================================================================*/
/* Socket Types                                                               */
/*===========================================================================*/

typedef enum {
    GF_SOCK_STREAM = 1,             /* TCP */
    GF_SOCK_DGRAM = 2,              /* UDP */
    GF_SOCK_RAW = 3                 /* Raw IP */
} gf_sock_type_t;

typedef enum {
    GF_IPPROTO_IP = 0,
    GF_IPPROTO_ICMP = 1,
    GF_IPPROTO_TCP = 6,
    GF_IPPROTO_UDP = 17
} gf_ipproto_t;

typedef int gf_socket_t;
#define GF_INVALID_SOCKET   (-1)

/*===========================================================================*/
/* Socket Address                                                             */
/*===========================================================================*/

typedef struct {
    gf_net_af_t     family;
    uint16_t        port;           /* Port in host byte order */
    gf_ipv4_addr_t  addr;
} gf_sockaddr_in_t;

/*===========================================================================*/
/* TCP State Machine                                                          */
/*===========================================================================*/

typedef enum {
    GF_TCP_STATE_CLOSED = 0,
    GF_TCP_STATE_LISTEN,
    GF_TCP_STATE_SYN_SENT,
    GF_TCP_STATE_SYN_RECEIVED,
    GF_TCP_STATE_ESTABLISHED,
    GF_TCP_STATE_FIN_WAIT_1,
    GF_TCP_STATE_FIN_WAIT_2,
    GF_TCP_STATE_CLOSE_WAIT,
    GF_TCP_STATE_CLOSING,
    GF_TCP_STATE_LAST_ACK,
    GF_TCP_STATE_TIME_WAIT
} gf_tcp_state_t;

/*===========================================================================*/
/* Network Interface                                                          */
/*===========================================================================*/

typedef enum {
    GF_NETIF_TYPE_ETHERNET = 0,
    GF_NETIF_TYPE_WIFI,
    GF_NETIF_TYPE_CELLULAR,
    GF_NETIF_TYPE_LOOPBACK
} gf_netif_type_t;

typedef enum {
    GF_NETIF_FLAG_UP        = (1 << 0),
    GF_NETIF_FLAG_BROADCAST = (1 << 1),
    GF_NETIF_FLAG_LINK_UP   = (1 << 2),
    GF_NETIF_FLAG_DHCP      = (1 << 3),
    GF_NETIF_FLAG_DEFAULT   = (1 << 4)
} gf_netif_flags_t;

typedef struct {
    uint8_t             mac[6];
    gf_ipv4_addr_t      ip;
    gf_ipv4_addr_t      netmask;
    gf_ipv4_addr_t      gateway;
    gf_ipv4_addr_t      dns_primary;
    gf_ipv4_addr_t      dns_secondary;
} gf_netif_addr_t;

typedef struct gf_netif gf_netif_t;

/* Network interface output function (provided by hardware driver) */
typedef int (*gf_netif_output_fn)(gf_netif_t *netif, const void *data, size_t len);

/* Network interface link input function */
typedef int (*gf_netif_input_fn)(gf_netif_t *netif, void *data, size_t maxlen);

struct gf_netif {
    char                name[8];
    gf_netif_type_t     type;
    uint16_t            mtu;
    uint32_t            flags;
    gf_netif_addr_t     addr;
    gf_netif_output_fn  output;
    gf_netif_input_fn   input;
    void               *driver_data;
    
    /* Statistics */
    uint32_t            rx_packets;
    uint32_t            tx_packets;
    uint32_t            rx_bytes;
    uint32_t            tx_bytes;
    uint32_t            rx_errors;
    uint32_t            tx_errors;
};

/*===========================================================================*/
/* Packet Buffer (pbuf)                                                       */
/*===========================================================================*/

typedef enum {
    GF_PBUF_RAM = 0,                /* Allocated from heap */
    GF_PBUF_ROM,                    /* Static/const data */
    GF_PBUF_REF,                    /* Reference to external buffer */
    GF_PBUF_POOL                    /* Allocated from pool */
} gf_pbuf_type_t;

typedef struct gf_pbuf {
    struct gf_pbuf     *next;       /* Chain pointer */
    void               *payload;    /* Data pointer */
    uint16_t            tot_len;    /* Total length of chain */
    uint16_t            len;        /* Length of this buffer */
    gf_pbuf_type_t      type;
    uint8_t             ref;        /* Reference count */
    uint8_t             flags;
} gf_pbuf_t;

/*===========================================================================*/
/* Socket Options                                                             */
/*===========================================================================*/

typedef enum {
    GF_SO_REUSEADDR     = 1,
    GF_SO_KEEPALIVE     = 2,
    GF_SO_BROADCAST     = 3,
    GF_SO_RCVBUF        = 4,
    GF_SO_SNDBUF        = 5,
    GF_SO_RCVTIMEO      = 6,
    GF_SO_SNDTIMEO      = 7,
    GF_SO_LINGER        = 8,
    GF_SO_ERROR         = 9,
    GF_TCP_NODELAY      = 10
} gf_sockopt_t;

/*===========================================================================*/
/* Error Codes                                                                */
/*===========================================================================*/

typedef enum {
    GF_NET_OK = 0,
    GF_NET_ERR_GENERAL = -1,
    GF_NET_ERR_MEMORY = -2,
    GF_NET_ERR_TIMEOUT = -3,
    GF_NET_ERR_CONNREFUSED = -4,
    GF_NET_ERR_CONNRESET = -5,
    GF_NET_ERR_NOTCONN = -6,
    GF_NET_ERR_ALREADY = -7,
    GF_NET_ERR_INPROGRESS = -8,
    GF_NET_ERR_ADDRINUSE = -9,
    GF_NET_ERR_ADDRNOTAVAIL = -10,
    GF_NET_ERR_NETUNREACH = -11,
    GF_NET_ERR_HOSTUNREACH = -12,
    GF_NET_ERR_NOBUFS = -13,
    GF_NET_ERR_WOULDBLOCK = -14,
    GF_NET_ERR_INVAL = -15,
    GF_NET_ERR_ISCONN = -16,
    GF_NET_ERR_PIPE = -17
} gf_net_err_t;

/*===========================================================================*/
/* Stack Configuration                                                        */
/*===========================================================================*/

typedef struct {
    size_t              rx_buffer_size;
    size_t              tx_buffer_size;
    uint32_t            tcp_timeout_ms;
    uint32_t            arp_timeout_ms;
    uint8_t             tcp_max_retries;
    bool                enable_checksum;
    bool                enable_nagle;           /* TCP Nagle algorithm */
} gf_tcp_ip_config_t;

/*===========================================================================*/
/* Stack Initialization                                                       */
/*===========================================================================*/

/**
 * @brief Initialize TCP/IP stack
 * @param config Stack configuration (NULL for defaults)
 * @return GF_NET_OK on success
 */
int gf_net_init(const gf_tcp_ip_config_t *config);

/**
 * @brief Shutdown TCP/IP stack
 */
void gf_net_shutdown(void);

/**
 * @brief Process pending network operations (call periodically)
 */
void gf_net_tick(void);

/*===========================================================================*/
/* Network Interface Management                                               */
/*===========================================================================*/

/**
 * @brief Add network interface
 */
int gf_netif_add(gf_netif_t *netif);

/**
 * @brief Remove network interface
 */
int gf_netif_remove(gf_netif_t *netif);

/**
 * @brief Set default network interface
 */
void gf_netif_set_default(gf_netif_t *netif);

/**
 * @brief Get default network interface
 */
gf_netif_t *gf_netif_get_default(void);

/**
 * @brief Set interface up/down
 */
void gf_netif_set_up(gf_netif_t *netif, bool up);

/**
 * @brief Set interface addresses
 */
int gf_netif_set_addr(gf_netif_t *netif, const gf_netif_addr_t *addr);

/**
 * @brief Process received packet
 */
int gf_netif_input(gf_netif_t *netif, gf_pbuf_t *p);

/*===========================================================================*/
/* Socket API (BSD-style)                                                     */
/*===========================================================================*/

/**
 * @brief Create socket
 */
gf_socket_t gf_socket(gf_net_af_t domain, gf_sock_type_t type, gf_ipproto_t protocol);

/**
 * @brief Close socket
 */
int gf_close(gf_socket_t sock);

/**
 * @brief Bind socket to local address
 */
int gf_bind(gf_socket_t sock, const gf_sockaddr_in_t *addr);

/**
 * @brief Listen for connections (TCP)
 */
int gf_listen(gf_socket_t sock, int backlog);

/**
 * @brief Accept incoming connection (TCP)
 */
gf_socket_t gf_accept(gf_socket_t sock, gf_sockaddr_in_t *addr);

/**
 * @brief Connect to remote address
 */
int gf_connect(gf_socket_t sock, const gf_sockaddr_in_t *addr);

/**
 * @brief Send data (TCP)
 */
int gf_send(gf_socket_t sock, const void *data, size_t len, int flags);

/**
 * @brief Receive data (TCP)
 */
int gf_recv(gf_socket_t sock, void *buf, size_t len, int flags);

/**
 * @brief Send datagram (UDP)
 */
int gf_sendto(gf_socket_t sock, const void *data, size_t len, int flags,
              const gf_sockaddr_in_t *dest);

/**
 * @brief Receive datagram (UDP)
 */
int gf_recvfrom(gf_socket_t sock, void *buf, size_t len, int flags,
                gf_sockaddr_in_t *src);

/**
 * @brief Set socket option
 */
int gf_setsockopt(gf_socket_t sock, gf_sockopt_t opt, const void *val, size_t len);

/**
 * @brief Get socket option
 */
int gf_getsockopt(gf_socket_t sock, gf_sockopt_t opt, void *val, size_t *len);

/**
 * @brief Get socket peer address
 */
int gf_getpeername(gf_socket_t sock, gf_sockaddr_in_t *addr);

/**
 * @brief Get socket local address
 */
int gf_getsockname(gf_socket_t sock, gf_sockaddr_in_t *addr);

/**
 * @brief Shutdown socket
 */
int gf_shutdown_socket(gf_socket_t sock, int how);

/*===========================================================================*/
/* Packet Buffer Management                                                   */
/*===========================================================================*/

/**
 * @brief Allocate packet buffer
 */
gf_pbuf_t *gf_pbuf_alloc(gf_pbuf_type_t type, uint16_t size);

/**
 * @brief Free packet buffer
 */
void gf_pbuf_free(gf_pbuf_t *p);

/**
 * @brief Increment reference count
 */
void gf_pbuf_ref(gf_pbuf_t *p);

/**
 * @brief Chain packet buffers
 */
void gf_pbuf_chain(gf_pbuf_t *head, gf_pbuf_t *tail);

/**
 * @brief Copy data to packet buffer
 */
int gf_pbuf_copy_to(gf_pbuf_t *p, const void *data, uint16_t len);

/**
 * @brief Copy data from packet buffer
 */
int gf_pbuf_copy_from(const gf_pbuf_t *p, void *data, uint16_t len);

/*===========================================================================*/
/* Utility Functions                                                          */
/*===========================================================================*/

/**
 * @brief Convert IPv4 address to string
 */
const char *gf_inet_ntoa(gf_ipv4_addr_t addr, char *buf, size_t len);

/**
 * @brief Parse IPv4 address from string
 */
int gf_inet_aton(const char *str, gf_ipv4_addr_t *addr);

/**
 * @brief Host to network byte order (16-bit)
 */
uint16_t gf_htons(uint16_t hostshort);

/**
 * @brief Network to host byte order (16-bit)
 */
uint16_t gf_ntohs(uint16_t netshort);

/**
 * @brief Host to network byte order (32-bit)
 */
uint32_t gf_htonl(uint32_t hostlong);

/**
 * @brief Network to host byte order (32-bit)
 */
uint32_t gf_ntohl(uint32_t netlong);

/**
 * @brief Calculate Internet checksum
 */
uint16_t gf_inet_checksum(const void *data, size_t len);

/*===========================================================================*/
/* TCP State Query                                                            */
/*===========================================================================*/

/**
 * @brief Get TCP connection state
 */
gf_tcp_state_t gf_tcp_get_state(gf_socket_t sock);

/**
 * @brief Get TCP connection statistics
 */
typedef struct {
    gf_tcp_state_t  state;
    uint32_t        bytes_sent;
    uint32_t        bytes_recv;
    uint32_t        retransmits;
    uint32_t        rtt_ms;         /* Smoothed RTT estimate */
    uint16_t        send_window;
    uint16_t        recv_window;
    uint32_t        seq_next;
    uint32_t        ack_next;
} gf_tcp_stats_t;

int gf_tcp_get_stats(gf_socket_t sock, gf_tcp_stats_t *stats);

#endif /* GF_TCP_IP_H */
