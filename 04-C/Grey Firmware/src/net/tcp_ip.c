/**
 * @file tcp_ip.c
 * @brief Lightweight TCP/IP Stack Implementation
 * 
 * This implementation provides a BSD-style socket API for embedded systems.
 * Inspired by lwIP and uIP, designed for educational clarity.
 */

#include "net/tcp_ip.h"
#include "core/error_handler.h"
#include <string.h>
#include <stdio.h>

/*===========================================================================*/
/* Internal Structures                                                        */
/*===========================================================================*/

typedef enum {
    SOCK_STATE_FREE = 0,
    SOCK_STATE_ALLOCATED,
    SOCK_STATE_BOUND,
    SOCK_STATE_LISTENING,
    SOCK_STATE_CONNECTING,
    SOCK_STATE_CONNECTED,
    SOCK_STATE_CLOSING
} sock_state_t;

typedef struct {
    sock_state_t        state;
    gf_sock_type_t      type;
    gf_ipproto_t        protocol;
    gf_sockaddr_in_t    local_addr;
    gf_sockaddr_in_t    remote_addr;
    gf_tcp_state_t      tcp_state;
    
    /* TCP sequence numbers */
    uint32_t            seq_num;
    uint32_t            ack_num;
    uint32_t            seq_next;
    
    /* Buffers */
    uint8_t            *rx_buf;
    uint16_t            rx_head;
    uint16_t            rx_tail;
    uint16_t            rx_size;
    uint8_t            *tx_buf;
    uint16_t            tx_head;
    uint16_t            tx_tail;
    uint16_t            tx_size;
    
    /* Options */
    uint32_t            recv_timeout;
    uint32_t            send_timeout;
    bool                reuse_addr;
    bool                keep_alive;
    bool                no_delay;
    
    /* Statistics */
    uint32_t            bytes_sent;
    uint32_t            bytes_recv;
    uint32_t            retransmits;
    uint32_t            rtt_ms;
    
    /* Backlog for listening sockets */
    int                 backlog;
    int                 pending_count;
} socket_pcb_t;

/*===========================================================================*/
/* Static State                                                               */
/*===========================================================================*/

static gf_tcp_ip_config_t s_config;
static socket_pcb_t s_sockets[GF_NET_MAX_SOCKETS];
static gf_netif_t *s_default_netif;
static gf_netif_t *s_netifs[GF_NET_MAX_INTERFACES];
static uint8_t s_netif_count;
static uint16_t s_next_ephemeral_port = GF_NET_EPHEMERAL_PORT_START;
static bool s_initialized;

/* RX/TX buffer pools */
static uint8_t s_rx_buffer_pool[GF_NET_MAX_SOCKETS][1024];
static uint8_t s_tx_buffer_pool[GF_NET_MAX_SOCKETS][1024];

/*===========================================================================*/
/* Helper Functions                                                           */
/*===========================================================================*/

static socket_pcb_t *get_socket(gf_socket_t sock)
{
    if (sock < 0 || sock >= GF_NET_MAX_SOCKETS) {
        return NULL;
    }
    if (s_sockets[sock].state == SOCK_STATE_FREE) {
        return NULL;
    }
    return &s_sockets[sock];
}

static gf_socket_t allocate_socket(void)
{
    for (int i = 0; i < GF_NET_MAX_SOCKETS; i++) {
        if (s_sockets[i].state == SOCK_STATE_FREE) {
            memset(&s_sockets[i], 0, sizeof(socket_pcb_t));
            s_sockets[i].state = SOCK_STATE_ALLOCATED;
            s_sockets[i].rx_buf = s_rx_buffer_pool[i];
            s_sockets[i].rx_size = sizeof(s_rx_buffer_pool[i]);
            s_sockets[i].tx_buf = s_tx_buffer_pool[i];
            s_sockets[i].tx_size = sizeof(s_tx_buffer_pool[i]);
            return i;
        }
    }
    return GF_INVALID_SOCKET;
}

static uint16_t get_ephemeral_port(void)
{
    uint16_t port = s_next_ephemeral_port++;
    if (s_next_ephemeral_port == 0) {
        s_next_ephemeral_port = GF_NET_EPHEMERAL_PORT_START;
    }
    return port;
}

static bool port_in_use(uint16_t port, gf_sock_type_t type)
{
    for (int i = 0; i < GF_NET_MAX_SOCKETS; i++) {
        if (s_sockets[i].state != SOCK_STATE_FREE &&
            s_sockets[i].type == type &&
            s_sockets[i].local_addr.port == port) {
            return true;
        }
    }
    return false;
}

/*===========================================================================*/
/* Stack Initialization                                                       */
/*===========================================================================*/

int gf_net_init(const gf_tcp_ip_config_t *config)
{
    if (config) {
        memcpy(&s_config, config, sizeof(gf_tcp_ip_config_t));
    } else {
        /* Defaults */
        s_config.rx_buffer_size = 1024;
        s_config.tx_buffer_size = 1024;
        s_config.tcp_timeout_ms = 30000;
        s_config.arp_timeout_ms = 60000;
        s_config.tcp_max_retries = GF_NET_TCP_MAX_RETRIES;
        s_config.enable_checksum = true;
        s_config.enable_nagle = true;
    }
    
    memset(s_sockets, 0, sizeof(s_sockets));
    memset(s_netifs, 0, sizeof(s_netifs));
    s_netif_count = 0;
    s_default_netif = NULL;
    s_next_ephemeral_port = GF_NET_EPHEMERAL_PORT_START;
    s_initialized = true;
    
    return GF_NET_OK;
}

void gf_net_shutdown(void)
{
    /* Close all sockets */
    for (int i = 0; i < GF_NET_MAX_SOCKETS; i++) {
        if (s_sockets[i].state != SOCK_STATE_FREE) {
            gf_close(i);
        }
    }
    
    s_initialized = false;
}

void gf_net_tick(void)
{
    if (!s_initialized) return;
    
    /* Process pending operations for each socket */
    for (int i = 0; i < GF_NET_MAX_SOCKETS; i++) {
        socket_pcb_t *pcb = &s_sockets[i];
        if (pcb->state == SOCK_STATE_FREE) continue;
        
        /* Handle TCP timeouts, retransmissions, etc. */
        /* Stub: actual implementation would check timers */
    }
}

/*===========================================================================*/
/* Network Interface Management                                               */
/*===========================================================================*/

int gf_netif_add(gf_netif_t *netif)
{
    if (!netif || s_netif_count >= GF_NET_MAX_INTERFACES) {
        return GF_NET_ERR_GENERAL;
    }
    
    s_netifs[s_netif_count++] = netif;
    
    /* Set as default if first interface */
    if (!s_default_netif) {
        s_default_netif = netif;
        netif->flags |= GF_NETIF_FLAG_DEFAULT;
    }
    
    return GF_NET_OK;
}

int gf_netif_remove(gf_netif_t *netif)
{
    for (uint8_t i = 0; i < s_netif_count; i++) {
        if (s_netifs[i] == netif) {
            /* Shift remaining interfaces */
            for (uint8_t j = i; j < s_netif_count - 1; j++) {
                s_netifs[j] = s_netifs[j + 1];
            }
            s_netif_count--;
            
            if (s_default_netif == netif) {
                s_default_netif = s_netif_count > 0 ? s_netifs[0] : NULL;
            }
            return GF_NET_OK;
        }
    }
    return GF_NET_ERR_GENERAL;
}

void gf_netif_set_default(gf_netif_t *netif)
{
    if (s_default_netif) {
        s_default_netif->flags &= ~GF_NETIF_FLAG_DEFAULT;
    }
    s_default_netif = netif;
    if (netif) {
        netif->flags |= GF_NETIF_FLAG_DEFAULT;
    }
}

gf_netif_t *gf_netif_get_default(void)
{
    return s_default_netif;
}

void gf_netif_set_up(gf_netif_t *netif, bool up)
{
    if (!netif) return;
    
    if (up) {
        netif->flags |= GF_NETIF_FLAG_UP;
    } else {
        netif->flags &= ~GF_NETIF_FLAG_UP;
    }
}

int gf_netif_set_addr(gf_netif_t *netif, const gf_netif_addr_t *addr)
{
    if (!netif || !addr) {
        return GF_NET_ERR_INVAL;
    }
    
    memcpy(&netif->addr, addr, sizeof(gf_netif_addr_t));
    return GF_NET_OK;
}

int gf_netif_input(gf_netif_t *netif, gf_pbuf_t *p)
{
    if (!netif || !p) {
        return GF_NET_ERR_INVAL;
    }
    
    netif->rx_packets++;
    netif->rx_bytes += p->tot_len;
    
    /* Stub: would parse IP header and route to protocol handler */
    return GF_NET_OK;
}

/*===========================================================================*/
/* Socket API                                                                 */
/*===========================================================================*/

gf_socket_t gf_socket(gf_net_af_t domain, gf_sock_type_t type, gf_ipproto_t protocol)
{
    if (!s_initialized) {
        return GF_INVALID_SOCKET;
    }
    
    if (domain != GF_NET_AF_INET) {
        return GF_INVALID_SOCKET;  /* Only IPv4 supported */
    }
    
    gf_socket_t sock = allocate_socket();
    if (sock == GF_INVALID_SOCKET) {
        return GF_INVALID_SOCKET;
    }
    
    socket_pcb_t *pcb = &s_sockets[sock];
    pcb->type = type;
    pcb->protocol = protocol;
    pcb->local_addr.family = domain;
    pcb->remote_addr.family = domain;
    
    if (type == GF_SOCK_STREAM) {
        pcb->tcp_state = GF_TCP_STATE_CLOSED;
    }
    
    return sock;
}

int gf_close(gf_socket_t sock)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->type == GF_SOCK_STREAM && 
        pcb->tcp_state == GF_TCP_STATE_ESTABLISHED) {
        /* Initiate graceful close */
        pcb->tcp_state = GF_TCP_STATE_FIN_WAIT_1;
        pcb->state = SOCK_STATE_CLOSING;
        /* Stub: would send FIN */
    }
    
    /* Free socket */
    memset(pcb, 0, sizeof(socket_pcb_t));
    return GF_NET_OK;
}

int gf_bind(gf_socket_t sock, const gf_sockaddr_in_t *addr)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !addr) {
        return GF_NET_ERR_INVAL;
    }
    
    /* Check if port in use */
    if (addr->port != 0 && !pcb->reuse_addr && 
        port_in_use(addr->port, pcb->type)) {
        return GF_NET_ERR_ADDRINUSE;
    }
    
    memcpy(&pcb->local_addr, addr, sizeof(gf_sockaddr_in_t));
    if (pcb->local_addr.port == 0) {
        pcb->local_addr.port = get_ephemeral_port();
    }
    
    pcb->state = SOCK_STATE_BOUND;
    return GF_NET_OK;
}

int gf_listen(gf_socket_t sock, int backlog)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || pcb->type != GF_SOCK_STREAM) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->state != SOCK_STATE_BOUND) {
        return GF_NET_ERR_INVAL;
    }
    
    pcb->backlog = backlog > 0 ? backlog : 1;
    pcb->pending_count = 0;
    pcb->tcp_state = GF_TCP_STATE_LISTEN;
    pcb->state = SOCK_STATE_LISTENING;
    
    return GF_NET_OK;
}

gf_socket_t gf_accept(gf_socket_t sock, gf_sockaddr_in_t *addr)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || pcb->state != SOCK_STATE_LISTENING) {
        return GF_INVALID_SOCKET;
    }
    
    (void)addr;  /* Will be filled when connection is accepted */
    
    /* Stub: would wait for incoming connection and create new socket */
    /* For demo, simulate no pending connections */
    return GF_INVALID_SOCKET;
}

int gf_connect(gf_socket_t sock, const gf_sockaddr_in_t *addr)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !addr) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->state == SOCK_STATE_CONNECTED) {
        return GF_NET_ERR_ISCONN;
    }
    
    /* Auto-bind if not bound */
    if (pcb->state == SOCK_STATE_ALLOCATED) {
        gf_sockaddr_in_t local = { .family = GF_NET_AF_INET, .port = 0 };
        gf_bind(sock, &local);
    }
    
    memcpy(&pcb->remote_addr, addr, sizeof(gf_sockaddr_in_t));
    
    if (pcb->type == GF_SOCK_STREAM) {
        /* TCP: send SYN */
        pcb->tcp_state = GF_TCP_STATE_SYN_SENT;
        pcb->state = SOCK_STATE_CONNECTING;
        pcb->seq_num = 0x12345678;  /* Would use random ISN */
        
        /* Stub: would actually send SYN packet and wait for SYN-ACK */
        /* Simulate successful connection for demo */
        pcb->tcp_state = GF_TCP_STATE_ESTABLISHED;
        pcb->state = SOCK_STATE_CONNECTED;
        pcb->ack_num = 0x87654321;
    } else {
        /* UDP: just record remote address */
        pcb->state = SOCK_STATE_CONNECTED;
    }
    
    return GF_NET_OK;
}

int gf_send(gf_socket_t sock, const void *data, size_t len, int flags)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !data) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->type == GF_SOCK_STREAM && 
        pcb->tcp_state != GF_TCP_STATE_ESTABLISHED) {
        return GF_NET_ERR_NOTCONN;
    }
    
    (void)flags;
    
    /* Copy to TX buffer */
    size_t space = pcb->tx_size - pcb->tx_tail;
    size_t to_copy = len < space ? len : space;
    
    memcpy(pcb->tx_buf + pcb->tx_tail, data, to_copy);
    pcb->tx_tail += to_copy;
    pcb->bytes_sent += to_copy;
    
    /* Stub: would trigger actual packet transmission */
    
    return (int)to_copy;
}

int gf_recv(gf_socket_t sock, void *buf, size_t len, int flags)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !buf) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->type == GF_SOCK_STREAM && 
        pcb->tcp_state != GF_TCP_STATE_ESTABLISHED) {
        return GF_NET_ERR_NOTCONN;
    }
    
    (void)flags;
    
    /* Copy from RX buffer */
    size_t available = pcb->rx_tail - pcb->rx_head;
    if (available == 0) {
        return GF_NET_ERR_WOULDBLOCK;
    }
    
    size_t to_copy = len < available ? len : available;
    memcpy(buf, pcb->rx_buf + pcb->rx_head, to_copy);
    pcb->rx_head += to_copy;
    pcb->bytes_recv += to_copy;
    
    /* Compact buffer if mostly empty */
    if (pcb->rx_head > pcb->rx_size / 2) {
        memmove(pcb->rx_buf, pcb->rx_buf + pcb->rx_head, 
                pcb->rx_tail - pcb->rx_head);
        pcb->rx_tail -= pcb->rx_head;
        pcb->rx_head = 0;
    }
    
    return (int)to_copy;
}

int gf_sendto(gf_socket_t sock, const void *data, size_t len, int flags,
              const gf_sockaddr_in_t *dest)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !data || !dest) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->type != GF_SOCK_DGRAM) {
        return GF_NET_ERR_INVAL;
    }
    
    (void)flags;
    
    /* Stub: would construct UDP packet and send */
    pcb->bytes_sent += len;
    
    return (int)len;
}

int gf_recvfrom(gf_socket_t sock, void *buf, size_t len, int flags,
                gf_sockaddr_in_t *src)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !buf) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->type != GF_SOCK_DGRAM) {
        return GF_NET_ERR_INVAL;
    }
    
    (void)flags;
    (void)len;  /* Would use for max receive size */
    
    /* Stub: would receive from RX queue */
    if (src) {
        memset(src, 0, sizeof(gf_sockaddr_in_t));
    }
    
    return GF_NET_ERR_WOULDBLOCK;
}

int gf_setsockopt(gf_socket_t sock, gf_sockopt_t opt, const void *val, size_t len)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !val) {
        return GF_NET_ERR_INVAL;
    }
    
    switch (opt) {
        case GF_SO_REUSEADDR:
            if (len >= sizeof(int)) {
                pcb->reuse_addr = *(const int *)val != 0;
            }
            break;
        case GF_SO_KEEPALIVE:
            if (len >= sizeof(int)) {
                pcb->keep_alive = *(const int *)val != 0;
            }
            break;
        case GF_SO_RCVTIMEO:
            if (len >= sizeof(uint32_t)) {
                pcb->recv_timeout = *(const uint32_t *)val;
            }
            break;
        case GF_SO_SNDTIMEO:
            if (len >= sizeof(uint32_t)) {
                pcb->send_timeout = *(const uint32_t *)val;
            }
            break;
        case GF_TCP_NODELAY:
            if (len >= sizeof(int)) {
                pcb->no_delay = *(const int *)val != 0;
            }
            break;
        default:
            return GF_NET_ERR_INVAL;
    }
    
    return GF_NET_OK;
}

int gf_getsockopt(gf_socket_t sock, gf_sockopt_t opt, void *val, size_t *len)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !val || !len) {
        return GF_NET_ERR_INVAL;
    }
    
    switch (opt) {
        case GF_SO_REUSEADDR:
            if (*len >= sizeof(int)) {
                *(int *)val = pcb->reuse_addr ? 1 : 0;
                *len = sizeof(int);
            }
            break;
        case GF_SO_KEEPALIVE:
            if (*len >= sizeof(int)) {
                *(int *)val = pcb->keep_alive ? 1 : 0;
                *len = sizeof(int);
            }
            break;
        default:
            return GF_NET_ERR_INVAL;
    }
    
    return GF_NET_OK;
}

int gf_getpeername(gf_socket_t sock, gf_sockaddr_in_t *addr)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !addr) {
        return GF_NET_ERR_INVAL;
    }
    
    if (pcb->state != SOCK_STATE_CONNECTED) {
        return GF_NET_ERR_NOTCONN;
    }
    
    memcpy(addr, &pcb->remote_addr, sizeof(gf_sockaddr_in_t));
    return GF_NET_OK;
}

int gf_getsockname(gf_socket_t sock, gf_sockaddr_in_t *addr)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !addr) {
        return GF_NET_ERR_INVAL;
    }
    
    memcpy(addr, &pcb->local_addr, sizeof(gf_sockaddr_in_t));
    return GF_NET_OK;
}

int gf_shutdown_socket(gf_socket_t sock, int how)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb) {
        return GF_NET_ERR_INVAL;
    }
    
    (void)how;
    
    if (pcb->type == GF_SOCK_STREAM && 
        pcb->tcp_state == GF_TCP_STATE_ESTABLISHED) {
        pcb->tcp_state = GF_TCP_STATE_FIN_WAIT_1;
    }
    
    return GF_NET_OK;
}

/*===========================================================================*/
/* Packet Buffer Management                                                   */
/*===========================================================================*/

static uint8_t s_pbuf_pool[16][GF_NET_MTU];
static bool s_pbuf_used[16];

gf_pbuf_t *gf_pbuf_alloc(gf_pbuf_type_t type, uint16_t size)
{
    if (size > GF_NET_MTU) {
        return NULL;
    }
    
    /* Find free buffer from pool */
    for (int i = 0; i < 16; i++) {
        if (!s_pbuf_used[i]) {
            s_pbuf_used[i] = true;
            
            gf_pbuf_t *p = (gf_pbuf_t *)s_pbuf_pool[i];
            p->next = NULL;
            p->payload = s_pbuf_pool[i] + sizeof(gf_pbuf_t);
            p->tot_len = size;
            p->len = size;
            p->type = type;
            p->ref = 1;
            p->flags = 0;
            
            return p;
        }
    }
    
    return NULL;
}

void gf_pbuf_free(gf_pbuf_t *p)
{
    while (p) {
        gf_pbuf_t *next = p->next;
        
        p->ref--;
        if (p->ref == 0) {
            /* Find which pool slot this came from */
            for (int i = 0; i < 16; i++) {
                if ((void *)p == (void *)s_pbuf_pool[i]) {
                    s_pbuf_used[i] = false;
                    break;
                }
            }
        }
        
        p = next;
    }
}

void gf_pbuf_ref(gf_pbuf_t *p)
{
    if (p) {
        p->ref++;
    }
}

void gf_pbuf_chain(gf_pbuf_t *head, gf_pbuf_t *tail)
{
    if (!head || !tail) return;
    
    gf_pbuf_t *p = head;
    while (p->next) {
        p = p->next;
    }
    p->next = tail;
    head->tot_len += tail->tot_len;
}

int gf_pbuf_copy_to(gf_pbuf_t *p, const void *data, uint16_t len)
{
    if (!p || !data || len > p->len) {
        return GF_NET_ERR_INVAL;
    }
    
    memcpy(p->payload, data, len);
    return GF_NET_OK;
}

int gf_pbuf_copy_from(const gf_pbuf_t *p, void *data, uint16_t len)
{
    if (!p || !data) {
        return GF_NET_ERR_INVAL;
    }
    
    uint16_t copied = 0;
    while (p && copied < len) {
        uint16_t to_copy = (len - copied) < p->len ? (len - copied) : p->len;
        memcpy((uint8_t *)data + copied, p->payload, to_copy);
        copied += to_copy;
        p = p->next;
    }
    
    return copied;
}

/*===========================================================================*/
/* Utility Functions                                                          */
/*===========================================================================*/

const char *gf_inet_ntoa(gf_ipv4_addr_t addr, char *buf, size_t len)
{
    if (!buf || len < 16) {
        return NULL;
    }
    
    snprintf(buf, len, "%u.%u.%u.%u", 
             addr.addr[0], addr.addr[1], addr.addr[2], addr.addr[3]);
    return buf;
}

int gf_inet_aton(const char *str, gf_ipv4_addr_t *addr)
{
    if (!str || !addr) {
        return GF_NET_ERR_INVAL;
    }
    
    unsigned int a, b, c, d;
    if (sscanf(str, "%u.%u.%u.%u", &a, &b, &c, &d) != 4) {
        return GF_NET_ERR_INVAL;
    }
    
    if (a > 255 || b > 255 || c > 255 || d > 255) {
        return GF_NET_ERR_INVAL;
    }
    
    addr->addr[0] = (uint8_t)a;
    addr->addr[1] = (uint8_t)b;
    addr->addr[2] = (uint8_t)c;
    addr->addr[3] = (uint8_t)d;
    
    return GF_NET_OK;
}

uint16_t gf_htons(uint16_t hostshort)
{
    /* Assuming little-endian host */
    return ((hostshort & 0x00FF) << 8) | ((hostshort & 0xFF00) >> 8);
}

uint16_t gf_ntohs(uint16_t netshort)
{
    return gf_htons(netshort);  /* Same operation */
}

uint32_t gf_htonl(uint32_t hostlong)
{
    return ((hostlong & 0x000000FF) << 24) |
           ((hostlong & 0x0000FF00) << 8)  |
           ((hostlong & 0x00FF0000) >> 8)  |
           ((hostlong & 0xFF000000) >> 24);
}

uint32_t gf_ntohl(uint32_t netlong)
{
    return gf_htonl(netlong);  /* Same operation */
}

uint16_t gf_inet_checksum(const void *data, size_t len)
{
    const uint16_t *ptr = (const uint16_t *)data;
    uint32_t sum = 0;
    
    while (len > 1) {
        sum += *ptr++;
        len -= 2;
    }
    
    if (len == 1) {
        sum += *(const uint8_t *)ptr;
    }
    
    /* Fold 32-bit sum to 16 bits */
    while (sum >> 16) {
        sum = (sum & 0xFFFF) + (sum >> 16);
    }
    
    return (uint16_t)~sum;
}

/*===========================================================================*/
/* TCP State Query                                                            */
/*===========================================================================*/

gf_tcp_state_t gf_tcp_get_state(gf_socket_t sock)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || pcb->type != GF_SOCK_STREAM) {
        return GF_TCP_STATE_CLOSED;
    }
    return pcb->tcp_state;
}

int gf_tcp_get_stats(gf_socket_t sock, gf_tcp_stats_t *stats)
{
    socket_pcb_t *pcb = get_socket(sock);
    if (!pcb || !stats || pcb->type != GF_SOCK_STREAM) {
        return GF_NET_ERR_INVAL;
    }
    
    stats->state = pcb->tcp_state;
    stats->bytes_sent = pcb->bytes_sent;
    stats->bytes_recv = pcb->bytes_recv;
    stats->retransmits = pcb->retransmits;
    stats->rtt_ms = pcb->rtt_ms;
    stats->send_window = GF_NET_TCP_WINDOW_SIZE;
    stats->recv_window = GF_NET_TCP_WINDOW_SIZE;
    stats->seq_next = pcb->seq_next;
    stats->ack_next = pcb->ack_num;
    
    return GF_NET_OK;
}
