/**
 * @file dns.c
 * @brief DNS Resolver with Caching Implementation
 */

#include "net/dns.h"
#include "core/error_handler.h"
#include <string.h>
#include <stdio.h>

/*===========================================================================*/
/* DNS Protocol Constants                                                     */
/*===========================================================================*/

#define DNS_HEADER_SIZE         12
#define DNS_FLAG_QR             0x8000  /* Query/Response */
#define DNS_FLAG_OPCODE_MASK    0x7800
#define DNS_FLAG_AA             0x0400  /* Authoritative Answer */
#define DNS_FLAG_TC             0x0200  /* Truncated */
#define DNS_FLAG_RD             0x0100  /* Recursion Desired */
#define DNS_FLAG_RA             0x0080  /* Recursion Available */
#define DNS_FLAG_RCODE_MASK     0x000F

/*===========================================================================*/
/* Internal Structures                                                        */
/*===========================================================================*/

/* DNS Header (network byte order) */
typedef struct __attribute__((packed)) {
    uint16_t    id;
    uint16_t    flags;
    uint16_t    qdcount;    /* Question count */
    uint16_t    ancount;    /* Answer count */
    uint16_t    nscount;    /* Authority count */
    uint16_t    arcount;    /* Additional count */
} dns_header_t;

/* Pending async query */
typedef struct {
    char                hostname[GF_DNS_MAX_NAME_LEN];
    gf_dns_callback_t   callback;
    void               *user_data;
    uint16_t            query_id;
    uint32_t            start_time;
    uint8_t             retry_count;
    uint8_t             server_index;
    bool                active;
} dns_pending_query_t;

/*===========================================================================*/
/* Static State                                                               */
/*===========================================================================*/

static gf_dns_config_t s_config;
static gf_dns_cache_entry_t s_cache[GF_DNS_CACHE_SIZE];
static gf_dns_cache_stats_t s_cache_stats;
static dns_pending_query_t s_pending_queries[4];
static uint16_t s_next_query_id = 0x1234;
static bool s_initialized;
static gf_socket_t s_dns_socket = GF_INVALID_SOCKET;
static uint32_t s_current_time;  /* Stub: would use HAL timer */

/*===========================================================================*/
/* Helper Functions                                                           */
/*===========================================================================*/

static uint16_t get_next_query_id(void)
{
    return s_next_query_id++;
}

static size_t encode_dns_name(const char *hostname, uint8_t *buf, size_t maxlen)
{
    if (!hostname || !buf || maxlen < 2) {
        return 0;
    }
    
    size_t pos = 0;
    const char *ptr = hostname;
    
    while (*ptr && pos < maxlen - 2) {
        /* Find next dot or end */
        const char *dot = strchr(ptr, '.');
        size_t label_len;
        
        if (dot) {
            label_len = dot - ptr;
        } else {
            label_len = strlen(ptr);
        }
        
        if (label_len > 63 || pos + label_len + 1 >= maxlen) {
            return 0;  /* Label too long */
        }
        
        buf[pos++] = (uint8_t)label_len;
        memcpy(buf + pos, ptr, label_len);
        pos += label_len;
        
        if (dot) {
            ptr = dot + 1;
        } else {
            break;
        }
    }
    
    buf[pos++] = 0;  /* Root label */
    return pos;
}

__attribute__((unused))
static size_t decode_dns_name(const uint8_t *packet, size_t packet_len,
                               size_t offset, char *name, size_t maxlen)
{
    if (!packet || !name || offset >= packet_len) {
        return 0;
    }
    
    size_t name_pos = 0;
    size_t pos = offset;
    size_t jumps = 0;
    size_t original_pos = 0;
    bool jumped = false;
    
    while (pos < packet_len && jumps < 10) {
        uint8_t len = packet[pos];
        
        if (len == 0) {
            pos++;
            break;
        }
        
        if ((len & 0xC0) == 0xC0) {
            /* Compression pointer */
            if (pos + 1 >= packet_len) return 0;
            
            if (!jumped) {
                original_pos = pos + 2;
                jumped = true;
            }
            
            pos = ((len & 0x3F) << 8) | packet[pos + 1];
            jumps++;
            continue;
        }
        
        if (pos + 1 + len > packet_len) return 0;
        if (name_pos + len + 1 >= maxlen) return 0;
        
        if (name_pos > 0) {
            name[name_pos++] = '.';
        }
        
        memcpy(name + name_pos, packet + pos + 1, len);
        name_pos += len;
        pos += 1 + len;
    }
    
    name[name_pos] = '\0';
    return jumped ? original_pos : pos;
}

static int build_dns_query(const char *hostname, gf_dns_type_t type,
                            uint16_t query_id, uint8_t *buf, size_t maxlen)
{
    if (maxlen < DNS_HEADER_SIZE + GF_DNS_MAX_NAME_LEN + 4) {
        return -1;
    }
    
    /* Build header */
    dns_header_t *hdr = (dns_header_t *)buf;
    hdr->id = gf_htons(query_id);
    hdr->flags = gf_htons(DNS_FLAG_RD);  /* Recursion desired */
    hdr->qdcount = gf_htons(1);
    hdr->ancount = 0;
    hdr->nscount = 0;
    hdr->arcount = 0;
    
    /* Encode hostname */
    size_t name_len = encode_dns_name(hostname, buf + DNS_HEADER_SIZE, 
                                       maxlen - DNS_HEADER_SIZE - 4);
    if (name_len == 0) {
        return -1;
    }
    
    /* Add question type and class */
    size_t qpos = DNS_HEADER_SIZE + name_len;
    buf[qpos++] = (uint8_t)(type >> 8);
    buf[qpos++] = (uint8_t)(type & 0xFF);
    buf[qpos++] = 0;
    buf[qpos++] = GF_DNS_CLASS_IN;
    
    return (int)qpos;
}

__attribute__((unused))
static gf_dns_rcode_t parse_dns_response(const uint8_t *data, size_t len,
                                          gf_dns_result_t *result)
{
    if (len < DNS_HEADER_SIZE) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    const dns_header_t *hdr = (const dns_header_t *)data;
    uint16_t flags = gf_ntohs(hdr->flags);
    
    /* Check it's a response */
    if (!(flags & DNS_FLAG_QR)) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    gf_dns_rcode_t rcode = (gf_dns_rcode_t)(flags & DNS_FLAG_RCODE_MASK);
    result->rcode = rcode;
    
    if (rcode != GF_DNS_RCODE_OK) {
        return rcode;
    }
    
    uint16_t qdcount = gf_ntohs(hdr->qdcount);
    uint16_t ancount = gf_ntohs(hdr->ancount);
    
    /* Skip questions */
    size_t pos = DNS_HEADER_SIZE;
    for (uint16_t i = 0; i < qdcount; i++) {
        char name[GF_DNS_MAX_NAME_LEN];
        size_t new_pos = decode_dns_name(data, len, pos, name, sizeof(name));
        if (new_pos == 0) return GF_DNS_RCODE_FORMATERR;
        pos = new_pos + 4;  /* Skip QTYPE and QCLASS */
    }
    
    /* Parse answers */
    result->answer_count = 0;
    for (uint16_t i = 0; i < ancount && result->answer_count < 8; i++) {
        char name[GF_DNS_MAX_NAME_LEN];
        size_t new_pos = decode_dns_name(data, len, pos, name, sizeof(name));
        if (new_pos == 0 || new_pos + 10 > len) break;
        
        pos = new_pos;
        
        uint16_t rtype = (data[pos] << 8) | data[pos + 1];
        uint16_t rclass = (data[pos + 2] << 8) | data[pos + 3];
        uint32_t ttl = (data[pos + 4] << 24) | (data[pos + 5] << 16) |
                       (data[pos + 6] << 8) | data[pos + 7];
        uint16_t rdlength = (data[pos + 8] << 8) | data[pos + 9];
        pos += 10;
        
        if (pos + rdlength > len) break;
        
        gf_dns_record_t *rec = &result->answers[result->answer_count];
        rec->type = (gf_dns_type_t)rtype;
        rec->class_ = (gf_dns_class_t)rclass;
        rec->ttl = ttl;
        
        switch (rtype) {
            case GF_DNS_TYPE_A:
                if (rdlength == 4) {
                    memcpy(rec->rdata.a.addr, data + pos, 4);
                    result->answer_count++;
                }
                break;
                
            case GF_DNS_TYPE_CNAME:
                decode_dns_name(data, len, pos, rec->rdata.cname.name,
                               sizeof(rec->rdata.cname.name));
                result->answer_count++;
                break;
                
            case GF_DNS_TYPE_AAAA:
                if (rdlength == 16) {
                    memcpy(rec->rdata.aaaa.addr, data + pos, 16);
                    result->answer_count++;
                }
                break;
                
            default:
                /* Skip unsupported record types */
                break;
        }
        
        pos += rdlength;
    }
    
    return GF_DNS_RCODE_OK;
}

/*===========================================================================*/
/* Cache Management                                                           */
/*===========================================================================*/

static int find_cache_slot(const char *hostname, gf_dns_type_t type)
{
    /* Look for existing entry */
    for (int i = 0; i < GF_DNS_CACHE_SIZE; i++) {
        if (s_cache[i].valid &&
            s_cache[i].type == type &&
            strcmp(s_cache[i].name, hostname) == 0) {
            return i;
        }
    }
    return -1;
}

static int find_free_cache_slot(void)
{
    int oldest_slot = 0;
    uint32_t oldest_time = UINT32_MAX;
    
    for (int i = 0; i < GF_DNS_CACHE_SIZE; i++) {
        if (!s_cache[i].valid) {
            return i;
        }
        
        /* Check for expired entry */
        if (s_cache[i].expiry_time <= s_current_time) {
            return i;
        }
        
        /* LRU eviction */
        if (s_cache[i].last_used < oldest_time) {
            oldest_time = s_cache[i].last_used;
            oldest_slot = i;
        }
    }
    
    s_cache_stats.evictions++;
    return oldest_slot;
}

bool gf_dns_cache_lookup(const char *hostname, gf_dns_type_t type,
                          gf_dns_record_t *record)
{
    if (!s_config.enable_cache || !hostname || !record) {
        return false;
    }
    
    int slot = find_cache_slot(hostname, type);
    if (slot < 0) {
        s_cache_stats.misses++;
        return false;
    }
    
    /* Check expiry */
    if (s_cache[slot].expiry_time <= s_current_time) {
        s_cache[slot].valid = false;
        s_cache_stats.misses++;
        return false;
    }
    
    memcpy(record, &s_cache[slot].record, sizeof(gf_dns_record_t));
    s_cache[slot].last_used = s_current_time;
    s_cache_stats.hits++;
    
    return true;
}

void gf_dns_cache_add(const char *hostname, const gf_dns_record_t *record)
{
    if (!s_config.enable_cache || !hostname || !record) {
        return;
    }
    
    /* Check TTL bounds */
    uint32_t ttl = record->ttl;
    if (s_config.cache_min_ttl > 0 && ttl < s_config.cache_min_ttl) {
        ttl = s_config.cache_min_ttl;
    }
    if (s_config.cache_max_ttl > 0 && ttl > s_config.cache_max_ttl) {
        ttl = s_config.cache_max_ttl;
    }
    
    int slot = find_cache_slot(hostname, record->type);
    if (slot < 0) {
        slot = find_free_cache_slot();
    }
    
    strncpy(s_cache[slot].name, hostname, GF_DNS_MAX_NAME_LEN - 1);
    s_cache[slot].name[GF_DNS_MAX_NAME_LEN - 1] = '\0';
    s_cache[slot].type = record->type;
    memcpy(&s_cache[slot].record, record, sizeof(gf_dns_record_t));
    s_cache[slot].expiry_time = s_current_time + ttl;
    s_cache[slot].last_used = s_current_time;
    s_cache[slot].valid = true;
}

void gf_dns_cache_clear(void)
{
    memset(s_cache, 0, sizeof(s_cache));
    s_cache_stats.entries_used = 0;
}

void gf_dns_cache_prune(void)
{
    for (int i = 0; i < GF_DNS_CACHE_SIZE; i++) {
        if (s_cache[i].valid && s_cache[i].expiry_time <= s_current_time) {
            s_cache[i].valid = false;
        }
    }
    
    /* Update entry count */
    s_cache_stats.entries_used = 0;
    for (int i = 0; i < GF_DNS_CACHE_SIZE; i++) {
        if (s_cache[i].valid) {
            s_cache_stats.entries_used++;
        }
    }
}

void gf_dns_cache_get_stats(gf_dns_cache_stats_t *stats)
{
    if (stats) {
        memcpy(stats, &s_cache_stats, sizeof(gf_dns_cache_stats_t));
    }
}

/*===========================================================================*/
/* DNS Client API                                                             */
/*===========================================================================*/

int gf_dns_init(const gf_dns_config_t *config)
{
    if (config) {
        memcpy(&s_config, config, sizeof(gf_dns_config_t));
    } else {
        /* Defaults */
        memset(&s_config, 0, sizeof(gf_dns_config_t));
        
        /* Google DNS as default */
        s_config.servers[0].addr[0] = 8;
        s_config.servers[0].addr[1] = 8;
        s_config.servers[0].addr[2] = 8;
        s_config.servers[0].addr[3] = 8;
        s_config.server_count = 1;
        s_config.timeout_ms = GF_DNS_DEFAULT_TIMEOUT_MS;
        s_config.max_retries = GF_DNS_MAX_RETRIES;
        s_config.enable_cache = true;
        s_config.cache_max_ttl = 86400;  /* 24 hours */
        s_config.cache_min_ttl = 60;
    }
    
    memset(s_cache, 0, sizeof(s_cache));
    memset(&s_cache_stats, 0, sizeof(s_cache_stats));
    memset(s_pending_queries, 0, sizeof(s_pending_queries));
    
    /* Create UDP socket for DNS queries */
    s_dns_socket = gf_socket(GF_NET_AF_INET, GF_SOCK_DGRAM, GF_IPPROTO_UDP);
    
    s_initialized = true;
    return 0;
}

void gf_dns_shutdown(void)
{
    if (s_dns_socket != GF_INVALID_SOCKET) {
        gf_close(s_dns_socket);
        s_dns_socket = GF_INVALID_SOCKET;
    }
    s_initialized = false;
}

gf_dns_rcode_t gf_dns_resolve(const char *hostname, gf_ipv4_addr_t *addr,
                               uint32_t timeout_ms)
{
    if (!s_initialized || !hostname || !addr) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    /* Check if it's already an IP address */
    if (gf_inet_aton(hostname, addr) == GF_NET_OK) {
        return GF_DNS_RCODE_OK;
    }
    
    /* Check cache first */
    gf_dns_record_t cached;
    if (gf_dns_cache_lookup(hostname, GF_DNS_TYPE_A, &cached)) {
        memcpy(addr, &cached.rdata.a, sizeof(gf_ipv4_addr_t));
        return GF_DNS_RCODE_OK;
    }
    
    /* Build and send query */
    uint8_t query_buf[512];
    uint16_t query_id = get_next_query_id();
    int query_len = build_dns_query(hostname, GF_DNS_TYPE_A, query_id,
                                    query_buf, sizeof(query_buf));
    if (query_len < 0) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    /* Prepare destination address */
    gf_sockaddr_in_t dns_addr = {
        .family = GF_NET_AF_INET,
        .port = GF_DNS_DEFAULT_PORT,
        .addr = s_config.servers[0]
    };
    
    /* Send query */
    gf_sendto(s_dns_socket, query_buf, query_len, 0, &dns_addr);
    
    /* Stub: would wait for response with timeout */
    /* For demo, simulate successful response */
    (void)timeout_ms;
    
    /* Simulated response - localhost */
    addr->addr[0] = 127;
    addr->addr[1] = 0;
    addr->addr[2] = 0;
    addr->addr[3] = 1;
    
    /* Cache result */
    gf_dns_record_t rec = {
        .type = GF_DNS_TYPE_A,
        .class_ = GF_DNS_CLASS_IN,
        .ttl = 300
    };
    memcpy(&rec.rdata.a, addr, sizeof(gf_ipv4_addr_t));
    gf_dns_cache_add(hostname, &rec);
    
    return GF_DNS_RCODE_OK;
}

int gf_dns_resolve_async(const char *hostname, gf_dns_callback_t callback,
                          void *user_data)
{
    if (!s_initialized || !hostname || !callback) {
        return -1;
    }
    
    /* Find free pending query slot */
    dns_pending_query_t *pq = NULL;
    for (int i = 0; i < 4; i++) {
        if (!s_pending_queries[i].active) {
            pq = &s_pending_queries[i];
            break;
        }
    }
    
    if (!pq) {
        return -1;  /* No slots available */
    }
    
    strncpy(pq->hostname, hostname, GF_DNS_MAX_NAME_LEN - 1);
    pq->hostname[GF_DNS_MAX_NAME_LEN - 1] = '\0';
    pq->callback = callback;
    pq->user_data = user_data;
    pq->query_id = get_next_query_id();
    pq->start_time = s_current_time;
    pq->retry_count = 0;
    pq->server_index = 0;
    pq->active = true;
    
    /* Build and send initial query */
    uint8_t query_buf[512];
    int query_len = build_dns_query(hostname, GF_DNS_TYPE_A, pq->query_id,
                                    query_buf, sizeof(query_buf));
    if (query_len < 0) {
        pq->active = false;
        return -1;
    }
    
    gf_sockaddr_in_t dns_addr = {
        .family = GF_NET_AF_INET,
        .port = GF_DNS_DEFAULT_PORT,
        .addr = s_config.servers[0]
    };
    
    gf_sendto(s_dns_socket, query_buf, query_len, 0, &dns_addr);
    
    return 0;
}

gf_dns_rcode_t gf_dns_query(const char *hostname, gf_dns_type_t type,
                             gf_dns_result_t *result)
{
    if (!s_initialized || !hostname || !result) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    memset(result, 0, sizeof(gf_dns_result_t));
    result->from_cache = false;
    
    /* Check cache */
    if (s_config.enable_cache) {
        gf_dns_record_t cached;
        if (gf_dns_cache_lookup(hostname, type, &cached)) {
            result->rcode = GF_DNS_RCODE_OK;
            result->answer_count = 1;
            memcpy(&result->answers[0], &cached, sizeof(gf_dns_record_t));
            result->from_cache = true;
            return GF_DNS_RCODE_OK;
        }
    }
    
    /* Build and send query */
    uint8_t query_buf[512];
    uint16_t query_id = get_next_query_id();
    int query_len = build_dns_query(hostname, type, query_id,
                                    query_buf, sizeof(query_buf));
    if (query_len < 0) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    /* Stub: would send and wait for response */
    result->rcode = GF_DNS_RCODE_OK;
    result->query_time_ms = 10;
    
    return GF_DNS_RCODE_OK;
}

gf_dns_rcode_t gf_dns_reverse_lookup(gf_ipv4_addr_t addr, char *hostname,
                                      size_t len)
{
    if (!hostname || len < 32) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    /* Format as in-addr.arpa domain */
    char arpa_name[64];
    snprintf(arpa_name, sizeof(arpa_name), "%u.%u.%u.%u.in-addr.arpa",
             addr.addr[3], addr.addr[2], addr.addr[1], addr.addr[0]);
    
    /* Query PTR record */
    gf_dns_result_t result;
    gf_dns_rcode_t rcode = gf_dns_query(arpa_name, GF_DNS_TYPE_PTR, &result);
    
    if (rcode == GF_DNS_RCODE_OK && result.answer_count > 0) {
        /* Stub: would copy hostname from PTR record */
        snprintf(hostname, len, "host-%u-%u-%u-%u",
                 addr.addr[0], addr.addr[1], addr.addr[2], addr.addr[3]);
    }
    
    return rcode;
}

/*===========================================================================*/
/* DNS Server Management                                                      */
/*===========================================================================*/

void gf_dns_set_servers(const gf_ipv4_addr_t *servers, uint8_t count)
{
    if (!servers || count == 0) {
        return;
    }
    
    s_config.server_count = count > GF_DNS_MAX_SERVERS ? 
                            GF_DNS_MAX_SERVERS : count;
    
    for (uint8_t i = 0; i < s_config.server_count; i++) {
        memcpy(&s_config.servers[i], &servers[i], sizeof(gf_ipv4_addr_t));
    }
}

uint8_t gf_dns_get_servers(gf_ipv4_addr_t *servers, uint8_t max_count)
{
    if (!servers || max_count == 0) {
        return 0;
    }
    
    uint8_t count = s_config.server_count < max_count ? 
                    s_config.server_count : max_count;
    
    for (uint8_t i = 0; i < count; i++) {
        memcpy(&servers[i], &s_config.servers[i], sizeof(gf_ipv4_addr_t));
    }
    
    return count;
}

/*===========================================================================*/
/* mDNS                                                                       */
/*===========================================================================*/

gf_dns_rcode_t gf_mdns_resolve(const char *hostname, gf_ipv4_addr_t *addr,
                                uint32_t timeout_ms)
{
    if (!s_initialized || !hostname || !addr) {
        return GF_DNS_RCODE_FORMATERR;
    }
    
    /* Check for .local suffix */
    size_t len = strlen(hostname);
    if (len < 7 || strcmp(hostname + len - 6, ".local") != 0) {
        return GF_DNS_RCODE_NXDOMAIN;
    }
    
    (void)timeout_ms;
    
    /* Stub: would send mDNS query to 224.0.0.251:5353 */
    return GF_DNS_RCODE_TIMEOUT;
}

int gf_mdns_announce(const char *service_type, const char *instance_name,
                      uint16_t port, const char *txt_record)
{
    if (!s_initialized || !service_type || !instance_name) {
        return -1;
    }
    
    (void)port;
    (void)txt_record;
    
    /* Stub: would send mDNS announcement */
    return 0;
}

int gf_mdns_browse(const char *service_type, gf_mdns_browse_callback_t callback,
                    void *user_data, uint32_t timeout_ms)
{
    if (!s_initialized || !service_type || !callback) {
        return -1;
    }
    
    (void)user_data;
    (void)timeout_ms;
    
    /* Stub: would send mDNS browse query */
    return 0;
}

/*===========================================================================*/
/* Periodic Processing                                                        */
/*===========================================================================*/

void gf_dns_tick(void)
{
    if (!s_initialized) return;
    
    s_current_time++;  /* Stub: would use HAL timer */
    
    /* Check for pending query timeouts and retry */
    for (int i = 0; i < 4; i++) {
        dns_pending_query_t *pq = &s_pending_queries[i];
        if (!pq->active) continue;
        
        uint32_t elapsed = s_current_time - pq->start_time;
        if (elapsed >= s_config.timeout_ms) {
            if (pq->retry_count < s_config.max_retries) {
                /* Retry with next server */
                pq->server_index = (pq->server_index + 1) % s_config.server_count;
                pq->retry_count++;
                pq->start_time = s_current_time;
                
                /* Resend query */
                uint8_t query_buf[512];
                int query_len = build_dns_query(pq->hostname, GF_DNS_TYPE_A,
                                                pq->query_id, query_buf, 
                                                sizeof(query_buf));
                if (query_len > 0) {
                    gf_sockaddr_in_t dns_addr = {
                        .family = GF_NET_AF_INET,
                        .port = GF_DNS_DEFAULT_PORT,
                        .addr = s_config.servers[pq->server_index]
                    };
                    gf_sendto(s_dns_socket, query_buf, query_len, 0, &dns_addr);
                }
            } else {
                /* Max retries exceeded - call callback with timeout */
                gf_dns_result_t result = {
                    .rcode = GF_DNS_RCODE_TIMEOUT,
                    .answer_count = 0
                };
                pq->callback(pq->hostname, &result, pq->user_data);
                pq->active = false;
            }
        }
    }
    
    /* Periodic cache maintenance */
    static uint32_t last_prune = 0;
    if (s_current_time - last_prune >= 60) {
        gf_dns_cache_prune();
        last_prune = s_current_time;
    }
}
