/**
 * @file dns.h
 * @brief DNS Resolver with Caching
 * 
 * INDUSTRY RELEVANCE:
 *   DNS is essential for embedded network devices:
 *   - IoT devices need to resolve cloud service endpoints
 *   - Industrial systems use FQDN for SCADA/historian connections
 *   - Medical devices connect to EHR systems via hostname
 *   - Automotive connects to OTA update servers
 * 
 * This implementation provides:
 *   - Asynchronous DNS resolution
 *   - Configurable cache with TTL management
 *   - Multiple DNS server support with failover
 *   - Common record types (A, AAAA, CNAME, TXT)
 *   - mDNS support for local network discovery
 */

#ifndef GF_DNS_H
#define GF_DNS_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>
#include "net/tcp_ip.h"

/*===========================================================================*/
/* Constants and Limits                                                       */
/*===========================================================================*/

#define GF_DNS_MAX_NAME_LEN         256
#define GF_DNS_MAX_SERVERS          3
#define GF_DNS_CACHE_SIZE           16
#define GF_DNS_DEFAULT_PORT         53
#define GF_DNS_MDNS_PORT            5353
#define GF_DNS_DEFAULT_TIMEOUT_MS   5000
#define GF_DNS_MAX_RETRIES          3

/*===========================================================================*/
/* DNS Record Types                                                           */
/*===========================================================================*/

typedef enum {
    GF_DNS_TYPE_A       = 1,        /* IPv4 address */
    GF_DNS_TYPE_NS      = 2,        /* Name server */
    GF_DNS_TYPE_CNAME   = 5,        /* Canonical name */
    GF_DNS_TYPE_SOA     = 6,        /* Start of authority */
    GF_DNS_TYPE_PTR     = 12,       /* Pointer (reverse lookup) */
    GF_DNS_TYPE_MX      = 15,       /* Mail exchange */
    GF_DNS_TYPE_TXT     = 16,       /* Text record */
    GF_DNS_TYPE_AAAA    = 28,       /* IPv6 address */
    GF_DNS_TYPE_SRV     = 33,       /* Service locator */
    GF_DNS_TYPE_ANY     = 255       /* Any record type */
} gf_dns_type_t;

typedef enum {
    GF_DNS_CLASS_IN     = 1,        /* Internet */
    GF_DNS_CLASS_ANY    = 255       /* Any class */
} gf_dns_class_t;

/*===========================================================================*/
/* DNS Response Codes                                                         */
/*===========================================================================*/

typedef enum {
    GF_DNS_RCODE_OK         = 0,    /* No error */
    GF_DNS_RCODE_FORMATERR  = 1,    /* Format error */
    GF_DNS_RCODE_SERVFAIL   = 2,    /* Server failure */
    GF_DNS_RCODE_NXDOMAIN   = 3,    /* Name does not exist */
    GF_DNS_RCODE_NOTIMP     = 4,    /* Not implemented */
    GF_DNS_RCODE_REFUSED    = 5,    /* Query refused */
    GF_DNS_RCODE_TIMEOUT    = 100,  /* Timeout (internal) */
    GF_DNS_RCODE_NETWORK    = 101   /* Network error (internal) */
} gf_dns_rcode_t;

/*===========================================================================*/
/* DNS Query Result                                                           */
/*===========================================================================*/

typedef struct {
    gf_dns_type_t       type;
    gf_dns_class_t      class_;
    uint32_t            ttl;        /* Time-to-live in seconds */
    union {
        gf_ipv4_addr_t  a;          /* A record */
        gf_ipv6_addr_t  aaaa;       /* AAAA record */
        struct {
            char        name[GF_DNS_MAX_NAME_LEN];
        } cname;                    /* CNAME record */
        struct {
            char        data[256];
        } txt;                      /* TXT record */
        struct {
            uint16_t    priority;
            uint16_t    weight;
            uint16_t    port;
            char        target[GF_DNS_MAX_NAME_LEN];
        } srv;                      /* SRV record */
    } rdata;
} gf_dns_record_t;

typedef struct {
    gf_dns_rcode_t      rcode;
    uint8_t             answer_count;
    gf_dns_record_t     answers[8]; /* Max 8 answers */
    uint32_t            query_time_ms;
    uint8_t             server_used;
    bool                from_cache;
} gf_dns_result_t;

/*===========================================================================*/
/* Cache Entry                                                                */
/*===========================================================================*/

typedef struct {
    char                name[GF_DNS_MAX_NAME_LEN];
    gf_dns_type_t       type;
    gf_dns_record_t     record;
    uint32_t            expiry_time;    /* When entry expires */
    uint32_t            last_used;      /* For LRU eviction */
    bool                valid;
} gf_dns_cache_entry_t;

/*===========================================================================*/
/* DNS Configuration                                                          */
/*===========================================================================*/

typedef struct {
    gf_ipv4_addr_t      servers[GF_DNS_MAX_SERVERS];
    uint8_t             server_count;
    uint32_t            timeout_ms;
    uint8_t             max_retries;
    bool                enable_cache;
    uint32_t            cache_max_ttl;  /* Max TTL to respect (0=use as-is) */
    uint32_t            cache_min_ttl;  /* Min TTL to cache */
    bool                enable_mdns;    /* Enable mDNS for .local domains */
} gf_dns_config_t;

/*===========================================================================*/
/* Callback for Async Resolution                                              */
/*===========================================================================*/

typedef void (*gf_dns_callback_t)(const char *hostname, 
                                   const gf_dns_result_t *result, 
                                   void *user_data);

/*===========================================================================*/
/* DNS Client API                                                             */
/*===========================================================================*/

/**
 * @brief Initialize DNS client
 * @param config DNS configuration (NULL for defaults with 8.8.8.8)
 * @return 0 on success
 */
int gf_dns_init(const gf_dns_config_t *config);

/**
 * @brief Shutdown DNS client
 */
void gf_dns_shutdown(void);

/**
 * @brief Resolve hostname to IPv4 address (synchronous)
 * @param hostname Hostname to resolve
 * @param addr Output IPv4 address
 * @param timeout_ms Timeout in milliseconds (0 = default)
 * @return DNS response code
 */
gf_dns_rcode_t gf_dns_resolve(const char *hostname, 
                               gf_ipv4_addr_t *addr,
                               uint32_t timeout_ms);

/**
 * @brief Resolve hostname to IPv4 address (asynchronous)
 * @param hostname Hostname to resolve
 * @param callback Completion callback
 * @param user_data User data passed to callback
 * @return 0 on success (query started)
 */
int gf_dns_resolve_async(const char *hostname,
                          gf_dns_callback_t callback,
                          void *user_data);

/**
 * @brief Full DNS query (synchronous)
 * @param hostname Hostname to query
 * @param type Record type to query
 * @param result Output result structure
 * @return DNS response code
 */
gf_dns_rcode_t gf_dns_query(const char *hostname,
                             gf_dns_type_t type,
                             gf_dns_result_t *result);

/**
 * @brief Reverse DNS lookup (PTR record)
 * @param addr IP address to look up
 * @param hostname Output hostname buffer
 * @param len Buffer length
 * @return DNS response code
 */
gf_dns_rcode_t gf_dns_reverse_lookup(gf_ipv4_addr_t addr,
                                      char *hostname,
                                      size_t len);

/*===========================================================================*/
/* Cache Management                                                           */
/*===========================================================================*/

/**
 * @brief Look up entry in cache
 * @param hostname Hostname to find
 * @param type Record type
 * @param record Output record (if found)
 * @return true if found and not expired
 */
bool gf_dns_cache_lookup(const char *hostname,
                          gf_dns_type_t type,
                          gf_dns_record_t *record);

/**
 * @brief Add entry to cache
 * @param hostname Hostname
 * @param record Record to cache
 */
void gf_dns_cache_add(const char *hostname, const gf_dns_record_t *record);

/**
 * @brief Clear entire cache
 */
void gf_dns_cache_clear(void);

/**
 * @brief Remove expired entries from cache
 */
void gf_dns_cache_prune(void);

/**
 * @brief Get cache statistics
 */
typedef struct {
    uint32_t    hits;           /* Cache hits */
    uint32_t    misses;         /* Cache misses */
    uint32_t    entries_used;   /* Current entries in cache */
    uint32_t    evictions;      /* LRU evictions */
} gf_dns_cache_stats_t;

void gf_dns_cache_get_stats(gf_dns_cache_stats_t *stats);

/*===========================================================================*/
/* DNS Server Management                                                      */
/*===========================================================================*/

/**
 * @brief Set DNS servers
 * @param servers Array of server addresses
 * @param count Number of servers
 */
void gf_dns_set_servers(const gf_ipv4_addr_t *servers, uint8_t count);

/**
 * @brief Get current DNS servers
 */
uint8_t gf_dns_get_servers(gf_ipv4_addr_t *servers, uint8_t max_count);

/*===========================================================================*/
/* mDNS (Multicast DNS)                                                       */
/*===========================================================================*/

/**
 * @brief Resolve .local hostname via mDNS
 */
gf_dns_rcode_t gf_mdns_resolve(const char *hostname, 
                                gf_ipv4_addr_t *addr,
                                uint32_t timeout_ms);

/**
 * @brief Announce local service via mDNS
 */
int gf_mdns_announce(const char *service_type,
                      const char *instance_name,
                      uint16_t port,
                      const char *txt_record);

/**
 * @brief Browse for services via mDNS
 */
typedef void (*gf_mdns_browse_callback_t)(const char *instance_name,
                                           gf_ipv4_addr_t addr,
                                           uint16_t port,
                                           void *user_data);

int gf_mdns_browse(const char *service_type,
                    gf_mdns_browse_callback_t callback,
                    void *user_data,
                    uint32_t timeout_ms);

/*===========================================================================*/
/* Periodic Processing                                                        */
/*===========================================================================*/

/**
 * @brief Process DNS operations (call from main loop)
 */
void gf_dns_tick(void);

#endif /* GF_DNS_H */
