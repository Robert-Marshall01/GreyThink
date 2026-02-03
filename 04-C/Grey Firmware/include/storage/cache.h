/**
 * @file cache.h
 * @brief Local Caching Module for Edge Storage
 * 
 * Provides efficient caching mechanisms for edge devices, enabling
 * data persistence during offline operation with intelligent eviction
 * policies and cache coherence support.
 * 
 * Key Features:
 * - LRU/LFU/FIFO eviction policies
 * - Write-through and write-back modes
 * - Cache partitioning for priority data
 * - Persistent cache with flash wear leveling
 * - TTL-based automatic expiration
 * 
 * Industry Relevance:
 * - IoT gateways with intermittent connectivity
 * - Telemetry buffering during network outages
 * - Sensor data aggregation before cloud sync
 */

#ifndef GF_CACHE_H
#define GF_CACHE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define GF_CACHE_KEY_MAX_LEN        64
#define GF_CACHE_MAX_PARTITIONS     8
#define GF_CACHE_MAX_ENTRIES        1024
#define GF_CACHE_DEFAULT_TTL_MS     (60 * 60 * 1000)  /* 1 hour */

/*===========================================================================*/
/* Enumerations                                                               */
/*===========================================================================*/

/**
 * @brief Cache eviction policies
 */
typedef enum {
    GF_CACHE_EVICT_LRU       = 0x00,  /** Least Recently Used */
    GF_CACHE_EVICT_LFU       = 0x01,  /** Least Frequently Used */
    GF_CACHE_EVICT_FIFO      = 0x02,  /** First In, First Out */
    GF_CACHE_EVICT_TTL       = 0x03,  /** Time-To-Live based */
    GF_CACHE_EVICT_PRIORITY  = 0x04,  /** Priority-based, low priority first */
} gf_cache_evict_policy_t;

/**
 * @brief Cache write modes
 */
typedef enum {
    GF_CACHE_WRITE_THROUGH   = 0x00,  /** Write to cache and backing store */
    GF_CACHE_WRITE_BACK      = 0x01,  /** Write to cache, lazy sync to store */
    GF_CACHE_WRITE_AROUND    = 0x02,  /** Write directly to store, invalidate cache */
} gf_cache_write_mode_t;

/**
 * @brief Cache backing store types
 */
typedef enum {
    GF_CACHE_STORE_MEMORY    = 0x00,  /** RAM only (volatile) */
    GF_CACHE_STORE_FLASH     = 0x01,  /** Flash storage (persistent) */
    GF_CACHE_STORE_EEPROM    = 0x02,  /** EEPROM (persistent, small) */
    GF_CACHE_STORE_EXTERNAL  = 0x03,  /** External SD/MMC */
} gf_cache_store_type_t;

/**
 * @brief Cache entry priority levels
 */
typedef enum {
    GF_CACHE_PRIORITY_LOW    = 0,
    GF_CACHE_PRIORITY_NORMAL = 1,
    GF_CACHE_PRIORITY_HIGH   = 2,
    GF_CACHE_PRIORITY_CRITICAL = 3,  /** Never evict unless explicit */
} gf_cache_priority_t;

/**
 * @brief Cache operation result codes
 */
typedef enum {
    GF_CACHE_OK              = 0,
    GF_CACHE_ERR_NOT_FOUND   = -1,
    GF_CACHE_ERR_FULL        = -2,
    GF_CACHE_ERR_INVALID     = -3,
    GF_CACHE_ERR_EXPIRED     = -4,
    GF_CACHE_ERR_IO          = -5,
    GF_CACHE_ERR_NOMEM       = -6,
} gf_cache_result_t;

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Cache configuration
 */
typedef struct {
    uint32_t                max_entries;      /** Maximum cache entries */
    uint32_t                max_size_bytes;   /** Maximum total size */
    gf_cache_evict_policy_t evict_policy;     /** Eviction policy */
    gf_cache_write_mode_t   write_mode;       /** Write policy */
    gf_cache_store_type_t   store_type;       /** Backing store type */
    uint32_t                default_ttl_ms;   /** Default TTL in milliseconds */
    bool                    persist_on_shutdown; /** Save cache on shutdown */
    bool                    load_on_init;     /** Restore cache on init */
    uint8_t                 partition_id;     /** Partition identifier */
} gf_cache_config_t;

/**
 * @brief Cache entry metadata
 */
typedef struct {
    char                    key[GF_CACHE_KEY_MAX_LEN];
    uint32_t                size;             /** Data size in bytes */
    uint64_t                created_ms;       /** Creation timestamp */
    uint64_t                accessed_ms;      /** Last access timestamp */
    uint64_t                expires_ms;       /** Expiration timestamp (0=never) */
    uint32_t                access_count;     /** Access frequency */
    gf_cache_priority_t     priority;         /** Entry priority */
    bool                    dirty;            /** Modified, needs sync */
    uint32_t                version;          /** Version for coherence */
} gf_cache_entry_meta_t;

/**
 * @brief Cache statistics
 */
typedef struct {
    uint32_t                entries;          /** Current entry count */
    uint32_t                size_bytes;       /** Current size in bytes */
    uint64_t                hits;             /** Cache hit count */
    uint64_t                misses;           /** Cache miss count */
    uint64_t                evictions;        /** Eviction count */
    uint64_t                expirations;      /** TTL expiration count */
    uint64_t                writes;           /** Write operation count */
    uint64_t                reads;            /** Read operation count */
    uint32_t                dirty_entries;    /** Entries pending sync */
    float                   hit_ratio;        /** Hit ratio percentage */
} gf_cache_stats_t;

/**
 * @brief Cache partition info
 */
typedef struct {
    uint8_t                 id;
    char                    name[32];
    gf_cache_config_t       config;
    gf_cache_stats_t        stats;
} gf_cache_partition_t;

/**
 * @brief Cache write callback for backing store integration
 */
typedef int (*gf_cache_write_cb_t)(const char *key, const void *data,
                                   size_t size, void *ctx);

/**
 * @brief Cache read callback for backing store integration
 */
typedef int (*gf_cache_read_cb_t)(const char *key, void *data,
                                  size_t max_size, size_t *actual_size,
                                  void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize the cache subsystem
 */
int gf_cache_init(const gf_cache_config_t *config);

/**
 * @brief Shutdown and optionally persist cache
 */
int gf_cache_shutdown(void);

/**
 * @brief Create a named cache partition
 */
int gf_cache_create_partition(const char *name, const gf_cache_config_t *config);

/**
 * @brief Select active cache partition
 */
int gf_cache_select_partition(uint8_t partition_id);

/**
 * @brief Store data in cache
 */
int gf_cache_put(const char *key, const void *data, size_t size);

/**
 * @brief Store data with explicit TTL
 */
int gf_cache_put_ttl(const char *key, const void *data, size_t size,
                     uint32_t ttl_ms);

/**
 * @brief Store data with priority
 */
int gf_cache_put_priority(const char *key, const void *data, size_t size,
                          gf_cache_priority_t priority);

/**
 * @brief Retrieve data from cache
 */
int gf_cache_get(const char *key, void *data, size_t max_size, size_t *actual_size);

/**
 * @brief Check if key exists (without fetching)
 */
bool gf_cache_exists(const char *key);

/**
 * @brief Get entry metadata without fetching data
 */
int gf_cache_get_meta(const char *key, gf_cache_entry_meta_t *meta);

/**
 * @brief Remove entry from cache
 */
int gf_cache_remove(const char *key);

/**
 * @brief Clear all cache entries
 */
int gf_cache_clear(void);

/**
 * @brief Clear expired entries
 */
int gf_cache_evict_expired(void);

/**
 * @brief Force eviction of N entries
 */
int gf_cache_evict(int count);

/**
 * @brief Touch entry to update access time
 */
int gf_cache_touch(const char *key);

/**
 * @brief Update TTL for existing entry
 */
int gf_cache_set_ttl(const char *key, uint32_t ttl_ms);

/**
 * @brief Flush dirty entries to backing store
 */
int gf_cache_sync(void);

/**
 * @brief Get cache statistics
 */
int gf_cache_get_stats(gf_cache_stats_t *stats);

/**
 * @brief Reset cache statistics
 */
int gf_cache_reset_stats(void);

/**
 * @brief Register callbacks for backing store
 */
int gf_cache_set_callbacks(gf_cache_write_cb_t write_cb,
                           gf_cache_read_cb_t read_cb, void *ctx);

/**
 * @brief Iterate over cache entries
 */
typedef int (*gf_cache_iter_cb_t)(const char *key,
                                  const gf_cache_entry_meta_t *meta,
                                  void *ctx);
int gf_cache_iterate(gf_cache_iter_cb_t callback, void *ctx);

/**
 * @brief Cache periodic processing
 */
int gf_cache_process(void);

#endif /* GF_CACHE_H */
