/**
 * @file kv_store.h
 * @brief Key-Value Store for Flash
 * 
 * WHAT: Simple, reliable key-value storage optimized for embedded flash.
 *       Power-fail safe updates with minimal RAM overhead.
 * 
 * WHY: Configuration and state persistence is common in embedded systems.
 *      A well-designed KV store shows understanding of flash constraints
 *      and data integrity requirements.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Configuration storage
 *   - Calibration data
 *   - User preferences
 *   - Device pairing data
 *   - Runtime state persistence
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Transactional updates
 *   - CRC/checksum integrity
 *   - Key hashing
 *   - Defragmentation
 */

#ifndef GF_KV_STORE_H
#define GF_KV_STORE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_KV_KEY_MAX_LEN       32
#define GF_KV_VALUE_MAX_LEN     256

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_KV_OK = 0,
    GF_KV_NOT_FOUND,
    GF_KV_NO_SPACE,
    GF_KV_INVALID_KEY,
    GF_KV_CORRUPT,
    GF_KV_ERROR
} gf_kv_error_t;

typedef struct {
    uint32_t        entry_count;
    uint32_t        bytes_used;
    uint32_t        bytes_free;
    uint32_t        defrag_needed;      /* Reclaimable bytes */
    uint32_t        read_count;
    uint32_t        write_count;
} gf_kv_stats_t;

/* Iterator for enumeration */
typedef struct {
    uint32_t        _internal[4];       /* Opaque state */
} gf_kv_iter_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize KV store
 * 
 * @param base_addr Flash region start
 * @param size      Size in bytes (must be multiple of sector size)
 */
int gf_kv_init(uint32_t base_addr, size_t size);

/**
 * @brief Read value by key
 */
gf_kv_error_t gf_kv_get(const char *key, void *value, size_t *len);

/**
 * @brief Read value with type hint
 */
gf_kv_error_t gf_kv_get_u32(const char *key, uint32_t *value);
gf_kv_error_t gf_kv_get_i32(const char *key, int32_t *value);
gf_kv_error_t gf_kv_get_str(const char *key, char *value, size_t max_len);

/**
 * @brief Write value by key
 */
gf_kv_error_t gf_kv_set(const char *key, const void *value, size_t len);

/**
 * @brief Write with type hint
 */
gf_kv_error_t gf_kv_set_u32(const char *key, uint32_t value);
gf_kv_error_t gf_kv_set_i32(const char *key, int32_t value);
gf_kv_error_t gf_kv_set_str(const char *key, const char *value);

/**
 * @brief Delete key
 */
gf_kv_error_t gf_kv_delete(const char *key);

/**
 * @brief Check if key exists
 */
bool gf_kv_exists(const char *key);

/**
 * @brief Iterate over all keys
 */
int gf_kv_iter_start(gf_kv_iter_t *iter);
bool gf_kv_iter_next(gf_kv_iter_t *iter, char *key, size_t key_max);

/**
 * @brief Defragment storage (reclaim deleted space)
 */
int gf_kv_defrag(void);

/**
 * @brief Erase all data
 */
int gf_kv_erase_all(void);

/**
 * @brief Get statistics
 */
void gf_kv_get_stats(gf_kv_stats_t *stats);

/**
 * @brief Commit pending writes (ensure durability)
 */
int gf_kv_commit(void);

/**
 * @brief Get driver descriptor
 */
const void* gf_kv_get_driver(void);

#endif /* GF_KV_STORE_H */
