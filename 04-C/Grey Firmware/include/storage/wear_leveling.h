/**
 * @file wear_leveling.h
 * @brief Flash Wear Leveling Layer
 * 
 * WHAT: Extends flash lifespan by distributing writes across sectors.
 *       Implements log-structured storage with garbage collection
 *       for frequently-written data.
 * 
 * WHY: Flash has limited write cycles (typically 10K-100K). Wear leveling
 *      is essential for data logging and configuration storage. Understanding
 *      these algorithms demonstrates embedded storage expertise.
 * 
 * INDUSTRY APPLICATIONS:
 *   - All flash-based storage applications
 *   - Data loggers, event recorders
 *   - Configuration storage with frequent updates
 *   - Filesystems on raw flash
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Log-structured writes
 *   - Garbage collection
 *   - Bad block management
 *   - Wear statistics tracking
 *   - Power-fail safe updates
 */

#ifndef GF_WEAR_LEVELING_H
#define GF_WEAR_LEVELING_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef struct {
    uint32_t        base_address;
    uint32_t        size;
    uint32_t        sector_size;
    uint32_t        page_size;
} gf_wl_config_t;

typedef struct {
    uint32_t        total_writes;
    uint32_t        gc_count;           /* Garbage collection runs */
    uint32_t        min_erase_count;
    uint32_t        max_erase_count;
    uint32_t        avg_erase_count;
    uint32_t        bad_sectors;
    uint8_t         wear_level_percent; /* 0 = even, 100 = uneven */
    uint32_t        free_space;
    uint32_t        used_space;
} gf_wl_stats_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

int gf_wl_init(const gf_wl_config_t *config);
int gf_wl_format(void);
int gf_wl_read(uint32_t logical_addr, void *data, size_t len);
int gf_wl_write(uint32_t logical_addr, const void *data, size_t len);
int gf_wl_erase(uint32_t logical_addr, size_t len);
void gf_wl_get_stats(gf_wl_stats_t *stats);
int gf_wl_gc(void);             /* Force garbage collection */
int gf_wl_flush(void);          /* Flush pending writes */
const void* gf_wl_get_driver(void);

#endif /* GF_WEAR_LEVELING_H */
