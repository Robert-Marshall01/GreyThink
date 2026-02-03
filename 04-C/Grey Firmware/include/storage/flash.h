/**
 * @file flash.h
 * @brief Flash Memory Abstraction Layer
 * 
 * WHAT: Unified interface for internal flash, external NOR/NAND flash,
 *       and EEPROM. Handles erase blocks, page programming, and
 *       read-while-write restrictions.
 * 
 * WHY: Flash programming requires understanding sector sizes, wear
 *      limitations, and timing constraints. A good abstraction layer
 *      demonstrates ability to design hardware-independent firmware.
 * 
 * INDUSTRY APPLICATIONS:
 *   - All embedded: Firmware storage, configuration, logging
 *   - IoT: OTA update storage, credential storage
 *   - Automotive: Calibration data, event logging
 *   - Industrial: Recipe storage, production data
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Sector/block erase requirements
 *   - Page-aligned writes
 *   - Read-while-write handling
 *   - Memory-mapped vs command-based access
 *   - Timeout and error handling
 */

#ifndef GF_FLASH_H
#define GF_FLASH_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_FLASH_MAX_REGIONS        8

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_FLASH_TYPE_INTERNAL = 0, /* MCU internal flash */
    GF_FLASH_TYPE_NOR,          /* External NOR (SPI/QSPI) */
    GF_FLASH_TYPE_NAND,         /* External NAND */
    GF_FLASH_TYPE_EEPROM        /* EEPROM (byte-addressable) */
} gf_flash_type_t;

typedef struct {
    gf_flash_type_t type;
    uint32_t        base_address;
    uint32_t        total_size;
    uint32_t        sector_size;        /* Erase unit */
    uint32_t        page_size;          /* Write unit */
    uint32_t        write_time_us;
    uint32_t        erase_time_ms;
    bool            memory_mapped;      /* Direct read access */
} gf_flash_info_t;

typedef enum {
    GF_FLASH_OK = 0,
    GF_FLASH_ERR_ALIGN,         /* Address/size alignment error */
    GF_FLASH_ERR_BUSY,          /* Flash busy (write in progress) */
    GF_FLASH_ERR_PROTECTED,     /* Write protection enabled */
    GF_FLASH_ERR_VERIFY,        /* Verify after write failed */
    GF_FLASH_ERR_TIMEOUT,
    GF_FLASH_ERR_INVALID
} gf_flash_error_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize flash subsystem
 */
int gf_flash_init(void);

/**
 * @brief Get information about a flash region
 */
int gf_flash_get_info(uint8_t region, gf_flash_info_t *info);

/**
 * @brief Read from flash
 */
gf_flash_error_t gf_flash_read(uint32_t address, void *data, size_t len);

/**
 * @brief Program (write) to flash
 * 
 * Caller must ensure target is erased. Address and len must be
 * page-aligned for most flash types.
 */
gf_flash_error_t gf_flash_program(uint32_t address, const void *data, size_t len);

/**
 * @brief Erase sector(s) containing address range
 */
gf_flash_error_t gf_flash_erase(uint32_t address, size_t len);

/**
 * @brief Erase entire chip
 */
gf_flash_error_t gf_flash_chip_erase(uint8_t region);

/**
 * @brief Check if address range is erased (all 0xFF)
 */
bool gf_flash_is_erased(uint32_t address, size_t len);

/**
 * @brief Set/clear write protection
 */
int gf_flash_set_protection(uint32_t address, size_t len, bool protect);

/**
 * @brief Get driver descriptor
 */
const void* gf_flash_get_driver(void);

#endif /* GF_FLASH_H */
