/**
 * @file ota_update.h
 * @brief Over-The-Air Firmware Update Stub
 * 
 * WHAT: Secure OTA update mechanism with differential updates, rollback
 *       protection, and A/B partition management.
 * 
 * WHY: Deployed devices need field updates to fix bugs, add features, and
 *      patch security vulnerabilities. A robust OTA system prevents bricking,
 *      ensures update integrity, and minimizes downtime. This capability is
 *      mandatory for connected devices in all industries.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - A/B partition scheme for atomic updates
 *      - Cryptographic signature verification
 *      - Delta/patch updates to minimize bandwidth
 *      - Rollback on boot failure detection
 * 
 * Industry applications: automotive ECUs, IoT devices, medical equipment
 * 
 * NOTE: This is an annotated stub. Production implementation requires
 *       careful flash layout and bootloader integration.
 */

#ifndef GF_OTA_UPDATE_H
#define GF_OTA_UPDATE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_OTA_BLOCK_SIZE           4096    /* Flash write block size */
#define GF_OTA_MAX_IMAGE_SIZE       (256 * 1024)  /* 256KB max */
#define GF_OTA_SIGNATURE_LEN        64      /* ECDSA-256 signature */
#define GF_OTA_HASH_LEN             32      /* SHA-256 hash */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_OTA_IDLE = 0,
    GF_OTA_DOWNLOADING,
    GF_OTA_VERIFYING,
    GF_OTA_APPLYING,
    GF_OTA_COMPLETE,
    GF_OTA_ERROR
} gf_ota_state_t;

typedef enum {
    GF_OTA_ERR_NONE = 0,
    GF_OTA_ERR_NO_SPACE,
    GF_OTA_ERR_SIGNATURE,
    GF_OTA_ERR_VERSION,
    GF_OTA_ERR_FLASH,
    GF_OTA_ERR_NETWORK,
    GF_OTA_ERR_TIMEOUT,
    GF_OTA_ERR_CORRUPTED
} gf_ota_error_t;

/* Firmware image header */
typedef struct {
    uint32_t    magic;              /* 0x47465755 "GFFU" */
    uint16_t    version_major;
    uint16_t    version_minor;
    uint16_t    version_patch;
    uint16_t    hw_compatibility;   /* Hardware revision bitmask */
    uint32_t    image_size;
    uint32_t    image_crc32;
    uint8_t     sha256_hash[GF_OTA_HASH_LEN];
    uint8_t     signature[GF_OTA_SIGNATURE_LEN];
    uint32_t    timestamp;
    uint32_t    flags;
} __attribute__((packed)) gf_ota_header_t;

/* Update progress info */
typedef struct {
    gf_ota_state_t  state;
    gf_ota_error_t  last_error;
    uint32_t        bytes_received;
    uint32_t        bytes_total;
    uint8_t         progress_percent;
} gf_ota_progress_t;

/* Progress callback */
typedef void (*gf_ota_progress_cb)(const gf_ota_progress_t *progress, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize OTA subsystem
 * @return GF_OK on success
 */
int gf_ota_init(void);

/**
 * @brief Check for available update
 * @param url Update server URL
 * @param available Output: true if update available
 * @param new_version Output: version string of available update
 * @return GF_OK on success
 */
int gf_ota_check_update(const char *url, bool *available, char *new_version);

/**
 * @brief Start firmware download
 * @param url Firmware image URL
 * @param progress_cb Optional progress callback
 * @param ctx Callback context
 * @return GF_OK if download started
 */
int gf_ota_start_download(const char *url, gf_ota_progress_cb progress_cb, void *ctx);

/**
 * @brief Abort in-progress update
 */
int gf_ota_abort(void);

/**
 * @brief Apply downloaded update (requires reboot)
 * @return GF_OK if ready to apply
 */
int gf_ota_apply(void);

/**
 * @brief Mark current firmware as good (prevent rollback)
 */
int gf_ota_confirm_boot(void);

/**
 * @brief Get current update progress
 */
void gf_ota_get_progress(gf_ota_progress_t *progress);

/**
 * @brief Get running firmware version
 */
void gf_ota_get_version(uint16_t *major, uint16_t *minor, uint16_t *patch);

/**
 * @brief Check if rollback is available
 */
bool gf_ota_can_rollback(void);

/**
 * @brief Trigger rollback to previous firmware
 */
int gf_ota_rollback(void);

/**
 * @brief Process OTA events (call from main loop)
 */
void gf_ota_process(void);

/**
 * @brief Get OTA driver descriptor for registration
 */
const void* gf_ota_get_driver(void);

#endif /* GF_OTA_UPDATE_H */
