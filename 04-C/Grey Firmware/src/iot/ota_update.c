/**
 * @file ota_update.c
 * @brief OTA Update Stub Implementation
 * 
 * Compact implementation demonstrating OTA update architecture.
 * Production code would include full HTTP client, flash drivers,
 * and cryptographic verification.
 */

#include "grey_firmware.h"
#include "iot/ota_update.h"
#include "core/message_bus.h"
#include "core/error_handler.h"
#include "core/driver_registry.h"
#include <string.h>

/*===========================================================================*/
/* Memory Layout Constants                                                    */
/*===========================================================================*/

/*
 * FLASH LAYOUT for A/B partition scheme:
 * 
 * 0x0000_0000 - 0x0000_7FFF : Bootloader (32KB)
 * 0x0000_8000 - 0x0000_8FFF : Partition table (4KB)
 * 0x0000_9000 - 0x0004_8FFF : Slot A (256KB)
 * 0x0004_9000 - 0x0008_8FFF : Slot B (256KB)
 * 0x0008_9000 - 0x0008_FFFF : NVS/Config (28KB)
 * 
 * Active slot is determined by boot flags in partition table.
 */

#define FLASH_SLOT_A_ADDR       0x00009000
#define FLASH_SLOT_B_ADDR       0x00049000
#define FLASH_PARTITION_TABLE   0x00008000

/*===========================================================================*/
/* Private State                                                              */
/*===========================================================================*/

static struct {
    gf_ota_progress_t   progress;
    gf_ota_header_t     pending_header;
    gf_ota_progress_cb  progress_cb;
    void               *progress_cb_ctx;
    uint32_t            write_offset;
    uint8_t             active_slot;    /* 0=A, 1=B */
    bool                initialized;
} s_ota;

/* Current firmware version (set at build time) */
static const uint16_t FW_VERSION_MAJOR = GF_VERSION_MAJOR;
static const uint16_t FW_VERSION_MINOR = GF_VERSION_MINOR;
static const uint16_t FW_VERSION_PATCH = GF_VERSION_PATCH;

/*===========================================================================*/
/* Driver Interface                                                           */
/*===========================================================================*/

static int ota_drv_init(void *config) {
    (void)config;
    memset(&s_ota, 0, sizeof(s_ota));
    s_ota.active_slot = 0; /* Would read from partition table */
    s_ota.initialized = true;
    return 0;
}

static gf_driver_t s_ota_driver = {
    .name = "ota",
    .version = 0x0100,
    .capabilities = GF_DRV_CAP_ASYNC,
    .ops = { .init = ota_drv_init },
    .deps = { NULL }
};

/*===========================================================================*/
/* Flash Operations (Stubs)                                                   */
/*===========================================================================*/

/*
 * PRODUCTION NOTES:
 * 
 * Flash operations must handle:
 * - Sector erase before write (typically 4KB sectors)
 * - Page-aligned writes (typically 256 bytes)
 * - Write verification (read-back compare)
 * - Wear leveling considerations
 * - Power-fail safety (atomic flag updates)
 */

static int flash_erase_slot(uint8_t slot) {
    (void)slot;
    /* Would erase entire partition */
    return 0;
}

__attribute__((unused))
static int flash_write(uint32_t addr, const void *data, uint32_t len) {
    (void)addr; (void)data; (void)len;
    /* Would write to flash with verification */
    return 0;
}

static int flash_set_boot_slot(uint8_t slot) {
    (void)slot;
    /* Would update partition table atomically */
    return 0;
}

/*===========================================================================*/
/* Network Operations (Stubs)                                                 */
/*===========================================================================*/

static int http_get_header(const char *url, gf_ota_header_t *header) {
    (void)url; (void)header;
    /* Would fetch firmware header from server */
    return 0;
}

__attribute__((unused))
static int http_get_chunk(const char *url, uint32_t offset, 
                          uint8_t *buffer, uint32_t len) {
    (void)url; (void)offset; (void)buffer; (void)len;
    /* Would fetch firmware chunk */
    return 0;
}

/*===========================================================================*/
/* Update Helpers                                                             */
/*===========================================================================*/

static void notify_progress(void) {
    gf_msg_publish(GF_TOPIC_OTA_PROGRESS, &s_ota.progress, 
                  sizeof(gf_ota_progress_t), GF_MSG_QOS_FIRE_FORGET, 
                  GF_MSG_PRIO_NORMAL);
    
    if (s_ota.progress_cb) {
        s_ota.progress_cb(&s_ota.progress, s_ota.progress_cb_ctx);
    }
}

static bool verify_header(const gf_ota_header_t *header) {
    /* Check magic */
    if (header->magic != 0x47465755) return false;  /* "GFFU" */
    
    /* Check version is newer */
    if (header->version_major < FW_VERSION_MAJOR) return false;
    if (header->version_major == FW_VERSION_MAJOR &&
        header->version_minor < FW_VERSION_MINOR) return false;
    
    /* Check size */
    if (header->image_size > GF_OTA_MAX_IMAGE_SIZE) return false;
    
    return true;
}

/*===========================================================================*/
/* Public API                                                                 */
/*===========================================================================*/

int gf_ota_init(void) {
    return ota_drv_init(NULL);
}

int gf_ota_check_update(const char *url, bool *available, char *new_version) {
    gf_ota_header_t header;
    
    if (http_get_header(url, &header) != 0) {
        return -1;
    }
    
    *available = (header.version_major > FW_VERSION_MAJOR) ||
                 (header.version_major == FW_VERSION_MAJOR && 
                  header.version_minor > FW_VERSION_MINOR);
    
    if (new_version && *available) {
        /* Format version string */
        /* sprintf(new_version, "%d.%d.%d", ...) */
    }
    
    return 0;
}

int gf_ota_start_download(const char *url, gf_ota_progress_cb progress_cb, void *ctx) {
    if (s_ota.progress.state != GF_OTA_IDLE) {
        return -1;
    }
    
    s_ota.progress_cb = progress_cb;
    s_ota.progress_cb_ctx = ctx;
    
    /* Fetch header first */
    if (http_get_header(url, &s_ota.pending_header) != 0) {
        s_ota.progress.state = GF_OTA_ERROR;
        s_ota.progress.last_error = GF_OTA_ERR_NETWORK;
        return -2;
    }
    
    if (!verify_header(&s_ota.pending_header)) {
        s_ota.progress.state = GF_OTA_ERROR;
        s_ota.progress.last_error = GF_OTA_ERR_VERSION;
        return -3;
    }
    
    /* Erase inactive slot */
    uint8_t target_slot = 1 - s_ota.active_slot;
    flash_erase_slot(target_slot);
    
    /* Start download */
    s_ota.progress.state = GF_OTA_DOWNLOADING;
    s_ota.progress.bytes_total = s_ota.pending_header.image_size;
    s_ota.write_offset = 0;
    
    notify_progress();
    
    return 0;
}

int gf_ota_abort(void) {
    s_ota.progress.state = GF_OTA_IDLE;
    s_ota.write_offset = 0;
    return 0;
}

int gf_ota_apply(void) {
    if (s_ota.progress.state != GF_OTA_COMPLETE) {
        return -1;
    }
    
    /* Set boot slot to newly downloaded firmware */
    uint8_t target_slot = 1 - s_ota.active_slot;
    flash_set_boot_slot(target_slot);
    
    /* Trigger reboot */
    gf_system_reset("OTA update applied");
    
    return 0;
}

int gf_ota_confirm_boot(void) {
    /* Mark current slot as confirmed good */
    /* This prevents rollback on next boot */
    return 0;
}

void gf_ota_get_progress(gf_ota_progress_t *progress) {
    if (progress) {
        memcpy(progress, &s_ota.progress, sizeof(gf_ota_progress_t));
    }
}

void gf_ota_get_version(uint16_t *major, uint16_t *minor, uint16_t *patch) {
    if (major) *major = FW_VERSION_MAJOR;
    if (minor) *minor = FW_VERSION_MINOR;
    if (patch) *patch = FW_VERSION_PATCH;
}

bool gf_ota_can_rollback(void) {
    /* Check if previous slot has valid firmware */
    return true; /* Stub */
}

int gf_ota_rollback(void) {
    if (!gf_ota_can_rollback()) {
        return -1;
    }
    
    uint8_t target_slot = 1 - s_ota.active_slot;
    flash_set_boot_slot(target_slot);
    gf_system_reset("OTA rollback");
    
    return 0;
}

void gf_ota_process(void) {
    /*
     * Production implementation would:
     * - Fetch firmware in chunks
     * - Write chunks to flash
     * - Update progress
     * - Verify signature when complete
     */
}

const void* gf_ota_get_driver(void) {
    return &s_ota_driver;
}
