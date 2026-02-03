/**
 * @file secure_boot.h
 * @brief Secure Bootloader - SPOTLIGHT SUBSYSTEM
 * 
 * WHAT: Production-grade secure bootloader with cryptographic signature
 *       verification, chain of trust, and rollback protection.
 * 
 * WHY: Secure boot is the foundation of device security. A compromised
 *      bootloader allows complete system takeover, persisting across
 *      firmware updates and factory resets. Secure boot prevents:
 *      - Malicious firmware installation
 *      - Downgrade attacks
 *      - Physical flash modification
 *      
 *      Senior firmware roles require deep understanding of:
 *      - Chain of trust from ROM to application
 *      - Cryptographic operations (ECDSA, SHA-256, AES)
 *      - Anti-rollback mechanisms
 *      - Secure key storage
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Complete memory layout for bootloader, slots, and metadata
 *      - ECDSA-P256 signature verification
 *      - SHA-256 image integrity check
 *      - Secure counter-based anti-rollback
 *      - Boot failure detection and recovery
 *      - Debug lock for production builds
 * 
 * Industry applications: IoT security, automotive, banking terminals, medical
 */

#ifndef GF_SECURE_BOOT_H
#define GF_SECURE_BOOT_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Memory Layout                                                              */
/*===========================================================================*/

/**
 * FLASH MEMORY MAP:
 * 
 * ┌─────────────────────────────────────────────────┐ 0x0800_0000
 * │  Bootloader (32 KB)                             │
 * │  - Vector table, secure boot code               │
 * │  - Public key for signature verification        │
 * ├─────────────────────────────────────────────────┤ 0x0800_8000
 * │  Bootloader Data (4 KB)                         │
 * │  - Boot configuration                           │
 * │  - Anti-rollback counter                        │
 * │  - Boot status flags                            │
 * ├─────────────────────────────────────────────────┤ 0x0800_9000
 * │  Firmware Slot A (256 KB)                       │
 * │  - Image header (512 bytes)                     │
 * │  - Application code                             │
 * │  - Signature block                              │
 * ├─────────────────────────────────────────────────┤ 0x0804_9000
 * │  Firmware Slot B (256 KB)                       │
 * │  - Image header (512 bytes)                     │
 * │  - Application code                             │
 * │  - Signature block                              │
 * ├─────────────────────────────────────────────────┤ 0x0808_9000
 * │  NVS / User Data (28 KB)                        │
 * │  - Calibration, configuration                   │
 * └─────────────────────────────────────────────────┘ 0x0808_FFFF
 */

#define GF_BOOT_FLASH_BASE          0x08000000
#define GF_BOOT_BOOTLOADER_ADDR     0x08000000
#define GF_BOOT_BOOTLOADER_SIZE     0x00008000  /* 32 KB */
#define GF_BOOT_DATA_ADDR           0x08008000
#define GF_BOOT_DATA_SIZE           0x00001000  /* 4 KB */
#define GF_BOOT_SLOT_A_ADDR         0x08009000
#define GF_BOOT_SLOT_B_ADDR         0x08049000
#define GF_BOOT_SLOT_SIZE           0x00040000  /* 256 KB */
#define GF_BOOT_NVS_ADDR            0x08089000

/*===========================================================================*/
/* Cryptographic Constants                                                    */
/*===========================================================================*/

#define GF_BOOT_HASH_SIZE           32          /* SHA-256 */
#define GF_BOOT_SIGNATURE_SIZE      64          /* ECDSA-P256 (r,s) */
#define GF_BOOT_PUBKEY_SIZE         64          /* ECDSA-P256 public key */
#define GF_BOOT_AES_KEY_SIZE        32          /* AES-256 for optional encryption */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/**
 * Boot status codes
 */
typedef enum {
    GF_BOOT_OK = 0,
    GF_BOOT_ERR_NO_IMAGE,           /* No valid image found */
    GF_BOOT_ERR_INVALID_HEADER,     /* Image header malformed */
    GF_BOOT_ERR_HASH_MISMATCH,      /* SHA-256 hash failed */
    GF_BOOT_ERR_SIGNATURE,          /* ECDSA signature invalid */
    GF_BOOT_ERR_ROLLBACK,           /* Version rollback detected */
    GF_BOOT_ERR_SLOT_EMPTY,         /* Slot has no image */
    GF_BOOT_ERR_STARTUP_FAIL,       /* Application didn't confirm boot */
    GF_BOOT_ERR_HW_FAULT,           /* Hardware error during boot */
    GF_BOOT_ERR_KEY_INVALID         /* Public key not valid */
} gf_boot_result_t;

/**
 * Slot status
 */
typedef enum {
    GF_SLOT_EMPTY = 0,              /* No image */
    GF_SLOT_PENDING,                /* Image downloaded, not verified */
    GF_SLOT_VALID,                  /* Image verified, bootable */
    GF_SLOT_ACTIVE,                 /* Currently booted from this slot */
    GF_SLOT_CONFIRMED,              /* Boot confirmed good */
    GF_SLOT_INVALID                 /* Verification failed */
} gf_slot_status_t;

/**
 * Image header structure (512 bytes, page-aligned for flash)
 */
typedef struct {
    uint32_t    magic;              /* 0x47464657 "GFFW" */
    uint32_t    header_size;        /* Size of this header */
    uint32_t    image_size;         /* Size of application code */
    uint32_t    entry_point;        /* Application entry address */
    uint32_t    vector_table;       /* Vector table address */
    
    /* Version info (for anti-rollback) */
    uint16_t    version_major;
    uint16_t    version_minor;
    uint16_t    version_patch;
    uint16_t    version_build;
    uint32_t    security_version;   /* Monotonic counter for rollback */
    
    /* Hardware compatibility */
    uint32_t    hw_id;              /* Hardware ID this image supports */
    uint32_t    hw_version_min;     /* Minimum hardware version */
    uint32_t    hw_version_max;     /* Maximum hardware version */
    
    /* Timestamps and metadata */
    uint32_t    build_timestamp;    /* Unix timestamp of build */
    uint8_t     build_id[16];       /* Git commit hash or build ID */
    
    /* Cryptographic fields */
    uint8_t     image_hash[GF_BOOT_HASH_SIZE];      /* SHA-256 of image */
    uint8_t     header_hash[GF_BOOT_HASH_SIZE];     /* SHA-256 of header (excl signatures) */
    uint8_t     signature[GF_BOOT_SIGNATURE_SIZE];  /* ECDSA signature of header_hash */
    
    /* Dependencies (optional) */
    uint32_t    min_bootloader_version;
    
    /* Flags */
    uint32_t    flags;
    
    /* Padding to 512 bytes */
    uint8_t     reserved[312];
} __attribute__((packed)) gf_image_header_t;

/* Compile-time size check - header is 512 bytes for flash alignment */
_Static_assert(sizeof(gf_image_header_t) == 512, "Image header must be 512 bytes");

/**
 * Header flags
 */
#define GF_BOOT_FLAG_ENCRYPTED      (1 << 0)    /* Image is encrypted */
#define GF_BOOT_FLAG_COMPRESSED     (1 << 1)    /* Image is compressed */
#define GF_BOOT_FLAG_DEBUG          (1 << 2)    /* Debug build */
#define GF_BOOT_FLAG_SECURE_ONLY    (1 << 3)    /* Refuse boot on debug HW */

/**
 * Boot data (stored in protected flash region)
 */
typedef struct {
    uint32_t    magic;              /* 0x424F4F54 "BOOT" */
    uint8_t     active_slot;        /* 0=A, 1=B */
    uint8_t     boot_attempts;      /* Failed boot counter */
    uint8_t     max_attempts;       /* Max before rollback */
    uint8_t     flags;
    gf_slot_status_t slot_status[2];
    uint32_t    security_version;   /* Current minimum security version */
    uint32_t    boot_count;         /* Total boots */
    uint32_t    last_boot_time;     /* Timestamp of last boot */
    gf_boot_result_t last_error;    /* Last boot error code */
    uint32_t    crc32;              /* Data integrity */
} gf_boot_data_t;

/**
 * Boot information (returned by bootloader query)
 */
typedef struct {
    uint8_t             active_slot;
    gf_slot_status_t    slot_a_status;
    gf_slot_status_t    slot_b_status;
    uint32_t            slot_a_version;
    uint32_t            slot_b_version;
    uint32_t            security_version;
    uint32_t            boot_count;
    gf_boot_result_t    last_error;
    bool                boot_confirmed;
    bool                debug_enabled;
} gf_boot_info_t;

/*===========================================================================*/
/* API Functions - Bootloader Stage                                           */
/*===========================================================================*/

/**
 * @brief Bootloader main entry point
 * 
 * Called from reset vector. Performs secure boot sequence:
 * 1. Initialize minimal hardware (clocks, flash)
 * 2. Verify bootloader integrity (if hardware supports)
 * 3. Select boot slot based on configuration
 * 4. Verify image signature
 * 5. Check anti-rollback version
 * 6. Jump to application
 */
void gf_boot_main(void);

/**
 * @brief Verify image in specified slot
 * @param slot Slot number (0=A, 1=B)
 * @return GF_BOOT_OK if valid
 */
gf_boot_result_t gf_boot_verify_slot(uint8_t slot);

/**
 * @brief Get image header from slot
 */
gf_boot_result_t gf_boot_get_header(uint8_t slot, gf_image_header_t *header);

/**
 * @brief Jump to application
 * @param slot Slot to boot from
 */
void gf_boot_jump_to_app(uint8_t slot);

/**
 * @brief Enter recovery mode (serial bootloader)
 */
void gf_boot_enter_recovery(void);

/*===========================================================================*/
/* API Functions - Application Stage                                          */
/*===========================================================================*/

/**
 * @brief Initialize secure boot interface (from application)
 */
int gf_boot_init(void);

/**
 * @brief Confirm successful boot (call after app starts)
 * 
 * If not called within timeout, bootloader will try other slot on reset.
 */
int gf_boot_confirm(void);

/**
 * @brief Get boot information
 */
void gf_boot_get_info(gf_boot_info_t *info);

/**
 * @brief Set active slot for next boot
 * @param slot Slot to boot (0=A, 1=B)
 */
int gf_boot_set_active_slot(uint8_t slot);

/**
 * @brief Mark a slot as invalid (force other slot)
 */
int gf_boot_invalidate_slot(uint8_t slot);

/**
 * @brief Request reboot to bootloader recovery mode
 */
int gf_boot_request_recovery(void);

/**
 * @brief Verify image before applying OTA update
 */
gf_boot_result_t gf_boot_verify_update(const gf_image_header_t *header,
                                        const uint8_t *image, uint32_t size);

/**
 * @brief Update security version after successful boot
 * 
 * This burns the anti-rollback version, preventing boot of older images.
 */
int gf_boot_update_security_version(uint32_t version);

/**
 * @brief Check if debug is allowed
 */
bool gf_boot_debug_allowed(void);

/**
 * @brief Lock debug permanently (production)
 */
int gf_boot_lock_debug(void);

/*===========================================================================*/
/* Cryptographic Functions                                                    */
/*===========================================================================*/

/**
 * @brief Verify ECDSA-P256 signature
 * @param hash SHA-256 hash to verify
 * @param signature ECDSA signature (64 bytes: r || s)
 * @param public_key Public key (64 bytes: x || y)
 * @return true if signature valid
 */
bool gf_boot_verify_ecdsa(const uint8_t *hash, const uint8_t *signature,
                           const uint8_t *public_key);

/**
 * @brief Calculate SHA-256 hash
 * @param data Input data
 * @param len Data length
 * @param hash Output hash (32 bytes)
 */
void gf_boot_sha256(const uint8_t *data, uint32_t len, uint8_t *hash);

/**
 * @brief Secure memory comparison (constant-time)
 */
bool gf_boot_secure_compare(const void *a, const void *b, uint32_t len);

/**
 * @brief Get secure boot driver descriptor
 */
const void* gf_boot_get_driver(void);

#endif /* GF_SECURE_BOOT_H */
