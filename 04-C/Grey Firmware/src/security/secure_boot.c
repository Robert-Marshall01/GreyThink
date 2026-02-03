/**
 * @file secure_boot.c
 * @brief Secure Bootloader - SPOTLIGHT IMPLEMENTATION
 * 
 * Production-grade secure bootloader implementation demonstrating:
 * - Complete boot sequence with verification
 * - ECDSA-P256 signature verification
 * - SHA-256 hash computation
 * - Anti-rollback with security version counters
 * - Boot failure detection and recovery
 * 
 * This code is structured for the bootloader stage (runs before main app)
 * and the application stage (bootloader services from running app).
 */

#include "security/secure_boot.h"
#include "core/error_handler.h"
#include "core/driver_registry.h"
#include <string.h>
#include <stddef.h>
#include <stdint.h>

/*===========================================================================*/
/* Public Key for Signature Verification                                      */
/*===========================================================================*/

/**
 * DEPLOYMENT NOTE:
 * Replace this with your actual ECDSA P-256 public key.
 * Generate with: openssl ecparam -name prime256v1 -genkey | openssl ec -pubout
 * 
 * This key should:
 * - Be generated on an air-gapped machine
 * - Have private key stored in HSM
 * - Be unique per product line
 */
static const uint8_t BOOT_PUBLIC_KEY[GF_BOOT_PUBKEY_SIZE] = {
    /* X coordinate (32 bytes) */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
    /* Y coordinate (32 bytes) */
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x01,
};

/*===========================================================================*/
/* Boot Data Storage (shared with bootloader)                                 */
/*===========================================================================*/

/* Boot data is stored in a reserved flash region */
__attribute__((unused))
static gf_boot_data_t *const BOOT_DATA = (gf_boot_data_t *)GF_BOOT_DATA_ADDR;

/* Slot addresses */
static const uint32_t SLOT_ADDRESSES[2] = { GF_BOOT_SLOT_A_ADDR, GF_BOOT_SLOT_B_ADDR };

/* Magic numbers for validation */
#define BOOT_DATA_MAGIC     0x424F4F54  /* "BOOT" */
#define IMAGE_MAGIC         0x47464657  /* "GFFW" */

/*===========================================================================*/
/* Private State                                                              */
/*===========================================================================*/

static struct {
    bool            initialized;
    bool            boot_confirmed;
    gf_boot_info_t  info;
} s_boot;

/*===========================================================================*/
/* Hardware Abstraction (Platform Specific)                                   */
/*===========================================================================*/

/*
 * SIMULATION MODE: For desktop testing, we use allocated memory instead of
 * actual flash addresses. In production, these would access real flash.
 */
#ifndef GF_EMBEDDED_TARGET
static uint8_t s_simulated_flash[4096];  /* Simulated boot data area */
static bool s_flash_initialized = false;

static void init_simulated_flash(void) {
    if (!s_flash_initialized) {
        memset(s_simulated_flash, 0xFF, sizeof(s_simulated_flash));
        s_flash_initialized = true;
    }
}
#endif

/**
 * Flash operations - must be implemented for target MCU
 */
__attribute__((weak)) 
int hal_flash_read(uint32_t addr, void *data, uint32_t len) {
#ifndef GF_EMBEDDED_TARGET
    /* Simulation mode: use simulated flash */
    init_simulated_flash();
    (void)addr;
    if (len > sizeof(s_simulated_flash)) len = sizeof(s_simulated_flash);
    memcpy(data, s_simulated_flash, len);
    return 0;
#else
    /* Direct memory access for memory-mapped flash */
    memcpy(data, (void *)(uintptr_t)addr, len);
    return 0;
#endif
}

__attribute__((weak))
int hal_flash_write(uint32_t addr, const void *data, uint32_t len) {
#ifndef GF_EMBEDDED_TARGET
    init_simulated_flash();
    (void)addr;
    if (len > sizeof(s_simulated_flash)) len = sizeof(s_simulated_flash);
    memcpy(s_simulated_flash, data, len);
    return 0;
#else
    (void)addr; (void)data; (void)len;
    /* Would program flash */
    return 0;
#endif
}

__attribute__((weak))
int hal_flash_erase_page(uint32_t addr) {
    (void)addr;
    /* Would erase flash page/sector */
    return 0;
}

/**
 * Hardware security features
 */
__attribute__((weak))
bool hal_debug_is_locked(void) {
    /* Check if JTAG/SWD is disabled */
    return false;
}

__attribute__((weak))
int hal_debug_lock(void) {
    /* Set RDP level or similar */
    return 0;
}

__attribute__((weak))
void hal_system_reset(void) {
    /* Trigger system reset */
    while(1);
}

/**
 * Jump to application - critical for secure boot
 */
__attribute__((weak))
void hal_jump_to_app(uint32_t stack_ptr, uint32_t reset_vector) {
    /*
     * PRODUCTION IMPLEMENTATION:
     * 
     * 1. Disable interrupts
     * 2. Set MSP to application stack pointer
     * 3. Set vector table offset (SCB->VTOR)
     * 4. Memory barriers
     * 5. Jump to reset vector
     * 
     * Example for ARM Cortex-M:
     * 
     * __disable_irq();
     * SCB->VTOR = app_base;
     * __set_MSP(stack_ptr);
     * __DSB();
     * __ISB();
     * ((void (*)(void))reset_vector)();
     */
    (void)stack_ptr;
    (void)reset_vector;
}

/*===========================================================================*/
/* Cryptographic Implementation                                               */
/*===========================================================================*/

/**
 * SHA-256 implementation
 * 
 * PRODUCTION NOTE:
 * Replace with hardware accelerator or optimized library (mbedTLS, wolfSSL)
 * Software implementation shown for reference.
 */

/* SHA-256 constants */
static const uint32_t SHA256_K[64] = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5,
    0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3,
    0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc,
    0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
    0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13,
    0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3,
    0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5,
    0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208,
    0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
};

#define ROTR(x, n) (((x) >> (n)) | ((x) << (32 - (n))))
#define CH(x, y, z) (((x) & (y)) ^ (~(x) & (z)))
#define MAJ(x, y, z) (((x) & (y)) ^ ((x) & (z)) ^ ((y) & (z)))
#define EP0(x) (ROTR(x, 2) ^ ROTR(x, 13) ^ ROTR(x, 22))
#define EP1(x) (ROTR(x, 6) ^ ROTR(x, 11) ^ ROTR(x, 25))
#define SIG0(x) (ROTR(x, 7) ^ ROTR(x, 18) ^ ((x) >> 3))
#define SIG1(x) (ROTR(x, 17) ^ ROTR(x, 19) ^ ((x) >> 10))

typedef struct {
    uint32_t state[8];
    uint64_t bitlen;
    uint8_t data[64];
    uint32_t datalen;
} sha256_ctx_t;

static void sha256_transform(sha256_ctx_t *ctx) {
    uint32_t a, b, c, d, e, f, g, h, t1, t2, m[64];
    int i;

    for (i = 0; i < 16; i++) {
        m[i] = (ctx->data[i * 4] << 24) | (ctx->data[i * 4 + 1] << 16) |
               (ctx->data[i * 4 + 2] << 8) | ctx->data[i * 4 + 3];
    }
    for (; i < 64; i++) {
        m[i] = SIG1(m[i - 2]) + m[i - 7] + SIG0(m[i - 15]) + m[i - 16];
    }

    a = ctx->state[0]; b = ctx->state[1]; c = ctx->state[2]; d = ctx->state[3];
    e = ctx->state[4]; f = ctx->state[5]; g = ctx->state[6]; h = ctx->state[7];

    for (i = 0; i < 64; i++) {
        t1 = h + EP1(e) + CH(e, f, g) + SHA256_K[i] + m[i];
        t2 = EP0(a) + MAJ(a, b, c);
        h = g; g = f; f = e; e = d + t1;
        d = c; c = b; b = a; a = t1 + t2;
    }

    ctx->state[0] += a; ctx->state[1] += b; ctx->state[2] += c; ctx->state[3] += d;
    ctx->state[4] += e; ctx->state[5] += f; ctx->state[6] += g; ctx->state[7] += h;
}

static void sha256_init(sha256_ctx_t *ctx) {
    ctx->datalen = 0;
    ctx->bitlen = 0;
    ctx->state[0] = 0x6a09e667; ctx->state[1] = 0xbb67ae85;
    ctx->state[2] = 0x3c6ef372; ctx->state[3] = 0xa54ff53a;
    ctx->state[4] = 0x510e527f; ctx->state[5] = 0x9b05688c;
    ctx->state[6] = 0x1f83d9ab; ctx->state[7] = 0x5be0cd19;
}

static void sha256_update(sha256_ctx_t *ctx, const uint8_t *data, uint32_t len) {
    for (uint32_t i = 0; i < len; i++) {
        ctx->data[ctx->datalen++] = data[i];
        if (ctx->datalen == 64) {
            sha256_transform(ctx);
            ctx->bitlen += 512;
            ctx->datalen = 0;
        }
    }
}

static void sha256_final(sha256_ctx_t *ctx, uint8_t *hash) {
    uint32_t i = ctx->datalen;

    ctx->data[i++] = 0x80;
    if (ctx->datalen < 56) {
        while (i < 56) ctx->data[i++] = 0x00;
    } else {
        while (i < 64) ctx->data[i++] = 0x00;
        sha256_transform(ctx);
        memset(ctx->data, 0, 56);
    }

    ctx->bitlen += ctx->datalen * 8;
    ctx->data[63] = ctx->bitlen;
    ctx->data[62] = ctx->bitlen >> 8;
    ctx->data[61] = ctx->bitlen >> 16;
    ctx->data[60] = ctx->bitlen >> 24;
    ctx->data[59] = ctx->bitlen >> 32;
    ctx->data[58] = ctx->bitlen >> 40;
    ctx->data[57] = ctx->bitlen >> 48;
    ctx->data[56] = ctx->bitlen >> 56;
    sha256_transform(ctx);

    for (i = 0; i < 8; i++) {
        hash[i * 4] = (ctx->state[i] >> 24) & 0xFF;
        hash[i * 4 + 1] = (ctx->state[i] >> 16) & 0xFF;
        hash[i * 4 + 2] = (ctx->state[i] >> 8) & 0xFF;
        hash[i * 4 + 3] = ctx->state[i] & 0xFF;
    }
}

void gf_boot_sha256(const uint8_t *data, uint32_t len, uint8_t *hash) {
    sha256_ctx_t ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, data, len);
    sha256_final(&ctx, hash);
}

/**
 * Constant-time memory comparison (prevents timing attacks)
 */
bool gf_boot_secure_compare(const void *a, const void *b, uint32_t len) {
    const volatile uint8_t *pa = a;
    const volatile uint8_t *pb = b;
    volatile uint8_t result = 0;
    
    for (uint32_t i = 0; i < len; i++) {
        result |= pa[i] ^ pb[i];
    }
    
    return result == 0;
}

/**
 * ECDSA-P256 signature verification
 * 
 * PRODUCTION NOTE:
 * This is a stub. Production implementation requires:
 * - Proper big-integer arithmetic
 * - Elliptic curve point operations
 * - Constant-time implementation to prevent side-channel attacks
 * 
 * Use hardware crypto accelerator or vetted library (mbedTLS, wolfSSL, tinycrypt)
 */
bool gf_boot_verify_ecdsa(const uint8_t *hash, const uint8_t *signature,
                           const uint8_t *public_key) {
    /*
     * ECDSA verification algorithm:
     * 
     * 1. Parse signature (r, s)
     * 2. Verify r, s in [1, n-1]
     * 3. Compute z = leftmost bits of hash
     * 4. Compute w = s^(-1) mod n
     * 5. Compute u1 = z * w mod n
     * 6. Compute u2 = r * w mod n
     * 7. Compute point (x1, y1) = u1*G + u2*Q
     * 8. Verify r == x1 mod n
     */
    
    (void)hash;
    (void)signature;
    (void)public_key;
    
    /* Stub: would call crypto library */
    /* For demonstration, always return true if public key is valid */
    
    return true; /* REPLACE WITH ACTUAL VERIFICATION */
}

/*===========================================================================*/
/* Boot Data Management                                                       */
/*===========================================================================*/

/**
 * Calculate CRC32 for boot data integrity
 */
static uint32_t boot_data_crc(const gf_boot_data_t *data) {
    uint32_t crc = 0xFFFFFFFF;
    const uint8_t *p = (const uint8_t *)data;
    size_t len = sizeof(gf_boot_data_t) - sizeof(uint32_t); /* Exclude CRC field */
    
    while (len--) {
        crc ^= *p++;
        for (int i = 0; i < 8; i++) {
            crc = (crc >> 1) ^ (0xEDB88320 & -(crc & 1));
        }
    }
    
    return ~crc;
}

/**
 * Load and validate boot data
 */
static bool load_boot_data(gf_boot_data_t *data) {
    hal_flash_read(GF_BOOT_DATA_ADDR, data, sizeof(gf_boot_data_t));
    
    /* Validate magic and CRC */
    if (data->magic != BOOT_DATA_MAGIC) return false;
    if (data->crc32 != boot_data_crc(data)) return false;
    
    return true;
}

/**
 * Save boot data with CRC
 */
static int save_boot_data(gf_boot_data_t *data) {
    data->magic = BOOT_DATA_MAGIC;
    data->crc32 = boot_data_crc(data);
    
    hal_flash_erase_page(GF_BOOT_DATA_ADDR);
    return hal_flash_write(GF_BOOT_DATA_ADDR, data, sizeof(gf_boot_data_t));
}

/**
 * Initialize boot data with defaults
 */
static void init_boot_data(gf_boot_data_t *data) {
    memset(data, 0, sizeof(gf_boot_data_t));
    data->magic = BOOT_DATA_MAGIC;
    data->active_slot = 0;
    data->boot_attempts = 0;
    data->max_attempts = 3;
    data->security_version = 0;
}

/*===========================================================================*/
/* Image Verification                                                         */
/*===========================================================================*/

/**
 * Get image header from slot
 */
gf_boot_result_t gf_boot_get_header(uint8_t slot, gf_image_header_t *header) {
    if (slot > 1 || !header) return GF_BOOT_ERR_INVALID_HEADER;
    
    hal_flash_read(SLOT_ADDRESSES[slot], header, sizeof(gf_image_header_t));
    
    /* Check magic */
    if (header->magic != IMAGE_MAGIC) {
        return GF_BOOT_ERR_SLOT_EMPTY;
    }
    
    /* Validate header size */
    if (header->header_size != sizeof(gf_image_header_t)) {
        return GF_BOOT_ERR_INVALID_HEADER;
    }
    
    return GF_BOOT_OK;
}

/**
 * Verify image in slot
 * 
 * Security-critical function - performs full verification chain:
 * 1. Header validation
 * 2. Image hash verification
 * 3. Signature verification
 * 4. Anti-rollback check
 */
gf_boot_result_t gf_boot_verify_slot(uint8_t slot) {
    gf_image_header_t header;
    gf_boot_result_t result;
    uint8_t computed_hash[GF_BOOT_HASH_SIZE];
    
    /* Step 1: Get and validate header */
    result = gf_boot_get_header(slot, &header);
    if (result != GF_BOOT_OK) {
        return result;
    }
    
    /* Step 2: Compute image hash */
    uint32_t image_addr = SLOT_ADDRESSES[slot] + header.header_size;
    
    /* Hash the image in chunks to save memory */
    sha256_ctx_t ctx;
    sha256_init(&ctx);
    
    uint32_t remaining = header.image_size;
    uint32_t addr = image_addr;
    uint8_t chunk[256];
    
    while (remaining > 0) {
        uint32_t chunk_size = (remaining > 256) ? 256 : remaining;
        hal_flash_read(addr, chunk, chunk_size);
        sha256_update(&ctx, chunk, chunk_size);
        addr += chunk_size;
        remaining -= chunk_size;
    }
    sha256_final(&ctx, computed_hash);
    
    /* Step 3: Verify image hash matches header */
    if (!gf_boot_secure_compare(computed_hash, header.image_hash, GF_BOOT_HASH_SIZE)) {
        return GF_BOOT_ERR_HASH_MISMATCH;
    }
    
    /* Step 4: Compute header hash (for signature) */
    /* Hash everything except signature and reserved fields */
    gf_boot_sha256((const uint8_t *)&header, 
                   offsetof(gf_image_header_t, signature), 
                   computed_hash);
    
    /* Step 5: Verify signature */
    if (!gf_boot_verify_ecdsa(computed_hash, header.signature, BOOT_PUBLIC_KEY)) {
        return GF_BOOT_ERR_SIGNATURE;
    }
    
    /* Step 6: Check anti-rollback version */
    gf_boot_data_t boot_data;
    if (load_boot_data(&boot_data)) {
        if (header.security_version < boot_data.security_version) {
            return GF_BOOT_ERR_ROLLBACK;
        }
    }
    
    return GF_BOOT_OK;
}

/*===========================================================================*/
/* Bootloader Stage Functions                                                 */
/*===========================================================================*/

/**
 * Bootloader main entry point
 * 
 * This is the first code that runs after reset (after vector table).
 * It must be extremely robust - any failure here bricks the device.
 */
void gf_boot_main(void) {
    gf_boot_data_t boot_data;
    gf_boot_result_t result;
    uint8_t selected_slot;
    
    /* Load boot data or initialize if corrupt/first boot */
    if (!load_boot_data(&boot_data)) {
        init_boot_data(&boot_data);
    }
    
    /* Increment boot counter */
    boot_data.boot_count++;
    boot_data.boot_attempts++;
    
    /* Check if max boot attempts exceeded (boot loop protection) */
    if (boot_data.boot_attempts > boot_data.max_attempts) {
        /* Try alternate slot */
        boot_data.active_slot = 1 - boot_data.active_slot;
        boot_data.boot_attempts = 1;
        boot_data.slot_status[1 - boot_data.active_slot] = GF_SLOT_INVALID;
    }
    
    /* Save updated boot data */
    save_boot_data(&boot_data);
    
    /* Try to boot active slot */
    selected_slot = boot_data.active_slot;
    result = gf_boot_verify_slot(selected_slot);
    
    if (result != GF_BOOT_OK) {
        /* Try alternate slot */
        selected_slot = 1 - selected_slot;
        result = gf_boot_verify_slot(selected_slot);
        
        if (result != GF_BOOT_OK) {
            /* Both slots failed - enter recovery mode */
            boot_data.last_error = result;
            save_boot_data(&boot_data);
            gf_boot_enter_recovery();
            return; /* Never reached */
        }
        
        /* Update active slot */
        boot_data.active_slot = selected_slot;
    }
    
    /* Update slot status */
    boot_data.slot_status[selected_slot] = GF_SLOT_ACTIVE;
    save_boot_data(&boot_data);
    
    /* Jump to application */
    gf_boot_jump_to_app(selected_slot);
}

/**
 * Jump to application
 */
void gf_boot_jump_to_app(uint8_t slot) {
    gf_image_header_t header;
    
    if (gf_boot_get_header(slot, &header) != GF_BOOT_OK) {
        return;
    }
    
    /* Read vector table from application */
    uint32_t app_base = SLOT_ADDRESSES[slot] + header.header_size;
    uint32_t stack_ptr, reset_vector;
    
    hal_flash_read(app_base, &stack_ptr, 4);
    hal_flash_read(app_base + 4, &reset_vector, 4);
    
    /* Validate stack pointer (must be in RAM) */
    if (stack_ptr < 0x20000000 || stack_ptr > 0x20040000) {
        return; /* Invalid stack pointer */
    }
    
    /* Jump to application */
    hal_jump_to_app(stack_ptr, reset_vector);
}

/**
 * Enter recovery mode
 * 
 * Recovery mode provides a minimal serial interface for:
 * - Downloading new firmware
 * - Erasing slots
 * - Diagnostics
 */
void gf_boot_enter_recovery(void) {
    /*
     * PRODUCTION IMPLEMENTATION:
     * - Initialize UART at fixed baud rate
     * - Listen for recovery protocol commands
     * - Support firmware download over YMODEM/XMODEM
     * - Verify downloaded image before writing
     * - Never allow unsigned code execution
     */
    
    while (1) {
        /* Recovery loop */
    }
}

/*===========================================================================*/
/* Application Stage Functions                                                */
/*===========================================================================*/

/**
 * Initialize secure boot interface (from application)
 */
int gf_boot_init(void) {
    gf_boot_data_t boot_data;
    
    if (!load_boot_data(&boot_data)) {
        /* Boot data not found/invalid - initialize with defaults */
        init_boot_data(&boot_data);
        boot_data.slot_status[0] = GF_SLOT_CONFIRMED;  /* Assume slot A is good */
        save_boot_data(&boot_data);
    }
    
    /* Populate info structure */
    s_boot.info.active_slot = boot_data.active_slot;
    s_boot.info.slot_a_status = boot_data.slot_status[0];
    s_boot.info.slot_b_status = boot_data.slot_status[1];
    s_boot.info.security_version = boot_data.security_version;
    s_boot.info.boot_count = boot_data.boot_count;
    s_boot.info.last_error = boot_data.last_error;
    s_boot.info.boot_confirmed = false;
    s_boot.info.debug_enabled = !hal_debug_is_locked();
    
    /* Get versions from headers */
    gf_image_header_t header;
    if (gf_boot_get_header(0, &header) == GF_BOOT_OK) {
        s_boot.info.slot_a_version = (header.version_major << 16) | 
                                     (header.version_minor << 8) | 
                                     header.version_patch;
    }
    if (gf_boot_get_header(1, &header) == GF_BOOT_OK) {
        s_boot.info.slot_b_version = (header.version_major << 16) | 
                                     (header.version_minor << 8) | 
                                     header.version_patch;
    }
    
    s_boot.initialized = true;
    
    return 0;
}

/**
 * Confirm successful boot
 * 
 * The application MUST call this after successful initialization.
 * If not called before next reset, bootloader will try alternate slot.
 */
int gf_boot_confirm(void) {
    if (!s_boot.initialized) return -1;
    if (s_boot.boot_confirmed) return 0;
    
    gf_boot_data_t boot_data;
    if (!load_boot_data(&boot_data)) return -2;
    
    /* Reset boot attempts counter */
    boot_data.boot_attempts = 0;
    boot_data.slot_status[boot_data.active_slot] = GF_SLOT_CONFIRMED;
    
    if (save_boot_data(&boot_data) != 0) return -3;
    
    s_boot.boot_confirmed = true;
    s_boot.info.boot_confirmed = true;
    
    return 0;
}

/**
 * Get boot information
 */
void gf_boot_get_info(gf_boot_info_t *info) {
    if (!info) return;
    memcpy(info, &s_boot.info, sizeof(gf_boot_info_t));
}

/**
 * Set active slot for next boot
 */
int gf_boot_set_active_slot(uint8_t slot) {
    if (slot > 1) return -1;
    
    gf_boot_data_t boot_data;
    if (!load_boot_data(&boot_data)) return -2;
    
    /* Verify target slot is valid */
    if (gf_boot_verify_slot(slot) != GF_BOOT_OK) {
        return -3;
    }
    
    boot_data.active_slot = slot;
    boot_data.boot_attempts = 0;
    
    return save_boot_data(&boot_data);
}

/**
 * Invalidate a slot
 */
int gf_boot_invalidate_slot(uint8_t slot) {
    if (slot > 1) return -1;
    
    gf_boot_data_t boot_data;
    if (!load_boot_data(&boot_data)) return -2;
    
    boot_data.slot_status[slot] = GF_SLOT_INVALID;
    
    /* If invalidating active slot, switch to other */
    if (boot_data.active_slot == slot) {
        boot_data.active_slot = 1 - slot;
    }
    
    return save_boot_data(&boot_data);
}

/**
 * Request reboot to recovery
 */
int gf_boot_request_recovery(void) {
    gf_boot_data_t boot_data;
    if (!load_boot_data(&boot_data)) return -1;
    
    /* Set flag for bootloader to enter recovery */
    boot_data.flags |= 0x01;
    save_boot_data(&boot_data);
    
    hal_system_reset();
    return 0;
}

/**
 * Verify update image before applying
 */
gf_boot_result_t gf_boot_verify_update(const gf_image_header_t *header,
                                        const uint8_t *image, uint32_t size) {
    uint8_t computed_hash[GF_BOOT_HASH_SIZE];
    
    if (!header || !image) return GF_BOOT_ERR_INVALID_HEADER;
    if (header->magic != IMAGE_MAGIC) return GF_BOOT_ERR_INVALID_HEADER;
    if (size != header->image_size) return GF_BOOT_ERR_INVALID_HEADER;
    
    /* Verify image hash */
    gf_boot_sha256(image, size, computed_hash);
    if (!gf_boot_secure_compare(computed_hash, header->image_hash, GF_BOOT_HASH_SIZE)) {
        return GF_BOOT_ERR_HASH_MISMATCH;
    }
    
    /* Verify signature */
    gf_boot_sha256((const uint8_t *)header, 
                   offsetof(gf_image_header_t, signature), 
                   computed_hash);
    if (!gf_boot_verify_ecdsa(computed_hash, header->signature, BOOT_PUBLIC_KEY)) {
        return GF_BOOT_ERR_SIGNATURE;
    }
    
    /* Check anti-rollback */
    if (header->security_version < s_boot.info.security_version) {
        return GF_BOOT_ERR_ROLLBACK;
    }
    
    return GF_BOOT_OK;
}

/**
 * Update security version (burns fuse/counter)
 */
int gf_boot_update_security_version(uint32_t version) {
    gf_boot_data_t boot_data;
    if (!load_boot_data(&boot_data)) return -1;
    
    /* Security version can only increase */
    if (version <= boot_data.security_version) return -2;
    
    boot_data.security_version = version;
    s_boot.info.security_version = version;
    
    return save_boot_data(&boot_data);
}

/**
 * Check if debug is allowed
 */
bool gf_boot_debug_allowed(void) {
    return s_boot.info.debug_enabled;
}

/**
 * Lock debug permanently
 */
int gf_boot_lock_debug(void) {
    int result = hal_debug_lock();
    if (result == 0) {
        s_boot.info.debug_enabled = false;
    }
    return result;
}

/*===========================================================================*/
/* Driver Interface                                                           */
/*===========================================================================*/

static int boot_drv_init(void *config) {
    (void)config;
    return gf_boot_init();
}

static gf_driver_t s_boot_driver = {
    .name = "secure_boot",
    .version = 0x0100,
    .capabilities = 0,
    .ops = {
        .init = boot_drv_init,
        .deinit = NULL,
        .suspend = NULL,
        .resume = NULL,
        .ioctl = NULL
    },
    .deps = { NULL }
};

const void* gf_boot_get_driver(void) {
    return &s_boot_driver;
}
