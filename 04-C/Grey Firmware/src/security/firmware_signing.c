/**
 * @file firmware_signing.c
 * @brief Firmware Signing Utility Stub Implementation
 */

#include "security/firmware_signing.h"
#include "core/driver_registry.h"
#include <string.h>
#include <stddef.h>

int gf_sign_init(gf_sign_ctx_t *ctx) {
    if (!ctx) return -1;
    memset(ctx, 0, sizeof(gf_sign_ctx_t));
    ctx->initialized = true;
    return 0;
}

int gf_sign_update(gf_sign_ctx_t *ctx, const uint8_t *data, uint32_t len) {
    if (!ctx || !ctx->initialized) return -1;
    /* Would update SHA-256 context */
    (void)data;
    ctx->total_size += len;
    return 0;
}

int gf_sign_finalize(gf_sign_ctx_t *ctx, uint8_t *hash) {
    if (!ctx || !ctx->initialized || !hash) return -1;
    /* Would finalize SHA-256 and output hash */
    memset(hash, 0, GF_BOOT_HASH_SIZE);
    ctx->initialized = false;
    return 0;
}

int gf_sign_prepare_header(gf_image_header_t *header,
                            const uint8_t *image, uint32_t image_size,
                            uint16_t version_major, uint16_t version_minor,
                            uint16_t version_patch, uint32_t security_version) {
    if (!header || !image) return -1;
    
    memset(header, 0, sizeof(gf_image_header_t));
    
    /* Set magic and sizes */
    header->magic = 0x47464657;  /* "GFFW" */
    header->header_size = sizeof(gf_image_header_t);
    header->image_size = image_size;
    
    /* Set version info */
    header->version_major = version_major;
    header->version_minor = version_minor;
    header->version_patch = version_patch;
    header->security_version = security_version;
    
    /* Compute image hash */
    gf_boot_sha256(image, image_size, header->image_hash);
    
    /* Header hash is computed over header without signature */
    /* This would be done just before signing */
    
    return 0;
}

int gf_sign_apply_signature(gf_image_header_t *header, const uint8_t *private_key) {
    (void)header; (void)private_key;
    /*
     * PRODUCTION IMPLEMENTATION:
     * 1. Compute header hash
     * 2. Sign with ECDSA-P256
     * 3. Store signature in header
     * 
     * This is typically done on a secure build server with HSM.
     */
    return 0;
}

bool gf_sign_verify(const gf_image_header_t *header) {
    if (!header) return false;
    if (header->magic != 0x47464657) return false;
    
    /* Verify signature using secure boot verification */
    uint8_t header_hash[GF_BOOT_HASH_SIZE];
    gf_boot_sha256((const uint8_t *)header, 
                   offsetof(gf_image_header_t, signature),
                   header_hash);
    
    /* Use public key embedded in secure boot */
    extern bool gf_boot_verify_ecdsa(const uint8_t *hash, const uint8_t *sig,
                                      const uint8_t *pubkey);
    /* Would call actual verification */
    
    return true;
}

int gf_sign_create_image(uint8_t *output, uint32_t *output_size,
                          const uint8_t *image, uint32_t image_size,
                          const uint8_t *private_key) {
    if (!output || !output_size || !image) return -1;
    
    gf_image_header_t header;
    gf_sign_prepare_header(&header, image, image_size, 1, 0, 0, 1);
    gf_sign_apply_signature(&header, private_key);
    
    /* Copy header + image to output */
    memcpy(output, &header, sizeof(header));
    memcpy(output + sizeof(header), image, image_size);
    
    *output_size = sizeof(header) + image_size;
    
    return 0;
}

int gf_sign_parse_image(const uint8_t *image, uint32_t size,
                         gf_image_header_t *header,
                         const uint8_t **app_data, uint32_t *app_size) {
    if (!image || size < sizeof(gf_image_header_t)) return -1;
    
    memcpy(header, image, sizeof(gf_image_header_t));
    
    if (header->magic != 0x47464657) return -2;
    if (size < header->header_size + header->image_size) return -3;
    
    if (app_data) *app_data = image + header->header_size;
    if (app_size) *app_size = header->image_size;
    
    return 0;
}

static gf_driver_t s_sign_driver = {
    .name = "fw_sign", .version = 0x0100
};
const void* gf_sign_get_driver(void) { return &s_sign_driver; }
