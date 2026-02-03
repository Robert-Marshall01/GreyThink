/**
 * @file firmware_signing.h
 * @brief Firmware Signing Utility Stub
 * 
 * WHAT: Utilities for signing firmware images with ECDSA-P256 and
 *       preparing images for secure boot.
 * 
 * WHY: Secure firmware signing is critical for the chain of trust.
 *      The signing process must be done in a secure environment (HSM,
 *      secure build server) to protect the private key. Understanding
 *      the signing workflow is essential for production firmware roles.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Image header preparation
 *      - Hash computation for signing
 *      - Signature application
 *      - Image packaging for OTA
 * 
 * NOTE: Signing is typically done on a build server, not on-device.
 *       These utilities may run on the device for verification.
 */

#ifndef GF_FIRMWARE_SIGNING_H
#define GF_FIRMWARE_SIGNING_H

#include <stdint.h>
#include <stdbool.h>
#include "security/secure_boot.h"

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_SIGN_OK = 0,
    GF_SIGN_ERR_INVALID_INPUT,
    GF_SIGN_ERR_HASH_FAIL,
    GF_SIGN_ERR_SIGN_FAIL,
    GF_SIGN_ERR_KEY_INVALID
} gf_sign_result_t;

/* Signing context (for incremental operations) */
typedef struct {
    uint8_t     hash_state[128];    /* SHA-256 context */
    uint32_t    total_size;
    bool        initialized;
} gf_sign_ctx_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize signing context
 */
int gf_sign_init(gf_sign_ctx_t *ctx);

/**
 * @brief Add data to hash computation
 */
int gf_sign_update(gf_sign_ctx_t *ctx, const uint8_t *data, uint32_t len);

/**
 * @brief Finalize hash and get digest
 */
int gf_sign_finalize(gf_sign_ctx_t *ctx, uint8_t *hash);

/**
 * @brief Prepare image header (fill in computed fields)
 * @param header Header to prepare
 * @param image Application binary
 * @param image_size Size of application
 * @param version_major Major version
 * @param version_minor Minor version
 * @param version_patch Patch version
 * @param security_version Anti-rollback version
 */
int gf_sign_prepare_header(gf_image_header_t *header,
                            const uint8_t *image, uint32_t image_size,
                            uint16_t version_major, uint16_t version_minor,
                            uint16_t version_patch, uint32_t security_version);

/**
 * @brief Apply signature to header (requires private key - build server only)
 * @param header Header to sign
 * @param private_key ECDSA private key (32 bytes)
 */
int gf_sign_apply_signature(gf_image_header_t *header, const uint8_t *private_key);

/**
 * @brief Verify signature on header (uses embedded public key)
 */
bool gf_sign_verify(const gf_image_header_t *header);

/**
 * @brief Create complete signed image
 * @param output Output buffer (header + image)
 * @param output_size Output size
 * @param image Application binary
 * @param image_size Application size
 * @param private_key Signing key
 */
int gf_sign_create_image(uint8_t *output, uint32_t *output_size,
                          const uint8_t *image, uint32_t image_size,
                          const uint8_t *private_key);

/**
 * @brief Parse and validate signed image
 * @param image Signed image data
 * @param size Total size
 * @param header Output: parsed header
 * @param app_data Output: pointer to application data
 * @param app_size Output: application size
 */
int gf_sign_parse_image(const uint8_t *image, uint32_t size,
                         gf_image_header_t *header,
                         const uint8_t **app_data, uint32_t *app_size);

/**
 * @brief Get firmware signing driver descriptor
 */
const void* gf_sign_get_driver(void);

#endif /* GF_FIRMWARE_SIGNING_H */
