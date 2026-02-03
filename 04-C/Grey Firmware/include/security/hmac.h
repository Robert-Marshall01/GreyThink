/**
 * @file hmac.h
 * @brief HMAC Authentication
 * 
 * WHAT: Hash-based Message Authentication Code for data integrity
 *       and authentication. Supports SHA-256 and SHA-384 variants.
 * 
 * WHY: HMAC is foundational for secure protocols (TLS, JWT, API auth).
 *      Understanding HMAC vs plain hashing demonstrates security awareness
 *      essential for connected devices.
 * 
 * INDUSTRY APPLICATIONS:
 *   - API authentication
 *   - Firmware signature verification
 *   - Secure boot chain
 *   - Protocol message authentication
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - HMAC construction
 *   - Constant-time comparison
 *   - Key derivation
 *   - Streaming vs one-shot API
 */

#ifndef GF_HMAC_H
#define GF_HMAC_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_HMAC_SHA256_LEN      32
#define GF_HMAC_SHA384_LEN      48
#define GF_HMAC_MAX_KEY_LEN     64

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_HMAC_SHA256 = 0,
    GF_HMAC_SHA384
} gf_hmac_alg_t;

/* Opaque context for streaming HMAC */
typedef struct {
    uint8_t _internal[256];     /* Implementation-specific */
} gf_hmac_ctx_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Compute HMAC in one shot
 * 
 * @param alg       Hash algorithm
 * @param key       HMAC key
 * @param key_len   Key length
 * @param data      Data to authenticate
 * @param data_len  Data length
 * @param mac       Output MAC buffer
 * @param mac_len   MAC buffer size (set to actual length on return)
 */
int gf_hmac(gf_hmac_alg_t alg,
            const uint8_t *key, size_t key_len,
            const uint8_t *data, size_t data_len,
            uint8_t *mac, size_t *mac_len);

/**
 * @brief Verify HMAC in constant time
 */
bool gf_hmac_verify(gf_hmac_alg_t alg,
                    const uint8_t *key, size_t key_len,
                    const uint8_t *data, size_t data_len,
                    const uint8_t *expected_mac, size_t mac_len);

/* Streaming API */
int gf_hmac_init(gf_hmac_ctx_t *ctx, gf_hmac_alg_t alg,
                 const uint8_t *key, size_t key_len);
int gf_hmac_update(gf_hmac_ctx_t *ctx, const uint8_t *data, size_t len);
int gf_hmac_final(gf_hmac_ctx_t *ctx, uint8_t *mac, size_t *mac_len);

/**
 * @brief Constant-time memory comparison
 */
bool gf_secure_compare(const void *a, const void *b, size_t len);

const void* gf_hmac_get_driver(void);

#endif /* GF_HMAC_H */
