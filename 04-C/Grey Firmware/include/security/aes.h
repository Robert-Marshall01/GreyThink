/**
 * @file aes.h
 * @brief AES Encryption/Decryption
 * 
 * WHAT: AES-128/192/256 symmetric encryption with multiple modes
 *       (ECB, CBC, CTR, GCM). Supports hardware acceleration
 *       where available.
 * 
 * WHY: AES is the standard symmetric cipher. Understanding modes,
 *      IV requirements, and authenticated encryption (GCM) is
 *      essential for secure IoT devices.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Data-at-rest encryption
 *   - Communication encryption
 *   - Firmware protection
 *   - Credential storage
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - AES modes and their properties
 *   - IV/nonce requirements
 *   - AEAD (Authenticated Encryption)
 *   - Hardware acceleration abstraction
 */

#ifndef GF_AES_H
#define GF_AES_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_AES_BLOCK_SIZE       16
#define GF_AES_128_KEY_SIZE     16
#define GF_AES_192_KEY_SIZE     24
#define GF_AES_256_KEY_SIZE     32
#define GF_AES_GCM_TAG_SIZE     16
#define GF_AES_GCM_NONCE_SIZE   12

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_AES_128 = 0,
    GF_AES_192,
    GF_AES_256
} gf_aes_keysize_t;

typedef enum {
    GF_AES_ECB = 0,     /* Electronic Codebook (avoid for multi-block) */
    GF_AES_CBC,         /* Cipher Block Chaining */
    GF_AES_CTR,         /* Counter mode */
    GF_AES_GCM          /* Galois/Counter Mode (authenticated) */
} gf_aes_mode_t;

typedef enum {
    GF_AES_OK = 0,
    GF_AES_ERR_KEY,
    GF_AES_ERR_IV,
    GF_AES_ERR_SIZE,
    GF_AES_ERR_TAG,     /* GCM authentication failed */
    GF_AES_ERR_PARAM
} gf_aes_error_t;

/* Context for CBC/CTR streaming */
typedef struct {
    uint8_t _internal[288];     /* Implementation-specific */
} gf_aes_ctx_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize AES context
 */
gf_aes_error_t gf_aes_init(gf_aes_ctx_t *ctx,
                            gf_aes_mode_t mode,
                            const uint8_t *key, gf_aes_keysize_t keysize,
                            const uint8_t *iv);  /* NULL for ECB */

/**
 * @brief Encrypt data
 * 
 * For CBC: input_len must be multiple of 16
 * For CTR/GCM: any length
 */
gf_aes_error_t gf_aes_encrypt(gf_aes_ctx_t *ctx,
                               const uint8_t *input, size_t input_len,
                               uint8_t *output);

/**
 * @brief Decrypt data
 */
gf_aes_error_t gf_aes_decrypt(gf_aes_ctx_t *ctx,
                               const uint8_t *input, size_t input_len,
                               uint8_t *output);

/**
 * @brief AES-GCM authenticated encryption (one-shot)
 * 
 * @param key       Encryption key
 * @param keysize   Key size enum
 * @param nonce     12-byte nonce (must never reuse with same key)
 * @param aad       Additional authenticated data (may be NULL)
 * @param aad_len   AAD length
 * @param input     Plaintext
 * @param input_len Plaintext length
 * @param output    Ciphertext (same length as plaintext)
 * @param tag       16-byte authentication tag output
 */
gf_aes_error_t gf_aes_gcm_encrypt(const uint8_t *key, gf_aes_keysize_t keysize,
                                   const uint8_t nonce[12],
                                   const uint8_t *aad, size_t aad_len,
                                   const uint8_t *input, size_t input_len,
                                   uint8_t *output,
                                   uint8_t tag[16]);

/**
 * @brief AES-GCM authenticated decryption (one-shot)
 * 
 * Returns GF_AES_ERR_TAG if authentication fails.
 */
gf_aes_error_t gf_aes_gcm_decrypt(const uint8_t *key, gf_aes_keysize_t keysize,
                                   const uint8_t nonce[12],
                                   const uint8_t *aad, size_t aad_len,
                                   const uint8_t *input, size_t input_len,
                                   const uint8_t tag[16],
                                   uint8_t *output);

/**
 * @brief Check if hardware acceleration is available
 */
bool gf_aes_has_hw_accel(void);

const void* gf_aes_get_driver(void);

#endif /* GF_AES_H */
