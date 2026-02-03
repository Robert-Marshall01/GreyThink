/**
 * @file secure_enclave.h
 * @brief Secure Enclave and TEE Abstraction Interface
 *
 * INDUSTRY RELEVANCE:
 * Secure enclaves protect sensitive operations on edge devices through
 * hardware-isolated execution environments. This module demonstrates:
 * - ARM TrustZone integration abstraction
 * - Secure key storage and cryptographic operations
 * - Secure boot chain attestation
 * - FIDO2/WebAuthn security key support
 *
 * These skills apply to security-focused companies (Apple Secure Enclave,
 * Google Titan, Microsoft Pluton), payment systems (chip cards, POS terminals),
 * and IoT security requiring hardware root of trust.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires certified secure hardware.
 */

#ifndef GF_SECURE_ENCLAVE_H
#define GF_SECURE_ENCLAVE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_ENCLAVE_MAX_KEY_SIZE     64      /**< Maximum key size (bytes) */
#define GF_ENCLAVE_MAX_CERT_SIZE    2048    /**< Maximum certificate size */
#define GF_ENCLAVE_MAX_SEALED_SIZE  4096    /**< Maximum sealed data size */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Enclave type
 */
typedef enum {
    GF_ENCLAVE_ARM_TRUSTZONE,   /**< ARM TrustZone */
    GF_ENCLAVE_INTEL_SGX,       /**< Intel SGX */
    GF_ENCLAVE_AMD_SEV,         /**< AMD SEV */
    GF_ENCLAVE_TPM,             /**< Trusted Platform Module */
    GF_ENCLAVE_SOFT             /**< Software emulation (debug only) */
} gf_enclave_type_t;

/**
 * @brief Key algorithm
 */
typedef enum {
    GF_KEY_AES_128,             /**< AES-128 symmetric */
    GF_KEY_AES_256,             /**< AES-256 symmetric */
    GF_KEY_RSA_2048,            /**< RSA-2048 */
    GF_KEY_RSA_4096,            /**< RSA-4096 */
    GF_KEY_ECC_P256,            /**< ECDSA P-256 */
    GF_KEY_ECC_P384,            /**< ECDSA P-384 */
    GF_KEY_ED25519              /**< Ed25519 */
} gf_key_algorithm_t;

/**
 * @brief Key usage flags
 */
typedef enum {
    GF_KEY_USAGE_SIGN       = 0x01,     /**< Digital signature */
    GF_KEY_USAGE_VERIFY     = 0x02,     /**< Signature verification */
    GF_KEY_USAGE_ENCRYPT    = 0x04,     /**< Encryption */
    GF_KEY_USAGE_DECRYPT    = 0x08,     /**< Decryption */
    GF_KEY_USAGE_DERIVE     = 0x10,     /**< Key derivation */
    GF_KEY_USAGE_WRAP       = 0x20      /**< Key wrapping */
} gf_key_usage_t;

/**
 * @brief Key handle (opaque reference to key in enclave)
 */
typedef uint32_t gf_key_handle_t;

/**
 * @brief Key attributes
 */
typedef struct {
    gf_key_algorithm_t algorithm;       /**< Key algorithm */
    uint16_t key_size_bits;             /**< Key size in bits */
    uint8_t usage_flags;                /**< Allowed usage (bitmask) */
    bool exportable;                    /**< Can be exported */
    bool persistent;                    /**< Survive reboot */
    char label[32];                     /**< Key label/identifier */
} gf_key_attributes_t;

/**
 * @brief Attestation data
 */
typedef struct {
    uint8_t device_id[32];              /**< Unique device ID */
    uint8_t public_key[64];             /**< Attestation public key */
    uint8_t certificate[GF_ENCLAVE_MAX_CERT_SIZE]; /**< Certificate chain */
    uint16_t cert_len;                  /**< Certificate length */
    uint32_t boot_count;                /**< Boot counter */
    uint8_t pcr_values[8][32];          /**< Platform Configuration Registers */
    uint32_t security_version;          /**< Security version number */
    uint64_t timestamp;                 /**< Attestation timestamp */
} gf_attestation_t;

/**
 * @brief Sealed data (encrypted by enclave)
 */
typedef struct {
    uint8_t data[GF_ENCLAVE_MAX_SEALED_SIZE]; /**< Encrypted data */
    uint16_t data_len;                  /**< Data length */
    uint8_t nonce[12];                  /**< Encryption nonce */
    uint8_t tag[16];                    /**< Authentication tag */
    uint32_t seal_version;              /**< Seal version */
} gf_sealed_data_t;

/**
 * @brief Enclave status
 */
typedef struct {
    gf_enclave_type_t type;             /**< Enclave type */
    bool initialized;                   /**< Enclave ready */
    bool locked;                        /**< Enclave locked */
    uint32_t stored_keys;               /**< Number of stored keys */
    uint32_t available_storage;         /**< Available key storage */
    uint32_t crypto_ops;                /**< Crypto operations performed */
    uint32_t attestations;              /**< Attestations generated */
    bool anti_replay_active;            /**< Anti-replay protection */
} gf_enclave_status_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize secure enclave
 * @param type Enclave type
 * @return 0 on success, negative error code on failure
 */
int gf_enclave_init(gf_enclave_type_t type);

/**
 * @brief Shutdown secure enclave
 */
void gf_enclave_deinit(void);

/**
 * @brief Generate key pair in enclave
 * @param attributes Key attributes
 * @param[out] handle Output key handle
 * @return 0 on success
 */
int gf_enclave_generate_key(const gf_key_attributes_t* attributes, gf_key_handle_t* handle);

/**
 * @brief Import key into enclave
 * @param attributes Key attributes
 * @param key_data Raw key data
 * @param key_len Key length
 * @param[out] handle Output key handle
 * @return 0 on success
 */
int gf_enclave_import_key(const gf_key_attributes_t* attributes,
                           const uint8_t* key_data,
                           uint16_t key_len,
                           gf_key_handle_t* handle);

/**
 * @brief Delete key from enclave
 * @param handle Key handle
 * @return 0 on success
 */
int gf_enclave_delete_key(gf_key_handle_t handle);

/**
 * @brief Export public key (for asymmetric keys)
 * @param handle Key handle
 * @param[out] public_key Public key buffer
 * @param[out] public_key_len Public key length
 * @return 0 on success
 */
int gf_enclave_export_public(gf_key_handle_t handle,
                              uint8_t* public_key,
                              uint16_t* public_key_len);

/**
 * @brief Sign data with enclave key
 * @param handle Key handle
 * @param data Data to sign
 * @param data_len Data length
 * @param[out] signature Signature output
 * @param[out] sig_len Signature length
 * @return 0 on success
 */
int gf_enclave_sign(gf_key_handle_t handle,
                     const uint8_t* data,
                     uint16_t data_len,
                     uint8_t* signature,
                     uint16_t* sig_len);

/**
 * @brief Verify signature with enclave key
 * @param handle Key handle (or use external public key)
 * @param data Original data
 * @param data_len Data length
 * @param signature Signature to verify
 * @param sig_len Signature length
 * @return 0 if valid, -1 if invalid
 */
int gf_enclave_verify(gf_key_handle_t handle,
                       const uint8_t* data,
                       uint16_t data_len,
                       const uint8_t* signature,
                       uint16_t sig_len);

/**
 * @brief Encrypt data with enclave key
 * @param handle Key handle
 * @param plaintext Input data
 * @param plaintext_len Input length
 * @param[out] ciphertext Output buffer
 * @param[out] ciphertext_len Output length
 * @return 0 on success
 */
int gf_enclave_encrypt(gf_key_handle_t handle,
                        const uint8_t* plaintext,
                        uint16_t plaintext_len,
                        uint8_t* ciphertext,
                        uint16_t* ciphertext_len);

/**
 * @brief Decrypt data with enclave key
 * @param handle Key handle
 * @param ciphertext Encrypted data
 * @param ciphertext_len Encrypted length
 * @param[out] plaintext Output buffer
 * @param[out] plaintext_len Output length
 * @return 0 on success
 */
int gf_enclave_decrypt(gf_key_handle_t handle,
                        const uint8_t* ciphertext,
                        uint16_t ciphertext_len,
                        uint8_t* plaintext,
                        uint16_t* plaintext_len);

/**
 * @brief Seal data to enclave
 * @param data Data to seal
 * @param data_len Data length
 * @param[out] sealed Sealed data output
 * @return 0 on success
 */
int gf_enclave_seal(const uint8_t* data, uint16_t data_len, gf_sealed_data_t* sealed);

/**
 * @brief Unseal data from enclave
 * @param sealed Sealed data
 * @param[out] data Unsealed data output
 * @param[out] data_len Output length
 * @return 0 on success
 */
int gf_enclave_unseal(const gf_sealed_data_t* sealed, uint8_t* data, uint16_t* data_len);

/**
 * @brief Generate attestation report
 * @param challenge Challenge nonce
 * @param challenge_len Challenge length
 * @param[out] attestation Attestation output
 * @return 0 on success
 */
int gf_enclave_attest(const uint8_t* challenge,
                       uint16_t challenge_len,
                       gf_attestation_t* attestation);

/**
 * @brief Get enclave status
 * @param[out] status Status output
 */
void gf_enclave_get_status(gf_enclave_status_t* status);

/**
 * @brief Generate random bytes from enclave RNG
 * @param[out] buffer Output buffer
 * @param len Number of bytes
 * @return 0 on success
 */
int gf_enclave_random(uint8_t* buffer, uint16_t len);

/**
 * @brief Lock enclave (prevent further operations until unlock)
 * @return 0 on success
 */
int gf_enclave_lock(void);

/**
 * @brief Unlock enclave
 * @param pin PIN code
 * @param pin_len PIN length
 * @return 0 on success, -1 if wrong PIN
 */
int gf_enclave_unlock(const uint8_t* pin, uint8_t pin_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_SECURE_ENCLAVE_H */
