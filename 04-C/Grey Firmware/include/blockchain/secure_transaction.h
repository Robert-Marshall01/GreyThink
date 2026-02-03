/**
 * @file secure_transaction.h
 * @brief Secure Transaction Module Interface
 * 
 * INDUSTRY RELEVANCE:
 * Secure transaction handling enables trusted IoT commerce:
 * - Machine-to-machine (M2M) micropayments
 * - EV charging station automated billing
 * - Smart meter usage-based payments
 * - Industrial equipment pay-per-use models
 * - Autonomous vehicle toll and parking payments
 * - Secure firmware licensing and activation
 * 
 * This module demonstrates expertise in:
 * - Hardware security module (HSM) integration
 * - Elliptic curve cryptography (secp256k1, Ed25519)
 * - Transaction signing and verification
 * - Replay attack prevention (nonces)
 * - Multi-signature authorization
 * - Secure key storage and derivation
 */

#ifndef GF_SECURE_TRANSACTION_H
#define GF_SECURE_TRANSACTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_STX_KEY_SIZE             32
#define GF_STX_SIG_SIZE             64
#define GF_STX_ADDR_SIZE            20
#define GF_STX_MAX_MULTISIG         5

typedef enum {
    GF_STX_OK = 0,
    GF_STX_ERROR_NOT_INITIALIZED,
    GF_STX_ERROR_KEY_NOT_FOUND,
    GF_STX_ERROR_SIGNATURE_FAILED,
    GF_STX_ERROR_VERIFICATION_FAILED,
    GF_STX_ERROR_INSUFFICIENT_SIGS,
    GF_STX_ERROR_NONCE_INVALID,
    GF_STX_ERROR_HSM_FAULT,
    GF_STX_ERROR_AMOUNT_INVALID
} gf_stx_status_t;

typedef enum {
    GF_STX_CURVE_SECP256K1,         /* Bitcoin/Ethereum compatible */
    GF_STX_CURVE_ED25519,           /* EdDSA */
    GF_STX_CURVE_P256               /* NIST P-256 */
} gf_stx_curve_t;

typedef enum {
    GF_STX_KEY_EPHEMERAL,           /* Generated for single use */
    GF_STX_KEY_PERSISTENT,          /* Stored in secure element */
    GF_STX_KEY_DERIVED              /* Derived from master key */
} gf_stx_key_type_t;

typedef struct {
    uint8_t public_key[GF_STX_KEY_SIZE];
    uint8_t address[GF_STX_ADDR_SIZE];
    gf_stx_key_type_t type;
    gf_stx_curve_t curve;
    uint32_t key_id;                /* For HSM reference */
    bool locked;                    /* Requires unlock before use */
} gf_stx_keypair_t;

typedef struct {
    uint8_t from[GF_STX_ADDR_SIZE];
    uint8_t to[GF_STX_ADDR_SIZE];
    uint64_t amount;
    uint8_t data[128];
    uint16_t data_length;
    uint32_t nonce;
    uint32_t timestamp;
    uint32_t expiry;                /* Transaction expiry time */
    uint8_t chain_id;               /* Network identifier */
} gf_stx_tx_t;

typedef struct {
    uint8_t signature[GF_STX_SIG_SIZE];
    uint8_t signer[GF_STX_ADDR_SIZE];
    uint8_t recovery_id;
} gf_stx_signature_t;

typedef struct {
    gf_stx_tx_t transaction;
    uint8_t num_signatures;
    gf_stx_signature_t signatures[GF_STX_MAX_MULTISIG];
    uint8_t hash[32];
    bool signed_;
    bool verified;
} gf_stx_signed_tx_t;

typedef struct {
    gf_stx_curve_t default_curve;
    bool use_hsm;
    uint32_t key_derivation_path[5];    /* BIP32-like path */
    uint8_t chain_id;
    bool require_multisig;
    uint8_t multisig_threshold;
} gf_stx_config_t;

gf_stx_status_t gf_stx_init(const gf_stx_config_t* config);
void gf_stx_shutdown(void);
gf_stx_status_t gf_stx_generate_keypair(gf_stx_keypair_t* keypair);
gf_stx_status_t gf_stx_derive_keypair(uint32_t index, gf_stx_keypair_t* keypair);
gf_stx_status_t gf_stx_import_key(const uint8_t* private_key, gf_stx_keypair_t* keypair);
gf_stx_status_t gf_stx_get_address(const gf_stx_keypair_t* keypair, uint8_t* address);
gf_stx_status_t gf_stx_create_tx(const gf_stx_tx_t* tx, gf_stx_signed_tx_t* signed_tx);
gf_stx_status_t gf_stx_sign_tx(gf_stx_signed_tx_t* tx, const gf_stx_keypair_t* keypair);
gf_stx_status_t gf_stx_add_signature(gf_stx_signed_tx_t* tx, const gf_stx_signature_t* sig);
gf_stx_status_t gf_stx_verify_tx(const gf_stx_signed_tx_t* tx);
gf_stx_status_t gf_stx_serialize_tx(const gf_stx_signed_tx_t* tx, uint8_t* buffer, uint16_t* length);
gf_stx_status_t gf_stx_deserialize_tx(const uint8_t* buffer, uint16_t length, gf_stx_signed_tx_t* tx);
gf_stx_status_t gf_stx_increment_nonce(void);
uint32_t gf_stx_get_nonce(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SECURE_TRANSACTION_H */
