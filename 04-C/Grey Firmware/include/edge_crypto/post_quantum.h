/**
 * @file post_quantum.h
 * @brief Post-Quantum Cryptography for Edge Devices
 * 
 * INDUSTRY RELEVANCE:
 * Quantum computers threaten current cryptography (RSA, ECC).
 * Edge devices need quantum-resistant algorithms for:
 * - Long-term data protection
 * - Secure key exchange
 * - Digital signatures
 * - Firmware authentication
 * 
 * Standards: NIST PQC (CRYSTALS-Kyber, CRYSTALS-Dilithium)
 * Frameworks: liboqs, PQClean
 */

#ifndef GF_POST_QUANTUM_H
#define GF_POST_QUANTUM_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_PQ_PUBLIC_KEY_SIZE       1568    /* Kyber-768 */
#define GF_PQ_SECRET_KEY_SIZE       3168    /* Kyber-768 */
#define GF_PQ_CIPHERTEXT_SIZE       1088    /* Kyber-768 */
#define GF_PQ_SHARED_SECRET_SIZE    32
#define GF_PQ_SIGNATURE_SIZE        3293    /* Dilithium-3 */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_PQ_OK = 0,
    GF_PQ_ERROR_NULL_PTR,
    GF_PQ_ERROR_NOT_INITIALIZED,
    GF_PQ_ERROR_KEYGEN_FAILED,
    GF_PQ_ERROR_ENCAP_FAILED,
    GF_PQ_ERROR_DECAP_FAILED,
    GF_PQ_ERROR_SIGN_FAILED,
    GF_PQ_ERROR_VERIFY_FAILED,
    GF_PQ_ERROR_INVALID_KEY,
    GF_PQ_ERROR_RNG_FAULT,
    GF_PQ_ERROR_BUFFER_TOO_SMALL,
    GF_PQ_WARN_KEY_EXPIRING
} gf_pq_status_t;

typedef enum {
    GF_PQ_ALG_KYBER_512,        /* NIST Level 1 */
    GF_PQ_ALG_KYBER_768,        /* NIST Level 3 (recommended) */
    GF_PQ_ALG_KYBER_1024,       /* NIST Level 5 */
    GF_PQ_ALG_DILITHIUM_2,      /* Signature Level 2 */
    GF_PQ_ALG_DILITHIUM_3,      /* Signature Level 3 */
    GF_PQ_ALG_DILITHIUM_5,      /* Signature Level 5 */
    GF_PQ_ALG_FALCON_512,       /* Compact signatures */
    GF_PQ_ALG_SPHINCS_SHA2      /* Stateless hash-based */
} gf_pq_algorithm_t;

typedef enum {
    GF_PQ_KEY_TYPE_KEM,         /* Key encapsulation */
    GF_PQ_KEY_TYPE_SIGNATURE    /* Digital signature */
} gf_pq_key_type_t;

/**
 * @brief Algorithm configuration
 */
typedef struct {
    gf_pq_algorithm_t algorithm;
    uint16_t security_level;    /* NIST level 1-5 */
    uint32_t public_key_size;
    uint32_t secret_key_size;
    uint32_t ciphertext_size;
    uint32_t signature_size;
} gf_pq_params_t;

/**
 * @brief Key pair
 */
typedef struct {
    gf_pq_algorithm_t algorithm;
    gf_pq_key_type_t type;
    uint8_t* public_key;
    uint32_t public_key_len;
    uint8_t* secret_key;
    uint32_t secret_key_len;
    uint64_t created_ms;
    uint64_t expires_ms;
    uint32_t key_id;
} gf_pq_keypair_t;

/**
 * @brief Encapsulated key
 */
typedef struct {
    uint8_t ciphertext[GF_PQ_CIPHERTEXT_SIZE];
    uint32_t ciphertext_len;
    uint32_t peer_key_id;
} gf_pq_encapsulation_t;

/**
 * @brief Signature
 */
typedef struct {
    uint8_t signature[GF_PQ_SIGNATURE_SIZE];
    uint32_t signature_len;
    gf_pq_algorithm_t algorithm;
    uint32_t signer_key_id;
} gf_pq_signature_t;

/**
 * @brief Hybrid mode (classical + post-quantum)
 */
typedef struct {
    bool enabled;
    uint8_t classical_algorithm;    /* e.g., X25519 */
    gf_pq_algorithm_t pq_algorithm;
    uint8_t combined_secret[64];    /* KDF of both secrets */
} gf_pq_hybrid_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_pq_status_t gf_pq_init(void);
void gf_pq_shutdown(void);

/* Algorithm parameters */
gf_pq_status_t gf_pq_get_params(gf_pq_algorithm_t alg, gf_pq_params_t* params);

/* Key generation */
gf_pq_status_t gf_pq_keygen_kem(gf_pq_algorithm_t alg, gf_pq_keypair_t* keypair);
gf_pq_status_t gf_pq_keygen_sign(gf_pq_algorithm_t alg, gf_pq_keypair_t* keypair);
gf_pq_status_t gf_pq_free_keypair(gf_pq_keypair_t* keypair);

/* Key Encapsulation Mechanism */
gf_pq_status_t gf_pq_encapsulate(const uint8_t* public_key,
                                  uint32_t public_key_len,
                                  gf_pq_algorithm_t alg,
                                  uint8_t* shared_secret,
                                  gf_pq_encapsulation_t* encap);
gf_pq_status_t gf_pq_decapsulate(const gf_pq_keypair_t* keypair,
                                  const gf_pq_encapsulation_t* encap,
                                  uint8_t* shared_secret);

/* Digital Signatures */
gf_pq_status_t gf_pq_sign(const gf_pq_keypair_t* keypair,
                           const uint8_t* message,
                           uint32_t message_len,
                           gf_pq_signature_t* sig);
gf_pq_status_t gf_pq_verify(const uint8_t* public_key,
                             uint32_t public_key_len,
                             const uint8_t* message,
                             uint32_t message_len,
                             const gf_pq_signature_t* sig);

/* Hybrid mode */
gf_pq_status_t gf_pq_hybrid_keygen(gf_pq_hybrid_t* hybrid, gf_pq_keypair_t* keypair);
gf_pq_status_t gf_pq_hybrid_exchange(gf_pq_hybrid_t* hybrid,
                                      const uint8_t* peer_classical_key,
                                      const uint8_t* peer_pq_key,
                                      uint8_t* shared_secret);

/* Key serialization */
gf_pq_status_t gf_pq_export_public_key(const gf_pq_keypair_t* keypair,
                                        uint8_t* buffer,
                                        uint32_t* buffer_len);
gf_pq_status_t gf_pq_import_public_key(gf_pq_algorithm_t alg,
                                        const uint8_t* buffer,
                                        uint32_t buffer_len,
                                        gf_pq_keypair_t* keypair);

/* RNG seeding */
gf_pq_status_t gf_pq_add_entropy(const uint8_t* entropy, uint32_t len);

#endif /* GF_POST_QUANTUM_H */
