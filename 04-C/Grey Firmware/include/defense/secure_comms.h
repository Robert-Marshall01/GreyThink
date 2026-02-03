/**
 * @file secure_comms.h
 * @brief Secure Communications (Encrypted Radio) Stub
 * 
 * INDUSTRY RELEVANCE:
 * Defense and high-security applications require encrypted communications
 * that meet standards like NSA Suite B, Type 1 encryption, or FIPS 140-3.
 * Firmware engineers work with AES-256, elliptic curve cryptography,
 * key management, and anti-jamming techniques. This sector represents
 * $15B+ in military communications and critical infrastructure.
 * 
 * Key challenges:
 * - Hardware security module (HSM) integration
 * - Zeroization of keys on tamper detection
 * - Low-latency encryption for real-time voice
 * - Frequency hopping and spread spectrum
 * - TEMPEST compliance (emission security)
 */

#ifndef GF_SECURE_COMMS_H
#define GF_SECURE_COMMS_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Secure communications status codes */
typedef enum {
    GF_SCOMM_OK = 0,
    GF_SCOMM_KEY_NOT_LOADED,        /* No encryption key available */
    GF_SCOMM_KEY_EXPIRED,           /* Key past validity period */
    GF_SCOMM_AUTH_FAILED,           /* Peer authentication failed */
    GF_SCOMM_DECRYPT_ERROR,         /* Decryption failure */
    GF_SCOMM_TAMPER_DETECTED,       /* Tamper event detected */
    GF_SCOMM_JAMMING_DETECTED,      /* RF jamming detected */
    GF_SCOMM_SYNC_LOST,             /* Crypto sync lost */
    GF_SCOMM_HSM_ERROR,             /* HSM hardware fault */
    GF_SCOMM_ZEROIZED               /* Keys have been zeroized */
} gf_scomm_status_t;

/* Encryption algorithms */
typedef enum {
    GF_CRYPTO_AES_128_GCM,          /* AES-128 Galois/Counter Mode */
    GF_CRYPTO_AES_256_GCM,          /* AES-256 GCM (Suite B) */
    GF_CRYPTO_CHACHA20_POLY1305,    /* ChaCha20-Poly1305 */
    GF_CRYPTO_TYPE1                 /* NSA Type 1 (classified) */
} gf_scomm_crypto_t;

/* Key exchange methods */
typedef enum {
    GF_KEX_ECDH_P256,               /* Elliptic Curve Diffie-Hellman P-256 */
    GF_KEX_ECDH_P384,               /* ECDH P-384 (Suite B) */
    GF_KEX_X25519,                  /* Curve25519 key exchange */
    GF_KEX_MANUAL                   /* Manual key fill */
} gf_scomm_kex_t;

/* Radio modes */
typedef enum {
    GF_RADIO_FIXED_FREQ,            /* Fixed frequency operation */
    GF_RADIO_FREQ_HOPPING,          /* Frequency hopping spread spectrum */
    GF_RADIO_DIRECT_SEQUENCE,       /* Direct sequence spread spectrum */
    GF_RADIO_HYBRID                 /* Combined FHSS/DSSS */
} gf_scomm_radio_mode_t;

/* Secure communications configuration */
typedef struct {
    gf_scomm_crypto_t cipher;       /* Encryption algorithm */
    gf_scomm_kex_t key_exchange;    /* Key exchange method */
    gf_scomm_radio_mode_t radio_mode; /* Radio operating mode */
    uint32_t frequency_hz;          /* Center frequency */
    uint16_t hop_rate_hz;           /* Hop rate (FHSS) */
    uint8_t tx_power_dbm;           /* Transmit power */
    bool enable_comsec;             /* Communications security */
    bool enable_transec;            /* Transmission security */
    uint32_t key_validity_sec;      /* Key validity period */
} gf_scomm_config_t;

/* Key material structure */
typedef struct {
    uint8_t key_id[16];             /* Key identifier */
    uint8_t key_data[32];           /* Encrypted key material */
    uint64_t valid_from;            /* Start of validity */
    uint64_t valid_until;           /* End of validity */
    uint8_t crypto_period;          /* Crypto period number */
    bool is_active;                 /* Currently active key */
} gf_scomm_key_t;

/* Link status information */
typedef struct {
    bool link_established;          /* Secure link active */
    int8_t rssi_dbm;                /* Received signal strength */
    uint8_t snr_db;                 /* Signal-to-noise ratio */
    uint32_t packets_sent;          /* Encrypted packets sent */
    uint32_t packets_received;      /* Packets received */
    uint32_t decrypt_errors;        /* Decryption failures */
    uint32_t sync_losses;           /* Sync loss count */
    gf_scomm_status_t status;       /* Current status */
} gf_scomm_link_status_t;

/**
 * @brief Initialize secure communications subsystem
 * @param config Communication configuration
 * @return Status code
 */
gf_scomm_status_t gf_scomm_init(const gf_scomm_config_t* config);

/**
 * @brief Load encryption key material
 * @param key Key structure with encrypted key data
 * @return Status code
 */
gf_scomm_status_t gf_scomm_load_key(const gf_scomm_key_t* key);

/**
 * @brief Establish secure link with peer
 * @param peer_id Peer device identifier
 * @return Status code
 */
gf_scomm_status_t gf_scomm_establish_link(const uint8_t* peer_id);

/**
 * @brief Transmit encrypted message
 * @param data Plaintext data to send
 * @param length Data length in bytes
 * @return Status code
 */
gf_scomm_status_t gf_scomm_transmit(const uint8_t* data, size_t length);

/**
 * @brief Receive and decrypt message
 * @param buffer Buffer for decrypted data
 * @param buffer_size Buffer size
 * @param received_length Actual bytes received
 * @return Status code
 */
gf_scomm_status_t gf_scomm_receive(uint8_t* buffer, size_t buffer_size, size_t* received_length);

/**
 * @brief Get link status
 * @param status Output for link status
 * @return Status code
 */
gf_scomm_status_t gf_scomm_get_status(gf_scomm_link_status_t* status);

/**
 * @brief Zeroize all key material (emergency)
 * @return Status code
 */
gf_scomm_status_t gf_scomm_zeroize(void);

/**
 * @brief Perform cryptographic self-test (FIPS)
 * @return Status code (OK if passed)
 */
gf_scomm_status_t gf_scomm_self_test(void);

/**
 * @brief Shutdown and secure cleanup
 */
void gf_scomm_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SECURE_COMMS_H */
