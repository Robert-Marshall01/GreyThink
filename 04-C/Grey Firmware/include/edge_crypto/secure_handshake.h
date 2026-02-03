/**
 * @file secure_handshake.h
 * @brief Secure Handshake Protocol for Edge Devices
 * 
 * INDUSTRY RELEVANCE:
 * Edge devices need lightweight, secure session establishment:
 * - TLS 1.3 for internet services
 * - DTLS for UDP protocols
 * - Noise Protocol for IoT
 * - Custom protocols for constrained devices
 * 
 * Considerations: Memory, CPU cycles, code size, latency
 * Companies: ARM (PSA), wolfSSL, Mbed TLS
 */

#ifndef GF_SECURE_HANDSHAKE_H
#define GF_SECURE_HANDSHAKE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_HS_MAX_SESSIONS          8
#define GF_HS_MAX_CERTS             4
#define GF_HS_SESSION_ID_SIZE       32
#define GF_HS_MASTER_SECRET_SIZE    48
#define GF_HS_NONCE_SIZE            12

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_HS_OK = 0,
    GF_HS_ERROR_NULL_PTR,
    GF_HS_ERROR_NOT_INITIALIZED,
    GF_HS_ERROR_HANDSHAKE_FAILED,
    GF_HS_ERROR_CERT_INVALID,
    GF_HS_ERROR_CERT_EXPIRED,
    GF_HS_ERROR_CERT_UNTRUSTED,
    GF_HS_ERROR_KEY_EXCHANGE_FAILED,
    GF_HS_ERROR_AUTH_FAILED,
    GF_HS_ERROR_DECRYPT_FAILED,
    GF_HS_ERROR_MAC_FAILED,
    GF_HS_ERROR_REPLAY_DETECTED,
    GF_HS_ERROR_SESSION_EXPIRED,
    GF_HS_ERROR_NO_RESOURCES,
    GF_HS_WARN_CERT_EXPIRING
} gf_hs_status_t;

typedef enum {
    GF_HS_STATE_IDLE,
    GF_HS_STATE_CLIENT_HELLO,
    GF_HS_STATE_SERVER_HELLO,
    GF_HS_STATE_CERT_EXCHANGE,
    GF_HS_STATE_KEY_EXCHANGE,
    GF_HS_STATE_FINISHED,
    GF_HS_STATE_ESTABLISHED,
    GF_HS_STATE_REKEYING,
    GF_HS_STATE_CLOSING,
    GF_HS_STATE_CLOSED,
    GF_HS_STATE_ERROR
} gf_hs_state_t;

typedef enum {
    GF_HS_PROTO_TLS_1_3,
    GF_HS_PROTO_DTLS_1_3,
    GF_HS_PROTO_NOISE_XX,       /* Noise framework */
    GF_HS_PROTO_NOISE_IK,
    GF_HS_PROTO_PSK,            /* Pre-shared key only */
    GF_HS_PROTO_CUSTOM
} gf_hs_protocol_t;

typedef enum {
    GF_HS_CIPHER_AES_128_GCM,
    GF_HS_CIPHER_AES_256_GCM,
    GF_HS_CIPHER_CHACHA20_POLY1305,
    GF_HS_CIPHER_AES_128_CCM,
    GF_HS_CIPHER_AES_128_CCM_8  /* Short tag for constrained */
} gf_hs_cipher_t;

typedef enum {
    GF_HS_KEX_X25519,
    GF_HS_KEX_P256,
    GF_HS_KEX_P384,
    GF_HS_KEX_KYBER_768,        /* Post-quantum */
    GF_HS_KEX_HYBRID_X25519_KYBER
} gf_hs_kex_t;

typedef enum {
    GF_HS_AUTH_CERT_ECDSA,
    GF_HS_AUTH_CERT_RSA,
    GF_HS_AUTH_PSK,
    GF_HS_AUTH_TOKEN,
    GF_HS_AUTH_MUTUAL           /* Client cert required */
} gf_hs_auth_t;

/**
 * @brief Handshake configuration
 */
typedef struct {
    gf_hs_protocol_t protocol;
    gf_hs_cipher_t cipher;
    gf_hs_kex_t key_exchange;
    gf_hs_auth_t auth_mode;
    bool is_server;
    bool session_resumption;
    bool early_data;
    uint32_t timeout_ms;
    uint32_t session_lifetime_sec;
} gf_hs_config_t;

/**
 * @brief Certificate
 */
typedef struct {
    uint8_t* cert_der;
    uint32_t cert_len;
    uint8_t* private_key;
    uint32_t private_key_len;
    uint64_t not_before_ms;
    uint64_t not_after_ms;
    char common_name[64];
    bool is_ca;
} gf_hs_certificate_t;

/**
 * @brief Pre-shared key
 */
typedef struct {
    uint8_t identity[32];
    uint8_t identity_len;
    uint8_t key[32];
    uint8_t key_len;
    uint64_t valid_from_ms;
    uint64_t valid_until_ms;
} gf_hs_psk_t;

/**
 * @brief Session handle
 */
typedef struct {
    uint8_t session_id[GF_HS_SESSION_ID_SIZE];
    gf_hs_state_t state;
    gf_hs_protocol_t protocol;
    gf_hs_cipher_t cipher;
    uint8_t master_secret[GF_HS_MASTER_SECRET_SIZE];
    uint64_t established_time_ms;
    uint64_t last_activity_ms;
    uint64_t sequence_number;
    char peer_identity[64];
    bool resumable;
} gf_hs_session_t;

/**
 * @brief Handshake message
 */
typedef struct {
    uint8_t* data;
    uint32_t data_len;
    bool is_encrypted;
} gf_hs_message_t;

/**
 * @brief Session metrics
 */
typedef struct {
    uint32_t handshakes_completed;
    uint32_t handshakes_failed;
    uint32_t sessions_resumed;
    uint32_t bytes_encrypted;
    uint32_t bytes_decrypted;
    uint32_t replay_attempts;
    uint16_t avg_handshake_ms;
} gf_hs_metrics_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef gf_hs_status_t (*gf_hs_verify_cb_t)(const gf_hs_certificate_t* cert,
                                             void* user_data);

typedef void (*gf_hs_established_cb_t)(const gf_hs_session_t* session,
                                        void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_hs_status_t gf_hs_init(const gf_hs_config_t* config);
void gf_hs_shutdown(void);

/* Certificate management */
gf_hs_status_t gf_hs_set_certificate(const gf_hs_certificate_t* cert);
gf_hs_status_t gf_hs_add_trusted_ca(const gf_hs_certificate_t* ca);
gf_hs_status_t gf_hs_set_psk(const gf_hs_psk_t* psk);

/* Handshake execution */
gf_hs_status_t gf_hs_start(gf_hs_session_t* session);
gf_hs_status_t gf_hs_process_message(gf_hs_session_t* session,
                                      const uint8_t* in_data,
                                      uint32_t in_len,
                                      uint8_t* out_data,
                                      uint32_t* out_len);
gf_hs_state_t gf_hs_get_state(const gf_hs_session_t* session);
bool gf_hs_is_established(const gf_hs_session_t* session);

/* Application data transfer */
gf_hs_status_t gf_hs_encrypt(gf_hs_session_t* session,
                              const uint8_t* plaintext,
                              uint32_t plaintext_len,
                              uint8_t* ciphertext,
                              uint32_t* ciphertext_len);
gf_hs_status_t gf_hs_decrypt(gf_hs_session_t* session,
                              const uint8_t* ciphertext,
                              uint32_t ciphertext_len,
                              uint8_t* plaintext,
                              uint32_t* plaintext_len);

/* Session management */
gf_hs_status_t gf_hs_rekey(gf_hs_session_t* session);
gf_hs_status_t gf_hs_close(gf_hs_session_t* session);
gf_hs_status_t gf_hs_resume(const uint8_t* session_id, gf_hs_session_t* session);

/* Callbacks */
gf_hs_status_t gf_hs_set_verify_callback(gf_hs_verify_cb_t cb, void* user_data);
gf_hs_status_t gf_hs_set_established_callback(gf_hs_established_cb_t cb, void* user_data);

/* Metrics */
gf_hs_status_t gf_hs_get_metrics(gf_hs_metrics_t* metrics);

#endif /* GF_SECURE_HANDSHAKE_H */
