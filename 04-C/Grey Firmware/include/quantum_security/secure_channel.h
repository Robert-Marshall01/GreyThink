/**
 * @file secure_channel.h
 * @brief Quantum-Safe Secure Channel Abstraction
 * 
 * @details
 * This module provides a unified secure channel abstraction that can use
 * either traditional cryptography, post-quantum algorithms, or QKD-derived
 * keys for establishing encrypted communications.
 * 
 * INDUSTRY RELEVANCE:
 * - Quantum-resistant VPN/tunnels
 * - Secure SCADA communications
 * - Financial transaction security
 * - Government classified networks
 * - Long-term data protection
 * - Hybrid classical/quantum security
 * 
 * KEY FEATURES:
 * - Hybrid key exchange (classical + PQC)
 * - QKD key integration
 * - Forward secrecy support
 * - Session key rotation
 * - Automatic algorithm selection
 * - Crypto agility for algorithm migration
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_SECURE_CHANNEL_H
#define GF_SECURE_CHANNEL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define GF_SC_MAX_KEY_SIZE      256   /**< Maximum key size (bytes) */
#define GF_SC_MAX_MESSAGE_SIZE  4096  /**< Maximum message size */
#define GF_SC_NONCE_SIZE        12    /**< Nonce size for AEAD */
#define GF_SC_TAG_SIZE          16    /**< Authentication tag size */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_SC_OK = 0,
    GF_SC_ERROR_NOT_INITIALIZED,
    GF_SC_ERROR_NULL_PTR,
    GF_SC_ERROR_HANDSHAKE,
    GF_SC_ERROR_AUTH_FAILED,
    GF_SC_ERROR_DECRYPT,
    GF_SC_ERROR_KEY_EXPIRED,
    GF_SC_ERROR_NO_KEY,
    GF_SC_ERROR_BUFFER_SIZE,
    GF_SC_WARN_KEY_ROTATION
} gf_sc_status_t;

typedef enum {
    GF_SC_KEX_CLASSIC,            /**< Traditional ECDH/DH */
    GF_SC_KEX_PQC,                /**< Post-quantum only */
    GF_SC_KEX_HYBRID,             /**< Classic + PQC hybrid */
    GF_SC_KEX_QKD                 /**< QKD-derived keys */
} gf_sc_kex_mode_t;

typedef enum {
    GF_SC_CIPHER_AES256_GCM,      /**< AES-256-GCM AEAD */
    GF_SC_CIPHER_CHACHA20_POLY,   /**< ChaCha20-Poly1305 */
    GF_SC_CIPHER_AES256_CCM       /**< AES-256-CCM (constrained) */
} gf_sc_cipher_t;

typedef enum {
    GF_SC_PQC_KYBER512,           /**< CRYSTALS-Kyber-512 */
    GF_SC_PQC_KYBER768,           /**< CRYSTALS-Kyber-768 */
    GF_SC_PQC_KYBER1024,          /**< CRYSTALS-Kyber-1024 */
    GF_SC_PQC_SABER,              /**< SABER */
    GF_SC_PQC_NTRU                /**< NTRU */
} gf_sc_pqc_algo_t;

typedef enum {
    GF_SC_STATE_IDLE,
    GF_SC_STATE_HANDSHAKING,
    GF_SC_STATE_ESTABLISHED,
    GF_SC_STATE_REKEYING,
    GF_SC_STATE_CLOSED
} gf_sc_state_t;

typedef struct {
    gf_sc_kex_mode_t kex_mode;    /**< Key exchange mode */
    gf_sc_cipher_t cipher;        /**< Symmetric cipher */
    gf_sc_pqc_algo_t pqc_algo;    /**< PQC algorithm (if applicable) */
    uint32_t rekey_interval_sec;  /**< Rekey interval */
    uint32_t rekey_bytes;         /**< Rekey after N bytes */
    bool mutual_auth;             /**< Require mutual auth */
    bool forward_secrecy;         /**< Enable forward secrecy */
} gf_sc_config_t;

typedef struct {
    uint32_t channel_id;          /**< Channel identifier */
    gf_sc_state_t state;          /**< Channel state */
    uint64_t bytes_encrypted;     /**< Bytes encrypted */
    uint64_t bytes_decrypted;     /**< Bytes decrypted */
    uint32_t messages_sent;       /**< Messages sent */
    uint32_t messages_received;   /**< Messages received */
    uint32_t rekeys_performed;    /**< Number of rekeys */
    uint64_t key_established_ms;  /**< Key establishment time */
    uint64_t key_expires_ms;      /**< Key expiration time */
} gf_sc_channel_info_t;

typedef struct {
    uint32_t channel_id;          /**< Channel ID for reference */
    uint8_t ciphertext[GF_SC_MAX_MESSAGE_SIZE + GF_SC_TAG_SIZE];
    uint16_t ciphertext_len;      /**< Ciphertext length */
    uint8_t nonce[GF_SC_NONCE_SIZE];  /**< Message nonce */
    uint64_t sequence_num;        /**< Sequence number */
} gf_sc_message_t;

typedef void (*gf_sc_message_cb_t)(uint32_t channel_id, const uint8_t* plaintext,
                                    uint16_t length, void* user_data);
typedef void (*gf_sc_event_cb_t)(uint32_t channel_id, gf_sc_state_t new_state,
                                  void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_sc_status_t gf_sc_init(void);
void gf_sc_shutdown(void);
gf_sc_status_t gf_sc_create_channel(const gf_sc_config_t* config, uint32_t* channel_id);
gf_sc_status_t gf_sc_destroy_channel(uint32_t channel_id);
gf_sc_status_t gf_sc_set_qkd_key(uint32_t channel_id, const uint8_t* key,
                                  uint16_t key_len, uint32_t key_id);
gf_sc_status_t gf_sc_handshake_initiate(uint32_t channel_id);
gf_sc_status_t gf_sc_handshake_respond(uint32_t channel_id, const uint8_t* init_msg,
                                        uint16_t msg_len);
gf_sc_status_t gf_sc_encrypt(uint32_t channel_id, const uint8_t* plaintext,
                              uint16_t length, gf_sc_message_t* message);
gf_sc_status_t gf_sc_decrypt(uint32_t channel_id, const gf_sc_message_t* message,
                              uint8_t* plaintext, uint16_t* length);
gf_sc_status_t gf_sc_rekey(uint32_t channel_id);
gf_sc_status_t gf_sc_get_channel_info(uint32_t channel_id, gf_sc_channel_info_t* info);
gf_sc_status_t gf_sc_register_message_callback(gf_sc_message_cb_t callback, void* user_data);
gf_sc_status_t gf_sc_register_event_callback(gf_sc_event_cb_t callback, void* user_data);
gf_sc_status_t gf_sc_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SECURE_CHANNEL_H */
