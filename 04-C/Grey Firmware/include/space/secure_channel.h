/**
 * @file secure_channel.h
 * @brief Secure Telemetry Channel - Edge Space Communications
 * 
 * @details Industry Relevance:
 * Space-to-ground communications require strong security against:
 * - Command spoofing (unauthorized spacecraft control)
 * - Telemetry interception (competitive/military intelligence)
 * - Replay attacks (re-transmitting old valid commands)
 * - Jamming detection and mitigation
 * 
 * CCSDS Space Data Link Security (SDLS) and Space Packet Protocol security
 * extensions define cryptographic mechanisms for satellite communications.
 * 
 * Algorithms: AES-GCM for authenticated encryption, HMAC-SHA256 for
 * authentication, X25519 for key exchange, post-quantum considerations.
 * 
 * Constraints: Limited uplink bandwidth, key management challenges,
 * radiation-induced bit flips affecting crypto operations.
 * 
 * Standards: CCSDS 355.0-B-1 (SDLS), NIST SP 800-175B
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_SPACE_SECURE_CHANNEL_H
#define GF_SPACE_SECURE_CHANNEL_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum security header size */
#define GF_SEC_HEADER_SIZE              16

/** Maximum security trailer size (auth tag) */
#define GF_SEC_TRAILER_SIZE             16

/** AES key size (256-bit) */
#define GF_SEC_KEY_SIZE                 32

/** IV/Nonce size */
#define GF_SEC_IV_SIZE                  12

/** Authentication tag size */
#define GF_SEC_TAG_SIZE                 16

/** Anti-replay window size */
#define GF_SEC_REPLAY_WINDOW            64

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Security modes
 */
typedef enum {
    GF_SEC_MODE_NONE,               /**< No security (testing) */
    GF_SEC_MODE_AUTH_ONLY,          /**< Authentication only */
    GF_SEC_MODE_ENCRYPT_ONLY,       /**< Encryption only (not recommended) */
    GF_SEC_MODE_ENCRYPT_AUTH,       /**< Authenticated encryption (recommended) */
    GF_SEC_MODE_POST_QUANTUM        /**< Post-quantum algorithms */
} gf_sec_mode_t;

/**
 * @brief Key types
 */
typedef enum {
    GF_SEC_KEY_MASTER,              /**< Master key (rarely used directly) */
    GF_SEC_KEY_SESSION,             /**< Session key */
    GF_SEC_KEY_COMMAND,             /**< Command authentication key */
    GF_SEC_KEY_TELEMETRY            /**< Telemetry key */
} gf_sec_key_type_t;

/**
 * @brief Security association (SA)
 */
typedef struct {
    uint16_t sa_id;                 /**< Security Association ID */
    gf_sec_mode_t mode;             /**< Security mode */
    uint8_t key[GF_SEC_KEY_SIZE];   /**< Session key */
    uint8_t iv[GF_SEC_IV_SIZE];     /**< Current IV */
    uint64_t sequence_num;          /**< Sequence number (anti-replay) */
    uint64_t replay_window[GF_SEC_REPLAY_WINDOW / 64];
    uint32_t expires_time;          /**< Key expiration time */
    bool authenticated;             /**< SA authenticated */
    bool active;                    /**< SA currently active */
} gf_sec_sa_t;

/**
 * @brief Secure frame header (prepended to data)
 */
typedef struct {
    uint16_t sa_id;                 /**< Security Association ID */
    uint16_t length;                /**< Secured data length */
    uint8_t iv[GF_SEC_IV_SIZE];     /**< Frame IV/nonce */
} gf_sec_header_t;

/**
 * @brief Handshake message for key exchange
 */
typedef struct {
    uint8_t message_type;           /**< Handshake type */
    uint16_t version;               /**< Protocol version */
    uint8_t public_key[32];         /**< X25519 public key */
    uint8_t random[32];             /**< Random nonce */
    uint8_t signature[64];          /**< Ed25519 signature */
} gf_sec_handshake_t;

/**
 * @brief Channel security status
 */
typedef struct {
    gf_sec_mode_t mode;             /**< Current security mode */
    uint16_t active_sa;             /**< Active SA ID */
    uint64_t frames_secured;        /**< Frames encrypted/authenticated */
    uint64_t frames_verified;       /**< Frames successfully verified */
    uint32_t auth_failures;         /**< Authentication failures */
    uint32_t replay_detections;     /**< Replay attacks blocked */
    uint32_t key_age_s;             /**< Current key age in seconds */
    bool handshake_complete;        /**< Key exchange complete */
    bool rekeying_needed;           /**< Key rotation recommended */
} gf_sec_status_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize secure channel subsystem
 * @return 0 on success, negative error code on failure
 */
int gf_sec_init(void);

/**
 * @brief Shutdown secure channel
 * @return 0 on success
 */
int gf_sec_shutdown(void);

/**
 * @brief Set security mode
 * @param mode Security mode to use
 * @return 0 on success
 */
int gf_sec_set_mode(gf_sec_mode_t mode);

/**
 * @brief Load pre-shared key
 * @param key_type Type of key
 * @param key Key material
 * @param key_len Key length
 * @return 0 on success
 */
int gf_sec_load_key(gf_sec_key_type_t key_type,
                    const uint8_t* key, uint16_t key_len);

/**
 * @brief Initiate key exchange handshake
 * @param handshake Output handshake message
 * @return 0 on success
 */
int gf_sec_initiate_handshake(gf_sec_handshake_t* handshake);

/**
 * @brief Process received handshake message
 * @param handshake Received handshake
 * @param response Output response (if any)
 * @return 0 on complete, 1 on pending, negative on error
 */
int gf_sec_process_handshake(const gf_sec_handshake_t* handshake,
                             gf_sec_handshake_t* response);

/**
 * @brief Secure outgoing frame (encrypt + authenticate)
 * @param plaintext Input data
 * @param plain_len Plaintext length
 * @param secured Output buffer
 * @param secured_len Output: secured frame length
 * @return 0 on success
 */
int gf_sec_protect(const uint8_t* plaintext, uint16_t plain_len,
                   uint8_t* secured, uint16_t* secured_len);

/**
 * @brief Verify and decrypt incoming frame
 * @param secured Input secured data
 * @param secured_len Secured length
 * @param plaintext Output buffer
 * @param plain_len Output: plaintext length
 * @return 0 on success, -1 on auth failure, -2 on replay
 */
int gf_sec_unprotect(const uint8_t* secured, uint16_t secured_len,
                     uint8_t* plaintext, uint16_t* plain_len);

/**
 * @brief Check if replay attack (without full processing)
 * @param sequence_num Received sequence number
 * @return true if replay detected
 */
bool gf_sec_is_replay(uint64_t sequence_num);

/**
 * @brief Get security channel status
 * @param status Output status structure
 * @return 0 on success
 */
int gf_sec_get_status(gf_sec_status_t* status);

/**
 * @brief Force key rotation
 * @return 0 on success
 */
int gf_sec_rotate_key(void);

/**
 * @brief Zeroize all key material (emergency)
 * @return 0 on success
 */
int gf_sec_zeroize(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SPACE_SECURE_CHANNEL_H */
