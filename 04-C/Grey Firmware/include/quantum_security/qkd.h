/**
 * @file qkd.h
 * @brief Quantum Key Distribution Interface
 * 
 * @details
 * This module provides interfaces for Quantum Key Distribution (QKD)
 * systems, enabling unconditionally secure key exchange based on
 * quantum mechanical principles.
 * 
 * INDUSTRY RELEVANCE:
 * - Financial sector secure communications
 * - Government/defense networks
 * - Critical infrastructure protection
 * - Healthcare data networks
 * - Future-proof cryptography
 * - Quantum-safe IoT networks
 * 
 * KEY PROTOCOLS:
 * - BB84 (Bennett-Brassard 1984)
 * - E91 (Ekert 1991 - entanglement)
 * - BBM92 (Bennett-Brassard-Mermin)
 * - COW (Coherent One-Way)
 * - DPS (Differential Phase Shift)
 * - MDI-QKD (Measurement Device Independent)
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_QKD_H
#define GF_QKD_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define GF_QKD_MAX_KEY_SIZE     512   /**< Maximum key size (bytes) */
#define GF_QKD_BLOCK_SIZE       256   /**< Key block size (bits) */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_QKD_OK = 0,
    GF_QKD_ERROR_NOT_INITIALIZED,
    GF_QKD_ERROR_NULL_PTR,
    GF_QKD_ERROR_NO_PEER,
    GF_QKD_ERROR_CHANNEL,
    GF_QKD_ERROR_EAVESDROPPER,    /**< Eve detected! */
    GF_QKD_ERROR_KEY_LOW,
    GF_QKD_ERROR_PROTOCOL,
    GF_QKD_ERROR_TIMEOUT,
    GF_QKD_WARN_QBER_HIGH,        /**< High quantum bit error rate */
    GF_QKD_WARN_KEY_RATE_LOW
} gf_qkd_status_t;

typedef enum {
    GF_QKD_PROTOCOL_BB84,         /**< Standard BB84 protocol */
    GF_QKD_PROTOCOL_BB84_DECOY,   /**< BB84 with decoy states */
    GF_QKD_PROTOCOL_E91,          /**< Entanglement-based E91 */
    GF_QKD_PROTOCOL_COW,          /**< Coherent One-Way */
    GF_QKD_PROTOCOL_DPS,          /**< Differential Phase Shift */
    GF_QKD_PROTOCOL_MDI           /**< Measurement Device Independent */
} gf_qkd_protocol_t;

typedef enum {
    GF_QKD_ROLE_ALICE,            /**< Key transmitter */
    GF_QKD_ROLE_BOB               /**< Key receiver */
} gf_qkd_role_t;

typedef enum {
    GF_QKD_STATE_IDLE,
    GF_QKD_STATE_CONNECTING,
    GF_QKD_STATE_KEY_EXCHANGE,
    GF_QKD_STATE_SIFTING,
    GF_QKD_STATE_ERROR_CORRECTION,
    GF_QKD_STATE_PRIVACY_AMP,
    GF_QKD_STATE_KEY_READY,
    GF_QKD_STATE_ERROR
} gf_qkd_state_t;

typedef struct {
    gf_qkd_protocol_t protocol;   /**< QKD protocol */
    gf_qkd_role_t role;           /**< Alice or Bob */
    uint32_t key_rate_target_bps; /**< Target key rate (bits/sec) */
    float max_qber_pct;           /**< Max acceptable QBER (%) */
    uint32_t timeout_ms;          /**< Protocol timeout */
    bool continuous_keying;       /**< Continuous key generation */
} gf_qkd_config_t;

typedef struct {
    uint8_t key[GF_QKD_MAX_KEY_SIZE];  /**< Key material */
    uint16_t key_bits;                  /**< Key length in bits */
    uint32_t key_id;                    /**< Key identifier */
    uint64_t timestamp_ms;              /**< Generation timestamp */
    float qber_pct;                     /**< Quantum bit error rate */
    float security_parameter;           /**< Security parameter (epsilon) */
} gf_qkd_key_t;

typedef struct {
    gf_qkd_state_t state;         /**< Current state */
    float qber_pct;               /**< Current QBER */
    float key_rate_bps;           /**< Current key rate */
    uint32_t keys_generated;      /**< Total keys generated */
    uint32_t keys_available;      /**< Keys in buffer */
    uint64_t bits_exchanged;      /**< Total qubits exchanged */
    float link_efficiency;        /**< Channel efficiency */
} gf_qkd_status_report_t;

typedef void (*gf_qkd_key_cb_t)(const gf_qkd_key_t* key, void* user_data);
typedef void (*gf_qkd_eve_cb_t)(float qber_pct, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_qkd_status_t gf_qkd_init(const gf_qkd_config_t* config);
void gf_qkd_shutdown(void);
gf_qkd_status_t gf_qkd_connect(const char* peer_address);
gf_qkd_status_t gf_qkd_disconnect(void);
gf_qkd_status_t gf_qkd_start_keying(void);
gf_qkd_status_t gf_qkd_stop_keying(void);
gf_qkd_status_t gf_qkd_get_key(gf_qkd_key_t* key);
gf_qkd_status_t gf_qkd_get_key_by_id(uint32_t key_id, gf_qkd_key_t* key);
gf_qkd_status_t gf_qkd_consume_key(uint32_t key_id);
gf_qkd_status_t gf_qkd_get_status(gf_qkd_status_report_t* status);
gf_qkd_status_t gf_qkd_register_key_callback(gf_qkd_key_cb_t callback, void* user_data);
gf_qkd_status_t gf_qkd_register_eve_callback(gf_qkd_eve_cb_t callback, void* user_data);
gf_qkd_status_t gf_qkd_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_QKD_H */
