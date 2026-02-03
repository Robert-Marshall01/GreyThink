/**
 * @file secure_transaction.h
 * @brief Secure Transaction Processing for Edge Financial Systems
 * 
 * @details
 * This module provides secure transaction processing for embedded
 * financial systems, including payment terminals, ATMs, and
 * cryptocurrency hardware wallets.
 * 
 * INDUSTRY RELEVANCE:
 * - Payment Terminals: Verifone, Ingenico POS systems
 * - ATM Manufacturing: NCR, Diebold Nixdorf
 * - Cryptocurrency: Ledger, Trezor hardware wallets
 * - Banking Infrastructure: HSMs, PIN pads
 * - IoT Payments: Vending machines, parking meters
 * 
 * COMPLIANCE:
 * - PCI DSS: Payment Card Industry Data Security Standard
 * - PCI PTS: PIN Transaction Security
 * - EMV: Chip card standards (Europay, Mastercard, Visa)
 * - ISO 8583: Financial transaction messaging
 * - FIPS 140-2/3: Cryptographic module validation
 * 
 * SECURITY FEATURES:
 * - Hardware Security Module (HSM) integration
 * - Secure key storage and management
 * - Transaction encryption (TDES, AES)
 * - Tamper detection and response
 * - Secure boot chain validation
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_SECURE_TRANSACTION_H
#define GF_SECURE_TRANSACTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum transaction data size */
#define GF_TXN_MAX_DATA_SIZE            4096

/** PAN maximum length */
#define GF_TXN_PAN_MAX_LENGTH           19

/** Transaction ID length */
#define GF_TXN_ID_LENGTH                32

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Transaction type
 */
typedef enum {
    GF_TXN_PURCHASE,              /**< Purchase */
    GF_TXN_REFUND,                /**< Refund */
    GF_TXN_PREAUTH,               /**< Pre-authorization */
    GF_TXN_CAPTURE,               /**< Capture pre-auth */
    GF_TXN_VOID,                  /**< Void transaction */
    GF_TXN_BALANCE,               /**< Balance inquiry */
    GF_TXN_TRANSFER,              /**< Funds transfer */
    GF_TXN_WITHDRAWAL             /**< Cash withdrawal */
} gf_txn_type_t;

/**
 * @brief Transaction status
 */
typedef enum {
    GF_TXN_STATUS_PENDING,        /**< Pending processing */
    GF_TXN_STATUS_PROCESSING,     /**< In processing */
    GF_TXN_STATUS_APPROVED,       /**< Approved */
    GF_TXN_STATUS_DECLINED,       /**< Declined */
    GF_TXN_STATUS_ERROR,          /**< Error occurred */
    GF_TXN_STATUS_TIMEOUT,        /**< Timeout */
    GF_TXN_STATUS_CANCELLED       /**< Cancelled */
} gf_txn_status_t;

/**
 * @brief Card entry method
 */
typedef enum {
    GF_CARD_ENTRY_CHIP,           /**< EMV chip */
    GF_CARD_ENTRY_SWIPE,          /**< Magnetic stripe */
    GF_CARD_ENTRY_CONTACTLESS,    /**< NFC/RFID */
    GF_CARD_ENTRY_MANUAL,         /**< Manual entry */
    GF_CARD_ENTRY_QR              /**< QR code */
} gf_card_entry_t;

/**
 * @brief Transaction request
 */
typedef struct {
    gf_txn_type_t type;           /**< Transaction type */
    uint64_t amount_cents;        /**< Amount in cents */
    char currency[4];             /**< Currency code (USD) */
    gf_card_entry_t entry_method; /**< Card entry method */
    uint8_t encrypted_pan[24];    /**< Encrypted PAN */
    uint8_t encrypted_pin[16];    /**< Encrypted PIN block */
    char merchant_id[16];         /**< Merchant ID */
    char terminal_id[16];         /**< Terminal ID */
    uint32_t timestamp;           /**< Transaction timestamp */
} gf_txn_request_t;

/**
 * @brief Transaction response
 */
typedef struct {
    char txn_id[GF_TXN_ID_LENGTH]; /**< Transaction ID */
    gf_txn_status_t status;       /**< Status */
    char auth_code[8];            /**< Authorization code */
    char response_code[4];        /**< Response code */
    char response_msg[64];        /**< Response message */
    uint64_t balance_cents;       /**< Balance (if applicable) */
} gf_txn_response_t;

/**
 * @brief HSM key slot
 */
typedef struct {
    uint8_t slot_id;              /**< Key slot */
    char key_name[16];            /**< Key name */
    uint8_t key_type;             /**< Key type */
    bool loaded;                  /**< Key loaded */
    uint32_t usage_count;         /**< Usage counter */
} gf_key_slot_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize secure transaction system
 * @return 0 on success
 */
int gf_txn_init(void);

/**
 * @brief Shutdown transaction system
 */
void gf_txn_shutdown(void);

/**
 * @brief Process transaction
 * @param request Transaction request
 * @param response Output response
 * @return 0 on success
 */
int gf_txn_process(const gf_txn_request_t* request, 
                    gf_txn_response_t* response);

/**
 * @brief Encrypt data with HSM
 * @param plaintext Input data
 * @param length Data length
 * @param ciphertext Output encrypted data
 * @param key_slot Key slot to use
 * @return 0 on success
 */
int gf_txn_encrypt(const uint8_t* plaintext, uint32_t length,
                    uint8_t* ciphertext, uint8_t key_slot);

/**
 * @brief Generate PIN block
 * @param pin PIN digits
 * @param pan PAN for PIN block format
 * @param pin_block Output PIN block
 * @return 0 on success
 */
int gf_txn_generate_pin_block(const char* pin, const char* pan,
                               uint8_t* pin_block);

/**
 * @brief Verify tamper status
 * @return true if tamper detected
 */
bool gf_txn_check_tamper(void);

/**
 * @brief Zeroize keys (tamper response)
 */
void gf_txn_zeroize(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SECURE_TRANSACTION_H */
