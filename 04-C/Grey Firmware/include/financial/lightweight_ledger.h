/**
 * @file lightweight_ledger.h
 * @brief Lightweight Ledger Module for Edge Financial Systems
 * 
 * @details
 * This module provides a compact ledger implementation for embedded
 * financial systems, enabling offline transaction logging, reconciliation,
 * and audit trail maintenance.
 * 
 * INDUSTRY RELEVANCE:
 * - Offline Payments: Transit cards, parking meters, vending machines
 * - Microfinance: Mobile banking in low-connectivity areas
 * - Cryptocurrency: Transaction history and UTXO tracking
 * - Gaming/Gambling: Slot machine accounting
 * - Retail: Store-and-forward transaction processing
 * 
 * COMPLIANCE:
 * - SOX (Sarbanes-Oxley): Audit trail requirements
 * - PCI DSS: Transaction logging requirements
 * - AML/KYC: Anti-money laundering record keeping
 * - GAAP: Generally Accepted Accounting Principles
 * 
 * FEATURES:
 * - Double-entry bookkeeping
 * - Cryptographic integrity (Merkle tree)
 * - Compact storage for embedded systems
 * - Batch reconciliation support
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_LIGHTWEIGHT_LEDGER_H
#define GF_LIGHTWEIGHT_LEDGER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum ledger entries */
#define GF_LEDGER_MAX_ENTRIES           10000

/** Account ID length */
#define GF_LEDGER_ACCOUNT_ID_LEN        16

/** Hash size (SHA-256) */
#define GF_LEDGER_HASH_SIZE             32

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Entry type
 */
typedef enum {
    GF_LEDGER_DEBIT,              /**< Debit entry */
    GF_LEDGER_CREDIT,             /**< Credit entry */
    GF_LEDGER_ADJUSTMENT,         /**< Adjustment */
    GF_LEDGER_REVERSAL            /**< Reversal */
} gf_ledger_entry_type_t;

/**
 * @brief Entry status
 */
typedef enum {
    GF_LEDGER_STATUS_PENDING,     /**< Pending */
    GF_LEDGER_STATUS_POSTED,      /**< Posted */
    GF_LEDGER_STATUS_RECONCILED,  /**< Reconciled */
    GF_LEDGER_STATUS_VOIDED       /**< Voided */
} gf_ledger_status_t;

/**
 * @brief Ledger entry
 */
typedef struct {
    uint32_t sequence;            /**< Sequence number */
    gf_ledger_entry_type_t type;  /**< Entry type */
    char account_id[GF_LEDGER_ACCOUNT_ID_LEN];
    int64_t amount_cents;         /**< Amount (signed) */
    char currency[4];             /**< Currency code */
    uint32_t timestamp;           /**< Entry timestamp */
    char reference[32];           /**< Reference/TXN ID */
    gf_ledger_status_t status;    /**< Entry status */
    uint8_t entry_hash[GF_LEDGER_HASH_SIZE]; /**< Entry hash */
    uint8_t prev_hash[GF_LEDGER_HASH_SIZE];  /**< Previous entry hash */
} gf_ledger_entry_t;

/**
 * @brief Account balance
 */
typedef struct {
    char account_id[GF_LEDGER_ACCOUNT_ID_LEN];
    int64_t balance_cents;        /**< Current balance */
    int64_t pending_cents;        /**< Pending amount */
    uint32_t last_activity;       /**< Last activity timestamp */
    uint32_t txn_count;           /**< Transaction count */
} gf_account_balance_t;

/**
 * @brief Ledger statistics
 */
typedef struct {
    uint32_t total_entries;       /**< Total entries */
    uint32_t pending_entries;     /**< Pending entries */
    int64_t total_debits;         /**< Total debits */
    int64_t total_credits;        /**< Total credits */
    uint8_t root_hash[GF_LEDGER_HASH_SIZE]; /**< Merkle root */
    bool integrity_valid;         /**< Integrity check passed */
} gf_ledger_stats_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize ledger
 * @return 0 on success
 */
int gf_ledger_init(void);

/**
 * @brief Shutdown ledger
 */
void gf_ledger_shutdown(void);

/**
 * @brief Add ledger entry
 * @param entry Entry to add
 * @return 0 on success
 */
int gf_ledger_add_entry(gf_ledger_entry_t* entry);

/**
 * @brief Get account balance
 * @param account_id Account identifier
 * @param balance Output balance
 * @return 0 on success
 */
int gf_ledger_get_balance(const char* account_id, 
                           gf_account_balance_t* balance);

/**
 * @brief Verify ledger integrity
 * @return true if valid
 */
bool gf_ledger_verify_integrity(void);

/**
 * @brief Get ledger statistics
 * @param stats Output statistics
 * @return 0 on success
 */
int gf_ledger_get_stats(gf_ledger_stats_t* stats);

/**
 * @brief Reconcile batch
 * @param start_seq Starting sequence
 * @param end_seq Ending sequence
 * @return 0 on success
 */
int gf_ledger_reconcile(uint32_t start_seq, uint32_t end_seq);

/**
 * @brief Export entries for sync
 * @param start_seq Starting sequence
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Bytes written
 */
int gf_ledger_export(uint32_t start_seq, uint8_t* buffer, 
                      uint32_t buffer_size);

#ifdef __cplusplus
}
#endif

#endif /* GF_LIGHTWEIGHT_LEDGER_H */
