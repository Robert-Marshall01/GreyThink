/**
 * @file finance_compliance.h
 * @brief Financial Compliance Logging Stub
 * 
 * Industry Relevance:
 * Financial services require extensive compliance logging for regulatory
 * requirements including SOX, PCI-DSS, MiFID II, and AML/KYC. This module:
 * - Provides immutable audit trails for transactions
 * - Logs access to sensitive financial data
 * - Generates regulatory reports (SAR, CTR)
 * - Supports real-time compliance monitoring
 * 
 * Standards: SOX, PCI-DSS, MiFID II, GDPR, AML/KYC
 * 
 * @author Grey Firmware Project
 */

#ifndef FINANCE_COMPLIANCE_H
#define FINANCE_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Compliance regulation */
typedef enum {
    FIN_COMPLIANCE_SOX,      /**< Sarbanes-Oxley Act */
    FIN_COMPLIANCE_PCI_DSS,  /**< Payment Card Industry */
    FIN_COMPLIANCE_MIFID,    /**< Markets in Financial Instruments */
    FIN_COMPLIANCE_AML,      /**< Anti-Money Laundering */
    FIN_COMPLIANCE_KYC       /**< Know Your Customer */
} fin_compliance_t;

/** Audit event type */
typedef enum {
    FIN_AUDIT_TX_CREATE,     /**< Transaction created */
    FIN_AUDIT_TX_APPROVE,    /**< Transaction approved */
    FIN_AUDIT_TX_EXECUTE,    /**< Transaction executed */
    FIN_AUDIT_DATA_ACCESS,   /**< Sensitive data accessed */
    FIN_AUDIT_CONFIG_CHANGE, /**< Configuration change */
    FIN_AUDIT_AUTH_ATTEMPT   /**< Authentication attempt */
} fin_audit_event_t;

/** Audit log entry */
typedef struct {
    uint64_t sequence;       /**< Sequence number (immutable) */
    uint32_t timestamp;      /**< Event timestamp */
    fin_audit_event_t event; /**< Event type */
    fin_compliance_t regulation; /**< Applicable regulation */
    uint32_t user_id;        /**< User/system identifier */
    uint8_t tx_hash[32];     /**< Related transaction hash */
    uint8_t prev_hash[32];   /**< Previous log entry hash */
    char details[128];       /**< Event details */
} fin_audit_entry_t;

/** Compliance status */
typedef struct {
    uint64_t total_entries;  /**< Total audit entries */
    uint32_t pending_reports;/**< Pending regulatory reports */
    bool chain_valid;        /**< Audit chain integrity */
    float compliance_score;  /**< Overall compliance (0-100) */
    uint32_t last_audit;     /**< Last external audit timestamp */
} fin_compliance_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize compliance logging
 * @return 0 on success, negative on error
 */
int finance_compliance_init(void);

/**
 * @brief Log audit event
 * @param entry Audit entry to log
 * @return 0 on success, negative on error
 */
int finance_compliance_log(const fin_audit_entry_t *entry);

/**
 * @brief Verify audit chain integrity
 * @return 0 if valid, -1 if corrupted
 */
int finance_compliance_verify_chain(void);

/**
 * @brief Get compliance status
 * @param status Output status
 * @return 0 on success, negative on error
 */
int finance_compliance_get_status(fin_compliance_status_t *status);

/**
 * @brief Generate regulatory report
 * @param regulation Regulation type
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written, negative on error
 */
int finance_compliance_generate_report(fin_compliance_t regulation,
                                       uint8_t *buffer, size_t max_len);

/**
 * @brief Shutdown compliance system
 */
void finance_compliance_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* FINANCE_COMPLIANCE_H */
