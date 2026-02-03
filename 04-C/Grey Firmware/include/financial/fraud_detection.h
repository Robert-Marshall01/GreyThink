/**
 * @file fraud_detection.h
 * @brief Fraud Detection Module for Edge Financial Systems
 * 
 * @details
 * This module provides real-time fraud detection capabilities for
 * embedded payment systems, using rule-based and ML-based approaches
 * suitable for resource-constrained devices.
 * 
 * INDUSTRY RELEVANCE:
 * - Payment Networks: Visa, Mastercard real-time fraud scoring
 * - Banks: Transaction monitoring and suspicious activity detection
 * - E-commerce: Card-not-present fraud prevention
 * - ATM Networks: Skimming and cash-out attack detection
 * - Mobile Payments: Device fingerprinting and behavioral analysis
 * 
 * COMPLIANCE:
 * - PSD2 SCA: Strong Customer Authentication (EU)
 * - AML/BSA: Anti-Money Laundering / Bank Secrecy Act
 * - FFIEC: Federal Financial Institutions Examination Council
 * - PCI DSS: Fraud monitoring requirements
 * 
 * DETECTION METHODS:
 * - Velocity checks (transaction frequency)
 * - Geolocation anomalies
 * - Amount pattern analysis
 * - Device fingerprint matching
 * - Behavioral biometrics
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_FRAUD_DETECTION_H
#define GF_FRAUD_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum rules */
#define GF_FRAUD_MAX_RULES              64

/** Risk score range */
#define GF_FRAUD_SCORE_MAX              1000

/** High-risk threshold */
#define GF_FRAUD_HIGH_RISK_THRESHOLD    700

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Risk level
 */
typedef enum {
    GF_RISK_LOW,                  /**< Low risk */
    GF_RISK_MEDIUM,               /**< Medium risk */
    GF_RISK_HIGH,                 /**< High risk */
    GF_RISK_CRITICAL              /**< Block transaction */
} gf_risk_level_t;

/**
 * @brief Fraud indicator type
 */
typedef enum {
    GF_FRAUD_VELOCITY,            /**< High velocity */
    GF_FRAUD_GEOLOCATION,         /**< Location anomaly */
    GF_FRAUD_AMOUNT,              /**< Amount anomaly */
    GF_FRAUD_DEVICE,              /**< Unknown device */
    GF_FRAUD_TIME,                /**< Unusual time */
    GF_FRAUD_PATTERN,             /**< Spending pattern */
    GF_FRAUD_BLACKLIST,           /**< Blacklisted entity */
    GF_FRAUD_CARD_TEST            /**< Card testing attack */
} gf_fraud_indicator_t;

/**
 * @brief Transaction context for fraud check
 */
typedef struct {
    uint64_t amount_cents;        /**< Transaction amount */
    char merchant_category[8];    /**< MCC code */
    char country[4];              /**< Country code */
    float latitude;               /**< Location latitude */
    float longitude;              /**< Location longitude */
    uint32_t timestamp;           /**< Transaction time */
    uint8_t device_fingerprint[16]; /**< Device hash */
    char card_hash[16];           /**< Card identifier hash */
    uint32_t txn_count_1h;        /**< Transactions in last hour */
    uint32_t txn_count_24h;       /**< Transactions in last 24h */
} gf_fraud_context_t;

/**
 * @brief Fraud assessment result
 */
typedef struct {
    uint16_t risk_score;          /**< Risk score (0-1000) */
    gf_risk_level_t risk_level;   /**< Risk level */
    uint8_t indicator_count;      /**< Number of indicators */
    gf_fraud_indicator_t indicators[8]; /**< Triggered indicators */
    bool recommend_block;         /**< Recommend blocking */
    bool require_step_up;         /**< Require step-up auth */
    char reason[64];              /**< Primary reason */
} gf_fraud_result_t;

/**
 * @brief Fraud rule definition
 */
typedef struct {
    uint8_t rule_id;              /**< Rule identifier */
    gf_fraud_indicator_t type;    /**< Indicator type */
    uint16_t score_contribution;  /**< Score contribution */
    bool enabled;                 /**< Rule enabled */
    char description[64];         /**< Rule description */
} gf_fraud_rule_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize fraud detection
 * @return 0 on success
 */
int gf_fraud_init(void);

/**
 * @brief Shutdown fraud detection
 */
void gf_fraud_shutdown(void);

/**
 * @brief Assess transaction for fraud
 * @param context Transaction context
 * @param result Output assessment
 * @return 0 on success
 */
int gf_fraud_assess(const gf_fraud_context_t* context, 
                     gf_fraud_result_t* result);

/**
 * @brief Add to velocity tracker
 * @param card_hash Card identifier
 * @param amount Amount
 * @return 0 on success
 */
int gf_fraud_track_velocity(const char* card_hash, uint64_t amount);

/**
 * @brief Check blacklist
 * @param card_hash Card identifier
 * @return true if blacklisted
 */
bool gf_fraud_check_blacklist(const char* card_hash);

/**
 * @brief Add to blacklist
 * @param card_hash Card identifier
 * @param reason Reason
 * @return 0 on success
 */
int gf_fraud_add_blacklist(const char* card_hash, const char* reason);

/**
 * @brief Get rule configuration
 * @param rule_id Rule ID
 * @param rule Output rule
 * @return 0 on success
 */
int gf_fraud_get_rule(uint8_t rule_id, gf_fraud_rule_t* rule);

/**
 * @brief Update rule
 * @param rule Updated rule
 * @return 0 on success
 */
int gf_fraud_update_rule(const gf_fraud_rule_t* rule);

#ifdef __cplusplus
}
#endif

#endif /* GF_FRAUD_DETECTION_H */
