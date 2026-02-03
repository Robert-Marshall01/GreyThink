/**
 * @file risk_telemetry.h
 * @brief Financial Risk Telemetry Collector Stub
 * 
 * Industry Relevance:
 * Real-time risk assessment is critical for algorithmic trading, DeFi protocols,
 * and payment fraud detection. This module provides:
 * - Portfolio risk metrics (VaR, CVaR, drawdown)
 * - Liquidity risk monitoring for DeFi positions
 * - Anomaly detection for fraud prevention
 * - Regulatory reporting data aggregation
 * 
 * Applications: Trading platforms, DeFi protocols, payment processors, risk management
 * 
 * @author Grey Firmware Project
 */

#ifndef RISK_TELEMETRY_H
#define RISK_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Risk level classification */
typedef enum {
    RISK_LEVEL_LOW,
    RISK_LEVEL_MODERATE,
    RISK_LEVEL_ELEVATED,
    RISK_LEVEL_HIGH,
    RISK_LEVEL_CRITICAL
} risk_level_t;

/** Risk category */
typedef enum {
    RISK_MARKET,             /**< Market/price risk */
    RISK_CREDIT,             /**< Counterparty risk */
    RISK_LIQUIDITY,          /**< Liquidity risk */
    RISK_OPERATIONAL,        /**< Operational risk */
    RISK_FRAUD               /**< Fraud/security risk */
} risk_category_t;

/** Portfolio risk metrics */
typedef struct {
    float value_at_risk;     /**< 99% VaR (percentage) */
    float conditional_var;   /**< CVaR/Expected Shortfall */
    float max_drawdown;      /**< Maximum drawdown */
    float sharpe_ratio;      /**< Risk-adjusted return */
    float liquidity_score;   /**< Liquidity (0-100) */
    risk_level_t overall;    /**< Overall risk level */
} portfolio_risk_t;

/** Risk event for alerting */
typedef struct {
    uint32_t event_id;       /**< Unique event ID */
    uint32_t timestamp;      /**< Event timestamp */
    risk_category_t category;/**< Risk category */
    risk_level_t level;      /**< Severity level */
    float metric_value;      /**< Triggering metric value */
    float threshold;         /**< Configured threshold */
    char description[64];    /**< Event description */
} risk_event_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize risk telemetry system
 * @return 0 on success, negative on error
 */
int risk_telemetry_init(void);

/**
 * @brief Update portfolio metrics
 * @param elapsed_ms Time since last update
 * @return 0 on success, negative on error
 */
int risk_telemetry_update(uint32_t elapsed_ms);

/**
 * @brief Get current portfolio risk
 * @param risk Output risk metrics
 * @return 0 on success, negative on error
 */
int risk_telemetry_get_portfolio(portfolio_risk_t *risk);

/**
 * @brief Set risk threshold for alerts
 * @param category Risk category
 * @param threshold Alert threshold
 * @return 0 on success, negative on error
 */
int risk_telemetry_set_threshold(risk_category_t category, float threshold);

/**
 * @brief Get pending risk events
 * @param events Output event array
 * @param max_events Maximum events to return
 * @return Number of events, negative on error
 */
int risk_telemetry_get_events(risk_event_t *events, uint16_t max_events);

/**
 * @brief Acknowledge risk event
 * @param event_id Event to acknowledge
 * @return 0 on success, negative on error
 */
int risk_telemetry_ack_event(uint32_t event_id);

/**
 * @brief Shutdown risk telemetry
 */
void risk_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* RISK_TELEMETRY_H */
