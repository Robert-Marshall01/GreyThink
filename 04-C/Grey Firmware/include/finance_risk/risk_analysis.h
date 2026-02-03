/**
 * @file risk_analysis.h
 * @brief Risk Analysis Module for Edge Finance Risk Systems
 * 
 * INDUSTRY RELEVANCE:
 * Real-time risk management is mandatory for financial institutions under
 * Basel III/IV, Dodd-Frank, and MiFID II. Edge risk analysis enables:
 * - Pre-trade risk checks with sub-millisecond latency
 * - Position limit enforcement
 * - VaR (Value at Risk) calculation
 * - Greeks computation for derivatives (Delta, Gamma, Vega)
 * 
 * Target applications: Trading risk systems, portfolio management, clearing
 * houses, prime brokerage, regulatory capital calculation.
 * 
 * Standards: Basel III/IV (capital), FRTB (market risk), ISDA SIMM (margin),
 *            VaR 99%/99.9%, Expected Shortfall (ES)
 */

#ifndef GF_RISK_ANALYSIS_H
#define GF_RISK_ANALYSIS_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Risk Analysis Types                                                        */
/*===========================================================================*/

typedef enum {
    ASSET_EQUITY,
    ASSET_FIXED_INCOME,
    ASSET_FX,
    ASSET_COMMODITY,
    ASSET_DERIVATIVE,
    ASSET_CRYPTO
} asset_class_t;

typedef enum {
    RISK_MARKET,            /* Market risk */
    RISK_CREDIT,            /* Credit/counterparty risk */
    RISK_LIQUIDITY,         /* Liquidity risk */
    RISK_OPERATIONAL,       /* Operational risk */
    RISK_CONCENTRATION      /* Concentration risk */
} risk_type_t;

typedef enum {
    LIMIT_NOTIONAL,         /* Total notional value */
    LIMIT_VAR,              /* VaR limit */
    LIMIT_GREEK_DELTA,      /* Delta exposure limit */
    LIMIT_GREEK_GAMMA,      /* Gamma exposure limit */
    LIMIT_GREEK_VEGA,       /* Vega exposure limit */
    LIMIT_POSITION,         /* Position size limit */
    LIMIT_LOSS_DAILY        /* Daily loss limit */
} limit_type_t;

typedef struct {
    char symbol[12];
    asset_class_t asset_class;
    double quantity;
    double avg_price;
    double market_value;
    double unrealized_pnl;
    double realized_pnl;
} position_t;

typedef struct {
    double delta;           /* Price sensitivity */
    double gamma;           /* Delta change rate */
    double vega;            /* Volatility sensitivity */
    double theta;           /* Time decay */
    double rho;             /* Interest rate sensitivity */
} greeks_t;

typedef struct {
    double var_95;          /* 95% VaR */
    double var_99;          /* 99% VaR */
    double var_999;         /* 99.9% VaR */
    double expected_shortfall; /* Expected Shortfall (ES) */
    double stress_loss;     /* Stress test loss */
    uint32_t holding_period_days;
    uint32_t confidence_pct;
} var_result_t;

typedef struct {
    limit_type_t type;
    double limit_value;
    double current_value;
    double utilization_pct;
    bool breached;
    bool warning;           /* >80% utilization */
} limit_status_t;

typedef struct {
    uint16_t lookback_days;     /* Historical lookback */
    uint16_t scenarios;         /* Monte Carlo scenarios */
    double confidence_level;    /* VaR confidence */
    bool enable_stress_test;
    double stress_multiplier;
} risk_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int risk_analysis_init(const risk_config_t* config);
int risk_analysis_shutdown(void);

int risk_update_position(const position_t* position);
int risk_calculate_portfolio_var(var_result_t* result);
int risk_calculate_greeks(const char* symbol, greeks_t* greeks);

int risk_check_pre_trade(const char* symbol, double quantity, double price,
                        bool* approved, char* reject_reason);

int risk_set_limit(limit_type_t type, double value);
int risk_get_limit_status(limit_type_t type, limit_status_t* status);
int risk_get_all_limits(limit_status_t* limits, uint8_t max, uint8_t* count);

int risk_run_stress_test(const char* scenario, double* impact);
int risk_get_exposure_by_class(asset_class_t class, double* exposure);

bool risk_is_trading_allowed(void);
int risk_generate_report(uint8_t* data, uint32_t max_len, uint32_t* len);

#endif /* GF_RISK_ANALYSIS_H */
