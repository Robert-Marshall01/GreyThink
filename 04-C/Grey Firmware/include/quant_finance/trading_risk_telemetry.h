/**
 * @file trading_risk_telemetry.h
 * @brief Risk Telemetry Collector for Quantitative Trading Systems
 * 
 * INDUSTRY RELEVANCE:
 * Algorithmic trading requires real-time risk monitoring to prevent
 * catastrophic losses and ensure regulatory compliance. This module
 * tracks position limits, PnL thresholds, market exposure, and
 * volatility metrics for automated kill-switch triggers.
 * 
 * Applications:
 * - Trading desk risk management
 * - Algorithmic strategy monitoring
 * - Portfolio risk analytics
 * - Regulatory capital calculations
 * - Pre-trade compliance checks
 * 
 * Standards: MiFID II, Dodd-Frank, Basel III, SEC Rule 15c3-5
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TRADING_RISK_TELEMETRY_H
#define GF_TRADING_RISK_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define RISK_MAX_STRATEGIES         64      /* Maximum trading strategies */
#define RISK_MAX_POSITIONS          1024    /* Maximum positions tracked */
#define RISK_TELEMETRY_RATE_HZ      100     /* Risk check frequency */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Risk level */
typedef enum {
    RISK_LEVEL_NORMAL,          /* Within all limits */
    RISK_LEVEL_ELEVATED,        /* Approaching limits */
    RISK_LEVEL_WARNING,         /* Limit threshold crossed */
    RISK_LEVEL_CRITICAL,        /* Hard limit breach */
    RISK_LEVEL_HALTED           /* Trading halted */
} risk_level_t;

/** Risk breach type */
typedef enum {
    BREACH_NONE = 0,
    BREACH_POSITION_LIMIT,      /* Position size exceeded */
    BREACH_DAILY_LOSS,          /* Daily loss limit hit */
    BREACH_DRAWDOWN,            /* Max drawdown exceeded */
    BREACH_EXPOSURE,            /* Gross exposure limit */
    BREACH_CONCENTRATION,       /* Sector concentration */
    BREACH_VELOCITY,            /* Order velocity limit */
    BREACH_VOLATILITY,          /* Vol-adjusted limit */
    BREACH_LIQUIDITY,           /* Liquidity threshold */
    BREACH_MARGIN               /* Margin call */
} risk_breach_t;

/** Position metrics */
typedef struct {
    uint32_t symbol_id;
    char symbol[16];
    int64_t quantity;           /* Signed quantity */
    int64_t avg_price;          /* Average entry price */
    int64_t market_value;       /* Current market value */
    int64_t unrealized_pnl;     /* Unrealized P&L */
    int64_t realized_pnl;       /* Realized P&L */
    float weight_pct;           /* Portfolio weight */
} position_metrics_t;

/** Strategy risk metrics */
typedef struct {
    uint8_t strategy_id;
    char strategy_name[32];
    risk_level_t level;
    int64_t daily_pnl;
    int64_t max_drawdown;
    int64_t gross_exposure;
    int64_t net_exposure;
    float sharpe_ratio;
    float volatility_ann;
    uint32_t orders_today;
    uint32_t fills_today;
    bool trading_enabled;
} strategy_risk_t;

/** Aggregate risk summary */
typedef struct {
    risk_level_t overall_level;
    int64_t total_pnl;
    int64_t total_exposure;
    float portfolio_var;        /* Value at Risk */
    float portfolio_cvar;       /* Conditional VaR */
    uint8_t strategies_active;
    uint8_t strategies_halted;
    uint16_t breaches_today;
    uint32_t orders_today;
    bool kill_switch_active;
} aggregate_risk_t;

/** Risk limit */
typedef struct {
    risk_breach_t type;
    int64_t soft_limit;         /* Warning threshold */
    int64_t hard_limit;         /* Halt threshold */
    int64_t current_value;
    bool breached;
} risk_limit_t;

/** Module configuration */
typedef struct {
    int64_t max_daily_loss;
    int64_t max_position_value;
    int64_t max_gross_exposure;
    float max_drawdown_pct;
    uint32_t max_orders_per_second;
    bool auto_kill_switch;
} risk_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int trading_risk_init(const risk_config_t *config);
void trading_risk_shutdown(void);

int trading_risk_register_strategy(uint8_t strategy_id, const char *name);
int trading_risk_update_position(uint32_t symbol_id, int64_t quantity, int64_t price);
int trading_risk_record_fill(uint8_t strategy_id, int64_t pnl);

int trading_risk_get_strategy(uint8_t strategy_id, strategy_risk_t *risk);
int trading_risk_get_aggregate(aggregate_risk_t *risk);
int trading_risk_get_position(uint32_t symbol_id, position_metrics_t *pos);

bool trading_risk_check_order(uint8_t strategy_id, int64_t order_value);
int trading_risk_enable_strategy(uint8_t strategy_id);
int trading_risk_disable_strategy(uint8_t strategy_id);
int trading_risk_kill_switch(bool activate);

void trading_risk_update(uint32_t elapsed_us);

#endif /* GF_TRADING_RISK_TELEMETRY_H */
