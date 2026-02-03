/**
 * @file trading_compliance.h
 * @brief Compliance Logging for Trading Standards in Quantitative Finance
 * 
 * INDUSTRY RELEVANCE:
 * Financial trading systems must maintain comprehensive audit trails for
 * regulatory compliance and market surveillance. This module provides
 * tamper-evident logging of all trading activity, order modifications,
 * and risk events for regulatory reporting and investigation support.
 * 
 * Applications:
 * - Regulatory audit trails (MiFID II, SEC)
 * - Best execution documentation
 * - Trade surveillance systems
 * - Compliance reporting
 * - Forensic investigation support
 * 
 * Standards: MiFID II/MiFIR, SEC Rule 17a-4, Dodd-Frank, MAR (Market Abuse Regulation)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TRADING_COMPLIANCE_H
#define GF_TRADING_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define COMPLIANCE_MAX_LOGS         100000  /* Maximum log entries */
#define COMPLIANCE_SYMBOL_LEN       16      /* Symbol name length */
#define COMPLIANCE_RETENTION_DAYS   2555    /* 7-year retention (MiFID II) */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Compliance event type */
typedef enum {
    COMP_ORDER_NEW,             /* New order submitted */
    COMP_ORDER_CANCEL,          /* Order canceled */
    COMP_ORDER_MODIFY,          /* Order modified */
    COMP_ORDER_REJECT,          /* Order rejected */
    COMP_FILL,                  /* Order filled */
    COMP_QUOTE,                 /* Quote submitted */
    COMP_RISK_BREACH,           /* Risk limit breached */
    COMP_KILL_SWITCH,           /* Kill switch activated */
    COMP_STRATEGY_START,        /* Strategy started */
    COMP_STRATEGY_STOP,         /* Strategy stopped */
    COMP_MANUAL_OVERRIDE,       /* Manual intervention */
    COMP_SUSPICIOUS_ACTIVITY    /* Potential market abuse */
} compliance_event_t;

/** Order type */
typedef enum {
    ORDER_MARKET,
    ORDER_LIMIT,
    ORDER_STOP,
    ORDER_STOP_LIMIT,
    ORDER_IOC,                  /* Immediate or cancel */
    ORDER_FOK,                  /* Fill or kill */
    ORDER_GTC                   /* Good til canceled */
} order_type_t;

/** Order side */
typedef enum {
    COMPLIANCE_BUY,
    COMPLIANCE_SELL,
    COMPLIANCE_SHORT
} compliance_side_t;

/** Compliance log entry */
typedef struct {
    uint64_t log_id;
    uint64_t timestamp_ns;
    compliance_event_t event;
    uint8_t strategy_id;
    uint64_t order_id;
    char symbol[COMPLIANCE_SYMBOL_LEN];
    order_type_t order_type;
    compliance_side_t side;
    int64_t price;
    uint64_t quantity;
    uint64_t filled_quantity;
    char venue[8];
    char reason[48];
    uint8_t hash[32];           /* SHA-256 chain hash */
} compliance_log_t;

/** Audit request */
typedef struct {
    uint64_t start_time_ns;
    uint64_t end_time_ns;
    char symbol_filter[COMPLIANCE_SYMBOL_LEN];
    uint8_t strategy_filter;
    compliance_event_t event_filter;
    uint32_t max_results;
} audit_request_t;

/** Compliance statistics */
typedef struct {
    uint64_t total_orders;
    uint64_t total_fills;
    uint64_t total_cancels;
    uint64_t total_rejects;
    uint16_t risk_breaches;
    uint16_t kill_switch_events;
    uint16_t suspicious_flags;
    uint64_t storage_bytes;
    uint32_t retention_days;
    bool integrity_verified;
} compliance_stats_t;

/** Module configuration */
typedef struct {
    const char *log_path;
    uint32_t max_entries;
    uint32_t retention_days;
    bool enable_chain_hash;
    bool enable_encryption;
    bool enable_compression;
} compliance_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int trading_compliance_init(const compliance_config_t *config);
void trading_compliance_shutdown(void);

int trading_compliance_log_order(uint64_t order_id, const char *symbol,
                                  order_type_t type, compliance_side_t side,
                                  int64_t price, uint64_t quantity);
int trading_compliance_log_fill(uint64_t order_id, int64_t fill_price,
                                 uint64_t fill_quantity, const char *venue);
int trading_compliance_log_cancel(uint64_t order_id, const char *reason);
int trading_compliance_log_event(compliance_event_t event, uint8_t strategy_id,
                                  const char *description);

int trading_compliance_query(const audit_request_t *request,
                              compliance_log_t *logs, uint32_t *count);
int trading_compliance_get_stats(compliance_stats_t *stats);
int trading_compliance_verify_integrity(uint64_t start_id, uint64_t end_id);

int trading_compliance_export(const char *filename, uint64_t start_time,
                               uint64_t end_time);
int trading_compliance_archive(uint32_t days_to_archive);

#endif /* GF_TRADING_COMPLIANCE_H */
