/**
 * @file finance_compliance.h
 * @brief Compliance Logging for Financial Regulatory Standards
 * 
 * INDUSTRY RELEVANCE:
 * Financial institutions face $10B+ annually in regulatory fines. Compliance
 * logging enables:
 * - Transaction audit trails for SEC/FINRA/FCA examinations
 * - Best execution documentation (MiFID II RTS 27/28)
 * - Trade reconstruction for T+1 reporting
 * - Anti-money laundering (AML) suspicious activity monitoring
 * 
 * Target applications: Broker-dealer compliance, exchange surveillance,
 * clearing house reporting, regulatory technology (RegTech), audit systems.
 * 
 * Standards: SEC Rule 17a-4 (records), MiFID II (best execution),
 *            Dodd-Frank (swap reporting), GDPR (data protection),
 *            SOX (internal controls), PCI DSS (card data)
 */

#ifndef GF_FINANCE_COMPLIANCE_H
#define GF_FINANCE_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Financial Compliance Types                                                 */
/*===========================================================================*/

typedef enum {
    REG_SEC_17A4,           /* SEC Rule 17a-4 (broker records) */
    REG_MIFID2,             /* MiFID II (EU markets) */
    REG_DODD_FRANK,         /* Dodd-Frank (swap reporting) */
    REG_EMIR,               /* EMIR (EU derivatives) */
    REG_CAT,                /* Consolidated Audit Trail */
    REG_GDPR,               /* Data protection */
    REG_AML_BSA,            /* Anti-Money Laundering */
    REG_CUSTOM
} regulation_type_t;

typedef enum {
    AUDIT_ORDER_RECEIVED,   /* Order received from client */
    AUDIT_ORDER_ROUTED,     /* Order routed to venue */
    AUDIT_ORDER_EXECUTED,   /* Order executed */
    AUDIT_ORDER_CANCELLED,  /* Order cancelled */
    AUDIT_TRADE_REPORTED,   /* Trade reported to TRF */
    AUDIT_POSITION_CHANGE,  /* Position changed */
    AUDIT_LIMIT_BREACH,     /* Risk limit breached */
    AUDIT_SUSPICIOUS_ACTIVITY, /* SAR flagged */
    AUDIT_DATA_ACCESS,      /* Data accessed */
    AUDIT_SYSTEM_EVENT      /* System event logged */
} audit_event_type_t;

typedef struct {
    char order_id[24];
    char client_id[16];
    char symbol[12];
    char side;              /* 'B' or 'S' */
    double quantity;
    double price;
    uint64_t timestamp_ns;
    char venue[8];
    char routing_id[16];
} order_record_t;

typedef struct {
    audit_event_type_t event;
    uint64_t timestamp_ns;
    char entity_id[24];     /* Order/trade/user ID */
    char user_id[16];       /* Acting user */
    char details[128];      /* Event details */
    uint8_t severity;       /* 0=info, 1=warning, 2=alert */
    uint32_t hash;          /* Integrity hash */
    uint32_t sequence;      /* Sequence number */
} audit_record_t;

typedef struct {
    char report_id[24];
    regulation_type_t regulation;
    uint32_t start_time;
    uint32_t end_time;
    uint32_t record_count;
    bool complete;
    bool submitted;
    uint32_t submission_time;
} compliance_report_t;

typedef struct {
    regulation_type_t regulation;
    bool enabled;
    uint32_t retention_years;   /* Record retention period */
    bool require_encryption;
    bool require_immutable;     /* WORM storage */
    char reporting_entity[32];
} compliance_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int finance_compliance_init(void);
int finance_compliance_shutdown(void);

int compliance_set_regulation(regulation_type_t reg, 
                             const compliance_config_t* config);

int compliance_log_order(const order_record_t* order);
int compliance_log_audit(const audit_record_t* record);

int compliance_query_orders(uint64_t start_time, uint64_t end_time,
                           order_record_t* orders, uint32_t max,
                           uint32_t* count);
int compliance_query_audit(uint64_t start_time, uint64_t end_time,
                          audit_record_t* records, uint32_t max,
                          uint32_t* count);

int compliance_generate_report(regulation_type_t reg, uint32_t start_time,
                              uint32_t end_time, compliance_report_t* report);
int compliance_submit_report(const compliance_report_t* report);

int compliance_verify_integrity(uint64_t start_time, uint64_t end_time,
                               bool* valid, uint32_t* errors);

bool compliance_is_audit_complete(uint32_t start_time, uint32_t end_time);

#endif /* GF_FINANCE_COMPLIANCE_H */
