/**
 * @file inventory_telemetry.h
 * @brief Retail Inventory Telemetry System
 * 
 * INDUSTRY RELEVANCE:
 * Real-time inventory visibility reduces out-of-stocks by 80% and improves
 * customer satisfaction. Robotic inventory scanning combined with telemetry
 * enables dynamic pricing, automated reordering, and planogram compliance
 * monitoring across thousands of SKUs.
 * 
 * KEY CAPABILITIES:
 * - Real-time stock level reporting
 * - Planogram compliance scoring
 * - Price tag verification
 * - Expiration date tracking
 * - Out-of-stock detection and alerting
 * - Shrinkage pattern analysis
 * 
 * DATA SOURCES:
 * - Robot vision scanning
 * - RFID reader integration
 * - Weight sensor shelves
 * - POS transaction correlation
 * 
 * INTEGRATION:
 * - WMS (Warehouse Management System)
 * - ERP inventory modules
 * - Demand forecasting systems
 * - Dynamic pricing engines
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_INVENTORY_TELEMETRY_H
#define GF_INVENTORY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Stock status */
typedef enum {
    STOCK_STATUS_IN_STOCK,
    STOCK_STATUS_LOW,
    STOCK_STATUS_OUT_OF_STOCK,
    STOCK_STATUS_OVERSTOCK,
    STOCK_STATUS_UNKNOWN
} stock_status_t;

/** Planogram compliance */
typedef enum {
    PLANO_COMPLIANT,
    PLANO_MISPLACED,
    PLANO_MISSING,
    PLANO_EXTRA,
    PLANO_WRONG_FACING
} plano_status_t;

/** Inventory scan result */
typedef struct {
    uint64_t sku;              /**< Item SKU */
    uint8_t aisle;
    uint8_t shelf;
    uint8_t position;
    uint16_t quantity;
    stock_status_t status;
    plano_status_t plano;
    uint32_t last_scan_time;
    float confidence;          /**< Detection confidence */
} inventory_item_t;

/** Zone summary */
typedef struct {
    uint8_t zone_id;
    uint16_t total_skus;
    uint16_t in_stock;
    uint16_t low_stock;
    uint16_t out_of_stock;
    float plano_compliance_pct;
    uint32_t last_scan_time;
} zone_summary_t;

/** Alert types */
typedef enum {
    ALERT_OUT_OF_STOCK,
    ALERT_LOW_STOCK,
    ALERT_PLANO_VIOLATION,
    ALERT_EXPIRED_ITEM,
    ALERT_PRICE_MISMATCH
} inventory_alert_t;

/** Alert record */
typedef struct {
    uint32_t alert_id;
    inventory_alert_t type;
    uint64_t sku;
    uint8_t aisle;
    uint8_t shelf;
    uint32_t timestamp;
    char description[64];
} alert_record_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize inventory telemetry
 * @param store_id Store identifier
 * @return 0 on success, negative on error
 */
int inv_telem_init(uint32_t store_id);

/**
 * @brief Report scanned item
 * @param item Inventory item data
 * @return 0 on success, negative on error
 */
int inv_telem_report_item(const inventory_item_t* item);

/**
 * @brief Get zone summary
 * @param zone_id Zone to query
 * @param summary Output summary
 * @return 0 on success, negative on error
 */
int inv_telem_get_zone(uint8_t zone_id, zone_summary_t* summary);

/**
 * @brief Get active alerts
 * @param alerts Output alert array
 * @param max_alerts Maximum to return
 * @param count Actual count
 * @return 0 on success, negative on error
 */
int inv_telem_get_alerts(alert_record_t* alerts, uint16_t max_alerts,
                         uint16_t* count);

/**
 * @brief Acknowledge alert
 * @param alert_id Alert to acknowledge
 * @return 0 on success, negative on error
 */
int inv_telem_ack_alert(uint32_t alert_id);

/**
 * @brief Generate inventory report
 * @param zone_id Zone (0 = all)
 * @param buffer Output buffer
 * @param max_len Maximum length
 * @param len Actual length
 * @return 0 on success, negative on error
 */
int inv_telem_generate_report(uint8_t zone_id, uint8_t* buffer,
                              uint32_t max_len, uint32_t* len);

/**
 * @brief Shutdown inventory telemetry
 * @return 0 on success, negative on error
 */
int inv_telem_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_INVENTORY_TELEMETRY_H */
