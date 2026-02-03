/**
 * @file inventory_telemetry.h
 * @brief Inventory Telemetry Module for Smart Retail
 * 
 * INDUSTRY RELEVANCE:
 * Real-time inventory tracking enables:
 * - Automatic reordering
 * - Shrinkage detection
 * - Planogram compliance
 * - Out-of-stock alerts
 * - Omnichannel fulfillment
 * 
 * Integration: ERP systems, WMS, POS
 * Companies: SAP, Oracle Retail, Blue Yonder
 */

#ifndef GF_INVENTORY_TELEMETRY_H
#define GF_INVENTORY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_INVENTORY_MAX_SKUS           10000
#define GF_INVENTORY_MAX_LOCATIONS      256
#define GF_INVENTORY_HISTORY_DAYS       30
#define GF_INVENTORY_SKU_SIZE           20
#define GF_INVENTORY_LOCATION_SIZE      16

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_INVENTORY_OK = 0,
    GF_INVENTORY_ERROR_NULL_PTR,
    GF_INVENTORY_ERROR_NOT_INITIALIZED,
    GF_INVENTORY_ERROR_SKU_NOT_FOUND,
    GF_INVENTORY_ERROR_LOCATION_NOT_FOUND,
    GF_INVENTORY_ERROR_QUANTITY_NEGATIVE,
    GF_INVENTORY_ERROR_STORAGE_FULL,
    GF_INVENTORY_ERROR_SYNC_FAILED,
    GF_INVENTORY_WARN_LOW_STOCK,
    GF_INVENTORY_WARN_OVERSTOCK
} gf_inventory_status_t;

typedef enum {
    GF_INV_EVENT_RECEIVED,      /* Stock received */
    GF_INV_EVENT_SOLD,          /* Sold to customer */
    GF_INV_EVENT_RETURNED,      /* Customer return */
    GF_INV_EVENT_DAMAGED,       /* Damaged/write-off */
    GF_INV_EVENT_TRANSFERRED,   /* Inter-store transfer */
    GF_INV_EVENT_ADJUSTED,      /* Manual adjustment */
    GF_INV_EVENT_SHRINKAGE      /* Theft/loss detected */
} gf_inventory_event_t;

typedef enum {
    GF_LOCATION_SHELF,
    GF_LOCATION_BACKROOM,
    GF_LOCATION_WAREHOUSE,
    GF_LOCATION_DISPLAY,
    GF_LOCATION_CHECKOUT,
    GF_LOCATION_RETURNS
} gf_location_type_t;

/**
 * @brief SKU definition
 */
typedef struct {
    char sku[GF_INVENTORY_SKU_SIZE];
    char description[64];
    char category[32];
    float unit_price;
    float unit_cost;
    uint16_t reorder_point;
    uint16_t reorder_qty;
    uint16_t max_stock;
    bool perishable;
    uint16_t shelf_life_days;
} gf_inventory_sku_t;

/**
 * @brief Location definition
 */
typedef struct {
    char location_id[GF_INVENTORY_LOCATION_SIZE];
    gf_location_type_t type;
    char zone[16];
    uint16_t capacity;
    bool refrigerated;
} gf_inventory_location_t;

/**
 * @brief Stock level
 */
typedef struct {
    char sku[GF_INVENTORY_SKU_SIZE];
    char location_id[GF_INVENTORY_LOCATION_SIZE];
    int32_t quantity;
    int32_t reserved;           /* Reserved for orders */
    int32_t available;          /* quantity - reserved */
    uint64_t last_updated_ms;
    uint64_t last_sold_ms;
} gf_stock_level_t;

/**
 * @brief Transaction record
 */
typedef struct {
    uint64_t transaction_id;
    uint64_t timestamp_ms;
    char sku[GF_INVENTORY_SKU_SIZE];
    char location_id[GF_INVENTORY_LOCATION_SIZE];
    gf_inventory_event_t event;
    int32_t quantity_change;
    int32_t quantity_after;
    char reference[32];         /* PO/SO/Transfer number */
} gf_inventory_transaction_t;

/**
 * @brief Velocity metrics
 */
typedef struct {
    char sku[GF_INVENTORY_SKU_SIZE];
    float daily_sales_avg;
    float weekly_sales_avg;
    float turn_rate;            /* Annual inventory turns */
    uint16_t days_of_supply;
    uint16_t stockout_count;
    float sell_through_pct;
} gf_inventory_velocity_t;

/**
 * @brief Alert thresholds
 */
typedef struct {
    uint16_t low_stock_threshold;
    uint16_t overstock_threshold;
    uint16_t velocity_drop_pct;
    uint16_t shrinkage_threshold;
    bool auto_reorder_enabled;
} gf_inventory_alert_config_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_inventory_alert_cb_t)(const char* sku,
                                         gf_inventory_status_t alert,
                                         int32_t quantity,
                                         void* user_data);

typedef void (*gf_inventory_transaction_cb_t)(const gf_inventory_transaction_t* txn,
                                               void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_inventory_status_t gf_inventory_init(void);
void gf_inventory_shutdown(void);

/* Master data */
gf_inventory_status_t gf_inventory_add_sku(const gf_inventory_sku_t* sku);
gf_inventory_status_t gf_inventory_add_location(const gf_inventory_location_t* loc);
gf_inventory_status_t gf_inventory_get_sku(const char* sku, gf_inventory_sku_t* out);

/* Stock operations */
gf_inventory_status_t gf_inventory_adjust(const char* sku,
                                           const char* location,
                                           gf_inventory_event_t event,
                                           int32_t qty_change,
                                           const char* reference);
gf_inventory_status_t gf_inventory_get_stock(const char* sku,
                                              const char* location,
                                              gf_stock_level_t* stock);
gf_inventory_status_t gf_inventory_get_total_stock(const char* sku, int32_t* total);
gf_inventory_status_t gf_inventory_reserve(const char* sku,
                                            const char* location,
                                            int32_t qty);
gf_inventory_status_t gf_inventory_release_reserve(const char* sku,
                                                    const char* location,
                                                    int32_t qty);

/* Analytics */
gf_inventory_status_t gf_inventory_get_velocity(const char* sku,
                                                 gf_inventory_velocity_t* velocity);
gf_inventory_status_t gf_inventory_get_low_stock(gf_stock_level_t* items,
                                                  uint32_t max_items,
                                                  uint32_t* count);

/* Alerts */
gf_inventory_status_t gf_inventory_configure_alerts(const gf_inventory_alert_config_t* config);
gf_inventory_status_t gf_inventory_register_alert_callback(gf_inventory_alert_cb_t cb,
                                                            void* user_data);
gf_inventory_status_t gf_inventory_register_transaction_callback(gf_inventory_transaction_cb_t cb,
                                                                  void* user_data);

/* Sync */
gf_inventory_status_t gf_inventory_sync_to_cloud(void);
gf_inventory_status_t gf_inventory_sync_from_cloud(void);

/* Periodic processing */
gf_inventory_status_t gf_inventory_process(void);

#endif /* GF_INVENTORY_TELEMETRY_H */
