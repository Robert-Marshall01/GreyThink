/**
 * @file inventory_telemetry.h
 * @brief Warehouse Inventory Telemetry Module Interface
 * 
 * INDUSTRY RELEVANCE:
 * Real-time inventory visibility enables:
 * - Just-in-time replenishment
 * - Shrinkage detection and prevention
 * - Demand forecasting accuracy
 * - Omnichannel fulfillment optimization
 * 
 * Technologies include RFID (Zebra, Impinj), computer vision (Standard AI),
 * and weight-based systems. Embedded engineers contribute:
 * - High-speed RFID reader interfaces
 * - Edge inference for product recognition
 * - Multi-zone coordination
 * - Real-time database synchronization
 * 
 * STANDARDS:
 * - GS1 (Global Trade Item Number - GTIN)
 * - EPCglobal (Electronic Product Code)
 * - ISO 18000 (RFID Air Interface)
 * - ISO 28000 (Supply Chain Security)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_INVENTORY_TELEMETRY_H
#define GF_INVENTORY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define INVENTORY_MAX_ZONES         256  /**< Maximum tracked zones */
#define INVENTORY_MAX_ITEMS         65536/**< Maximum tracked items */
#define RFID_TAG_SIZE               24   /**< EPC tag length bytes */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Zone type */
typedef enum {
    ZONE_RECEIVING,
    ZONE_STORAGE,
    ZONE_PICKING,
    ZONE_PACKING,
    ZONE_SHIPPING,
    ZONE_RETURNS,
    ZONE_QUALITY_HOLD
} zone_type_t;

/** Inventory event */
typedef enum {
    EVENT_ITEM_RECEIVED,
    EVENT_ITEM_STOWED,
    EVENT_ITEM_PICKED,
    EVENT_ITEM_PACKED,
    EVENT_ITEM_SHIPPED,
    EVENT_ITEM_RETURNED,
    EVENT_CYCLE_COUNT,
    EVENT_ADJUSTMENT
} inventory_event_t;

/** Item location */
typedef struct {
    char zone_id[16];
    char aisle[8];
    char rack[8];
    char shelf[8];
    char bin[8];
} item_location_t;

/** Inventory item */
typedef struct {
    uint8_t epc_tag[RFID_TAG_SIZE];
    char sku[32];
    char description[64];
    item_location_t location;
    uint16_t quantity;
    float weight_kg;
    uint32_t last_seen;
    bool in_transit;
} inventory_item_t;

/** Inventory metrics */
typedef struct {
    uint32_t total_items;
    uint32_t items_in_transit;
    uint32_t items_received_today;
    uint32_t items_shipped_today;
    float fill_rate_pct;
    float inventory_accuracy_pct;
    uint32_t timestamp;
} inventory_metrics_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int inventory_telemetry_init(void);
int inventory_register_zone(const char *zone_id, zone_type_t type);
int inventory_record_event(const uint8_t *epc_tag, inventory_event_t event,
                           const item_location_t *location);
int inventory_get_item(const uint8_t *epc_tag, inventory_item_t *item);
int inventory_get_metrics(inventory_metrics_t *metrics);
int inventory_cycle_count(const char *zone_id, uint32_t *count);
int inventory_sync_to_wms(void);  /**< Sync to Warehouse Management System */
void inventory_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_INVENTORY_TELEMETRY_H */
