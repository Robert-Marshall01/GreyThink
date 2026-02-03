/**
 * @file supply_telemetry.h
 * @brief Emergency Supply Chain Telemetry
 * 
 * INDUSTRY RELEVANCE:
 * Disaster logistics requires real-time tracking of supplies, conditions, and
 * delivery status. This module provides end-to-end visibility for humanitarian
 * aid, medical supplies, and critical resources with cold chain monitoring
 * and chain-of-custody tracking.
 * 
 * Key applications:
 * - FEMA/Red Cross supply chain visibility
 * - Vaccine cold chain monitoring
 * - Last-mile delivery tracking
 * - Resource allocation optimization
 * - Donor accountability reporting
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_SUPPLY_TELEMETRY_H
#define GF_SUPPLY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define SUPPLY_MAX_ITEMS            256     /**< Max tracked items */
#define SUPPLY_MAX_WAYPOINTS        32      /**< Max route waypoints */
#define SUPPLY_TEMP_ALERT_HIGH      8.0f    /**< Cold chain high alert (C) */
#define SUPPLY_TEMP_ALERT_LOW       2.0f    /**< Cold chain low alert (C) */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Supply category
 */
typedef enum {
    SUPPLY_CAT_MEDICAL = 0,         /**< Medical supplies */
    SUPPLY_CAT_WATER,               /**< Potable water */
    SUPPLY_CAT_FOOD,                /**< Food/nutrition */
    SUPPLY_CAT_SHELTER,             /**< Shelter materials */
    SUPPLY_CAT_EQUIPMENT,           /**< Equipment/tools */
    SUPPLY_CAT_FUEL                 /**< Fuel/energy */
} supply_category_t;

/**
 * @brief Item condition
 */
typedef enum {
    SUPPLY_COND_NORMAL = 0,
    SUPPLY_COND_DAMAGED,
    SUPPLY_COND_EXPIRED,
    SUPPLY_COND_TEMP_EXCURSION,
    SUPPLY_COND_CONTAMINATED,
    SUPPLY_COND_MISSING
} supply_condition_t;

/**
 * @brief Tracked item
 */
typedef struct {
    uint32_t item_id;
    supply_category_t category;
    char description[64];
    uint16_t quantity;
    char unit[16];
    float weight_kg;
    float temperature;              /**< Current temp (C) */
    supply_condition_t condition;
    bool cold_chain_required;
    uint32_t expiry_date;           /**< Unix timestamp */
} supply_item_t;

/**
 * @brief Shipment status
 */
typedef struct {
    uint32_t shipment_id;
    float latitude;
    float longitude;
    uint8_t waypoint_current;
    uint8_t waypoint_total;
    float eta_minutes;
    char carrier[32];
    bool in_transit;
    bool delivered;
} supply_shipment_t;

/**
 * @brief Demand request
 */
typedef struct {
    uint32_t request_id;
    supply_category_t category;
    uint16_t quantity_needed;
    char location_name[64];
    float latitude;
    float longitude;
    uint8_t priority;               /**< 0=critical, 255=low */
    uint32_t deadline;
    bool fulfilled;
} supply_demand_t;

/**
 * @brief Chain telemetry
 */
typedef struct {
    uint32_t items_tracked;
    uint32_t shipments_active;
    uint32_t items_delivered;
    uint32_t temp_excursions;
    uint32_t items_damaged;
    float fulfillment_rate;
} supply_stats_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize supply telemetry
 * @return 0 on success, negative on error
 */
int supply_tlm_init(void);

/**
 * @brief Shutdown telemetry
 * @return 0 on success, negative on error
 */
int supply_tlm_shutdown(void);

/**
 * @brief Register supply item
 * @param item Item details
 * @return Item ID on success, negative on error
 */
int supply_register_item(const supply_item_t* item);

/**
 * @brief Update item status
 * @param item_id Item ID
 * @param condition New condition
 * @param temperature Current temperature
 * @return 0 on success, negative on error
 */
int supply_update_item(uint32_t item_id, supply_condition_t condition,
                       float temperature);

/**
 * @brief Create shipment
 * @param item_ids Array of item IDs
 * @param item_count Number of items
 * @param dest_lat Destination latitude
 * @param dest_lon Destination longitude
 * @return Shipment ID on success, negative on error
 */
int supply_create_shipment(const uint32_t* item_ids, uint8_t item_count,
                           float dest_lat, float dest_lon);

/**
 * @brief Update shipment location
 * @param shipment_id Shipment ID
 * @param lat Current latitude
 * @param lon Current longitude
 * @return 0 on success, negative on error
 */
int supply_update_location(uint32_t shipment_id, float lat, float lon);

/**
 * @brief Register demand request
 * @param demand Demand details
 * @return Request ID on success, negative on error
 */
int supply_register_demand(const supply_demand_t* demand);

/**
 * @brief Get supply chain statistics
 * @param stats Output statistics
 * @return 0 on success, negative on error
 */
int supply_get_stats(supply_stats_t* stats);

/**
 * @brief Process telemetry (check alerts, update ETAs)
 * @return 0 on success, negative on error
 */
int supply_tlm_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SUPPLY_TELEMETRY_H */
