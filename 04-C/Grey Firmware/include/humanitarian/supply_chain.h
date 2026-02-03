/**
 * @file supply_chain.h
 * @brief Humanitarian Supply Chain Telemetry
 * 
 * INDUSTRY RELEVANCE:
 * Humanitarian organizations (UNHCR, WFP, Red Cross) manage complex
 * supply chains during crises. IoT tracking of supplies from warehouse
 * to last-mile delivery ensures aid reaches those in need and prevents
 * diversion or spoilage (especially cold chain for vaccines).
 * 
 * This stub demonstrates:
 * - Asset tracking and geolocation
 * - Cold chain temperature monitoring
 * - Arrival confirmation and receipt
 * - Supply chain visibility
 * 
 * STANDARDS:
 * - Sphere Humanitarian Standards
 * - GS1 standards (barcodes, RFID)
 * - WHO PQS (Vaccine cold chain)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_HUMANITARIAN_SUPPLY_CHAIN_H
#define GF_HUMANITARIAN_SUPPLY_CHAIN_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    SUPPLY_STATUS_SOURCED,
    SUPPLY_STATUS_WAREHOUSE,
    SUPPLY_STATUS_IN_TRANSIT,
    SUPPLY_STATUS_AT_HUB,
    SUPPLY_STATUS_LAST_MILE,
    SUPPLY_STATUS_DELIVERED,
    SUPPLY_STATUS_VERIFIED
} supply_status_t;

typedef enum {
    SUPPLY_TYPE_FOOD,
    SUPPLY_TYPE_WATER,
    SUPPLY_TYPE_MEDICAL,
    SUPPLY_TYPE_SHELTER,
    SUPPLY_TYPE_HYGIENE,
    SUPPLY_TYPE_NFI,         /**< Non-food items */
    SUPPLY_TYPE_VACCINE      /**< Cold chain required */
} supply_type_t;

typedef struct {
    uint32_t shipment_id;
    supply_type_t type;
    supply_status_t status;
    float latitude;
    float longitude;
    float temperature_c;
    uint32_t qty_units;
    uint32_t beneficiaries;
    uint64_t eta_timestamp;
} supply_telemetry_t;

int supply_chain_init(void);
int supply_chain_track(uint32_t shipment_id, supply_telemetry_t *telem);
int supply_chain_update_status(uint32_t shipment_id, supply_status_t status);
int supply_chain_confirm_delivery(uint32_t shipment_id, uint32_t beneficiaries);

#ifdef __cplusplus
}
#endif

#endif /* GF_HUMANITARIAN_SUPPLY_CHAIN_H */
