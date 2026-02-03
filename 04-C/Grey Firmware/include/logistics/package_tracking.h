/**
 * @file package_tracking.h
 * @brief Package Tracking Telemetry for Logistics Systems
 * 
 * @details
 * This module provides real-time package tracking telemetry including
 * barcode/RFID scanning, location updates, condition monitoring, and
 * chain-of-custody logging for logistics operations.
 * 
 * INDUSTRY RELEVANCE:
 * - Express delivery (FedEx, UPS, DHL)
 * - E-commerce fulfillment (Amazon, Shopify)
 * - Cold chain logistics (pharmaceuticals, food)
 * - Last-mile delivery tracking
 * - Customs and border processing
 * - Supply chain visibility platforms
 * 
 * KEY FEATURES:
 * - Multi-scanner support (1D, 2D, RFID)
 * - IoT sensor integration (temp, shock, tilt)
 * - GPS/indoor location fusion
 * - Event-driven status updates
 * - Blockchain/DLT integration ready
 * - Chain-of-custody audit trails
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_PACKAGE_TRACKING_H
#define GF_PACKAGE_TRACKING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_PT_OK = 0,
    GF_PT_ERROR_NOT_INITIALIZED,
    GF_PT_ERROR_NULL_PTR,
    GF_PT_ERROR_SCAN_FAILED,
    GF_PT_ERROR_INVALID_BARCODE,
    GF_PT_ERROR_QUEUE_FULL,
    GF_PT_ERROR_COMMS,
    GF_PT_WARN_TEMPERATURE,
    GF_PT_WARN_SHOCK_DETECTED
} gf_pt_status_t;

typedef enum {
    GF_PT_SCAN_1D_BARCODE,        /**< 1D barcode (UPC, Code128) */
    GF_PT_SCAN_2D_BARCODE,        /**< 2D barcode (QR, DataMatrix) */
    GF_PT_SCAN_RFID_UHF,          /**< UHF RFID (EPC Gen2) */
    GF_PT_SCAN_RFID_HF,           /**< HF RFID (NFC, ISO 15693) */
    GF_PT_SCAN_OCR                /**< Optical character recognition */
} gf_pt_scan_type_t;

typedef enum {
    GF_PT_EVENT_RECEIVED,         /**< Package received at facility */
    GF_PT_EVENT_SORTED,           /**< Sorted to destination bin */
    GF_PT_EVENT_LOADED,           /**< Loaded onto vehicle */
    GF_PT_EVENT_IN_TRANSIT,       /**< In transit update */
    GF_PT_EVENT_ARRIVED,          /**< Arrived at destination */
    GF_PT_EVENT_OUT_FOR_DELIVERY, /**< Out for delivery */
    GF_PT_EVENT_DELIVERED,        /**< Successfully delivered */
    GF_PT_EVENT_RETURNED,         /**< Returned to sender */
    GF_PT_EVENT_EXCEPTION,        /**< Delivery exception */
    GF_PT_EVENT_DAMAGED           /**< Damage detected */
} gf_pt_event_type_t;

typedef struct {
    char tracking_number[32];     /**< Tracking number */
    char carrier_code[8];         /**< Carrier code */
    float weight_kg;              /**< Package weight */
    float length_cm;              /**< Length */
    float width_cm;               /**< Width */
    float height_cm;              /**< Height */
    bool requires_signature;      /**< Signature required */
    bool temperature_sensitive;   /**< Cold chain package */
    bool fragile;                 /**< Fragile handling */
} gf_pt_package_t;

typedef struct {
    double latitude;              /**< GPS latitude */
    double longitude;             /**< GPS longitude */
    float accuracy_m;             /**< Position accuracy */
    uint32_t facility_id;         /**< Facility ID if indoor */
    uint16_t zone_id;             /**< Zone within facility */
    uint64_t timestamp_ms;        /**< Location timestamp */
} gf_pt_location_t;

typedef struct {
    float temperature_c;          /**< Current temperature */
    float temp_min_c;             /**< Min temp recorded */
    float temp_max_c;             /**< Max temp recorded */
    float humidity_pct;           /**< Relative humidity */
    float shock_g;                /**< Max shock (g-force) */
    uint8_t tilt_deg;             /**< Tilt angle */
    bool light_exposure;          /**< Light exposure detected */
} gf_pt_condition_t;

typedef struct {
    char tracking_number[32];     /**< Tracking number */
    gf_pt_event_type_t event;     /**< Event type */
    gf_pt_location_t location;    /**< Event location */
    gf_pt_condition_t condition;  /**< Package condition */
    uint32_t handler_id;          /**< Handler/operator ID */
    uint64_t timestamp_ms;        /**< Event timestamp */
    char notes[64];               /**< Additional notes */
} gf_pt_tracking_event_t;

typedef void (*gf_pt_scan_cb_t)(const char* code, gf_pt_scan_type_t type, void* user_data);
typedef void (*gf_pt_alert_cb_t)(const char* tracking, gf_pt_status_t alert, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_pt_status_t gf_pt_init(uint32_t facility_id);
void gf_pt_shutdown(void);
gf_pt_status_t gf_pt_scan(gf_pt_scan_type_t type, char* code, uint16_t max_len);
gf_pt_status_t gf_pt_register_package(const gf_pt_package_t* package);
gf_pt_status_t gf_pt_log_event(const gf_pt_tracking_event_t* event);
gf_pt_status_t gf_pt_update_condition(const char* tracking, const gf_pt_condition_t* condition);
gf_pt_status_t gf_pt_get_history(const char* tracking, gf_pt_tracking_event_t* events,
                                  uint8_t max_events, uint8_t* count);
gf_pt_status_t gf_pt_register_scan_callback(gf_pt_scan_cb_t callback, void* user_data);
gf_pt_status_t gf_pt_register_alert_callback(gf_pt_alert_cb_t callback, void* user_data);
gf_pt_status_t gf_pt_flush_telemetry(void);
gf_pt_status_t gf_pt_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PACKAGE_TRACKING_H */
