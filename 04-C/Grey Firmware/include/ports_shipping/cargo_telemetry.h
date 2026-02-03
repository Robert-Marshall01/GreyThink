/**
 * @file cargo_telemetry.h
 * @brief Cargo Tracking Telemetry for Smart Ports & Shipping
 * 
 * INDUSTRY RELEVANCE:
 * Supply chain visibility is critical for $28 trillion in global trade.
 * Real-time cargo tracking enables:
 * - Cold chain monitoring (pharmaceuticals, food)
 * - Location tracking via GPS/BLE/RFID
 * - Condition monitoring (shock, tilt, humidity)
 * - Customs pre-clearance and documentation
 * 
 * Target applications: Smart containers, IoT cargo trackers, supply chain
 * visibility platforms, cold chain logistics, high-value freight monitoring.
 * 
 * Standards: ISO 18185 (container seals), ISO 17712 (security seals),
 *            GS1 EPCIS (supply chain events)
 */

#ifndef GF_CARGO_TELEMETRY_H
#define GF_CARGO_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Cargo Telemetry Types                                                      */
/*===========================================================================*/

typedef enum {
    CARGO_TYPE_DRY,         /* Standard dry container */
    CARGO_TYPE_REEFER,      /* Refrigerated container */
    CARGO_TYPE_TANK,        /* Tank container (liquids) */
    CARGO_TYPE_FLAT,        /* Flat rack container */
    CARGO_TYPE_OPEN         /* Open top container */
} cargo_type_t;

typedef enum {
    CARGO_EVENT_LOADED,     /* Loaded onto vessel */
    CARGO_EVENT_UNLOADED,   /* Unloaded from vessel */
    CARGO_EVENT_GATE_IN,    /* Entered terminal gate */
    CARGO_EVENT_GATE_OUT,   /* Exited terminal gate */
    CARGO_EVENT_SEAL_BREAK, /* Security seal broken */
    CARGO_EVENT_SHOCK,      /* Impact detected */
    CARGO_EVENT_TILT,       /* Excessive tilt detected */
    CARGO_EVENT_TEMP_ALARM  /* Temperature excursion */
} cargo_event_t;

typedef struct {
    char container_id[12];  /* ISO 6346 container ID */
    cargo_type_t type;
    float latitude;
    float longitude;
    float temperature_c;    /* Internal temperature */
    float humidity_pct;     /* Internal humidity */
    float shock_g;          /* Last shock magnitude */
    float tilt_deg;         /* Current tilt angle */
    bool door_open;         /* Door sensor state */
    bool seal_intact;       /* Security seal intact */
    uint32_t timestamp;     /* Unix timestamp */
} cargo_state_t;

typedef struct {
    float temp_min_c;       /* Minimum allowed temperature */
    float temp_max_c;       /* Maximum allowed temperature */
    float humidity_max;     /* Maximum humidity */
    float shock_threshold;  /* Shock alarm threshold (G) */
    float tilt_threshold;   /* Tilt alarm threshold (deg) */
    uint32_t report_interval_s; /* Telemetry interval */
} cargo_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int cargo_telemetry_init(const char* container_id, cargo_type_t type);
int cargo_telemetry_shutdown(void);

int cargo_set_config(const cargo_config_t* config);
int cargo_get_state(cargo_state_t* state);

int cargo_log_event(cargo_event_t event);
int cargo_get_event_history(cargo_event_t* events, uint16_t max_events);

int cargo_set_gps_position(float lat, float lon);
int cargo_transmit_telemetry(void);

bool cargo_is_cold_chain_compliant(void);

#endif /* GF_CARGO_TELEMETRY_H */
