/**
 * @file emergency_beacon.h
 * @brief Emergency Beacon Protocol for Disaster Response
 * 
 * INDUSTRY RELEVANCE:
 * Emergency beacons save lives in:
 * - Maritime distress (EPIRB)
 * - Aviation (ELT)
 * - Personal locator beacons (PLB)
 * - Avalanche rescue
 * - Building collapse (structural monitoring)
 * 
 * Standards: COSPAS-SARSAT, AIS, ADS-B
 * Companies: ACR Electronics, McMurdo, Orolia
 */

#ifndef GF_EMERGENCY_BEACON_H
#define GF_EMERGENCY_BEACON_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_BEACON_ID_SIZE           15      /* 15-char hex ID */
#define GF_BEACON_MSG_MAX_SIZE      128
#define GF_BEACON_MAX_CONTACTS      4

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_BEACON_OK = 0,
    GF_BEACON_ERROR_NULL_PTR,
    GF_BEACON_ERROR_NOT_INITIALIZED,
    GF_BEACON_ERROR_NO_GPS,
    GF_BEACON_ERROR_TX_FAILED,
    GF_BEACON_ERROR_BATTERY_LOW,
    GF_BEACON_ERROR_ANTENNA_FAULT,
    GF_BEACON_ERROR_NOT_REGISTERED,
    GF_BEACON_WARN_WEAK_SIGNAL,
    GF_BEACON_WARN_GPS_DEGRADED
} gf_beacon_status_t;

typedef enum {
    GF_BEACON_TYPE_EPIRB,       /* Emergency Position Indicating Radio Beacon */
    GF_BEACON_TYPE_ELT,         /* Emergency Locator Transmitter (aviation) */
    GF_BEACON_TYPE_PLB,         /* Personal Locator Beacon */
    GF_BEACON_TYPE_AIS_SART,    /* AIS Search and Rescue Transmitter */
    GF_BEACON_TYPE_AVALANCHE,   /* Avalanche transceiver */
    GF_BEACON_TYPE_STRUCTURAL   /* Building/infrastructure beacon */
} gf_beacon_type_t;

typedef enum {
    GF_BEACON_STATE_IDLE,
    GF_BEACON_STATE_STANDBY,
    GF_BEACON_STATE_ARMED,
    GF_BEACON_STATE_TRANSMITTING,
    GF_BEACON_STATE_ACKNOWLEDGED,
    GF_BEACON_STATE_TEST_MODE,
    GF_BEACON_STATE_FAULT
} gf_beacon_state_t;

typedef enum {
    GF_DISTRESS_UNSPECIFIED,
    GF_DISTRESS_FIRE,
    GF_DISTRESS_FLOODING,
    GF_DISTRESS_COLLISION,
    GF_DISTRESS_MEDICAL,
    GF_DISTRESS_ABANDONING,
    GF_DISTRESS_PERSON_OVERBOARD,
    GF_DISTRESS_CRASH,
    GF_DISTRESS_EARTHQUAKE,
    GF_DISTRESS_AVALANCHE
} gf_distress_type_t;

/**
 * @brief Beacon configuration
 */
typedef struct {
    gf_beacon_type_t type;
    char beacon_id[GF_BEACON_ID_SIZE + 1];
    char vessel_name[32];
    char call_sign[8];
    uint32_t mmsi;              /* Maritime Mobile Service Identity */
    uint16_t transmit_interval_sec;
    bool auto_activate_enabled;
    float auto_activate_depth_m;    /* For EPIRB */
} gf_beacon_config_t;

/**
 * @brief GPS fix
 */
typedef struct {
    double latitude;
    double longitude;
    float altitude_m;
    float accuracy_m;
    float speed_kts;
    float heading_deg;
    uint64_t fix_time_ms;
    uint8_t satellites;
    bool valid;
} gf_beacon_gps_t;

/**
 * @brief Distress message
 */
typedef struct {
    gf_distress_type_t type;
    gf_beacon_gps_t position;
    uint8_t persons_aboard;
    char message[GF_BEACON_MSG_MAX_SIZE];
    uint64_t activation_time_ms;
    uint32_t transmission_count;
} gf_distress_msg_t;

/**
 * @brief Emergency contact
 */
typedef struct {
    char name[32];
    char phone[20];
    char email[48];
    char relationship[16];
} gf_emergency_contact_t;

/**
 * @brief Beacon telemetry
 */
typedef struct {
    gf_beacon_state_t state;
    float battery_pct;
    float temperature_c;
    float signal_strength_dbm;
    uint32_t tx_count;
    uint64_t last_tx_time_ms;
    bool acknowledgment_received;
    uint64_t acknowledgment_time_ms;
} gf_beacon_telemetry_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_beacon_ack_cb_t)(uint64_t ack_time_ms, void* user_data);

typedef void (*gf_beacon_fault_cb_t)(gf_beacon_status_t fault, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_beacon_status_t gf_beacon_init(const gf_beacon_config_t* config);
void gf_beacon_shutdown(void);

/* Registration */
gf_beacon_status_t gf_beacon_set_contacts(const gf_emergency_contact_t* contacts,
                                           uint8_t count);

/* Control */
gf_beacon_status_t gf_beacon_arm(void);
gf_beacon_status_t gf_beacon_disarm(void);
gf_beacon_status_t gf_beacon_activate(gf_distress_type_t distress);
gf_beacon_status_t gf_beacon_deactivate(void);
gf_beacon_status_t gf_beacon_test(void);
gf_beacon_state_t gf_beacon_get_state(void);

/* Position */
gf_beacon_status_t gf_beacon_update_gps(const gf_beacon_gps_t* gps);
gf_beacon_status_t gf_beacon_get_position(gf_beacon_gps_t* gps);

/* Status */
gf_beacon_status_t gf_beacon_get_telemetry(gf_beacon_telemetry_t* telemetry);
float gf_beacon_get_battery_pct(void);

/* Callbacks */
gf_beacon_status_t gf_beacon_register_ack_callback(gf_beacon_ack_cb_t cb, void* user_data);
gf_beacon_status_t gf_beacon_register_fault_callback(gf_beacon_fault_cb_t cb, void* user_data);

/* Periodic processing */
gf_beacon_status_t gf_beacon_process(void);

#endif /* GF_EMERGENCY_BEACON_H */
