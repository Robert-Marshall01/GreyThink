/**
 * @file ev_charging.h
 * @brief Electric Vehicle Charging Station Controller Stub
 * 
 * INDUSTRY RELEVANCE:
 * The global EV charging infrastructure market is projected to exceed $100B
 * by 2030. Firmware engineers in this space work with high-power systems
 * (up to 350kW DC fast charging), automotive communication protocols
 * (ISO 15118, OCPP), and grid integration. This domain requires expertise
 * in power electronics control, safety interlocks, and real-time payment
 * processing integration.
 * 
 * Key challenges:
 * - High-voltage safety (up to 1000V DC)
 * - Communication with vehicle (CAN, PLC over pilot signal)
 * - Dynamic power allocation with grid constraints
 * - Billing/payment system integration
 * - Thermal management of power electronics
 */

#ifndef GF_EV_CHARGING_H
#define GF_EV_CHARGING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Charging station status codes */
typedef enum {
    GF_EVSE_OK = 0,
    GF_EVSE_NOT_CONNECTED,          /* No vehicle connected */
    GF_EVSE_VEHICLE_DETECTED,       /* Vehicle connected, not charging */
    GF_EVSE_CHARGING,               /* Active charging session */
    GF_EVSE_CHARGE_COMPLETE,        /* Charging finished */
    GF_EVSE_FAULT,                  /* Fault condition */
    GF_EVSE_GROUND_FAULT,           /* Ground fault detected */
    GF_EVSE_OVERCURRENT,            /* Overcurrent protection tripped */
    GF_EVSE_OVERTEMP,               /* Thermal protection active */
    GF_EVSE_COMM_ERROR,             /* Vehicle communication failed */
    GF_EVSE_GRID_LIMIT              /* Grid power limit reached */
} gf_evse_status_t;

/* Charging modes per IEC 61851 */
typedef enum {
    GF_CHARGE_MODE_1,               /* AC slow, no communication */
    GF_CHARGE_MODE_2,               /* AC with in-cable control */
    GF_CHARGE_MODE_3,               /* AC with EVSE control */
    GF_CHARGE_MODE_4                /* DC fast charging */
} gf_evse_mode_t;

/* Connector types */
typedef enum {
    GF_CONNECTOR_TYPE1_J1772,       /* SAE J1772 (North America) */
    GF_CONNECTOR_TYPE2_MENNEKES,    /* IEC 62196-2 (Europe) */
    GF_CONNECTOR_CCS1,              /* Combined Charging System 1 */
    GF_CONNECTOR_CCS2,              /* Combined Charging System 2 */
    GF_CONNECTOR_CHADEMO,           /* CHAdeMO (legacy Japan) */
    GF_CONNECTOR_GB_T,              /* GB/T (China) */
    GF_CONNECTOR_NACS               /* North American Charging Standard */
} gf_evse_connector_t;

/* Charging station configuration */
typedef struct {
    gf_evse_mode_t mode;            /* Charging mode */
    gf_evse_connector_t connector;  /* Connector type */
    uint32_t max_power_kw;          /* Maximum power output */
    uint16_t max_voltage_v;         /* Maximum voltage */
    uint16_t max_current_a;         /* Maximum current */
    bool enable_iso15118;           /* ISO 15118 Plug&Charge */
    bool enable_v2g;                /* Vehicle-to-Grid support */
    char station_id[32];            /* Unique station identifier */
} gf_evse_config_t;

/* Charging session data */
typedef struct {
    uint32_t session_id;            /* Unique session ID */
    uint64_t start_time;            /* Session start timestamp */
    float energy_kwh;               /* Energy delivered */
    float current_power_kw;         /* Instantaneous power */
    uint16_t voltage_v;             /* Current voltage */
    uint16_t current_a;             /* Current amperage */
    uint8_t soc_percent;            /* Vehicle state of charge */
    uint16_t time_remaining_min;    /* Estimated time to full */
    gf_evse_status_t status;        /* Current status */
    int8_t connector_temp_c;        /* Connector temperature */
} gf_evse_session_t;

/* Vehicle information (from ISO 15118) */
typedef struct {
    char vehicle_id[32];            /* Vehicle identification */
    uint8_t battery_capacity_kwh;   /* Battery capacity */
    uint8_t current_soc;            /* Current state of charge */
    uint8_t target_soc;             /* Target state of charge */
    uint32_t max_power_kw;          /* Vehicle max power acceptance */
    bool v2g_capable;               /* V2G capability */
    bool plug_and_charge;           /* Automatic payment enabled */
} gf_evse_vehicle_t;

/**
 * @brief Initialize EVSE controller
 * @param config Station configuration
 * @return Status code
 */
gf_evse_status_t gf_evse_init(const gf_evse_config_t* config);

/**
 * @brief Start charging session
 * @param session Output for session data
 * @return Status code
 */
gf_evse_status_t gf_evse_start_charge(gf_evse_session_t* session);

/**
 * @brief Stop charging session
 * @return Status code
 */
gf_evse_status_t gf_evse_stop_charge(void);

/**
 * @brief Get current session status
 * @param session Output for session data
 * @return Status code
 */
gf_evse_status_t gf_evse_get_session(gf_evse_session_t* session);

/**
 * @brief Get connected vehicle information
 * @param vehicle Output for vehicle data
 * @return Status code
 */
gf_evse_status_t gf_evse_get_vehicle(gf_evse_vehicle_t* vehicle);

/**
 * @brief Set power limit (for grid management)
 * @param max_power_kw Maximum power in kilowatts
 * @return Status code
 */
gf_evse_status_t gf_evse_set_power_limit(uint32_t max_power_kw);

/**
 * @brief Enable Vehicle-to-Grid discharge
 * @param power_kw Discharge power in kilowatts
 * @return Status code
 */
gf_evse_status_t gf_evse_enable_v2g(uint32_t power_kw);

/**
 * @brief Perform safety interlock check
 * @return Status code (OK if safe)
 */
gf_evse_status_t gf_evse_safety_check(void);

/**
 * @brief Emergency stop - immediate shutdown
 */
void gf_evse_emergency_stop(void);

/**
 * @brief Shutdown and release resources
 */
void gf_evse_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EV_CHARGING_H */
