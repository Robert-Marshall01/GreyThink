/**
 * @file vehicle_to_grid.h
 * @brief Vehicle-to-Grid (V2G) Communication Module Stub
 * 
 * INDUSTRY RELEVANCE:
 * V2G technology enables electric vehicles to serve as distributed energy
 * storage, feeding power back to the grid during peak demand. This market
 * is expected to reach $17B by 2027. Firmware engineers must implement
 * bidirectional power flow control, grid frequency regulation, and compliance
 * with utility communication protocols (IEEE 2030.5, OpenADR).
 * 
 * Key challenges:
 * - Real-time grid frequency monitoring and response
 * - Battery degradation optimization during V2G cycles
 * - Utility communication protocol compliance
 * - Dynamic pricing and demand response
 * - Seamless coordination with vehicle BMS
 */

#ifndef GF_VEHICLE_TO_GRID_H
#define GF_VEHICLE_TO_GRID_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* V2G operation status codes */
typedef enum {
    GF_V2G_OK = 0,
    GF_V2G_NOT_CONNECTED,           /* No vehicle connected */
    GF_V2G_NOT_CAPABLE,             /* Vehicle lacks V2G support */
    GF_V2G_CHARGING,                /* Vehicle is charging (not discharging) */
    GF_V2G_DISCHARGING,             /* Active V2G discharge */
    GF_V2G_SOC_LOW,                 /* Battery below minimum threshold */
    GF_V2G_GRID_OFFLINE,            /* Grid communication lost */
    GF_V2G_INVERTER_FAULT,          /* Power inverter fault */
    GF_V2G_SCHEDULE_CONFLICT,       /* Conflicts with user schedule */
    GF_V2G_RATE_UNFAVORABLE         /* Current rate not profitable */
} gf_v2g_status_t;

/* Grid service types */
typedef enum {
    GF_V2G_SERVICE_NONE,            /* No active service */
    GF_V2G_SERVICE_PEAK_SHAVE,      /* Peak demand reduction */
    GF_V2G_SERVICE_FREQUENCY_REG,   /* Grid frequency regulation */
    GF_V2G_SERVICE_SPINNING_RESERVE,/* Emergency reserve capacity */
    GF_V2G_SERVICE_DEMAND_RESPONSE, /* Utility demand response */
    GF_V2G_SERVICE_ARBITRAGE        /* Energy price arbitrage */
} gf_v2g_service_t;

/* Grid connection parameters */
typedef struct {
    uint16_t max_discharge_kw;      /* Maximum discharge power */
    uint16_t max_charge_kw;         /* Maximum charge power */
    uint8_t min_soc_percent;        /* Minimum SOC to maintain */
    uint8_t max_soc_percent;        /* Maximum SOC limit */
    float min_price_kwh;            /* Minimum price for discharge */
    bool auto_schedule;             /* Automatic scheduling enabled */
    char utility_account[32];       /* Utility account identifier */
} gf_v2g_config_t;

/* Real-time grid status */
typedef struct {
    float grid_frequency_hz;        /* Grid frequency (e.g., 60.00 Hz) */
    float voltage_v;                /* Grid voltage */
    float current_price_kwh;        /* Current energy price $/kWh */
    float carbon_intensity;         /* Grid carbon intensity g/kWh */
    bool demand_response_active;    /* Utility DR event active */
    gf_v2g_service_t active_service;/* Current service mode */
    int32_t power_flow_w;           /* +discharge, -charge */
} gf_v2g_grid_status_t;

/* V2G session statistics */
typedef struct {
    float energy_exported_kwh;      /* Total energy to grid */
    float energy_imported_kwh;      /* Total energy from grid */
    float revenue_earned;           /* Revenue from V2G services */
    float carbon_offset_kg;         /* CO2 offset from V2G */
    uint32_t service_hours;         /* Total service time */
    uint16_t frequency_events;      /* Frequency regulation events */
} gf_v2g_stats_t;

/**
 * @brief Initialize V2G subsystem
 * @param config V2G configuration parameters
 * @return Status code
 */
gf_v2g_status_t gf_v2g_init(const gf_v2g_config_t* config);

/**
 * @brief Connect to grid management system
 * @param utility_endpoint Utility server address
 * @return Status code
 */
gf_v2g_status_t gf_v2g_connect(const char* utility_endpoint);

/**
 * @brief Start V2G discharge session
 * @param service Service type to provide
 * @param power_kw Requested discharge power
 * @return Status code
 */
gf_v2g_status_t gf_v2g_start_discharge(gf_v2g_service_t service, uint16_t power_kw);

/**
 * @brief Stop V2G discharge
 * @return Status code
 */
gf_v2g_status_t gf_v2g_stop_discharge(void);

/**
 * @brief Get current grid status
 * @param status Output for grid status
 * @return Status code
 */
gf_v2g_status_t gf_v2g_get_grid_status(gf_v2g_grid_status_t* status);

/**
 * @brief Get V2G statistics
 * @param stats Output for statistics
 * @return Status code
 */
gf_v2g_status_t gf_v2g_get_stats(gf_v2g_stats_t* stats);

/**
 * @brief Respond to grid frequency deviation
 * @param target_frequency Target frequency in Hz
 * @return Status code
 */
gf_v2g_status_t gf_v2g_frequency_response(float target_frequency);

/**
 * @brief Set discharge schedule
 * @param start_hour Start hour (0-23)
 * @param end_hour End hour (0-23)
 * @param days_mask Bitmask for days of week
 * @return Status code
 */
gf_v2g_status_t gf_v2g_set_schedule(uint8_t start_hour, uint8_t end_hour, uint8_t days_mask);

/**
 * @brief Disconnect from grid and shutdown
 */
void gf_v2g_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_VEHICLE_TO_GRID_H */
