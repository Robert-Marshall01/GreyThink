/**
 * @file environmental_logger.h
 * @brief Environmental Compliance Logger - Smart Ocean Monitoring
 * 
 * @details Industry Relevance:
 * Marine environmental compliance requires continuous monitoring and
 * documentation for:
 * - Ballast water management (invasive species prevention)
 * - Emissions monitoring (SOx, NOx, particulates)
 * - Oil discharge monitoring (15 ppm limit, MARPOL Annex I)
 * - Waste disposal tracking
 * - Noise monitoring (marine mammal protection)
 * - Protected area transit logging
 * 
 * Regulatory agencies (IMO, EPA, Port State Control) require tamper-evident
 * logs that can be audited. Violations result in significant fines.
 * 
 * IMO mandates Electronic Record Books (ERB) for streamlined compliance.
 * 
 * Standards: MARPOL, BWM Convention, IMO MEPC guidelines, EPA VGP
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_OCEAN_ENVIRONMENTAL_LOGGER_H
#define GF_OCEAN_ENVIRONMENTAL_LOGGER_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum log entries */
#define GF_ENV_LOG_MAX_ENTRIES          10000

/** Log retention period (days) */
#define GF_ENV_LOG_RETENTION_DAYS       1095  /* 3 years */

/** Oil content alarm threshold (ppm) */
#define GF_ENV_OIL_ALARM_PPM            15

/** SOx ECA limit (% fuel sulfur) */
#define GF_ENV_SOX_ECA_LIMIT_PCT        0.10f

/** NOx Tier III limit (g/kWh) varies by speed */
#define GF_ENV_NOX_TIER3_LIMIT          3.4f

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Environmental log entry types
 */
typedef enum {
    GF_ENV_LOG_OIL_DISCHARGE,       /**< Oily water discharge */
    GF_ENV_LOG_BALLAST,             /**< Ballast water operation */
    GF_ENV_LOG_EMISSIONS,           /**< Exhaust emissions reading */
    GF_ENV_LOG_GARBAGE,             /**< Garbage disposal */
    GF_ENV_LOG_SEWAGE,              /**< Sewage discharge */
    GF_ENV_LOG_FUEL_CHANGE,         /**< Fuel changeover */
    GF_ENV_LOG_ECA_TRANSIT,         /**< ECA entry/exit */
    GF_ENV_LOG_PROTECTED_AREA,      /**< Protected area transit */
    GF_ENV_LOG_NOISE,               /**< Noise level measurement */
    GF_ENV_LOG_INCIDENT             /**< Environmental incident */
} gf_env_log_type_t;

/**
 * @brief Ballast water operations
 */
typedef enum {
    GF_ENV_BALLAST_LOADING,         /**< Taking on ballast */
    GF_ENV_BALLAST_DISCHARGE,       /**< Discharging ballast */
    GF_ENV_BALLAST_EXCHANGE,        /**< Mid-ocean exchange */
    GF_ENV_BALLAST_TREATMENT,       /**< BWMS treatment */
    GF_ENV_BALLAST_SAMPLING         /**< Compliance sampling */
} gf_env_ballast_op_t;

/**
 * @brief Oil discharge record
 */
typedef struct {
    uint32_t timestamp;             /**< Operation time */
    double latitude_deg;            /**< Position lat */
    double longitude_deg;           /**< Position lon */
    float oil_content_ppm;          /**< Oil content reading */
    float volume_m3;                /**< Volume discharged */
    uint8_t sep_equipment;          /**< OWS equipment ID */
    bool alarm_active;              /**< 15 ppm alarm */
    bool auto_stop;                 /**< Auto-stop activated */
} gf_env_oil_discharge_t;

/**
 * @brief Ballast water record
 */
typedef struct {
    uint32_t timestamp;             /**< Operation time */
    gf_env_ballast_op_t operation;  /**< Operation type */
    uint8_t tank_id;                /**< Tank identifier */
    double latitude_deg;            /**< Position lat */
    double longitude_deg;           /**< Position lon */
    float depth_m;                  /**< Water depth */
    float volume_m3;                /**< Volume */
    float salinity_psu;             /**< Salinity */
    float temperature_c;            /**< Temperature */
    bool bwms_treated;              /**< Treatment system used */
} gf_env_ballast_record_t;

/**
 * @brief Emissions record
 */
typedef struct {
    uint32_t timestamp;             /**< Measurement time */
    float sox_pct;                  /**< SO2 content (% fuel S) */
    float nox_gkwh;                 /**< NOx (g/kWh) */
    float co2_pct;                  /**< CO2 content */
    float pm_mgm3;                  /**< Particulate matter */
    float opacity_pct;              /**< Exhaust opacity */
    uint8_t engine_id;              /**< Engine identifier */
    float engine_load_pct;          /**< Engine load */
    bool eca_zone;                  /**< In emission control area */
    bool scrubber_active;           /**< Scrubber operating */
} gf_env_emissions_t;

/**
 * @brief Protected area record
 */
typedef struct {
    uint32_t entry_time;            /**< Entry timestamp */
    uint32_t exit_time;             /**< Exit timestamp */
    char area_name[32];             /**< Area name/ID */
    double entry_lat;               /**< Entry position lat */
    double entry_lon;               /**< Entry position lon */
    double exit_lat;                /**< Exit position lat */
    double exit_lon;                /**< Exit position lon */
    float avg_speed_kts;            /**< Average speed in area */
    bool speed_compliant;           /**< Speed limit compliance */
    bool noise_compliant;           /**< Noise limit compliance */
} gf_env_protected_area_t;

/**
 * @brief Log entry structure
 */
typedef struct {
    uint64_t sequence_num;          /**< Entry sequence */
    uint32_t timestamp;             /**< Entry timestamp */
    gf_env_log_type_t type;         /**< Entry type */
    char vessel_id[16];             /**< Vessel identifier */
    char officer[32];               /**< Recording officer */
    uint8_t data[256];              /**< Type-specific data */
    uint8_t signature[64];          /**< Digital signature */
    bool signed_valid;              /**< Signature verified */
} gf_env_log_entry_t;

/**
 * @brief Logger status
 */
typedef struct {
    uint64_t last_sequence;         /**< Last sequence number */
    uint64_t entries_total;         /**< Total entries */
    float storage_used_pct;         /**< Storage usage */
    bool chain_valid;               /**< Integrity check */
    uint32_t last_audit_time;       /**< Last audit timestamp */
    uint8_t active_violations;      /**< Current violations */
} gf_env_log_status_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize environmental logger
 * @return 0 on success, negative error code on failure
 */
int gf_env_log_init(void);

/**
 * @brief Log oil discharge event
 * @param record Discharge record
 * @param officer Recording officer
 * @return 0 on success
 */
int gf_env_log_oil_discharge(const gf_env_oil_discharge_t* record,
                             const char* officer);

/**
 * @brief Log ballast water operation
 * @param record Ballast record
 * @param officer Recording officer
 * @return 0 on success
 */
int gf_env_log_ballast(const gf_env_ballast_record_t* record,
                       const char* officer);

/**
 * @brief Log emissions measurement
 * @param record Emissions record
 * @return 0 on success
 */
int gf_env_log_emissions(const gf_env_emissions_t* record);

/**
 * @brief Log protected area transit
 * @param record Protected area record
 * @param officer Recording officer
 * @return 0 on success
 */
int gf_env_log_protected_area(const gf_env_protected_area_t* record,
                              const char* officer);

/**
 * @brief Query log entries
 * @param type Entry type (or -1 for all)
 * @param since Start timestamp
 * @param entries Output entry array
 * @param max_entries Maximum to return
 * @return Number of entries found
 */
int gf_env_log_query(int type,
                     uint32_t since,
                     gf_env_log_entry_t* entries,
                     uint16_t max_entries);

/**
 * @brief Verify log integrity
 * @param start_seq Start sequence
 * @param end_seq End sequence
 * @return 0 if valid, -1 if tampered
 */
int gf_env_log_verify(uint64_t start_seq, uint64_t end_seq);

/**
 * @brief Export log for Port State Control
 * @param type Entry type (or -1 for all)
 * @param since Start timestamp
 * @param until End timestamp
 * @param format Export format (0=JSON, 1=IMO ERB)
 * @param output Output buffer
 * @param output_len Output length
 * @return 0 on success
 */
int gf_env_log_export(int type,
                      uint32_t since,
                      uint32_t until,
                      uint8_t format,
                      uint8_t* output,
                      uint32_t* output_len);

/**
 * @brief Get logger status
 * @param status Output status
 * @return 0 on success
 */
int gf_env_log_get_status(gf_env_log_status_t* status);

/**
 * @brief Shutdown environmental logger
 * @return 0 on success
 */
int gf_env_log_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_OCEAN_ENVIRONMENTAL_LOGGER_H */
