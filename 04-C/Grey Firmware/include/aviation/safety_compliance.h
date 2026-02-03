/**
 * @file safety_compliance.h
 * @brief Aviation Safety Compliance Telemetry Interface
 * 
 * @details
 * This module provides safety compliance monitoring and telemetry for
 * aviation systems. It tracks regulatory compliance, safety metrics,
 * and generates reports for aviation authorities and operators.
 * 
 * INDUSTRY RELEVANCE:
 * - Airlines: Flight Operations Quality Assurance (FOQA) programs
 * - OEMs: Type certificate compliance monitoring (Boeing, Airbus)
 * - MRO: Maintenance tracking and airworthiness directives
 * - Regulators: FAA ASIAS, EASA Safety Intelligence program data
 * - Insurance: Risk assessment and claims investigation
 * 
 * COMPLIANCE FRAMEWORKS:
 * - SMS (Safety Management System) per ICAO Annex 19
 * - FOQA/FDM (Flight Data Monitoring) programs
 * - ASAP (Aviation Safety Action Program)
 * - LOSA (Line Operations Safety Audit)
 * - 14 CFR Part 5: Safety Management Systems
 * 
 * SAFETY METRICS:
 * - Exceedance monitoring (hard landings, overspeeds)
 * - Approach stability criteria
 * - TCAS/GPWS activation logging
 * - Engine limit monitoring
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_SAFETY_COMPLIANCE_H
#define GF_SAFETY_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum exceedance events per flight */
#define GF_SAFETY_MAX_EXCEEDANCES       32

/** Maximum warning events per flight */
#define GF_SAFETY_MAX_WARNINGS          64

/** FOQA parameter count */
#define GF_FOQA_PARAM_COUNT             50

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Safety event severity
 */
typedef enum {
    GF_SAFETY_SEV_INFO,           /**< Informational */
    GF_SAFETY_SEV_CAUTION,        /**< Caution level */
    GF_SAFETY_SEV_WARNING,        /**< Warning level */
    GF_SAFETY_SEV_CRITICAL        /**< Critical/emergency */
} gf_safety_severity_t;

/**
 * @brief Exceedance types
 */
typedef enum {
    GF_EXCEED_OVERSPEED,          /**< Vmo/Mmo exceeded */
    GF_EXCEED_UNDERSPEED,         /**< Below Vs/stall */
    GF_EXCEED_OVERWEIGHT,         /**< Above MTOW */
    GF_EXCEED_HARD_LANDING,       /**< Landing g-force limit */
    GF_EXCEED_BANK_ANGLE,         /**< Bank angle limit */
    GF_EXCEED_PITCH_ANGLE,        /**< Pitch angle limit */
    GF_EXCEED_FLAP_SPEED,         /**< Flap extension speed */
    GF_EXCEED_GEAR_SPEED,         /**< Gear extension speed */
    GF_EXCEED_ENGINE_TEMP,        /**< EGT/ITT limit */
    GF_EXCEED_ENGINE_N1,          /**< N1 redline */
    GF_EXCEED_ENGINE_TORQUE,      /**< Torque limit */
    GF_EXCEED_G_LOAD,             /**< Load factor limit */
    GF_EXCEED_ALTITUDE,           /**< Altitude deviation */
    GF_EXCEED_UNSTABLE_APPROACH,  /**< Unstable approach criteria */
    GF_EXCEED_TAILSTRIKE          /**< Tail strike risk */
} gf_exceedance_type_t;

/**
 * @brief Safety system alert types
 */
typedef enum {
    GF_ALERT_GPWS,                /**< Ground proximity warning */
    GF_ALERT_TCAS_TA,             /**< TCAS traffic advisory */
    GF_ALERT_TCAS_RA,             /**< TCAS resolution advisory */
    GF_ALERT_WINDSHEAR,           /**< Windshear warning */
    GF_ALERT_STALL,               /**< Stall warning */
    GF_ALERT_FIRE,                /**< Fire detection */
    GF_ALERT_CABIN_ALT,           /**< Cabin altitude warning */
    GF_ALERT_CONFIG,              /**< Configuration warning */
    GF_ALERT_AUTOPILOT_DISC       /**< Autopilot disconnect */
} gf_alert_type_t;

/**
 * @brief Approach stability criteria
 */
typedef struct {
    float airspeed_deviation_kts; /**< Airspeed deviation from target */
    float glideslope_deviation_dots; /**< Glideslope deviation */
    float localizer_deviation_dots;  /**< Localizer deviation */
    float descent_rate_fpm;       /**< Descent rate */
    float bank_angle_deg;         /**< Bank angle */
    float sink_rate_at_50ft_fpm;  /**< Sink rate at 50ft */
    bool configured;              /**< Landing config complete */
    bool stable;                  /**< Approach is stable */
    uint16_t stable_altitude_ft;  /**< Altitude when stabilized */
} gf_approach_stability_t;

/**
 * @brief Exceedance event record
 */
typedef struct {
    gf_exceedance_type_t type;    /**< Exceedance type */
    gf_safety_severity_t severity; /**< Severity level */
    float value;                  /**< Recorded value */
    float limit;                  /**< Limit exceeded */
    uint32_t duration_ms;         /**< Duration of exceedance */
    uint32_t timestamp_ms;        /**< Event timestamp */
    float altitude_ft;            /**< Altitude at event */
    float airspeed_kts;           /**< Airspeed at event */
    char phase[16];               /**< Flight phase */
} gf_exceedance_event_t;

/**
 * @brief Alert event record
 */
typedef struct {
    gf_alert_type_t type;         /**< Alert type */
    gf_safety_severity_t severity; /**< Severity */
    uint32_t timestamp_ms;        /**< Event timestamp */
    uint32_t duration_ms;         /**< Alert duration */
    float altitude_ft;            /**< Altitude */
    char details[64];             /**< Additional details */
} gf_alert_event_t;

/**
 * @brief Flight safety summary
 */
typedef struct {
    uint32_t flight_number;       /**< Flight identifier */
    uint32_t departure_time;      /**< Departure UTC */
    uint32_t arrival_time;        /**< Arrival UTC */
    uint16_t exceedance_count;    /**< Number of exceedances */
    uint16_t warning_count;       /**< Number of warnings */
    uint16_t caution_count;       /**< Number of cautions */
    bool unstable_approach;       /**< Unstable approach detected */
    bool hard_landing;            /**< Hard landing detected */
    float max_g_load;             /**< Maximum g-load */
    float landing_rate_fpm;       /**< Landing vertical speed */
    uint8_t safety_score;         /**< Overall safety score (0-100) */
} gf_flight_safety_summary_t;

/**
 * @brief Safety configuration
 */
typedef struct {
    float overspeed_margin_kts;   /**< Overspeed warning margin */
    float bank_limit_deg;         /**< Bank angle limit */
    float pitch_up_limit_deg;     /**< Pitch up limit */
    float pitch_down_limit_deg;   /**< Pitch down limit */
    float hard_landing_g;         /**< Hard landing threshold */
    bool enable_foqa;             /**< Enable FOQA logging */
    bool enable_realtime;         /**< Enable real-time telemetry */
} gf_safety_config_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize safety compliance system
 * @param config Safety configuration
 * @return 0 on success
 */
int gf_safety_init(const gf_safety_config_t* config);

/**
 * @brief Shutdown safety compliance system
 */
void gf_safety_shutdown(void);

/**
 * @brief Start flight monitoring
 * @param flight_number Flight identifier
 * @return 0 on success
 */
int gf_safety_start_flight(uint32_t flight_number);

/**
 * @brief End flight monitoring
 * @param summary Output flight summary
 * @return 0 on success
 */
int gf_safety_end_flight(gf_flight_safety_summary_t* summary);

/**
 * @brief Record exceedance event
 * @param event Exceedance event
 * @return 0 on success
 */
int gf_safety_record_exceedance(const gf_exceedance_event_t* event);

/**
 * @brief Record safety alert
 * @param alert Alert event
 * @return 0 on success
 */
int gf_safety_record_alert(const gf_alert_event_t* alert);

/**
 * @brief Check approach stability
 * @param stability Output stability data
 * @return 0 on success
 */
int gf_safety_check_approach(gf_approach_stability_t* stability);

/**
 * @brief Get current flight summary
 * @param summary Output summary
 * @return 0 on success
 */
int gf_safety_get_summary(gf_flight_safety_summary_t* summary);

/**
 * @brief Process safety monitoring (call periodically)
 * @param delta_ms Time since last call
 */
void gf_safety_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_SAFETY_COMPLIANCE_H */
