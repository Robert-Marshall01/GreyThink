/**
 * @file hydrogen_telemetry.h
 * @brief Hydrogen Production Efficiency Telemetry
 * 
 * INDUSTRY RELEVANCE:
 * Hydrogen infrastructure requires comprehensive telemetry for efficiency
 * optimization, predictive maintenance, and regulatory compliance. Cloud
 * analytics platforms analyze production data to maximize green hydrogen
 * yield. Companies like H2Pro, Linde, Air Products, and renewable hydrogen
 * project developers need firmware engineers with telemetry expertise.
 * 
 * This module provides telemetry collection and reporting for hydrogen
 * production systems including efficiency metrics, operational data, and
 * compliance logging.
 * 
 * KEY CAPABILITIES:
 * - Production efficiency tracking (kWh/kg H2)
 * - Stack degradation monitoring
 * - Availability and utilization metrics
 * - Maintenance prediction data
 * - Carbon intensity tracking
 * - Grid/renewable integration data
 * - Compliance audit trails
 * 
 * STANDARDS COMPLIANCE:
 * - CertifHy (EU hydrogen certification)
 * - ISO 14064 (Greenhouse gas accounting)
 * - DOE H2A Analysis (Hydrogen cost modeling)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_HYDROGEN_TELEMETRY_H
#define GF_HYDROGEN_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define HT_MAX_METRICS         64    /**< Max telemetry metrics */
#define HT_HISTORY_HOURS       168   /**< 1 week of hourly data */
#define HT_REPORT_INTERVAL_S   60    /**< Default reporting interval */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Efficiency metrics */
typedef struct {
    float specific_energy_kwh_kg;  /**< Energy per kg H2 */
    float stack_efficiency_pct;    /**< Stack electrical efficiency */
    float system_efficiency_pct;   /**< System-level efficiency */
    float availability_pct;        /**< System availability */
    float capacity_factor_pct;     /**< Utilization vs rated */
} ht_efficiency_t;

/** Production summary */
typedef struct {
    float h2_produced_kg;          /**< Total hydrogen produced */
    float energy_consumed_kwh;     /**< Total energy consumed */
    float water_consumed_l;        /**< Total water used */
    float o2_produced_kg;          /**< Oxygen byproduct */
    uint32_t operating_hours;      /**< Run time hours */
    uint32_t start_cycles;         /**< Number of starts */
} ht_production_summary_t;

/** Carbon tracking */
typedef struct {
    float carbon_intensity_g_kg;   /**< g CO2e per kg H2 */
    float renewable_fraction_pct;  /**< % renewable electricity */
    bool green_certified;          /**< CertifHy green eligible */
} ht_carbon_t;

/** Telemetry packet */
typedef struct {
    uint32_t timestamp;
    uint32_t sequence;
    ht_efficiency_t efficiency;
    ht_production_summary_t production;
    ht_carbon_t carbon;
    uint8_t fault_count;
    uint8_t warning_count;
} ht_packet_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize hydrogen telemetry
 * @param system_id Unique system identifier
 * @return 0 on success
 */
int ht_init(uint32_t system_id);

/**
 * @brief Update efficiency metrics
 * @param efficiency Current efficiency data
 * @return 0 on success
 */
int ht_update_efficiency(const ht_efficiency_t* efficiency);

/**
 * @brief Log production data point
 * @param h2_kg Hydrogen produced in interval
 * @param energy_kwh Energy consumed in interval
 * @return 0 on success
 */
int ht_log_production(float h2_kg, float energy_kwh);

/**
 * @brief Generate telemetry packet
 * @param packet Output packet
 * @return 0 on success
 */
int ht_generate_packet(ht_packet_t* packet);

/**
 * @brief Get production summary
 * @param summary Output summary
 * @return 0 on success
 */
int ht_get_summary(ht_production_summary_t* summary);

/**
 * @brief Set carbon tracking parameters
 * @param renewable_pct Renewable electricity fraction
 * @return 0 on success
 */
int ht_set_carbon_source(float renewable_pct);

/**
 * @brief Shutdown telemetry
 */
void ht_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_HYDROGEN_TELEMETRY_H */
