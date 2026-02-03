/**
 * @file charge_telemetry.h
 * @brief Charge/Discharge Telemetry Collector for Energy Storage Systems
 * 
 * @details
 * Comprehensive telemetry collection for battery charging and discharging
 * operations. Tracks energy flow, efficiency metrics, cycle statistics,
 * and provides data for state-of-health (SOH) estimation.
 * 
 * INDUSTRY RELEVANCE:
 * - EV fleet charging analytics (ChargePoint, EVgo)
 * - Grid storage performance monitoring
 * - Battery warranty and lifecycle tracking
 * - Energy trading and billing systems
 * - Predictive maintenance systems
 * 
 * KEY FEATURES:
 * - Real-time energy flow metering (Wh, Ah)
 * - Coulomb counting for SOC tracking
 * - Charge/discharge efficiency calculation
 * - Cycle counting and depth-of-discharge tracking
 * - Power quality metrics (ripple, harmonics)
 * - Historical data logging
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_CHARGE_TELEMETRY_H
#define GF_CHARGE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum history entries */
#define GF_CHARGE_HISTORY_MAX       100

/** Telemetry sample rate (Hz) */
#define GF_CHARGE_SAMPLE_RATE_HZ    10

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Telemetry status codes
 */
typedef enum {
    GF_CHARGE_TEL_OK = 0,
    GF_CHARGE_TEL_ERROR_NOT_INIT,
    GF_CHARGE_TEL_ERROR_NULL_PTR,
    GF_CHARGE_TEL_ERROR_BUFFER_FULL,
    GF_CHARGE_TEL_ERROR_SENSOR,
    GF_CHARGE_TEL_WARN_DATA_STALE
} gf_charge_tel_status_t;

/**
 * @brief Charge session state
 */
typedef enum {
    GF_CHARGE_SESSION_IDLE,
    GF_CHARGE_SESSION_PRECHARGE,
    GF_CHARGE_SESSION_CC,         /**< Constant current phase */
    GF_CHARGE_SESSION_CV,         /**< Constant voltage phase */
    GF_CHARGE_SESSION_TOPOFF,     /**< Top-off phase */
    GF_CHARGE_SESSION_COMPLETE,
    GF_CHARGE_SESSION_FAULT
} gf_charge_session_state_t;

/**
 * @brief Real-time telemetry snapshot
 */
typedef struct {
    uint32_t timestamp_ms;        /**< Timestamp */
    int32_t current_ma;           /**< Instantaneous current (mA) */
    uint32_t voltage_mv;          /**< Instantaneous voltage (mV) */
    int32_t power_mw;             /**< Instantaneous power (mW) */
    uint32_t energy_wh;           /**< Accumulated energy (Wh) */
    uint32_t charge_mah;          /**< Accumulated charge (mAh) */
    uint8_t soc_pct;              /**< State of charge (%) */
    int16_t temperature_c10;      /**< Pack temperature (0.1°C) */
} gf_charge_snapshot_t;

/**
 * @brief Charge session summary
 */
typedef struct {
    uint32_t session_id;          /**< Unique session identifier */
    uint32_t start_time_ms;       /**< Session start time */
    uint32_t duration_s;          /**< Session duration (seconds) */
    uint8_t start_soc_pct;        /**< SOC at start */
    uint8_t end_soc_pct;          /**< SOC at end */
    uint32_t energy_in_wh;        /**< Energy delivered (Wh) */
    uint32_t energy_out_wh;       /**< Energy discharged (Wh) */
    uint16_t efficiency_permille; /**< Round-trip efficiency (0.1%) */
    int16_t max_temp_c10;         /**< Maximum temperature */
    int32_t max_current_ma;       /**< Maximum current */
    gf_charge_session_state_t end_state; /**< Final state */
} gf_charge_session_t;

/**
 * @brief Cycle statistics
 */
typedef struct {
    uint32_t full_cycles;         /**< Full equivalent cycles */
    uint32_t partial_cycles;      /**< Partial cycle count */
    uint8_t avg_dod_pct;          /**< Average depth of discharge */
    uint8_t max_dod_pct;          /**< Maximum depth of discharge */
    uint32_t total_wh_throughput; /**< Total Wh through pack */
    uint16_t avg_efficiency_pm;   /**< Average efficiency (0.1%) */
} gf_charge_cycle_stats_t;

/**
 * @brief Telemetry event callback
 */
typedef void (*gf_charge_tel_cb_t)(const gf_charge_snapshot_t* snapshot,
                                    void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize telemetry collector
 * @return Status code
 */
gf_charge_tel_status_t gf_charge_tel_init(void);

/**
 * @brief Shutdown telemetry collector
 */
void gf_charge_tel_shutdown(void);

/**
 * @brief Start new charge/discharge session
 * @param is_charging True for charge, false for discharge
 * @return Session ID or 0 on error
 */
uint32_t gf_charge_tel_start_session(bool is_charging);

/**
 * @brief End current session
 * @param summary Output session summary
 * @return Status code
 */
gf_charge_tel_status_t gf_charge_tel_end_session(gf_charge_session_t* summary);

/**
 * @brief Get current telemetry snapshot
 * @param snapshot Output snapshot
 * @return Status code
 */
gf_charge_tel_status_t gf_charge_tel_get_snapshot(gf_charge_snapshot_t* snapshot);

/**
 * @brief Get cycle statistics
 * @param stats Output statistics
 * @return Status code
 */
gf_charge_tel_status_t gf_charge_tel_get_cycle_stats(gf_charge_cycle_stats_t* stats);

/**
 * @brief Register telemetry callback
 * @param callback Telemetry callback
 * @param user_data User context
 * @return Status code
 */
gf_charge_tel_status_t gf_charge_tel_register_callback(gf_charge_tel_cb_t callback,
                                                        void* user_data);

/**
 * @brief Process telemetry (call at sample rate)
 * @return Status code
 */
gf_charge_tel_status_t gf_charge_tel_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CHARGE_TELEMETRY_H */
