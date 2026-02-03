/**
 * @file smart_grid.h
 * @brief Smart Grid Telemetry and Control Interface
 * 
 * INDUSTRY RELEVANCE:
 * Modern power grids require distributed intelligence for demand response,
 * renewable integration, and grid stability. This module demonstrates:
 * - Real-time power quality monitoring (voltage, frequency, harmonics)
 * - Bidirectional metering for net metering and V2G
 * - Demand response signaling (OpenADR, IEEE 2030.5)
 * - Grid-tie inverter control interface
 * 
 * Applications: Smart meters, home energy management, EV chargers,
 *               distributed generation, microgrid controllers
 * Standards: IEEE 2030.5 (SEP 2.0), OpenADR 2.0, IEC 61850
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_SMART_GRID_H
#define GF_SMART_GRID_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Smart Grid Types
 ******************************************************************************/

/** Grid connection state */
typedef enum {
    GF_GRID_DISCONNECTED,
    GF_GRID_CONNECTING,       /**< Synchronizing */
    GF_GRID_CONNECTED,        /**< Normal operation */
    GF_GRID_ISLANDED,         /**< Operating in island mode */
    GF_GRID_FAULT             /**< Grid fault detected */
} gf_grid_state_t;

/** Power flow direction */
typedef enum {
    GF_POWER_IMPORT,          /**< Drawing from grid */
    GF_POWER_EXPORT,          /**< Feeding to grid */
    GF_POWER_BALANCED         /**< Net zero */
} gf_power_direction_t;

/** Demand response events */
typedef enum {
    GF_DR_NONE,
    GF_DR_MODERATE,           /**< Reduce non-critical loads */
    GF_DR_HIGH,               /**< Significant reduction needed */
    GF_DR_CRITICAL,           /**< Emergency load shed */
    GF_DR_NEGATIVE_PRICE      /**< Excess generation, increase load */
} gf_demand_response_t;

/** Power quality event types */
typedef enum {
    GF_PQ_NORMAL,
    GF_PQ_SAG,                /**< Voltage sag */
    GF_PQ_SWELL,              /**< Voltage swell */
    GF_PQ_FLICKER,            /**< Voltage flicker */
    GF_PQ_HARMONIC,           /**< THD exceeded */
    GF_PQ_FREQUENCY,          /**< Frequency deviation */
    GF_PQ_OUTAGE              /**< Complete outage */
} gf_power_quality_event_t;

/*******************************************************************************
 * Smart Grid Configuration
 ******************************************************************************/

/** Grid connection configuration */
typedef struct {
    uint16_t nominal_voltage_v;   /**< Nominal grid voltage */
    uint16_t nominal_freq_hz;     /**< 50 or 60 Hz */
    uint8_t phase_count;          /**< 1 or 3 phase */
    uint16_t max_export_w;        /**< Maximum export power */
    uint16_t max_import_w;        /**< Maximum import power */
    bool enable_island_mode;      /**< Allow islanding */
    bool enable_bidirectional;    /**< V2G / net metering */
} gf_grid_connection_config_t;

/** Power quality thresholds */
typedef struct {
    uint8_t voltage_sag_pct;      /**< Under-voltage threshold */
    uint8_t voltage_swell_pct;    /**< Over-voltage threshold */
    uint16_t freq_deviation_mhz;  /**< Frequency deviation limit */
    uint8_t thd_max_pct;          /**< Max total harmonic distortion */
    uint16_t flicker_threshold;   /**< Flicker Pst limit */
} gf_pq_thresholds_t;

/** Smart grid configuration */
typedef struct {
    gf_grid_connection_config_t connection;
    gf_pq_thresholds_t pq_thresholds;
    uint32_t meter_interval_ms;   /**< Metering sample rate */
    uint32_t telemetry_interval_s;/**< Telemetry upload rate */
    bool enable_demand_response;
    bool enable_pq_monitoring;
    char utility_endpoint[64];    /**< Utility SCADA endpoint */
    char meter_id[32];            /**< Smart meter ID */
} gf_smart_grid_config_t;

/** Real-time grid measurement */
typedef struct {
    /* Voltage */
    uint16_t voltage_v[3];        /**< Per-phase voltage (V) */
    uint16_t voltage_thd_pct[3];  /**< Voltage THD per phase */
    
    /* Current */
    int16_t current_a[3];         /**< Per-phase current (0.1A) - signed for direction */
    uint16_t current_thd_pct[3];  /**< Current THD per phase */
    
    /* Power */
    int32_t active_power_w;       /**< Real power (signed) */
    int32_t reactive_power_var;   /**< Reactive power */
    uint32_t apparent_power_va;   /**< Apparent power */
    uint8_t power_factor;         /**< Power factor × 100 */
    
    /* Frequency */
    uint32_t frequency_mhz;       /**< Grid frequency (mHz) */
    
    /* Energy */
    uint64_t import_energy_wh;    /**< Total imported */
    uint64_t export_energy_wh;    /**< Total exported */
    
    /* Status */
    gf_grid_state_t state;
    gf_power_direction_t direction;
    gf_demand_response_t dr_event;
    gf_power_quality_event_t pq_event;
} gf_grid_measurement_t;

/*******************************************************************************
 * Smart Grid Statistics
 ******************************************************************************/

typedef struct {
    uint64_t total_import_wh;
    uint64_t total_export_wh;
    uint32_t pq_events_count;
    uint32_t dr_events_count;
    uint32_t outage_count;
    uint32_t outage_duration_s;
    uint32_t island_transitions;
    float avg_power_factor;
    float avg_voltage_v;
    float avg_frequency_hz;
    int32_t peak_import_w;
    int32_t peak_export_w;
} gf_smart_grid_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize smart grid interface
 * @param config Grid configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_init(const gf_smart_grid_config_t *config);

/**
 * @brief Connect to grid
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_connect(void);

/**
 * @brief Disconnect from grid
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_disconnect(void);

/**
 * @brief Get current grid measurement
 * @param measurement Output measurement
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_measure(gf_grid_measurement_t *measurement);

/**
 * @brief Process grid telemetry (scheduler hook)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_process(void);

/**
 * @brief Set power export setpoint
 * @param power_w Export power (negative for import limit)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_set_export(int32_t power_w);

/**
 * @brief Get grid state
 * @return Current state
 */
gf_grid_state_t gf_grid_get_state(void);

/**
 * @brief Enter island mode
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_enter_island(void);

/**
 * @brief Exit island mode (resync to grid)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_exit_island(void);

/**
 * @brief Register demand response callback
 * @param callback Called on DR events
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_register_dr_callback(
    void (*callback)(gf_demand_response_t event));

/**
 * @brief Register power quality callback
 * @param callback Called on PQ events
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_register_pq_callback(
    void (*callback)(gf_power_quality_event_t event));

/**
 * @brief Upload telemetry to utility
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_upload_telemetry(void);

/**
 * @brief Get grid statistics
 * @return Current statistics
 */
gf_smart_grid_stats_t gf_grid_get_stats(void);

/**
 * @brief Shutdown smart grid interface
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_grid_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SMART_GRID_H */
