/**
 * @file demand_response.h
 * @brief Microgrid Demand Response Module
 * 
 * INDUSTRY RELEVANCE:
 * Demand response is essential for grid stability and peak reduction. This module
 * implements load prioritization, curtailment scheduling, and price-responsive
 * control. Demonstrates understanding of utility DR programs (OpenADR), real-time
 * pricing, and coordinated load management.
 * 
 * Key applications:
 * - OpenADR 2.0 compliant demand response
 * - Time-of-use rate optimization
 * - Peak demand charge reduction
 * - Frequency regulation ancillary services
 * - Virtual power plant aggregation
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_DEMAND_RESPONSE_H
#define GF_DEMAND_RESPONSE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define DR_MAX_EVENTS               16      /**< Max concurrent DR events */
#define DR_MAX_ASSETS               64      /**< Max controllable assets */
#define DR_SCHEDULE_SLOTS           48      /**< 30-min slots per day */
#define DR_PRICE_TIERS              8       /**< Max price tiers */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief DR event type
 */
typedef enum {
    DR_EVENT_NONE = 0,
    DR_EVENT_LOAD_SHED,             /**< Emergency load shedding */
    DR_EVENT_PRICE_RESPONSE,        /**< Price signal response */
    DR_EVENT_CAPACITY_RESERVE,      /**< Capacity reservation */
    DR_EVENT_FREQUENCY_RESPONSE,    /**< Frequency regulation */
    DR_EVENT_VOLTAGE_SUPPORT        /**< Voltage support */
} dr_event_type_t;

/**
 * @brief DR signal from utility/aggregator
 */
typedef struct {
    dr_event_type_t type;
    uint32_t start_time;            /**< Event start (Unix timestamp) */
    uint32_t duration_seconds;
    float target_reduction_kw;      /**< Target load reduction */
    float price_signal;             /**< Price $/kWh or signal level */
    uint8_t priority;               /**< Event priority (0=highest) */
    bool mandatory;                 /**< Mandatory vs voluntary */
} dr_event_t;

/**
 * @brief Asset control capability
 */
typedef struct {
    uint8_t asset_id;
    char name[32];
    float max_curtailable_kw;       /**< Max reduction possible */
    float min_runtime_minutes;      /**< Minimum on-time */
    float min_offtime_minutes;      /**< Minimum off-time */
    bool currently_active;
    bool available;                 /**< Available for DR */
} dr_asset_t;

/**
 * @brief Schedule entry
 */
typedef struct {
    uint8_t slot;                   /**< 30-minute slot index */
    float target_demand_kw;         /**< Target demand limit */
    float price_threshold;          /**< Price trigger threshold */
    bool curtailment_enabled;
} dr_schedule_entry_t;

/**
 * @brief DR program statistics
 */
typedef struct {
    uint32_t events_received;
    uint32_t events_responded;
    uint32_t events_failed;
    float total_kwh_reduced;
    float total_savings;
    float average_response_time_ms;
    float current_baseline_kw;
    float current_actual_kw;
} dr_stats_t;

/**
 * @brief DR configuration
 */
typedef struct {
    bool enable_auto_response;
    bool enable_price_response;
    float min_response_kw;          /**< Minimum worthwhile response */
    float price_threshold_high;     /**< High price trigger */
    float price_threshold_critical; /**< Critical price trigger */
    uint16_t response_delay_ms;     /**< Intentional response delay */
} dr_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize demand response module
 * @param config DR configuration
 * @return 0 on success, negative on error
 */
int dr_init(const dr_config_t* config);

/**
 * @brief Shutdown DR module
 * @return 0 on success, negative on error
 */
int dr_shutdown(void);

/**
 * @brief Register controllable asset
 * @param asset Asset capabilities
 * @return Asset ID on success, negative on error
 */
int dr_register_asset(const dr_asset_t* asset);

/**
 * @brief Receive DR event signal
 * @param event DR event from utility/aggregator
 * @return 0 on success, negative on error
 */
int dr_receive_event(const dr_event_t* event);

/**
 * @brief Process active DR events
 * @return Current reduction (kW)
 */
float dr_process(void);

/**
 * @brief Set demand schedule
 * @param schedule Schedule entries
 * @param count Number of entries
 * @return 0 on success, negative on error
 */
int dr_set_schedule(const dr_schedule_entry_t* schedule, uint8_t count);

/**
 * @brief Get current DR statistics
 * @param stats Output statistics
 * @return 0 on success, negative on error
 */
int dr_get_stats(dr_stats_t* stats);

/**
 * @brief Opt out of current DR event
 * @param event_id Event to opt out of
 * @return 0 on success, negative on error
 */
int dr_opt_out(uint8_t event_id);

/**
 * @brief Calculate baseline demand
 * @return Baseline demand (kW)
 */
float dr_calculate_baseline(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_DEMAND_RESPONSE_H */
