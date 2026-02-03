/**
 * @file grid_controller.h
 * @brief Local Microgrid Controller Driver
 * 
 * INDUSTRY RELEVANCE:
 * Microgrids are reshaping energy infrastructure - from military bases and
 * campuses to remote communities and island nations. This controller manages
 * local generation, storage, and loads with seamless grid-connected and
 * islanded operation. Critical for energy resilience and renewable integration.
 * 
 * Key applications:
 * - Military base energy security (ESTCP programs)
 * - Campus and hospital microgrids
 * - Remote/island community power systems
 * - Industrial park energy management
 * - EV charging infrastructure integration
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_GRID_CONTROLLER_H
#define GF_GRID_CONTROLLER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define GRID_MAX_SOURCES            16      /**< Max generation sources */
#define GRID_MAX_LOADS              32      /**< Max controllable loads */
#define GRID_MAX_STORAGE            8       /**< Max storage systems */
#define GRID_BALANCE_INTERVAL_MS    100     /**< Balance loop interval */
#define GRID_VOLTAGE_NOMINAL        480.0f  /**< Nominal voltage (V) */
#define GRID_FREQUENCY_NOMINAL      60.0f   /**< Nominal frequency (Hz) */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Grid operating mode
 */
typedef enum {
    GRID_MODE_OFF = 0,
    GRID_MODE_GRID_CONNECTED,       /**< Connected to utility grid */
    GRID_MODE_ISLANDED,             /**< Standalone operation */
    GRID_MODE_TRANSITIONING,        /**< Mode transition in progress */
    GRID_MODE_FAULT                 /**< Fault condition */
} grid_mode_t;

/**
 * @brief Generation source type
 */
typedef enum {
    GRID_SOURCE_SOLAR = 0,
    GRID_SOURCE_WIND,
    GRID_SOURCE_DIESEL,
    GRID_SOURCE_NATURAL_GAS,
    GRID_SOURCE_FUEL_CELL,
    GRID_SOURCE_HYDRO,
    GRID_SOURCE_BATTERY,
    GRID_SOURCE_UTILITY
} grid_source_type_t;

/**
 * @brief Load priority for demand response
 */
typedef enum {
    GRID_LOAD_CRITICAL = 0,         /**< Life safety, never shed */
    GRID_LOAD_ESSENTIAL,            /**< Important, shed last */
    GRID_LOAD_NORMAL,               /**< Regular loads */
    GRID_LOAD_DEFERRABLE,           /**< Can be delayed */
    GRID_LOAD_CURTAILABLE           /**< Can be reduced/shed */
} grid_load_priority_t;

/**
 * @brief Generation source status
 */
typedef struct {
    uint8_t source_id;
    grid_source_type_t type;
    float power_available_kw;       /**< Available power (kW) */
    float power_output_kw;          /**< Current output (kW) */
    float efficiency;               /**< Current efficiency (0-1) */
    bool online;
    bool dispatchable;              /**< Can be dispatched on demand */
} grid_source_status_t;

/**
 * @brief Load status
 */
typedef struct {
    uint8_t load_id;
    char name[32];
    grid_load_priority_t priority;
    float power_demand_kw;          /**< Current demand (kW) */
    float power_allocated_kw;       /**< Allocated power (kW) */
    bool connected;
    bool curtailed;
} grid_load_status_t;

/**
 * @brief Grid balance state
 */
typedef struct {
    grid_mode_t mode;
    float total_generation_kw;
    float total_demand_kw;
    float total_storage_kw;         /**< Positive=charging */
    float net_import_kw;            /**< From utility grid */
    float voltage;
    float frequency;
    float power_factor;
    bool balanced;
    uint32_t balance_iterations;
} grid_balance_state_t;

/**
 * @brief Controller configuration
 */
typedef struct {
    float voltage_tolerance;        /**< Voltage deviation tolerance */
    float frequency_tolerance;      /**< Frequency deviation tolerance */
    float reserve_margin;           /**< Spinning reserve margin */
    bool enable_demand_response;
    bool enable_peak_shaving;
    bool enable_load_shifting;
    uint16_t island_detection_ms;
} grid_controller_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize grid controller
 * @param config Controller configuration
 * @return 0 on success, negative on error
 */
int grid_controller_init(const grid_controller_config_t* config);

/**
 * @brief Shutdown controller
 * @return 0 on success, negative on error
 */
int grid_controller_shutdown(void);

/**
 * @brief Register generation source
 * @param type Source type
 * @param max_power Maximum power (kW)
 * @param dispatchable Is dispatchable
 * @return Source ID on success, negative on error
 */
int grid_register_source(grid_source_type_t type, float max_power, 
                         bool dispatchable);

/**
 * @brief Register controllable load
 * @param name Load name
 * @param priority Load priority
 * @param max_power Maximum power (kW)
 * @return Load ID on success, negative on error
 */
int grid_register_load(const char* name, grid_load_priority_t priority,
                       float max_power);

/**
 * @brief Run balance loop
 * @return 0 on success, negative on error
 */
int grid_balance(void);

/**
 * @brief Get current balance state
 * @param state Output state structure
 * @return 0 on success, negative on error
 */
int grid_get_state(grid_balance_state_t* state);

/**
 * @brief Request island mode transition
 * @return 0 on success, negative on error
 */
int grid_request_island(void);

/**
 * @brief Request grid reconnection
 * @return 0 on success, negative on error
 */
int grid_request_reconnect(void);

/**
 * @brief Emergency load shed
 * @param target_reduction_kw Target reduction (kW)
 * @return Actual reduction (kW)
 */
float grid_emergency_shed(float target_reduction_kw);

#ifdef __cplusplus
}
#endif

#endif /* GF_GRID_CONTROLLER_H */
