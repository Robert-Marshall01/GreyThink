/**
 * @file thermal_monitor.h
 * @brief Thermal Monitoring Module for Battery Systems
 * 
 * @details
 * Multi-zone thermal monitoring for battery packs and energy storage systems.
 * Provides temperature sensing, thermal gradient analysis, and cooling control
 * to prevent thermal runaway and optimize battery performance.
 * 
 * INDUSTRY RELEVANCE:
 * - EV thermal management (Tesla, BMW, Ford)
 * - Grid storage thermal control (Fluence, LG Energy)
 * - Data center UPS cooling
 * - Aerospace battery thermal management
 * - Consumer electronics thermal throttling
 * 
 * KEY FEATURES:
 * - Multi-zone temperature sensing (NTC thermistors)
 * - Thermal gradient detection for runaway prediction
 * - Active cooling control (fans, liquid cooling)
 * - Passive thermal management (phase change materials)
 * - Emergency thermal shutdown triggers
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_THERMAL_MONITOR_H
#define GF_THERMAL_MONITOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum thermal zones */
#define GF_THERMAL_MAX_ZONES        32

/** Thermal runaway rate threshold (°C/sec) */
#define GF_THERMAL_RUNAWAY_RATE     2.0f

/** Maximum safe temperature (°C) */
#define GF_THERMAL_MAX_SAFE_C       60

/** Critical temperature (°C) */
#define GF_THERMAL_CRITICAL_C       80

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Thermal status codes
 */
typedef enum {
    GF_THERMAL_OK = 0,
    GF_THERMAL_ERROR_NOT_INITIALIZED,
    GF_THERMAL_ERROR_NULL_PTR,
    GF_THERMAL_ERROR_SENSOR_FAULT,
    GF_THERMAL_ERROR_COOLING_FAULT,
    GF_THERMAL_WARN_HIGH_TEMP,
    GF_THERMAL_WARN_GRADIENT,
    GF_THERMAL_CRITICAL_RUNAWAY,
    GF_THERMAL_CRITICAL_OVER_TEMP
} gf_thermal_status_t;

/**
 * @brief Cooling modes
 */
typedef enum {
    GF_THERMAL_COOL_OFF,          /**< Cooling disabled */
    GF_THERMAL_COOL_PASSIVE,      /**< Passive cooling only */
    GF_THERMAL_COOL_LOW,          /**< Low-power cooling */
    GF_THERMAL_COOL_MEDIUM,       /**< Medium cooling */
    GF_THERMAL_COOL_HIGH,         /**< High-power cooling */
    GF_THERMAL_COOL_EMERGENCY     /**< Emergency maximum cooling */
} gf_thermal_cooling_t;

/**
 * @brief Thermal zone configuration
 */
typedef struct {
    uint8_t zone_id;              /**< Zone identifier */
    int16_t warning_temp_c10;     /**< Warning threshold (0.1°C) */
    int16_t critical_temp_c10;    /**< Critical threshold (0.1°C) */
    float thermal_mass_j_c;       /**< Thermal mass (J/°C) */
    bool is_cell_zone;            /**< True if monitoring cells */
} gf_thermal_zone_config_t;

/**
 * @brief Thermal zone reading
 */
typedef struct {
    int16_t temperature_c10;      /**< Current temperature (0.1°C) */
    int16_t temp_rate_c10_s;      /**< Rate of change (0.1°C/sec) */
    int16_t max_temp_c10;         /**< Maximum recorded */
    bool warning_active;          /**< Warning threshold exceeded */
    bool critical_active;         /**< Critical threshold exceeded */
    bool runaway_detected;        /**< Thermal runaway detected */
} gf_thermal_reading_t;

/**
 * @brief Thermal event callback
 */
typedef void (*gf_thermal_event_cb_t)(gf_thermal_status_t status, 
                                       uint8_t zone_id, int16_t temp_c10,
                                       void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize thermal monitoring
 * @return Status code
 */
gf_thermal_status_t gf_thermal_init(void);

/**
 * @brief Shutdown thermal monitoring
 */
void gf_thermal_shutdown(void);

/**
 * @brief Configure thermal zone
 * @param zone Zone index
 * @param config Zone configuration
 * @return Status code
 */
gf_thermal_status_t gf_thermal_configure_zone(uint8_t zone,
                                               const gf_thermal_zone_config_t* config);

/**
 * @brief Read thermal zone
 * @param zone Zone index
 * @param reading Output reading
 * @return Status code
 */
gf_thermal_status_t gf_thermal_read_zone(uint8_t zone,
                                          gf_thermal_reading_t* reading);

/**
 * @brief Set cooling mode
 * @param mode Cooling mode
 * @return Status code
 */
gf_thermal_status_t gf_thermal_set_cooling(gf_thermal_cooling_t mode);

/**
 * @brief Get current cooling mode
 * @return Current cooling mode
 */
gf_thermal_cooling_t gf_thermal_get_cooling(void);

/**
 * @brief Register thermal event callback
 * @param callback Event callback
 * @param user_data User context
 * @return Status code
 */
gf_thermal_status_t gf_thermal_register_callback(gf_thermal_event_cb_t callback,
                                                  void* user_data);

/**
 * @brief Process thermal monitoring (call periodically)
 * @return Status code
 */
gf_thermal_status_t gf_thermal_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_THERMAL_MONITOR_H */
