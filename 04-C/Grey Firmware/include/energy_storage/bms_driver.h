/**
 * @file bms_driver.h
 * @brief Battery Management System (BMS) Driver for Smart Energy Storage
 * 
 * @details
 * Production-grade BMS driver for multi-cell lithium battery packs. Provides
 * cell-level monitoring, balancing control, and safety enforcement for
 * grid-scale energy storage, electric vehicles, and portable electronics.
 * 
 * INDUSTRY RELEVANCE:
 * - Electric vehicle battery packs (Tesla, Rivian, BYD)
 * - Grid-scale energy storage (Tesla Megapack, Fluence)
 * - Residential storage (Powerwall, Enphase)
 * - Portable electronics and power tools
 * - UPS and backup power systems
 * - Marine and aerospace battery systems
 * 
 * KEY CHALLENGES:
 * - Cell imbalance detection and correction
 * - State of Charge (SoC) and State of Health (SoH) estimation
 * - Thermal runaway prevention
 * - Fast charging optimization
 * - Cycle life maximization
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_BMS_DRIVER_H
#define GF_BMS_DRIVER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum cells in a pack */
#define GF_BMS_MAX_CELLS            96

/** Maximum parallel strings */
#define GF_BMS_MAX_STRINGS          8

/** Minimum cell voltage (deep discharge protection) */
#define GF_BMS_MIN_CELL_VOLTAGE_MV  2500

/** Maximum cell voltage (overcharge protection) */
#define GF_BMS_MAX_CELL_VOLTAGE_MV  4200

/** Nominal cell voltage */
#define GF_BMS_NOMINAL_VOLTAGE_MV   3700

/** Balance threshold (mV difference) */
#define GF_BMS_BALANCE_THRESHOLD_MV 20

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief BMS status codes
 */
typedef enum {
    GF_BMS_OK = 0,
    GF_BMS_ERROR_NOT_INITIALIZED,
    GF_BMS_ERROR_NULL_PTR,
    GF_BMS_ERROR_INVALID_PARAM,
    GF_BMS_ERROR_COMMUNICATION,
    GF_BMS_ERROR_CELL_FAULT,
    GF_BMS_ERROR_OVER_VOLTAGE,
    GF_BMS_ERROR_UNDER_VOLTAGE,
    GF_BMS_ERROR_OVER_CURRENT,
    GF_BMS_ERROR_OVER_TEMPERATURE,
    GF_BMS_ERROR_IMBALANCE,
    GF_BMS_WARN_LOW_SOC,
    GF_BMS_WARN_HIGH_TEMP,
    GF_BMS_WARN_DEGRADATION
} gf_bms_status_t;

/**
 * @brief Battery chemistry types
 */
typedef enum {
    GF_BMS_CHEM_LFP,              /**< Lithium Iron Phosphate (LiFePO4) */
    GF_BMS_CHEM_NMC,              /**< Nickel Manganese Cobalt */
    GF_BMS_CHEM_NCA,              /**< Nickel Cobalt Aluminum */
    GF_BMS_CHEM_LTO,              /**< Lithium Titanate */
    GF_BMS_CHEM_LCO,              /**< Lithium Cobalt Oxide */
    GF_BMS_CHEM_SOLID_STATE       /**< Solid state battery */
} gf_bms_chemistry_t;

/**
 * @brief BMS operating mode
 */
typedef enum {
    GF_BMS_MODE_STANDBY,          /**< Low power standby */
    GF_BMS_MODE_CHARGING,         /**< Active charging */
    GF_BMS_MODE_DISCHARGING,      /**< Active discharging */
    GF_BMS_MODE_BALANCING,        /**< Cell balancing active */
    GF_BMS_MODE_FAULT,            /**< Fault condition */
    GF_BMS_MODE_PRECHARGE,        /**< Precharge sequence */
    GF_BMS_MODE_STORAGE           /**< Long-term storage mode */
} gf_bms_mode_t;

/**
 * @brief Individual cell data
 */
typedef struct {
    uint16_t voltage_mv;          /**< Cell voltage in mV */
    int16_t temperature_c10;      /**< Temperature in 0.1°C units */
    uint8_t soc_pct;              /**< State of charge (0-100%) */
    uint8_t soh_pct;              /**< State of health (0-100%) */
    bool is_balancing;            /**< Balancing active on this cell */
    bool fault_detected;          /**< Cell-level fault */
} gf_bms_cell_t;

/**
 * @brief Pack-level data
 */
typedef struct {
    uint32_t pack_voltage_mv;     /**< Total pack voltage */
    int32_t pack_current_ma;      /**< Pack current (+ = charge, - = discharge) */
    int16_t max_temp_c10;         /**< Maximum cell temperature */
    int16_t min_temp_c10;         /**< Minimum cell temperature */
    uint16_t max_cell_voltage_mv; /**< Highest cell voltage */
    uint16_t min_cell_voltage_mv; /**< Lowest cell voltage */
    uint8_t soc_pct;              /**< Pack state of charge */
    uint8_t soh_pct;              /**< Pack state of health */
    uint32_t energy_wh;           /**< Available energy (Wh) */
    uint32_t capacity_ah;         /**< Remaining capacity (mAh) */
    uint32_t cycle_count;         /**< Charge cycle count */
} gf_bms_pack_t;

/**
 * @brief BMS configuration
 */
typedef struct {
    gf_bms_chemistry_t chemistry;       /**< Battery chemistry */
    uint8_t cell_count;                 /**< Number of cells in series */
    uint8_t string_count;               /**< Number of parallel strings */
    uint32_t nominal_capacity_mah;      /**< Pack capacity (mAh) */
    uint16_t max_charge_current_ma;     /**< Maximum charge current */
    uint16_t max_discharge_current_ma;  /**< Maximum discharge current */
    int16_t charge_temp_min_c10;        /**< Min charge temperature (0.1°C) */
    int16_t charge_temp_max_c10;        /**< Max charge temperature (0.1°C) */
    int16_t discharge_temp_min_c10;     /**< Min discharge temperature */
    int16_t discharge_temp_max_c10;     /**< Max discharge temperature */
    bool enable_balancing;              /**< Enable cell balancing */
    bool enable_precharge;              /**< Enable precharge sequence */
} gf_bms_config_t;

/**
 * @brief BMS event callback
 */
typedef void (*gf_bms_event_cb_t)(gf_bms_status_t status, uint8_t cell_id,
                                   void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize BMS driver
 * @param config BMS configuration
 * @return Status code
 */
gf_bms_status_t gf_bms_init(const gf_bms_config_t* config);

/**
 * @brief Shutdown BMS driver
 */
void gf_bms_shutdown(void);

/**
 * @brief Set operating mode
 * @param mode Target operating mode
 * @return Status code
 */
gf_bms_status_t gf_bms_set_mode(gf_bms_mode_t mode);

/**
 * @brief Get current operating mode
 * @return Current mode
 */
gf_bms_mode_t gf_bms_get_mode(void);

/**
 * @brief Get cell data
 * @param cell_id Cell index (0 to cell_count-1)
 * @param cell Output cell data
 * @return Status code
 */
gf_bms_status_t gf_bms_get_cell(uint8_t cell_id, gf_bms_cell_t* cell);

/**
 * @brief Get pack-level data
 * @param pack Output pack data
 * @return Status code
 */
gf_bms_status_t gf_bms_get_pack(gf_bms_pack_t* pack);

/**
 * @brief Enable/disable charging
 * @param enable True to enable charging
 * @return Status code
 */
gf_bms_status_t gf_bms_enable_charging(bool enable);

/**
 * @brief Enable/disable discharging
 * @param enable True to enable discharging
 * @return Status code
 */
gf_bms_status_t gf_bms_enable_discharging(bool enable);

/**
 * @brief Trigger cell balancing
 * @return Status code
 */
gf_bms_status_t gf_bms_start_balancing(void);

/**
 * @brief Register event callback
 * @param callback Event callback function
 * @param user_data User context
 * @return Status code
 */
gf_bms_status_t gf_bms_register_callback(gf_bms_event_cb_t callback,
                                          void* user_data);

/**
 * @brief Process BMS (call periodically)
 * @return Status code
 */
gf_bms_status_t gf_bms_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_BMS_DRIVER_H */
