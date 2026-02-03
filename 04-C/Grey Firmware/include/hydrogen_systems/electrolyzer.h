/**
 * @file electrolyzer.h
 * @brief Hydrogen Electrolyzer Control Interface
 * 
 * INDUSTRY RELEVANCE:
 * Electrolyzers are the key technology for green hydrogen production,
 * splitting water into hydrogen and oxygen using renewable electricity.
 * This is essential for energy storage, industrial decarbonization, and
 * synthetic fuel production. Major players include NEL, ITM Power, 
 * Siemens Energy, Plug Power, Cummins, and Thyssenkrupp.
 * 
 * This module provides control interfaces for PEM and alkaline electrolyzers
 * including power modulation, production rate control, and safety interlocks.
 * 
 * KEY CAPABILITIES:
 * - Power input control (DC bus management)
 * - Production rate modulation
 * - Stack voltage/current regulation
 * - Temperature management
 * - Pressure control (hydrogen side)
 * - Purity monitoring
 * - Load following for renewable integration
 * 
 * STANDARDS COMPLIANCE:
 * - IEC 62282-3 (Fuel cell technologies)
 * - ISO 22734 (Hydrogen generators)
 * - NFPA 2 (Hydrogen technologies code)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ELECTROLYZER_H
#define GF_ELECTROLYZER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define EL_MAX_STACKS          8     /**< Max electrolyzer stacks */
#define EL_MIN_POWER_PCT       10    /**< Minimum operating power */
#define EL_MAX_PRESSURE_BAR    35    /**< Max hydrogen pressure */
#define EL_TARGET_PURITY_PCT   99.999f /**< 5-nines purity target */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Electrolyzer type */
typedef enum {
    EL_TYPE_PEM,           /**< Proton Exchange Membrane */
    EL_TYPE_ALKALINE,      /**< Alkaline electrolyzer */
    EL_TYPE_SOEC           /**< Solid Oxide Electrolyzer Cell */
} el_type_t;

/** Operating mode */
typedef enum {
    EL_MODE_OFF,
    EL_MODE_STANDBY,
    EL_MODE_STARTUP,
    EL_MODE_RUNNING,
    EL_MODE_LOAD_FOLLOW,   /**< Follow renewable input */
    EL_MODE_SHUTDOWN,
    EL_MODE_FAULT
} el_mode_t;

/** Production status */
typedef struct {
    float h2_rate_kg_h;        /**< Hydrogen production rate */
    float o2_rate_kg_h;        /**< Oxygen production rate */
    float h2_purity_pct;       /**< Hydrogen purity */
    float h2_pressure_bar;     /**< Output pressure */
    float efficiency_pct;      /**< Electrical efficiency */
    float power_input_kw;      /**< Electrical power consumed */
    float water_rate_l_h;      /**< Water consumption */
} el_production_t;

/** Stack configuration */
typedef struct {
    uint8_t stack_id;
    el_type_t type;
    float rated_power_kw;
    float rated_h2_rate_kg_h;
    uint16_t cell_count;
    float cell_area_cm2;
} el_stack_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize electrolyzer subsystem
 * @param config Stack configuration
 * @return 0 on success
 */
int el_init(const el_stack_config_t* config);

/**
 * @brief Set operating mode
 * @param mode Target mode
 * @return 0 on success
 */
int el_set_mode(el_mode_t mode);

/**
 * @brief Set power setpoint (for load following)
 * @param power_pct Power percentage (0-100)
 * @return 0 on success
 */
int el_set_power(float power_pct);

/**
 * @brief Set production rate setpoint
 * @param h2_rate_kg_h Target hydrogen rate
 * @return 0 on success
 */
int el_set_production_rate(float h2_rate_kg_h);

/**
 * @brief Get current production status
 * @param status Output status structure
 * @return 0 on success
 */
int el_get_production(el_production_t* status);

/**
 * @brief Get current operating mode
 * @return Current mode
 */
el_mode_t el_get_mode(void);

/**
 * @brief Emergency stop
 * @return 0 on success
 */
int el_emergency_stop(void);

/**
 * @brief Shutdown electrolyzer
 */
void el_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ELECTROLYZER_H */
