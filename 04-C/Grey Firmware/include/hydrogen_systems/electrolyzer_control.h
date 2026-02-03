/**
 * @file electrolyzer_control.h
 * @brief Electrolyzer Control Module for Hydrogen Production
 * 
 * INDUSTRY RELEVANCE:
 * Electrolyzers split water into hydrogen and oxygen using electricity,
 * enabling green hydrogen production from renewable sources. This technology
 * is critical for energy storage, industrial decarbonization, and fuel cell
 * vehicle refueling. Expertise valued at ITM Power, Nel ASA, Siemens Energy,
 * Thyssenkrupp, and major oil companies transitioning to hydrogen.
 * 
 * This module provides control interfaces for PEM and alkaline electrolyzers
 * including power management, water flow control, and safety interlocks.
 * 
 * KEY CAPABILITIES:
 * - Power regulation (DC rectifier control)
 * - Water flow management (deionized water)
 * - Stack temperature control
 * - Hydrogen purity monitoring
 * - Pressure regulation
 * - Safety interlock management
 * - Efficiency optimization
 * 
 * ELECTROLYZER TYPES:
 * - PEM (Proton Exchange Membrane): Fast response, high purity
 * - Alkaline: Mature, lower cost, larger scale
 * - SOEC (Solid Oxide): High efficiency, high temperature
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ELECTROLYZER_CONTROL_H
#define GF_ELECTROLYZER_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define EL_MAX_STACKS          8     /**< Max electrolyzer stacks */
#define EL_POWER_MIN_KW        5     /**< Minimum operating power */
#define EL_POWER_MAX_KW        2000  /**< Maximum power (2 MW) */
#define EL_H2_PURITY_MIN       99.5f /**< Min H2 purity % */
#define EL_WATER_RESISTIVITY   18.0f /**< Min water resistivity (MΩ·cm) */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Electrolyzer type */
typedef enum {
    EL_TYPE_PEM,       /**< Proton Exchange Membrane */
    EL_TYPE_ALKALINE,  /**< Alkaline (KOH) */
    EL_TYPE_SOEC       /**< Solid Oxide (high temp) */
} el_type_t;

/** Operating mode */
typedef enum {
    EL_MODE_OFF,
    EL_MODE_STANDBY,
    EL_MODE_WARMUP,
    EL_MODE_PRODUCTION,
    EL_MODE_COOLDOWN,
    EL_MODE_MAINTENANCE,
    EL_MODE_FAULT
} el_mode_t;

/** Control setpoint */
typedef struct {
    float power_setpoint_kw;   /**< Target power consumption */
    float h2_rate_nm3h;        /**< Target H2 production rate */
    float stack_temp_c;        /**< Target stack temperature */
    float pressure_bar;        /**< Target output pressure */
} el_setpoint_t;

/** Stack status */
typedef struct {
    uint8_t stack_id;
    el_mode_t mode;
    float voltage_v;
    float current_a;
    float power_kw;
    float h2_flow_nm3h;        /**< Normal m³/hour */
    float efficiency_pct;      /**< kWh/kg H2 equivalent */
    float temperature_c;
    float h2_purity_pct;
    bool water_flow_ok;
    bool pressure_ok;
} el_stack_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize electrolyzer control
 * @param type Electrolyzer technology type
 * @param stack_count Number of stacks
 * @return 0 on success
 */
int el_init(el_type_t type, uint8_t stack_count);

/**
 * @brief Set production setpoint
 * @param stack_id Stack identifier
 * @param setpoint Target operating point
 * @return 0 on success
 */
int el_set_setpoint(uint8_t stack_id, const el_setpoint_t* setpoint);

/**
 * @brief Start hydrogen production
 * @param stack_id Stack identifier
 * @return 0 on success
 */
int el_start_production(uint8_t stack_id);

/**
 * @brief Stop production (controlled shutdown)
 * @param stack_id Stack identifier
 * @return 0 on success
 */
int el_stop_production(uint8_t stack_id);

/**
 * @brief Emergency stop all stacks
 * @return 0 on success
 */
int el_emergency_stop(void);

/**
 * @brief Get stack status
 * @param stack_id Stack identifier
 * @param status Output status structure
 * @return 0 on success
 */
int el_get_status(uint8_t stack_id, el_stack_status_t* status);

/**
 * @brief Process control loop (call periodically)
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int el_process(uint32_t delta_ms);

/**
 * @brief Shutdown electrolyzer control
 */
void el_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ELECTROLYZER_CONTROL_H */
