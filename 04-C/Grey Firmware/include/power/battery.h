/**
 * @file battery.h
 * @brief Battery Management System
 * 
 * WHAT: Fuel gauge integration, charging control, and battery health
 *       monitoring for rechargeable battery systems.
 * 
 * WHY: Battery management is critical for portable devices. Understanding
 *      coulomb counting, voltage-based SOC estimation, and charge profiles
 *      demonstrates power electronics firmware expertise.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Consumer: Smartphones, tablets, laptops
 *   - Medical: Portable diagnostics, infusion pumps
 *   - Industrial: Power tools, UPS systems
 *   - Automotive: EV battery packs, 12V systems
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - State of Charge (SOC) estimation
 *   - Charge profile management (CC-CV)
 *   - Battery health/degradation tracking
 *   - Temperature-compensated charging
 *   - Fuel gauge IC integration
 */

#ifndef GF_BATTERY_H
#define GF_BATTERY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_BATT_HISTORY_SAMPLES     32

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_BATT_CHEM_LIION = 0,     /* Li-Ion (3.0-4.2V) */
    GF_BATT_CHEM_LIPO,          /* Li-Polymer */
    GF_BATT_CHEM_LIFEPO4,       /* LiFePO4 (2.5-3.65V) */
    GF_BATT_CHEM_NIMH,          /* NiMH */
    GF_BATT_CHEM_LEAD_ACID      /* Lead acid */
} gf_battery_chem_t;

typedef enum {
    GF_CHARGE_STATE_NOT_CHARGING = 0,
    GF_CHARGE_STATE_TRICKLE,    /* Pre-conditioning */
    GF_CHARGE_STATE_CC,         /* Constant current */
    GF_CHARGE_STATE_CV,         /* Constant voltage */
    GF_CHARGE_STATE_COMPLETE,
    GF_CHARGE_STATE_FAULT
} gf_charge_state_t;

typedef struct {
    gf_battery_chem_t   chemistry;
    uint16_t            capacity_mah;
    uint16_t            charge_current_ma;
    uint16_t            charge_voltage_mv;
    uint8_t             cell_count;         /* Series cells */
    int8_t              temp_min_c;
    int8_t              temp_max_c;
} gf_battery_config_t;

typedef struct {
    uint16_t            voltage_mv;
    int16_t             current_ma;         /* Positive = discharge */
    int8_t              temperature_c;
    uint8_t             soc_percent;
    uint8_t             soh_percent;        /* State of Health */
    uint32_t            remaining_mah;
    uint32_t            time_to_empty_min;
    uint32_t            time_to_full_min;
    gf_charge_state_t   charge_state;
    uint32_t            cycle_count;
    bool                charger_present;
} gf_battery_status_t;

typedef void (*gf_battery_callback)(const gf_battery_status_t *status, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

int gf_battery_init(const gf_battery_config_t *config);
void gf_battery_get_status(gf_battery_status_t *status);
int gf_battery_enable_charging(bool enable);
uint8_t gf_battery_get_soc(void);
void gf_battery_set_callback(gf_battery_callback callback, void *ctx);
void gf_battery_calibrate_full(void);
void gf_battery_calibrate_empty(void);
void gf_battery_process(void);     /* Call periodically */
const void* gf_battery_get_driver(void);

#endif /* GF_BATTERY_H */
