/**
 * @file energy_optimization.h
 * @brief Energy Optimization Module for Smart Hospitality Systems
 * 
 * INDUSTRY RELEVANCE:
 * Hotels spend $2,196 per room annually on energy. Optimization reduces costs
 * 20-30% while maintaining guest comfort. This module enables:
 * - Demand response participation for utility incentives
 * - Peak shaving through load scheduling
 * - Renewable integration (solar, battery)
 * - LEED/Green Globe certification support
 * 
 * Target applications: Hotel chains, resorts, convention centers, cruise ships,
 * hospitals, universities, commercial real estate.
 * 
 * Standards: ASHRAE 90.1 (energy efficiency), LEED (green building),
 *            ENERGY STAR (benchmarking), ISO 50001 (energy management),
 *            OpenADR (demand response)
 */

#ifndef GF_ENERGY_OPTIMIZATION_H
#define GF_ENERGY_OPTIMIZATION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Energy Optimization Types                                                  */
/*===========================================================================*/

typedef enum {
    LOAD_HVAC,              /* Heating/ventilation/AC */
    LOAD_LIGHTING,          /* Interior/exterior lighting */
    LOAD_KITCHEN,           /* Kitchen equipment */
    LOAD_LAUNDRY,           /* Laundry equipment */
    LOAD_POOL,              /* Pool/spa heating */
    LOAD_EV_CHARGING,       /* Electric vehicle charging */
    LOAD_MISC,              /* Miscellaneous loads */
    LOAD_COUNT
} load_type_t;

typedef enum {
    RATE_OFF_PEAK,          /* Lowest rate period */
    RATE_SHOULDER,          /* Mid-rate period */
    RATE_ON_PEAK,           /* Highest rate period */
    RATE_DEMAND_RESPONSE    /* DR event active */
} rate_period_t;

typedef enum {
    OPT_MODE_COMFORT,       /* Guest comfort priority */
    OPT_MODE_BALANCED,      /* Balance comfort/efficiency */
    OPT_MODE_EFFICIENCY,    /* Energy efficiency priority */
    OPT_MODE_DEMAND_RESPONSE /* Active demand response */
} optimization_mode_t;

typedef struct {
    load_type_t load;
    float current_kw;       /* Current power draw */
    float max_kw;           /* Maximum rated power */
    float sheddable_kw;     /* Power that can be shed */
    bool active;            /* Load currently active */
    bool curtailable;       /* Can be curtailed */
    uint8_t priority;       /* 0=critical, higher=flexible */
} load_status_t;

typedef struct {
    float total_kw;             /* Total current power */
    float peak_kw_today;        /* Today's peak demand */
    float kwh_today;            /* Today's energy usage */
    float kwh_month;            /* Month-to-date usage */
    float cost_today;           /* Today's cost ($) */
    float cost_month;           /* Month-to-date cost ($) */
    rate_period_t current_rate;
    optimization_mode_t mode;
    float solar_generation_kw;  /* Solar production */
    float battery_soc_pct;      /* Battery state of charge */
} energy_status_t;

typedef struct {
    uint32_t start_time;        /* DR event start */
    uint32_t duration_s;        /* Event duration */
    float reduction_kw;         /* Requested reduction */
    float incentive_per_kw;     /* Incentive per kW reduced */
    bool mandatory;             /* Mandatory event */
} dr_event_t;

typedef struct {
    float peak_limit_kw;        /* Peak demand limit */
    float baseline_kwh_day;     /* Baseline daily usage */
    bool enable_dr;             /* Enable demand response */
    bool enable_solar;          /* Enable solar integration */
    bool enable_battery;        /* Enable battery storage */
    float comfort_band_c;       /* Allowed temp deviation */
} optimization_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int energy_optimization_init(const optimization_config_t* config);
int energy_optimization_shutdown(void);

int energy_set_mode(optimization_mode_t mode);
int energy_get_status(energy_status_t* status);
int energy_get_load_status(load_type_t load, load_status_t* status);

int energy_set_peak_limit(float max_kw);
int energy_request_shed(float kw_to_shed);
int energy_restore_loads(void);

int energy_receive_dr_event(const dr_event_t* event);
int energy_respond_to_dr(bool accept);
int energy_get_dr_savings(float* kw_reduced, float* incentive_earned);

int energy_schedule_load(load_type_t load, uint32_t start_time, 
                        uint32_t duration_s);
int energy_get_forecast(float* hourly_kw, uint8_t hours);

int energy_get_savings_report(uint8_t* data, uint32_t max_len, uint32_t* len);
float energy_get_efficiency_score(void);

#endif /* GF_ENERGY_OPTIMIZATION_H */
