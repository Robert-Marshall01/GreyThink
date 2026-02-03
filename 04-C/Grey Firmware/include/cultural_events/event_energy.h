/**
 * @file event_energy.h
 * @brief Event Energy Optimization and Sustainability Interface
 * 
 * INDUSTRY RELEVANCE:
 * Major events consume significant energy requiring optimization:
 * - Concert lighting (10-100kW per show)
 * - HVAC for climate control
 * - AV systems and displays
 * - Food service equipment
 * 
 * Sustainability increasingly demanded by sponsors and attendees.
 * Embedded engineers enable:
 * - Real-time energy monitoring per zone/system
 * - Demand response during peak periods
 * - Renewable integration (solar, battery storage)
 * - Carbon footprint calculation
 * 
 * STANDARDS:
 * - ISO 50001 (Energy Management)
 * - ISO 14064 (GHG Accounting)
 * - PAS 2060 (Carbon Neutrality)
 * - LEED for Events
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_EVENT_ENERGY_H
#define GF_EVENT_ENERGY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define ENERGY_MAX_METERS         256  /**< Maximum energy meters */
#define ENERGY_MAX_ZONES          128  /**< Maximum energy zones */
#define ENERGY_SAMPLE_INTERVAL_S  60   /**< Metering interval */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Energy source */
typedef enum {
    SOURCE_GRID,
    SOURCE_DIESEL_GEN,
    SOURCE_SOLAR,
    SOURCE_BATTERY,
    SOURCE_WIND,
    SOURCE_HYBRID
} energy_source_t;

/** Load category */
typedef enum {
    LOAD_LIGHTING,
    LOAD_AUDIO,
    LOAD_VIDEO,
    LOAD_HVAC,
    LOAD_FOOD_SERVICE,
    LOAD_TRANSPORT,
    LOAD_GENERAL
} energy_load_t;

/** Power quality */
typedef enum {
    QUALITY_EXCELLENT,
    QUALITY_GOOD,
    QUALITY_ACCEPTABLE,
    QUALITY_POOR
} power_quality_t;

/** Energy meter reading */
typedef struct {
    uint32_t meter_id;
    float power_kw;
    float energy_kwh;
    float voltage_v;
    float current_a;
    float power_factor;
    power_quality_t quality;
    uint32_t timestamp;
} energy_reading_t;

/** Zone energy profile */
typedef struct {
    char zone_id[16];
    float current_load_kw;
    float peak_load_kw;
    float cumulative_kwh;
    energy_source_t primary_source;
    float renewable_pct;
    float carbon_kg_co2;
} zone_energy_profile_t;

/** Event energy summary */
typedef struct {
    float total_energy_kwh;
    float peak_demand_kw;
    float average_load_kw;
    float renewable_pct;
    float carbon_kg_co2;
    float cost_estimate;
    uint32_t duration_hours;
} event_energy_summary_t;

/** Demand response action */
typedef enum {
    DR_ACTION_NONE,
    DR_ACTION_DIM_LIGHTS,
    DR_ACTION_REDUCE_HVAC,
    DR_ACTION_SHED_NONESSENTIAL,
    DR_ACTION_SWITCH_BATTERY,
    DR_ACTION_START_GENERATOR
} demand_response_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int event_energy_init(void);
int event_energy_add_meter(uint32_t meter_id, const char *zone_id,
                           energy_load_t load_type);
int event_energy_set_source(const char *zone_id, energy_source_t source);
int event_energy_read_meter(uint32_t meter_id, energy_reading_t *reading);
int event_energy_get_zone_profile(const char *zone_id, 
                                  zone_energy_profile_t *profile);
int event_energy_get_summary(event_energy_summary_t *summary);
int event_energy_demand_response(demand_response_t action);
int event_energy_set_carbon_factor(energy_source_t source, 
                                   float kg_co2_per_kwh);
float event_energy_calculate_carbon(void);
void event_energy_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EVENT_ENERGY_H */
