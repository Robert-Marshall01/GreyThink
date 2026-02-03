/**
 * @file hydroponic_sensor.h
 * @brief Hydroponic System Sensor Interface for Vertical Farms
 *
 * INDUSTRY RELEVANCE:
 * Vertical farming is revolutionizing agriculture with 95% less water usage
 * and year-round local production. Precise sensor monitoring of nutrient
 * solutions, pH, EC, and dissolved oxygen enables optimal crop growth
 * in controlled environments.
 *
 * MARKET CONTEXT:
 * - AeroFarms, Plenty, Bowery Farming indoor agriculture
 * - Urban food security and local production initiatives
 * - Space agriculture research (NASA VEGGIE, ESA MELiSSA)
 * - Restaurant and grocery store on-site growing
 * - Cannabis cultivation precision agriculture
 *
 * TECHNICAL APPROACH:
 * - Multi-parameter water quality sensors (pH, EC, DO, temp)
 * - Flow rate monitoring for nutrient delivery
 * - Root zone moisture and temperature sensing
 * - Ambient CO2 and humidity monitoring
 * - Light spectrum and intensity measurement
 *
 * @author Grey Firmware Project
 */

#ifndef GF_HYDROPONIC_SENSOR_H
#define GF_HYDROPONIC_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Nutrient solution parameters
 */
typedef struct {
    float ph;                    /**< pH level (5.5-6.5 optimal) */
    float ec_ms_cm;              /**< Electrical conductivity mS/cm */
    float temp_c;                /**< Solution temperature */
    float do_mg_l;               /**< Dissolved oxygen mg/L */
    float tds_ppm;               /**< Total dissolved solids */
    float flow_rate_lpm;         /**< Flow rate liters/min */
    uint32_t timestamp;
    bool valid;
} gf_nutrient_reading_t;

/**
 * @brief Environmental parameters
 */
typedef struct {
    float air_temp_c;
    float humidity_percent;
    float co2_ppm;
    float vpd_kpa;               /**< Vapor pressure deficit */
    float light_ppfd;            /**< Photosynthetic photon flux */
    uint16_t light_spectrum[8];  /**< Spectrum bins */
    uint32_t timestamp;
} gf_grow_environment_t;

/**
 * @brief Root zone parameters
 */
typedef struct {
    float moisture_percent;
    float temp_c;
    float ec_ms_cm;
    uint8_t zone_id;
} gf_rootzone_t;

/* Function prototypes */
int gf_hydro_sensor_init(void);
int gf_hydro_read_nutrients(gf_nutrient_reading_t *reading);
int gf_hydro_read_environment(gf_grow_environment_t *env);
int gf_hydro_read_rootzone(uint8_t zone, gf_rootzone_t *rz);
int gf_hydro_calibrate_ph(float ref_value);
int gf_hydro_calibrate_ec(float ref_value);
bool gf_hydro_sensor_healthy(void);
void gf_hydro_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_HYDROPONIC_SENSOR_H */
