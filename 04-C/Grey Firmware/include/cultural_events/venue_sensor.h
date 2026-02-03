/**
 * @file venue_sensor.h
 * @brief Smart Venue Environmental Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Smart venues optimize attendee experience and safety:
 * - Concert halls (MSG, O2 Arena)
 * - Sports stadiums (NFL, FIFA venues)
 * - Convention centers (CES, SXSW)
 * - Theme parks (Disney, Universal)
 * 
 * Sensor networks monitor air quality, noise levels, temperature
 * to ensure comfort and regulatory compliance. Embedded engineers:
 * - Deploy wireless mesh sensor networks
 * - Fuse multi-modal environmental data
 * - Provide real-time HVAC feedback
 * - Enable predictive maintenance
 * 
 * STANDARDS:
 * - ASHRAE 55 (Thermal Comfort)
 * - ASHRAE 62.1 (Ventilation)
 * - IEC 61672 (Sound Level Meters)
 * - ISO 16000 (Indoor Air Quality)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_VENUE_SENSOR_H
#define GF_VENUE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define VENUE_MAX_ZONES           512  /**< Maximum venue zones */
#define VENUE_MAX_SENSORS         4096 /**< Maximum sensors */
#define VENUE_SAMPLE_RATE_HZ      10   /**< Environmental sample rate */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Sensor type */
typedef enum {
    SENSOR_TEMPERATURE,
    SENSOR_HUMIDITY,
    SENSOR_CO2,
    SENSOR_PM25,
    SENSOR_NOISE_LEVEL,
    SENSOR_LIGHT_LEVEL,
    SENSOR_OCCUPANCY,
    SENSOR_AIR_VELOCITY
} venue_sensor_type_t;

/** Zone type */
typedef enum {
    VENUE_ZONE_GENERAL_ADMISSION,
    VENUE_ZONE_VIP,
    VENUE_ZONE_STAGE,
    VENUE_ZONE_CONCOURSE,
    VENUE_ZONE_RESTROOM,
    VENUE_ZONE_FOOD_COURT,
    VENUE_ZONE_ENTRANCE,
    VENUE_ZONE_EMERGENCY_EXIT
} venue_zone_type_t;

/** Comfort level */
typedef enum {
    COMFORT_EXCELLENT,
    COMFORT_GOOD,
    COMFORT_ACCEPTABLE,
    COMFORT_POOR,
    COMFORT_CRITICAL
} comfort_level_t;

/** Environmental reading */
typedef struct {
    uint32_t sensor_id;
    venue_sensor_type_t type;
    float value;
    uint32_t timestamp;
    bool valid;
} venue_reading_t;

/** Zone status */
typedef struct {
    char zone_id[16];
    venue_zone_type_t type;
    float temperature_c;
    float humidity_pct;
    float co2_ppm;
    float pm25_ugm3;
    float noise_dba;
    float light_lux;
    uint32_t occupancy;
    comfort_level_t comfort;
} zone_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int venue_sensor_init(void);
int venue_register_zone(const char *zone_id, venue_zone_type_t type,
                        uint16_t capacity);
int venue_add_sensor(uint32_t sensor_id, venue_sensor_type_t type,
                     const char *zone_id);
int venue_read_sensor(uint32_t sensor_id, venue_reading_t *reading);
int venue_get_zone_status(const char *zone_id, zone_status_t *status);
int venue_set_comfort_thresholds(const char *zone_id, float temp_min,
                                  float temp_max, float humidity_max);
comfort_level_t venue_calculate_comfort(const zone_status_t *status);
void venue_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_VENUE_SENSOR_H */
