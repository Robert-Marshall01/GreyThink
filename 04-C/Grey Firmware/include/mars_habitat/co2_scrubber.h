/**
 * @file co2_scrubber.h
 * @brief CO₂ Scrubber Sensor Driver for Mars Habitats
 * 
 * INDUSTRY RELEVANCE:
 * Mars habitat life support systems require continuous atmospheric monitoring
 * and CO₂ removal to maintain crew safety. This driver interfaces with
 * electrochemical and NDIR sensors measuring CO₂ levels, scrubber efficiency,
 * and filter saturation. Critical for NASA Artemis/Gateway, SpaceX Starship
 * habitation modules, and commercial space station developments.
 * 
 * Applications:
 * - Mars surface habitats (NASA, SpaceX)
 * - Lunar Gateway life support
 * - Commercial space stations
 * - Submarine atmospheric control
 * - Underground bunker ventilation
 * 
 * Standards: NASA-STD-3001 (Human-System Standard), ECSS-E-ST-34C (Environmental Control)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CO2_SCRUBBER_H
#define GF_CO2_SCRUBBER_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define CO2_MAX_SENSORS             8       /* Maximum CO₂ sensor channels */
#define CO2_MAX_SCRUBBERS           4       /* Maximum scrubber units */
#define CO2_SAMPLE_RATE_HZ          10      /* Sampling frequency */

/* Safety thresholds (ppm) */
#define CO2_NOMINAL_PPM             400     /* Earth-like nominal */
#define CO2_ELEVATED_PPM            1000    /* Elevated warning */
#define CO2_WARNING_PPM             5000    /* Crew warning */
#define CO2_DANGER_PPM              20000   /* Immediate action required */
#define CO2_CRITICAL_PPM            50000   /* Life threatening */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** CO₂ sensor type */
typedef enum {
    CO2_SENSOR_NDIR,            /* Non-dispersive infrared */
    CO2_SENSOR_ELECTROCHEMICAL, /* Electrochemical cell */
    CO2_SENSOR_PHOTOACOUSTIC,   /* Photoacoustic spectroscopy */
    CO2_SENSOR_METAL_OXIDE      /* Metal oxide semiconductor */
} co2_sensor_type_t;

/** Scrubber technology */
typedef enum {
    SCRUBBER_LITHIUM_HYDROXIDE, /* LiOH chemical absorption */
    SCRUBBER_MOLECULAR_SIEVE,   /* Zeolite molecular sieve */
    SCRUBBER_SABATIER,          /* Sabatier reactor (CO₂ + H₂ → CH₄ + H₂O) */
    SCRUBBER_MOXIE,             /* MOXIE-style O₂ generation */
    SCRUBBER_ALGAE_BIOREACTOR   /* Biological photosynthesis */
} scrubber_type_t;

/** Scrubber status */
typedef enum {
    SCRUBBER_STATUS_IDLE,
    SCRUBBER_STATUS_ACTIVE,
    SCRUBBER_STATUS_REGENERATING,
    SCRUBBER_STATUS_SATURATED,
    SCRUBBER_STATUS_FAULT
} scrubber_status_t;

/** CO₂ reading */
typedef struct {
    uint8_t sensor_id;
    co2_sensor_type_t type;
    uint32_t co2_ppm;
    float temperature_c;
    float relative_accuracy;
    uint32_t timestamp_ms;
    bool valid;
} co2_reading_t;

/** Scrubber state */
typedef struct {
    uint8_t unit_id;
    scrubber_type_t type;
    scrubber_status_t status;
    float efficiency_pct;
    float saturation_pct;
    float flow_rate_lpm;
    uint32_t runtime_hours;
    uint32_t cycles_remaining;
} scrubber_state_t;

/** Module configuration */
typedef struct {
    uint8_t num_sensors;
    uint8_t num_scrubbers;
    uint32_t target_co2_ppm;
    bool auto_regeneration;
    bool redundant_mode;
} co2_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int co2_scrubber_init(const co2_config_t *config);
void co2_scrubber_shutdown(void);

int co2_read_sensor(uint8_t sensor_id, co2_reading_t *reading);
int co2_read_all_sensors(co2_reading_t *readings, uint8_t max_count);
uint32_t co2_get_average_ppm(void);

int co2_scrubber_start(uint8_t unit_id);
int co2_scrubber_stop(uint8_t unit_id);
int co2_scrubber_regenerate(uint8_t unit_id);
int co2_get_scrubber_state(uint8_t unit_id, scrubber_state_t *state);

bool co2_is_level_safe(void);
int co2_get_alert_level(void);

#endif /* GF_CO2_SCRUBBER_H */
