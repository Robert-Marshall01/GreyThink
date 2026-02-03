/**
 * @file radiation_sensor.h
 * @brief Advanced Space Habitat Radiation Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Long-duration space missions (Lunar Gateway, Mars transit) require continuous
 * radiation monitoring to protect crew health. Solar particle events (SPE) and
 * galactic cosmic rays (GCR) pose significant risks. This sensor interface
 * enables real-time dosimetry for crew safety decisions.
 * 
 * TECHNICAL SCOPE:
 * - Multi-sensor fusion (silicon diodes, scintillators, tissue-equivalent)
 * - Dose rate measurement (μSv/hr to Sv/hr range)
 * - Particle discrimination (proton, heavy ion, neutron)
 * - Linear Energy Transfer (LET) spectroscopy
 * - SPE onset detection with <60 second warning
 * 
 * STANDARDS COMPLIANCE:
 * - NASA-STD-3001 (Crew health)
 * - ICRP Publication 123 (Space radiation)
 * - ESA ECSS-E-ST-10-12C (Radiation effects)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_RADIATION_SENSOR_H
#define GF_RADIATION_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define RAD_MAX_SENSORS         8     /**< Maximum radiation sensors */
#define RAD_SPECTRUM_BINS       64    /**< LET spectrum bins */
#define RAD_HISTORY_SAMPLES     1440  /**< 24 hours at 1-minute intervals */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Radiation sensor type */
typedef enum {
    RAD_SENSOR_SILICON_DIODE,      /**< PIN diode dosimeter */
    RAD_SENSOR_SCINTILLATOR,       /**< Plastic/crystal scintillator */
    RAD_SENSOR_TISSUE_EQUIV,       /**< TEPC (tissue-equivalent) */
    RAD_SENSOR_NEUTRON,            /**< Neutron dosimeter */
    RAD_SENSOR_CHERENKOV           /**< High-energy particle detector */
} rad_sensor_type_t;

/** Particle type classification */
typedef enum {
    RAD_PARTICLE_PROTON,
    RAD_PARTICLE_ELECTRON,
    RAD_PARTICLE_ALPHA,
    RAD_PARTICLE_HEAVY_ION,
    RAD_PARTICLE_NEUTRON,
    RAD_PARTICLE_GAMMA,
    RAD_PARTICLE_UNKNOWN
} rad_particle_t;

/** Radiation environment classification */
typedef enum {
    RAD_ENV_NOMINAL,               /**< Normal GCR background */
    RAD_ENV_ELEVATED,              /**< Slightly elevated */
    RAD_ENV_SPE_ONSET,             /**< Solar particle event starting */
    RAD_ENV_SPE_PEAK,              /**< SPE at maximum */
    RAD_ENV_SPE_DECAY,             /**< SPE declining */
    RAD_ENV_BELT_PASSAGE,          /**< Van Allen belt transit */
    RAD_ENV_EMERGENCY              /**< Immediate shelter required */
} rad_environment_t;

/** Individual sensor reading */
typedef struct {
    rad_sensor_type_t type;
    float dose_rate_usv_hr;        /**< Current dose rate (μSv/hr) */
    float cumulative_dose_msv;     /**< Total mission dose (mSv) */
    float let_spectrum[RAD_SPECTRUM_BINS]; /**< LET spectrum data */
    float quality_factor;          /**< Radiation quality factor */
    float dose_equivalent_usv_hr;  /**< Dose equivalent rate */
    uint32_t particle_count;       /**< Particles detected per second */
    uint64_t timestamp_us;
    bool healthy;
} rad_sensor_reading_t;

/** Aggregated radiation status */
typedef struct {
    rad_environment_t environment;
    float ambient_dose_rate_usv_hr;
    float peak_dose_rate_usv_hr;
    float crew_exposure_msv[6];    /**< Per-astronaut exposure */
    float spe_probability;         /**< SPE prediction (0-1) */
    uint32_t time_to_limit_hr;     /**< Hours until dose limit */
    bool shelter_recommended;
    bool eva_safe;
} rad_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize radiation sensor array */
int rad_sensor_init(void);

/** Configure individual sensor */
int rad_sensor_config(uint8_t sensor_id, rad_sensor_type_t type);

/** Read current sensor data */
int rad_sensor_read(uint8_t sensor_id, rad_sensor_reading_t *reading);

/** Get fused radiation status */
int rad_get_status(rad_status_t *status);

/** Check if EVA conditions are safe */
bool rad_is_eva_safe(void);

/** Trigger shelter alert */
int rad_trigger_shelter_alert(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_RADIATION_SENSOR_H */
