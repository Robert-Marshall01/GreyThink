/**
 * @file regolith_sensor.h
 * @brief Regolith Extraction Sensor Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Lunar regolith (surface dust/soil) contains valuable resources including
 * oxygen, water ice, and rare earth elements. Companies like ispace, Astrobotic,
 * and NASA's Artemis program are developing in-situ resource utilization (ISRU)
 * systems. Embedded engineers need expertise in:
 * - High-reliability sensor interfaces for lunar environment (-280°C to +120°C)
 * - Radiation-hardened electronics design
 * - Low-power operation during lunar night (14 Earth days)
 * - Real-time material composition analysis
 * 
 * This module demonstrates sensor acquisition for regolith extraction systems
 * used in lunar mining operations, oxygen production, and construction.
 * 
 * STANDARDS:
 * - NASA-STD-8739.8 (Software Assurance and Safety)
 * - ECSS-E-ST-40C (Space Engineering - Software)
 * - CCSDS 133.0-B-2 (Space Packet Protocol)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_REGOLITH_SENSOR_H
#define GF_REGOLITH_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define REGOLITH_MAX_SENSORS        16   /**< Maximum sensor channels */
#define REGOLITH_SAMPLE_RATE_HZ     100  /**< Default sample rate */
#define REGOLITH_COMP_CHANNELS      8    /**< Composition analysis channels */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Sensor type enumeration */
typedef enum {
    REGOLITH_SENSOR_DENSITY,         /**< Bulk density measurement */
    REGOLITH_SENSOR_COMPOSITION,     /**< Chemical composition (XRF) */
    REGOLITH_SENSOR_PARTICLE_SIZE,   /**< Particle size distribution */
    REGOLITH_SENSOR_MOISTURE,        /**< Water ice content */
    REGOLITH_SENSOR_TEMPERATURE,     /**< Regolith temperature */
    REGOLITH_SENSOR_DEPTH,           /**< Excavation depth */
    REGOLITH_SENSOR_FORCE,           /**< Cutting/drilling force */
    REGOLITH_SENSOR_VIBRATION        /**< Vibration/resonance */
} regolith_sensor_type_t;

/** Composition element */
typedef enum {
    ELEM_OXYGEN,      /**< O - Primary extraction target */
    ELEM_SILICON,     /**< Si - Abundant in regolith */
    ELEM_IRON,        /**< Fe - Metal extraction */
    ELEM_ALUMINUM,    /**< Al - Construction material */
    ELEM_CALCIUM,     /**< Ca */
    ELEM_MAGNESIUM,   /**< Mg */
    ELEM_TITANIUM,    /**< Ti - High-value metal */
    ELEM_HYDROGEN     /**< H - Water ice indicator */
} regolith_element_t;

/** Sensor reading structure */
typedef struct {
    regolith_sensor_type_t type;
    uint8_t channel;
    float value;
    float uncertainty;      /**< Measurement uncertainty */
    uint32_t timestamp;
    bool valid;
} regolith_reading_t;

/** Composition analysis result */
typedef struct {
    float percentages[REGOLITH_COMP_CHANNELS];  /**< Element percentages */
    float confidence;                            /**< Analysis confidence */
    uint32_t integration_time_ms;               /**< XRF integration time */
    bool water_ice_detected;
} composition_result_t;

/** Sensor configuration */
typedef struct {
    regolith_sensor_type_t type;
    uint8_t channel;
    uint16_t sample_rate_hz;
    float calibration_offset;
    float calibration_scale;
    bool enabled;
} regolith_sensor_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize the regolith sensor subsystem
 * @return 0 on success, negative error code on failure
 */
int regolith_sensor_init(void);

/**
 * @brief Configure a sensor channel
 * @param config Pointer to sensor configuration
 * @return 0 on success, negative error code on failure
 */
int regolith_sensor_config(const regolith_sensor_config_t *config);

/**
 * @brief Read a single sensor value
 * @param type Sensor type to read
 * @param channel Sensor channel
 * @param reading Pointer to store reading
 * @return 0 on success, negative error code on failure
 */
int regolith_sensor_read(regolith_sensor_type_t type, uint8_t channel,
                         regolith_reading_t *reading);

/**
 * @brief Perform composition analysis
 * @param result Pointer to store composition result
 * @return 0 on success, negative error code on failure
 */
int regolith_analyze_composition(composition_result_t *result);

/**
 * @brief Run sensor self-test
 * @return 0 if all sensors pass, negative error code on failure
 */
int regolith_sensor_selftest(void);

/**
 * @brief Shutdown the sensor subsystem
 */
void regolith_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_REGOLITH_SENSOR_H */
