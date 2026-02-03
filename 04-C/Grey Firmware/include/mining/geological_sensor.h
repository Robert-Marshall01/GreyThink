/**
 * @file geological_sensor.h
 * @brief Geological Sensor Telemetry Interface
 * 
 * @details
 * This module provides interfaces for geological sensing in mining
 * operations, including rock characterization, ore detection, and
 * ground stability monitoring.
 * 
 * INDUSTRY RELEVANCE:
 * - Mining Exploration: Real-time ore grade sensing
 * - Tunnel Construction: Ground condition monitoring
 * - Oil & Gas: Formation evaluation while drilling (LWD/MWD)
 * - Geotechnical: Slope stability monitoring
 * - Civil Engineering: Foundation investigation
 * 
 * SENSOR TECHNOLOGIES:
 * - Gamma ray spectrometry (ore detection)
 * - Ground-penetrating radar (GPR)
 * - Seismic sensors (microseismic monitoring)
 * - Electromagnetic (EM) surveys
 * - XRF analyzers (elemental analysis)
 * 
 * SAFETY APPLICATIONS:
 * - Roof fall prediction
 * - Water ingress detection
 * - Gas pocket identification
 * - Void detection
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_GEOLOGICAL_SENSOR_H
#define GF_GEOLOGICAL_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum sensor channels */
#define GF_GEO_MAX_CHANNELS             16

/** Maximum spectral bins */
#define GF_GEO_MAX_SPECTRAL_BINS        256

/** Maximum seismic events stored */
#define GF_GEO_MAX_SEISMIC_EVENTS       100

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Geological sensor types
 */
typedef enum {
    GF_GEO_SENSOR_GAMMA,          /**< Gamma ray */
    GF_GEO_SENSOR_XRF,            /**< X-ray fluorescence */
    GF_GEO_SENSOR_GPR,            /**< Ground penetrating radar */
    GF_GEO_SENSOR_EM,             /**< Electromagnetic */
    GF_GEO_SENSOR_SEISMIC,        /**< Seismic/vibration */
    GF_GEO_SENSOR_ACOUSTIC,       /**< Acoustic emission */
    GF_GEO_SENSOR_RESISTIVITY,    /**< Electrical resistivity */
    GF_GEO_SENSOR_INCLINOMETER    /**< Inclinometer/tilt */
} gf_geo_sensor_type_t;

/**
 * @brief Rock classification
 */
typedef enum {
    GF_ROCK_CLASS_UNKNOWN,        /**< Unknown */
    GF_ROCK_CLASS_IGNEOUS,        /**< Igneous rock */
    GF_ROCK_CLASS_SEDIMENTARY,    /**< Sedimentary */
    GF_ROCK_CLASS_METAMORPHIC,    /**< Metamorphic */
    GF_ROCK_CLASS_ORE,            /**< Ore body */
    GF_ROCK_CLASS_FAULT,          /**< Fault zone */
    GF_ROCK_CLASS_VOID            /**< Void/cavity */
} gf_rock_class_t;

/**
 * @brief Seismic event severity
 */
typedef enum {
    GF_SEISMIC_BACKGROUND,        /**< Background noise */
    GF_SEISMIC_MINOR,             /**< Minor event */
    GF_SEISMIC_MODERATE,          /**< Moderate event */
    GF_SEISMIC_SIGNIFICANT,       /**< Significant event */
    GF_SEISMIC_MAJOR              /**< Major event */
} gf_seismic_severity_t;

/**
 * @brief Ore grade reading
 */
typedef struct {
    char element[8];              /**< Element symbol */
    float grade_pct;              /**< Grade (%) or ppm */
    float confidence;             /**< Confidence (0-1) */
    bool above_cutoff;            /**< Above economic cutoff */
} gf_ore_grade_t;

/**
 * @brief Seismic event
 */
typedef struct {
    uint32_t timestamp;           /**< Event timestamp */
    float magnitude;              /**< Richter magnitude */
    float location_x;             /**< X coordinate */
    float location_y;             /**< Y coordinate */
    float location_z;             /**< Z coordinate (depth) */
    gf_seismic_severity_t severity;
    float energy_joules;          /**< Released energy */
} gf_seismic_event_t;

/**
 * @brief Ground stability assessment
 */
typedef struct {
    float stability_index;        /**< 0-100 (higher = more stable) */
    float deformation_mm;         /**< Measured deformation */
    float stress_mpa;             /**< Estimated stress */
    bool warning_active;          /**< Stability warning */
    char risk_zone[32];           /**< Risk zone identifier */
} gf_ground_stability_t;

/**
 * @brief Geological reading
 */
typedef struct {
    gf_geo_sensor_type_t sensor;  /**< Sensor type */
    uint32_t timestamp;           /**< Reading timestamp */
    float depth_m;                /**< Depth */
    gf_rock_class_t rock_class;   /**< Rock classification */
    float value;                  /**< Primary value */
    float quality;                /**< Data quality (0-1) */
} gf_geo_reading_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize geological sensor system
 * @return 0 on success
 */
int gf_geo_sensor_init(void);

/**
 * @brief Shutdown geological sensor system
 */
void gf_geo_sensor_shutdown(void);

/**
 * @brief Get sensor reading
 * @param sensor Sensor type
 * @param reading Output reading
 * @return 0 on success
 */
int gf_geo_get_reading(gf_geo_sensor_type_t sensor, 
                        gf_geo_reading_t* reading);

/**
 * @brief Get ore grade analysis
 * @param grades Output array
 * @param max_elements Maximum elements
 * @return Number of elements detected
 */
int gf_geo_get_ore_grade(gf_ore_grade_t* grades, uint8_t max_elements);

/**
 * @brief Get recent seismic events
 * @param events Output array
 * @param max_events Maximum events
 * @return Number of events
 */
int gf_geo_get_seismic_events(gf_seismic_event_t* events, 
                               uint8_t max_events);

/**
 * @brief Get ground stability assessment
 * @param stability Output assessment
 * @return 0 on success
 */
int gf_geo_get_stability(gf_ground_stability_t* stability);

/**
 * @brief Process geological sensors (call periodically)
 * @param delta_ms Time since last call
 */
void gf_geo_sensor_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_GEOLOGICAL_SENSOR_H */
