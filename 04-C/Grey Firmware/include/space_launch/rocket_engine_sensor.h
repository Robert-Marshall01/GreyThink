/**
 * @file rocket_engine_sensor.h
 * @brief Rocket Engine Sensor Interface for Space Launch Systems
 *
 * INDUSTRY RELEVANCE:
 * Space launch systems require extreme precision in monitoring rocket engine
 * parameters. This stub provides the foundation for interfacing with thrust
 * chambers, turbopumps, and combustion sensors - critical for companies like
 * SpaceX, Blue Origin, Rocket Lab, and traditional aerospace contractors.
 *
 * Key capabilities demonstrated:
 * - High-frequency sensor sampling (>1kHz for vibration)
 * - Cryogenic temperature range support (-253°C to +3500°C)
 * - Redundant sensor fusion for flight-critical systems
 * - MIL-STD-1553 / SpaceWire compatible data formats
 *
 * @note This is a stub header for portfolio demonstration.
 * @see docs/rocket_systems.md for spotlight implementation details.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_ROCKET_ENGINE_SENSOR_H
#define GF_ROCKET_ENGINE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum number of engine sensors per vehicle */
#define GF_ROCKET_MAX_SENSORS           128

/** Sensor sampling rates (Hz) */
#define GF_ROCKET_SAMPLE_RATE_THRUST    1000
#define GF_ROCKET_SAMPLE_RATE_TEMP      100
#define GF_ROCKET_SAMPLE_RATE_VIBRATION 10000

/** Temperature ranges (Celsius) */
#define GF_ROCKET_TEMP_CRYO_MIN         (-253)  /* Liquid hydrogen */
#define GF_ROCKET_TEMP_CHAMBER_MAX      3500    /* Combustion chamber */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Sensor types for rocket engines */
typedef enum {
    GF_SENSOR_THRUST_MAIN,          /**< Main engine thrust transducer */
    GF_SENSOR_THRUST_CHAMBER,       /**< Chamber pressure sensor */
    GF_SENSOR_TEMP_CHAMBER,         /**< Combustion chamber thermocouple */
    GF_SENSOR_TEMP_NOZZLE,          /**< Nozzle throat temperature */
    GF_SENSOR_TEMP_TURBOPUMP,       /**< Turbopump bearing temperature */
    GF_SENSOR_TEMP_PROPELLANT,      /**< Propellant tank temperature */
    GF_SENSOR_VIBRATION_AXIAL,      /**< Axial vibration accelerometer */
    GF_SENSOR_VIBRATION_LATERAL,    /**< Lateral vibration accelerometer */
    GF_SENSOR_FLOW_OXIDIZER,        /**< Oxidizer flow meter */
    GF_SENSOR_FLOW_FUEL,            /**< Fuel flow meter */
    GF_SENSOR_PRESSURE_INJECTOR,    /**< Injector head pressure */
    GF_SENSOR_PRESSURE_TANK         /**< Propellant tank pressure */
} gf_rocket_sensor_type_t;

/** Sensor health status */
typedef enum {
    GF_SENSOR_HEALTH_OK,
    GF_SENSOR_HEALTH_DEGRADED,
    GF_SENSOR_HEALTH_FAILED,
    GF_SENSOR_HEALTH_OFFLINE
} gf_sensor_health_t;

/** Individual sensor reading */
typedef struct {
    uint8_t sensor_id;
    gf_rocket_sensor_type_t type;
    float value;
    float uncertainty;
    uint64_t timestamp_ns;
    gf_sensor_health_t health;
    bool redundant_agree;           /**< Redundant sensor agreement */
} gf_rocket_sensor_reading_t;

/** Engine sensor configuration */
typedef struct {
    uint8_t engine_id;
    const char* engine_name;
    uint16_t sensor_count;
    uint32_t sample_rate_hz;
    bool triple_redundant;
} gf_engine_sensor_config_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

/**
 * @brief Initialize rocket engine sensor subsystem
 * @param config Engine sensor configuration
 * @return 0 on success, negative on error
 */
int gf_rocket_sensor_init(const gf_engine_sensor_config_t* config);

/**
 * @brief Read current sensor value
 * @param sensor_id Sensor identifier
 * @param reading Output reading structure
 * @return 0 on success, negative on error
 */
int gf_rocket_sensor_read(uint8_t sensor_id, gf_rocket_sensor_reading_t* reading);

/**
 * @brief Get sensor health status
 * @param sensor_id Sensor identifier
 * @return Current health status
 */
gf_sensor_health_t gf_rocket_sensor_health(uint8_t sensor_id);

/**
 * @brief Perform sensor self-test
 * @param sensor_id Sensor identifier
 * @return 0 on pass, negative on failure
 */
int gf_rocket_sensor_selftest(uint8_t sensor_id);

/**
 * @brief Shutdown sensor subsystem
 */
void gf_rocket_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ROCKET_ENGINE_SENSOR_H */
