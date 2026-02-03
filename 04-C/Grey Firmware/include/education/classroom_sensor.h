/**
 * @file classroom_sensor.h
 * @brief Classroom Sensor Interface for Smart Education Technology
 * 
 * @details
 * This module provides environmental and occupancy sensing for
 * smart classroom applications, enabling adaptive learning
 * environments and facility optimization.
 * 
 * INDUSTRY RELEVANCE:
 * - EdTech Companies: Google Classroom, Microsoft Teams for Education
 * - School Districts: Smart building management integration
 * - Universities: Campus-wide IoT deployments
 * - Corporate Training: Learning center optimization
 * - Museum/Exhibition: Interactive exhibit sensing
 * 
 * APPLICATIONS:
 * - Environmental monitoring (CO2, temperature, humidity)
 * - Occupancy detection and counting
 * - Noise level monitoring
 * - Light level optimization
 * - Air quality for student wellness
 * 
 * COMPLIANCE:
 * - ASHRAE 62.1: Ventilation for acceptable IAQ
 * - WELL Building Standard: Cognitive function optimization
 * - ADA: Accessibility sensing
 * - FERPA: Student data privacy
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_CLASSROOM_SENSOR_H
#define GF_CLASSROOM_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum classrooms */
#define GF_CLASSROOM_MAX_ROOMS          50

/** Maximum sensors per room */
#define GF_CLASSROOM_MAX_SENSORS        8

/** Optimal CO2 level (ppm) */
#define GF_CLASSROOM_CO2_OPTIMAL        800

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Sensor types
 */
typedef enum {
    GF_CLASS_SENSOR_CO2,          /**< CO2 sensor */
    GF_CLASS_SENSOR_TEMP,         /**< Temperature */
    GF_CLASS_SENSOR_HUMIDITY,     /**< Humidity */
    GF_CLASS_SENSOR_LIGHT,        /**< Light level */
    GF_CLASS_SENSOR_NOISE,        /**< Noise level */
    GF_CLASS_SENSOR_OCCUPANCY,    /**< Occupancy detection */
    GF_CLASS_SENSOR_PM25,         /**< Particulate matter */
    GF_CLASS_SENSOR_VOC           /**< Volatile organic compounds */
} gf_classroom_sensor_type_t;

/**
 * @brief Room status
 */
typedef enum {
    GF_ROOM_VACANT,               /**< Unoccupied */
    GF_ROOM_OCCUPIED,             /**< Occupied */
    GF_ROOM_IN_SESSION,           /**< Class in session */
    GF_ROOM_TRANSITION,           /**< Between classes */
    GF_ROOM_MAINTENANCE           /**< Under maintenance */
} gf_room_status_t;

/**
 * @brief Environmental quality score
 */
typedef struct {
    uint8_t overall_score;        /**< Overall score (0-100) */
    uint8_t air_quality;          /**< Air quality score */
    uint8_t thermal_comfort;      /**< Thermal comfort score */
    uint8_t lighting;             /**< Lighting score */
    uint8_t acoustics;            /**< Acoustics score */
    bool alerts_active;           /**< Any alerts active */
} gf_env_quality_t;

/**
 * @brief Classroom reading
 */
typedef struct {
    char room_id[16];             /**< Room identifier */
    gf_room_status_t status;      /**< Room status */
    uint16_t occupancy_count;     /**< Current occupancy */
    uint16_t capacity;            /**< Room capacity */
    float temperature_c;          /**< Temperature (°C) */
    float humidity_pct;           /**< Humidity (%) */
    uint16_t co2_ppm;             /**< CO2 level (ppm) */
    float light_lux;              /**< Light level (lux) */
    float noise_dba;              /**< Noise level (dBA) */
    gf_env_quality_t quality;     /**< Quality scores */
    uint32_t timestamp;           /**< Reading timestamp */
} gf_classroom_reading_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    char room_id[16];             /**< Room identifier */
    uint16_t capacity;            /**< Room capacity */
    float temp_setpoint_c;        /**< Temperature setpoint */
    uint16_t co2_threshold_ppm;   /**< CO2 alert threshold */
    float light_target_lux;       /**< Target light level */
    float noise_threshold_dba;    /**< Noise alert threshold */
} gf_classroom_config_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize classroom sensor system
 * @return 0 on success
 */
int gf_classroom_sensor_init(void);

/**
 * @brief Shutdown classroom sensor system
 */
void gf_classroom_sensor_shutdown(void);

/**
 * @brief Configure room
 * @param config Room configuration
 * @return 0 on success
 */
int gf_classroom_configure(const gf_classroom_config_t* config);

/**
 * @brief Get room reading
 * @param room_id Room identifier
 * @param reading Output reading
 * @return 0 on success
 */
int gf_classroom_get_reading(const char* room_id, 
                              gf_classroom_reading_t* reading);

/**
 * @brief Get environmental quality
 * @param room_id Room identifier
 * @param quality Output quality
 * @return 0 on success
 */
int gf_classroom_get_quality(const char* room_id,
                              gf_env_quality_t* quality);

/**
 * @brief Update occupancy count
 * @param room_id Room identifier
 * @param count New count
 * @return 0 on success
 */
int gf_classroom_update_occupancy(const char* room_id, uint16_t count);

/**
 * @brief Process classroom sensors (call periodically)
 * @param delta_ms Time since last call
 */
void gf_classroom_sensor_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_CLASSROOM_SENSOR_H */
