/**
 * @file wearable_sensor.h
 * @brief Smart Textile Wearable Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Smart textiles represent a $5B+ market integrating sensors into
 * clothing for healthcare, sports, military, and industrial applications.
 * Companies like Hexoskin, Sensoria, and Google/Levi's Jacquard create
 * garments with embedded biometric and environmental sensing.
 * 
 * This stub demonstrates:
 * - Flexible sensor integration (strain, temperature, biopotential)
 * - Low-power textile-embedded processing
 * - Washable electronics considerations
 * - Multi-zone body mapping
 * 
 * STANDARDS:
 * - IEC 63203 (Wearable Electronic Devices)
 * - ISO 10993 (Biocompatibility)
 * - AATCC (Textile testing methods)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_WEARABLE_SENSOR_H
#define GF_WEARABLE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration
 * ============================================================================ */

#define TEXTILE_MAX_ZONES         16     /**< Body zones with sensors */
#define TEXTILE_MAX_SENSORS       64     /**< Total sensors per garment */
#define TEXTILE_SAMPLE_RATE_HZ    50     /**< Default sample rate */

/* ============================================================================
 * Types
 * ============================================================================ */

/** Sensor type embedded in textile */
typedef enum {
    TEXTILE_SENSOR_STRAIN,       /**< Stretch/flex sensing */
    TEXTILE_SENSOR_PRESSURE,     /**< Contact pressure */
    TEXTILE_SENSOR_TEMPERATURE,  /**< Skin temperature */
    TEXTILE_SENSOR_HUMIDITY,     /**< Moisture/sweat */
    TEXTILE_SENSOR_ECG,          /**< Electrocardiogram */
    TEXTILE_SENSOR_EMG,          /**< Electromyography */
    TEXTILE_SENSOR_GSR,          /**< Galvanic skin response */
    TEXTILE_SENSOR_MOTION        /**< Accelerometer/gyro */
} textile_sensor_type_t;

/** Body zone location */
typedef enum {
    ZONE_CHEST,
    ZONE_BACK,
    ZONE_LEFT_ARM,
    ZONE_RIGHT_ARM,
    ZONE_LEFT_LEG,
    ZONE_RIGHT_LEG,
    ZONE_CORE,
    ZONE_HEAD
} body_zone_t;

/** Sensor reading */
typedef struct {
    textile_sensor_type_t type;
    body_zone_t zone;
    float value;
    float quality;        /**< Signal quality 0-1 */
    uint64_t timestamp_us;
    bool contact_good;    /**< Skin contact status */
} textile_reading_t;

/** Sensor configuration */
typedef struct {
    uint8_t sensor_id;
    textile_sensor_type_t type;
    body_zone_t zone;
    float gain;
    float threshold_low;
    float threshold_high;
    uint16_t sample_rate_hz;
} textile_sensor_config_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

int textile_sensor_init(void);
int textile_sensor_configure(uint8_t sensor_id, const textile_sensor_config_t *config);
int textile_sensor_read(uint8_t sensor_id, textile_reading_t *reading);
int textile_sensor_read_zone(body_zone_t zone, textile_reading_t *readings, uint8_t max);
int textile_sensor_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_WEARABLE_SENSOR_H */
