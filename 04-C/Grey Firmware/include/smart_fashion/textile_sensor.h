/**
 * @file textile_sensor.h
 * @brief Wearable Textile Sensor Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Smart textiles (e-textiles) integrate electronics into clothing for health
 * monitoring, athletic performance, and interactive fashion. The market is
 * projected to reach $15B+ by 2030, driven by:
 * - Health wearables (ECG, respiration, posture)
 * - Athletic performance tracking
 * - Interactive/responsive clothing
 * - Worker safety monitoring
 * 
 * Companies like Hexoskin, Sensoria, and Google ATAP develop smart textiles.
 * Embedded engineers need expertise in:
 * - Flexible/stretchable electronics interfacing
 * - Low-power operation for day-long wear
 * - Washability and durability considerations
 * - Multi-sensor fusion on constrained MCUs
 * 
 * STANDARDS:
 * - IEC 63203 (Wearable Device Standards)
 * - ISO 11092 (Thermal Resistance of Textiles)
 * - OEKO-TEX 100 (Safety in Textiles)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TEXTILE_SENSOR_H
#define GF_TEXTILE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define TEXTILE_MAX_ZONES           16   /**< Maximum sensor zones */
#define TEXTILE_SAMPLE_RATE_HZ      50   /**< Default sample rate */
#define TEXTILE_LOW_POWER_HZ        1    /**< Low-power sample rate */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Textile sensor type */
typedef enum {
    TEXTILE_SENSOR_STRAIN,       /**< Stretch/strain gauge */
    TEXTILE_SENSOR_PRESSURE,     /**< Pressure-sensitive zone */
    TEXTILE_SENSOR_TEMPERATURE,  /**< Temperature sensor */
    TEXTILE_SENSOR_HUMIDITY,     /**< Moisture/sweat detection */
    TEXTILE_SENSOR_ECG,          /**< Heart rate electrodes */
    TEXTILE_SENSOR_EMG,          /**< Muscle activity */
    TEXTILE_SENSOR_MOTION,       /**< Accelerometer/gyro */
    TEXTILE_SENSOR_LIGHT         /**< Ambient light/color */
} textile_sensor_type_t;

/** Garment zone location */
typedef enum {
    ZONE_CHEST,
    ZONE_BACK,
    ZONE_LEFT_ARM,
    ZONE_RIGHT_ARM,
    ZONE_LEFT_LEG,
    ZONE_RIGHT_LEG,
    ZONE_WAIST,
    ZONE_NECK
} garment_zone_t;

/** Sensor reading */
typedef struct {
    textile_sensor_type_t type;
    garment_zone_t zone;
    float value;
    uint32_t timestamp;
    bool valid;
} textile_reading_t;

/** Sensor configuration */
typedef struct {
    textile_sensor_type_t type;
    garment_zone_t zone;
    uint16_t sample_rate_hz;
    float threshold;             /**< Event trigger threshold */
    bool enabled;
} textile_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int textile_sensor_init(void);
int textile_sensor_config(const textile_config_t *config);
int textile_sensor_read(garment_zone_t zone, textile_sensor_type_t type,
                        textile_reading_t *reading);
int textile_sensor_read_all(textile_reading_t *readings, uint8_t *count);
int textile_enter_low_power(void);
int textile_exit_low_power(void);
void textile_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TEXTILE_SENSOR_H */
