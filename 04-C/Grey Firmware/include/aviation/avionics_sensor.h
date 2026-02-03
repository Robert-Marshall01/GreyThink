/**
 * @file avionics_sensor.h
 * @brief Avionics Sensor Input Driver Interface
 * 
 * @details
 * This module provides a hardware abstraction layer for avionics sensor
 * acquisition in aircraft and UAV systems. It handles critical flight
 * parameters from multiple sensor sources with redundancy and voting.
 * 
 * INDUSTRY RELEVANCE:
 * - Commercial Aviation: Boeing, Airbus flight control systems require
 *   DO-178C compliant sensor interfaces with strict timing guarantees
 * - General Aviation: Garmin, Honeywell avionics systems for smaller aircraft
 * - Urban Air Mobility: eVTOL vehicles (Joby, Archer) need lightweight
 *   yet reliable sensor fusion
 * - Military/Defense: Fighter jet and drone sensor systems
 * - Space Launch: SpaceX, Blue Origin vehicle instrumentation
 * 
 * COMPLIANCE:
 * - DO-178C: Software Considerations in Airborne Systems (DAL A-D)
 * - DO-254: Design Assurance for Airborne Electronic Hardware
 * - ARINC 429: Digital Information Transfer System
 * - MIL-STD-1553: Military Standard Data Bus
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_AVIONICS_SENSOR_H
#define GF_AVIONICS_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum number of redundant sensor channels */
#define GF_AVIONICS_MAX_CHANNELS        4

/** Maximum number of sensor types */
#define GF_AVIONICS_MAX_SENSOR_TYPES    16

/** Sensor update rate (Hz) */
#define GF_AVIONICS_SAMPLE_RATE_HZ      100

/** ARINC 429 word size (bits) */
#define GF_ARINC429_WORD_BITS           32

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Sensor type enumeration
 */
typedef enum {
    GF_SENSOR_PITOT_STATIC,       /**< Pitot-static (airspeed/altitude) */
    GF_SENSOR_ATTITUDE,           /**< AHRS attitude (roll/pitch/yaw) */
    GF_SENSOR_HEADING,            /**< Magnetometer heading */
    GF_SENSOR_GPS,                /**< GPS position/velocity */
    GF_SENSOR_RADAR_ALT,          /**< Radar altimeter */
    GF_SENSOR_AOA,                /**< Angle of attack */
    GF_SENSOR_SIDESLIP,           /**< Sideslip angle */
    GF_SENSOR_ENGINE,             /**< Engine parameters */
    GF_SENSOR_FUEL,               /**< Fuel quantity/flow */
    GF_SENSOR_TEMP_EXT,           /**< External temperature */
    GF_SENSOR_ICE_DETECT,         /**< Ice detection */
    GF_SENSOR_STALL_WARN          /**< Stall warning */
} gf_avionics_sensor_type_t;

/**
 * @brief Sensor data quality/status
 */
typedef enum {
    GF_SENSOR_QUALITY_GOOD,       /**< Valid data */
    GF_SENSOR_QUALITY_DEGRADED,   /**< Reduced accuracy */
    GF_SENSOR_QUALITY_FAILED,     /**< Sensor failure */
    GF_SENSOR_QUALITY_NCD         /**< No computed data */
} gf_sensor_quality_t;

/**
 * @brief ARINC 429 SSM (Sign/Status Matrix)
 */
typedef enum {
    GF_ARINC_SSM_FAILURE = 0,     /**< Failure warning */
    GF_ARINC_SSM_NCD = 1,         /**< No computed data */
    GF_ARINC_SSM_TEST = 2,        /**< Functional test */
    GF_ARINC_SSM_NORMAL = 3       /**< Normal operation */
} gf_arinc_ssm_t;

/**
 * @brief Single sensor reading
 */
typedef struct {
    gf_avionics_sensor_type_t type;
    uint8_t channel;              /**< Redundant channel (0-3) */
    float value;                  /**< Primary value */
    float secondary;              /**< Secondary value (if applicable) */
    float tertiary;               /**< Tertiary value (if applicable) */
    gf_sensor_quality_t quality;  /**< Data quality */
    uint32_t timestamp_ms;        /**< Sample timestamp */
    uint16_t sequence;            /**< Sequence number */
    bool valid;                   /**< Data validity flag */
} gf_sensor_reading_t;

/**
 * @brief Voted/fused sensor output
 */
typedef struct {
    gf_avionics_sensor_type_t type;
    float voted_value;            /**< Voted/fused value */
    float confidence;             /**< Confidence (0.0-1.0) */
    uint8_t channels_valid;       /**< Number of valid channels */
    uint8_t channels_total;       /**< Total channels available */
    bool fault_detected;          /**< Cross-channel fault */
    uint8_t failed_channel;       /**< Which channel failed (if any) */
    gf_arinc_ssm_t ssm;           /**< ARINC status */
} gf_sensor_voted_t;

/**
 * @brief Air data computer output
 */
typedef struct {
    float indicated_airspeed_kts; /**< IAS in knots */
    float true_airspeed_kts;      /**< TAS in knots */
    float mach_number;            /**< Mach number */
    float pressure_altitude_ft;   /**< Pressure altitude */
    float density_altitude_ft;    /**< Density altitude */
    float vertical_speed_fpm;     /**< Vertical speed (ft/min) */
    float static_air_temp_c;      /**< SAT in Celsius */
    float total_air_temp_c;       /**< TAT in Celsius */
    gf_arinc_ssm_t ssm;           /**< Status */
} gf_air_data_t;

/**
 * @brief Attitude heading reference output
 */
typedef struct {
    float roll_deg;               /**< Roll angle (degrees) */
    float pitch_deg;              /**< Pitch angle (degrees) */
    float heading_deg;            /**< True heading (degrees) */
    float roll_rate_dps;          /**< Roll rate (deg/sec) */
    float pitch_rate_dps;         /**< Pitch rate (deg/sec) */
    float yaw_rate_dps;           /**< Yaw rate (deg/sec) */
    bool attitude_valid;          /**< Attitude data valid */
    bool heading_valid;           /**< Heading data valid */
    gf_arinc_ssm_t ssm;           /**< Status */
} gf_ahrs_data_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    gf_avionics_sensor_type_t type;
    uint8_t channel_count;        /**< Number of redundant channels */
    uint32_t sample_period_ms;    /**< Sample period */
    float vote_threshold;         /**< Cross-channel vote threshold */
    bool enable_ssm;              /**< Enable ARINC SSM */
} gf_sensor_config_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize avionics sensor subsystem
 * @return 0 on success, error code otherwise
 */
int gf_avionics_sensor_init(void);

/**
 * @brief Shutdown avionics sensor subsystem
 */
void gf_avionics_sensor_shutdown(void);

/**
 * @brief Configure a sensor type
 * @param config Sensor configuration
 * @return 0 on success
 */
int gf_avionics_sensor_configure(const gf_sensor_config_t* config);

/**
 * @brief Read raw sensor data from specific channel
 * @param type Sensor type
 * @param channel Channel number
 * @param reading Output reading
 * @return 0 on success
 */
int gf_avionics_sensor_read(gf_avionics_sensor_type_t type, 
                             uint8_t channel,
                             gf_sensor_reading_t* reading);

/**
 * @brief Get voted/fused sensor value
 * @param type Sensor type
 * @param voted Output voted value
 * @return 0 on success
 */
int gf_avionics_sensor_get_voted(gf_avionics_sensor_type_t type,
                                  gf_sensor_voted_t* voted);

/**
 * @brief Get air data computer output
 * @param data Output air data
 * @return 0 on success
 */
int gf_avionics_get_air_data(gf_air_data_t* data);

/**
 * @brief Get AHRS output
 * @param data Output AHRS data
 * @return 0 on success
 */
int gf_avionics_get_ahrs(gf_ahrs_data_t* data);

/**
 * @brief Inject sensor value (for testing/simulation)
 * @param type Sensor type
 * @param channel Channel
 * @param value Value to inject
 * @param quality Quality status
 */
void gf_avionics_sensor_inject(gf_avionics_sensor_type_t type,
                                uint8_t channel,
                                float value,
                                gf_sensor_quality_t quality);

/**
 * @brief Process sensor subsystem (call periodically)
 * @param delta_ms Time since last call
 */
void gf_avionics_sensor_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_AVIONICS_SENSOR_H */
