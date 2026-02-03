/**
 * @file track_sensor.h
 * @brief Track Sensor Telemetry for Smart Rail Systems
 * 
 * @details
 * Comprehensive track-side sensor interface for rail infrastructure
 * monitoring. Collects telemetry from track circuits, axle counters,
 * wheel detectors, and environmental sensors.
 * 
 * INDUSTRY RELEVANCE:
 * - Track circuit monitoring (Alstom, Thales)
 * - Point machine status
 * - Hot axle/wheel detectors (WILD)
 * - Rail temperature monitoring
 * - Track geometry measurement
 * 
 * KEY FEATURES:
 * - Multi-sensor fusion
 * - Train detection and counting
 * - Track occupancy status
 * - Rail break detection
 * - Weather condition monitoring
 * - Predictive maintenance data
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_TRACK_SENSOR_H
#define GF_TRACK_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum track sections */
#define GF_TRACK_MAX_SECTIONS       128

/** Maximum sensors per section */
#define GF_TRACK_MAX_SENSORS        8

/** Maximum temperature warning (°C) */
#define GF_TRACK_TEMP_WARNING_C     50

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Track sensor status codes
 */
typedef enum {
    GF_TRACK_OK = 0,
    GF_TRACK_ERROR_NOT_INIT,
    GF_TRACK_ERROR_NULL_PTR,
    GF_TRACK_ERROR_INVALID_SECTION,
    GF_TRACK_ERROR_COMM_FAULT,
    GF_TRACK_ERROR_SENSOR_FAULT,
    GF_TRACK_WARN_DEGRADED,
    GF_TRACK_WARN_MAINTENANCE
} gf_track_status_t;

/**
 * @brief Sensor types
 */
typedef enum {
    GF_TRACK_SENSOR_CIRCUIT,      /**< Track circuit */
    GF_TRACK_SENSOR_AXLE_COUNTER, /**< Axle counter */
    GF_TRACK_SENSOR_WHEEL_DETECT, /**< Wheel detector */
    GF_TRACK_SENSOR_HOT_BOX,      /**< Hot axle detector */
    GF_TRACK_SENSOR_RAIL_TEMP,    /**< Rail temperature */
    GF_TRACK_SENSOR_STRAIN,       /**< Rail strain gauge */
    GF_TRACK_SENSOR_ACOUSTIC,     /**< Acoustic emission */
    GF_TRACK_SENSOR_WEATHER       /**< Weather station */
} gf_track_sensor_type_t;

/**
 * @brief Track section occupancy
 */
typedef enum {
    GF_TRACK_CLEAR,               /**< Section clear */
    GF_TRACK_OCCUPIED,            /**< Train in section */
    GF_TRACK_UNKNOWN,             /**< Status unknown */
    GF_TRACK_BLOCKED              /**< Section blocked */
} gf_track_occupancy_t;

/**
 * @brief Point (switch) state
 */
typedef enum {
    GF_TRACK_POINT_NORMAL,        /**< Normal position */
    GF_TRACK_POINT_REVERSE,       /**< Reverse position */
    GF_TRACK_POINT_MOVING,        /**< In transit */
    GF_TRACK_POINT_FAULT          /**< Position fault */
} gf_track_point_state_t;

/**
 * @brief Track section info
 */
typedef struct {
    uint16_t section_id;          /**< Section identifier */
    gf_track_occupancy_t occupancy; /**< Occupancy status */
    uint32_t axle_count;          /**< Axle count */
    int16_t rail_temp_c10;        /**< Rail temperature (0.1°C) */
    bool rail_break_detected;     /**< Rail break detected */
    uint32_t last_train_time;     /**< Last train passage */
} gf_track_section_info_t;

/**
 * @brief Hot axle detector reading
 */
typedef struct {
    uint32_t timestamp;           /**< Detection time */
    uint16_t bearing_temp_c;      /**< Bearing temperature */
    uint16_t wheel_temp_c;        /**< Wheel temperature */
    uint8_t axle_number;          /**< Axle number in consist */
    bool alarm;                   /**< Temperature alarm */
} gf_track_hotbox_reading_t;

/**
 * @brief Weather conditions
 */
typedef struct {
    int16_t ambient_temp_c10;     /**< Ambient temperature (0.1°C) */
    uint16_t humidity_permille;   /**< Relative humidity (0.1%) */
    uint16_t wind_speed_mm_s;     /**< Wind speed (mm/s) */
    uint16_t wind_direction_deg;  /**< Wind direction (degrees) */
    uint16_t precipitation_mm_h;  /**< Precipitation (mm/hour) */
    bool ice_warning;             /**< Ice formation warning */
} gf_track_weather_t;

/**
 * @brief Sensor event callback
 */
typedef void (*gf_track_event_cb_t)(uint16_t section_id,
                                     gf_track_sensor_type_t sensor,
                                     const void* data, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize track sensor system
 * @return Status code
 */
gf_track_status_t gf_track_init(void);

/**
 * @brief Shutdown track sensor system
 */
void gf_track_shutdown(void);

/**
 * @brief Get section info
 * @param section_id Section identifier
 * @param info Output section info
 * @return Status code
 */
gf_track_status_t gf_track_get_section(uint16_t section_id,
                                        gf_track_section_info_t* info);

/**
 * @brief Get occupancy status
 * @param section_id Section identifier
 * @return Occupancy status
 */
gf_track_occupancy_t gf_track_get_occupancy(uint16_t section_id);

/**
 * @brief Get point state
 * @param point_id Point identifier
 * @return Point state
 */
gf_track_point_state_t gf_track_get_point_state(uint16_t point_id);

/**
 * @brief Get weather conditions
 * @param station_id Weather station ID
 * @param weather Output weather data
 * @return Status code
 */
gf_track_status_t gf_track_get_weather(uint8_t station_id,
                                        gf_track_weather_t* weather);

/**
 * @brief Get hot axle reading
 * @param detector_id Detector ID
 * @param reading Output reading
 * @return Status code
 */
gf_track_status_t gf_track_get_hotbox(uint16_t detector_id,
                                       gf_track_hotbox_reading_t* reading);

/**
 * @brief Register event callback
 * @param callback Event callback
 * @param user_data User context
 * @return Status code
 */
gf_track_status_t gf_track_register_callback(gf_track_event_cb_t callback,
                                              void* user_data);

/**
 * @brief Process sensor readings
 * @return Status code
 */
gf_track_status_t gf_track_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TRACK_SENSOR_H */
