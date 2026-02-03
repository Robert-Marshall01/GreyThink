/**
 * @file animal_tracker.h
 * @brief Wildlife Animal Tracking Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Wildlife monitoring is critical for conservation, biodiversity research,
 * and environmental protection. GPS collars, bio-loggers, and camera traps
 * are used by organizations like WWF, USGS, Movebank, and tech companies
 * building conservation technology. Firmware engineers enable low-power,
 * long-range tracking solutions in remote environments.
 * 
 * This module provides interfaces for GPS/GNSS tracking, accelerometers,
 * heart rate monitors, and proximity sensors used in wildlife collars
 * and bio-logging devices.
 * 
 * KEY CAPABILITIES:
 * - GPS/GNSS positioning (low-power modes)
 * - Accelerometer activity classification
 * - Heart rate / physiological monitoring
 * - Proximity detection (inter-animal)
 * - VHF beacon support
 * - Ultra-low-power sleep modes
 * - Geofencing and alert triggers
 * 
 * STANDARDS COMPLIANCE:
 * - Movebank data standards
 * - CITES tracking requirements
 * - ARGOS satellite compatibility
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ANIMAL_TRACKER_H
#define GF_ANIMAL_TRACKER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define AT_MAX_GEOFENCES       8     /**< Maximum geofence regions */
#define AT_FIX_TIMEOUT_S       120   /**< GPS fix timeout */
#define AT_PROXIMITY_RANGE_M   50    /**< Proximity detection range */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Tracking mode */
typedef enum {
    AT_MODE_OFF,
    AT_MODE_SLEEP,         /**< Ultra-low-power sleep */
    AT_MODE_SCHEDULED,     /**< Periodic fix acquisition */
    AT_MODE_CONTINUOUS,    /**< Continuous tracking */
    AT_MODE_BURST          /**< High-frequency burst */
} at_mode_t;

/** Activity classification */
typedef enum {
    AT_ACTIVITY_UNKNOWN,
    AT_ACTIVITY_RESTING,
    AT_ACTIVITY_WALKING,
    AT_ACTIVITY_RUNNING,
    AT_ACTIVITY_FEEDING,
    AT_ACTIVITY_FLYING
} at_activity_t;

/** GPS fix */
typedef struct {
    double latitude;
    double longitude;
    float altitude_m;
    float hdop;            /**< Horizontal dilution of precision */
    uint8_t satellites;
    uint32_t timestamp;
    bool valid;
} at_position_t;

/** Physiological data */
typedef struct {
    float heart_rate_bpm;
    float body_temp_c;
    float activity_level;  /**< 0.0-1.0 activity index */
    at_activity_t activity;
} at_physio_t;

/** Tracking record */
typedef struct {
    uint32_t record_id;
    at_position_t position;
    at_physio_t physio;
    float battery_pct;
    uint32_t uptime_s;
} at_record_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize animal tracker
 * @param device_id Unique collar ID
 * @return 0 on success
 */
int at_init(uint32_t device_id);

/**
 * @brief Set tracking mode
 * @param mode Operating mode
 * @param interval_s Fix interval for scheduled mode
 * @return 0 on success
 */
int at_set_mode(at_mode_t mode, uint32_t interval_s);

/**
 * @brief Get current position
 * @param position Output position
 * @return 0 on success
 */
int at_get_position(at_position_t* position);

/**
 * @brief Get physiological data
 * @param physio Output physiology
 * @return 0 on success
 */
int at_get_physiology(at_physio_t* physio);

/**
 * @brief Add geofence region
 * @param id Geofence ID
 * @param lat Center latitude
 * @param lon Center longitude
 * @param radius_m Radius in meters
 * @return 0 on success
 */
int at_add_geofence(uint8_t id, double lat, double lon, float radius_m);

/**
 * @brief Store tracking record
 * @param record Record to store
 * @return 0 on success
 */
int at_store_record(const at_record_t* record);

/**
 * @brief Shutdown tracker
 */
void at_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ANIMAL_TRACKER_H */
