/**
 * @file scooter_control.h
 * @brief Autonomous Scooter Control Interface
 * 
 * INDUSTRY RELEVANCE:
 * Electric scooters and micro-mobility vehicles represent a major urban
 * transportation segment. Companies like Bird, Lime, Spin, and Tier require
 * sophisticated firmware for motor control, battery management, geofencing,
 * and fleet telemetry. Autonomous features like self-parking and obstacle
 * avoidance are emerging differentiators.
 * 
 * This module provides motor control, navigation, and safety interfaces
 * for autonomous and semi-autonomous electric scooters.
 * 
 * KEY CAPABILITIES:
 * - BLDC motor control (FOC)
 * - Speed limiting and governing
 * - Regenerative braking
 * - Geofencing (no-ride zones, slow zones)
 * - Direction sensing and turn signals
 * - Kick-to-start detection
 * - Tilt/fall detection
 * - Remote lock/unlock
 * 
 * STANDARDS COMPLIANCE:
 * - UL 2272 (E-mobility battery safety)
 * - EN 15194 (Electric cycles)
 * - Local speed regulations
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_SCOOTER_CONTROL_H
#define GF_SCOOTER_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define SC_MAX_SPEED_KMH       25    /**< Max speed (region-dependent) */
#define SC_SLOW_ZONE_KMH       8     /**< Slow zone limit */
#define SC_MAX_GEOFENCES       16    /**< Max geofence regions */
#define SC_REGEN_MAX_PCT       30    /**< Max regenerative braking % */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Scooter state */
typedef enum {
    SC_STATE_OFF,
    SC_STATE_LOCKED,
    SC_STATE_UNLOCKED,
    SC_STATE_RIDING,
    SC_STATE_SLOW_ZONE,
    SC_STATE_NO_RIDE_ZONE,
    SC_STATE_LOW_BATTERY,
    SC_STATE_MAINTENANCE,
    SC_STATE_FAULT
} sc_state_t;

/** Motor control mode */
typedef enum {
    SC_MODE_OFF,
    SC_MODE_COAST,
    SC_MODE_POWER,
    SC_MODE_REGEN,
    SC_MODE_BRAKE
} sc_motor_mode_t;

/** Geofence type */
typedef enum {
    SC_ZONE_NORMAL,
    SC_ZONE_SLOW,
    SC_ZONE_NO_RIDE,
    SC_ZONE_NO_PARK,
    SC_ZONE_PREFERRED_PARK
} sc_zone_type_t;

/** Scooter status */
typedef struct {
    sc_state_t state;
    float speed_kmh;
    float throttle_pct;
    float battery_pct;
    float motor_temp_c;
    float controller_temp_c;
    float odometer_km;
    float trip_km;
    bool kickstand_down;
    bool rider_present;
} sc_status_t;

/** Geofence definition */
typedef struct {
    uint8_t zone_id;
    sc_zone_type_t type;
    double center_lat;
    double center_lon;
    float radius_m;
    float speed_limit_kmh;
} sc_geofence_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize scooter control
 * @param scooter_id Fleet identifier
 * @return 0 on success
 */
int sc_init(uint32_t scooter_id);

/**
 * @brief Unlock scooter for rental
 * @param rider_token Authentication token
 * @return 0 on success
 */
int sc_unlock(const char* rider_token);

/**
 * @brief Lock scooter (end rental)
 * @return 0 on success
 */
int sc_lock(void);

/**
 * @brief Set throttle input
 * @param throttle_pct Throttle 0-100%
 * @return 0 on success
 */
int sc_set_throttle(float throttle_pct);

/**
 * @brief Apply brake
 * @param brake_pct Brake force 0-100%
 * @return 0 on success
 */
int sc_apply_brake(float brake_pct);

/**
 * @brief Get scooter status
 * @param status Output status
 * @return 0 on success
 */
int sc_get_status(sc_status_t* status);

/**
 * @brief Update geofence
 * @param fence Geofence definition
 * @return 0 on success
 */
int sc_update_geofence(const sc_geofence_t* fence);

/**
 * @brief Process control loop
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int sc_process(uint32_t delta_ms);

/**
 * @brief Emergency stop
 * @return 0 on success
 */
int sc_emergency_stop(void);

/**
 * @brief Shutdown scooter control
 */
void sc_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SCOOTER_CONTROL_H */
