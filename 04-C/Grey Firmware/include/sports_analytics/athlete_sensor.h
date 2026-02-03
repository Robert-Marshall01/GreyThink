/**
 * @file athlete_sensor.h
 * @brief Athlete Wearable Sensor Interface for Sports Analytics
 *
 * INDUSTRY RELEVANCE:
 * Professional and collegiate sports increasingly rely on wearable sensors
 * for performance optimization and injury prevention. This module demonstrates
 * embedded expertise in sports technology used by companies like Catapult,
 * WHOOP, and professional team performance systems.
 *
 * Key capabilities demonstrated:
 * - IMU-based motion tracking (100Hz+ sampling)
 * - Heart rate and HRV monitoring
 * - GPS/GNSS position tracking for outdoor sports
 * - Low-latency wireless data streaming
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_ATHLETE_SENSOR_H
#define GF_ATHLETE_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Sensor placement */
typedef enum {
    GF_SENSOR_CHEST,
    GF_SENSOR_WRIST,
    GF_SENSOR_ANKLE,
    GF_SENSOR_TORSO,
    GF_SENSOR_HEAD
} gf_sensor_placement_t;

/** Motion data (IMU) */
typedef struct {
    float accel_x, accel_y, accel_z;    /**< Acceleration m/s² */
    float gyro_x, gyro_y, gyro_z;        /**< Angular velocity rad/s */
    float mag_x, mag_y, mag_z;           /**< Magnetic field μT */
    uint64_t timestamp_us;
} gf_motion_data_t;

/** Physiological data */
typedef struct {
    uint16_t heart_rate_bpm;
    float hrv_rmssd_ms;
    float core_temp_c;
    float skin_temp_c;
    uint8_t spo2_pct;
    uint16_t respiration_rate;
} gf_physio_data_t;

/** Position data */
typedef struct {
    double latitude;
    double longitude;
    float altitude_m;
    float speed_mps;
    float heading_deg;
    uint8_t satellites;
    float hdop;
} gf_position_data_t;

/** Combined athlete reading */
typedef struct {
    uint32_t athlete_id;
    gf_sensor_placement_t placement;
    gf_motion_data_t motion;
    gf_physio_data_t physio;
    gf_position_data_t position;
    uint8_t battery_pct;
} gf_athlete_reading_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_athlete_sensor_init(uint32_t athlete_id, gf_sensor_placement_t placement);
int gf_athlete_sensor_read(gf_athlete_reading_t* reading);
int gf_athlete_sensor_stream_start(void (*callback)(const gf_athlete_reading_t*));
int gf_athlete_sensor_stream_stop(void);
void gf_athlete_sensor_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ATHLETE_SENSOR_H */
