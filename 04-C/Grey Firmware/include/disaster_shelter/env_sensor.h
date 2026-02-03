/**
 * @file env_sensor.h
 * @brief Environmental Sensor Interface for Disaster Shelters
 *
 * INDUSTRY RELEVANCE:
 * Emergency shelters must maintain life-safety conditions during disasters.
 * This module provides sensing capabilities for FEMA-compliant shelters,
 * storm rooms, and civil defense bunkers - critical for government
 * contracts and emergency preparedness manufacturers.
 *
 * Key capabilities demonstrated:
 * - Multi-gas detection (CO, CO2, O2, toxic gases)
 * - Pressure differential monitoring (blast/NBC)
 * - Temperature and humidity tracking
 * - Battery-backed operation for grid outages
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_SHELTER_ENV_SENSOR_H
#define GF_SHELTER_ENV_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Shelter environment parameters */
typedef enum {
    GF_SHELTER_TEMP,                /**< Temperature °C */
    GF_SHELTER_HUMIDITY,            /**< Relative humidity % */
    GF_SHELTER_CO2_PPM,             /**< CO2 concentration */
    GF_SHELTER_CO_PPM,              /**< Carbon monoxide */
    GF_SHELTER_O2_PCT,              /**< Oxygen percentage */
    GF_SHELTER_PRESSURE_PA,         /**< Atmospheric pressure */
    GF_SHELTER_PRESSURE_DIFF,       /**< Differential pressure */
    GF_SHELTER_RADIATION            /**< Radiation level μSv/h */
} gf_shelter_param_t;

/** Environment reading with alert thresholds */
typedef struct {
    gf_shelter_param_t param;
    float value;
    float alert_low;
    float alert_high;
    bool in_alarm;
} gf_shelter_reading_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_shelter_env_init(void);
int gf_shelter_env_read(gf_shelter_param_t param, gf_shelter_reading_t* reading);
int gf_shelter_env_set_alarm(gf_shelter_param_t param, float low, float high);
bool gf_shelter_env_is_safe(void);
void gf_shelter_env_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SHELTER_ENV_SENSOR_H */
