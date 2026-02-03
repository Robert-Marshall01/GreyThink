/**
 * @file power_airflow_control.h
 * @brief Power & Airflow Control for Disaster Shelters
 *
 * INDUSTRY RELEVANCE:
 * Life support in enclosed shelters requires precise control of ventilation
 * and power management. This module demonstrates embedded expertise in HVAC
 * control, NBC filtration systems, and UPS management for emergency
 * preparedness applications.
 *
 * Key capabilities demonstrated:
 * - HEPA/NBC filter control and monitoring
 * - Positive pressure maintenance
 * - Battery/generator switchover logic
 * - Load shedding for extended operation
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_SHELTER_POWER_AIRFLOW_H
#define GF_SHELTER_POWER_AIRFLOW_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Power source types */
typedef enum {
    GF_POWER_GRID,
    GF_POWER_BATTERY,
    GF_POWER_GENERATOR,
    GF_POWER_SOLAR
} gf_power_source_t;

/** Ventilation mode */
typedef enum {
    GF_VENT_MODE_NORMAL,            /**< Fresh air intake */
    GF_VENT_MODE_FILTERED,          /**< HEPA filtered only */
    GF_VENT_MODE_NBC,               /**< NBC filtration */
    GF_VENT_MODE_SEALED             /**< Complete seal */
} gf_vent_mode_t;

/** System status */
typedef struct {
    gf_power_source_t power_source;
    float battery_pct;
    float battery_hours_remaining;
    gf_vent_mode_t vent_mode;
    float airflow_cfm;
    float filter_life_pct;
    bool positive_pressure_ok;
} gf_shelter_status_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_shelter_power_init(void);
int gf_shelter_set_vent_mode(gf_vent_mode_t mode);
int gf_shelter_set_airflow(float cfm);
int gf_shelter_get_status(gf_shelter_status_t* status);
int gf_shelter_load_shed(uint8_t priority_level);
void gf_shelter_power_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SHELTER_POWER_AIRFLOW_H */
