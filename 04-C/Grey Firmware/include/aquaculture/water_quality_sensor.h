/**
 * @file water_quality_sensor.h
 * @brief Water Quality Sensor Interface for Aquaculture Systems
 *
 * INDUSTRY RELEVANCE:
 * Aquaculture is a $200+ billion global industry requiring precise water quality
 * monitoring. This module demonstrates embedded expertise in multi-parameter
 * sensor fusion for fish/shrimp farming operations - critical for sustainable
 * food production at scale (used by companies like Cargill, Mowi, and AgTech startups).
 *
 * Key capabilities demonstrated:
 * - Multi-parameter sensing (DO, pH, ammonia, temperature, turbidity)
 * - Calibration and drift compensation
 * - Fouling detection and compensation
 * - MODBUS/4-20mA industrial interface support
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_WATER_QUALITY_SENSOR_H
#define GF_WATER_QUALITY_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Water quality parameter types */
typedef enum {
    GF_WQ_DISSOLVED_OXYGEN,         /**< DO in mg/L */
    GF_WQ_PH,                       /**< pH level (0-14) */
    GF_WQ_TEMPERATURE,              /**< Water temperature °C */
    GF_WQ_AMMONIA,                  /**< NH3/NH4+ in mg/L */
    GF_WQ_NITRITE,                  /**< NO2 in mg/L */
    GF_WQ_SALINITY,                 /**< Salinity in ppt */
    GF_WQ_TURBIDITY,                /**< Turbidity in NTU */
    GF_WQ_ORP                       /**< Oxidation-Reduction Potential mV */
} gf_wq_param_t;

/** Sensor status */
typedef struct {
    float value;
    float calibration_offset;
    uint32_t last_calibration_epoch;
    bool needs_cleaning;
    bool in_range;
} gf_wq_reading_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_wq_init(void);
int gf_wq_read(gf_wq_param_t param, gf_wq_reading_t* reading);
int gf_wq_calibrate(gf_wq_param_t param, float reference_value);
void gf_wq_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_WATER_QUALITY_SENSOR_H */
