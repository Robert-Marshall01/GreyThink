/**
 * @file thermal_sensor.h
 * @brief Thermal Imaging Sensor Driver for Smart Firefighting Systems
 * 
 * INDUSTRY RELEVANCE:
 * Fire detection and suppression systems protect lives and $60+ billion in
 * assets annually. Thermal imaging enables:
 * - Early fire detection via temperature anomaly sensing
 * - Firefighter situational awareness through smoke
 * - Hot spot identification for targeted suppression
 * - Post-fire verification and overhaul assistance
 * 
 * Target applications: Industrial fire detection, wildfire monitoring, building
 * fire systems, firefighter helmets, drone-based fire surveillance, data centers.
 * 
 * Standards: NFPA 72 (fire alarm), EN 54 (fire detection), UL 268 (smoke detection)
 */

#ifndef GF_THERMAL_SENSOR_H
#define GF_THERMAL_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Thermal Sensor Types                                                       */
/*===========================================================================*/

typedef enum {
    THERMAL_RES_16X16,      /* 256 pixels (low-power) */
    THERMAL_RES_32X32,      /* 1024 pixels (standard) */
    THERMAL_RES_64X64,      /* 4096 pixels (high-res) */
    THERMAL_RES_80X60,      /* 4800 pixels (FLIR Lepton) */
    THERMAL_RES_160X120     /* 19200 pixels (high-end) */
} thermal_resolution_t;

typedef enum {
    THERMAL_RANGE_LOW,      /* -40°C to +85°C (general) */
    THERMAL_RANGE_MED,      /* -20°C to +300°C (industrial) */
    THERMAL_RANGE_HIGH,     /* 0°C to +650°C (fire detection) */
    THERMAL_RANGE_EXTREME   /* 0°C to +1500°C (metallurgy) */
} thermal_range_t;

typedef struct {
    uint16_t width;         /* Frame width in pixels */
    uint16_t height;        /* Frame height in pixels */
    float* temperatures;    /* Temperature array (°C) */
    uint32_t timestamp;     /* Capture timestamp */
    float min_temp;         /* Minimum in frame */
    float max_temp;         /* Maximum in frame */
    float avg_temp;         /* Average temperature */
} thermal_frame_t;

typedef struct {
    uint16_t x;             /* Hot spot X coordinate */
    uint16_t y;             /* Hot spot Y coordinate */
    float temperature;      /* Hot spot temperature °C */
    float area_pixels;      /* Hot spot area in pixels */
    float gradient;         /* Temperature gradient °C/s */
    bool is_growing;        /* Hot spot expanding */
} hot_spot_t;

typedef struct {
    thermal_resolution_t resolution;
    thermal_range_t range;
    float emissivity;       /* Target emissivity (0.0-1.0) */
    float alarm_threshold;  /* Temperature alarm threshold °C */
    float gradient_threshold; /* Rate-of-rise threshold °C/s */
    uint32_t frame_rate_hz; /* Frame rate target */
} thermal_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int thermal_sensor_init(const thermal_config_t* config);
int thermal_sensor_shutdown(void);

int thermal_capture_frame(thermal_frame_t* frame);
int thermal_get_pixel(uint16_t x, uint16_t y, float* temp);

int thermal_detect_hot_spots(hot_spot_t* spots, uint8_t max_spots, 
                            uint8_t* count);
int thermal_get_max_temp(float* max_temp, uint16_t* x, uint16_t* y);

int thermal_set_alarm_threshold(float threshold_c);
int thermal_set_emissivity(float emissivity);

int thermal_calibrate_nuc(void);  /* Non-uniformity correction */
int thermal_calibrate_ffc(void);  /* Flat-field correction */

bool thermal_is_alarm_active(void);

#endif /* GF_THERMAL_SENSOR_H */
