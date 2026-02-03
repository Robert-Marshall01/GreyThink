/**
 * @file crop_spraying.h
 * @brief Precision Crop Spraying Actuator Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Agricultural drones represent a $5B+ market for precision farming.
 * Crop spraying systems require:
 * - Variable-rate application based on crop health maps
 * - GPS-synchronized spray patterns for field coverage
 * - Real-time flow control and nozzle management
 * - Drift prevention in varying wind conditions
 * - Chemical tank level monitoring and refill alerts
 * 
 * This stub demonstrates embedded systems expertise for:
 * - DJI Agras, XAG, and other agricultural drone platforms
 * - John Deere, AGCO, and precision agriculture OEMs
 * - AgTech startups building autonomous farming solutions
 * 
 * STANDARDS COMPLIANCE:
 * - ISO 22077 (Agricultural sprayers)
 * - ASABE S561 (Agricultural spray equipment)
 * - EPA regulations for pesticide application
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_AGRICULTURE_DRONES_CROP_SPRAYING_H
#define GF_AGRICULTURE_DRONES_CROP_SPRAYING_H

#include <stdint.h>
#include <stdbool.h>

/* Nozzle configuration */
#define SPRAY_MAX_NOZZLES       8
#define SPRAY_MAX_ZONES         4

/** Nozzle type */
typedef enum {
    NOZZLE_FLAT_FAN,
    NOZZLE_HOLLOW_CONE,
    NOZZLE_FULL_CONE,
    NOZZLE_AIR_INDUCTION,
    NOZZLE_TWIN_FAN
} nozzle_type_t;

/** Spray mode */
typedef enum {
    SPRAY_MODE_OFF,
    SPRAY_MODE_MANUAL,
    SPRAY_MODE_AUTO_GPS,
    SPRAY_MODE_VARIABLE_RATE,
    SPRAY_MODE_SPOT_SPRAY
} spray_mode_t;

/** Nozzle status */
typedef struct {
    uint8_t nozzle_id;
    nozzle_type_t type;
    bool enabled;
    float flow_rate_lpm;        /**< Liters per minute */
    float pressure_bar;
    float droplet_size_um;      /**< VMD in microns */
    uint32_t spray_time_s;      /**< Total spray time */
    bool clogged;
} nozzle_status_t;

/** System status */
typedef struct {
    spray_mode_t mode;
    float tank_level_pct;
    float tank_volume_l;
    float total_applied_l;
    float application_rate_lha; /**< L/hectare */
    float coverage_area_ha;
    float wind_speed_ms;
    float wind_direction_deg;
    bool drift_warning;
    nozzle_status_t nozzles[SPRAY_MAX_NOZZLES];
} spray_status_t;

/* API Functions */
int spray_init(void);
int spray_set_mode(spray_mode_t mode);
int spray_set_rate(float liters_per_hectare);
int spray_enable_nozzle(uint8_t nozzle_id, bool enable);
int spray_set_pressure(float bar);
int spray_get_status(spray_status_t *status);
int spray_emergency_stop(void);
float spray_get_remaining_area(void);

#endif /* GF_AGRICULTURE_DRONES_CROP_SPRAYING_H */
