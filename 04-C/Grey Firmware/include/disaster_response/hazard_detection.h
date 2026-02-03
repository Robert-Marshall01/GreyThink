/**
 * @file hazard_detection.h
 * @brief Multi-Sensor Fusion for Hazard Detection
 * 
 * INDUSTRY RELEVANCE:
 * Disaster response systems use sensor fusion for:
 * - Earthquake early warning (seismometers)
 * - Fire detection (smoke, thermal, gas)
 * - Flood monitoring (water level, flow)
 * - Structural health monitoring
 * - Radiation detection
 * - Chemical/biological agents
 * 
 * Companies: Honeywell, Siemens, Johnson Controls
 */

#ifndef GF_HAZARD_DETECTION_H
#define GF_HAZARD_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_HAZARD_MAX_SENSORS       32
#define GF_HAZARD_MAX_ZONES         16
#define GF_HAZARD_HISTORY_SAMPLES   1000

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_HAZARD_OK = 0,
    GF_HAZARD_ERROR_NULL_PTR,
    GF_HAZARD_ERROR_NOT_INITIALIZED,
    GF_HAZARD_ERROR_SENSOR_FAULT,
    GF_HAZARD_ERROR_CALIBRATION,
    GF_HAZARD_ERROR_COMM_FAIL,
    GF_HAZARD_ERROR_ZONE_FULL,
    GF_HAZARD_WARN_SENSOR_DEGRADED,
    GF_HAZARD_WARN_LOW_BATTERY
} gf_hazard_status_t;

typedef enum {
    GF_HAZARD_NONE,
    GF_HAZARD_FIRE,
    GF_HAZARD_SMOKE,
    GF_HAZARD_GAS_LEAK,
    GF_HAZARD_FLOOD,
    GF_HAZARD_EARTHQUAKE,
    GF_HAZARD_STRUCTURAL,
    GF_HAZARD_RADIATION,
    GF_HAZARD_CHEMICAL,
    GF_HAZARD_BIOLOGICAL,
    GF_HAZARD_INTRUSION,
    GF_HAZARD_POWER_FAIL
} gf_hazard_type_t;

typedef enum {
    GF_SEVERITY_NORMAL,
    GF_SEVERITY_ADVISORY,
    GF_SEVERITY_WARNING,
    GF_SEVERITY_CRITICAL,
    GF_SEVERITY_EMERGENCY
} gf_hazard_severity_t;

typedef enum {
    GF_SENSOR_SMOKE_PHOTO,      /* Photoelectric smoke */
    GF_SENSOR_SMOKE_ION,        /* Ionization smoke */
    GF_SENSOR_THERMAL,          /* Heat detector */
    GF_SENSOR_IR_FLAME,         /* IR flame detector */
    GF_SENSOR_GAS_CO,           /* Carbon monoxide */
    GF_SENSOR_GAS_CO2,          /* Carbon dioxide */
    GF_SENSOR_GAS_METHANE,      /* Combustible gas */
    GF_SENSOR_GAS_H2S,          /* Hydrogen sulfide */
    GF_SENSOR_WATER_LEVEL,      /* Flood sensor */
    GF_SENSOR_FLOW,             /* Water flow */
    GF_SENSOR_SEISMIC,          /* Accelerometer */
    GF_SENSOR_STRAIN,           /* Structural strain */
    GF_SENSOR_TILT,             /* Inclinometer */
    GF_SENSOR_RADIATION,        /* Geiger counter */
    GF_SENSOR_PIR               /* Passive infrared */
} gf_hazard_sensor_type_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    uint8_t sensor_id;
    gf_hazard_sensor_type_t type;
    char location[32];
    uint8_t zone_id;
    float threshold_warning;
    float threshold_critical;
    float threshold_emergency;
    uint16_t sample_rate_hz;
    bool enabled;
} gf_hazard_sensor_config_t;

/**
 * @brief Zone definition
 */
typedef struct {
    uint8_t zone_id;
    char name[32];
    uint8_t priority;
    uint16_t evacuation_time_sec;
    bool cross_zone_enabled;
    uint8_t linked_zones[4];
} gf_hazard_zone_t;

/**
 * @brief Sensor reading
 */
typedef struct {
    uint8_t sensor_id;
    gf_hazard_sensor_type_t type;
    float value;
    float value_filtered;
    float rate_of_change;
    gf_hazard_severity_t severity;
    uint64_t timestamp_ms;
    bool fault;
} gf_hazard_reading_t;

/**
 * @brief Fused hazard assessment
 */
typedef struct {
    gf_hazard_type_t hazard;
    gf_hazard_severity_t severity;
    uint8_t zone_id;
    float confidence;           /* 0-1 */
    uint8_t contributing_sensors;
    uint64_t detection_time_ms;
    bool confirmed;
    float estimated_size;       /* Fire: sq meters, Flood: liters */
} gf_hazard_assessment_t;

/**
 * @brief Alarm output
 */
typedef struct {
    uint8_t zone_id;
    gf_hazard_type_t hazard;
    gf_hazard_severity_t severity;
    bool audible_alarm;
    bool visual_alarm;
    bool evacuation_order;
    char message[64];
} gf_hazard_alarm_t;

/**
 * @brief Event log entry
 */
typedef struct {
    uint64_t timestamp_ms;
    uint8_t zone_id;
    gf_hazard_type_t hazard;
    gf_hazard_severity_t severity;
    char description[64];
    bool acknowledged;
    uint8_t acknowledged_by;
} gf_hazard_event_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_hazard_alarm_cb_t)(const gf_hazard_alarm_t* alarm, void* user_data);

typedef void (*gf_hazard_clear_cb_t)(uint8_t zone_id, gf_hazard_type_t hazard, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_hazard_status_t gf_hazard_init(void);
void gf_hazard_shutdown(void);

/* Configuration */
gf_hazard_status_t gf_hazard_add_sensor(const gf_hazard_sensor_config_t* config);
gf_hazard_status_t gf_hazard_remove_sensor(uint8_t sensor_id);
gf_hazard_status_t gf_hazard_add_zone(const gf_hazard_zone_t* zone);
gf_hazard_status_t gf_hazard_calibrate_sensor(uint8_t sensor_id);

/* Sensor data */
gf_hazard_status_t gf_hazard_update_reading(uint8_t sensor_id, float value);
gf_hazard_status_t gf_hazard_get_reading(uint8_t sensor_id, gf_hazard_reading_t* reading);

/* Assessment */
gf_hazard_status_t gf_hazard_get_assessment(uint8_t zone_id, gf_hazard_assessment_t* assessment);
gf_hazard_status_t gf_hazard_get_all_active(gf_hazard_assessment_t* assessments,
                                             uint8_t max_count,
                                             uint8_t* actual_count);

/* Alarm control */
gf_hazard_status_t gf_hazard_acknowledge(uint8_t zone_id, gf_hazard_type_t hazard);
gf_hazard_status_t gf_hazard_silence_alarm(uint8_t zone_id);
gf_hazard_status_t gf_hazard_reset_zone(uint8_t zone_id);
gf_hazard_status_t gf_hazard_test_alarm(uint8_t zone_id);

/* Event log */
gf_hazard_status_t gf_hazard_get_events(uint64_t start_time,
                                         uint64_t end_time,
                                         gf_hazard_event_t* events,
                                         uint32_t max_events,
                                         uint32_t* actual_count);

/* Callbacks */
gf_hazard_status_t gf_hazard_register_alarm_callback(gf_hazard_alarm_cb_t cb, void* user_data);
gf_hazard_status_t gf_hazard_register_clear_callback(gf_hazard_clear_cb_t cb, void* user_data);

/* Periodic processing */
gf_hazard_status_t gf_hazard_process(void);

#endif /* GF_HAZARD_DETECTION_H */
