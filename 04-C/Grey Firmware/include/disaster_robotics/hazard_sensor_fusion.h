/**
 * @file hazard_sensor_fusion.h
 * @brief Sensor Fusion for Hazard Detection in Disaster Robotics
 * 
 * INDUSTRY RELEVANCE:
 * Disaster response robots must detect multiple hazard types simultaneously
 * using heterogeneous sensor arrays. This module fuses thermal imaging,
 * gas detection, structural sensors, and victim detection into a unified
 * hazard assessment for autonomous navigation and operator alerts.
 * 
 * Applications:
 * - Urban search and rescue (USAR)
 * - HAZMAT incident response
 * - Wildfire assessment
 * - Industrial accident investigation
 * - Nuclear/radiation emergencies
 * 
 * Standards: NIST USAR Robot Standards, ASTM E2521 (Robot Standards)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_HAZARD_SENSOR_FUSION_H
#define GF_HAZARD_SENSOR_FUSION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define HAZARD_MAX_SENSORS          32      /* Maximum sensor inputs */
#define HAZARD_MAX_ZONES            16      /* Maximum monitored zones */
#define HAZARD_FUSION_RATE_HZ       20      /* Fusion update rate */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Hazard type classification */
typedef enum {
    HAZARD_NONE = 0,
    HAZARD_FIRE,                /* Active fire/thermal */
    HAZARD_SMOKE,               /* Smoke/visibility */
    HAZARD_TOXIC_GAS,           /* Toxic atmosphere */
    HAZARD_EXPLOSIVE_GAS,       /* Explosive atmosphere */
    HAZARD_RADIATION,           /* Ionizing radiation */
    HAZARD_STRUCTURAL,          /* Structural instability */
    HAZARD_ELECTRICAL,          /* Live electrical */
    HAZARD_FLOOD,               /* Water hazard */
    HAZARD_BIOHAZARD,           /* Biological contamination */
    HAZARD_ASPHYXIATION         /* Oxygen deficiency */
} hazard_type_t;

/** Sensor modality */
typedef enum {
    SENSOR_THERMAL_IMAGING,     /* FLIR/thermal camera */
    SENSOR_GAS_MULTI,           /* Multi-gas detector */
    SENSOR_RADIATION_DOSIMETER, /* Radiation sensor */
    SENSOR_LIDAR,               /* 3D structural mapping */
    SENSOR_AUDIO,               /* Victim audio detection */
    SENSOR_VIBRATION,           /* Seismic/structural */
    SENSOR_ELECTROMAGNETIC,     /* EMF/electrical detection */
    SENSOR_CAMERA_RGB,          /* Visual camera */
    SENSOR_HUMIDITY,            /* Moisture detection */
    SENSOR_CO2                  /* CO₂ concentration */
} sensor_modality_t;

/** Hazard severity */
typedef enum {
    SEVERITY_CLEAR,             /* No hazard */
    SEVERITY_CAUTION,           /* Proceed with care */
    SEVERITY_WARNING,           /* Avoid if possible */
    SEVERITY_DANGER,            /* Do not enter */
    SEVERITY_IMPASSABLE         /* Cannot traverse */
} hazard_severity_t;

/** Detected hazard */
typedef struct {
    hazard_type_t type;
    hazard_severity_t severity;
    float confidence;           /* 0.0 - 1.0 */
    float position_x_m;
    float position_y_m;
    float position_z_m;
    float radius_m;
    uint32_t first_detected_ms;
    uint32_t last_updated_ms;
    bool active;
} detected_hazard_t;

/** Victim detection */
typedef struct {
    float position_x_m;
    float position_y_m;
    float position_z_m;
    float confidence;
    bool thermal_signature;
    bool audio_detected;
    bool motion_detected;
    uint32_t timestamp_ms;
} victim_detection_t;

/** Zone assessment */
typedef struct {
    uint8_t zone_id;
    hazard_severity_t overall_severity;
    detected_hazard_t hazards[4];
    uint8_t hazard_count;
    victim_detection_t victims[4];
    uint8_t victim_count;
    bool robot_traversable;
    bool human_safe;
} zone_assessment_t;

/** Fusion configuration */
typedef struct {
    uint8_t num_sensors;
    float thermal_threshold_c;
    float gas_ppm_threshold;
    float radiation_usv_threshold;
    bool victim_detection_enabled;
    bool auto_hazard_avoidance;
} fusion_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int hazard_fusion_init(const fusion_config_t *config);
void hazard_fusion_shutdown(void);

int hazard_fusion_add_sensor(uint8_t sensor_id, sensor_modality_t modality);
int hazard_fusion_update_sensor(uint8_t sensor_id, const void *data, size_t len);

int hazard_fusion_get_assessment(zone_assessment_t *assessment);
int hazard_fusion_get_hazards(detected_hazard_t *hazards, uint8_t max_count);
int hazard_fusion_get_victims(victim_detection_t *victims, uint8_t max_count);

bool hazard_fusion_is_path_safe(float x1, float y1, float x2, float y2);
hazard_severity_t hazard_fusion_get_severity(float x, float y);

void hazard_fusion_update(uint32_t elapsed_ms);

#endif /* GF_HAZARD_SENSOR_FUSION_H */
