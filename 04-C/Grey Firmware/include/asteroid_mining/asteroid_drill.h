/**
 * @file asteroid_drill.h
 * @brief Asteroid Drill Sensor Driver Stub
 * 
 * Industry Relevance:
 * Space resource extraction is a rapidly growing sector with companies like
 * Planetary Resources, Deep Space Industries, and NASA's OSIRIS-REx mission
 * demonstrating the viability of asteroid mining. This stub showcases:
 * - Extreme environment sensor drivers (vacuum, radiation, thermal cycling)
 * - High-torque drill motor control for regolith and metallic cores
 * - Real-time vibration analysis for drill bit health monitoring
 * - Autonomous fault detection in communication-delayed environments
 * 
 * Applications: Space mining operations, sample return missions, ISRU systems
 * 
 * @author Grey Firmware Project
 */

#ifndef ASTEROID_DRILL_H
#define ASTEROID_DRILL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Drill operation mode */
typedef enum {
    DRILL_MODE_IDLE,
    DRILL_MODE_SURVEY,       /**< Low-torque surface sampling */
    DRILL_MODE_CORE,         /**< High-torque core extraction */
    DRILL_MODE_PERCUSSION,   /**< Impact drilling for hard materials */
    DRILL_MODE_RETRACT       /**< Emergency retraction */
} drill_mode_t;

/** Drill bit material type detection */
typedef enum {
    MATERIAL_UNKNOWN,
    MATERIAL_REGOLITH,       /**< Loose surface material */
    MATERIAL_SILICATE,       /**< Rocky silicate */
    MATERIAL_CARBONACEOUS,   /**< Carbon-rich material */
    MATERIAL_METALLIC,       /**< Iron-nickel core */
    MATERIAL_ICE             /**< Water ice deposits */
} asteroid_material_t;

/** Drill sensor readings */
typedef struct {
    float torque_nm;         /**< Current torque (N·m) */
    float rpm;               /**< Rotation speed */
    float depth_m;           /**< Penetration depth (meters) */
    float temperature_c;     /**< Drill bit temperature */
    float vibration_g;       /**< Vibration amplitude (g) */
    float power_w;           /**< Power consumption (watts) */
    asteroid_material_t material; /**< Detected material type */
} drill_sensor_data_t;

/** Drill configuration */
typedef struct {
    float max_torque_nm;     /**< Maximum allowed torque */
    float max_depth_m;       /**< Maximum drill depth */
    float target_rpm;        /**< Target rotation speed */
    float overheat_temp_c;   /**< Overheat threshold */
    bool percussion_enabled; /**< Enable percussion mode */
} drill_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize drill sensor subsystem
 * @param config Initial configuration
 * @return 0 on success, negative on error
 */
int asteroid_drill_init(const drill_config_t *config);

/**
 * @brief Set drill operation mode
 * @param mode Target operating mode
 * @return 0 on success, negative on error
 */
int asteroid_drill_set_mode(drill_mode_t mode);

/**
 * @brief Read current drill sensor data
 * @param data Output sensor readings
 * @return 0 on success, negative on error
 */
int asteroid_drill_read(drill_sensor_data_t *data);

/**
 * @brief Set target drill parameters
 * @param rpm Target rotation speed
 * @param torque_limit Maximum torque limit
 * @return 0 on success, negative on error
 */
int asteroid_drill_set_params(float rpm, float torque_limit);

/**
 * @brief Emergency stop drill operation
 * @return 0 on success, negative on error
 */
int asteroid_drill_emergency_stop(void);

/**
 * @brief Check drill bit wear level
 * @return Wear percentage (0-100), negative on error
 */
int asteroid_drill_get_wear(void);

/**
 * @brief Shutdown drill subsystem
 */
void asteroid_drill_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* ASTEROID_DRILL_H */
