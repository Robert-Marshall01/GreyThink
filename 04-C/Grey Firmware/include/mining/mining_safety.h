/**
 * @file mining_safety.h
 * @brief Mining Safety Interlock Module
 * 
 * @details
 * This module provides safety interlock systems for mining operations,
 * ensuring worker safety and equipment protection through monitored
 * safety functions and emergency response.
 * 
 * INDUSTRY RELEVANCE:
 * - Underground Mining: Collision avoidance, proximity detection
 * - Surface Mining: Haul truck safety systems
 * - Processing Plants: Equipment lockout/tagout
 * - Tunneling: Gas monitoring and ventilation control
 * - Quarrying: Blast zone safety management
 * 
 * SAFETY COMPLIANCE:
 * - MSHA 30 CFR: Mine Safety and Health Administration
 * - ISO 19296: Mining machinery safety
 * - IEC 61508: Functional Safety (SIL ratings)
 * - EN 13849: Safety of machinery (PLe)
 * - AS 4024: Australian machinery safety
 * 
 * SAFETY FUNCTIONS:
 * - Gas detection and evacuation triggers
 * - Collision avoidance and proximity warning
 * - Emergency shutdown (E-stop)
 * - Fire detection and suppression
 * - Ventilation/atmospheric monitoring
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_MINING_SAFETY_H
#define GF_MINING_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum safety zones */
#define GF_SAFETY_MAX_ZONES             32

/** Maximum gas sensors */
#define GF_SAFETY_MAX_GAS_SENSORS       16

/** Maximum personnel tags */
#define GF_SAFETY_MAX_PERSONNEL         200

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Gas types monitored
 */
typedef enum {
    GF_GAS_METHANE,               /**< CH4 (explosive) */
    GF_GAS_CARBON_MONOXIDE,       /**< CO (toxic) */
    GF_GAS_CARBON_DIOXIDE,        /**< CO2 */
    GF_GAS_HYDROGEN_SULFIDE,      /**< H2S (toxic) */
    GF_GAS_NITROGEN_DIOXIDE,      /**< NO2 */
    GF_GAS_SULFUR_DIOXIDE,        /**< SO2 */
    GF_GAS_OXYGEN                 /**< O2 (deficiency) */
} gf_gas_type_t;

/**
 * @brief Safety alert levels
 */
typedef enum {
    GF_ALERT_NONE,                /**< Normal */
    GF_ALERT_ADVISORY,            /**< Advisory */
    GF_ALERT_WARNING,             /**< Warning */
    GF_ALERT_DANGER,              /**< Danger */
    GF_ALERT_EVACUATION           /**< Evacuation required */
} gf_alert_level_t;

/**
 * @brief Proximity zone type
 */
typedef enum {
    GF_ZONE_SAFE,                 /**< Safe area */
    GF_ZONE_CAUTION,              /**< Caution zone */
    GF_ZONE_DANGER,               /**< Danger zone */
    GF_ZONE_EXCLUSION             /**< Exclusion zone */
} gf_proximity_zone_t;

/**
 * @brief Gas reading
 */
typedef struct {
    gf_gas_type_t type;           /**< Gas type */
    float concentration_ppm;      /**< Concentration (ppm or %) */
    float twel_ppm;               /**< TWA exposure limit */
    float stel_ppm;               /**< STEL */
    gf_alert_level_t alert;       /**< Current alert level */
    uint32_t timestamp;           /**< Reading timestamp */
    bool sensor_ok;               /**< Sensor status */
} gf_gas_reading_t;

/**
 * @brief Personnel location
 */
typedef struct {
    char tag_id[16];              /**< RFID/UWB tag ID */
    char name[32];                /**< Person name */
    float x, y, z;                /**< Position */
    char zone[32];                /**< Current zone */
    gf_proximity_zone_t proximity; /**< Proximity status */
    uint32_t last_seen;           /**< Last seen timestamp */
    bool evacuated;               /**< Evacuation confirmed */
} gf_personnel_location_t;

/**
 * @brief Safety zone definition
 */
typedef struct {
    char zone_id[16];             /**< Zone identifier */
    gf_proximity_zone_t type;     /**< Zone type */
    float center_x, center_y, center_z;
    float radius_m;               /**< Zone radius */
    bool active;                  /**< Zone active */
    uint8_t personnel_count;      /**< People in zone */
} gf_safety_zone_t;

/**
 * @brief Safety system status
 */
typedef struct {
    gf_alert_level_t overall_alert;
    bool evacuation_active;       /**< Evacuation in progress */
    uint8_t personnel_underground; /**< People underground */
    uint8_t personnel_evacuated;  /**< People evacuated */
    uint8_t gas_alerts;           /**< Active gas alerts */
    bool ventilation_ok;          /**< Ventilation status */
    bool fire_detected;           /**< Fire detection */
    uint32_t last_check;          /**< Last safety check */
} gf_safety_status_t;

/*******************************************************************************
 * Function Prototypes  
 ******************************************************************************/

/**
 * @brief Initialize mining safety system
 * @return 0 on success
 */
int gf_mining_safety_init(void);

/**
 * @brief Shutdown safety system
 */
void gf_mining_safety_shutdown(void);

/**
 * @brief Trigger evacuation
 * @param reason Evacuation reason
 * @return 0 on success
 */
int gf_safety_trigger_evacuation(const char* reason);

/**
 * @brief Clear evacuation
 * @return 0 on success
 */
int gf_safety_clear_evacuation(void);

/**
 * @brief Get gas reading
 * @param type Gas type
 * @param reading Output reading
 * @return 0 on success
 */
int gf_safety_get_gas(gf_gas_type_t type, gf_gas_reading_t* reading);

/**
 * @brief Get personnel count
 * @param zone Zone name (NULL for all)
 * @return Personnel count
 */
int gf_safety_get_personnel_count(const char* zone);

/**
 * @brief Get safety status
 * @param status Output status
 * @return 0 on success
 */
int gf_safety_get_status(gf_safety_status_t* status);

/**
 * @brief E-stop activation
 * @param equipment Equipment ID (NULL for all)
 */
void gf_safety_estop(const char* equipment);

/**
 * @brief Process safety system (call periodically)
 * @param delta_ms Time since last call
 */
void gf_mining_safety_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_MINING_SAFETY_H */
