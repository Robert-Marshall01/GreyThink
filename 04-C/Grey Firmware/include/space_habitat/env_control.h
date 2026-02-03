/**
 * @file env_control.h
 * @brief Environmental Control System for Space Habitat Life Support
 * 
 * @details
 * This module implements the Environmental Control and Life Support System (ECLSS)
 * control loops for maintaining habitable conditions in space habitats. Controls
 * atmospheric composition, thermal regulation, humidity, and trace contaminants.
 * 
 * INDUSTRY RELEVANCE:
 * - ISS Environmental Control (NASA ECLSS)
 * - Commercial space stations (Axiom, Orbital Reef, Starlab)
 * - Lunar Gateway life support systems
 * - Mars surface habitat ISRU integration
 * - Submarine atmosphere management
 * - Bioregenerative life support research
 * 
 * KEY FEATURES:
 * - Closed-loop O2/CO2 balance control
 * - Humidity regulation with condensate recovery
 * - Trace contaminant removal scheduling
 * - Thermal control integration
 * - Fault-tolerant control with graceful degradation
 * - Emergency mode for rapid atmosphere correction
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_ENV_CONTROL_H
#define GF_ENV_CONTROL_H

#include <stdint.h>
#include <stdbool.h>
#include "life_support_sensor.h"

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Nominal O2 percentage (Earth sea level) */
#define GF_EC_NOMINAL_O2_PCT        20.9f

/** Maximum safe O2 percentage (fire hazard) */
#define GF_EC_MAX_SAFE_O2_PCT       23.5f

/** Minimum safe O2 percentage (hypoxia) */
#define GF_EC_MIN_SAFE_O2_PCT       19.5f

/** Nominal CO2 level (ppm) */
#define GF_EC_NOMINAL_CO2_PPM       400.0f

/** Warning CO2 level (ppm) */
#define GF_EC_WARN_CO2_PPM          2500.0f

/** Emergency CO2 level (ppm) - crew cognitive impact */
#define GF_EC_EMERGENCY_CO2_PPM     5000.0f

/** Nominal cabin pressure (kPa) */
#define GF_EC_NOMINAL_PRESSURE_KPA  101.3f

/** Nominal humidity percentage */
#define GF_EC_NOMINAL_HUMIDITY_PCT  45.0f

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Environmental control status codes
 */
typedef enum {
    GF_EC_OK = 0,
    GF_EC_ERROR_NOT_INITIALIZED,
    GF_EC_ERROR_NULL_PTR,
    GF_EC_ERROR_INVALID_PARAM,
    GF_EC_ERROR_SENSOR_FAULT,
    GF_EC_ERROR_ACTUATOR_FAULT,
    GF_EC_ERROR_CONTROL_LOOP,
    GF_EC_WARN_O2_LOW,
    GF_EC_WARN_O2_HIGH,
    GF_EC_WARN_CO2_HIGH,
    GF_EC_WARN_HUMIDITY_LOW,
    GF_EC_WARN_HUMIDITY_HIGH,
    GF_EC_WARN_PRESSURE_LOW,
    GF_EC_EMERGENCY_ATMOSPHERE
} gf_ec_status_t;

/**
 * @brief Control system operating mode
 */
typedef enum {
    GF_EC_MODE_STANDBY,           /**< System idle, monitoring only */
    GF_EC_MODE_NOMINAL,           /**< Normal closed-loop control */
    GF_EC_MODE_ECONOMY,           /**< Power-saving mode (wider bands) */
    GF_EC_MODE_SLEEP,             /**< Crew sleep mode (lower O2 consumption) */
    GF_EC_MODE_EVA,               /**< Extra-Vehicular Activity mode */
    GF_EC_MODE_EMERGENCY,         /**< Emergency atmosphere correction */
    GF_EC_MODE_DEPRESSURIZING,    /**< Controlled depressurization */
    GF_EC_MODE_REPRESSURIZING     /**< Controlled repressurization */
} gf_ec_mode_t;

/**
 * @brief Actuator types for environmental control
 */
typedef enum {
    GF_EC_ACT_O2_GENERATOR,       /**< O2 generation (electrolysis/MOXIE) */
    GF_EC_ACT_O2_RELEASE,         /**< O2 tank release valve */
    GF_EC_ACT_CO2_SCRUBBER,       /**< CO2 removal system (CDRA) */
    GF_EC_ACT_N2_RELEASE,         /**< N2 makeup release */
    GF_EC_ACT_HUMIDIFIER,         /**< Humidity addition */
    GF_EC_ACT_DEHUMIDIFIER,       /**< Condensate collection */
    GF_EC_ACT_HEATER,             /**< Cabin heating */
    GF_EC_ACT_COOLER,             /**< Thermal control (radiator) */
    GF_EC_ACT_FAN,                /**< Air circulation fan */
    GF_EC_ACT_TRACE_CONTAM,       /**< Trace contaminant removal */
    GF_EC_ACT_VENT                /**< Emergency vent valve */
} gf_ec_actuator_t;

/**
 * @brief Control loop PID configuration
 */
typedef struct {
    float kp;                     /**< Proportional gain */
    float ki;                     /**< Integral gain */
    float kd;                     /**< Derivative gain */
    float output_min;             /**< Minimum output */
    float output_max;             /**< Maximum output */
    float integral_limit;         /**< Anti-windup limit */
    float deadband;               /**< Deadband around setpoint */
} gf_ec_pid_config_t;

/**
 * @brief Environmental setpoints
 */
typedef struct {
    float o2_pct;                 /**< Target O2 percentage */
    float co2_max_ppm;            /**< Maximum CO2 level */
    float pressure_kpa;           /**< Target cabin pressure */
    float temperature_c;          /**< Target temperature */
    float humidity_pct;           /**< Target relative humidity */
} gf_ec_setpoints_t;

/**
 * @brief Environmental control system configuration
 */
typedef struct {
    gf_ec_setpoints_t setpoints;          /**< Target setpoints */
    gf_ec_pid_config_t o2_pid;            /**< O2 control loop config */
    gf_ec_pid_config_t co2_pid;           /**< CO2 control loop config */
    gf_ec_pid_config_t humidity_pid;      /**< Humidity control config */
    gf_ec_pid_config_t temperature_pid;   /**< Temperature control config */
    uint32_t control_interval_ms;         /**< Control loop interval */
    bool auto_emergency;                  /**< Auto-enter emergency mode */
    uint8_t crew_count;                   /**< Number of crew (O2 consumption) */
} gf_ec_config_t;

/**
 * @brief Environmental status snapshot
 */
typedef struct {
    gf_ec_mode_t mode;                    /**< Current operating mode */
    gf_ls_atmosphere_t atmosphere;        /**< Current atmosphere */
    float o2_generation_rate;             /**< O2 generation rate (kg/hr) */
    float co2_removal_rate;               /**< CO2 removal rate (kg/hr) */
    float humidity_add_rate;              /**< Humidity addition rate */
    float power_consumption_w;            /**< Total ECLSS power consumption */
    uint32_t uptime_hr;                   /**< System uptime (hours) */
    uint8_t active_faults;                /**< Number of active faults */
    bool emergency_active;                /**< Emergency mode active */
} gf_ec_status_snapshot_t;

/**
 * @brief Environmental alarm severity
 */
typedef enum {
    GF_EC_ALARM_INFO,
    GF_EC_ALARM_WARNING,
    GF_EC_ALARM_CAUTION,
    GF_EC_ALARM_EMERGENCY
} gf_ec_alarm_severity_t;

/**
 * @brief Environmental alarm callback
 */
typedef void (*gf_ec_alarm_cb_t)(gf_ec_alarm_severity_t severity, 
                                  const char* message, void* user_data);

/**
 * @brief Mode change callback
 */
typedef void (*gf_ec_mode_cb_t)(gf_ec_mode_t old_mode, gf_ec_mode_t new_mode,
                                 void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize environmental control system
 * @param config System configuration
 * @return Status code
 */
gf_ec_status_t gf_ec_init(const gf_ec_config_t* config);

/**
 * @brief Shutdown environmental control
 */
void gf_ec_shutdown(void);

/**
 * @brief Set operating mode
 * @param mode New operating mode
 * @return Status code
 */
gf_ec_status_t gf_ec_set_mode(gf_ec_mode_t mode);

/**
 * @brief Get current operating mode
 * @return Current mode
 */
gf_ec_mode_t gf_ec_get_mode(void);

/**
 * @brief Update environmental setpoints
 * @param setpoints New setpoints
 * @return Status code
 */
gf_ec_status_t gf_ec_set_setpoints(const gf_ec_setpoints_t* setpoints);

/**
 * @brief Get current setpoints
 * @param setpoints Output setpoints
 * @return Status code
 */
gf_ec_status_t gf_ec_get_setpoints(gf_ec_setpoints_t* setpoints);

/**
 * @brief Manual actuator override
 * @param actuator Actuator to control
 * @param output Output level (0.0 to 1.0)
 * @return Status code
 */
gf_ec_status_t gf_ec_actuator_override(gf_ec_actuator_t actuator, float output);

/**
 * @brief Release actuator override (return to automatic)
 * @param actuator Actuator to release
 * @return Status code
 */
gf_ec_status_t gf_ec_actuator_release(gf_ec_actuator_t actuator);

/**
 * @brief Get system status snapshot
 * @param status Output status snapshot
 * @return Status code
 */
gf_ec_status_t gf_ec_get_status(gf_ec_status_snapshot_t* status);

/**
 * @brief Register alarm callback
 * @param callback Alarm callback function
 * @param user_data User context
 * @return Status code
 */
gf_ec_status_t gf_ec_register_alarm_callback(gf_ec_alarm_cb_t callback,
                                              void* user_data);

/**
 * @brief Register mode change callback
 * @param callback Mode change callback function
 * @param user_data User context
 * @return Status code
 */
gf_ec_status_t gf_ec_register_mode_callback(gf_ec_mode_cb_t callback,
                                             void* user_data);

/**
 * @brief Update crew count (affects O2 consumption model)
 * @param crew_count Number of crew members
 * @return Status code
 */
gf_ec_status_t gf_ec_set_crew_count(uint8_t crew_count);

/**
 * @brief Trigger emergency atmosphere correction
 * @return Status code
 */
gf_ec_status_t gf_ec_trigger_emergency(void);

/**
 * @brief Clear emergency condition
 * @return Status code
 */
gf_ec_status_t gf_ec_clear_emergency(void);

/**
 * @brief Process control loops (call periodically)
 * @return Status code
 */
gf_ec_status_t gf_ec_process(void);

/**
 * @brief Get current power consumption
 * @return Power in watts
 */
float gf_ec_get_power_consumption(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ENV_CONTROL_H */
