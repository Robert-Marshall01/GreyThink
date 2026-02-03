/**
 * @file microgrid_telemetry.h
 * @brief Microgrid Telemetry and SCADA Interface
 * 
 * INDUSTRY RELEVANCE:
 * Microgrid monitoring requires real-time telemetry integration with SCADA,
 * EMS, and cloud analytics platforms. This module provides standardized
 * data collection, historian integration, and alarm management for grid
 * operations centers and predictive maintenance systems.
 * 
 * Key applications:
 * - SCADA/EMS integration (DNP3, IEC 61850)
 * - Cloud-based grid analytics
 * - Regulatory compliance reporting
 * - Predictive maintenance analytics
 * - Real-time grid visualization
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_MICROGRID_TELEMETRY_H
#define GF_MICROGRID_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define MG_TLM_MAX_POINTS           256     /**< Max telemetry points */
#define MG_TLM_MAX_ALARMS           64      /**< Max active alarms */
#define MG_TLM_HISTORY_DEPTH        1440    /**< 24 hours at 1-min intervals */
#define MG_TLM_REPORT_INTERVAL_MS   1000    /**< Default report interval */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Telemetry point type
 */
typedef enum {
    MG_POINT_ANALOG_INPUT = 0,      /**< Analog measurement */
    MG_POINT_ANALOG_OUTPUT,         /**< Analog setpoint */
    MG_POINT_BINARY_INPUT,          /**< Digital status */
    MG_POINT_BINARY_OUTPUT,         /**< Digital control */
    MG_POINT_COUNTER,               /**< Accumulated counter */
    MG_POINT_FROZEN_COUNTER         /**< Frozen counter value */
} mg_point_type_t;

/**
 * @brief Alarm severity
 */
typedef enum {
    MG_ALARM_INFO = 0,
    MG_ALARM_WARNING,
    MG_ALARM_MINOR,
    MG_ALARM_MAJOR,
    MG_ALARM_CRITICAL
} mg_alarm_severity_t;

/**
 * @brief Telemetry point definition
 */
typedef struct {
    uint16_t point_id;
    mg_point_type_t type;
    char name[32];
    char unit[8];
    float scale_factor;
    float offset;
    float alarm_low;
    float alarm_high;
} mg_point_def_t;

/**
 * @brief Telemetry point value
 */
typedef struct {
    uint16_t point_id;
    uint32_t timestamp;
    float value;
    uint8_t quality;                /**< Quality flags */
    bool alarm_active;
} mg_point_value_t;

/**
 * @brief Active alarm
 */
typedef struct {
    uint16_t alarm_id;
    uint16_t point_id;
    mg_alarm_severity_t severity;
    uint32_t timestamp;
    float trigger_value;
    char message[64];
    bool acknowledged;
} mg_alarm_t;

/**
 * @brief Grid status summary
 */
typedef struct {
    float total_generation_kw;
    float total_load_kw;
    float storage_soc_percent;
    float grid_import_kw;
    float voltage_pu;               /**< Per-unit voltage */
    float frequency_hz;
    float power_factor;
    uint8_t sources_online;
    uint8_t loads_active;
    uint8_t alarms_active;
    uint32_t uptime_seconds;
} mg_grid_summary_t;

/**
 * @brief Telemetry configuration
 */
typedef struct {
    uint16_t report_interval_ms;
    uint16_t history_interval_ms;
    bool enable_dnp3;
    bool enable_modbus;
    bool enable_mqtt;
    bool enable_historian;
    char mqtt_topic_prefix[32];
} mg_tlm_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize telemetry module
 * @param config Telemetry configuration
 * @return 0 on success, negative on error
 */
int mg_tlm_init(const mg_tlm_config_t* config);

/**
 * @brief Shutdown telemetry module
 * @return 0 on success, negative on error
 */
int mg_tlm_shutdown(void);

/**
 * @brief Register telemetry point
 * @param def Point definition
 * @return Point ID on success, negative on error
 */
int mg_tlm_register_point(const mg_point_def_t* def);

/**
 * @brief Update point value
 * @param point_id Point ID
 * @param value New value
 * @return 0 on success, negative on error
 */
int mg_tlm_update(uint16_t point_id, float value);

/**
 * @brief Get point value
 * @param point_id Point ID
 * @param value Output value structure
 * @return 0 on success, negative on error
 */
int mg_tlm_get_value(uint16_t point_id, mg_point_value_t* value);

/**
 * @brief Get grid status summary
 * @param summary Output summary
 * @return 0 on success, negative on error
 */
int mg_tlm_get_summary(mg_grid_summary_t* summary);

/**
 * @brief Get active alarms
 * @param alarms Output alarm array
 * @param max_alarms Array size
 * @return Number of active alarms
 */
int mg_tlm_get_alarms(mg_alarm_t* alarms, uint8_t max_alarms);

/**
 * @brief Acknowledge alarm
 * @param alarm_id Alarm to acknowledge
 * @return 0 on success, negative on error
 */
int mg_tlm_ack_alarm(uint16_t alarm_id);

/**
 * @brief Process telemetry reporting
 * @return 0 on success, negative on error
 */
int mg_tlm_process(void);

/**
 * @brief Get historical data
 * @param point_id Point ID
 * @param start_time Start timestamp
 * @param end_time End timestamp
 * @param values Output value array
 * @param max_values Array size
 * @return Number of values retrieved
 */
int mg_tlm_get_history(uint16_t point_id, uint32_t start_time, 
                       uint32_t end_time, mg_point_value_t* values,
                       uint16_t max_values);

#ifdef __cplusplus
}
#endif

#endif /* GF_MICROGRID_TELEMETRY_H */
