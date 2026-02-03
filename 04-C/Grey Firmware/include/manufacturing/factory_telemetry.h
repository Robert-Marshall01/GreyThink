/**
 * @file factory_telemetry.h
 * @brief Factory Telemetry and OEE Monitoring Interface
 *
 * INDUSTRY RELEVANCE:
 * Industrial telemetry and Overall Equipment Effectiveness (OEE) monitoring
 * are essential for Industry 4.0 smart factories. This module demonstrates:
 * - Real-time production metrics collection
 * - OEE calculation (Availability × Performance × Quality)
 * - Predictive maintenance data aggregation
 * - Integration with MES (Manufacturing Execution Systems)
 *
 * These skills apply to industrial IoT platforms (PTC ThingWorx, Siemens
 * MindSphere, GE Predix), factory automation, and companies pursuing
 * digital transformation initiatives.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires integration with factory systems.
 */

#ifndef GF_FACTORY_TELEMETRY_H
#define GF_FACTORY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_FACTORY_MAX_MACHINES     64      /**< Maximum monitored machines */
#define GF_FACTORY_MAX_SENSORS      256     /**< Maximum sensor channels */
#define GF_FACTORY_MAX_ALARMS       128     /**< Maximum active alarms */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Machine state
 */
typedef enum {
    GF_MACHINE_OFF,             /**< Powered off */
    GF_MACHINE_IDLE,            /**< Powered, idle */
    GF_MACHINE_RUNNING,         /**< Running production */
    GF_MACHINE_SETUP,           /**< Changeover/setup */
    GF_MACHINE_FAULT,           /**< Faulted */
    GF_MACHINE_MAINTENANCE      /**< Planned maintenance */
} gf_machine_state_t;

/**
 * @brief Alarm severity
 */
typedef enum {
    GF_ALARM_INFO,              /**< Informational */
    GF_ALARM_WARNING,           /**< Warning */
    GF_ALARM_MINOR,             /**< Minor fault */
    GF_ALARM_MAJOR,             /**< Major fault */
    GF_ALARM_CRITICAL           /**< Critical - stop production */
} gf_alarm_severity_t;

/**
 * @brief Sensor data type
 */
typedef enum {
    GF_DATA_TEMPERATURE,        /**< Temperature (°C) */
    GF_DATA_PRESSURE,           /**< Pressure (bar) */
    GF_DATA_VIBRATION,          /**< Vibration (mm/s RMS) */
    GF_DATA_CURRENT,            /**< Current (A) */
    GF_DATA_POWER,              /**< Power (kW) */
    GF_DATA_SPEED,              /**< Speed (RPM or mm/s) */
    GF_DATA_POSITION,           /**< Position (mm) */
    GF_DATA_COUNT,              /**< Counter value */
    GF_DATA_CUSTOM              /**< Custom metric */
} gf_data_type_t;

/**
 * @brief Machine configuration
 */
typedef struct {
    uint16_t machine_id;                /**< Machine identifier */
    char name[32];                      /**< Machine name */
    char location[32];                  /**< Physical location */
    float ideal_cycle_time_s;           /**< Ideal cycle time */
    uint32_t planned_uptime_min;        /**< Planned daily uptime */
} gf_machine_config_t;

/**
 * @brief Sensor reading
 */
typedef struct {
    uint16_t sensor_id;                 /**< Sensor identifier */
    uint16_t machine_id;                /**< Associated machine */
    gf_data_type_t type;                /**< Data type */
    float value;                        /**< Current value */
    float min_value;                    /**< Minimum since reset */
    float max_value;                    /**< Maximum since reset */
    float avg_value;                    /**< Average since reset */
    uint64_t timestamp_ms;              /**< Reading timestamp */
    bool valid;                         /**< Reading valid */
} gf_sensor_reading_t;

/**
 * @brief OEE metrics
 */
typedef struct {
    float availability;                 /**< Availability (0-1) */
    float performance;                  /**< Performance (0-1) */
    float quality;                      /**< Quality (0-1) */
    float oee;                          /**< Overall OEE (0-1) */
    uint32_t planned_time_min;          /**< Planned production time */
    uint32_t actual_runtime_min;        /**< Actual runtime */
    uint32_t downtime_min;              /**< Unplanned downtime */
    uint32_t ideal_count;               /**< Ideal part count */
    uint32_t actual_count;              /**< Actual part count */
    uint32_t good_count;                /**< Good parts */
    uint32_t reject_count;              /**< Rejected parts */
} gf_oee_metrics_t;

/**
 * @brief Alarm record
 */
typedef struct {
    uint32_t alarm_id;                  /**< Alarm identifier */
    uint16_t machine_id;                /**< Machine raising alarm */
    uint16_t alarm_code;                /**< Alarm code */
    gf_alarm_severity_t severity;       /**< Severity level */
    char description[64];               /**< Alarm description */
    uint64_t timestamp_ms;              /**< Alarm timestamp */
    uint64_t ack_time_ms;               /**< Acknowledgment time (0 = unacked) */
    uint64_t clear_time_ms;             /**< Clear time (0 = active) */
} gf_alarm_record_t;

/**
 * @brief Production shift summary
 */
typedef struct {
    uint32_t shift_id;                  /**< Shift identifier */
    uint64_t start_time;                /**< Shift start (UTC) */
    uint64_t end_time;                  /**< Shift end (UTC) */
    uint32_t parts_produced;            /**< Parts produced */
    uint32_t parts_rejected;            /**< Parts rejected */
    gf_oee_metrics_t oee;               /**< Shift OEE */
    uint32_t downtime_events;           /**< Number of downtime events */
    uint32_t alarm_count;               /**< Alarms raised */
    float energy_kwh;                   /**< Energy consumed */
} gf_shift_summary_t;

/**
 * @brief Predictive maintenance indicator
 */
typedef struct {
    uint16_t machine_id;                /**< Machine identifier */
    uint16_t component_id;              /**< Component identifier */
    char component_name[32];            /**< Component name */
    float health_score;                 /**< Health score (0-100) */
    float remaining_life_hours;         /**< Estimated remaining life */
    uint32_t cycles_since_service;      /**< Cycles since last service */
    float trend_slope;                  /**< Health trend (negative = degrading) */
    bool maintenance_recommended;       /**< Maintenance flag */
} gf_maintenance_indicator_t;

/**
 * @brief Telemetry configuration
 */
typedef struct {
    uint16_t sample_interval_ms;        /**< Sensor sample interval */
    uint16_t transmit_interval_ms;      /**< Transmit interval */
    bool enable_compression;            /**< Compress telemetry data */
    bool enable_edge_analytics;         /**< Enable edge processing */
} gf_telemetry_config_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_alarm_cb_t)(const gf_alarm_record_t* alarm, void* user_data);
typedef void (*gf_oee_update_cb_t)(uint16_t machine_id, const gf_oee_metrics_t* oee, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize factory telemetry system
 * @param config Telemetry configuration
 * @return 0 on success, negative error code on failure
 */
int gf_factory_init(const gf_telemetry_config_t* config);

/**
 * @brief Shutdown factory telemetry system
 */
void gf_factory_deinit(void);

/**
 * @brief Register machine for monitoring
 * @param config Machine configuration
 * @return 0 on success
 */
int gf_factory_register_machine(const gf_machine_config_t* config);

/**
 * @brief Update machine state
 * @param machine_id Machine identifier
 * @param state New state
 */
void gf_factory_set_machine_state(uint16_t machine_id, gf_machine_state_t state);

/**
 * @brief Record sensor reading
 * @param reading Sensor reading
 */
void gf_factory_record_sensor(const gf_sensor_reading_t* reading);

/**
 * @brief Record production count
 * @param machine_id Machine identifier
 * @param good_count Good parts produced
 * @param reject_count Rejected parts
 */
void gf_factory_record_production(uint16_t machine_id, uint32_t good_count, uint32_t reject_count);

/**
 * @brief Raise alarm
 * @param machine_id Machine identifier
 * @param alarm_code Alarm code
 * @param severity Severity level
 * @param description Alarm description
 * @return Alarm ID
 */
uint32_t gf_factory_raise_alarm(uint16_t machine_id, uint16_t alarm_code,
                                 gf_alarm_severity_t severity, const char* description);

/**
 * @brief Acknowledge alarm
 * @param alarm_id Alarm ID
 */
void gf_factory_ack_alarm(uint32_t alarm_id);

/**
 * @brief Clear alarm
 * @param alarm_id Alarm ID
 */
void gf_factory_clear_alarm(uint32_t alarm_id);

/**
 * @brief Get OEE metrics for machine
 * @param machine_id Machine identifier
 * @param[out] oee Output OEE metrics
 * @return 0 on success
 */
int gf_factory_get_oee(uint16_t machine_id, gf_oee_metrics_t* oee);

/**
 * @brief Get shift summary
 * @param machine_id Machine identifier
 * @param[out] summary Output shift summary
 * @return 0 on success
 */
int gf_factory_get_shift_summary(uint16_t machine_id, gf_shift_summary_t* summary);

/**
 * @brief Get maintenance indicators
 * @param machine_id Machine identifier
 * @param[out] indicators Output indicator array
 * @param max_indicators Array capacity
 * @return Number of indicators
 */
int gf_factory_get_maintenance_indicators(uint16_t machine_id,
                                           gf_maintenance_indicator_t* indicators,
                                           uint8_t max_indicators);

/**
 * @brief Set alarm callback
 * @param callback Alarm event callback
 * @param user_data User context
 */
void gf_factory_set_alarm_callback(gf_alarm_cb_t callback, void* user_data);

/**
 * @brief Set OEE update callback
 * @param callback OEE update callback
 * @param user_data User context
 */
void gf_factory_set_oee_callback(gf_oee_update_cb_t callback, void* user_data);

/**
 * @brief Start new shift
 * @param shift_name Shift identifier (e.g., "Day Shift")
 */
void gf_factory_start_shift(const char* shift_name);

/**
 * @brief End current shift
 */
void gf_factory_end_shift(void);

/**
 * @brief Process telemetry updates (call periodically)
 */
void gf_factory_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FACTORY_TELEMETRY_H */
