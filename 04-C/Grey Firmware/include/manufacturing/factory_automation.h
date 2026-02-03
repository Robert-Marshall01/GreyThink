/**
 * @file factory_automation.h
 * @brief Factory Automation Telemetry Interface
 * 
 * INDUSTRY RELEVANCE:
 * Factory automation telemetry enables Industry 4.0 operations:
 * - Overall Equipment Effectiveness (OEE) monitoring
 * - Predictive maintenance using machine learning
 * - Real-time production tracking and scheduling
 * - Energy consumption optimization
 * - Quality control and Statistical Process Control (SPC)
 * - Supply chain integration and JIT manufacturing
 * 
 * This module demonstrates expertise in:
 * - OPC-UA and MQTT industrial protocols
 * - ISA-95/ISA-88 batch and discrete manufacturing models
 * - High-frequency data collection (vibration, power, temp)
 * - Edge analytics for anomaly detection
 * - MES (Manufacturing Execution System) integration
 * - Digital twin data synchronization
 */

#ifndef GF_FACTORY_AUTOMATION_H
#define GF_FACTORY_AUTOMATION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_FA_MAX_MACHINES          64
#define GF_FA_MAX_SENSORS           16
#define GF_FA_MAX_PRODUCTS          100

typedef enum {
    GF_FA_OK = 0,
    GF_FA_ERROR_NOT_INITIALIZED,
    GF_FA_ERROR_MACHINE_OFFLINE,
    GF_FA_ERROR_SENSOR_FAULT,
    GF_FA_ERROR_COMM_TIMEOUT,
    GF_FA_ERROR_DATABASE_FULL,
    GF_FA_WARN_OEE_LOW,
    GF_FA_WARN_MAINTENANCE_DUE
} gf_fa_status_t;

typedef enum {
    GF_FA_STATE_OFFLINE,
    GF_FA_STATE_IDLE,
    GF_FA_STATE_RUNNING,
    GF_FA_STATE_SETUP,
    GF_FA_STATE_MAINTENANCE,
    GF_FA_STATE_FAULT,
    GF_FA_STATE_STARVED,            /* Waiting for material */
    GF_FA_STATE_BLOCKED             /* Output blocked */
} gf_fa_machine_state_t;

typedef enum {
    GF_FA_DOWNTIME_PLANNED,
    GF_FA_DOWNTIME_BREAKDOWN,
    GF_FA_DOWNTIME_SETUP,
    GF_FA_DOWNTIME_WAITING,
    GF_FA_DOWNTIME_QUALITY
} gf_fa_downtime_t;

typedef struct {
    float availability;             /* % uptime */
    float performance;              /* % of ideal cycle time */
    float quality;                  /* % good parts */
    float oee;                      /* Overall = A * P * Q */
} gf_fa_oee_t;

typedef struct {
    uint16_t machine_id;
    char name[32];
    gf_fa_machine_state_t state;
    uint32_t parts_produced;
    uint32_t parts_rejected;
    float cycle_time_s;
    float ideal_cycle_time_s;
    uint32_t runtime_s;
    uint32_t downtime_s;
    float energy_kwh;
    gf_fa_oee_t oee;
} gf_fa_machine_t;

typedef struct {
    uint16_t sensor_id;
    uint16_t machine_id;
    uint8_t sensor_type;
    float value;
    float min_threshold;
    float max_threshold;
    bool alarm_active;
    uint32_t timestamp_ms;
} gf_fa_sensor_t;

typedef struct {
    uint32_t product_id;
    char serial[32];
    uint16_t machine_id;
    uint32_t start_time;
    uint32_t end_time;
    float cycle_time_s;
    bool quality_pass;
    float measurements[8];          /* Quality measurements */
} gf_fa_product_t;

typedef struct {
    uint16_t gateway_id;
    uint16_t poll_interval_ms;
    bool opcua_enabled;
    bool mqtt_enabled;
    char mes_endpoint[128];
    bool predictive_maintenance;
    uint16_t history_depth;
} gf_fa_config_t;

gf_fa_status_t gf_fa_init(const gf_fa_config_t* config);
void gf_fa_shutdown(void);
gf_fa_status_t gf_fa_register_machine(const gf_fa_machine_t* machine);
gf_fa_status_t gf_fa_update_state(uint16_t machine_id, gf_fa_machine_state_t state);
gf_fa_status_t gf_fa_record_production(uint16_t machine_id, bool good_part);
gf_fa_status_t gf_fa_record_downtime(uint16_t machine_id, gf_fa_downtime_t reason, uint32_t duration_s);
gf_fa_status_t gf_fa_update_sensor(const gf_fa_sensor_t* sensor);
gf_fa_status_t gf_fa_get_oee(uint16_t machine_id, gf_fa_oee_t* oee);
gf_fa_status_t gf_fa_get_plant_oee(gf_fa_oee_t* oee);
gf_fa_status_t gf_fa_trace_product(uint32_t product_id, gf_fa_product_t* trace);
gf_fa_status_t gf_fa_predict_maintenance(uint16_t machine_id, uint32_t* hours_remaining);
gf_fa_status_t gf_fa_push_to_mes(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FACTORY_AUTOMATION_H */
