/**
 * @file energy_telemetry.h
 * @brief Energy Telemetry Collector for Renewable Systems
 * 
 * INDUSTRY RELEVANCE:
 * Renewable energy installations require comprehensive telemetry for:
 * - SCADA integration and remote monitoring
 * - Production forecasting and analytics
 * - Grid operator reporting (generation curtailment)
 * - Maintenance scheduling (predictive analytics)
 * - Regulatory compliance (capacity factor reporting)
 * 
 * Protocols: DNP3, IEC 61850, Modbus TCP, OPC-UA
 * Companies: Vestas, GE Renewable, Envision, Goldwind
 */

#ifndef GF_ENERGY_TELEMETRY_H
#define GF_ENERGY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_TELEMETRY_MAX_POINTS       64
#define GF_TELEMETRY_BUFFER_SECONDS   3600    /* 1 hour circular buffer */
#define GF_TELEMETRY_SAMPLE_RATE_HZ   10
#define GF_TELEMETRY_AGGREGATE_SEC    60      /* 1-minute aggregates */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_TELEM_OK = 0,
    GF_TELEM_ERROR_NULL_PTR,
    GF_TELEM_ERROR_NOT_INITIALIZED,
    GF_TELEM_ERROR_BUFFER_FULL,
    GF_TELEM_ERROR_POINT_NOT_FOUND,
    GF_TELEM_ERROR_INVALID_TIMESTAMP,
    GF_TELEM_ERROR_TRANSMISSION_FAILED,
    GF_TELEM_ERROR_PROTOCOL_ERROR,
    GF_TELEM_WARN_DATA_STALE
} gf_telemetry_status_t;

typedef enum {
    GF_TELEM_POINT_ANALOG,
    GF_TELEM_POINT_DIGITAL,
    GF_TELEM_POINT_COUNTER,
    GF_TELEM_POINT_SETPOINT
} gf_telemetry_point_type_t;

typedef enum {
    GF_TELEM_QUALITY_GOOD,
    GF_TELEM_QUALITY_UNCERTAIN,
    GF_TELEM_QUALITY_BAD,
    GF_TELEM_QUALITY_OFFLINE
} gf_telemetry_quality_t;

typedef enum {
    GF_TELEM_PROTO_MODBUS_TCP,
    GF_TELEM_PROTO_DNP3,
    GF_TELEM_PROTO_IEC61850,
    GF_TELEM_PROTO_OPCUA,
    GF_TELEM_PROTO_MQTT
} gf_telemetry_protocol_t;

/**
 * @brief Telemetry configuration
 */
typedef struct {
    gf_telemetry_protocol_t protocol;
    uint32_t sample_rate_hz;
    uint32_t buffer_size_samples;
    uint32_t aggregate_period_sec;
    bool store_and_forward;
    bool compression_enabled;
    uint16_t remote_port;
    char remote_host[64];
} gf_telemetry_config_t;

/**
 * @brief Data point definition
 */
typedef struct {
    uint16_t point_id;
    char name[32];
    char unit[16];
    gf_telemetry_point_type_t type;
    float scale_factor;
    float offset;
    float min_value;
    float max_value;
    float deadband;             /* Report on change threshold */
} gf_telemetry_point_def_t;

/**
 * @brief Data point value
 */
typedef struct {
    uint16_t point_id;
    uint64_t timestamp_ms;
    union {
        float analog;
        bool digital;
        uint64_t counter;
    } value;
    gf_telemetry_quality_t quality;
} gf_telemetry_value_t;

/**
 * @brief Aggregated statistics
 */
typedef struct {
    uint16_t point_id;
    uint64_t period_start_ms;
    uint64_t period_end_ms;
    float min_value;
    float max_value;
    float avg_value;
    float sum_value;
    uint32_t sample_count;
    uint32_t good_quality_count;
} gf_telemetry_aggregate_t;

/**
 * @brief Production summary
 */
typedef struct {
    uint64_t period_start_ms;
    uint64_t period_end_ms;
    float total_energy_kwh;
    float peak_power_kw;
    float avg_power_kw;
    float capacity_factor_pct;
    float availability_pct;
    uint32_t operating_hours;
    uint32_t fault_count;
    uint32_t curtailment_kwh;
} gf_telemetry_production_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_telemetry_value_cb_t)(const gf_telemetry_value_t* value,
                                         void* user_data);

typedef void (*gf_telemetry_aggregate_cb_t)(const gf_telemetry_aggregate_t* agg,
                                             void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_telemetry_status_t gf_telemetry_init(const gf_telemetry_config_t* config);
void gf_telemetry_shutdown(void);

/* Point management */
gf_telemetry_status_t gf_telemetry_add_point(const gf_telemetry_point_def_t* def);
gf_telemetry_status_t gf_telemetry_remove_point(uint16_t point_id);

/* Data recording */
gf_telemetry_status_t gf_telemetry_record_analog(uint16_t point_id, float value);
gf_telemetry_status_t gf_telemetry_record_digital(uint16_t point_id, bool value);
gf_telemetry_status_t gf_telemetry_record_counter(uint16_t point_id, uint64_t value);

/* Data retrieval */
gf_telemetry_status_t gf_telemetry_get_latest(uint16_t point_id,
                                               gf_telemetry_value_t* value);
gf_telemetry_status_t gf_telemetry_get_history(uint16_t point_id,
                                                uint64_t start_ms,
                                                uint64_t end_ms,
                                                gf_telemetry_value_t* values,
                                                uint32_t* count);
gf_telemetry_status_t gf_telemetry_get_aggregate(uint16_t point_id,
                                                  uint64_t start_ms,
                                                  uint64_t end_ms,
                                                  gf_telemetry_aggregate_t* agg);

/* Production reporting */
gf_telemetry_status_t gf_telemetry_get_production(uint64_t start_ms,
                                                   uint64_t end_ms,
                                                   gf_telemetry_production_t* prod);

/* Transmission */
gf_telemetry_status_t gf_telemetry_connect(void);
gf_telemetry_status_t gf_telemetry_disconnect(void);
gf_telemetry_status_t gf_telemetry_flush(void);
bool gf_telemetry_is_connected(void);

/* Callbacks */
gf_telemetry_status_t gf_telemetry_register_value_callback(gf_telemetry_value_cb_t cb,
                                                            void* user_data);
gf_telemetry_status_t gf_telemetry_register_aggregate_callback(gf_telemetry_aggregate_cb_t cb,
                                                                void* user_data);

/* Periodic processing */
gf_telemetry_status_t gf_telemetry_process(void);

#endif /* GF_ENERGY_TELEMETRY_H */
