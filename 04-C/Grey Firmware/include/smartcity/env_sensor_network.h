/**
 * @file env_sensor_network.h
 * @brief Environmental Sensor Network Interface
 * 
 * INDUSTRY RELEVANCE:
 * Urban environmental monitoring is critical for:
 * - Air quality management (EPA/WHO pollution standards)
 * - Noise pollution monitoring for urban planning
 * - Urban heat island effect tracking
 * - Flood and weather early warning systems
 * - Public health data for epidemiological studies
 * - Climate change impact assessment in urban areas
 * 
 * This module demonstrates expertise in:
 * - Multi-sensor mesh network coordination
 * - Low-power wide-area network (LPWAN) protocols
 * - Sensor calibration and quality assurance
 * - Edge aggregation and data fusion
 * - Integration with municipal data platforms
 */

#ifndef GF_ENV_SENSOR_NETWORK_H
#define GF_ENV_SENSOR_NETWORK_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_ESN_MAX_NODES        64
#define GF_ESN_MAX_SENSORS      8

typedef enum {
    GF_ESN_OK = 0,
    GF_ESN_ERROR_NOT_INITIALIZED,
    GF_ESN_ERROR_NODE_NOT_FOUND,
    GF_ESN_ERROR_NETWORK_FAULT,
    GF_ESN_ERROR_SENSOR_FAULT,
    GF_ESN_ERROR_DATA_STALE
} gf_esn_status_t;

typedef enum {
    GF_ESN_SENSOR_TEMPERATURE,
    GF_ESN_SENSOR_HUMIDITY,
    GF_ESN_SENSOR_PM25,
    GF_ESN_SENSOR_PM10,
    GF_ESN_SENSOR_CO2,
    GF_ESN_SENSOR_NO2,
    GF_ESN_SENSOR_O3,
    GF_ESN_SENSOR_NOISE_DB,
    GF_ESN_SENSOR_UV_INDEX,
    GF_ESN_SENSOR_RAINFALL
} gf_esn_sensor_type_t;

typedef struct {
    gf_esn_sensor_type_t type;
    float value;
    uint8_t quality;
    uint32_t timestamp_ms;
} gf_esn_reading_t;

typedef struct {
    uint16_t node_id;
    float latitude;
    float longitude;
    uint8_t sensor_count;
    gf_esn_reading_t sensors[GF_ESN_MAX_SENSORS];
    uint8_t link_quality;
    uint8_t battery_pct;
    uint32_t last_seen_ms;
} gf_esn_node_t;

typedef struct {
    uint16_t gateway_id;
    uint16_t network_id;
    uint8_t channel;
    uint16_t report_interval_s;
    bool mesh_enabled;
    bool aggregation_enabled;
} gf_esn_config_t;

gf_esn_status_t gf_esn_init(const gf_esn_config_t* config);
void gf_esn_shutdown(void);
gf_esn_status_t gf_esn_discover_nodes(uint16_t* count);
gf_esn_status_t gf_esn_get_node(uint16_t node_id, gf_esn_node_t* node);
gf_esn_status_t gf_esn_read_sensor(uint16_t node_id, gf_esn_sensor_type_t type, float* value);
gf_esn_status_t gf_esn_get_aggregate(gf_esn_sensor_type_t type, float* avg, float* min, float* max);
gf_esn_status_t gf_esn_set_alert_threshold(gf_esn_sensor_type_t type, float threshold);
uint16_t gf_esn_calculate_aqi(float pm25, float pm10, float o3, float no2);

#ifdef __cplusplus
}
#endif

#endif /* GF_ENV_SENSOR_NETWORK_H */
