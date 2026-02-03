/**
 * @file traffic_telemetry.h
 * @brief Traffic Telemetry Collector Stub
 * 
 * INDUSTRY RELEVANCE:
 * Smart city infrastructure relies on real-time traffic data from embedded
 * sensors for congestion management, signal timing optimization, and emergency
 * vehicle preemption. The global smart transportation market exceeds $130B.
 * Firmware engineers work with vehicle detection sensors (inductive loops,
 * radar, cameras), V2X communication, and edge analytics.
 * 
 * Key challenges:
 * - High-reliability operation (99.99% uptime requirements)
 * - Multi-sensor fusion for accuracy
 * - Real-time data transmission to traffic management centers
 * - Harsh environmental conditions (temperature, vibration)
 * - Long deployment lifecycles (10+ years)
 */

#ifndef GF_TRAFFIC_TELEMETRY_H
#define GF_TRAFFIC_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Telemetry collector status codes */
typedef enum {
    GF_TRAFFIC_OK = 0,
    GF_TRAFFIC_SENSOR_FAULT,        /* Sensor hardware failure */
    GF_TRAFFIC_COMM_ERROR,          /* Communication failure */
    GF_TRAFFIC_CALIBRATION_NEEDED,  /* Sensor needs recalibration */
    GF_TRAFFIC_BUFFER_FULL,         /* Local buffer overflow */
    GF_TRAFFIC_TIMESTAMP_ERROR,     /* Time synchronization lost */
    GF_TRAFFIC_POWER_LOW            /* Backup power active */
} gf_traffic_status_t;

/* Vehicle detection sensor types */
typedef enum {
    GF_SENSOR_INDUCTIVE_LOOP,       /* In-ground inductive loop */
    GF_SENSOR_RADAR,                /* Microwave radar detector */
    GF_SENSOR_LIDAR,                /* Light detection and ranging */
    GF_SENSOR_CAMERA,               /* Video detection */
    GF_SENSOR_ACOUSTIC,             /* Sound-based detection */
    GF_SENSOR_MAGNETOMETER,         /* Magnetic field detector */
    GF_SENSOR_INFRARED              /* Passive IR detector */
} gf_traffic_sensor_t;

/* Vehicle classification */
typedef enum {
    GF_VEHICLE_UNKNOWN,
    GF_VEHICLE_MOTORCYCLE,
    GF_VEHICLE_PASSENGER_CAR,
    GF_VEHICLE_SUV_TRUCK,
    GF_VEHICLE_BUS,
    GF_VEHICLE_COMMERCIAL_TRUCK,
    GF_VEHICLE_SEMI_TRAILER,
    GF_VEHICLE_EMERGENCY,
    GF_VEHICLE_BICYCLE,
    GF_VEHICLE_PEDESTRIAN
} gf_vehicle_class_t;

/* Collector configuration */
typedef struct {
    uint8_t lane_count;             /* Number of monitored lanes */
    gf_traffic_sensor_t sensors[4]; /* Sensor types per lane */
    uint16_t report_interval_sec;   /* Telemetry report interval */
    bool enable_classification;     /* Vehicle classification enabled */
    bool enable_speed_detection;    /* Speed measurement enabled */
    char intersection_id[16];       /* Intersection identifier */
    float latitude;                 /* GPS latitude */
    float longitude;                /* GPS longitude */
} gf_traffic_config_t;

/* Single vehicle detection event */
typedef struct {
    uint64_t timestamp_ms;          /* Detection timestamp */
    uint8_t lane_id;                /* Lane number (0-based) */
    gf_vehicle_class_t vehicle_class; /* Vehicle classification */
    float speed_kmh;                /* Vehicle speed */
    uint16_t length_cm;             /* Vehicle length estimate */
    uint8_t occupancy_percent;      /* Lane occupancy */
    bool direction_inbound;         /* Travel direction */
} gf_vehicle_event_t;

/* Aggregated traffic statistics */
typedef struct {
    uint64_t period_start;          /* Aggregation period start */
    uint32_t period_seconds;        /* Aggregation period length */
    uint32_t vehicle_count;         /* Total vehicles detected */
    float avg_speed_kmh;            /* Average speed */
    float avg_occupancy;            /* Average lane occupancy */
    uint16_t class_counts[10];      /* Count by vehicle class */
    float density_vehicles_km;      /* Traffic density */
    gf_traffic_status_t status;     /* Collector status */
} gf_traffic_stats_t;

/**
 * @brief Initialize traffic telemetry collector
 * @param config Collector configuration
 * @return Status code
 */
gf_traffic_status_t gf_traffic_init(const gf_traffic_config_t* config);

/**
 * @brief Start data collection
 * @return Status code
 */
gf_traffic_status_t gf_traffic_start(void);

/**
 * @brief Stop data collection
 * @return Status code
 */
gf_traffic_status_t gf_traffic_stop(void);

/**
 * @brief Get latest vehicle event
 * @param event Output for vehicle event
 * @return Status code
 */
gf_traffic_status_t gf_traffic_get_event(gf_vehicle_event_t* event);

/**
 * @brief Get aggregated traffic statistics
 * @param stats Output for statistics
 * @param period_seconds Aggregation period
 * @return Status code
 */
gf_traffic_status_t gf_traffic_get_stats(gf_traffic_stats_t* stats, uint32_t period_seconds);

/**
 * @brief Calibrate sensor for lane
 * @param lane_id Lane number
 * @param reference_length Reference vehicle length for calibration
 * @return Status code
 */
gf_traffic_status_t gf_traffic_calibrate(uint8_t lane_id, uint16_t reference_length);

/**
 * @brief Transmit telemetry to central server
 * @param stats Statistics to transmit
 * @return Status code
 */
gf_traffic_status_t gf_traffic_transmit(const gf_traffic_stats_t* stats);

/**
 * @brief Perform sensor self-test
 * @return Status code (OK if passed)
 */
gf_traffic_status_t gf_traffic_self_test(void);

/**
 * @brief Shutdown and release resources
 */
void gf_traffic_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TRAFFIC_TELEMETRY_H */
