/**
 * @file traffic_telemetry.h
 * @brief Traffic Telemetry Collector for Urban Mobility
 * 
 * INDUSTRY RELEVANCE:
 * Smart city infrastructure requires real-time traffic data for congestion
 * management, route optimization, and infrastructure planning. Companies
 * like Waze, TomTom, HERE, and municipal IoT deployments need firmware
 * expertise for roadside units, vehicle telemetry, and edge analytics.
 * 
 * This module provides traffic data collection, aggregation, and reporting
 * for urban mobility systems including vehicles, pedestrians, and cyclists.
 * 
 * KEY CAPABILITIES:
 * - Vehicle count and classification
 * - Speed measurement
 * - Congestion index calculation
 * - Intersection timing data
 * - Pedestrian/cyclist detection
 * - Parking occupancy
 * - Emergency vehicle preemption
 * - V2X message handling
 * 
 * STANDARDS COMPLIANCE:
 * - NTCIP (Traffic control devices)
 * - SAE J2735 (V2X messages)
 * - ETSI ITS standards
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TRAFFIC_TELEMETRY_H
#define GF_TRAFFIC_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define TT_MAX_LANES           8     /**< Max lanes per intersection */
#define TT_MAX_DETECTORS       16    /**< Max detection zones */
#define TT_HISTORY_MINUTES     60    /**< History buffer size */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Vehicle class */
typedef enum {
    TT_CLASS_UNKNOWN,
    TT_CLASS_MOTORCYCLE,
    TT_CLASS_CAR,
    TT_CLASS_TRUCK,
    TT_CLASS_BUS,
    TT_CLASS_BICYCLE,
    TT_CLASS_PEDESTRIAN,
    TT_CLASS_EMERGENCY
} tt_vehicle_class_t;

/** Congestion level */
typedef enum {
    TT_FLOW_FREE,
    TT_FLOW_LIGHT,
    TT_FLOW_MODERATE,
    TT_FLOW_HEAVY,
    TT_FLOW_GRIDLOCK
} tt_congestion_t;

/** Lane metrics */
typedef struct {
    uint8_t lane_id;
    uint16_t vehicle_count;
    float avg_speed_kmh;
    float occupancy_pct;
    tt_congestion_t congestion;
} tt_lane_metrics_t;

/** Intersection status */
typedef struct {
    uint32_t intersection_id;
    uint32_t timestamp;
    tt_lane_metrics_t lanes[TT_MAX_LANES];
    uint8_t lane_count;
    uint16_t waiting_vehicles;
    float avg_delay_s;
    bool emergency_preempt;
} tt_intersection_t;

/** Traffic event */
typedef struct {
    uint32_t event_id;
    uint32_t timestamp;
    tt_vehicle_class_t vehicle_class;
    uint8_t lane_id;
    float speed_kmh;
    float length_m;
    bool wrong_way;
} tt_event_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize traffic telemetry
 * @param intersection_id Unique intersection ID
 * @return 0 on success
 */
int tt_init(uint32_t intersection_id);

/**
 * @brief Record vehicle detection
 * @param event Detection event
 * @return 0 on success
 */
int tt_record_event(const tt_event_t* event);

/**
 * @brief Get intersection status
 * @param status Output status
 * @return 0 on success
 */
int tt_get_status(tt_intersection_t* status);

/**
 * @brief Get congestion index
 * @param congestion Output congestion level
 * @return 0 on success
 */
int tt_get_congestion(tt_congestion_t* congestion);

/**
 * @brief Trigger emergency preemption
 * @param vehicle_id Emergency vehicle ID
 * @param approach_dir Approach direction
 * @return 0 on success
 */
int tt_emergency_preempt(uint32_t vehicle_id, uint8_t approach_dir);

/**
 * @brief Generate telemetry report
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @param len Output data length
 * @return 0 on success
 */
int tt_generate_report(uint8_t* buffer, uint16_t max_len, uint16_t* len);

/**
 * @brief Shutdown telemetry
 */
void tt_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TRAFFIC_TELEMETRY_H */
