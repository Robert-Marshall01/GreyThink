/**
 * @file performance_telemetry.h
 * @brief Performance Telemetry Collector for Sports Analytics
 *
 * INDUSTRY RELEVANCE:
 * Elite sports teams analyze terabytes of performance data to optimize
 * training and game-day decisions. This module demonstrates data aggregation
 * and real-time analytics pipelines used by professional teams and
 * sports science research facilities.
 *
 * Key capabilities demonstrated:
 * - Multi-athlete data aggregation
 * - Real-time metric computation (load, intensity, fatigue)
 * - Time-series compression for storage efficiency
 * - Live dashboard data streaming
 *
 * @note This is a stub header for portfolio demonstration.
 *
 * Copyright (c) 2025 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_PERFORMANCE_TELEMETRY_H
#define GF_PERFORMANCE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Performance metric types */
typedef enum {
    GF_METRIC_PLAYER_LOAD,          /**< Cumulative movement load */
    GF_METRIC_HIGH_INTENSITY_DIST,  /**< Distance >5.5 m/s */
    GF_METRIC_SPRINT_COUNT,         /**< Number of sprints */
    GF_METRIC_ACCELERATION_EVENTS,  /**< High accel/decel count */
    GF_METRIC_HEART_RATE_ZONE,      /**< Time in HR zones */
    GF_METRIC_FATIGUE_INDEX         /**< Computed fatigue score */
} gf_perf_metric_t;

/** Session summary */
typedef struct {
    uint32_t session_id;
    uint32_t athlete_id;
    uint64_t start_time;
    uint64_t end_time;
    float total_distance_m;
    float player_load;
    float high_intensity_distance_m;
    uint16_t sprint_count;
    uint16_t accel_count;
    uint16_t decel_count;
    float avg_heart_rate;
    float max_heart_rate;
    float fatigue_index;
} gf_session_summary_t;

/** Real-time team snapshot */
typedef struct {
    uint32_t team_id;
    uint64_t timestamp;
    uint8_t athlete_count;
    float avg_player_load;
    float avg_intensity;
    uint8_t fatigue_alerts;
} gf_team_snapshot_t;

/*===========================================================================*/
/* Function Prototypes (Stub)                                                */
/*===========================================================================*/

int gf_perf_tlm_init(uint32_t team_id);
int gf_perf_tlm_start_session(uint32_t athlete_id, uint32_t* session_id);
int gf_perf_tlm_end_session(uint32_t session_id, gf_session_summary_t* summary);
int gf_perf_tlm_get_metric(uint32_t athlete_id, gf_perf_metric_t metric, float* value);
int gf_perf_tlm_get_team_snapshot(gf_team_snapshot_t* snapshot);
void gf_perf_tlm_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PERFORMANCE_TELEMETRY_H */
