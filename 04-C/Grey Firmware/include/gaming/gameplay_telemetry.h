/**
 * @file gameplay_telemetry.h
 * @brief Gaming Telemetry Collection Module
 * 
 * INDUSTRY RELEVANCE:
 * Game analytics drives monetization, balance tuning, and player
 * retention. Telemetry systems collect millions of events per second
 * from global player bases. Companies like Unity, Epic, and major
 * publishers invest heavily in analytics infrastructure.
 * 
 * This module tracks:
 * - Session metrics (duration, frequency)
 * - Performance data (FPS, latency, crashes)
 * - Player behavior (actions, progression)
 * - Matchmaking quality
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_GAMEPLAY_TELEMETRY_H
#define GF_GAMEPLAY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    TELEM_EVENT_SESSION_START,
    TELEM_EVENT_SESSION_END,
    TELEM_EVENT_LEVEL_START,
    TELEM_EVENT_LEVEL_END,
    TELEM_EVENT_DEATH,
    TELEM_EVENT_ACHIEVEMENT,
    TELEM_EVENT_PURCHASE,
    TELEM_EVENT_ERROR
} gameplay_event_t;

typedef struct {
    gameplay_event_t type;
    uint64_t timestamp_ms;
    uint32_t session_id;
    uint32_t player_id;
    char event_data[128];
    float x, y, z;         /**< Player position */
    uint32_t level_id;
    float play_time_s;
} telemetry_event_t;

typedef struct {
    float avg_fps;
    float min_fps;
    uint32_t frame_drops;
    float latency_ms;
    uint32_t disconnects;
    float crash_rate;
} performance_metrics_t;

int gameplay_telem_init(void);
int gameplay_telem_start_session(uint32_t player_id);
int gameplay_telem_end_session(void);
int gameplay_telem_log_event(const telemetry_event_t *event);
int gameplay_telem_get_performance(performance_metrics_t *metrics);
int gameplay_telem_flush(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_GAMEPLAY_TELEMETRY_H */
