/**
 * @file performance_telemetry.h
 * @brief Musical Performance Telemetry Collector
 * 
 * INDUSTRY RELEVANCE:
 * Performance telemetry enables:
 * - Live show visualization and lighting sync
 * - Audience engagement analytics for streaming
 * - Practice session analysis for education
 * - Sound engineer automation
 * - Session recording metadata capture
 * 
 * Key for live streaming platforms, music education apps, and venue technology.
 * Connected music platform users: 400M+ globally
 * 
 * STANDARDS: MIDI 2.0, OSC, Ableton Link
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_PERFORMANCE_TELEMETRY_H
#define GF_PERFORMANCE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Performance metrics */
typedef struct {
    float tempo_bpm;
    uint8_t time_signature_num;
    uint8_t time_signature_denom;
    uint32_t beat_count;
    uint32_t bar_count;
    float dynamic_level;      /**< 0-1 average loudness */
    float expression_intensity;
    uint16_t notes_per_minute;
} perf_metrics_t;

/* Energy levels for visualization */
typedef struct {
    float bass_energy;
    float mid_energy;
    float high_energy;
    float overall_energy;
    uint8_t spectral_centroid;
    float tempo_stability;
} energy_analysis_t;

/* Session statistics */
typedef struct {
    uint32_t session_start;
    uint32_t duration_sec;
    uint32_t total_notes;
    float avg_velocity;
    float velocity_variance;
    float timing_accuracy;    /**< 0-1 how close to grid */
    uint16_t key_signature;
    uint8_t most_played_note;
    float practice_score;     /**< 0-100 for education */
} session_stats_t;

/* Audience engagement (for streaming) */
typedef struct {
    uint32_t viewer_count;
    uint32_t reactions_per_min;
    float engagement_score;
    float energy_correlation;  /**< How audience reacts to energy */
} audience_data_t;

/**
 * @brief Initialize performance telemetry
 * @param sample_rate Audio sample rate
 * @return 0 on success
 */
int perf_telem_init(uint32_t sample_rate);

/**
 * @brief Start recording session
 * @param session_name Name for the session
 * @return 0 on success
 */
int perf_telem_start_session(const char *session_name);

/**
 * @brief Update with audio analysis
 * @param energy Energy analysis data
 * @return 0 on success
 */
int perf_telem_update_audio(const energy_analysis_t *energy);

/**
 * @brief Update with note events
 * @param note MIDI note number
 * @param velocity Note velocity
 * @param timestamp_us Event timestamp
 * @return 0 on success
 */
int perf_telem_note_event(uint8_t note, uint8_t velocity, uint32_t timestamp_us);

/**
 * @brief Get current performance metrics
 * @param metrics Output metrics structure
 * @return 0 on success
 */
int perf_telem_get_metrics(perf_metrics_t *metrics);

/**
 * @brief Get session statistics
 * @param stats Output statistics structure
 * @return 0 on success
 */
int perf_telem_get_stats(session_stats_t *stats);

/**
 * @brief Update audience data (for streaming)
 * @param audience Audience engagement data
 * @return 0 on success
 */
int perf_telem_update_audience(const audience_data_t *audience);

/**
 * @brief Generate telemetry packet
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written
 */
int perf_telem_generate(uint8_t *buffer, size_t max_len);

/**
 * @brief End session and finalize stats
 * @return 0 on success
 */
int perf_telem_end_session(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PERFORMANCE_TELEMETRY_H */
