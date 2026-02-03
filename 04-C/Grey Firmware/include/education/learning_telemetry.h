/**
 * @file learning_telemetry.h
 * @brief Learning Telemetry Collector for Smart Education Technology
 * 
 * @details
 * This module collects and aggregates learning analytics telemetry
 * from educational devices and systems, enabling data-driven
 * instruction and personalized learning.
 * 
 * INDUSTRY RELEVANCE:
 * - Learning Management Systems: Canvas, Blackboard, Moodle
 * - Adaptive Learning: McGraw-Hill ALEKS, Pearson MyLab
 * - Assessment Platforms: Khan Academy, IXL, Duolingo
 * - Corporate Training: Workday Learning, LinkedIn Learning
 * - K-12 Districts: PowerSchool, Infinite Campus integration
 * 
 * DATA CATEGORIES:
 * - Engagement metrics (time on task, interactions)
 * - Performance data (scores, completion rates)
 * - Behavioral patterns (learning paths, pacing)
 * - Collaboration metrics (group work, peer interaction)
 * - Environmental factors (classroom conditions)
 * 
 * COMPLIANCE:
 * - FERPA: Family Educational Rights and Privacy Act
 * - COPPA: Children's Online Privacy Protection Act
 * - GDPR: EU General Data Protection (for EU students)
 * - xAPI/Tin Can: Learning Experience API standard
 * - IMS Caliper: Learning analytics standard
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_LEARNING_TELEMETRY_H
#define GF_LEARNING_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum telemetry events queued */
#define GF_LTEL_MAX_QUEUE               1000

/** Maximum activities tracked */
#define GF_LTEL_MAX_ACTIVITIES          100

/** Student ID length */
#define GF_LTEL_ID_LENGTH               32

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Event type (xAPI-inspired)
 */
typedef enum {
    GF_LTEL_LAUNCHED,             /**< Activity launched */
    GF_LTEL_INITIALIZED,          /**< Activity initialized */
    GF_LTEL_PROGRESSED,           /**< Progress made */
    GF_LTEL_COMPLETED,            /**< Activity completed */
    GF_LTEL_PASSED,               /**< Assessment passed */
    GF_LTEL_FAILED,               /**< Assessment failed */
    GF_LTEL_ANSWERED,             /**< Question answered */
    GF_LTEL_INTERACTED,           /**< User interaction */
    GF_LTEL_SUSPENDED,            /**< Activity suspended */
    GF_LTEL_RESUMED,              /**< Activity resumed */
    GF_LTEL_TERMINATED            /**< Activity terminated */
} gf_ltel_event_type_t;

/**
 * @brief Activity type
 */
typedef enum {
    GF_ACTIVITY_LESSON,           /**< Lesson content */
    GF_ACTIVITY_QUIZ,             /**< Quiz/test */
    GF_ACTIVITY_SIMULATION,       /**< Simulation */
    GF_ACTIVITY_VIDEO,            /**< Video content */
    GF_ACTIVITY_READING,          /**< Reading assignment */
    GF_ACTIVITY_DISCUSSION,       /**< Discussion/forum */
    GF_ACTIVITY_PROJECT,          /**< Project work */
    GF_ACTIVITY_GAME              /**< Educational game */
} gf_activity_type_t;

/**
 * @brief Learning event
 */
typedef struct {
    gf_ltel_event_type_t type;    /**< Event type */
    char student_id[GF_LTEL_ID_LENGTH]; /**< Student identifier */
    char activity_id[GF_LTEL_ID_LENGTH]; /**< Activity identifier */
    gf_activity_type_t activity_type; /**< Activity type */
    uint32_t timestamp;           /**< Event timestamp */
    float score;                  /**< Score (if applicable) */
    float duration_s;             /**< Duration (seconds) */
    float progress_pct;           /**< Progress percentage */
    char context[64];             /**< Additional context */
} gf_learning_event_t;

/**
 * @brief Student engagement summary
 */
typedef struct {
    char student_id[GF_LTEL_ID_LENGTH];
    float total_time_s;           /**< Total time learning */
    uint16_t activities_completed; /**< Activities completed */
    uint16_t activities_attempted; /**< Activities attempted */
    float avg_score;              /**< Average score */
    float engagement_score;       /**< Engagement score (0-100) */
    float mastery_level;          /**< Mastery level (0-100) */
    uint32_t last_activity;       /**< Last activity timestamp */
} gf_student_summary_t;

/**
 * @brief Activity statistics
 */
typedef struct {
    char activity_id[GF_LTEL_ID_LENGTH];
    gf_activity_type_t type;      /**< Activity type */
    uint32_t attempt_count;       /**< Total attempts */
    uint32_t completion_count;    /**< Completions */
    float avg_score;              /**< Average score */
    float avg_duration_s;         /**< Average duration */
    float pass_rate;              /**< Pass rate (%) */
} gf_activity_stats_t;

/**
 * @brief Telemetry configuration
 */
typedef struct {
    bool anonymize;               /**< Anonymize student IDs */
    bool enable_realtime;         /**< Real-time streaming */
    uint16_t batch_size;          /**< Batch upload size */
    uint16_t flush_interval_s;    /**< Flush interval */
    char endpoint[64];            /**< Upload endpoint */
} gf_ltel_config_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize learning telemetry
 * @param config Telemetry configuration
 * @return 0 on success
 */
int gf_ltel_init(const gf_ltel_config_t* config);

/**
 * @brief Shutdown learning telemetry
 */
void gf_ltel_shutdown(void);

/**
 * @brief Record learning event
 * @param event Learning event
 * @return 0 on success
 */
int gf_ltel_record_event(const gf_learning_event_t* event);

/**
 * @brief Get student summary
 * @param student_id Student identifier
 * @param summary Output summary
 * @return 0 on success
 */
int gf_ltel_get_student_summary(const char* student_id,
                                 gf_student_summary_t* summary);

/**
 * @brief Get activity statistics
 * @param activity_id Activity identifier
 * @param stats Output statistics
 * @return 0 on success
 */
int gf_ltel_get_activity_stats(const char* activity_id,
                                gf_activity_stats_t* stats);

/**
 * @brief Flush queued events
 * @return Number of events flushed
 */
int gf_ltel_flush(void);

/**
 * @brief Export data (xAPI format)
 * @param start_time Start timestamp
 * @param end_time End timestamp
 * @param buffer Output buffer
 * @param buffer_size Buffer size
 * @return Bytes written
 */
int gf_ltel_export_xapi(uint32_t start_time, uint32_t end_time,
                         char* buffer, uint32_t buffer_size);

/**
 * @brief Process telemetry (call periodically)
 * @param delta_ms Time since last call
 */
void gf_ltel_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_LEARNING_TELEMETRY_H */
