/**
 * @file user_telemetry.h
 * @brief User Telemetry Collector for Entertainment Systems
 * 
 * INDUSTRY RELEVANCE:
 * Entertainment platforms collect anonymized usage data for analytics,
 * content optimization, and personalization. Privacy-preserving telemetry
 * is valuable for game studios, streaming services, and XR platform
 * providers. Companies like EA, Activision, Unity, and streaming platforms
 * need firmware that collects metrics while respecting user privacy.
 * 
 * This module provides privacy-aware telemetry collection for entertainment
 * systems including session data, performance metrics, and user preferences.
 * 
 * KEY CAPABILITIES:
 * - Session duration tracking
 * - Performance metrics (frame rate, latency)
 * - User comfort metrics (VR sickness indicators)
 * - Content engagement tracking
 * - Hardware utilization
 * - Error and crash reporting
 * - Opt-in/opt-out management
 * - Data anonymization
 * 
 * PRIVACY COMPLIANCE:
 * - GDPR data minimization
 * - CCPA opt-out support
 * - COPPA age verification hooks
 * - Differential privacy options
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_USER_TELEMETRY_H
#define GF_USER_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define UT_MAX_EVENTS          256   /**< Event queue size */
#define UT_SESSION_ID_LEN      16    /**< Session ID length */
#define UT_BATCH_SIZE          32    /**< Events per upload batch */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Consent level */
typedef enum {
    UT_CONSENT_NONE,       /**< No telemetry */
    UT_CONSENT_ESSENTIAL,  /**< Crash/error only */
    UT_CONSENT_ANALYTICS,  /**< Usage analytics */
    UT_CONSENT_FULL        /**< All telemetry */
} ut_consent_t;

/** Event category */
typedef enum {
    UT_CAT_SESSION,        /**< Session start/end */
    UT_CAT_PERFORMANCE,    /**< FPS, latency metrics */
    UT_CAT_COMFORT,        /**< VR comfort metrics */
    UT_CAT_CONTENT,        /**< Content engagement */
    UT_CAT_HARDWARE,       /**< Hardware status */
    UT_CAT_ERROR,          /**< Errors/crashes */
    UT_CAT_INPUT           /**< Input patterns */
} ut_category_t;

/** Performance metrics */
typedef struct {
    float avg_fps;
    float min_fps;
    float percentile_95_fps;
    float avg_latency_ms;
    float motion_to_photon_ms;
    float gpu_utilization_pct;
    float cpu_utilization_pct;
} ut_performance_t;

/** Comfort metrics (VR) */
typedef struct {
    float session_duration_min;
    uint16_t smooth_rotation_count;
    uint16_t snap_turn_count;
    float avg_locomotion_speed;
    uint8_t comfort_breaks;    /**< User-initiated pauses */
    bool discomfort_reported;
} ut_comfort_t;

/** Telemetry event */
typedef struct {
    uint32_t event_id;
    ut_category_t category;
    uint32_t timestamp;
    char event_name[32];
    float value;
    uint8_t data[32];      /**< Category-specific data */
} ut_event_t;

/** Session info */
typedef struct {
    uint8_t session_id[UT_SESSION_ID_LEN];
    uint32_t start_time;
    uint32_t duration_s;
    ut_performance_t performance;
    ut_comfort_t comfort;
    uint16_t events_logged;
    uint16_t errors_logged;
} ut_session_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize user telemetry
 * @param consent User consent level
 * @return 0 on success
 */
int ut_init(ut_consent_t consent);

/**
 * @brief Start new session
 * @param session Output session info
 * @return 0 on success
 */
int ut_start_session(ut_session_t* session);

/**
 * @brief End current session
 * @return 0 on success
 */
int ut_end_session(void);

/**
 * @brief Log telemetry event
 * @param event Event to log
 * @return 0 on success
 */
int ut_log_event(const ut_event_t* event);

/**
 * @brief Update performance metrics
 * @param perf Performance data
 * @return 0 on success
 */
int ut_update_performance(const ut_performance_t* perf);

/**
 * @brief Update comfort metrics
 * @param comfort Comfort data
 * @return 0 on success
 */
int ut_update_comfort(const ut_comfort_t* comfort);

/**
 * @brief Update consent level
 * @param consent New consent level
 * @return 0 on success
 */
int ut_set_consent(ut_consent_t consent);

/**
 * @brief Flush telemetry batch
 * @return 0 on success
 */
int ut_flush(void);

/**
 * @brief Get pending event count
 * @return Event count
 */
uint16_t ut_pending_count(void);

/**
 * @brief Delete all stored data
 * @return 0 on success
 */
int ut_delete_data(void);

/**
 * @brief Shutdown telemetry
 */
void ut_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_USER_TELEMETRY_H */
