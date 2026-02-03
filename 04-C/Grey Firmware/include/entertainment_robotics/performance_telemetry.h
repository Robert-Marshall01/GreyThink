/**
 * @file performance_telemetry.h
 * @brief Performance Telemetry Collector Stub
 * 
 * Industry Relevance:
 * Entertainment venues require detailed performance analytics for show
 * optimization, maintenance scheduling, and guest experience. Module provides:
 * - Real-time show execution metrics
 * - Equipment utilization and wear tracking
 * - Audience engagement correlation data
 * - Predictive maintenance for ride/show systems
 * 
 * Applications: Theme parks, live entertainment, museums, experiential venues
 * 
 * @author Grey Firmware Project
 */

#ifndef PERFORMANCE_TELEMETRY_H
#define PERFORMANCE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Show performance status */
typedef enum {
    SHOW_STATUS_READY,
    SHOW_STATUS_PRESHOW,
    SHOW_STATUS_RUNNING,
    SHOW_STATUS_INTERMISSION,
    SHOW_STATUS_COMPLETE,
    SHOW_STATUS_MAINTENANCE
} show_status_t;

/** Equipment category */
typedef enum {
    EQUIP_LIGHTING,
    EQUIP_AUDIO,
    EQUIP_MOTION,
    EQUIP_EFFECTS,
    EQUIP_PROJECTION,
    EQUIP_ANIMATRONIC
} equipment_category_t;

/** Show performance metrics */
typedef struct {
    uint32_t show_id;        /**< Unique show identifier */
    show_status_t status;    /**< Current status */
    uint32_t start_time;     /**< Show start timestamp */
    uint32_t duration_s;     /**< Expected duration */
    uint16_t cues_executed;  /**< Cues executed */
    uint16_t cues_total;     /**< Total cues */
    float timing_accuracy;   /**< Timing accuracy (percentage) */
    uint8_t equipment_faults;/**< Equipment faults during show */
} show_metrics_t;

/** Equipment utilization record */
typedef struct {
    uint16_t equipment_id;   /**< Equipment identifier */
    equipment_category_t category; /**< Equipment category */
    uint32_t runtime_hours;  /**< Total runtime hours */
    uint32_t cycle_count;    /**< Total cycles */
    float wear_percentage;   /**< Estimated wear (0-100) */
    uint32_t last_maintenance;/**< Last maintenance timestamp */
    uint32_t next_maintenance;/**< Predicted next maintenance */
} equipment_utilization_t;

/** Daily summary */
typedef struct {
    uint32_t date;           /**< Date (YYYYMMDD) */
    uint16_t shows_completed;/**< Shows completed */
    uint16_t show_faults;    /**< Shows with faults */
    float avg_timing;        /**< Average timing accuracy */
    uint32_t guest_count;    /**< Guest count (if available) */
    float uptime_pct;        /**< System uptime percentage */
} daily_summary_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize performance telemetry
 * @return 0 on success, negative on error
 */
int performance_telemetry_init(void);

/**
 * @brief Start show tracking
 * @param show_id Unique show identifier
 * @return 0 on success, negative on error
 */
int performance_telemetry_start_show(uint32_t show_id);

/**
 * @brief Update show metrics (call during show)
 * @param elapsed_ms Time since last update
 * @return 0 on success, negative on error
 */
int performance_telemetry_update(uint32_t elapsed_ms);

/**
 * @brief Record cue execution
 * @param cue_number Cue number
 * @param timing_error Timing error (ms, can be negative)
 * @return 0 on success, negative on error
 */
int performance_telemetry_record_cue(uint16_t cue_number, int32_t timing_error);

/**
 * @brief End show tracking
 * @param metrics Output show metrics
 * @return 0 on success, negative on error
 */
int performance_telemetry_end_show(show_metrics_t *metrics);

/**
 * @brief Get equipment utilization
 * @param equipment_id Equipment to query
 * @param util Output utilization data
 * @return 0 on success, negative on error
 */
int performance_telemetry_get_equipment(uint16_t equipment_id, equipment_utilization_t *util);

/**
 * @brief Get daily summary
 * @param date Date to query (YYYYMMDD)
 * @param summary Output summary
 * @return 0 on success, negative on error
 */
int performance_telemetry_get_daily(uint32_t date, daily_summary_t *summary);

/**
 * @brief Shutdown telemetry system
 */
void performance_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* PERFORMANCE_TELEMETRY_H */
