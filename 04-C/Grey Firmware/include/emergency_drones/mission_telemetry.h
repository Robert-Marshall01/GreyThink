/**
 * @file mission_telemetry.h
 * @brief SAR Mission Telemetry Collector Stub
 * 
 * Industry Relevance:
 * Real-time mission telemetry is critical for SAR coordination, enabling
 * incident commanders to track multiple assets and coordinate rescues. Module provides:
 * - Multi-drone fleet telemetry aggregation
 * - Mission progress tracking and ETA estimation
 * - Victim status and extraction coordination
 * - Post-mission analytics for training improvement
 * 
 * Applications: Emergency operations centers, multi-agency coordination, after-action review
 * 
 * @author Grey Firmware Project
 */

#ifndef MISSION_TELEMETRY_H
#define MISSION_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Mission phase */
typedef enum {
    MISSION_PHASE_PLANNING,
    MISSION_PHASE_DEPLOYMENT,
    MISSION_PHASE_ACTIVE_SEARCH,
    MISSION_PHASE_RESCUE_OPS,
    MISSION_PHASE_RECOVERY,
    MISSION_PHASE_COMPLETE
} mission_phase_t;

/** Asset type in mission */
typedef enum {
    ASSET_DRONE_SAR,
    ASSET_DRONE_DELIVERY,
    ASSET_GROUND_TEAM,
    ASSET_HELICOPTER,
    ASSET_BOAT
} asset_type_t;

/** Asset status record */
typedef struct {
    uint16_t asset_id;       /**< Unique asset identifier */
    asset_type_t type;       /**< Asset type */
    double latitude;         /**< Current position */
    double longitude;
    float altitude_m;
    float battery_fuel_pct;  /**< Remaining battery/fuel */
    bool available;          /**< Currently available */
    uint32_t last_update;    /**< Last telemetry timestamp */
} asset_status_t;

/** Mission summary */
typedef struct {
    uint32_t mission_id;     /**< Unique mission ID */
    mission_phase_t phase;   /**< Current phase */
    uint32_t start_time;     /**< Mission start timestamp */
    uint32_t elapsed_s;      /**< Elapsed seconds */
    uint16_t asset_count;    /**< Total assets deployed */
    uint16_t victims_found;  /**< Victims detected */
    uint16_t victims_rescued;/**< Victims extracted */
    float area_covered_km2;  /**< Area searched (km²) */
} mission_summary_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize mission telemetry system
 * @return 0 on success, negative on error
 */
int mission_telemetry_init(void);

/**
 * @brief Start new mission
 * @param mission_id Unique mission identifier
 * @return 0 on success, negative on error
 */
int mission_telemetry_start(uint32_t mission_id);

/**
 * @brief Register asset in mission
 * @param status Asset status
 * @return 0 on success, negative on error
 */
int mission_telemetry_add_asset(const asset_status_t *status);

/**
 * @brief Update asset telemetry
 * @param asset_id Asset to update
 * @param status New status
 * @return 0 on success, negative on error
 */
int mission_telemetry_update_asset(uint16_t asset_id, const asset_status_t *status);

/**
 * @brief Get mission summary
 * @param summary Output summary
 * @return 0 on success, negative on error
 */
int mission_telemetry_get_summary(mission_summary_t *summary);

/**
 * @brief End mission and archive
 * @return 0 on success, negative on error
 */
int mission_telemetry_end(void);

/**
 * @brief Shutdown telemetry system
 */
void mission_telemetry_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* MISSION_TELEMETRY_H */
