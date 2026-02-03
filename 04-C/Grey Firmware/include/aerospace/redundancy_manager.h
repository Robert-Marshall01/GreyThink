/**
 * @file redundancy_manager.h
 * @brief Redundancy Manager for Critical Avionics Systems
 *
 * INDUSTRY RELEVANCE:
 * Flight-critical systems require multiple levels of redundancy:
 * - Triple/Quad Modular Redundancy (TMR/QMR)
 * - Cross-channel data exchange and voting
 * - Byzantine fault tolerance
 * - Dissimilar redundancy for common-mode failures
 * - DO-178C compliance for DAL-A systems
 *
 * Used in: Flight control computers, engine controllers, autopilots
 *
 * @note This is a stub demonstrating aerospace redundancy patterns.
 */

#ifndef GF_REDUNDANCY_MANAGER_H
#define GF_REDUNDANCY_MANAGER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Redundancy Type Definitions                                               */
/*===========================================================================*/

/**
 * @brief Maximum number of redundant channels
 */
#define GF_REDUNDANCY_MAX_CHANNELS  4

/**
 * @brief Redundancy voting schemes
 */
typedef enum {
    GF_VOTE_MAJORITY,           /**< Majority voting (2-of-3, 3-of-4) */
    GF_VOTE_MEDIAN,             /**< Select median value */
    GF_VOTE_AVERAGE,            /**< Average with outlier rejection */
    GF_VOTE_FIRST_VALID,        /**< Use first valid channel */
    GF_VOTE_WEIGHTED            /**< Health-weighted average */
} gf_vote_scheme_t;

/**
 * @brief Channel health status
 */
typedef enum {
    GF_CHANNEL_HEALTHY,         /**< Channel operating normally */
    GF_CHANNEL_DEGRADED,        /**< Channel has minor issues */
    GF_CHANNEL_FAILED,          /**< Channel has failed */
    GF_CHANNEL_OFFLINE          /**< Channel not available */
} gf_channel_health_t;

/**
 * @brief Redundancy mode
 */
typedef enum {
    GF_REDUN_ACTIVE_ACTIVE,     /**< All channels active (voting) */
    GF_REDUN_ACTIVE_STANDBY,    /**< Primary + hot standby */
    GF_REDUN_ACTIVE_COLD,       /**< Primary + cold standby */
    GF_REDUN_DUPLEX             /**< Two-channel cross-check */
} gf_tmr_mode_t;

/**
 * @brief Channel data (generic)
 */
typedef struct {
    uint8_t channel_id;             /**< Channel identifier (0-3) */
    float value;                    /**< Channel value */
    uint64_t timestamp_us;          /**< Measurement timestamp */
    float confidence;               /**< Confidence 0.0-1.0 */
    gf_channel_health_t health;     /**< Channel health */
    bool valid;                     /**< Data valid flag */
} gf_channel_data_t;

/**
 * @brief Voted result
 */
typedef struct {
    float value;                    /**< Voted value */
    float confidence;               /**< Overall confidence */
    uint8_t valid_channels;         /**< Number of valid channels */
    uint8_t failed_channels;        /**< Bitmask of failed channels */
    bool consensus;                 /**< Consensus achieved */
    int8_t disagreement_channel;    /**< Channel that disagreed (-1 if none) */
} gf_voted_result_t;

/**
 * @brief Channel configuration
 */
typedef struct {
    uint8_t channel_count;          /**< Number of channels (2-4) */
    gf_vote_scheme_t vote_scheme;   /**< Voting algorithm */
    gf_tmr_mode_t mode;      /**< Redundancy mode */
    float agreement_threshold;      /**< Max deviation for agreement */
    float weight[GF_REDUNDANCY_MAX_CHANNELS];  /**< Channel weights */
    uint32_t stale_timeout_us;      /**< Data staleness timeout */
    bool allow_degraded;            /**< Allow degraded operation */
} gf_tmr_config_t;

/**
 * @brief Redundancy manager status
 */
typedef struct {
    gf_channel_health_t channel_health[GF_REDUNDANCY_MAX_CHANNELS];
    uint8_t healthy_channels;
    uint8_t active_channel;         /**< Primary active channel */
    uint32_t votes_performed;
    uint32_t disagreements;
    uint32_t failovers;
    bool system_healthy;
    bool degraded_mode;
} gf_tmr_status_t;

/**
 * @brief Redundancy manager handle
 */
typedef struct gf_tmr_mgr* gf_tmr_mgr_t;

/*===========================================================================*/
/* Redundancy Manager API                                                    */
/*===========================================================================*/

/**
 * @brief Initialize redundancy manager
 * @param config Configuration parameters
 * @param mgr Output handle
 * @return 0 on success
 */
int gf_tmr_init(const gf_tmr_config_t* config,
                        gf_tmr_mgr_t* mgr);

/**
 * @brief Update channel data
 * @param mgr Redundancy manager handle
 * @param channel_id Channel identifier (0 to count-1)
 * @param data Channel data
 * @return 0 on success
 */
int gf_tmr_update(gf_tmr_mgr_t mgr,
                          uint8_t channel_id,
                          const gf_channel_data_t* data);

/**
 * @brief Perform voting and get result
 * @param mgr Redundancy manager handle
 * @param result Output voted result
 * @return 0 on success, negative on no consensus
 */
int gf_tmr_vote(gf_tmr_mgr_t mgr,
                        gf_voted_result_t* result);

/**
 * @brief Set channel health status
 * @param mgr Redundancy manager handle
 * @param channel_id Channel identifier
 * @param health Health status
 * @return 0 on success
 */
int gf_tmr_set_health(gf_tmr_mgr_t mgr,
                              uint8_t channel_id,
                              gf_channel_health_t health);

/**
 * @brief Force failover to specific channel
 * @param mgr Redundancy manager handle
 * @param channel_id Target channel
 * @return 0 on success
 */
int gf_tmr_failover(gf_tmr_mgr_t mgr,
                            uint8_t channel_id);

/**
 * @brief Get redundancy status
 * @param mgr Redundancy manager handle
 * @param status Output status
 * @return 0 on success
 */
int gf_tmr_get_status(gf_tmr_mgr_t mgr,
                              gf_tmr_status_t* status);

/**
 * @brief Run cross-channel check
 * @param mgr Redundancy manager handle
 * @param mismatches Output: number of mismatches
 * @return 0 on success
 */
int gf_tmr_cross_check(gf_tmr_mgr_t mgr,
                               uint8_t* mismatches);

/**
 * @brief Deinitialize redundancy manager
 * @param mgr Redundancy manager handle
 */
void gf_tmr_deinit(gf_tmr_mgr_t mgr);

#ifdef __cplusplus
}
#endif

#endif /* GF_REDUNDANCY_MANAGER_H */
