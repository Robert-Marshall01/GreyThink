/**
 * @file redundancy_checker.h
 * @brief Redundancy Checker for Mission-Critical Systems Stub
 * 
 * INDUSTRY RELEVANCE:
 * Mission-critical systems in aerospace, defense, and nuclear require
 * redundant processing with voting mechanisms to ensure correct operation
 * despite hardware faults. Standards like DO-178C (avionics) and IEC 61508
 * (functional safety) mandate redundancy for safety-critical functions.
 * This domain includes triple modular redundancy (TMR), lockstep processing,
 * and Byzantine fault tolerance.
 * 
 * Key challenges:
 * - Sub-microsecond synchronization between redundant units
 * - Voting algorithm design for fault masking
 * - Common-cause failure prevention
 * - Graceful degradation strategies
 * - Real-time fault detection and isolation
 */

#ifndef GF_REDUNDANCY_CHECKER_H
#define GF_REDUNDANCY_CHECKER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Redundancy checker status codes */
typedef enum {
    GF_REDUND_OK = 0,
    GF_REDUND_SINGLE_FAULT,         /* One unit disagrees (masked) */
    GF_REDUND_DOUBLE_FAULT,         /* Two units disagree (degraded) */
    GF_REDUND_TOTAL_FAULT,          /* All units disagree (failure) */
    GF_REDUND_SYNC_LOST,            /* Synchronization lost */
    GF_REDUND_TIMEOUT,              /* Unit response timeout */
    GF_REDUND_CONFIG_ERROR,         /* Configuration mismatch */
    GF_REDUND_DEGRADED              /* Operating with reduced redundancy */
} gf_redund_status_t;

/* Redundancy architecture types */
typedef enum {
    GF_REDUND_DUAL,                 /* Dual redundancy (fail-safe) */
    GF_REDUND_TMR,                  /* Triple modular redundancy */
    GF_REDUND_QUAD,                 /* Quadruple redundancy */
    GF_REDUND_LOCKSTEP,             /* Lockstep dual execution */
    GF_REDUND_ACTIVE_STANDBY        /* Active/standby hot spare */
} gf_redund_arch_t;

/* Voting strategies */
typedef enum {
    GF_VOTE_MAJORITY,               /* Simple majority voting */
    GF_VOTE_MEDIAN,                 /* Median value selection */
    GF_VOTE_AVERAGE,                /* Average with outlier rejection */
    GF_VOTE_WEIGHTED,               /* Weighted by confidence */
    GF_VOTE_FIRST_VALID             /* First valid response */
} gf_redund_vote_t;

/* Fault handling actions */
typedef enum {
    GF_FAULT_MASK,                  /* Mask fault, continue operation */
    GF_FAULT_ISOLATE,               /* Isolate faulty unit */
    GF_FAULT_RECOVER,               /* Attempt recovery */
    GF_FAULT_SWITCH,                /* Switch to backup */
    GF_FAULT_HALT                   /* Safe halt */
} gf_redund_fault_action_t;

/* Redundancy configuration */
typedef struct {
    gf_redund_arch_t architecture;  /* Redundancy architecture */
    gf_redund_vote_t voting;        /* Voting strategy */
    uint8_t unit_count;             /* Number of redundant units */
    uint8_t min_units;              /* Minimum for operation */
    uint32_t sync_interval_us;      /* Sync interval in microseconds */
    uint32_t timeout_us;            /* Response timeout */
    gf_redund_fault_action_t fault_action; /* Default fault action */
} gf_redund_config_t;

/* Unit status */
typedef struct {
    uint8_t unit_id;                /* Unit identifier */
    bool healthy;                   /* Unit health status */
    bool synchronized;              /* Sync status */
    uint32_t fault_count;           /* Cumulative faults */
    uint64_t last_response_time;    /* Last response timestamp */
    uint32_t response_latency_us;   /* Response latency */
    float confidence;               /* Unit confidence 0.0-1.0 */
} gf_redund_unit_status_t;

/* Voting result */
typedef struct {
    bool consensus;                 /* All units agreed */
    uint8_t agreeing_units;         /* Number in agreement */
    uint8_t dissenting_units;       /* Number disagreeing */
    uint8_t dissenter_ids[4];       /* IDs of dissenters */
    gf_redund_status_t status;      /* Overall status */
} gf_redund_vote_result_t;

/* System health summary */
typedef struct {
    gf_redund_status_t status;      /* Current system status */
    uint8_t healthy_units;          /* Number of healthy units */
    uint8_t total_units;            /* Total units configured */
    uint32_t total_votes;           /* Total voting cycles */
    uint32_t disagreements;         /* Total disagreements */
    float availability;             /* System availability */
    bool degraded_mode;             /* Operating degraded */
} gf_redund_health_t;

/**
 * @brief Initialize redundancy checker
 * @param config Redundancy configuration
 * @return Status code
 */
gf_redund_status_t gf_redund_init(const gf_redund_config_t* config);

/**
 * @brief Register computation for redundant execution
 * @param func_id Function identifier
 * @param input Input data
 * @param input_size Input size in bytes
 * @return Status code
 */
gf_redund_status_t gf_redund_register_computation(uint32_t func_id, const void* input, size_t input_size);

/**
 * @brief Execute voting on computation results
 * @param func_id Function identifier
 * @param result Voted result output
 * @param result_size Result buffer size
 * @param vote_result Voting outcome
 * @return Status code
 */
gf_redund_status_t gf_redund_vote(uint32_t func_id, void* result, size_t result_size, gf_redund_vote_result_t* vote_result);

/**
 * @brief Force synchronization of all units
 * @return Status code
 */
gf_redund_status_t gf_redund_sync(void);

/**
 * @brief Get status of specific unit
 * @param unit_id Unit identifier
 * @param status Output for unit status
 * @return Status code
 */
gf_redund_status_t gf_redund_get_unit_status(uint8_t unit_id, gf_redund_unit_status_t* status);

/**
 * @brief Get overall system health
 * @param health Output for health summary
 * @return Status code
 */
gf_redund_status_t gf_redund_get_health(gf_redund_health_t* health);

/**
 * @brief Isolate faulty unit
 * @param unit_id Unit to isolate
 * @return Status code
 */
gf_redund_status_t gf_redund_isolate_unit(uint8_t unit_id);

/**
 * @brief Attempt unit recovery
 * @param unit_id Unit to recover
 * @return Status code
 */
gf_redund_status_t gf_redund_recover_unit(uint8_t unit_id);

/**
 * @brief Perform redundancy self-test
 * @return Status code (OK if passed)
 */
gf_redund_status_t gf_redund_self_test(void);

/**
 * @brief Shutdown redundancy checker
 */
void gf_redund_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_REDUNDANCY_CHECKER_H */
