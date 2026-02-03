/**
 * @file consensus.h
 * @brief Consensus Simulation Module Interface
 * 
 * INDUSTRY RELEVANCE:
 * Distributed consensus enables trustless coordination in IoT networks:
 * - Decentralized sensor network agreement on readings
 * - Byzantine fault tolerance for mission-critical systems
 * - Distributed ledger for multi-party industrial tracking
 * - Autonomous swarm decision making (drones, robots)
 * - Multi-vendor supply chain coordination
 * - Decentralized energy grid load balancing
 * 
 * This module demonstrates expertise in:
 * - Byzantine Fault Tolerant (BFT) algorithms
 * - Raft consensus for leader election
 * - Practical BFT (PBFT) for permissioned networks
 * - Proof of Authority for IoT networks
 * - Consensus timing and finality guarantees
 * - Network partition handling
 */

#ifndef GF_CONSENSUS_H
#define GF_CONSENSUS_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_CON_MAX_VALIDATORS       32
#define GF_CON_MAX_PROPOSALS        16
#define GF_CON_HASH_SIZE            32

typedef enum {
    GF_CON_OK = 0,
    GF_CON_ERROR_NOT_INITIALIZED,
    GF_CON_ERROR_NO_QUORUM,
    GF_CON_ERROR_TIMEOUT,
    GF_CON_ERROR_INVALID_PROPOSAL,
    GF_CON_ERROR_INVALID_VOTE,
    GF_CON_ERROR_NOT_LEADER,
    GF_CON_ERROR_BYZANTINE_FAULT,
    GF_CON_ERROR_NETWORK_PARTITION
} gf_con_status_t;

typedef enum {
    GF_CON_ROLE_FOLLOWER,
    GF_CON_ROLE_CANDIDATE,
    GF_CON_ROLE_LEADER,
    GF_CON_ROLE_VALIDATOR,
    GF_CON_ROLE_OBSERVER
} gf_con_role_t;

typedef enum {
    GF_CON_STATE_INIT,
    GF_CON_STATE_ELECTION,
    GF_CON_STATE_PRE_PREPARE,
    GF_CON_STATE_PREPARE,
    GF_CON_STATE_COMMIT,
    GF_CON_STATE_FINALIZED
} gf_con_state_t;

typedef enum {
    GF_CON_VOTE_ABSTAIN = 0,
    GF_CON_VOTE_YES,
    GF_CON_VOTE_NO,
    GF_CON_VOTE_TIMEOUT
} gf_con_vote_t;

typedef struct {
    uint8_t node_id[GF_CON_HASH_SIZE];
    char endpoint[64];
    gf_con_role_t role;
    uint64_t stake;                 /* For PoS-like systems */
    uint32_t last_seen_ms;
    bool active;
    bool byzantine;                 /* Detected as faulty */
} gf_con_validator_t;

typedef struct {
    uint32_t proposal_id;
    uint8_t proposer[GF_CON_HASH_SIZE];
    uint8_t data_hash[GF_CON_HASH_SIZE];
    uint32_t height;
    uint32_t round;
    uint32_t timestamp;
    gf_con_state_t state;
    uint8_t prepare_votes;
    uint8_t commit_votes;
    bool finalized;
} gf_con_proposal_t;

typedef struct {
    uint32_t proposal_id;
    uint8_t voter[GF_CON_HASH_SIZE];
    gf_con_vote_t vote;
    gf_con_state_t phase;           /* When vote was cast */
    uint8_t signature[64];
    uint32_t timestamp;
} gf_con_vote_msg_t;

typedef struct {
    uint32_t current_height;
    uint32_t current_round;
    gf_con_role_t my_role;
    uint8_t leader[GF_CON_HASH_SIZE];
    uint8_t active_validators;
    uint8_t quorum_size;
    uint32_t proposals_finalized;
    uint32_t proposals_failed;
    uint32_t byzantine_detected;
} gf_con_status_info_t;

typedef struct {
    uint8_t node_id[GF_CON_HASH_SIZE];
    uint8_t node_key[32];           /* Private key for signing */
    uint8_t quorum_threshold;       /* e.g., 2f+1 for 3f+1 validators */
    uint32_t proposal_timeout_ms;
    uint32_t vote_timeout_ms;
    uint32_t heartbeat_interval_ms;
    bool can_propose;               /* Authority to propose */
    bool byzantine_detection;       /* Enable fault detection */
} gf_con_config_t;

/* Callback for consensus events */
typedef void (*gf_con_callback_t)(gf_con_state_t new_state, 
                                   const gf_con_proposal_t* proposal,
                                   void* user_data);

gf_con_status_t gf_con_init(const gf_con_config_t* config);
void gf_con_shutdown(void);
gf_con_status_t gf_con_add_validator(const gf_con_validator_t* validator);
gf_con_status_t gf_con_remove_validator(const uint8_t* node_id);
gf_con_status_t gf_con_propose(const uint8_t* data, uint16_t length, uint32_t* proposal_id);
gf_con_status_t gf_con_vote(uint32_t proposal_id, gf_con_vote_t vote);
gf_con_status_t gf_con_receive_vote(const gf_con_vote_msg_t* vote);
gf_con_status_t gf_con_process(void);  /* Main consensus loop tick */
gf_con_status_t gf_con_get_status(gf_con_status_info_t* status);
gf_con_status_t gf_con_get_proposal(uint32_t proposal_id, gf_con_proposal_t* proposal);
gf_con_status_t gf_con_register_callback(gf_con_callback_t callback, void* user_data);
bool gf_con_is_finalized(uint32_t proposal_id);
uint8_t gf_con_calculate_quorum(uint8_t total_validators);

#ifdef __cplusplus
}
#endif

#endif /* GF_CONSENSUS_H */
