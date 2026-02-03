/**
 * @file blockchain_client.h
 * @brief Lightweight Blockchain Client Interface
 * 
 * INDUSTRY RELEVANCE:
 * Edge blockchain enables decentralized trust in IoT systems:
 * - Supply chain provenance tracking (origin, handling, custody)
 * - Industrial IoT data integrity and audit trails
 * - Smart grid energy trading and microgrid settlements
 * - Connected vehicle V2X payment and identity
 * - Medical device data certification and compliance
 * - Agricultural produce traceability (farm to table)
 * 
 * This client demonstrates expertise in:
 * - Lightweight consensus for resource-constrained devices
 * - Merkle tree construction for data integrity
 * - Cryptographic hash chain implementation
 * - Peer-to-peer discovery and synchronization
 * - Smart contract execution on edge devices
 * - Privacy-preserving transaction submission
 */

#ifndef GF_BLOCKCHAIN_CLIENT_H
#define GF_BLOCKCHAIN_CLIENT_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_BC_HASH_SIZE             32      /* SHA-256 hash size */
#define GF_BC_MAX_TX_DATA           256     /* Max transaction data */
#define GF_BC_MAX_PEERS             16
#define GF_BC_BLOCK_CACHE           64      /* Local block cache */

typedef enum {
    GF_BC_OK = 0,
    GF_BC_ERROR_NOT_INITIALIZED,
    GF_BC_ERROR_NO_PEERS,
    GF_BC_ERROR_SYNC_FAILED,
    GF_BC_ERROR_INVALID_BLOCK,
    GF_BC_ERROR_INVALID_TX,
    GF_BC_ERROR_DOUBLE_SPEND,
    GF_BC_ERROR_INSUFFICIENT_FUNDS,
    GF_BC_ERROR_SIGNATURE_INVALID,
    GF_BC_WARN_CHAIN_FORK
} gf_bc_status_t;

typedef enum {
    GF_BC_CONSENSUS_POW,            /* Proof of Work (lightweight) */
    GF_BC_CONSENSUS_POA,            /* Proof of Authority */
    GF_BC_CONSENSUS_PBFT,           /* Practical Byzantine Fault Tolerance */
    GF_BC_CONSENSUS_RAFT            /* Raft consensus */
} gf_bc_consensus_t;

typedef enum {
    GF_BC_TX_TRANSFER,              /* Value transfer */
    GF_BC_TX_DATA,                  /* Data anchoring */
    GF_BC_TX_CONTRACT,              /* Smart contract call */
    GF_BC_TX_REGISTER,              /* Device registration */
    GF_BC_TX_ATTESTATION            /* Data attestation */
} gf_bc_tx_type_t;

typedef struct {
    uint8_t hash[GF_BC_HASH_SIZE];
    uint8_t prev_hash[GF_BC_HASH_SIZE];
    uint32_t block_number;
    uint32_t timestamp;
    uint32_t nonce;
    uint16_t tx_count;
    uint8_t merkle_root[GF_BC_HASH_SIZE];
    uint8_t difficulty;
} gf_bc_block_header_t;

typedef struct {
    uint8_t tx_hash[GF_BC_HASH_SIZE];
    gf_bc_tx_type_t type;
    uint8_t from[GF_BC_HASH_SIZE];      /* Sender address */
    uint8_t to[GF_BC_HASH_SIZE];        /* Recipient address */
    uint64_t amount;                     /* Token amount */
    uint8_t data[GF_BC_MAX_TX_DATA];    /* Transaction data */
    uint16_t data_length;
    uint8_t signature[64];               /* ECDSA signature */
    uint32_t timestamp;
    uint32_t nonce;                      /* Sender nonce */
    bool confirmed;
    uint32_t confirmations;
} gf_bc_transaction_t;

typedef struct {
    uint8_t peer_id[GF_BC_HASH_SIZE];
    char endpoint[64];
    uint32_t last_block;
    uint32_t latency_ms;
    bool connected;
    bool trusted;
} gf_bc_peer_t;

typedef struct {
    uint8_t address[GF_BC_HASH_SIZE];
    uint64_t balance;
    uint32_t nonce;
    uint8_t code_hash[GF_BC_HASH_SIZE]; /* For contracts */
} gf_bc_account_t;

typedef struct {
    gf_bc_consensus_t consensus;
    uint8_t network_id;
    uint8_t difficulty;
    uint32_t block_interval_s;
    bool light_client;              /* SPV mode */
    bool mine_blocks;               /* Participate in consensus */
    uint8_t private_key[32];        /* Device identity */
} gf_bc_config_t;

typedef struct {
    uint32_t chain_height;
    uint32_t local_height;
    uint8_t connected_peers;
    uint64_t total_difficulty;
    bool synced;
    uint32_t pending_tx_count;
} gf_bc_chain_status_t;

gf_bc_status_t gf_bc_init(const gf_bc_config_t* config);
void gf_bc_shutdown(void);
gf_bc_status_t gf_bc_sync(void);
gf_bc_status_t gf_bc_get_status(gf_bc_chain_status_t* status);
gf_bc_status_t gf_bc_submit_tx(const gf_bc_transaction_t* tx);
gf_bc_status_t gf_bc_get_tx(const uint8_t* tx_hash, gf_bc_transaction_t* tx);
gf_bc_status_t gf_bc_get_block(uint32_t block_number, gf_bc_block_header_t* block);
gf_bc_status_t gf_bc_get_account(const uint8_t* address, gf_bc_account_t* account);
gf_bc_status_t gf_bc_add_peer(const gf_bc_peer_t* peer);
gf_bc_status_t gf_bc_remove_peer(const uint8_t* peer_id);
gf_bc_status_t gf_bc_anchor_data(const uint8_t* data, uint16_t length, uint8_t* tx_hash);
gf_bc_status_t gf_bc_verify_data(const uint8_t* data, uint16_t length, const uint8_t* tx_hash);
void gf_bc_compute_hash(const uint8_t* data, uint16_t length, uint8_t* hash);

#ifdef __cplusplus
}
#endif

#endif /* GF_BLOCKCHAIN_CLIENT_H */
