/**
 * @file evidence_chain.h
 * @brief Digital Evidence Chain of Custody System
 * 
 * INDUSTRY RELEVANCE:
 * Digital forensics and e-discovery require tamper-proof evidence handling:
 * - Cryptographic integrity verification
 * - Continuous chain of custody logging
 * - Access control and audit trails
 * - Compliance with Federal Rules of Evidence
 * 
 * Critical for law enforcement, legal proceedings, and regulatory investigations.
 * Market: $7.5B digital forensics by 2027.
 * 
 * STANDARDS: NIST SP 800-86, ISO 27037, Federal Rules of Evidence 901/902
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_EVIDENCE_CHAIN_H
#define GF_EVIDENCE_CHAIN_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define EVIDENCE_HASH_LEN 32
#define MAX_CHAIN_ENTRIES 1024

/* Evidence types */
typedef enum {
    EVIDENCE_DOCUMENT,
    EVIDENCE_IMAGE,
    EVIDENCE_VIDEO,
    EVIDENCE_AUDIO,
    EVIDENCE_DATABASE,
    EVIDENCE_DEVICE_IMAGE,
    EVIDENCE_NETWORK_CAPTURE
} evidence_type_t;

/* Chain events */
typedef enum {
    CHAIN_EVENT_CREATED,
    CHAIN_EVENT_ACCESSED,
    CHAIN_EVENT_COPIED,
    CHAIN_EVENT_TRANSFERRED,
    CHAIN_EVENT_ANALYZED,
    CHAIN_EVENT_MODIFIED,
    CHAIN_EVENT_SEALED,
    CHAIN_EVENT_RELEASED
} chain_event_t;

/* Evidence item */
typedef struct {
    char evidence_id[32];
    evidence_type_t type;
    uint8_t hash[EVIDENCE_HASH_LEN];
    uint64_t size_bytes;
    uint32_t created_timestamp;
    char case_number[24];
    char custodian[64];
    bool sealed;
    bool verified;
} evidence_item_t;

/* Chain entry */
typedef struct {
    char evidence_id[32];
    chain_event_t event;
    uint32_t timestamp;
    char actor[64];
    char location[64];
    uint8_t prev_hash[EVIDENCE_HASH_LEN];
    uint8_t entry_hash[EVIDENCE_HASH_LEN];
    char notes[128];
} chain_entry_t;

/**
 * @brief Initialize evidence chain system
 * @return 0 on success
 */
int evidence_chain_init(void);

/**
 * @brief Register new evidence item
 * @param item Evidence item data
 * @return 0 on success
 */
int evidence_chain_register(const evidence_item_t *item);

/**
 * @brief Add chain of custody entry
 * @param evidence_id Evidence identifier
 * @param event Event type
 * @param actor Person/system performing action
 * @param notes Additional notes
 * @return 0 on success
 */
int evidence_chain_add_entry(const char *evidence_id, chain_event_t event,
                             const char *actor, const char *notes);

/**
 * @brief Verify evidence integrity
 * @param evidence_id Evidence to verify
 * @param current_hash Current computed hash
 * @return true if integrity verified
 */
bool evidence_chain_verify(const char *evidence_id, const uint8_t *current_hash);

/**
 * @brief Get chain of custody for evidence
 * @param evidence_id Evidence identifier
 * @param entries Output array for chain entries
 * @param max_entries Maximum entries to return
 * @return Number of entries, or negative on error
 */
int evidence_chain_get(const char *evidence_id, chain_entry_t *entries,
                       uint16_t max_entries);

/**
 * @brief Seal evidence (no further modifications)
 * @param evidence_id Evidence to seal
 * @param authorized_by Authorizing party
 * @return 0 on success
 */
int evidence_chain_seal(const char *evidence_id, const char *authorized_by);

/**
 * @brief Export chain for legal proceedings
 * @param evidence_id Evidence identifier
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written
 */
int evidence_chain_export(const char *evidence_id, uint8_t *buffer, size_t max_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_EVIDENCE_CHAIN_H */
