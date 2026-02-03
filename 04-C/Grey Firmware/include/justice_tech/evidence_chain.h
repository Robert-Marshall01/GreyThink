/**
 * @file evidence_chain.h
 * @brief Secure Digital Evidence Chain of Custody
 * 
 * INDUSTRY RELEVANCE:
 * Digital evidence handling requires cryptographic integrity:
 * - Hash-chained evidence records (SHA-256/SHA-3)
 * - Tamper-evident storage with audit trails
 * - Multi-party custody transfer logging
 * - Time-stamping with trusted authorities
 * - Court-admissible evidence packaging
 * 
 * This stub demonstrates legal tech expertise for:
 * - Law enforcement body camera systems
 * - Court evidence management platforms
 * - eDiscovery and forensic investigation tools
 * - Regulatory compliance systems (SEC, FDA)
 * 
 * STANDARDS COMPLIANCE:
 * - NIST SP 800-86 (Digital Forensics)
 * - ISO 27037 (Digital Evidence Handling)
 * - Federal Rules of Evidence (Authentication)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_JUSTICE_TECH_EVIDENCE_CHAIN_H
#define GF_JUSTICE_TECH_EVIDENCE_CHAIN_H

#include <stdint.h>
#include <stdbool.h>

#define EVIDENCE_HASH_SIZE      32
#define EVIDENCE_MAX_HANDLERS   16

/** Evidence type */
typedef enum {
    EVIDENCE_VIDEO,
    EVIDENCE_AUDIO,
    EVIDENCE_DOCUMENT,
    EVIDENCE_IMAGE,
    EVIDENCE_DATABASE,
    EVIDENCE_DEVICE_IMAGE
} evidence_type_t;

/** Custody action */
typedef enum {
    CUSTODY_COLLECT,
    CUSTODY_TRANSFER,
    CUSTODY_STORE,
    CUSTODY_ANALYZE,
    CUSTODY_PRESENT,
    CUSTODY_DISPOSE
} custody_action_t;

/** Evidence record */
typedef struct {
    uint32_t evidence_id;
    evidence_type_t type;
    uint8_t hash[EVIDENCE_HASH_SIZE];
    uint8_t prev_hash[EVIDENCE_HASH_SIZE];
    uint32_t timestamp;
    uint32_t handler_id;
    custody_action_t action;
    char case_number[32];
    bool sealed;
    bool verified;
} evidence_record_t;

/** Chain integrity status */
typedef struct {
    uint32_t total_records;
    uint32_t verified_count;
    uint32_t broken_links;
    bool chain_valid;
    uint32_t last_verified;
} chain_status_t;

/* API Functions */
int evidence_chain_init(void);
int evidence_chain_add(const evidence_record_t *record);
int evidence_chain_verify(uint32_t evidence_id, bool *valid);
int evidence_chain_get_status(chain_status_t *status);
int evidence_chain_export(uint8_t *buffer, size_t max_len);
int evidence_chain_seal(uint32_t evidence_id);

#endif /* GF_JUSTICE_TECH_EVIDENCE_CHAIN_H */
