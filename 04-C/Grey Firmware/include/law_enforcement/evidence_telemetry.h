/**
 * @file evidence_telemetry.h
 * @brief Digital Evidence Telemetry for Law Enforcement
 * 
 * INDUSTRY RELEVANCE:
 * Digital evidence management requires secure, auditable telemetry for court
 * admissibility. Systems must maintain provenance, integrity, and chain-of-custody
 * from capture through trial. Cloud-based evidence management (DEMS) systems
 * rely on structured telemetry for case building and disclosure.
 * 
 * KEY CAPABILITIES:
 * - Evidence metadata capture (officer, location, time, incident)
 * - Cryptographic integrity verification (SHA-256/SHA-3)
 * - Chain-of-custody event logging
 * - Secure upload to evidence management systems
 * - Retention policy enforcement
 * - Redaction tracking for privacy compliance
 * 
 * STANDARDS COMPLIANCE:
 * - CJIS Security Policy
 * - NIST SP 800-86 (Digital Evidence Handling)
 * - SWGDE Best Practices
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_EVIDENCE_TELEMETRY_H
#define GF_EVIDENCE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Evidence type classification */
typedef enum {
    EVID_TYPE_VIDEO,
    EVID_TYPE_AUDIO,
    EVID_TYPE_PHOTO,
    EVID_TYPE_DOCUMENT,
    EVID_TYPE_SENSOR_DATA
} evid_type_t;

/** Chain-of-custody event */
typedef enum {
    COC_EVENT_CREATED,
    COC_EVENT_ACCESSED,
    COC_EVENT_TRANSFERRED,
    COC_EVENT_COPIED,
    COC_EVENT_REDACTED,
    COC_EVENT_DISCLOSED,
    COC_EVENT_ARCHIVED
} coc_event_t;

/** Evidence record */
typedef struct {
    uint64_t evidence_id;      /**< Unique evidence identifier */
    uint32_t case_number;      /**< Associated case number */
    uint32_t officer_id;       /**< Collecting officer ID */
    uint32_t capture_time;     /**< Unix timestamp of capture */
    evid_type_t type;          /**< Evidence type */
    uint8_t hash[32];          /**< SHA-256 hash */
    uint32_t size_bytes;       /**< File size */
    bool integrity_verified;   /**< Integrity check passed */
} evid_record_t;

/** Chain-of-custody entry */
typedef struct {
    uint64_t evidence_id;      /**< Evidence reference */
    coc_event_t event;         /**< Event type */
    uint32_t user_id;          /**< User performing action */
    uint32_t timestamp;        /**< Event timestamp */
    char notes[128];           /**< Event notes */
} coc_entry_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize evidence telemetry system
 * @param department_id Department identifier
 * @return 0 on success, negative on error
 */
int evid_init(uint32_t department_id);

/**
 * @brief Register new evidence item
 * @param record Evidence record to register
 * @return Unique evidence ID, or negative on error
 */
int64_t evid_register(const evid_record_t* record);

/**
 * @brief Log chain-of-custody event
 * @param entry Chain-of-custody entry
 * @return 0 on success, negative on error
 */
int evid_log_coc(const coc_entry_t* entry);

/**
 * @brief Verify evidence integrity
 * @param evidence_id Evidence to verify
 * @param expected_hash Expected SHA-256 hash
 * @return 0 if verified, -1 if tampered, -2 on error
 */
int evid_verify_integrity(uint64_t evidence_id, const uint8_t* expected_hash);

/**
 * @brief Upload evidence to cloud DEMS
 * @param evidence_id Evidence to upload
 * @return 0 on success, negative on error
 */
int evid_upload(uint64_t evidence_id);

/**
 * @brief Shutdown evidence telemetry
 * @return 0 on success, negative on error
 */
int evid_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EVIDENCE_TELEMETRY_H */
