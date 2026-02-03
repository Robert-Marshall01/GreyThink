/**
 * @file police_compliance.h
 * @brief Police Drone Safety and Compliance Module
 * 
 * INDUSTRY RELEVANCE:
 * Law enforcement drone operations require strict compliance with FAA
 * regulations, department policies, and civil liberties protections.
 * This module enforces operational constraints and generates compliance
 * documentation for legal and administrative oversight.
 * 
 * TECHNICAL SCOPE:
 * - Geofence enforcement (no-fly zones, restricted areas)
 * - Flight time and altitude limits
 * - Chain of custody for recorded evidence
 * - Warrant-based operation tracking
 * - Use-of-force policy integration
 * - Automated compliance reporting
 * 
 * COMPLIANCE CATEGORIES:
 * - Federal (FAA Part 107, CISA guidelines)
 * - State (privacy laws, wiretap statutes)
 * - Local (department policies, community guidelines)
 * - Evidentiary (authentication, preservation)
 * 
 * STANDARDS COMPLIANCE:
 * - FAA Part 107 (Small UAS)
 * - NIJ Standard 0604.01
 * - ACLU surveillance guidelines
 * - State privacy laws (varies)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_POLICE_COMPLIANCE_H
#define GF_POLICE_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define POLICE_MAX_GEOFENCES    64    /**< Maximum geofence zones */
#define POLICE_MAX_WARRANTS     16    /**< Active warrant capacity */
#define POLICE_AUDIT_ENTRIES    1000  /**< Audit log size */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Operation authorization type */
typedef enum {
    POLICE_AUTH_ROUTINE,           /**< Standard patrol */
    POLICE_AUTH_EXIGENT,           /**< Emergency circumstances */
    POLICE_AUTH_WARRANT,           /**< Court-authorized */
    POLICE_AUTH_CONSENT,           /**< Subject consent */
    POLICE_AUTH_TRAINING           /**< Training exercise */
} police_auth_t;

/** Geofence type */
typedef enum {
    GEOFENCE_NO_FLY,               /**< Absolute prohibition */
    GEOFENCE_RESTRICTED,           /**< Authorization required */
    GEOFENCE_SENSITIVE,            /**< Enhanced logging */
    GEOFENCE_AUTHORIZED            /**< Pre-approved area */
} geofence_type_t;

/** Compliance status */
typedef enum {
    POLICE_COMPLIANT,
    POLICE_WARNING,
    POLICE_VIOLATION,
    POLICE_CRITICAL_VIOLATION
} police_compliance_t;

/** Audit event type */
typedef enum {
    AUDIT_FLIGHT_START,
    AUDIT_FLIGHT_END,
    AUDIT_RECORDING_START,
    AUDIT_RECORDING_STOP,
    AUDIT_GEOFENCE_ENTRY,
    AUDIT_GEOFENCE_EXIT,
    AUDIT_DETECTION_EVENT,
    AUDIT_POLICY_OVERRIDE,
    AUDIT_DATA_ACCESS,
    AUDIT_DATA_EXPORT
} audit_event_t;

/** Geofence zone */
typedef struct {
    uint8_t zone_id;
    geofence_type_t type;
    float center_lat;
    float center_lon;
    float radius_m;
    float max_altitude_m;
    bool active;
    char description[64];
} geofence_zone_t;

/** Compliance status */
typedef struct {
    police_compliance_t status;
    bool geofence_violation;
    bool altitude_violation;
    bool time_violation;
    bool recording_violation;
    uint32_t violations_today;
    uint32_t warnings_today;
    police_auth_t current_auth;
    char active_warrant_id[32];
    uint64_t flight_start_time;
    uint32_t flight_duration_s;
} police_compliance_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize compliance module */
int police_compliance_init(void);

/** Set operation authorization */
int police_set_authorization(police_auth_t auth, const char *warrant_id);

/** Add geofence zone */
int police_add_geofence(const geofence_zone_t *zone);

/** Check current compliance */
int police_check_compliance(float lat, float lon, float alt_m, police_compliance_status_t *status);

/** Log audit event */
int police_log_audit(audit_event_t event, const char *details);

/** Generate compliance report */
int police_generate_report(uint8_t *buffer, size_t max_len);

/** Verify evidence chain of custody */
bool police_verify_chain_of_custody(const char *evidence_id);

/** Process compliance checks */
int police_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_POLICE_COMPLIANCE_H */
