/**
 * @file ems_compliance.h
 * @brief EMS Regulatory Compliance Module
 *
 * INDUSTRY RELEVANCE:
 * Emergency medical services face complex regulatory requirements
 * including HIPAA for patient data, state EMS protocols, and NEMSIS
 * data standards. Automated compliance reduces liability and ensures
 * consistent quality of care.
 *
 * MARKET CONTEXT:
 * - NEMSIS 3.5 national data standard compliance
 * - State protocol adherence verification
 * - Medication administration documentation
 * - Controlled substance tracking
 * - Quality assurance and peer review requirements
 *
 * TECHNICAL APPROACH:
 * - Real-time protocol checklist validation
 * - Automated timestamp and GPS documentation
 * - Crew certification verification
 * - Equipment maintenance compliance
 * - Secure data transmission with audit trail
 *
 * @author Grey Firmware Project
 */

#ifndef GF_EMS_COMPLIANCE_H
#define GF_EMS_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Compliance check status
 */
typedef enum {
    COMPLIANCE_OK,
    COMPLIANCE_WARNING,
    COMPLIANCE_VIOLATION,
    COMPLIANCE_NOT_APPLICABLE
} gf_compliance_status_t;

/**
 * @brief Protocol type for checklist
 */
typedef enum {
    PROTOCOL_CARDIAC,
    PROTOCOL_STROKE,
    PROTOCOL_TRAUMA,
    PROTOCOL_PEDIATRIC,
    PROTOCOL_AIRWAY,
    PROTOCOL_MEDICATION,
    PROTOCOL_COUNT
} gf_protocol_type_t;

/**
 * @brief Crew certification record
 */
typedef struct {
    uint32_t crew_id;
    char certification_level[16]; /**< EMT-B, AEMT, Paramedic */
    uint32_t cert_expiry;         /**< Unix timestamp */
    bool cpr_current;
    bool acls_current;
    bool pals_current;
} gf_crew_cert_t;

/**
 * @brief Protocol compliance record
 */
typedef struct {
    gf_protocol_type_t protocol;
    gf_compliance_status_t status;
    uint8_t steps_completed;
    uint8_t steps_required;
    char deviation_notes[128];
    uint32_t timestamp;
} gf_protocol_compliance_t;

/**
 * @brief Controlled substance log entry
 */
typedef struct {
    char drug_name[32];
    float dose_mg;
    uint32_t admin_time;
    uint32_t witness_id;
    char waste_witness[32];
    bool properly_documented;
} gf_controlled_log_t;

/* Function prototypes */
int gf_ems_compliance_init(void);
int gf_ems_verify_crew_cert(uint32_t crew_id, gf_crew_cert_t *cert);
int gf_ems_start_protocol(gf_protocol_type_t protocol);
int gf_ems_complete_step(gf_protocol_type_t protocol, uint8_t step);
int gf_ems_get_compliance(gf_protocol_type_t protocol, gf_protocol_compliance_t *comp);
int gf_ems_log_medication(const gf_controlled_log_t *log);
int gf_ems_generate_nemsis(uint8_t *buffer, size_t max_len);
int gf_ems_export_audit_trail(const char *filename);
void gf_ems_compliance_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EMS_COMPLIANCE_H */
