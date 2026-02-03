/**
 * @file medical_compliance.h
 * @brief Emergency Medicine Compliance Logging Interface
 * 
 * INDUSTRY RELEVANCE:
 * Medical devices require extensive compliance logging for:
 * - FDA 21 CFR Part 11 (Electronic Records/Signatures)
 * - HIPAA (Patient Privacy)
 * - Device traceability and recall support
 * - Quality system auditing
 * 
 * EMS systems must maintain tamper-evident logs of:
 * - All patient data access
 * - Device calibration/QC events
 * - Medication administration records
 * - Treatment protocol deviations
 * 
 * STANDARDS:
 * - FDA 21 CFR Part 11 (Electronic Records)
 * - HIPAA 45 CFR Part 164 (Privacy/Security)
 * - IEC 62304 (Software Lifecycle)
 * - ISO 14971 (Risk Management)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_MEDICAL_COMPLIANCE_H
#define GF_MEDICAL_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Audit event type */
typedef enum {
    AUDIT_USER_LOGIN,
    AUDIT_USER_LOGOUT,
    AUDIT_PATIENT_ACCESS,
    AUDIT_DATA_EXPORT,
    AUDIT_CONFIG_CHANGE,
    AUDIT_CALIBRATION,
    AUDIT_QC_PERFORMED,
    AUDIT_MEDICATION_GIVEN,
    AUDIT_PROTOCOL_OVERRIDE,
    AUDIT_DEVICE_ERROR
} audit_event_t;

/** User role */
typedef enum {
    ROLE_PARAMEDIC,
    ROLE_EMT,
    ROLE_NURSE,
    ROLE_PHYSICIAN,
    ROLE_ADMINISTRATOR,
    ROLE_SERVICE_TECH
} user_role_t;

/** Audit log entry */
typedef struct {
    uint32_t entry_id;
    audit_event_t event;
    char user_id[32];
    user_role_t role;
    char patient_id[20];         /**< If applicable */
    char description[128];
    uint32_t timestamp;
    uint8_t signature[32];       /**< HMAC signature */
} audit_entry_t;

/** Calibration record */
typedef struct {
    char device_serial[20];
    char calibrator_id[32];
    uint32_t calibration_date;
    uint32_t expiration_date;
    float calibration_values[8];
    uint8_t value_count;
    bool passed;
} calibration_record_t;

/** Protocol deviation */
typedef struct {
    char protocol_id[20];
    char deviation_reason[128];
    char authorizing_physician[64];
    uint32_t timestamp;
    bool acknowledged;
} protocol_deviation_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int medical_compliance_init(void);
int medical_log_audit(const audit_entry_t *entry);
int medical_user_authenticate(const char *user_id, const char *credential,
                               user_role_t *role);
int medical_record_calibration(const calibration_record_t *record);
int medical_record_deviation(const protocol_deviation_t *deviation);
int medical_verify_log_integrity(bool *valid);
int medical_export_audit_log(uint8_t *buffer, size_t max_len, size_t *written);
void medical_compliance_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_MEDICAL_COMPLIANCE_H */
