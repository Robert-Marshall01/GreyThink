/**
 * @file biotech_compliance.h
 * @brief Compliance Logging for Agricultural Biotech Standards
 * 
 * INDUSTRY RELEVANCE:
 * Agricultural biotechnology requires extensive regulatory compliance
 * documentation across multiple jurisdictions. This module enables:
 * - Chain-of-custody tracking for samples
 * - Audit trail generation for regulatory inspections
 * - Coexistence compliance (buffer zones, isolation distances)
 * - Export certification documentation
 * 
 * Target applications: Biotech seed production, trait licensing compliance,
 * import/export certification, organic/non-GMO coexistence, research trials.
 * 
 * Standards: USDA APHIS 7 CFR 340, EU Directive 2001/18/EC, Cartagena
 *            Protocol, ISTA Seed Testing Rules, ISPM 11 (pest risk)
 */

#ifndef GF_BIOTECH_COMPLIANCE_H
#define GF_BIOTECH_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Biotech Compliance Types                                                   */
/*===========================================================================*/

typedef enum {
    REGULATION_USDA_APHIS,      /* US APHIS regulations */
    REGULATION_EU_GMO,          /* EU 2001/18/EC */
    REGULATION_CARTAGENA,       /* Cartagena Protocol */
    REGULATION_ORGANIC,         /* USDA NOP / EU Organic */
    REGULATION_NON_GMO,         /* Non-GMO Project */
    REGULATION_CUSTOM
} regulation_t;

typedef enum {
    AUDIT_SAMPLE_COLLECTION,    /* Sample collected */
    AUDIT_ANALYSIS_START,       /* Analysis initiated */
    AUDIT_ANALYSIS_COMPLETE,    /* Analysis completed */
    AUDIT_TRAIT_DETECTED,       /* Trait detected */
    AUDIT_COMPLIANCE_CHECK,     /* Compliance verification */
    AUDIT_CERTIFICATE_ISSUED,   /* Certificate generated */
    AUDIT_EXPORT_APPROVAL,      /* Export approved */
    AUDIT_VIOLATION_DETECTED    /* Compliance violation */
} audit_event_t;

typedef struct {
    char sample_id[24];
    char field_id[16];
    float latitude;
    float longitude;
    uint32_t collection_time;
    char collector_id[16];
    char crop_variety[32];
} sample_chain_t;

typedef struct {
    audit_event_t event;
    uint32_t timestamp;
    char sample_id[24];
    char operator_id[16];
    char details[64];
    uint8_t severity;           /* 0=info, 1=warning, 2=violation */
    uint32_t hash;              /* Integrity hash */
} audit_record_t;

typedef struct {
    regulation_t regulation;
    bool enabled;
    float buffer_distance_m;    /* Required isolation distance */
    float threshold_pct;        /* Adventitious presence threshold */
    uint32_t retention_days;    /* Audit log retention period */
} compliance_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int biotech_compliance_init(void);
int biotech_compliance_shutdown(void);

int compliance_set_regulation(regulation_t reg, const compliance_config_t* config);
int compliance_log_audit(audit_event_t event, const char* sample_id, 
                        const char* details);

int compliance_register_sample(const sample_chain_t* sample);
int compliance_verify_chain(const char* sample_id, bool* valid);

int compliance_check_buffer(float lat, float lon, float* distance_to_gmo);
int compliance_check_threshold(const char* sample_id, bool* compliant);

int compliance_get_audit_log(audit_record_t* records, uint16_t max_records,
                            uint16_t* count);
int compliance_export_audit(uint8_t* data, uint32_t max_len, uint32_t* len);

bool compliance_is_field_approved(const char* field_id, regulation_t reg);

#endif /* GF_BIOTECH_COMPLIANCE_H */
