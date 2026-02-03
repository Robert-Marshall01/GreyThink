/**
 * @file textile_compliance.h
 * @brief Smart Textile Safety Compliance Module
 * 
 * INDUSTRY RELEVANCE:
 * Wearable electronics in textiles must meet safety standards for
 * biocompatibility, electrical safety, and durability. Medical-grade
 * wearables require FDA clearance while consumer devices need FCC/CE.
 * 
 * STANDARDS:
 * - IEC 60601-1 (Medical electrical equipment)
 * - ISO 10993 (Biocompatibility)
 * - IEC 62368-1 (Audio/video/IT equipment safety)
 * - OEKO-TEX Standard 100 (Textile safety)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TEXTILE_COMPLIANCE_H
#define GF_TEXTILE_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    TEXTILE_CERT_OEKO_TEX,
    TEXTILE_CERT_ISO_10993,
    TEXTILE_CERT_IEC_60601,
    TEXTILE_CERT_CE,
    TEXTILE_CERT_FDA_CLEARED
} textile_certification_t;

typedef struct {
    textile_certification_t cert;
    bool compliant;
    char test_id[32];
    uint32_t test_date;
    char notes[64];
} compliance_record_t;

int textile_compliance_init(void);
int textile_compliance_log(textile_certification_t cert, bool pass, const char *notes);
int textile_compliance_get_status(compliance_record_t *records, uint8_t max);
bool textile_compliance_check(textile_certification_t cert);

#ifdef __cplusplus
}
#endif

#endif /* GF_TEXTILE_COMPLIANCE_H */
