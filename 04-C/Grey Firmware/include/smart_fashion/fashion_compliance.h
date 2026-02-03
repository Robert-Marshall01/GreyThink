/**
 * @file fashion_compliance.h
 * @brief Fashion Sustainability Compliance Logging Interface
 * 
 * INDUSTRY RELEVANCE:
 * Fashion industry faces increasing regulatory pressure for sustainability.
 * EU Strategy for Sustainable Textiles (2022) mandates:
 * - Digital Product Passports by 2030
 * - Extended Producer Responsibility
 * - Supply chain transparency
 * - Circular economy requirements
 * 
 * Embedded systems track garment lifecycle for compliance with:
 * - Material composition verification
 * - Carbon footprint calculation
 * - End-of-life recycling guidance
 * - Worker condition attestation
 * 
 * STANDARDS:
 * - EU Textile Strategy (2022)
 * - Global Organic Textile Standard (GOTS)
 * - OEKO-TEX STeP (Sustainable Textile Production)
 * - Higg Index (Sustainability Assessment)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_FASHION_COMPLIANCE_H
#define GF_FASHION_COMPLIANCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Compliance standard */
typedef enum {
    COMPLIANCE_GOTS,             /**< Global Organic Textile Standard */
    COMPLIANCE_OEKOTEX,          /**< OEKO-TEX certification */
    COMPLIANCE_BLUESIGN,         /**< bluesign certification */
    COMPLIANCE_CRADLE2CRADLE,    /**< Cradle to Cradle Certified */
    COMPLIANCE_EU_ECODESIGN      /**< EU Ecodesign Directive */
} compliance_standard_t;

/** Material certification */
typedef struct {
    char material_name[32];
    char origin_country[4];
    compliance_standard_t standards[4];
    uint8_t standard_count;
    float recycled_content_pct;
    bool organic_certified;
} material_cert_t;

/** Carbon footprint entry */
typedef struct {
    float production_kg_co2;
    float transport_kg_co2;
    float use_phase_kg_co2;
    float eol_kg_co2;            /**< End of life */
    float total_kg_co2;
    uint32_t assessment_date;
} carbon_footprint_t;

/** Digital product passport */
typedef struct {
    char product_id[32];
    char manufacturer[64];
    material_cert_t materials[8];
    uint8_t material_count;
    carbon_footprint_t footprint;
    char recycling_instructions[128];
    uint32_t manufacture_date;
    bool passport_verified;
} product_passport_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int fashion_compliance_init(void);
int fashion_register_material(const material_cert_t *material);
int fashion_calculate_footprint(carbon_footprint_t *footprint);
int fashion_generate_passport(product_passport_t *passport);
int fashion_verify_standard(compliance_standard_t standard, bool *compliant);
int fashion_log_audit_event(const char *event_description);
void fashion_compliance_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_FASHION_COMPLIANCE_H */
