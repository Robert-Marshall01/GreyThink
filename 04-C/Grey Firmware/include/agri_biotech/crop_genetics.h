/**
 * @file crop_genetics.h
 * @brief Crop Genetics Monitoring Module for Edge Agriculture Biotech
 * 
 * INDUSTRY RELEVANCE:
 * Biotech-enabled agriculture is transforming food production through
 * genetic monitoring and trait verification. This module enables:
 * - Field-level genotype verification via portable DNA analysis
 * - GMO/non-GMO trait detection for compliance
 * - Disease resistance gene monitoring
 * - Harvest timing based on genetic maturity markers
 * 
 * Target applications: Seed certification, regulatory compliance, trait
 * licensing verification, plant breeding research, food traceability.
 * 
 * Standards: ISTA Rules (seed testing), OECD Seed Schemes, USDA APHIS
 *            7 CFR Part 340 (biotech regulations)
 */

#ifndef GF_CROP_GENETICS_H
#define GF_CROP_GENETICS_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Crop Genetics Types                                                        */
/*===========================================================================*/

typedef enum {
    CROP_CORN,
    CROP_SOYBEAN,
    CROP_WHEAT,
    CROP_RICE,
    CROP_COTTON,
    CROP_CANOLA,
    CROP_POTATO,
    CROP_TOMATO,
    CROP_CUSTOM
} crop_type_t;

typedef enum {
    TRAIT_HERBICIDE_TOLERANCE,  /* Glyphosate, glufosinate tolerance */
    TRAIT_INSECT_RESISTANCE,    /* Bt proteins */
    TRAIT_DISEASE_RESISTANCE,   /* Fungal, viral resistance */
    TRAIT_DROUGHT_TOLERANCE,    /* Drought stress genes */
    TRAIT_NUTRIENT_ENHANCED,    /* Enhanced nutritional content */
    TRAIT_YIELD_ENHANCED,       /* Yield improvement genes */
    TRAIT_MATURITY,             /* Early/late maturity markers */
    TRAIT_COUNT
} genetic_trait_t;

typedef enum {
    ZYGOSITY_HOMOZYGOUS,        /* Both alleles same */
    ZYGOSITY_HETEROZYGOUS,      /* Different alleles */
    ZYGOSITY_HEMIZYGOUS,        /* Single copy (transgene) */
    ZYGOSITY_NULL               /* Trait absent */
} zygosity_t;

typedef struct {
    genetic_trait_t trait;
    bool detected;
    zygosity_t zygosity;
    float confidence_pct;       /* Detection confidence */
    char gene_name[32];         /* Gene/event name */
    char marker_id[16];         /* PCR marker identifier */
} trait_result_t;

typedef struct {
    char sample_id[24];
    crop_type_t crop;
    char variety[32];
    uint8_t trait_count;
    trait_result_t traits[TRAIT_COUNT];
    bool gmo_detected;
    bool regulatory_compliant;
    uint32_t analysis_time;
    uint32_t timestamp;
} genetics_analysis_t;

typedef struct {
    crop_type_t crop;
    char variety[32];
    bool require_all_traits;    /* Require all expected traits */
    uint8_t expected_traits;    /* Bitmap of expected traits */
} genetics_config_t;

/*===========================================================================*/
/* API Functions (Stubs)                                                      */
/*===========================================================================*/

int crop_genetics_init(const genetics_config_t* config);
int crop_genetics_shutdown(void);

int genetics_start_analysis(const char* sample_id);
int genetics_get_analysis(genetics_analysis_t* analysis);
bool genetics_is_analysis_complete(void);

int genetics_check_trait(genetic_trait_t trait, trait_result_t* result);
int genetics_verify_variety(const char* expected_variety, float* match_pct);

int genetics_export_certificate(uint8_t* cert_data, uint16_t max_len);
int genetics_log_for_compliance(void);

#endif /* GF_CROP_GENETICS_H */
