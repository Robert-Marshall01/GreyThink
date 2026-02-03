/**
 * @file sample_processing.h
 * @brief Sample Processing Module for Precision Medicine
 * 
 * @details
 * This module manages biological sample processing workflows in
 * precision medicine instruments. It handles sample tracking,
 * preparation steps, and quality control.
 * 
 * INDUSTRY RELEVANCE:
 * - Clinical Labs: High-throughput sample processing automation
 * - Biobanks: Sample storage and retrieval systems
 * - Diagnostics: Automated sample prep for IVD instruments
 * - Research: Liquid handling robots and automation platforms
 * - Biopharma: Drug manufacturing QC sample processing
 * 
 * COMPLIANCE:
 * - ISO 15189: Medical Laboratory Accreditation
 * - CAP (College of American Pathologists) accreditation
 * - FDA 21 CFR Part 11: Electronic Records
 * - HIPAA: Patient sample privacy
 * - GLP/GMP: Good Laboratory/Manufacturing Practice
 * 
 * TRACEABILITY:
 * - Chain of custody tracking
 * - Sample integrity monitoring
 * - Processing step audit trail
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_SAMPLE_PROCESSING_H
#define GF_SAMPLE_PROCESSING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum samples in processing queue */
#define GF_SAMPLE_MAX_QUEUE             192

/** Maximum processing steps */
#define GF_SAMPLE_MAX_STEPS             16

/** Sample ID length */
#define GF_SAMPLE_ID_LENGTH             32

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Sample type
 */
typedef enum {
    GF_SAMPLE_BLOOD_WHOLE,        /**< Whole blood */
    GF_SAMPLE_BLOOD_PLASMA,       /**< Plasma */
    GF_SAMPLE_BLOOD_SERUM,        /**< Serum */
    GF_SAMPLE_URINE,              /**< Urine */
    GF_SAMPLE_SALIVA,             /**< Saliva */
    GF_SAMPLE_TISSUE,             /**< Tissue biopsy */
    GF_SAMPLE_SWAB,               /**< Swab (nasal, oral) */
    GF_SAMPLE_CSF,                /**< Cerebrospinal fluid */
    GF_SAMPLE_DNA_EXTRACT,        /**< Extracted DNA */
    GF_SAMPLE_RNA_EXTRACT         /**< Extracted RNA */
} gf_sample_type_t;

/**
 * @brief Processing step type
 */
typedef enum {
    GF_STEP_RECEIVE,              /**< Sample receipt */
    GF_STEP_ALIQUOT,              /**< Aliquoting */
    GF_STEP_CENTRIFUGE,           /**< Centrifugation */
    GF_STEP_EXTRACT,              /**< Nucleic acid extraction */
    GF_STEP_QUANTIFY,             /**< Quantification */
    GF_STEP_NORMALIZE,            /**< Normalization */
    GF_STEP_LIBRARY_PREP,         /**< Library preparation */
    GF_STEP_QC,                   /**< Quality control */
    GF_STEP_STORAGE,              /**< Storage */
    GF_STEP_DISPOSE               /**< Disposal */
} gf_processing_step_t;

/**
 * @brief Sample status
 */
typedef enum {
    GF_SAMPLE_STATUS_PENDING,     /**< Awaiting processing */
    GF_SAMPLE_STATUS_PROCESSING,  /**< In processing */
    GF_SAMPLE_STATUS_COMPLETE,    /**< Processing complete */
    GF_SAMPLE_STATUS_FAILED,      /**< Processing failed */
    GF_SAMPLE_STATUS_ON_HOLD      /**< On hold */
} gf_sample_status_t;

/**
 * @brief Sample record
 */
typedef struct {
    char sample_id[GF_SAMPLE_ID_LENGTH];
    char patient_id[GF_SAMPLE_ID_LENGTH];
    gf_sample_type_t type;
    gf_sample_status_t status;
    uint32_t receive_time;        /**< Receipt timestamp */
    uint32_t process_time;        /**< Processing timestamp */
    float volume_ul;              /**< Volume (µL) */
    float concentration_ng_ul;    /**< Concentration (ng/µL) */
    uint8_t current_step;         /**< Current processing step */
    bool qc_passed;               /**< QC status */
} gf_sample_record_t;

/**
 * @brief Processing step record
 */
typedef struct {
    gf_processing_step_t step;    /**< Step type */
    uint32_t start_time;          /**< Start timestamp */
    uint32_t end_time;            /**< End timestamp */
    char operator_id[16];         /**< Operator ID */
    char instrument_id[16];       /**< Instrument ID */
    bool success;                 /**< Step success */
    char notes[64];               /**< Step notes */
} gf_step_record_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize sample processing
 * @return 0 on success
 */
int gf_sample_init(void);

/**
 * @brief Shutdown sample processing
 */
void gf_sample_shutdown(void);

/**
 * @brief Register new sample
 * @param record Sample record
 * @return 0 on success
 */
int gf_sample_register(const gf_sample_record_t* record);

/**
 * @brief Update sample status
 * @param sample_id Sample identifier
 * @param status New status
 * @return 0 on success
 */
int gf_sample_update_status(const char* sample_id, 
                             gf_sample_status_t status);

/**
 * @brief Record processing step
 * @param sample_id Sample identifier
 * @param step Step record
 * @return 0 on success
 */
int gf_sample_record_step(const char* sample_id,
                           const gf_step_record_t* step);

/**
 * @brief Get sample record
 * @param sample_id Sample identifier
 * @param record Output record
 * @return 0 on success
 */
int gf_sample_get(const char* sample_id, gf_sample_record_t* record);

/**
 * @brief Process sample queue (call periodically)
 * @param delta_ms Time since last call
 */
void gf_sample_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_SAMPLE_PROCESSING_H */
