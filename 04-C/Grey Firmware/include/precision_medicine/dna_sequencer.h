/**
 * @file dna_sequencer.h
 * @brief DNA Sequencer Interface for Precision Medicine Devices
 * 
 * @details
 * This module provides a hardware abstraction layer for DNA sequencing
 * instruments used in precision medicine applications. It handles
 * sequencing control, data acquisition, and quality metrics.
 * 
 * INDUSTRY RELEVANCE:
 * - Clinical Diagnostics: Illumina, Thermo Fisher sequencers for
 *   cancer genomics, rare disease diagnosis
 * - Pharmaceutical R&D: Drug discovery and companion diagnostics
 * - Point-of-Care: Portable sequencers (Oxford Nanopore MinION)
 *   for rapid pathogen identification
 * - Research: Academic and government genomics programs
 * - Agriculture: Crop and livestock genomic selection
 * 
 * COMPLIANCE:
 * - FDA 21 CFR Part 11: Electronic Records
 * - FDA 21 CFR Part 820: Quality System Regulation
 * - ISO 13485: Medical Device Quality Management
 * - ISO 15189: Medical Laboratory Accreditation
 * - CAP/CLIA: Clinical Laboratory Standards
 * 
 * SEQUENCING TECHNOLOGIES:
 * - NGS (Next-Generation Sequencing): Illumina, Ion Torrent
 * - Long-read: PacBio, Oxford Nanopore
 * - Sanger: Traditional capillary electrophoresis
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_DNA_SEQUENCER_H
#define GF_DNA_SEQUENCER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum read length (bases) */
#define GF_SEQ_MAX_READ_LENGTH          300000

/** Maximum concurrent samples */
#define GF_SEQ_MAX_SAMPLES              96

/** Quality score range (Phred) */
#define GF_SEQ_PHRED_MAX                60

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Sequencing technology type
 */
typedef enum {
    GF_SEQ_TECH_SBS,              /**< Sequencing by Synthesis (Illumina) */
    GF_SEQ_TECH_ION,              /**< Ion semiconductor (Ion Torrent) */
    GF_SEQ_TECH_SMRT,             /**< Single-molecule real-time (PacBio) */
    GF_SEQ_TECH_NANOPORE,         /**< Nanopore sequencing */
    GF_SEQ_TECH_SANGER            /**< Sanger sequencing */
} gf_seq_technology_t;

/**
 * @brief Run state
 */
typedef enum {
    GF_SEQ_STATE_IDLE,            /**< Idle/ready */
    GF_SEQ_STATE_LOADING,         /**< Sample loading */
    GF_SEQ_STATE_PRIMING,         /**< Reagent priming */
    GF_SEQ_STATE_SEQUENCING,      /**< Active sequencing */
    GF_SEQ_STATE_WASHING,         /**< Wash cycle */
    GF_SEQ_STATE_COMPLETE,        /**< Run complete */
    GF_SEQ_STATE_ERROR            /**< Error state */
} gf_seq_state_t;

/**
 * @brief Quality metrics
 */
typedef struct {
    uint32_t total_reads;         /**< Total reads generated */
    uint32_t passing_filter;      /**< Reads passing filter */
    float q30_percent;            /**< Bases >= Q30 (%) */
    float mean_quality;           /**< Mean quality score */
    float cluster_density;        /**< Cluster density (k/mm²) */
    float error_rate;             /**< Estimated error rate */
    uint32_t yield_gb;            /**< Total yield (Gb) */
} gf_seq_quality_t;

/**
 * @brief Run configuration
 */
typedef struct {
    gf_seq_technology_t technology;
    uint16_t read_length;         /**< Read length (cycles) */
    bool paired_end;              /**< Paired-end sequencing */
    uint8_t index_length;         /**< Index read length */
    uint8_t sample_count;         /**< Number of samples */
    char run_id[32];              /**< Run identifier */
} gf_seq_config_t;

/**
 * @brief Run status
 */
typedef struct {
    gf_seq_state_t state;         /**< Current state */
    uint16_t current_cycle;       /**< Current cycle number */
    uint16_t total_cycles;        /**< Total cycles */
    float progress_pct;           /**< Run progress (%) */
    uint32_t time_remaining_s;    /**< Estimated time remaining */
    gf_seq_quality_t quality;     /**< Current quality metrics */
} gf_seq_status_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize DNA sequencer
 * @return 0 on success
 */
int gf_seq_init(void);

/**
 * @brief Shutdown sequencer
 */
void gf_seq_shutdown(void);

/**
 * @brief Start sequencing run
 * @param config Run configuration
 * @return 0 on success
 */
int gf_seq_start_run(const gf_seq_config_t* config);

/**
 * @brief Stop/abort run
 */
void gf_seq_stop_run(void);

/**
 * @brief Get run status
 * @param status Output status
 * @return 0 on success
 */
int gf_seq_get_status(gf_seq_status_t* status);

/**
 * @brief Get quality metrics
 * @param quality Output quality
 * @return 0 on success
 */
int gf_seq_get_quality(gf_seq_quality_t* quality);

/**
 * @brief Process sequencer (call periodically)
 * @param delta_ms Time since last call
 */
void gf_seq_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_DNA_SEQUENCER_H */
