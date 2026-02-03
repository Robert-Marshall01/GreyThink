/**
 * @file entropy_pool.h
 * @brief Secure Entropy Pool Manager
 * 
 * INDUSTRY RELEVANCE:
 * Cryptographic systems require high-quality entropy for key generation,
 * nonces, and initialization vectors. This module demonstrates:
 * - Multi-source entropy collection and mixing
 * - Forward/backward secrecy through continuous reseeding
 * - Entropy depletion tracking and starvation prevention
 * - FIPS/CC-certified entropy extraction
 * 
 * Applications: Secure boot, TLS/SSL stacks, PKI systems, HSMs,
 *               secure enclaves, blockchain nodes
 * Standards: NIST SP 800-90A/B/C, BSI AIS 20/31
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_ENTROPY_POOL_H
#define GF_ENTROPY_POOL_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Entropy Types
 ******************************************************************************/

/** Entropy source types */
typedef enum {
    GF_ENTROPY_SRC_HWRNG,        /**< Hardware RNG (TRNG) */
    GF_ENTROPY_SRC_QRNG,         /**< Quantum RNG */
    GF_ENTROPY_SRC_TIMING,       /**< CPU timing jitter */
    GF_ENTROPY_SRC_ADC_NOISE,    /**< ADC thermal noise */
    GF_ENTROPY_SRC_INTERRUPT,    /**< Interrupt timing */
    GF_ENTROPY_SRC_USER,         /**< User-provided entropy */
    GF_ENTROPY_SRC_EXTERNAL      /**< External entropy beacon */
} gf_entropy_source_t;

/** Pool state */
typedef enum {
    GF_POOL_EMPTY,               /**< No entropy available */
    GF_POOL_LOW,                 /**< Below minimum threshold */
    GF_POOL_NOMINAL,             /**< Normal operation */
    GF_POOL_FULL                 /**< At capacity */
} gf_pool_state_t;

/*******************************************************************************
 * Entropy Pool Configuration
 ******************************************************************************/

/** Entropy source descriptor */
typedef struct {
    gf_entropy_source_t type;
    char name[24];
    uint32_t entropy_bits_per_sample; /**< Estimated entropy */
    uint32_t sample_rate_hz;       /**< Collection rate */
    bool is_primary;               /**< Primary source flag */
    bool requires_conditioning;    /**< Needs whitening */
    uint8_t quality_score;         /**< 0-100 quality rating */
} gf_entropy_source_desc_t;

/** Pool configuration */
typedef struct {
    uint32_t pool_size_bits;       /**< Total pool capacity */
    uint32_t min_threshold_bits;   /**< Low entropy warning */
    uint32_t reseed_threshold_bits;/**< When to trigger reseed */
    uint32_t output_block_bits;    /**< Extraction block size */
    bool enable_prediction_resistance;
    bool enable_continuous_test;
    uint32_t max_output_per_request;
    void (*low_entropy_callback)(void);
} gf_entropy_pool_config_t;

/** Entropy contribution */
typedef struct {
    gf_entropy_source_t source;
    uint8_t *data;
    size_t data_len;
    uint32_t estimated_entropy_bits;
    uint32_t timestamp;
} gf_entropy_contribution_t;

/*******************************************************************************
 * Entropy Pool Statistics
 ******************************************************************************/

typedef struct {
    uint32_t current_entropy_bits;
    uint32_t total_entropy_collected;
    uint32_t total_entropy_extracted;
    uint32_t reseed_count;
    uint32_t starvation_events;
    uint32_t contributions_by_source[8];
    gf_pool_state_t current_state;
    uint32_t last_reseed_time;
    uint32_t extraction_requests;
} gf_entropy_pool_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize entropy pool
 * @param config Pool configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_entropy_pool_init(const gf_entropy_pool_config_t *config);

/**
 * @brief Register entropy source
 * @param source Source descriptor
 * @return Source ID or negative error
 */
int32_t gf_entropy_register_source(const gf_entropy_source_desc_t *source);

/**
 * @brief Add entropy to pool
 * @param contribution Entropy data
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_entropy_contribute(const gf_entropy_contribution_t *contribution);

/**
 * @brief Extract entropy from pool
 * @param buffer Output buffer
 * @param length Bytes to extract
 * @param prediction_resistant Force reseed before extraction
 * @return Actual entropy bits provided
 */
uint32_t gf_entropy_extract(uint8_t *buffer, size_t length, 
                            bool prediction_resistant);

/**
 * @brief Get current pool entropy level
 * @return Current entropy in bits
 */
uint32_t gf_entropy_get_level(void);

/**
 * @brief Get pool state
 * @return Current state
 */
gf_pool_state_t gf_entropy_get_state(void);

/**
 * @brief Force pool reseed
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_entropy_reseed(void);

/**
 * @brief Clear pool (security wipe)
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_entropy_pool_clear(void);

/**
 * @brief Get pool statistics
 * @return Current statistics
 */
gf_entropy_pool_stats_t gf_entropy_pool_get_stats(void);

/**
 * @brief Shutdown entropy pool
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_entropy_pool_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ENTROPY_POOL_H */
