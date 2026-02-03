/**
 * @file quantum_rng.h
 * @brief Quantum Random Number Generator Interface
 * 
 * INDUSTRY RELEVANCE:
 * True random number generation is critical for cryptography, simulation,
 * and security applications. Quantum RNG provides hardware-backed entropy
 * from quantum mechanical processes. This module demonstrates:
 * - Quantum entropy sources (photon detection, vacuum fluctuations)
 * - NIST SP 800-90B compliant health testing
 * - Continuous entropy estimation and conditioning
 * - FIPS 140-3 certified output generation
 * 
 * Applications: HSMs, secure enclaves, cryptographic key generation,
 *               Monte Carlo simulations, lottery systems
 * Standards: NIST SP 800-90B, AIS 31, BSI-CC-PP-0045
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_QUANTUM_RNG_H
#define GF_QUANTUM_RNG_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Quantum RNG Types
 ******************************************************************************/

/** Entropy source types */
typedef enum {
    GF_QRNG_PHOTON_DETECTION,    /**< Single photon arrival times */
    GF_QRNG_VACUUM_FLUCTUATION,  /**< Quantum vacuum noise */
    GF_QRNG_BEAM_SPLITTER,       /**< Photon path indeterminacy */
    GF_QRNG_TUNNELING,           /**< Electron tunneling events */
    GF_QRNG_RADIOACTIVE          /**< Radioactive decay timing */
} gf_qrng_source_t;

/** Health test status */
typedef enum {
    GF_QRNG_HEALTH_PASSED,
    GF_QRNG_HEALTH_STARTUP_FAIL, /**< Initial tests failed */
    GF_QRNG_HEALTH_CONTINUOUS_FAIL, /**< Runtime test failure */
    GF_QRNG_HEALTH_DEGRADED,     /**< Entropy below threshold */
    GF_QRNG_HEALTH_OFFLINE       /**< Source unavailable */
} gf_qrng_health_t;

/** Conditioning algorithm */
typedef enum {
    GF_QRNG_COND_NONE,
    GF_QRNG_COND_SHA256,         /**< SHA-256 whitening */
    GF_QRNG_COND_AES_CBC_MAC,    /**< AES-CBC-MAC conditioning */
    GF_QRNG_COND_HMAC_DRBG       /**< HMAC-DRBG post-processing */
} gf_qrng_conditioning_t;

/*******************************************************************************
 * Quantum RNG Configuration
 ******************************************************************************/

/** QRNG configuration */
typedef struct {
    gf_qrng_source_t source;
    gf_qrng_conditioning_t conditioning;
    uint32_t min_entropy_bits_per_byte; /**< Minimum 4 bits/byte */
    uint32_t output_rate_bps;           /**< Target output rate */
    bool enable_health_tests;
    uint16_t startup_test_count;        /**< Initial samples to test */
    uint16_t continuous_test_window;    /**< Rolling window size */
    bool fips_mode;                     /**< FIPS 140-3 compliant mode */
} gf_qrng_config_t;

/** Entropy estimation result */
typedef struct {
    uint32_t raw_entropy_per_sample;    /**< Measured (millibits) */
    uint32_t conditioned_entropy;       /**< After conditioning */
    uint32_t samples_collected;
    uint32_t samples_rejected;
    bool meets_threshold;
} gf_entropy_estimate_t;

/*******************************************************************************
 * Quantum RNG Statistics
 ******************************************************************************/

typedef struct {
    uint64_t bytes_generated;
    uint64_t raw_samples_collected;
    uint32_t startup_tests_run;
    uint32_t continuous_tests_run;
    uint32_t health_failures;
    uint32_t reseed_count;
    gf_qrng_health_t current_health;
    float min_entropy_observed;
    float avg_entropy_observed;
} gf_qrng_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize quantum RNG
 * @param config RNG configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_qrng_init(const gf_qrng_config_t *config);

/**
 * @brief Generate random bytes
 * @param buffer Output buffer
 * @param length Number of bytes to generate
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_qrng_generate(uint8_t *buffer, size_t length);

/**
 * @brief Generate random 32-bit integer
 * @return Random value
 */
uint32_t gf_qrng_get_u32(void);

/**
 * @brief Generate random 64-bit integer
 * @return Random value
 */
uint64_t gf_qrng_get_u64(void);

/**
 * @brief Run health test suite
 * @return Health status
 */
gf_qrng_health_t gf_qrng_health_test(void);

/**
 * @brief Get entropy estimation
 * @param estimate Output estimation
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_qrng_estimate_entropy(gf_entropy_estimate_t *estimate);

/**
 * @brief Force reseed from quantum source
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_qrng_reseed(void);

/**
 * @brief Get RNG statistics
 * @return Current statistics
 */
gf_qrng_stats_t gf_qrng_get_stats(void);

/**
 * @brief Shutdown quantum RNG
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_qrng_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_QUANTUM_RNG_H */
