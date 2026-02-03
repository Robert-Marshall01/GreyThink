/**
 * @file secure_inference.h
 * @brief Secure AI Inference Pipeline
 * 
 * INDUSTRY RELEVANCE:
 * AI models on edge devices are vulnerable to extraction, adversarial attacks,
 * and poisoning. This module provides model encryption, trusted execution, and
 * adversarial input detection. Essential for protecting proprietary models and
 * ensuring inference integrity in security-critical applications.
 * 
 * Key applications:
 * - Biometric authentication systems
 * - Financial fraud detection at edge
 * - Medical AI diagnostic protection
 * - Autonomous vehicle perception security
 * - Industrial quality control AI
 * 
 * @copyright Grey Firmware - Embedded Systems Portfolio
 */

#ifndef GF_SECURE_INFERENCE_H
#define GF_SECURE_INFERENCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Constants
 * ============================================================================ */

#define SINF_MAX_LAYERS             32      /**< Max model layers */
#define SINF_MAX_INPUT_SIZE         16384   /**< Max input size (bytes) */
#define SINF_ENCLAVE_SIZE           262144  /**< Secure enclave size */
#define SINF_KEY_SIZE               32      /**< Encryption key size */

/* ============================================================================
 * Types
 * ============================================================================ */

/**
 * @brief Execution environment
 */
typedef enum {
    SINF_ENV_NORMAL = 0,            /**< Normal world */
    SINF_ENV_TEE,                   /**< TrustZone TEE */
    SINF_ENV_SGX,                   /**< Intel SGX enclave */
    SINF_ENV_SEV,                   /**< AMD SEV */
    SINF_ENV_HSM                    /**< Hardware security module */
} sinf_environment_t;

/**
 * @brief Protection level
 */
typedef enum {
    SINF_PROTECT_NONE = 0,
    SINF_PROTECT_ENCRYPTED,         /**< Encrypted at rest */
    SINF_PROTECT_OBFUSCATED,        /**< Obfuscated weights */
    SINF_PROTECT_TEE,               /**< TEE execution */
    SINF_PROTECT_FULL               /**< All protections */
} sinf_protection_t;

/**
 * @brief Adversarial detection result
 */
typedef enum {
    SINF_INPUT_NORMAL = 0,
    SINF_INPUT_SUSPICIOUS,
    SINF_INPUT_ADVERSARIAL,
    SINF_INPUT_OUT_OF_DISTRIBUTION
} sinf_input_status_t;

/**
 * @brief Model attestation
 */
typedef struct {
    uint8_t model_hash[32];         /**< SHA-256 of model */
    uint8_t signature[64];          /**< Vendor signature */
    uint32_t version;
    uint32_t creation_date;
    bool verified;
} sinf_attestation_t;

/**
 * @brief Inference result
 */
typedef struct {
    float* output;                  /**< Output tensor */
    uint16_t output_size;
    sinf_input_status_t input_status;
    float confidence;
    float adversarial_score;        /**< Adversarial likelihood */
    uint32_t inference_time_us;
    bool enclave_executed;
} sinf_result_t;

/**
 * @brief Security metrics
 */
typedef struct {
    uint32_t inferences_total;
    uint32_t adversarial_blocked;
    uint32_t ood_detected;          /**< Out of distribution */
    uint32_t attestation_failures;
    uint32_t key_rotations;
    bool enclave_healthy;
} sinf_metrics_t;

/**
 * @brief Configuration
 */
typedef struct {
    sinf_environment_t environment;
    sinf_protection_t protection;
    bool enable_adversarial_detection;
    bool enable_ood_detection;
    float adversarial_threshold;
    bool require_attestation;
} sinf_config_t;

/* ============================================================================
 * API (Stub declarations)
 * ============================================================================ */

/**
 * @brief Initialize secure inference
 * @param config Security configuration
 * @return 0 on success, negative on error
 */
int sinf_init(const sinf_config_t* config);

/**
 * @brief Shutdown secure inference
 * @return 0 on success, negative on error
 */
int sinf_shutdown(void);

/**
 * @brief Load encrypted model
 * @param encrypted_model Encrypted model data
 * @param model_size Model size
 * @param key Decryption key
 * @return 0 on success, negative on error
 */
int sinf_load_model(const uint8_t* encrypted_model, uint32_t model_size,
                    const uint8_t* key);

/**
 * @brief Verify model attestation
 * @param attestation Attestation data
 * @return 0 on success, negative on error
 */
int sinf_verify_attestation(const sinf_attestation_t* attestation);

/**
 * @brief Run secure inference
 * @param input Input data
 * @param input_size Input size
 * @param result Output result
 * @return 0 on success, negative on error
 */
int sinf_infer(const float* input, uint16_t input_size, sinf_result_t* result);

/**
 * @brief Check for adversarial input
 * @param input Input data
 * @param input_size Input size
 * @param score Output adversarial score
 * @return Input status
 */
sinf_input_status_t sinf_check_adversarial(const float* input, 
                                            uint16_t input_size,
                                            float* score);

/**
 * @brief Rotate encryption key
 * @param new_key New encryption key
 * @return 0 on success, negative on error
 */
int sinf_rotate_key(const uint8_t* new_key);

/**
 * @brief Get security metrics
 * @param metrics Output metrics
 * @return 0 on success, negative on error
 */
int sinf_get_metrics(sinf_metrics_t* metrics);

/**
 * @brief Securely wipe model from memory
 * @return 0 on success, negative on error
 */
int sinf_wipe_model(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SECURE_INFERENCE_H */
