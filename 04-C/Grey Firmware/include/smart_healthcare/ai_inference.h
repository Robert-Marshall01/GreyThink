/**
 * @file ai_inference.h
 * @brief AI Inference Pipeline Stub - Edge Healthcare AI
 * 
 * @details Industry Relevance:
 * Edge AI enables real-time analysis of medical signals without cloud
 * latency or connectivity requirements:
 * - Arrhythmia detection in ECG waveforms
 * - Sleep apnea detection from SpO2/respiration
 * - Fall detection and prediction
 * - Voice biomarker analysis
 * - Medication compliance from activity patterns
 * 
 * FDA has established pathways for AI/ML-based SaMD (Software as Medical
 * Device) including "locked" algorithms and continuous learning systems.
 * Pre-determined change control plans (PCCP) enable algorithm updates.
 * 
 * Challenges: Model validation, data drift detection, explainability,
 * regulatory documentation, and post-market surveillance.
 * 
 * Standards: FDA guidance on AI/ML SaMD, IEC 62304, ISO 14971 (risk),
 * IMDRF SaMD classification framework
 * 
 * @version 1.0.0
 * @date 2024
 * @author Grey Firmware Team
 * 
 * @copyright Copyright (c) 2024 Grey Firmware Project
 * SPDX-License-Identifier: MIT
 */

#ifndef GF_HEALTHCARE_AI_INFERENCE_H
#define GF_HEALTHCARE_AI_INFERENCE_H

#ifdef __cplusplus
extern "C" {
#endif

#include <stdint.h>
#include <stdbool.h>

/*******************************************************************************
 * Constants & Configuration
 ******************************************************************************/

/** Maximum loaded models */
#define GF_AI_MED_MAX_MODELS            4

/** Maximum input tensor size */
#define GF_AI_MED_MAX_INPUT_SIZE        4096

/** Maximum output size */
#define GF_AI_MED_MAX_OUTPUT_SIZE       256

/** Confidence threshold for clinical alert */
#define GF_AI_MED_CONFIDENCE_THRESHOLD  0.85f

/** Maximum inference latency (ms) for real-time */
#define GF_AI_MED_MAX_LATENCY_MS        50

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Medical AI model types
 */
typedef enum {
    GF_AI_MED_MODEL_ARRHYTHMIA,     /**< ECG arrhythmia classifier */
    GF_AI_MED_MODEL_AFIB,           /**< Atrial fibrillation detector */
    GF_AI_MED_MODEL_SLEEP_APNEA,    /**< Sleep apnea detector */
    GF_AI_MED_MODEL_FALL,           /**< Fall detection */
    GF_AI_MED_MODEL_ACTIVITY,       /**< Activity classification */
    GF_AI_MED_MODEL_RESPIRATION,    /**< Respiration analysis */
    GF_AI_MED_MODEL_CUSTOM          /**< Custom model */
} gf_ai_med_model_type_t;

/**
 * @brief Inference result classification
 */
typedef enum {
    GF_AI_MED_RESULT_NORMAL,        /**< Normal finding */
    GF_AI_MED_RESULT_ABNORMAL,      /**< Abnormal, non-urgent */
    GF_AI_MED_RESULT_ATTENTION,     /**< Needs attention */
    GF_AI_MED_RESULT_URGENT,        /**< Urgent attention needed */
    GF_AI_MED_RESULT_INCONCLUSIVE   /**< Low confidence */
} gf_ai_med_result_t;

/**
 * @brief Model metadata (regulatory compliance)
 */
typedef struct {
    char model_id[32];              /**< Model unique ID */
    char version[16];               /**< Model version */
    char sha256[65];                /**< Model hash for integrity */
    gf_ai_med_model_type_t type;    /**< Model type */
    uint32_t validation_date;       /**< Last validation date */
    bool fda_cleared;               /**< FDA 510(k) cleared */
    bool ce_marked;                 /**< CE marked */
    char intended_use[128];         /**< Intended use statement */
} gf_ai_med_model_meta_t;

/**
 * @brief Arrhythmia classification output
 */
typedef struct {
    bool normal_sinus;              /**< Normal sinus rhythm */
    bool atrial_fib;                /**< Atrial fibrillation */
    bool atrial_flutter;            /**< Atrial flutter */
    bool pvc;                       /**< Premature ventricular contraction */
    bool pac;                       /**< Premature atrial contraction */
    bool vtach;                     /**< Ventricular tachycardia */
    bool bradycardia;               /**< Bradycardia */
    bool tachycardia;               /**< Tachycardia */
    float confidence;               /**< Overall confidence */
    gf_ai_med_result_t severity;    /**< Classification severity */
} gf_ai_med_arrhythmia_t;

/**
 * @brief Fall detection output
 */
typedef struct {
    bool fall_detected;             /**< Hard fall detected */
    bool near_fall;                 /**< Near-fall event */
    float impact_g;                 /**< Impact magnitude in g */
    float inactivity_s;             /**< Post-impact inactivity */
    float confidence;               /**< Detection confidence */
    uint32_t timestamp;             /**< Event timestamp */
} gf_ai_med_fall_t;

/**
 * @brief Inference request
 */
typedef struct {
    gf_ai_med_model_type_t model;   /**< Model to use */
    uint8_t input[GF_AI_MED_MAX_INPUT_SIZE]; /**< Input data */
    uint16_t input_len;             /**< Input length */
    uint32_t timestamp;             /**< Data timestamp */
    bool priority;                  /**< Priority inference */
} gf_ai_med_request_t;

/**
 * @brief Inference response
 */
typedef struct {
    gf_ai_med_model_type_t model;   /**< Model used */
    gf_ai_med_result_t result;      /**< Result classification */
    float confidence;               /**< Result confidence */
    uint8_t output[GF_AI_MED_MAX_OUTPUT_SIZE]; /**< Raw output */
    uint16_t output_len;            /**< Output length */
    uint32_t latency_us;            /**< Inference time */
    bool valid;                     /**< Result validity */
} gf_ai_med_response_t;

/**
 * @brief Inference pipeline status
 */
typedef struct {
    uint8_t models_loaded;          /**< Number of loaded models */
    uint32_t inferences_total;      /**< Total inferences run */
    uint32_t inferences_valid;      /**< Valid inferences */
    uint32_t avg_latency_us;        /**< Average latency */
    uint32_t max_latency_us;        /**< Maximum latency */
    float memory_used_pct;          /**< Memory utilization */
    bool processing;                /**< Currently processing */
} gf_ai_med_status_t;

/*******************************************************************************
 * API Functions (Stubs)
 ******************************************************************************/

/**
 * @brief Initialize AI inference pipeline
 * @return 0 on success, negative error code on failure
 */
int gf_ai_med_init(void);

/**
 * @brief Load AI model
 * @param type Model type to load
 * @param model_data Model binary data
 * @param model_len Model data length
 * @return 0 on success
 */
int gf_ai_med_load_model(gf_ai_med_model_type_t type,
                         const uint8_t* model_data,
                         uint32_t model_len);

/**
 * @brief Run inference
 * @param request Inference request
 * @param response Output response
 * @return 0 on success
 */
int gf_ai_med_infer(const gf_ai_med_request_t* request,
                    gf_ai_med_response_t* response);

/**
 * @brief Run arrhythmia analysis on ECG
 * @param ecg_samples ECG samples
 * @param sample_count Number of samples
 * @param result Output arrhythmia classification
 * @return 0 on success
 */
int gf_ai_med_analyze_arrhythmia(const int16_t* ecg_samples,
                                 uint16_t sample_count,
                                 gf_ai_med_arrhythmia_t* result);

/**
 * @brief Run fall detection on accelerometer data
 * @param accel_samples Accelerometer samples (x,y,z interleaved)
 * @param sample_count Number of samples
 * @param result Output fall detection result
 * @return 0 on success
 */
int gf_ai_med_detect_fall(const int16_t* accel_samples,
                          uint16_t sample_count,
                          gf_ai_med_fall_t* result);

/**
 * @brief Get model metadata
 * @param type Model type
 * @param meta Output metadata
 * @return 0 on success
 */
int gf_ai_med_get_model_meta(gf_ai_med_model_type_t type,
                             gf_ai_med_model_meta_t* meta);

/**
 * @brief Get pipeline status
 * @param status Output status
 * @return 0 on success
 */
int gf_ai_med_get_status(gf_ai_med_status_t* status);

/**
 * @brief Verify model integrity
 * @param type Model to verify
 * @return 0 if valid, -1 if corrupted
 */
int gf_ai_med_verify_model(gf_ai_med_model_type_t type);

/**
 * @brief Shutdown AI inference pipeline
 * @return 0 on success
 */
int gf_ai_med_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_HEALTHCARE_AI_INFERENCE_H */
