/**
 * @file show_inference.h
 * @brief AI Inference Pipeline for Adaptive Shows Stub
 * 
 * Industry Relevance:
 * Modern entertainment experiences use AI to personalize and adapt shows
 * in real-time based on audience behavior. This module demonstrates:
 * - Audience sentiment analysis from audio/video
 * - Real-time show pacing adjustment
 * - Guest flow prediction for queue management
 * - Animatronic/character AI behavior generation
 * 
 * Applications: Theme parks, interactive exhibits, escape rooms, immersive theater
 * 
 * @author Grey Firmware Project
 */

#ifndef SHOW_INFERENCE_H
#define SHOW_INFERENCE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Audience sentiment */
typedef enum {
    SENTIMENT_EXCITED,
    SENTIMENT_ENGAGED,
    SENTIMENT_NEUTRAL,
    SENTIMENT_BORED,
    SENTIMENT_NEGATIVE
} audience_sentiment_t;

/** Show adaptation type */
typedef enum {
    ADAPT_TIMING,            /**< Adjust timing/pacing */
    ADAPT_INTENSITY,         /**< Adjust effect intensity */
    ADAPT_CONTENT,           /**< Select alternate content */
    ADAPT_INTERACTION        /**< Trigger guest interaction */
} adaptation_type_t;

/** Model type */
typedef enum {
    MODEL_SENTIMENT,         /**< Sentiment analysis CNN */
    MODEL_FLOW,              /**< Guest flow prediction */
    MODEL_BEHAVIOR,          /**< Character behavior RNN */
    MODEL_VOICE              /**< Voice recognition */
} model_type_t;

/** Audience analysis result */
typedef struct {
    audience_sentiment_t sentiment; /**< Overall sentiment */
    float engagement_score;  /**< Engagement (0-100) */
    float energy_level;      /**< Energy level (0-100) */
    uint16_t guest_count;    /**< Detected guest count */
    float attention_pct;     /**< Percentage paying attention */
    uint32_t timestamp;      /**< Analysis timestamp */
} audience_analysis_t;

/** Show adaptation recommendation */
typedef struct {
    adaptation_type_t type;  /**< Adaptation type */
    float confidence;        /**< Recommendation confidence */
    int16_t timing_delta_s;  /**< Timing adjustment (seconds) */
    float intensity_scale;   /**< Intensity multiplier */
    uint16_t alternate_cue;  /**< Alternate cue suggestion */
    char interaction[64];    /**< Interaction prompt */
} adaptation_t;

/** Model status */
typedef struct {
    model_type_t type;       /**< Model type */
    bool loaded;             /**< Model loaded */
    float inference_ms;      /**< Last inference time (ms) */
    float accuracy;          /**< Estimated accuracy */
    uint32_t inferences;     /**< Total inferences */
} model_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize show inference system
 * @return 0 on success, negative on error
 */
int show_inference_init(void);

/**
 * @brief Load AI model
 * @param type Model type to load
 * @param model_data Model binary data
 * @param model_size Model size in bytes
 * @return 0 on success, negative on error
 */
int show_inference_load_model(model_type_t type, const uint8_t *model_data, size_t model_size);

/**
 * @brief Analyze current audience
 * @param analysis Output analysis
 * @return 0 on success, negative on error
 */
int show_inference_analyze_audience(audience_analysis_t *analysis);

/**
 * @brief Get show adaptation recommendation
 * @param current_cue Current cue number
 * @param adaptation Output adaptation
 * @return 0 on success, negative on error
 */
int show_inference_get_adaptation(uint16_t current_cue, adaptation_t *adaptation);

/**
 * @brief Run inference (call periodically)
 * @param elapsed_ms Time since last call
 * @return 0 on success, negative on error
 */
int show_inference_update(uint32_t elapsed_ms);

/**
 * @brief Get model status
 * @param type Model type
 * @param status Output status
 * @return 0 on success, negative on error
 */
int show_inference_get_model_status(model_type_t type, model_status_t *status);

/**
 * @brief Shutdown inference system
 */
void show_inference_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* SHOW_INFERENCE_H */
