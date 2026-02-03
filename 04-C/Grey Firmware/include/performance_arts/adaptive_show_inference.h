/**
 * @file adaptive_show_inference.h
 * @brief AI Inference Pipeline for Adaptive Show Control
 * 
 * INDUSTRY RELEVANCE:
 * Next-generation live entertainment uses AI to dynamically adjust show
 * elements based on audience response, performer cues, and environmental
 * conditions. This module provides real-time inference for lighting,
 * audio, effects, and narrative branching in immersive experiences.
 * 
 * Applications:
 * - Interactive theme park attractions
 * - Adaptive concert lighting
 * - Immersive theater experiences
 * - AI DJ/VJ systems
 * - Responsive art installations
 * 
 * Standards: Entertainment industry ML deployment best practices
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ADAPTIVE_SHOW_INFERENCE_H
#define GF_ADAPTIVE_SHOW_INFERENCE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define SHOW_MAX_MODELS             8       /* Maximum inference models */
#define SHOW_INFERENCE_RATE_HZ      30      /* Inference frequency */
#define SHOW_MAX_CUES               256     /* Maximum show cues */
#define SHOW_MAX_BRANCHES           16      /* Maximum narrative branches */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Model type */
typedef enum {
    MODEL_EMOTION_DETECTION,    /* Audience emotion analysis */
    MODEL_ENERGY_PREDICTION,    /* Energy level prediction */
    MODEL_GESTURE_RECOGNITION,  /* Performer gesture tracking */
    MODEL_AUDIO_ANALYSIS,       /* Music/audio classification */
    MODEL_CROWD_FLOW,           /* Crowd movement prediction */
    MODEL_LIGHTING_GEN,         /* Generative lighting patterns */
    MODEL_EFFECTS_TIMING        /* Effect timing optimization */
} show_model_t;

/** Emotion classification */
typedef enum {
    EMOTION_NEUTRAL,
    EMOTION_EXCITED,
    EMOTION_JOYFUL,
    EMOTION_AMAZED,
    EMOTION_TENSE,
    EMOTION_DISAPPOINTED,
    EMOTION_BORED
} audience_emotion_t;

/** Show intensity level */
typedef enum {
    INTENSITY_MINIMAL,
    INTENSITY_LOW,
    INTENSITY_MODERATE,
    INTENSITY_HIGH,
    INTENSITY_MAXIMUM
} show_intensity_t;

/** Cue recommendation */
typedef struct {
    uint16_t cue_id;
    char cue_name[32];
    float confidence;
    float timing_offset_s;
    show_intensity_t intensity;
    bool override_current;
} cue_recommendation_t;

/** Inference result */
typedef struct {
    show_model_t model;
    audience_emotion_t emotion;
    float energy_level;         /* 0.0 - 1.0 */
    float engagement_score;     /* 0.0 - 1.0 */
    show_intensity_t recommended_intensity;
    cue_recommendation_t next_cue;
    uint32_t inference_time_us;
    uint32_t timestamp_ms;
} inference_result_t;

/** Narrative state */
typedef struct {
    uint8_t current_branch;
    uint16_t current_beat;
    float branch_probabilities[SHOW_MAX_BRANCHES];
    uint8_t recommended_branch;
    float time_to_decision_s;
} narrative_state_t;

/** Show adaptation */
typedef struct {
    show_intensity_t lighting_intensity;
    show_intensity_t audio_intensity;
    show_intensity_t effects_intensity;
    float tempo_adjustment;     /* -1.0 to 1.0 */
    float energy_target;
    bool pyro_enabled;
    bool audience_interaction;
} show_adaptation_t;

/** Model performance */
typedef struct {
    show_model_t type;
    uint32_t inference_count;
    float avg_latency_ms;
    float accuracy_estimate;
    float confidence_avg;
    bool loaded;
    bool active;
} model_stats_t;

/** Module configuration */
typedef struct {
    uint8_t num_models;
    bool adaptive_lighting;
    bool adaptive_audio;
    bool narrative_branching;
    float min_confidence;
    uint32_t max_latency_ms;
} show_inference_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int show_inference_init(const show_inference_config_t *config);
void show_inference_shutdown(void);

int show_inference_load_model(show_model_t type, const void *model_data, size_t size);
int show_inference_unload_model(show_model_t type);
int show_inference_enable_model(show_model_t type, bool enable);

int show_inference_run(const void *input_data, size_t input_size,
                        inference_result_t *result);
int show_inference_get_adaptation(show_adaptation_t *adaptation);
int show_inference_get_narrative(narrative_state_t *state);

int show_inference_set_target_energy(float energy);
int show_inference_force_intensity(show_intensity_t intensity);
int show_inference_trigger_cue(uint16_t cue_id);

int show_inference_get_model_stats(show_model_t type, model_stats_t *stats);

void show_inference_update(uint32_t elapsed_ms);

#endif /* GF_ADAPTIVE_SHOW_INFERENCE_H */
