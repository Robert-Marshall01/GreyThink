/**
 * @file adaptive_sound.h
 * @brief AI Inference Pipeline for Adaptive Sound Processing
 * 
 * INDUSTRY RELEVANCE:
 * AI-powered adaptive audio is revolutionizing:
 * - Real-time audio effects that respond to playing style
 * - Automatic accompaniment generation
 * - Intelligent audio mixing and mastering
 * - Accessibility through sound-to-MIDI conversion
 * - Gaming and VR adaptive soundtracks
 * 
 * Powers products from: LANDR, iZotope, Native Instruments, Amper Music
 * AI audio market: $1.5B by 2026
 * 
 * STANDARDS: VST3, Audio Units, JUCE Framework compatibility
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ADAPTIVE_SOUND_H
#define GF_ADAPTIVE_SOUND_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* AI processing modes */
typedef enum {
    AI_MODE_EFFECTS,         /**< Adaptive effects processing */
    AI_MODE_ACCOMPANIMENT,   /**< Generate accompaniment */
    AI_MODE_MIXING,          /**< Intelligent mixing */
    AI_MODE_TRANSCRIPTION,   /**< Audio to notation */
    AI_MODE_SYNTHESIS        /**< Sound synthesis control */
} ai_mode_t;

/* Musical style/genre hints */
typedef enum {
    STYLE_NEUTRAL,
    STYLE_JAZZ,
    STYLE_ROCK,
    STYLE_CLASSICAL,
    STYLE_ELECTRONIC,
    STYLE_POP,
    STYLE_METAL,
    STYLE_AMBIENT
} music_style_t;

/* Effect parameters (AI-controlled) */
typedef struct {
    float reverb_amount;
    float delay_time_ms;
    float delay_feedback;
    float chorus_rate;
    float distortion;
    float compression_ratio;
    float eq_bass;
    float eq_mid;
    float eq_treble;
} effect_params_t;

/* AI inference result */
typedef struct {
    music_style_t detected_style;
    float style_confidence;
    float tempo_estimate;
    uint8_t key_signature;
    bool in_solo_section;
    bool energy_building;
    float complexity_score;
    effect_params_t suggested_effects;
} inference_result_t;

/* Model configuration */
typedef struct {
    ai_mode_t mode;
    music_style_t style_hint;
    float responsiveness;    /**< 0-1, faster = more reactive */
    float creativity;        /**< 0-1, higher = more variation */
    bool low_latency;        /**< Prioritize latency over quality */
    uint32_t block_size;     /**< Audio block size */
} ai_config_t;

/**
 * @brief Initialize AI sound pipeline
 * @param config AI configuration
 * @return 0 on success
 */
int adaptive_sound_init(const ai_config_t *config);

/**
 * @brief Process audio block through AI
 * @param input Input audio buffer
 * @param output Output audio buffer
 * @param samples Number of samples
 * @return 0 on success
 */
int adaptive_sound_process(const float *input, float *output, uint32_t samples);

/**
 * @brief Get current inference result
 * @param result Output inference result
 * @return 0 on success
 */
int adaptive_sound_get_inference(inference_result_t *result);

/**
 * @brief Set processing mode
 * @param mode AI processing mode
 * @return 0 on success
 */
int adaptive_sound_set_mode(ai_mode_t mode);

/**
 * @brief Update style hint
 * @param style Music style hint
 */
void adaptive_sound_set_style(music_style_t style);

/**
 * @brief Train on user preferences
 * @param feedback User feedback signal (-1 to 1)
 * @return 0 on success
 */
int adaptive_sound_feedback(float feedback);

/**
 * @brief Get latency in samples
 * @return Processing latency in samples
 */
uint32_t adaptive_sound_get_latency(void);

/**
 * @brief Enable/disable bypass mode
 * @param bypass true to bypass AI processing
 */
void adaptive_sound_bypass(bool bypass);

/**
 * @brief Get CPU usage percentage
 * @return CPU usage (0-100)
 */
float adaptive_sound_cpu_usage(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ADAPTIVE_SOUND_H */
