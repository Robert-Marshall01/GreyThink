/**
 * @file audio_pipeline.h
 * @brief Simple DAC/ADC Audio Pipeline
 * 
 * INDUSTRY RELEVANCE:
 *   Audio pipelines process audio data through configurable stages:
 *   - Voice Assistants: VAD → Wake word → ASR → TTS → Speaker
 *   - Automotive: ANC → Equalizer → Crossover → Amplifier
 *   - Teleconference: Echo cancel → Noise reduce → AGC → Encode
 *   - Medical: Filter → Amplify → Analyze → Alert
 * 
 * This stub demonstrates DSP integration points, buffer management,
 * and multi-stage audio processing architecture.
 */

#ifndef GF_AUDIO_PIPELINE_H
#define GF_AUDIO_PIPELINE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_AUDIO_STAGE_SOURCE = 0,      /* Input: ADC, I2S RX, file */
    GF_AUDIO_STAGE_FILTER,          /* DSP: EQ, lowpass, highpass */
    GF_AUDIO_STAGE_GAIN,            /* Volume/AGC */
    GF_AUDIO_STAGE_EFFECT,          /* Reverb, echo, etc. */
    GF_AUDIO_STAGE_ENCODE,          /* Compression, codec */
    GF_AUDIO_STAGE_DECODE,          /* Decompression */
    GF_AUDIO_STAGE_SINK             /* Output: DAC, I2S TX, file */
} gf_audio_stage_type_t;

typedef struct {
    gf_audio_stage_type_t   type;
    const char             *name;
    int  (*process)(int16_t *samples, size_t count, void *ctx);
    void                   *ctx;
    bool                    enabled;
    uint8_t                 order;      /* Processing order */
} gf_audio_stage_t;

typedef struct {
    uint32_t    sample_rate;
    uint8_t     channels;
    uint8_t     bit_depth;
    size_t      buffer_size_samples;
    bool        use_dma;
} gf_audio_pipeline_config_t;

typedef struct {
    bool        running;
    uint32_t    samples_processed;
    uint32_t    underruns;
    uint32_t    overruns;
    uint32_t    latency_us;
} gf_audio_pipeline_status_t;

/*===========================================================================*/
/* API                                                                        */
/*===========================================================================*/

/**
 * @brief Initialize audio pipeline
 */
int gf_audio_pipeline_init(const gf_audio_pipeline_config_t *config);

/**
 * @brief Add processing stage to pipeline
 */
int gf_audio_pipeline_add_stage(const gf_audio_stage_t *stage);

/**
 * @brief Remove processing stage
 */
int gf_audio_pipeline_remove_stage(const char *name);

/**
 * @brief Enable/disable a stage
 */
int gf_audio_pipeline_enable_stage(const char *name, bool enable);

/**
 * @brief Start audio pipeline
 */
int gf_audio_pipeline_start(void);

/**
 * @brief Stop audio pipeline
 */
void gf_audio_pipeline_stop(void);

/**
 * @brief Process a buffer of samples through pipeline
 */
int gf_audio_pipeline_process(int16_t *samples, size_t count);

/**
 * @brief Get pipeline status
 */
void gf_audio_pipeline_get_status(gf_audio_pipeline_status_t *status);

/**
 * @brief Set master volume (0-100)
 */
void gf_audio_pipeline_set_volume(uint8_t volume);

#endif /* GF_AUDIO_PIPELINE_H */
