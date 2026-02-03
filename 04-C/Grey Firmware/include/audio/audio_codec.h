/**
 * @file audio_codec.h
 * @brief Audio Codec Abstraction Layer
 * 
 * INDUSTRY RELEVANCE:
 *   Audio codecs (WM8960, CS43L22, TLV320, MAX98357) are found in:
 *   - Smart Speakers: Voice assistant devices, wireless speakers
 *   - Automotive: Premium audio, noise cancellation, voice recognition
 *   - Wearables: Earbuds, hearing aids, fitness trackers with audio
 *   - Communications: VoIP phones, conference systems, intercoms
 * 
 * This abstraction demonstrates codec-agnostic driver design,
 * I2C/SPI control interfaces, and audio signal path configuration.
 */

#ifndef GF_AUDIO_CODEC_H
#define GF_AUDIO_CODEC_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_CODEC_WM8960 = 0,        /* Wolfson/Cirrus */
    GF_CODEC_CS43L22,           /* Cirrus Logic */
    GF_CODEC_TLV320AIC,         /* TI */
    GF_CODEC_MAX98357,          /* Maxim */
    GF_CODEC_PCM5102,           /* TI DAC-only */
    GF_CODEC_GENERIC
} gf_codec_type_t;

typedef enum {
    GF_CODEC_INPUT_MIC = 0,
    GF_CODEC_INPUT_LINE,
    GF_CODEC_INPUT_DIGITAL
} gf_codec_input_t;

typedef enum {
    GF_CODEC_OUTPUT_HEADPHONE = 0,
    GF_CODEC_OUTPUT_SPEAKER,
    GF_CODEC_OUTPUT_LINE
} gf_codec_output_t;

typedef struct {
    gf_codec_type_t     type;
    uint8_t             i2c_addr;
    uint32_t            mclk_freq;
    uint32_t            sample_rate;
    uint8_t             bit_depth;
    bool                enable_adc;
    bool                enable_dac;
} gf_codec_config_t;

typedef struct {
    int8_t              volume_db;      /* -96 to +12 dB typical */
    bool                muted;
    gf_codec_input_t    active_input;
    gf_codec_output_t   active_output;
} gf_codec_status_t;

/*===========================================================================*/
/* API                                                                        */
/*===========================================================================*/

/**
 * @brief Initialize audio codec
 */
int gf_codec_init(const gf_codec_config_t *config);

/**
 * @brief Power down codec
 */
void gf_codec_deinit(void);

/**
 * @brief Set DAC volume (-96dB to +12dB)
 */
int gf_codec_set_volume(int8_t db);

/**
 * @brief Mute/unmute audio output
 */
void gf_codec_mute(bool mute);

/**
 * @brief Select input source
 */
int gf_codec_select_input(gf_codec_input_t input);

/**
 * @brief Select output destination
 */
int gf_codec_select_output(gf_codec_output_t output);

/**
 * @brief Set microphone gain (0-63 dB)
 */
int gf_codec_set_mic_gain(uint8_t gain_db);

/**
 * @brief Enable/disable automatic gain control
 */
void gf_codec_enable_agc(bool enable);

/**
 * @brief Get codec status
 */
void gf_codec_get_status(gf_codec_status_t *status);

#endif /* GF_AUDIO_CODEC_H */
