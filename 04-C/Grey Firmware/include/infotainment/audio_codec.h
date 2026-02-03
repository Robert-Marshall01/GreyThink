/**
 * @file audio_codec.h
 * @brief Automotive Audio Codec Driver Interface
 *
 * INDUSTRY RELEVANCE:
 * Automotive infotainment audio systems are a $15B+ market driving innovation
 * in in-vehicle entertainment and communication. This module demonstrates:
 * - Automotive-grade audio codec configuration (I2S, TDM)
 * - Multi-channel audio routing for premium sound systems
 * - DSP integration for active noise cancellation
 * - Hands-free calling and voice assistant integration
 *
 * These skills apply to roles at automotive OEMs (Tesla, BMW, Mercedes),
 * Tier-1 suppliers (Harman, Bose, Bang & Olufsen), and semiconductor
 * companies (Qualcomm, TI, NXP) developing infotainment platforms.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires automotive-qualified components.
 */

#ifndef GF_AUDIO_CODEC_H
#define GF_AUDIO_CODEC_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_AUDIO_MAX_CHANNELS       16      /**< Maximum audio channels */
#define GF_AUDIO_MAX_SAMPLE_RATE    192000  /**< Maximum sample rate */
#define GF_AUDIO_MAX_ZONES          4       /**< Maximum audio zones */
#define GF_AUDIO_MAX_SOURCES        8       /**< Maximum audio sources */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Audio interface types
 */
typedef enum {
    GF_AUDIO_IF_I2S,            /**< I2S interface */
    GF_AUDIO_IF_TDM,            /**< TDM (Time Division Multiplex) */
    GF_AUDIO_IF_SPDIF,          /**< S/PDIF optical/coax */
    GF_AUDIO_IF_A2B,            /**< Analog Devices A2B automotive bus */
    GF_AUDIO_IF_MOST,           /**< Media Oriented Systems Transport */
    GF_AUDIO_IF_AVB             /**< Audio Video Bridging (Ethernet) */
} gf_audio_interface_t;

/**
 * @brief Sample format
 */
typedef enum {
    GF_AUDIO_FMT_S16LE,         /**< Signed 16-bit little-endian */
    GF_AUDIO_FMT_S24LE,         /**< Signed 24-bit little-endian */
    GF_AUDIO_FMT_S32LE,         /**< Signed 32-bit little-endian */
    GF_AUDIO_FMT_FLOAT32        /**< 32-bit float */
} gf_audio_format_t;

/**
 * @brief Audio source types
 */
typedef enum {
    GF_AUDIO_SRC_MEDIA,         /**< Media player (USB, streaming) */
    GF_AUDIO_SRC_RADIO,         /**< AM/FM/DAB radio */
    GF_AUDIO_SRC_BLUETOOTH,     /**< Bluetooth A2DP */
    GF_AUDIO_SRC_PHONE,         /**< Hands-free phone call */
    GF_AUDIO_SRC_NAVIGATION,    /**< Navigation prompts */
    GF_AUDIO_SRC_ADAS,          /**< ADAS warnings */
    GF_AUDIO_SRC_VOICE_ASST,    /**< Voice assistant */
    GF_AUDIO_SRC_CHIME          /**< System chimes */
} gf_audio_source_t;

/**
 * @brief Audio zone (spatial zones in vehicle)
 */
typedef enum {
    GF_AUDIO_ZONE_DRIVER,       /**< Driver zone */
    GF_AUDIO_ZONE_PASSENGER,    /**< Front passenger */
    GF_AUDIO_ZONE_REAR_LEFT,    /**< Rear left passenger */
    GF_AUDIO_ZONE_REAR_RIGHT    /**< Rear right passenger */
} gf_audio_zone_t;

/**
 * @brief Codec configuration
 */
typedef struct {
    gf_audio_interface_t interface;     /**< Hardware interface */
    gf_audio_format_t format;           /**< Sample format */
    uint32_t sample_rate;               /**< Sample rate (Hz) */
    uint8_t channels;                   /**< Number of channels */
    uint8_t slot_width;                 /**< TDM slot width (bits) */
    uint8_t frame_sync_width;           /**< Frame sync pulse width */
    bool master_mode;                   /**< True = codec is master */
    uint8_t mclk_divider;               /**< Master clock divider */
} gf_audio_codec_config_t;

/**
 * @brief Channel routing entry
 */
typedef struct {
    gf_audio_source_t source;           /**< Audio source */
    uint8_t source_channel;             /**< Source channel index */
    uint8_t output_channel;             /**< Physical output channel */
    gf_audio_zone_t zone;               /**< Target zone */
    float gain_db;                      /**< Channel gain (-60 to +12 dB) */
    float pan;                          /**< Pan position (-1.0 to 1.0) */
} gf_audio_route_t;

/**
 * @brief Equalizer band
 */
typedef struct {
    float frequency_hz;                 /**< Center frequency */
    float gain_db;                      /**< Gain (-12 to +12 dB) */
    float q_factor;                     /**< Q factor (bandwidth) */
} gf_audio_eq_band_t;

/**
 * @brief Zone configuration
 */
typedef struct {
    gf_audio_zone_t zone;               /**< Zone identifier */
    float volume;                       /**< Volume (0.0 to 1.0) */
    float balance;                      /**< Left/right balance */
    float fade;                         /**< Front/rear fade */
    gf_audio_eq_band_t eq[10];          /**< 10-band parametric EQ */
    bool loudness_enable;               /**< Loudness compensation */
    bool speed_volume;                  /**< Speed-dependent volume */
} gf_audio_zone_config_t;

/**
 * @brief Audio status
 */
typedef struct {
    bool codec_ready;                   /**< Codec initialized */
    bool dac_active;                    /**< DAC streaming */
    bool adc_active;                    /**< ADC recording */
    uint32_t sample_rate;               /**< Current sample rate */
    uint8_t active_channels;            /**< Active channel count */
    uint32_t underruns;                 /**< DMA underrun count */
    uint32_t overruns;                  /**< DMA overrun count */
} gf_audio_status_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize audio codec subsystem
 * @param config Codec configuration
 * @return 0 on success, negative error code on failure
 */
int gf_audio_init(const gf_audio_codec_config_t* config);

/**
 * @brief Shutdown audio subsystem
 */
void gf_audio_deinit(void);

/**
 * @brief Start audio playback
 * @return 0 on success
 */
int gf_audio_start(void);

/**
 * @brief Stop audio playback
 */
void gf_audio_stop(void);

/**
 * @brief Set master volume
 * @param volume Volume level (0.0 to 1.0)
 */
void gf_audio_set_volume(float volume);

/**
 * @brief Configure audio zone
 * @param config Zone configuration
 * @return 0 on success
 */
int gf_audio_configure_zone(const gf_audio_zone_config_t* config);

/**
 * @brief Add audio route
 * @param route Routing configuration
 * @return 0 on success
 */
int gf_audio_add_route(const gf_audio_route_t* route);

/**
 * @brief Remove audio route
 * @param source Source to unroute
 * @param zone Zone to unroute from
 * @return 0 on success
 */
int gf_audio_remove_route(gf_audio_source_t source, gf_audio_zone_t zone);

/**
 * @brief Mute/unmute zone
 * @param zone Zone to mute
 * @param mute True to mute
 */
void gf_audio_mute_zone(gf_audio_zone_t zone, bool mute);

/**
 * @brief Duck audio for announcement
 * @param source Source requiring duck
 * @param duck_db Amount to duck other sources (negative)
 */
void gf_audio_duck(gf_audio_source_t source, float duck_db);

/**
 * @brief Get audio status
 * @param[out] status Status output
 */
void gf_audio_get_status(gf_audio_status_t* status);

/**
 * @brief Run codec self-test
 * @return 0 on success, negative error code on failure
 */
int gf_audio_self_test(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_AUDIO_CODEC_H */
