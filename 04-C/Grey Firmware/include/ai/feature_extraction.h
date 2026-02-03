/**
 * @file feature_extraction.h
 * @brief Feature Extraction Pipeline for Edge AI
 *
 * INDUSTRY RELEVANCE:
 * Raw sensor data often requires preprocessing before inference:
 * - Signal normalization and scaling
 * - Window-based feature extraction
 * - Spectral analysis (FFT for audio/vibration)
 * - Statistical features (mean, variance, RMS)
 * - Time-domain features (zero crossings, peak detection)
 *
 * This module provides a flexible preprocessing pipeline.
 */

#ifndef GF_FEATURE_EXTRACTION_H
#define GF_FEATURE_EXTRACTION_H

#include "ai/inference.h"
#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Feature Extraction Definitions                                            */
/*===========================================================================*/

/**
 * @brief Maximum pipeline stages
 */
#define GF_FE_MAX_STAGES        8

/**
 * @brief Maximum window size
 */
#define GF_FE_MAX_WINDOW        512

/**
 * @brief Feature extraction stages
 */
typedef enum {
    GF_FE_STAGE_NORMALIZE,      /**< Normalize to [0,1] or [-1,1] */
    GF_FE_STAGE_STANDARDIZE,    /**< Zero mean, unit variance */
    GF_FE_STAGE_WINDOW,         /**< Apply windowing function */
    GF_FE_STAGE_FFT,            /**< Fast Fourier Transform */
    GF_FE_STAGE_MEL_FILTER,     /**< Mel filterbank for audio */
    GF_FE_STAGE_DCT,            /**< Discrete Cosine Transform */
    GF_FE_STAGE_DELTA,          /**< Delta/derivative features */
    GF_FE_STAGE_STATS,          /**< Statistical features */
    GF_FE_STAGE_CUSTOM          /**< Custom processing function */
} gf_fe_stage_type_t;

/**
 * @brief Window types for FFT
 */
typedef enum {
    GF_WINDOW_RECTANGULAR,
    GF_WINDOW_HAMMING,
    GF_WINDOW_HANNING,
    GF_WINDOW_BLACKMAN,
    GF_WINDOW_FLAT_TOP
} gf_window_type_t;

/**
 * @brief Normalization modes
 */
typedef enum {
    GF_NORM_MINMAX_01,          /**< Scale to [0, 1] */
    GF_NORM_MINMAX_11,          /**< Scale to [-1, 1] */
    GF_NORM_ZSCORE,             /**< Zero mean, unit variance */
    GF_NORM_L2,                 /**< Unit L2 norm */
    GF_NORM_ROBUST              /**< Median/IQR based */
} gf_norm_mode_t;

/**
 * @brief Statistical features to extract
 */
typedef enum {
    GF_STAT_MEAN        = (1 << 0),
    GF_STAT_VARIANCE    = (1 << 1),
    GF_STAT_STD         = (1 << 2),
    GF_STAT_RMS         = (1 << 3),
    GF_STAT_MIN         = (1 << 4),
    GF_STAT_MAX         = (1 << 5),
    GF_STAT_RANGE       = (1 << 6),
    GF_STAT_SKEWNESS    = (1 << 7),
    GF_STAT_KURTOSIS    = (1 << 8),
    GF_STAT_ZERO_CROSS  = (1 << 9),
    GF_STAT_PEAK_COUNT  = (1 << 10),
    GF_STAT_ENTROPY     = (1 << 11),
    GF_STAT_ALL         = 0xFFF
} gf_stat_flags_t;

/*===========================================================================*/
/* Stage Configurations                                                      */
/*===========================================================================*/

/**
 * @brief Normalization stage config
 */
typedef struct {
    gf_norm_mode_t mode;
    float min_val;              /**< Known min (for fixed scaling) */
    float max_val;              /**< Known max (for fixed scaling) */
    float mean;                 /**< Pre-computed mean */
    float std;                  /**< Pre-computed std */
    bool use_fixed_params;      /**< Use pre-computed params */
} gf_stage_normalize_t;

/**
 * @brief Window stage config
 */
typedef struct {
    gf_window_type_t type;
    uint16_t window_size;
    uint16_t hop_size;          /**< Overlap (0 = no overlap) */
    bool apply_padding;
} gf_stage_window_t;

/**
 * @brief FFT stage config
 */
typedef struct {
    uint16_t fft_size;          /**< FFT size (power of 2) */
    bool output_magnitude;      /**< Output magnitude only */
    bool output_power;          /**< Output power spectrum */
    bool output_log;            /**< Log scale output */
    float floor_db;             /**< Floor for log output */
} gf_stage_fft_t;

/**
 * @brief Mel filterbank config
 */
typedef struct {
    uint8_t num_filters;        /**< Number of mel filters */
    uint16_t sample_rate;       /**< Sample rate in Hz */
    float low_freq;             /**< Lower frequency bound */
    float high_freq;            /**< Upper frequency bound */
} gf_stage_mel_t;

/**
 * @brief Statistical stage config
 */
typedef struct {
    gf_stat_flags_t features;   /**< Which stats to extract */
    uint16_t window_size;       /**< Window for stats computation */
    float peak_threshold;       /**< Threshold for peak detection */
} gf_stage_stats_t;

/**
 * @brief Custom stage callback
 */
typedef int (*gf_fe_custom_fn)(const float* input,
                                uint16_t in_len,
                                float* output,
                                uint16_t* out_len,
                                void* user_data);

/**
 * @brief Custom stage config
 */
typedef struct {
    gf_fe_custom_fn callback;
    void* user_data;
    uint16_t output_size;       /**< Expected output size */
} gf_stage_custom_t;

/**
 * @brief Generic stage configuration
 */
typedef struct {
    gf_fe_stage_type_t type;
    bool enabled;
    union {
        gf_stage_normalize_t normalize;
        gf_stage_window_t window;
        gf_stage_fft_t fft;
        gf_stage_mel_t mel;
        gf_stage_stats_t stats;
        gf_stage_custom_t custom;
    } config;
} gf_fe_stage_t;

/*===========================================================================*/
/* Pipeline Configuration                                                    */
/*===========================================================================*/

/**
 * @brief Feature extraction pipeline
 */
typedef struct {
    gf_fe_stage_t stages[GF_FE_MAX_STAGES];
    uint8_t num_stages;
    
    uint16_t input_size;        /**< Expected input size */
    uint16_t output_size;       /**< Final output size */
    
    /* Working buffers */
    float* work_buf_a;
    float* work_buf_b;
    uint16_t work_buf_size;
} gf_fe_pipeline_t;

/**
 * @brief Pipeline handle
 */
typedef gf_fe_pipeline_t* gf_feature_t;

/**
 * @brief Pipeline status
 */
typedef enum {
    GF_FE_OK = 0,
    GF_FE_ERROR_CONFIG = -1,
    GF_FE_ERROR_MEMORY = -2,
    GF_FE_ERROR_SIZE = -3,
    GF_FE_ERROR_STAGE = -4,
    GF_FE_ERROR_OVERFLOW = -5
} gf_fe_status_t;

/*===========================================================================*/
/* Pipeline API                                                              */
/*===========================================================================*/

/**
 * @brief Initialize feature extraction pipeline
 * @param input_size Expected input size
 * @param work_buffer Working buffer (size >= 2 * MAX(input, output))
 * @param work_size Working buffer size
 * @param pipeline Output handle
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_init(uint16_t input_size,
                                float* work_buffer,
                                uint16_t work_size,
                                gf_feature_t* pipeline);

/**
 * @brief Add normalization stage
 * @param pipeline Pipeline handle
 * @param config Normalization config
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_add_normalize(gf_feature_t pipeline,
                                         const gf_stage_normalize_t* config);

/**
 * @brief Add windowing stage
 * @param pipeline Pipeline handle
 * @param config Window config
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_add_window(gf_feature_t pipeline,
                                      const gf_stage_window_t* config);

/**
 * @brief Add FFT stage
 * @param pipeline Pipeline handle
 * @param config FFT config
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_add_fft(gf_feature_t pipeline,
                                   const gf_stage_fft_t* config);

/**
 * @brief Add mel filterbank stage
 * @param pipeline Pipeline handle
 * @param config Mel config
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_add_mel(gf_feature_t pipeline,
                                   const gf_stage_mel_t* config);

/**
 * @brief Add statistical features stage
 * @param pipeline Pipeline handle
 * @param config Stats config
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_add_stats(gf_feature_t pipeline,
                                     const gf_stage_stats_t* config);

/**
 * @brief Add custom processing stage
 * @param pipeline Pipeline handle
 * @param config Custom config
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_add_custom(gf_feature_t pipeline,
                                      const gf_stage_custom_t* config);

/**
 * @brief Process input through pipeline
 * @param pipeline Pipeline handle
 * @param input Input samples
 * @param input_len Input length
 * @param output Output features
 * @param output_len Output length (in/out)
 * @return GF_FE_OK on success
 */
gf_fe_status_t gf_feature_process(gf_feature_t pipeline,
                                   const float* input,
                                   uint16_t input_len,
                                   float* output,
                                   uint16_t* output_len);

/**
 * @brief Get expected output size
 * @param pipeline Pipeline handle
 * @return Output size in floats
 */
uint16_t gf_feature_get_output_size(gf_feature_t pipeline);

/**
 * @brief Reset pipeline state
 * @param pipeline Pipeline handle
 */
void gf_feature_reset(gf_feature_t pipeline);

/**
 * @brief Deinitialize pipeline
 * @param pipeline Pipeline handle
 */
void gf_feature_deinit(gf_feature_t pipeline);

/*===========================================================================*/
/* Standalone Feature Utilities                                              */
/*===========================================================================*/

/**
 * @brief Compute mean of array
 */
float gf_fe_mean(const float* data, uint16_t len);

/**
 * @brief Compute variance of array
 */
float gf_fe_variance(const float* data, uint16_t len);

/**
 * @brief Compute RMS of array
 */
float gf_fe_rms(const float* data, uint16_t len);

/**
 * @brief Count zero crossings
 */
uint16_t gf_fe_zero_crossings(const float* data, uint16_t len);

/**
 * @brief Detect peaks in data
 * @param data Input data
 * @param len Data length
 * @param threshold Minimum peak height
 * @param peaks Output peak indices
 * @param max_peaks Maximum peaks to find
 * @return Number of peaks found
 */
uint16_t gf_fe_find_peaks(const float* data,
                           uint16_t len,
                           float threshold,
                           uint16_t* peaks,
                           uint16_t max_peaks);

/**
 * @brief Apply window function in-place
 * @param data Data to window
 * @param len Data length
 * @param type Window type
 */
void gf_fe_apply_window(float* data, uint16_t len, gf_window_type_t type);

/**
 * @brief Compute FFT magnitude spectrum
 * @param input Complex input (interleaved real/imag)
 * @param magnitude Output magnitudes
 * @param fft_size FFT size
 */
void gf_fe_fft_magnitude(const float* input,
                          float* magnitude,
                          uint16_t fft_size);

/**
 * @brief Apply mel filterbank to spectrum
 * @param spectrum Power spectrum
 * @param spec_len Spectrum length
 * @param mel_out Mel energies output
 * @param num_filters Number of mel filters
 * @param sample_rate Sample rate
 * @param low_freq Low frequency
 * @param high_freq High frequency
 */
void gf_fe_mel_filterbank(const float* spectrum,
                           uint16_t spec_len,
                           float* mel_out,
                           uint8_t num_filters,
                           uint16_t sample_rate,
                           float low_freq,
                           float high_freq);

#ifdef __cplusplus
}
#endif

#endif /* GF_FEATURE_EXTRACTION_H */
