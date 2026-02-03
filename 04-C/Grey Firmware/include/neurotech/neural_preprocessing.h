/**
 * @file neural_preprocessing.h
 * @brief Neural Signal Preprocessing Module
 * 
 * @details
 * Real-time digital signal processing for neural signals. Implements
 * filtering, artifact rejection, feature extraction, and signal
 * conditioning for downstream analysis and classification.
 * 
 * INDUSTRY RELEVANCE:
 * - Brain-computer interfaces (OpenBCI, g.tec)
 * - Cochlear implant processors
 * - Deep brain stimulation systems
 * - Sleep staging algorithms
 * - Seizure detection systems
 * 
 * KEY FEATURES:
 * - IIR/FIR filter implementations
 * - Common Average Reference (CAR)
 * - Artifact removal (EOG, EMG)
 * - Band power extraction (delta, theta, alpha, beta, gamma)
 * - Time-frequency analysis (STFT)
 * - Adaptive noise cancellation
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_NEURAL_PREPROCESSING_H
#define GF_NEURAL_PREPROCESSING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum filter order */
#define GF_NEURAL_MAX_FILTER_ORDER  16

/** FFT window size */
#define GF_NEURAL_FFT_SIZE          256

/** Number of frequency bands */
#define GF_NEURAL_NUM_BANDS         5

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Preprocessing status codes
 */
typedef enum {
    GF_NEURAL_OK = 0,
    GF_NEURAL_ERROR_NOT_INIT,
    GF_NEURAL_ERROR_NULL_PTR,
    GF_NEURAL_ERROR_INVALID_PARAM,
    GF_NEURAL_ERROR_BUFFER_SIZE,
    GF_NEURAL_WARN_ARTIFACT,
    GF_NEURAL_WARN_SATURATION
} gf_neural_status_t;

/**
 * @brief Filter types
 */
typedef enum {
    GF_NEURAL_FILTER_LOWPASS,
    GF_NEURAL_FILTER_HIGHPASS,
    GF_NEURAL_FILTER_BANDPASS,
    GF_NEURAL_FILTER_BANDSTOP,
    GF_NEURAL_FILTER_NOTCH
} gf_neural_filter_type_t;

/**
 * @brief Frequency bands
 */
typedef enum {
    GF_NEURAL_BAND_DELTA = 0,     /**< 0.5-4 Hz */
    GF_NEURAL_BAND_THETA,         /**< 4-8 Hz */
    GF_NEURAL_BAND_ALPHA,         /**< 8-13 Hz */
    GF_NEURAL_BAND_BETA,          /**< 13-30 Hz */
    GF_NEURAL_BAND_GAMMA          /**< 30-100 Hz */
} gf_neural_band_t;

/**
 * @brief Artifact types
 */
typedef enum {
    GF_NEURAL_ARTIFACT_NONE = 0,
    GF_NEURAL_ARTIFACT_EOG,       /**< Eye movement */
    GF_NEURAL_ARTIFACT_EMG,       /**< Muscle activity */
    GF_NEURAL_ARTIFACT_ECG,       /**< Heart artifact */
    GF_NEURAL_ARTIFACT_MOVEMENT,  /**< Motion artifact */
    GF_NEURAL_ARTIFACT_LINE       /**< Line noise */
} gf_neural_artifact_t;

/**
 * @brief Filter configuration
 */
typedef struct {
    gf_neural_filter_type_t type; /**< Filter type */
    uint8_t order;                /**< Filter order */
    float freq_low_hz;            /**< Low cutoff frequency */
    float freq_high_hz;           /**< High cutoff frequency */
    float ripple_db;              /**< Passband ripple */
} gf_neural_filter_config_t;

/**
 * @brief Band power result
 */
typedef struct {
    float power[GF_NEURAL_NUM_BANDS]; /**< Power per band (uV^2) */
    float relative[GF_NEURAL_NUM_BANDS]; /**< Relative power (%) */
    float total_power;             /**< Total power */
    float peak_freq_hz;            /**< Peak frequency */
} gf_neural_band_power_t;

/**
 * @brief Preprocessed signal
 */
typedef struct {
    int32_t* samples;             /**< Preprocessed samples */
    uint32_t sample_count;        /**< Number of samples */
    gf_neural_artifact_t artifact; /**< Detected artifact */
    bool artifact_rejected;        /**< Sample rejected */
    float snr_db;                  /**< Signal-to-noise ratio */
} gf_neural_signal_t;

/**
 * @brief Processing pipeline configuration
 */
typedef struct {
    bool enable_car;               /**< Common Average Reference */
    bool enable_artifact_removal;  /**< Artifact removal */
    bool enable_notch_50hz;        /**< 50Hz notch */
    bool enable_notch_60hz;        /**< 60Hz notch */
    gf_neural_filter_config_t bandpass; /**< Bandpass filter */
    float artifact_threshold_uv;   /**< Artifact threshold */
} gf_neural_pipeline_config_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize preprocessing module
 * @return Status code
 */
gf_neural_status_t gf_neural_init(void);

/**
 * @brief Shutdown preprocessing module
 */
void gf_neural_shutdown(void);

/**
 * @brief Configure processing pipeline
 * @param config Pipeline configuration
 * @return Status code
 */
gf_neural_status_t gf_neural_configure(const gf_neural_pipeline_config_t* config);

/**
 * @brief Apply filter to signal
 * @param config Filter configuration
 * @param input Input samples
 * @param output Output samples
 * @param count Sample count
 * @return Status code
 */
gf_neural_status_t gf_neural_filter(const gf_neural_filter_config_t* config,
                                     const int32_t* input, int32_t* output,
                                     uint32_t count);

/**
 * @brief Apply Common Average Reference
 * @param channels Input channels
 * @param output Output channels
 * @param channel_count Number of channels
 * @param sample_count Samples per channel
 * @return Status code
 */
gf_neural_status_t gf_neural_apply_car(const int32_t** channels,
                                        int32_t** output,
                                        uint8_t channel_count,
                                        uint32_t sample_count);

/**
 * @brief Detect and remove artifacts
 * @param signal Input/output signal
 * @return Detected artifact type
 */
gf_neural_artifact_t gf_neural_artifact_removal(gf_neural_signal_t* signal);

/**
 * @brief Calculate band power
 * @param samples Input samples
 * @param count Sample count
 * @param sample_rate_hz Sample rate
 * @param result Output band power
 * @return Status code
 */
gf_neural_status_t gf_neural_band_power(const int32_t* samples,
                                         uint32_t count,
                                         float sample_rate_hz,
                                         gf_neural_band_power_t* result);

/**
 * @brief Process signal through pipeline
 * @param input Input samples
 * @param output Output processed signal
 * @param count Sample count
 * @return Status code
 */
gf_neural_status_t gf_neural_process(const int32_t* input,
                                      gf_neural_signal_t* output,
                                      uint32_t count);

#ifdef __cplusplus
}
#endif

#endif /* GF_NEURAL_PREPROCESSING_H */
