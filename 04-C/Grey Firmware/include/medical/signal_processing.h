/**
 * @file signal_processing.h
 * @brief Medical Signal Normalization and Processing Module
 *
 * INDUSTRY RELEVANCE:
 * Raw biosensor data requires extensive preprocessing for clinical use.
 * This module demonstrates understanding of:
 * - Real-time digital filtering (IIR/FIR) for noise removal
 * - Baseline wander correction in ECG signals
 * - Motion artifact detection and compensation
 * - Signal normalization for consistent analysis
 * - FDA 510(k) / IEC 62304 software lifecycle awareness
 *
 * Used in: Medical wearables, diagnostic equipment, continuous monitoring
 *
 * @note This is a stub demonstrating medical signal processing patterns.
 */

#ifndef GF_SIGNAL_PROCESSING_H
#define GF_SIGNAL_PROCESSING_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Filter Type Definitions                                                   */
/*===========================================================================*/

/**
 * @brief Filter types for signal processing
 */
typedef enum {
    GF_FILTER_LOWPASS,          /**< Low-pass filter */
    GF_FILTER_HIGHPASS,         /**< High-pass filter */
    GF_FILTER_BANDPASS,         /**< Band-pass filter */
    GF_FILTER_NOTCH,            /**< Notch filter (50/60Hz) */
    GF_FILTER_MEDIAN,           /**< Median filter (spike removal) */
    GF_FILTER_MOVING_AVERAGE,   /**< Moving average smoothing */
    GF_FILTER_SAVGOL            /**< Savitzky-Golay (derivative-preserving) */
} gf_filter_type_t;

/**
 * @brief Filter implementation method
 */
typedef enum {
    GF_FILTER_IIR,              /**< Infinite impulse response (efficient) */
    GF_FILTER_FIR               /**< Finite impulse response (linear phase) */
} gf_filter_impl_t;

/**
 * @brief Normalization methods
 */
typedef enum {
    GF_NORM_MINMAX,             /**< Scale to [0, 1] range */
    GF_NORM_ZSCORE,             /**< Zero mean, unit variance */
    GF_NORM_ROBUST,             /**< Median-based (outlier resistant) */
    GF_NORM_SIGNED,             /**< Scale to [-1, 1] range */
    GF_NORM_FIXED_RANGE         /**< Scale to specified range */
} gf_norm_method_t;

/**
 * @brief Baseline removal methods
 */
typedef enum {
    GF_BASELINE_HIGHPASS,       /**< High-pass filter method */
    GF_BASELINE_POLYNOMIAL,     /**< Polynomial fitting */
    GF_BASELINE_CUBIC_SPLINE,   /**< Cubic spline interpolation */
    GF_BASELINE_WAVELET         /**< Wavelet decomposition */
} gf_baseline_method_t;

/*===========================================================================*/
/* Configuration Structures                                                  */
/*===========================================================================*/

/**
 * @brief Digital filter configuration
 */
typedef struct {
    gf_filter_type_t type;
    gf_filter_impl_t implementation;
    float cutoff_low_hz;        /**< Low cutoff (highpass/bandpass) */
    float cutoff_high_hz;       /**< High cutoff (lowpass/bandpass) */
    float sample_rate_hz;       /**< Input sample rate */
    uint8_t order;              /**< Filter order (1-8) */
    float q_factor;             /**< Q factor for notch filter */
} gf_filter_config_t;

/**
 * @brief Signal normalization configuration
 */
typedef struct {
    gf_norm_method_t method;
    float window_seconds;       /**< Adaptation window size */
    float fixed_min;            /**< For FIXED_RANGE method */
    float fixed_max;            /**< For FIXED_RANGE method */
    bool adaptive;              /**< Enable adaptive normalization */
} gf_norm_config_t;

/**
 * @brief Complete signal processing chain configuration
 */
typedef struct {
    /* Pre-filtering */
    bool enable_dc_removal;
    
    /* Notch filtering */
    bool enable_notch;
    float notch_freq_hz;        /**< 50 or 60 Hz */
    float notch_q;              /**< Q factor (typically 30) */
    
    /* Band-pass filtering */
    bool enable_bandpass;
    float bandpass_low_hz;      /**< ECG: 0.5 Hz, PPG: 0.1 Hz */
    float bandpass_high_hz;     /**< ECG: 40 Hz, PPG: 5 Hz */
    uint8_t bandpass_order;
    
    /* Baseline correction */
    bool enable_baseline;
    gf_baseline_method_t baseline_method;
    
    /* Normalization */
    bool enable_normalization;
    gf_norm_config_t norm_config;
    
    /* Motion artifact */
    bool enable_motion_detect;
    float motion_threshold;
    
    float sample_rate_hz;
} gf_signal_chain_config_t;

/**
 * @brief Signal processing statistics
 */
typedef struct {
    float signal_mean;
    float signal_std;
    float signal_min;
    float signal_max;
    float snr_db;               /**< Estimated signal-to-noise ratio */
    uint32_t samples_processed;
    uint32_t artifacts_detected;
    bool baseline_stable;
} gf_signal_stats_t;

/**
 * @brief Signal processing handle
 */
typedef struct gf_signal_processor* gf_signal_processor_t;

/*===========================================================================*/
/* Signal Processing API                                                     */
/*===========================================================================*/

/**
 * @brief Initialize signal processing chain
 * @param config Chain configuration
 * @param processor Output handle
 * @return 0 on success
 */
int gf_signal_init(const gf_signal_chain_config_t* config,
                   gf_signal_processor_t* processor);

/**
 * @brief Process a block of samples
 * @param processor Processing handle
 * @param input Input samples (raw)
 * @param output Output samples (processed)
 * @param count Number of samples
 * @return Number of output samples, negative on error
 */
int gf_signal_process(gf_signal_processor_t processor,
                      const int32_t* input,
                      float* output,
                      size_t count);

/**
 * @brief Process single sample (real-time)
 * @param processor Processing handle
 * @param input Raw input sample
 * @param output Processed output
 * @return 0 on success
 */
int gf_signal_process_sample(gf_signal_processor_t processor,
                             int32_t input,
                             float* output);

/**
 * @brief Reset processing state (clear filter history)
 * @param processor Processing handle
 */
void gf_signal_reset(gf_signal_processor_t processor);

/**
 * @brief Get processing statistics
 * @param processor Processing handle
 * @param stats Output statistics
 * @return 0 on success
 */
int gf_signal_get_stats(gf_signal_processor_t processor,
                        gf_signal_stats_t* stats);

/**
 * @brief Check if motion artifact detected
 * @param processor Processing handle
 * @return true if motion artifact present
 */
bool gf_signal_motion_detected(gf_signal_processor_t processor);

/**
 * @brief Deinitialize processor
 * @param processor Processing handle
 */
void gf_signal_deinit(gf_signal_processor_t processor);

/*===========================================================================*/
/* Individual Filter Functions                                               */
/*===========================================================================*/

/**
 * @brief Apply IIR low-pass filter
 * @param state Filter state (2 * order floats)
 * @param coeffs Filter coefficients
 * @param order Filter order
 * @param sample Input sample
 * @return Filtered output
 */
float gf_filter_iir_lowpass(float* state, const float* coeffs,
                            uint8_t order, float sample);

/**
 * @brief Apply notch filter (Butterworth)
 * @param state Filter state (4 floats)
 * @param center_freq_hz Notch frequency
 * @param q_factor Q factor
 * @param sample_rate Sample rate
 * @param sample Input sample
 * @return Filtered output
 */
float gf_filter_notch(float* state, float center_freq_hz,
                      float q_factor, float sample_rate, float sample);

/**
 * @brief Calculate filter coefficients
 * @param config Filter configuration
 * @param coeffs Output coefficient array
 * @param max_coeffs Maximum coefficients
 * @return Number of coefficients
 */
int gf_filter_calc_coeffs(const gf_filter_config_t* config,
                          float* coeffs, size_t max_coeffs);

/*===========================================================================*/
/* ECG-Specific Processing                                                   */
/*===========================================================================*/

/**
 * @brief ECG processing result
 */
typedef struct {
    float processed_sample;     /**< Filtered ECG value */
    bool r_peak_detected;       /**< R-peak in this sample */
    float heart_rate_bpm;       /**< Instantaneous HR */
    float rr_interval_ms;       /**< R-R interval */
    bool artifact_present;      /**< Motion/noise artifact */
} gf_ecg_result_t;

/**
 * @brief Process ECG sample with R-peak detection
 * @param processor Processing handle
 * @param raw_sample Raw ADC sample
 * @param result Processing result
 * @return 0 on success
 */
int gf_ecg_process(gf_signal_processor_t processor,
                   int32_t raw_sample,
                   gf_ecg_result_t* result);

/*===========================================================================*/
/* PPG-Specific Processing                                                   */
/*===========================================================================*/

/**
 * @brief PPG processing result
 */
typedef struct {
    float processed_red;        /**< Processed red channel */
    float processed_ir;         /**< Processed IR channel */
    float spo2_ratio;           /**< R value for SpO2 calculation */
    float spo2_percent;         /**< Estimated SpO2 (%) */
    float pulse_rate_bpm;       /**< Pulse rate */
    float perfusion_index;      /**< DC/AC ratio indicator */
    bool valid;                 /**< Measurement validity */
} gf_ppg_result_t;

/**
 * @brief Process PPG samples for SpO2
 * @param processor Processing handle
 * @param red_sample Red LED sample
 * @param ir_sample IR LED sample
 * @param result Processing result
 * @return 0 on success
 */
int gf_ppg_process(gf_signal_processor_t processor,
                   int32_t red_sample,
                   int32_t ir_sample,
                   gf_ppg_result_t* result);

#ifdef __cplusplus
}
#endif

#endif /* GF_SIGNAL_PROCESSING_H */
