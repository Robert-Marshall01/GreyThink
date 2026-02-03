/**
 * @file eeg_sensor.h
 * @brief EEG Sensor Driver for Neurotech Interfaces
 * 
 * @details
 * Multi-channel EEG acquisition driver for brain-computer interface (BCI)
 * and neuromonitoring applications. Supports high-resolution sampling,
 * impedance checking, and artifact rejection.
 * 
 * INDUSTRY RELEVANCE:
 * - Medical EEG systems (Natus, Compumedics)
 * - Brain-computer interfaces (Neuralink, Emotiv)
 * - Sleep monitoring devices
 * - Neurofeedback systems
 * - Clinical research equipment
 * 
 * KEY FEATURES:
 * - Multi-channel acquisition (up to 64 channels)
 * - High-resolution ADC (24-bit)
 * - Configurable sample rates (250Hz - 16kHz)
 * - Real-time impedance monitoring
 * - Hardware bandpass filtering
 * - Artifact detection (eye blink, muscle)
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_EEG_SENSOR_H
#define GF_EEG_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum EEG channels */
#define GF_EEG_MAX_CHANNELS         64

/** Standard 10-20 system channels */
#define GF_EEG_10_20_CHANNELS       21

/** Sample buffer size */
#define GF_EEG_SAMPLE_BUFFER        256

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief EEG status codes
 */
typedef enum {
    GF_EEG_OK = 0,
    GF_EEG_ERROR_NOT_INITIALIZED,
    GF_EEG_ERROR_NULL_PTR,
    GF_EEG_ERROR_INVALID_CHANNEL,
    GF_EEG_ERROR_HARDWARE,
    GF_EEG_ERROR_BUFFER_OVERFLOW,
    GF_EEG_WARN_HIGH_IMPEDANCE,
    GF_EEG_WARN_ARTIFACT_DETECTED,
    GF_EEG_WARN_SIGNAL_SATURATION
} gf_eeg_status_t;

/**
 * @brief Sample rates
 */
typedef enum {
    GF_EEG_RATE_250HZ,
    GF_EEG_RATE_500HZ,
    GF_EEG_RATE_1KHZ,
    GF_EEG_RATE_2KHZ,
    GF_EEG_RATE_4KHZ,
    GF_EEG_RATE_8KHZ,
    GF_EEG_RATE_16KHZ
} gf_eeg_sample_rate_t;

/**
 * @brief Electrode montage
 */
typedef enum {
    GF_EEG_MONTAGE_REFERENTIAL,   /**< Common reference */
    GF_EEG_MONTAGE_BIPOLAR,       /**< Bipolar derivations */
    GF_EEG_MONTAGE_AVERAGE_REF,   /**< Average reference */
    GF_EEG_MONTAGE_LAPLACIAN      /**< Surface Laplacian */
} gf_eeg_montage_t;

/**
 * @brief Channel configuration
 */
typedef struct {
    uint8_t channel_id;           /**< Channel index */
    char label[8];                /**< Electrode label (e.g., "Fp1") */
    bool enabled;                 /**< Channel enabled */
    uint8_t gain;                 /**< Programmable gain (1-24) */
    bool dc_coupled;              /**< DC coupling enabled */
} gf_eeg_channel_config_t;

/**
 * @brief Acquisition configuration
 */
typedef struct {
    gf_eeg_sample_rate_t rate;    /**< Sample rate */
    gf_eeg_montage_t montage;     /**< Electrode montage */
    uint8_t channel_count;        /**< Active channels */
    uint8_t resolution_bits;      /**< ADC resolution */
    float highpass_hz;            /**< Hardware highpass */
    float lowpass_hz;             /**< Hardware lowpass */
    bool notch_50hz;              /**< 50Hz notch filter */
    bool notch_60hz;              /**< 60Hz notch filter */
} gf_eeg_config_t;

/**
 * @brief EEG sample frame
 */
typedef struct {
    uint32_t timestamp_us;        /**< Sample timestamp */
    uint32_t sequence;            /**< Sequence number */
    int32_t samples[GF_EEG_MAX_CHANNELS]; /**< Channel values (uV * 1000) */
    uint8_t quality[GF_EEG_MAX_CHANNELS]; /**< Signal quality (0-100) */
    bool artifact_flags[GF_EEG_MAX_CHANNELS]; /**< Artifact markers */
} gf_eeg_frame_t;

/**
 * @brief Impedance measurement
 */
typedef struct {
    uint32_t impedance_ohms[GF_EEG_MAX_CHANNELS]; /**< Channel impedances */
    bool good[GF_EEG_MAX_CHANNELS]; /**< Impedance acceptable */
} gf_eeg_impedance_t;

/**
 * @brief Data callback
 */
typedef void (*gf_eeg_data_cb_t)(const gf_eeg_frame_t* frame, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize EEG sensor
 * @return Status code
 */
gf_eeg_status_t gf_eeg_init(void);

/**
 * @brief Shutdown EEG sensor
 */
void gf_eeg_shutdown(void);

/**
 * @brief Configure acquisition
 * @param config Acquisition config
 * @return Status code
 */
gf_eeg_status_t gf_eeg_configure(const gf_eeg_config_t* config);

/**
 * @brief Configure channel
 * @param channel Channel index
 * @param config Channel config
 * @return Status code
 */
gf_eeg_status_t gf_eeg_configure_channel(uint8_t channel,
                                          const gf_eeg_channel_config_t* config);

/**
 * @brief Start acquisition
 * @return Status code
 */
gf_eeg_status_t gf_eeg_start(void);

/**
 * @brief Stop acquisition
 * @return Status code
 */
gf_eeg_status_t gf_eeg_stop(void);

/**
 * @brief Measure electrode impedances
 * @param impedance Output impedance values
 * @return Status code
 */
gf_eeg_status_t gf_eeg_measure_impedance(gf_eeg_impedance_t* impedance);

/**
 * @brief Register data callback
 * @param callback Data callback
 * @param user_data User context
 * @return Status code
 */
gf_eeg_status_t gf_eeg_register_callback(gf_eeg_data_cb_t callback,
                                          void* user_data);

/**
 * @brief Process EEG data
 * @return Status code
 */
gf_eeg_status_t gf_eeg_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EEG_SENSOR_H */
