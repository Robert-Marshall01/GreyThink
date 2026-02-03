/**
 * @file biosensor.h
 * @brief Biosensor Input Driver for ECG/PPG Medical Signals
 *
 * INDUSTRY RELEVANCE:
 * Medical wearables and patient monitoring devices require high-fidelity
 * biosignal acquisition. This module demonstrates understanding of:
 * - ECG (electrocardiogram) signal acquisition for heart monitoring
 * - PPG (photoplethysmography) for SpO2 and pulse detection
 * - Noise filtering and artifact rejection critical in clinical settings
 * - Sample rates and resolution requirements per IEC 60601 standards
 *
 * Used in: Patient monitors, wearable health devices, clinical diagnostics
 *
 * @note This is a stub demonstrating API design for medical firmware.
 */

#ifndef GF_BIOSENSOR_H
#define GF_BIOSENSOR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Biosensor Type Definitions                                                */
/*===========================================================================*/

/**
 * @brief Supported biosensor modalities
 */
typedef enum {
    GF_BIOSENSOR_ECG,           /**< Electrocardiogram (heart electrical) */
    GF_BIOSENSOR_PPG_RED,       /**< PPG red LED (SpO2) */
    GF_BIOSENSOR_PPG_IR,        /**< PPG infrared LED (SpO2) */
    GF_BIOSENSOR_PPG_GREEN,     /**< PPG green LED (pulse) */
    GF_BIOSENSOR_EDA,           /**< Electrodermal activity (stress) */
    GF_BIOSENSOR_TEMP,          /**< Skin temperature */
    GF_BIOSENSOR_ACCEL,         /**< Accelerometer (motion artifacts) */
    GF_BIOSENSOR_COUNT
} gf_biosensor_type_t;

/**
 * @brief Biosensor sample rates (Hz)
 */
typedef enum {
    GF_BIOSENSOR_RATE_25HZ   = 25,
    GF_BIOSENSOR_RATE_50HZ   = 50,
    GF_BIOSENSOR_RATE_100HZ  = 100,
    GF_BIOSENSOR_RATE_250HZ  = 250,   /**< Standard ECG rate */
    GF_BIOSENSOR_RATE_500HZ  = 500,   /**< High-resolution ECG */
    GF_BIOSENSOR_RATE_1000HZ = 1000
} gf_biosensor_rate_t;

/**
 * @brief ADC resolution for biosensor
 */
typedef enum {
    GF_BIOSENSOR_RES_12BIT = 12,
    GF_BIOSENSOR_RES_16BIT = 16,
    GF_BIOSENSOR_RES_18BIT = 18,
    GF_BIOSENSOR_RES_24BIT = 24       /**< Medical-grade resolution */
} gf_biosensor_resolution_t;

/**
 * @brief Lead configuration for ECG
 */
typedef enum {
    GF_ECG_LEAD_I,              /**< Lead I: RA to LA */
    GF_ECG_LEAD_II,             /**< Lead II: RA to LL (standard monitoring) */
    GF_ECG_LEAD_III,            /**< Lead III: LA to LL */
    GF_ECG_LEAD_AVR,            /**< Augmented voltage right */
    GF_ECG_LEAD_AVL,            /**< Augmented voltage left */
    GF_ECG_LEAD_AVF,            /**< Augmented voltage foot */
    GF_ECG_LEAD_V1,             /**< Precordial V1 */
    GF_ECG_LEAD_V2,             /**< Precordial V2 */
    GF_ECG_LEAD_V3,             /**< Precordial V3 */
    GF_ECG_LEAD_V4,             /**< Precordial V4 */
    GF_ECG_LEAD_V5,             /**< Precordial V5 */
    GF_ECG_LEAD_V6,             /**< Precordial V6 */
    GF_ECG_LEAD_COUNT
} gf_ecg_lead_t;

/**
 * @brief Signal quality indicator
 */
typedef enum {
    GF_SIGNAL_QUALITY_GOOD,     /**< Clean signal, reliable data */
    GF_SIGNAL_QUALITY_FAIR,     /**< Minor artifacts present */
    GF_SIGNAL_QUALITY_POOR,     /**< Significant noise/artifacts */
    GF_SIGNAL_QUALITY_UNUSABLE  /**< Lead-off or saturated */
} gf_signal_quality_t;

/**
 * @brief Biosensor configuration
 */
typedef struct {
    gf_biosensor_type_t type;
    gf_biosensor_rate_t sample_rate;
    gf_biosensor_resolution_t resolution;
    uint8_t gain;                       /**< Programmable gain (1-12) */
    bool dc_removal;                    /**< Enable DC offset removal */
    bool notch_filter;                  /**< Enable 50/60Hz notch filter */
    uint8_t notch_freq;                 /**< 50Hz or 60Hz */
    void (*data_ready_cb)(void* ctx);   /**< Data ready callback */
    void* callback_ctx;
} gf_biosensor_config_t;

/**
 * @brief Raw biosensor sample with metadata
 */
typedef struct {
    int32_t value;                      /**< Raw ADC value */
    uint32_t timestamp_us;              /**< Microsecond timestamp */
    gf_signal_quality_t quality;        /**< Signal quality */
    bool lead_off;                      /**< Lead-off detected */
    uint8_t channel;                    /**< Channel/lead index */
} gf_biosensor_sample_t;

/**
 * @brief Biosensor driver handle
 */
typedef struct gf_biosensor* gf_biosensor_handle_t;

/*===========================================================================*/
/* Biosensor Driver API                                                      */
/*===========================================================================*/

/**
 * @brief Initialize biosensor driver
 * @param config Configuration parameters
 * @param handle Output handle
 * @return 0 on success, negative on error
 */
int gf_biosensor_init(const gf_biosensor_config_t* config,
                      gf_biosensor_handle_t* handle);

/**
 * @brief Start continuous sampling
 * @param handle Biosensor handle
 * @return 0 on success
 */
int gf_biosensor_start(gf_biosensor_handle_t handle);

/**
 * @brief Stop sampling
 * @param handle Biosensor handle
 * @return 0 on success
 */
int gf_biosensor_stop(gf_biosensor_handle_t handle);

/**
 * @brief Read available samples
 * @param handle Biosensor handle
 * @param samples Output buffer
 * @param max_samples Buffer capacity
 * @param out_count Number of samples read
 * @return 0 on success
 */
int gf_biosensor_read(gf_biosensor_handle_t handle,
                      gf_biosensor_sample_t* samples,
                      size_t max_samples,
                      size_t* out_count);

/**
 * @brief Get current signal quality
 * @param handle Biosensor handle
 * @param quality Output quality indicator
 * @return 0 on success
 */
int gf_biosensor_get_quality(gf_biosensor_handle_t handle,
                             gf_signal_quality_t* quality);

/**
 * @brief Perform lead-off detection
 * @param handle Biosensor handle
 * @param lead_status Output: bit field of lead status
 * @return 0 on success, bits set = lead off
 */
int gf_biosensor_check_leads(gf_biosensor_handle_t handle,
                             uint16_t* lead_status);

/**
 * @brief Calibrate biosensor
 * @param handle Biosensor handle
 * @return 0 on success
 */
int gf_biosensor_calibrate(gf_biosensor_handle_t handle);

/**
 * @brief Deinitialize biosensor
 * @param handle Biosensor handle
 */
void gf_biosensor_deinit(gf_biosensor_handle_t handle);

/*===========================================================================*/
/* ECG-Specific Functions                                                    */
/*===========================================================================*/

/**
 * @brief Configure ECG lead selection
 * @param handle Biosensor handle configured for ECG
 * @param lead Lead to select
 * @return 0 on success
 */
int gf_ecg_set_lead(gf_biosensor_handle_t handle, gf_ecg_lead_t lead);

/**
 * @brief Enable right-leg drive for noise reduction
 * @param handle Biosensor handle
 * @param enable Enable/disable RLD
 * @return 0 on success
 */
int gf_ecg_set_rld(gf_biosensor_handle_t handle, bool enable);

/*===========================================================================*/
/* PPG-Specific Functions                                                    */
/*===========================================================================*/

/**
 * @brief Set PPG LED current
 * @param handle Biosensor handle configured for PPG
 * @param current_ma LED current in milliamps (0-50)
 * @return 0 on success
 */
int gf_ppg_set_led_current(gf_biosensor_handle_t handle, uint8_t current_ma);

/**
 * @brief Set PPG integration time
 * @param handle Biosensor handle
 * @param time_us Integration time in microseconds
 * @return 0 on success
 */
int gf_ppg_set_integration_time(gf_biosensor_handle_t handle, uint16_t time_us);

#ifdef __cplusplus
}
#endif

#endif /* GF_BIOSENSOR_H */
