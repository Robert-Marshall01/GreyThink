/**
 * @file ultrasound_sensor.h
 * @brief Medical Ultrasound Sensor Driver Interface
 *
 * INDUSTRY RELEVANCE:
 * Medical ultrasound imaging is a $8B+ market used across radiology, cardiology,
 * obstetrics, and point-of-care diagnostics. Firmware expertise in ultrasound
 * systems requires knowledge of:
 * - High-frequency transducer control (1-20 MHz)
 * - Beamforming and signal acquisition
 * - Real-time image reconstruction
 * - FDA 510(k) / IEC 60601 regulatory compliance
 *
 * This module demonstrates embedded medical device firmware skills applicable
 * to companies like GE Healthcare, Philips, Siemens Healthineers, and startups
 * building portable ultrasound devices (Butterfly Network, Clarius).
 *
 * @note This is a stub module demonstrating interface design.
 *       Production implementation requires FDA-cleared algorithms.
 */

#ifndef GF_ULTRASOUND_SENSOR_H
#define GF_ULTRASOUND_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_US_MAX_CHANNELS          128     /**< Maximum transducer elements */
#define GF_US_MAX_DEPTH_MM          300     /**< Maximum imaging depth */
#define GF_US_MAX_FRAME_RATE        60      /**< Maximum frames per second */
#define GF_US_ADC_RESOLUTION_BITS   14      /**< ADC bit depth */
#define GF_US_SAMPLE_RATE_MHZ       40      /**< RF sample rate */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Ultrasound imaging modes
 */
typedef enum {
    GF_US_MODE_BMODE,           /**< 2D brightness mode (standard imaging) */
    GF_US_MODE_MMODE,           /**< Motion mode (cardiac, fetal) */
    GF_US_MODE_DOPPLER_COLOR,   /**< Color flow Doppler */
    GF_US_MODE_DOPPLER_PW,      /**< Pulsed-wave Doppler */
    GF_US_MODE_DOPPLER_CW,      /**< Continuous-wave Doppler */
    GF_US_MODE_3D,              /**< 3D volume acquisition */
    GF_US_MODE_ELASTOGRAPHY     /**< Tissue stiffness imaging */
} gf_us_mode_t;

/**
 * @brief Transducer probe types
 */
typedef enum {
    GF_US_PROBE_LINEAR,         /**< Linear array (vascular, MSK) */
    GF_US_PROBE_CONVEX,         /**< Curvilinear (abdominal) */
    GF_US_PROBE_PHASED,         /**< Phased array (cardiac) */
    GF_US_PROBE_ENDOCAVITY,     /**< Transvaginal/transrectal */
    GF_US_PROBE_3D_4D           /**< 3D/4D mechanical wobbler */
} gf_us_probe_type_t;

/**
 * @brief Transmit/receive beamformer configuration
 */
typedef struct {
    uint8_t active_elements;        /**< Number of active transducer elements */
    uint8_t tx_focus_count;         /**< Number of transmit focal zones */
    float tx_focus_depths_mm[8];    /**< Transmit focal zone depths */
    float rx_focus_mm;              /**< Dynamic receive focus start */
    float apodization[GF_US_MAX_CHANNELS]; /**< Element weighting */
    uint16_t tx_voltage_v;          /**< Transmit pulse voltage */
    float tx_frequency_mhz;         /**< Center frequency */
    uint8_t tx_cycles;              /**< Pulse cycles (bandwidth) */
} gf_us_beamform_config_t;

/**
 * @brief Acquisition parameters
 */
typedef struct {
    gf_us_mode_t mode;              /**< Imaging mode */
    gf_us_probe_type_t probe;       /**< Probe type */
    float depth_mm;                 /**< Imaging depth */
    float sector_angle_deg;         /**< Sector angle (phased array) */
    uint16_t line_density;          /**< Scan lines per frame */
    uint8_t frame_rate;             /**< Target frame rate */
    uint8_t gain_db;                /**< Overall gain */
    uint8_t tgc_curve[8];           /**< Time-gain compensation */
    bool harmonic_imaging;          /**< Tissue harmonic mode */
    gf_us_beamform_config_t beam;   /**< Beamformer settings */
} gf_us_acquisition_t;

/**
 * @brief RF data frame from acquisition
 */
typedef struct {
    uint32_t frame_number;          /**< Monotonic frame counter */
    uint32_t timestamp_us;          /**< Acquisition timestamp */
    uint16_t num_lines;             /**< Number of scan lines */
    uint16_t samples_per_line;      /**< RF samples per line */
    int16_t* rf_data;               /**< Raw RF data buffer */
    size_t rf_data_size;            /**< Buffer size in bytes */
} gf_us_rf_frame_t;

/**
 * @brief Processed B-mode image
 */
typedef struct {
    uint32_t frame_number;          /**< Frame counter */
    uint32_t timestamp_us;          /**< Processing timestamp */
    uint16_t width;                 /**< Image width in pixels */
    uint16_t height;                /**< Image height in pixels */
    uint8_t* image_data;            /**< Grayscale pixel data */
    float pixel_spacing_mm[2];      /**< Physical pixel size [x, y] */
} gf_us_bmode_image_t;

/**
 * @brief Frame received callback
 */
typedef void (*gf_us_frame_callback_t)(const gf_us_rf_frame_t* frame, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize ultrasound sensor subsystem
 * @return 0 on success, negative error code on failure
 */
int gf_us_init(void);

/**
 * @brief Shutdown ultrasound subsystem
 */
void gf_us_deinit(void);

/**
 * @brief Detect and configure connected probe
 * @param[out] probe Detected probe type
 * @return 0 on success, -1 if no probe detected
 */
int gf_us_detect_probe(gf_us_probe_type_t* probe);

/**
 * @brief Configure acquisition parameters
 * @param config Acquisition configuration
 * @return 0 on success, negative error code on failure
 */
int gf_us_configure(const gf_us_acquisition_t* config);

/**
 * @brief Start continuous acquisition
 * @param callback Frame callback function
 * @param user_data User context pointer
 * @return 0 on success, negative error code on failure
 */
int gf_us_start_acquisition(gf_us_frame_callback_t callback, void* user_data);

/**
 * @brief Stop acquisition
 */
void gf_us_stop_acquisition(void);

/**
 * @brief Freeze/unfreeze display (continue background acquisition)
 * @param freeze True to freeze
 */
void gf_us_freeze(bool freeze);

/**
 * @brief Acquire single frame (triggered mode)
 * @param[out] frame Output frame buffer
 * @param timeout_ms Timeout in milliseconds
 * @return 0 on success, negative error code on failure
 */
int gf_us_acquire_frame(gf_us_rf_frame_t* frame, uint32_t timeout_ms);

/**
 * @brief Set gain in dB
 * @param gain_db Gain value (0-100)
 */
void gf_us_set_gain(uint8_t gain_db);

/**
 * @brief Set imaging depth
 * @param depth_mm Depth in millimeters
 */
void gf_us_set_depth(float depth_mm);

/**
 * @brief Run transducer self-test
 * @return 0 on success, negative error code on failure
 */
int gf_us_self_test(void);

/**
 * @brief Get thermal status (patient safety)
 * @param[out] surface_temp_c Transducer surface temperature
 * @param[out] mi Mechanical index (cavitation risk)
 * @param[out] ti Thermal index (heating risk)
 * @return 0 on success
 */
int gf_us_get_safety_indices(float* surface_temp_c, float* mi, float* ti);

#ifdef __cplusplus
}
#endif

#endif /* GF_ULTRASOUND_SENSOR_H */
