/**
 * @file seismic_sensor.h
 * @brief Seismic Sensor Interface for Disaster Prediction
 * 
 * INDUSTRY RELEVANCE:
 * Early earthquake warning systems save lives by providing seconds to minutes
 * of warning before destructive shaking arrives. Organizations like USGS,
 * JMA (Japan), and private companies like SkyAlert and Grillo build seismic
 * sensor networks requiring specialized firmware for high-precision, low-latency
 * data acquisition and real-time processing.
 * 
 * This module provides interfaces for MEMS accelerometers and force-balance
 * seismometers used in earthquake early warning (EEW) systems.
 * 
 * KEY CAPABILITIES:
 * - High-resolution acceleration measurement
 * - Multi-axis seismic sensing (3-component)
 * - Trigger detection (STA/LTA algorithm)
 * - P-wave and S-wave discrimination
 * - Magnitude estimation
 * - Network timing synchronization
 * - False alarm rejection
 * - Continuous data streaming
 * 
 * STANDARDS COMPLIANCE:
 * - SEED (Standard for Exchange of Earthquake Data)
 * - FDSN (Federation of Digital Seismograph Networks)
 * - ShakeAlert specifications
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_SEISMIC_SENSOR_H
#define GF_SEISMIC_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define SS_SAMPLE_RATE_HZ      200   /**< Samples per second */
#define SS_BUFFER_SECONDS      30    /**< Ring buffer duration */
#define SS_MAX_CHANNELS        3     /**< 3-component sensor */
#define SS_TRIGGER_STA_S       0.5f  /**< Short-term average window */
#define SS_TRIGGER_LTA_S       30.0f /**< Long-term average window */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Sensor type */
typedef enum {
    SS_TYPE_MEMS,          /**< MEMS accelerometer */
    SS_TYPE_FORCE_BALANCE, /**< Force-balance accelerometer */
    SS_TYPE_BROADBAND,     /**< Broadband seismometer */
    SS_TYPE_SHORT_PERIOD   /**< Short-period seismometer */
} ss_sensor_type_t;

/** Channel axis */
typedef enum {
    SS_AXIS_Z,             /**< Vertical */
    SS_AXIS_N,             /**< North-South */
    SS_AXIS_E              /**< East-West */
} ss_axis_t;

/** Event phase */
typedef enum {
    SS_PHASE_NONE,
    SS_PHASE_P_WAVE,       /**< Primary (compressional) */
    SS_PHASE_S_WAVE,       /**< Secondary (shear) */
    SS_PHASE_SURFACE       /**< Surface waves */
} ss_phase_t;

/** Seismic sample */
typedef struct {
    int32_t z_counts;      /**< Vertical component */
    int32_t n_counts;      /**< North component */
    int32_t e_counts;      /**< East component */
    uint32_t timestamp_us; /**< Microsecond timestamp */
    uint8_t quality;       /**< Data quality flags */
} ss_sample_t;

/** Detection result */
typedef struct {
    bool triggered;
    ss_phase_t phase;
    float peak_acceleration_g;
    float magnitude_estimate;
    uint32_t trigger_time_us;
    float sta_lta_ratio;
} ss_detection_t;

/** Sensor configuration */
typedef struct {
    ss_sensor_type_t type;
    float sensitivity_v_g;
    float full_scale_g;
    uint16_t sample_rate_hz;
    float trigger_threshold;
} ss_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize seismic sensor
 * @param config Sensor configuration
 * @return 0 on success
 */
int ss_init(const ss_config_t* config);

/**
 * @brief Read current sample
 * @param sample Output sample
 * @return 0 on success
 */
int ss_read_sample(ss_sample_t* sample);

/**
 * @brief Get peak ground acceleration
 * @param pga_g Output PGA in g
 * @return 0 on success
 */
int ss_get_pga(float* pga_g);

/**
 * @brief Check for trigger
 * @param detection Output detection result
 * @return 0 on success
 */
int ss_check_trigger(ss_detection_t* detection);

/**
 * @brief Get waveform buffer
 * @param samples Output sample array
 * @param max_samples Maximum samples
 * @param count Output sample count
 * @return 0 on success
 */
int ss_get_waveform(ss_sample_t* samples, uint32_t max_samples, 
                    uint32_t* count);

/**
 * @brief Send heartbeat (network health)
 * @return 0 on success
 */
int ss_send_heartbeat(void);

/**
 * @brief Shutdown sensor
 */
void ss_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_SEISMIC_SENSOR_H */
