/**
 * @file pulse_oximeter.h
 * @brief Pulse Oximetry (SpO2) Interface Stub
 * 
 * INDUSTRY RELEVANCE:
 * Pulse oximetry is ubiquitous in healthcare from hospital ICUs to consumer
 * smartwatches. The COVID-19 pandemic dramatically increased demand for
 * reliable SpO2 monitoring. Firmware engineers must handle photoplethysmography
 * (PPG) signal processing, motion artifact rejection, and low-perfusion
 * conditions. This domain spans medical-grade devices ($500M+ market) to
 * consumer wearables.
 * 
 * Key challenges:
 * - Motion artifact compensation during movement
 * - Low perfusion detection and handling
 * - Ambient light interference rejection
 * - Power optimization for continuous monitoring
 * - Skin tone calibration for accuracy across populations
 */

#ifndef GF_PULSE_OXIMETER_H
#define GF_PULSE_OXIMETER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Pulse oximeter status codes */
typedef enum {
    GF_SPO2_OK = 0,
    GF_SPO2_NO_FINGER,              /* Finger not detected */
    GF_SPO2_LOW_PERFUSION,          /* Weak pulse signal */
    GF_SPO2_MOTION_DETECTED,        /* Excessive motion artifact */
    GF_SPO2_AMBIENT_LIGHT,          /* Ambient light interference */
    GF_SPO2_SENSOR_ERROR,           /* Hardware malfunction */
    GF_SPO2_CALIBRATING,            /* Initial calibration */
    GF_SPO2_OUT_OF_RANGE            /* Reading outside valid range */
} gf_spo2_status_t;

/* LED configuration for PPG sensing */
typedef enum {
    GF_LED_RED_660NM,               /* Red LED for SpO2 calculation */
    GF_LED_IR_940NM,                /* Infrared LED for SpO2 calculation */
    GF_LED_GREEN_530NM,             /* Green LED for pulse/HRV */
    GF_LED_COUNT
} gf_spo2_led_t;

/* Sensor configuration */
typedef struct {
    uint16_t sample_rate_hz;        /* ADC sample rate (50-400 Hz) */
    uint8_t led_current_ma[GF_LED_COUNT]; /* LED drive current per channel */
    uint8_t adc_resolution_bits;    /* ADC resolution (16-24 bits) */
    uint16_t averaging_samples;     /* Moving average window size */
    bool enable_motion_comp;        /* Motion artifact compensation */
    bool enable_ambient_cancel;     /* Ambient light cancellation */
} gf_spo2_config_t;

/* SpO2 and pulse measurement result */
typedef struct {
    uint8_t spo2_percent;           /* Oxygen saturation 0-100% */
    uint16_t pulse_bpm;             /* Heart rate in beats per minute */
    uint16_t perfusion_index;       /* Perfusion index (PI) x100 */
    uint8_t signal_quality;         /* Signal quality 0-100% */
    gf_spo2_status_t status;        /* Measurement status */
    uint32_t timestamp;             /* Measurement timestamp */
    float pulse_amplitude;          /* AC component amplitude */
    uint8_t confidence;             /* Measurement confidence 0-100% */
} gf_spo2_reading_t;

/* Raw PPG waveform data for advanced processing */
typedef struct {
    uint32_t red_ac;                /* Red channel AC component */
    uint32_t red_dc;                /* Red channel DC component */
    uint32_t ir_ac;                 /* IR channel AC component */
    uint32_t ir_dc;                 /* IR channel DC component */
    int16_t ambient;                /* Ambient light reading */
    uint32_t timestamp_us;          /* Microsecond timestamp */
} gf_ppg_sample_t;

/**
 * @brief Initialize pulse oximeter hardware
 * @param config Sensor configuration
 * @return Status code
 */
gf_spo2_status_t gf_spo2_init(const gf_spo2_config_t* config);

/**
 * @brief Start continuous SpO2 monitoring
 * @return Status code
 */
gf_spo2_status_t gf_spo2_start(void);

/**
 * @brief Stop monitoring and conserve power
 * @return Status code
 */
gf_spo2_status_t gf_spo2_stop(void);

/**
 * @brief Get latest SpO2 and pulse reading
 * @param reading Output structure for measurement
 * @return Status code
 */
gf_spo2_status_t gf_spo2_read(gf_spo2_reading_t* reading);

/**
 * @brief Get raw PPG sample for custom processing
 * @param sample Output structure for raw data
 * @return Status code
 */
gf_spo2_status_t gf_spo2_get_raw(gf_ppg_sample_t* sample);

/**
 * @brief Check if finger/contact is detected
 * @return true if contact detected
 */
bool gf_spo2_is_contact_detected(void);

/**
 * @brief Adjust LED current for optimal signal
 * @param led LED channel to adjust
 * @param current_ma New current in milliamps
 * @return Status code
 */
gf_spo2_status_t gf_spo2_set_led_current(gf_spo2_led_t led, uint8_t current_ma);

/**
 * @brief Perform hardware self-test
 * @return Status code (OK if passed)
 */
gf_spo2_status_t gf_spo2_self_test(void);

/**
 * @brief Shutdown and release resources
 */
void gf_spo2_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_PULSE_OXIMETER_H */
