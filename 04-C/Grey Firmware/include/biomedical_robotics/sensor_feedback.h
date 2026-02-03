/**
 * @file sensor_feedback.h
 * @brief Sensor Feedback Loop Module for Biomedical Robotics
 * 
 * INDUSTRY RELEVANCE:
 * Prosthetics and surgical robots require closed-loop feedback:
 * - Force/torque sensing for grip modulation
 * - Position feedback for precise movements
 * - EMG signal processing for intent detection
 * - Temperature sensing for safety
 * - Vibrotactile feedback to user
 * 
 * Real-time constraints: <5ms sensor-to-actuator latency
 * Companies: Intuitive Surgical, Medtronic, Stryker
 */

#ifndef GF_SENSOR_FEEDBACK_H
#define GF_SENSOR_FEEDBACK_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_FEEDBACK_MAX_SENSORS     16
#define GF_FEEDBACK_SAMPLE_RATE_HZ  2000
#define GF_FEEDBACK_FILTER_ORDER    4
#define GF_EMG_CHANNELS             8

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_FEEDBACK_OK = 0,
    GF_FEEDBACK_ERROR_NULL_PTR,
    GF_FEEDBACK_ERROR_NOT_INITIALIZED,
    GF_FEEDBACK_ERROR_SENSOR_FAULT,
    GF_FEEDBACK_ERROR_OVERFLOW,
    GF_FEEDBACK_ERROR_CALIBRATION,
    GF_FEEDBACK_ERROR_TIMEOUT,
    GF_FEEDBACK_WARN_NOISE_HIGH,
    GF_FEEDBACK_WARN_SIGNAL_WEAK
} gf_feedback_status_t;

typedef enum {
    GF_SENSOR_FORCE,            /* Force/load cell */
    GF_SENSOR_POSITION,         /* Encoder/potentiometer */
    GF_SENSOR_TEMPERATURE,      /* Thermistor */
    GF_SENSOR_EMG,              /* Electromyography */
    GF_SENSOR_IMU,              /* Inertial measurement */
    GF_SENSOR_PRESSURE,         /* Contact pressure */
    GF_SENSOR_STRAIN            /* Strain gauge */
} gf_sensor_type_t;

typedef enum {
    GF_FILTER_NONE,
    GF_FILTER_LOWPASS,
    GF_FILTER_HIGHPASS,
    GF_FILTER_BANDPASS,
    GF_FILTER_NOTCH,            /* 50/60Hz rejection */
    GF_FILTER_MEDIAN,
    GF_FILTER_KALMAN
} gf_filter_type_t;

/**
 * @brief Sensor configuration
 */
typedef struct {
    uint8_t sensor_id;
    gf_sensor_type_t type;
    uint16_t sample_rate_hz;
    float scale_factor;
    float offset;
    float min_value;
    float max_value;
    gf_filter_type_t filter;
    float filter_cutoff_hz;
} gf_sensor_config_t;

/**
 * @brief Force sensor reading
 */
typedef struct {
    float force_x_n;
    float force_y_n;
    float force_z_n;
    float torque_x_nm;
    float torque_y_nm;
    float torque_z_nm;
} gf_force_reading_t;

/**
 * @brief EMG signal data
 */
typedef struct {
    float raw_mv[GF_EMG_CHANNELS];
    float filtered_mv[GF_EMG_CHANNELS];
    float rms_mv[GF_EMG_CHANNELS];
    float envelope[GF_EMG_CHANNELS];
    uint8_t active_channels;
} gf_emg_reading_t;

/**
 * @brief IMU reading
 */
typedef struct {
    float accel_x_g;
    float accel_y_g;
    float accel_z_g;
    float gyro_x_dps;
    float gyro_y_dps;
    float gyro_z_dps;
    float roll_deg;
    float pitch_deg;
    float yaw_deg;
} gf_imu_reading_t;

/**
 * @brief Feedback controller configuration
 */
typedef struct {
    float kp;                   /* Proportional gain */
    float ki;                   /* Integral gain */
    float kd;                   /* Derivative gain */
    float setpoint;
    float min_output;
    float max_output;
    float deadband;
    bool anti_windup;
} gf_feedback_controller_t;

/**
 * @brief Haptic feedback command
 */
typedef struct {
    uint8_t actuator_id;
    float intensity_pct;        /* 0-100% */
    uint16_t frequency_hz;
    uint16_t duration_ms;
    uint8_t pattern;            /* Pulse pattern */
} gf_haptic_cmd_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_sensor_data_cb_t)(uint8_t sensor_id,
                                     gf_sensor_type_t type,
                                     float value,
                                     void* user_data);

typedef void (*gf_emg_intent_cb_t)(uint8_t gesture_id,
                                    float confidence,
                                    void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_feedback_status_t gf_feedback_init(void);
void gf_feedback_shutdown(void);

/* Sensor management */
gf_feedback_status_t gf_feedback_add_sensor(const gf_sensor_config_t* config);
gf_feedback_status_t gf_feedback_calibrate_sensor(uint8_t sensor_id);
gf_feedback_status_t gf_feedback_enable_sensor(uint8_t sensor_id, bool enable);

/* Data acquisition */
gf_feedback_status_t gf_feedback_read_force(uint8_t sensor_id, gf_force_reading_t* reading);
gf_feedback_status_t gf_feedback_read_emg(gf_emg_reading_t* reading);
gf_feedback_status_t gf_feedback_read_imu(uint8_t sensor_id, gf_imu_reading_t* reading);
gf_feedback_status_t gf_feedback_read_analog(uint8_t sensor_id, float* value);

/* Feedback control */
gf_feedback_status_t gf_feedback_setup_controller(uint8_t sensor_id,
                                                   const gf_feedback_controller_t* ctrl);
gf_feedback_status_t gf_feedback_update_setpoint(uint8_t sensor_id, float setpoint);
gf_feedback_status_t gf_feedback_get_output(uint8_t sensor_id, float* output);

/* Haptic feedback */
gf_feedback_status_t gf_feedback_haptic_pulse(const gf_haptic_cmd_t* cmd);
gf_feedback_status_t gf_feedback_haptic_stop(uint8_t actuator_id);

/* Callbacks */
gf_feedback_status_t gf_feedback_register_sensor_callback(gf_sensor_data_cb_t cb,
                                                           void* user_data);
gf_feedback_status_t gf_feedback_register_intent_callback(gf_emg_intent_cb_t cb,
                                                           void* user_data);

/* Periodic processing */
gf_feedback_status_t gf_feedback_process(void);

#endif /* GF_SENSOR_FEEDBACK_H */
