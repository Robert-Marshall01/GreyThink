/**
 * @file instrument_sensor.h
 * @brief Smart Instrument Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Smart musical instruments and digital music technology are transforming:
 * - Live performance with real-time sound processing
 * - Music education through gesture and technique analysis
 * - Accessibility for musicians with disabilities
 * - Studio production with precise performance capture
 * 
 * Market includes: ROLI, Artiphon, Keith McMillen Instruments
 * Music tech market: $9B by 2027
 * 
 * STANDARDS: MIDI 2.0, OSC (Open Sound Control), MPE (MIDI Polyphonic Expression)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_INSTRUMENT_SENSOR_H
#define GF_INSTRUMENT_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define MAX_SENSOR_CHANNELS 128

/* Instrument types */
typedef enum {
    INST_KEYBOARD,
    INST_GUITAR,
    INST_DRUMS,
    INST_WIND,
    INST_STRING,
    INST_CONTROLLER,
    INST_GESTURAL
} instrument_type_t;

/* Sensor types */
typedef enum {
    SENSOR_PRESSURE,
    SENSOR_VELOCITY,
    SENSOR_POSITION,
    SENSOR_BEND,
    SENSOR_ACCELEROMETER,
    SENSOR_GYROSCOPE,
    SENSOR_PROXIMITY,
    SENSOR_BREATH
} sensor_type_t;

/* Note event */
typedef struct {
    uint8_t channel;
    uint8_t note;
    uint16_t velocity;      /**< 14-bit velocity (MIDI 2.0) */
    int16_t pitch_bend;     /**< Per-note pitch bend */
    uint16_t pressure;      /**< Aftertouch/pressure */
    uint16_t timbre;        /**< Y-axis expression (MPE) */
    uint32_t timestamp_us;
} note_event_t;

/* Gesture data */
typedef struct {
    float accel_x, accel_y, accel_z;
    float gyro_x, gyro_y, gyro_z;
    float roll, pitch, yaw;
    uint32_t timestamp_us;
} gesture_data_t;

/* Sensor configuration */
typedef struct {
    instrument_type_t instrument;
    uint8_t num_channels;
    uint32_t sample_rate_hz;
    bool mpe_enabled;
    bool gesture_enabled;
    uint8_t velocity_curve;   /**< 0=linear, 1=soft, 2=hard */
} sensor_config_t;

/**
 * @brief Initialize instrument sensor
 * @param config Sensor configuration
 * @return 0 on success
 */
int inst_sensor_init(const sensor_config_t *config);

/**
 * @brief Read note events from buffer
 * @param events Output event array
 * @param max_events Maximum events to read
 * @return Number of events read
 */
int inst_sensor_read_notes(note_event_t *events, uint16_t max_events);

/**
 * @brief Read gesture data
 * @param gesture Output gesture data
 * @return 0 on success
 */
int inst_sensor_read_gesture(gesture_data_t *gesture);

/**
 * @brief Set sensor sensitivity
 * @param sensor_type Type of sensor
 * @param sensitivity Sensitivity value (0-1)
 * @return 0 on success
 */
int inst_sensor_set_sensitivity(sensor_type_t sensor_type, float sensitivity);

/**
 * @brief Calibrate sensors
 * @param duration_ms Calibration duration
 * @return 0 on success
 */
int inst_sensor_calibrate(uint32_t duration_ms);

/**
 * @brief Get sensor health status
 * @param channel Channel to check (-1 for all)
 * @return Bitmask of healthy channels
 */
uint32_t inst_sensor_get_health(int channel);

/**
 * @brief Enable/disable MPE mode
 * @param enabled MPE enabled state
 */
void inst_sensor_set_mpe(bool enabled);

#ifdef __cplusplus
}
#endif

#endif /* GF_INSTRUMENT_SENSOR_H */
