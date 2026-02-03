/**
 * @file sonar_sensor.h
 * @brief Sonar Sensor Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Sonar (Sound Navigation and Ranging) is fundamental to marine systems including:
 * - Autonomous Underwater Vehicles (AUVs) for navigation and obstacle detection
 * - Commercial fishing for fish finding and depth mapping
 * - Naval defense for submarine detection and mine countermeasures
 * - Oceanographic research for seafloor mapping and marine life monitoring
 * - Offshore oil/gas for pipeline inspection and subsea infrastructure
 * 
 * This driver demonstrates expertise in:
 * - Acoustic signal processing in embedded systems
 * - Time-of-flight distance measurement
 * - Multi-beam sonar array handling
 * - Environmental compensation (temperature, salinity, pressure)
 * 
 * Architecture:
 * - Ping generation with configurable frequency (kHz range)
 * - Echo detection with time-of-flight calculation
 * - Distance calculation with speed-of-sound compensation
 * - Multi-beam support for wide-area scanning
 */

#ifndef GF_SONAR_SENSOR_H
#define GF_SONAR_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ===== Configuration ===== */

#define GF_SONAR_MAX_BEAMS          16      /* Maximum beam count for array sonar */
#define GF_SONAR_MAX_RANGE_M        300     /* Maximum range in meters */
#define GF_SONAR_MIN_RANGE_M        0.3f    /* Minimum range (blanking distance) */
#define GF_SONAR_DEFAULT_FREQ_KHZ   200     /* Default operating frequency */
#define GF_SONAR_SPEED_OF_SOUND     1500    /* Default speed of sound m/s in seawater */

/* ===== Status Codes ===== */

typedef enum {
    GF_SONAR_OK = 0,
    GF_SONAR_ERROR_NOT_INITIALIZED,
    GF_SONAR_ERROR_INVALID_PARAM,
    GF_SONAR_ERROR_TIMEOUT,
    GF_SONAR_ERROR_NO_ECHO,
    GF_SONAR_ERROR_HARDWARE,
    GF_SONAR_ERROR_BUSY,
    GF_SONAR_ERROR_CALIBRATION
} gf_sonar_status_t;

/* ===== Sonar Types ===== */

typedef enum {
    GF_SONAR_TYPE_SINGLE_BEAM,      /* Single transducer */
    GF_SONAR_TYPE_DUAL_BEAM,        /* Dual frequency */
    GF_SONAR_TYPE_MULTI_BEAM,       /* Multi-beam array */
    GF_SONAR_TYPE_SIDE_SCAN,        /* Side-scan imaging */
    GF_SONAR_TYPE_FORWARD_LOOKING   /* Forward-looking for navigation */
} gf_sonar_type_t;

/* ===== Data Structures ===== */

/**
 * @brief Single sonar echo return
 */
typedef struct {
    float distance_m;           /* Distance to target in meters */
    float confidence;           /* Echo confidence (0.0 - 1.0) */
    uint16_t amplitude;         /* Echo amplitude (raw ADC) */
    uint32_t tof_us;            /* Time of flight in microseconds */
    uint32_t timestamp_ms;      /* Measurement timestamp */
} gf_sonar_echo_t;

/**
 * @brief Multi-beam scan result
 */
typedef struct {
    uint8_t beam_count;         /* Number of valid beams */
    gf_sonar_echo_t beams[GF_SONAR_MAX_BEAMS];
    float beam_angles[GF_SONAR_MAX_BEAMS];  /* Beam angles in degrees */
    float swath_width_deg;      /* Total swath width */
} gf_sonar_scan_t;

/**
 * @brief Environmental parameters for speed-of-sound calculation
 */
typedef struct {
    float temperature_c;        /* Water temperature in Celsius */
    float salinity_ppt;         /* Salinity in parts per thousand */
    float depth_m;              /* Current depth in meters */
} gf_sonar_environment_t;

/**
 * @brief Sonar configuration
 */
typedef struct {
    gf_sonar_type_t type;
    uint16_t frequency_khz;         /* Operating frequency */
    float max_range_m;              /* Maximum detection range */
    float min_range_m;              /* Blanking distance */
    uint8_t gain;                   /* Receiver gain (0-100) */
    uint8_t pulse_length_us;        /* Transmit pulse length */
    uint16_t ping_interval_ms;      /* Time between pings */
    bool auto_gain;                 /* Enable automatic gain control */
    bool temperature_compensation;   /* Enable temp-based SOS adjustment */
} gf_sonar_config_t;

/**
 * @brief Echo detection callback
 */
typedef void (*gf_sonar_echo_callback_t)(const gf_sonar_echo_t* echo, void* user_data);

/* ===== API Functions ===== */

/**
 * @brief Initialize sonar sensor
 */
gf_sonar_status_t gf_sonar_init(const gf_sonar_config_t* config);

/**
 * @brief Shutdown sonar sensor
 */
void gf_sonar_shutdown(void);

/**
 * @brief Trigger a ping and wait for echo
 */
gf_sonar_status_t gf_sonar_ping(gf_sonar_echo_t* echo);

/**
 * @brief Trigger a ping (non-blocking)
 */
gf_sonar_status_t gf_sonar_ping_async(gf_sonar_echo_callback_t callback, void* user_data);

/**
 * @brief Perform multi-beam scan
 */
gf_sonar_status_t gf_sonar_scan(gf_sonar_scan_t* scan);

/**
 * @brief Update environmental parameters
 */
gf_sonar_status_t gf_sonar_set_environment(const gf_sonar_environment_t* env);

/**
 * @brief Calculate speed of sound based on environment
 */
float gf_sonar_calculate_sos(const gf_sonar_environment_t* env);

/**
 * @brief Set receiver gain
 */
gf_sonar_status_t gf_sonar_set_gain(uint8_t gain);

/**
 * @brief Get current configuration
 */
gf_sonar_status_t gf_sonar_get_config(gf_sonar_config_t* config);

/**
 * @brief Calibrate sonar (requires known distance target)
 */
gf_sonar_status_t gf_sonar_calibrate(float known_distance_m, uint8_t num_samples);

/**
 * @brief Get sonar statistics
 */
gf_sonar_status_t gf_sonar_get_stats(uint32_t* pings_sent, uint32_t* echoes_received,
                                      float* avg_distance, float* detection_rate);

#ifdef __cplusplus
}
#endif

#endif /* GF_SONAR_SENSOR_H */
