/**
 * @file marine_spotlight.c
 * @brief Marine Sonar & Telemetry Spotlight Implementation
 * 
 * This spotlight demonstrates production-grade embedded marine systems:
 * - Sonar signal processing pipeline (ping → echo → distance)
 * - Pressure/depth monitoring with calibration
 * - Underwater telemetry reporting (depth, temperature, sonar data)
 * - Environmental compensation (temperature, salinity, pressure)
 * 
 * INDUSTRY RELEVANCE:
 * Marine systems firmware is deployed in:
 * - Autonomous Underwater Vehicles (AUVs) for oceanography and defense
 * - Remotely Operated Vehicles (ROVs) for subsea inspection
 * - Commercial fishing (fish finders, depth sounders)
 * - Naval sonar systems (detection, ranging, imaging)
 * - Offshore oil/gas (ROV operations, pipeline inspection)
 * - Oceanographic research (CTD profilers, acoustic positioning)
 * 
 * Architecture:
 * - Sonar transducer simulation with realistic timing
 * - Echo detection with signal processing
 * - Depth calculation with water property compensation
 * - Telemetry aggregation and reporting
 * - Message bus integration for marine events
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/* ===== Marine System Configuration ===== */

#define GF_MARINE_MAX_DEPTH_M           1000.0f
#define GF_MARINE_MAX_SONAR_RANGE_M     500.0f
#define GF_MARINE_MAX_BEAMS             16
#define GF_MARINE_ECHO_BUFFER_SIZE      256
#define GF_MARINE_TELEMETRY_QUEUE_SIZE  32
#define GF_MARINE_PRESSURE_SAMPLES      16

/* Physical constants */
#define GF_MARINE_ATM_PRESSURE_MBAR     1013.25f
#define GF_MARINE_SEAWATER_DENSITY      1025.0f     /* kg/m³ */
#define GF_MARINE_GRAVITY               9.80665f    /* m/s² */
#define GF_MARINE_SOUND_SPEED_DEFAULT   1500.0f     /* m/s in seawater */

/* ===== Status Codes ===== */

typedef enum {
    GF_MARINE_OK = 0,
    GF_MARINE_ERROR_NULL_PTR,
    GF_MARINE_ERROR_NOT_INITIALIZED,
    GF_MARINE_ERROR_INVALID_PARAM,
    GF_MARINE_ERROR_NO_ECHO,
    GF_MARINE_ERROR_TIMEOUT,
    GF_MARINE_ERROR_SENSOR_FAULT,
    GF_MARINE_ERROR_OUT_OF_RANGE,
    GF_MARINE_ERROR_CALIBRATION,
    GF_MARINE_ERROR_BUFFER_FULL,
    GF_MARINE_WARN_DEPTH_LIMIT,
    GF_MARINE_WARN_WEAK_ECHO
} gf_marine_status_t;

/* ===== Sonar Types ===== */

typedef enum {
    GF_SONAR_TYPE_SINGLE_BEAM,
    GF_SONAR_TYPE_DUAL_BEAM,
    GF_SONAR_TYPE_MULTI_BEAM,
    GF_SONAR_TYPE_SIDE_SCAN,
    GF_SONAR_TYPE_FORWARD_LOOKING
} gf_marine_sonar_type_t;

typedef enum {
    GF_WATER_SEAWATER,
    GF_WATER_FRESHWATER,
    GF_WATER_BRACKISH
} gf_marine_water_type_t;

typedef enum {
    GF_LIGHTING_SURFACE,
    GF_LIGHTING_TWILIGHT,
    GF_LIGHTING_MIDNIGHT,
    GF_LIGHTING_ABYSSAL
} gf_marine_light_zone_t;

/* ===== Data Structures ===== */

/**
 * @brief Sonar echo detection result
 */
typedef struct {
    float distance_m;           /* Distance to target */
    float confidence;           /* Detection confidence (0.0-1.0) */
    uint16_t amplitude;         /* Raw echo amplitude (ADC counts) */
    uint32_t tof_us;            /* Time of flight in microseconds */
    int16_t bearing_deg;        /* Bearing angle (for multi-beam) */
    uint32_t timestamp_ms;      /* Detection timestamp */
    bool valid;                 /* Valid detection flag */
} gf_marine_echo_t;

/**
 * @brief Multi-beam sonar scan result
 */
typedef struct {
    uint8_t beam_count;
    gf_marine_echo_t beams[GF_MARINE_MAX_BEAMS];
    float swath_width_deg;
    float min_distance_m;
    float max_distance_m;
    uint32_t timestamp_ms;
} gf_marine_scan_t;

/**
 * @brief Depth measurement with metadata
 */
typedef struct {
    float depth_m;              /* Calculated depth */
    float pressure_mbar;        /* Raw pressure reading */
    float temperature_c;        /* Water temperature */
    float rate_mps;             /* Descent/ascent rate (+ve = down) */
    uint8_t quality;            /* Measurement quality (0-100) */
    uint32_t timestamp_ms;
} gf_marine_depth_t;

/**
 * @brief Environmental parameters
 */
typedef struct {
    float temperature_c;        /* Water temperature */
    float salinity_ppt;         /* Salinity in parts per thousand */
    float density_kgm3;         /* Water density */
    float sound_speed_mps;      /* Speed of sound */
    gf_marine_water_type_t water_type;
    gf_marine_light_zone_t light_zone;
} gf_marine_environment_t;

/**
 * @brief Telemetry packet
 */
typedef struct {
    uint32_t packet_id;
    uint32_t timestamp_ms;
    gf_marine_depth_t depth;
    gf_marine_echo_t bottom_echo;
    gf_marine_environment_t environment;
    float battery_voltage;
    float internal_temp_c;
    uint8_t system_status;
} gf_marine_telemetry_t;

/**
 * @brief Sonar configuration
 */
typedef struct {
    gf_marine_sonar_type_t type;
    uint16_t frequency_khz;         /* Operating frequency */
    float max_range_m;              /* Maximum detection range */
    float min_range_m;              /* Blanking distance */
    uint8_t gain;                   /* Receiver gain (0-100) */
    uint8_t pulse_length_us;        /* Transmit pulse duration */
    uint16_t ping_interval_ms;      /* Time between pings */
    bool auto_gain;                 /* Automatic gain control */
    bool temperature_compensation;   /* SOS compensation */
} gf_marine_sonar_config_t;

/**
 * @brief Depth sensor configuration
 */
typedef struct {
    gf_marine_water_type_t water_type;
    float max_depth_m;              /* Maximum operational depth */
    float warning_depth_m;          /* Warning threshold */
    float max_descent_rate_mps;     /* Maximum descent rate */
    float max_ascent_rate_mps;      /* Maximum ascent rate */
    float salinity_ppt;             /* For density calculation */
    uint8_t sample_rate_hz;
    bool auto_calibrate;            /* Zero at surface */
} gf_marine_depth_config_t;

/**
 * @brief System configuration
 */
typedef struct {
    gf_marine_sonar_config_t sonar;
    gf_marine_depth_config_t depth;
    uint16_t telemetry_interval_ms;
    bool continuous_mode;
} gf_marine_config_t;

/**
 * @brief Echo callback for async operation
 */
typedef void (*gf_marine_echo_callback_t)(const gf_marine_echo_t* echo, void* user_data);

/**
 * @brief Depth alert callback
 */
typedef void (*gf_marine_depth_callback_t)(gf_marine_status_t alert,
                                            const gf_marine_depth_t* depth,
                                            void* user_data);

/* ===== Internal Structures ===== */

/**
 * @brief Echo buffer for signal processing
 */
typedef struct {
    uint16_t samples[GF_MARINE_ECHO_BUFFER_SIZE];
    uint16_t sample_count;
    uint32_t start_time_us;
    bool capturing;
} gf_echo_buffer_t;

/**
 * @brief Pressure sample ring buffer
 */
typedef struct {
    float samples[GF_MARINE_PRESSURE_SAMPLES];
    uint8_t head;
    uint8_t count;
} gf_pressure_ring_t;

/**
 * @brief Depth calibration data
 */
typedef struct {
    float zero_offset_mbar;         /* Atmospheric offset */
    float scale_factor;             /* Pressure scale factor */
    float temp_coefficient;         /* Temperature compensation */
    bool calibrated;
    uint32_t calibration_time;
} gf_depth_calibration_t;

/**
 * @brief Sonar state machine
 */
typedef enum {
    GF_SONAR_STATE_IDLE,
    GF_SONAR_STATE_PING,
    GF_SONAR_STATE_LISTEN,
    GF_SONAR_STATE_PROCESS
} gf_sonar_state_t;

/**
 * @brief Marine system state
 */
typedef struct {
    bool initialized;
    gf_marine_config_t config;
    
    /* Sonar state */
    gf_sonar_state_t sonar_state;
    gf_echo_buffer_t echo_buffer;
    gf_marine_echo_t last_echo;
    uint32_t last_ping_time_ms;
    uint32_t pings_sent;
    uint32_t echoes_received;
    float detection_rate;
    gf_marine_echo_callback_t echo_callback;
    void* echo_callback_data;
    
    /* Depth state */
    gf_pressure_ring_t pressure_ring;
    gf_depth_calibration_t depth_cal;
    gf_marine_depth_t last_depth;
    float min_depth_m;
    float max_depth_m;
    gf_marine_depth_callback_t depth_callback;
    void* depth_callback_data;
    
    /* Environment */
    gf_marine_environment_t environment;
    
    /* Telemetry */
    gf_marine_telemetry_t telemetry_queue[GF_MARINE_TELEMETRY_QUEUE_SIZE];
    uint8_t telem_head;
    uint8_t telem_tail;
    uint8_t telem_count;
    uint32_t telemetry_counter;
    uint32_t last_telemetry_time;
    
    /* Statistics */
    uint32_t total_samples;
    float avg_processing_time_us;
    float max_processing_time_us;
} gf_marine_state_t;

/* Global state */
static gf_marine_state_t g_marine = {0};

/* Forward declaration */
void gf_marine_shutdown(void);

/* ===== Helper Functions ===== */

/**
 * @brief Clamp value to range
 */
static inline float marine_clamp(float value, float min, float max) {
    if (value < min) return min;
    if (value > max) return max;
    return value;
}

/**
 * @brief Get current timestamp (simulated)
 */
static uint32_t marine_get_timestamp_ms(void) {
    static uint32_t timestamp = 0;
    return timestamp++;
}

/**
 * @brief Get microsecond timestamp (simulated)
 */
static uint32_t marine_get_timestamp_us(void) {
    return marine_get_timestamp_ms() * 1000;
}

/* ===== Speed of Sound Calculation ===== */

/**
 * @brief Calculate speed of sound using UNESCO equation
 * 
 * Simplified Chen & Millero equation for seawater
 * Accuracy: ±0.2 m/s for typical ocean conditions
 * 
 * @param temperature     Water temperature in Celsius
 * @param salinity        Salinity in parts per thousand (PSU)
 * @param depth           Depth in meters
 * @return                Speed of sound in m/s
 */
float gf_marine_calculate_sound_speed(float temperature, float salinity, float depth) {
    /* Chen & Millero (1977) simplified equation */
    float T = temperature;
    float S = salinity;
    float Z = depth;
    
    /* Temperature contribution */
    float c0 = 1449.2f + 4.6f * T - 0.055f * T * T + 0.00029f * T * T * T;
    
    /* Salinity contribution */
    float c_s = (1.34f - 0.01f * T) * (S - 35.0f);
    
    /* Pressure/depth contribution (approximately 1.7 m/s per 100m) */
    float c_p = 0.016f * Z;
    
    float speed = c0 + c_s + c_p;
    return marine_clamp(speed, 1400.0f, 1600.0f);
}

/**
 * @brief Calculate water density from temperature and salinity
 * 
 * Simplified UNESCO equation for seawater density
 * 
 * @param temperature     Water temperature in Celsius
 * @param salinity        Salinity in PSU
 * @return                Density in kg/m³
 */
float gf_marine_calculate_density(float temperature, float salinity) {
    /* Simplified density equation */
    float T = temperature;
    float S = salinity;
    
    /* Pure water density */
    float rho_w = 999.842594f + 6.793952e-2f * T - 9.095290e-3f * T * T;
    
    /* Salinity contribution */
    float rho_s = (0.824493f - 4.0899e-3f * T) * S;
    
    return rho_w + rho_s;
}

/* ===== Depth Calculations ===== */

/**
 * @brief Convert pressure to depth
 * 
 * Uses hydrostatic equation: P = ρgh + P_atm
 * Rearranged: h = (P - P_atm) / (ρg)
 * 
 * @param pressure_mbar   Absolute pressure in millibars
 * @param density         Water density in kg/m³
 * @return                Depth in meters
 */
float gf_marine_pressure_to_depth(float pressure_mbar, float density) {
    /* Convert mbar to Pa (1 mbar = 100 Pa) */
    float pressure_pa = pressure_mbar * 100.0f;
    float atm_pa = GF_MARINE_ATM_PRESSURE_MBAR * 100.0f;
    
    /* Gauge pressure */
    float gauge_pa = pressure_pa - atm_pa;
    
    /* Depth from hydrostatic equation */
    if (density <= 0.0f) density = GF_MARINE_SEAWATER_DENSITY;
    
    float depth = gauge_pa / (density * GF_MARINE_GRAVITY);
    return (depth > 0.0f) ? depth : 0.0f;
}

/**
 * @brief Simulate pressure reading at given depth
 * 
 * @param depth_m         Depth in meters
 * @param density         Water density in kg/m³
 * @return                Simulated pressure in mbar
 */
static float marine_simulate_pressure(float depth_m, float density) {
    /* P = ρgh + P_atm */
    float pressure_pa = density * GF_MARINE_GRAVITY * depth_m;
    float total_pa = pressure_pa + GF_MARINE_ATM_PRESSURE_MBAR * 100.0f;
    
    /* Add small noise (±0.5 mbar) */
    float noise = ((float)(rand() % 100) - 50.0f) * 0.01f;
    
    return total_pa / 100.0f + noise;
}

/**
 * @brief Add pressure sample to ring buffer
 */
static void marine_add_pressure_sample(float pressure) {
    gf_pressure_ring_t* ring = &g_marine.pressure_ring;
    
    ring->samples[ring->head] = pressure;
    ring->head = (ring->head + 1) % GF_MARINE_PRESSURE_SAMPLES;
    
    if (ring->count < GF_MARINE_PRESSURE_SAMPLES) {
        ring->count++;
    }
}

/**
 * @brief Get filtered pressure (moving average)
 */
static float marine_get_filtered_pressure(void) {
    gf_pressure_ring_t* ring = &g_marine.pressure_ring;
    
    if (ring->count == 0) return GF_MARINE_ATM_PRESSURE_MBAR;
    
    float sum = 0.0f;
    for (uint8_t i = 0; i < ring->count; i++) {
        sum += ring->samples[i];
    }
    
    return sum / (float)ring->count;
}

/**
 * @brief Calculate descent/ascent rate
 */
static float marine_calculate_rate(float current_depth, float prev_depth, uint32_t dt_ms) {
    if (dt_ms == 0) return 0.0f;
    
    float dt_s = (float)dt_ms / 1000.0f;
    return (current_depth - prev_depth) / dt_s;
}

/* ===== Sonar Signal Processing ===== */

/**
 * @brief Initialize echo buffer for capture
 */
static void marine_echo_buffer_init(void) {
    gf_echo_buffer_t* buf = &g_marine.echo_buffer;
    memset(buf->samples, 0, sizeof(buf->samples));
    buf->sample_count = 0;
    buf->start_time_us = marine_get_timestamp_us();
    buf->capturing = true;
}

/**
 * @brief Add sample to echo buffer (for real-time capture)
 * @note Reserved for hardware integration - ADC/DMA would call this
 */
static void marine_echo_buffer_add(uint16_t sample) __attribute__((unused));
static void marine_echo_buffer_add(uint16_t sample) {
    gf_echo_buffer_t* buf = &g_marine.echo_buffer;
    
    if (!buf->capturing) return;
    if (buf->sample_count >= GF_MARINE_ECHO_BUFFER_SIZE) return;
    
    buf->samples[buf->sample_count++] = sample;
}

/**
 * @brief Simulate sonar echo signal
 * 
 * Creates a realistic echo signal with:
 * - Transmit pulse at start
 * - Decay over time
 * - Echo peak at target distance
 * - Background noise
 * 
 * @param target_distance_m     Distance to target
 * @param sound_speed           Speed of sound
 * @param gain                  Receiver gain
 */
static void marine_simulate_echo_signal(float target_distance_m, 
                                          float sound_speed,
                                          uint8_t gain) {
    marine_echo_buffer_init();
    
    gf_echo_buffer_t* buf = &g_marine.echo_buffer;
    
    /* Calculate expected time of flight */
    float tof_s = (2.0f * target_distance_m) / sound_speed;
    /* 1kHz sample rate = 1000us per sample, fits 256ms max range */
    uint32_t tof_samples = (uint32_t)(tof_s * 1000.0f);
    
    if (tof_samples > GF_MARINE_ECHO_BUFFER_SIZE - 5) {
        tof_samples = GF_MARINE_ECHO_BUFFER_SIZE - 5;
    }
    if (tof_samples < 5) tof_samples = 5;  /* Minimum blanking */
    
    /* Generate signal */
    for (uint16_t i = 0; i < GF_MARINE_ECHO_BUFFER_SIZE; i++) {
        float value = 0.0f;
        
        /* Transmit pulse (first few samples) */
        if (i < 3) {
            value = 4095.0f * (1.0f - i / 3.0f);
        }
        /* Echo pulse centered at TOF */
        else if (i >= tof_samples - 2 && i <= tof_samples + 2) {
            float dist = fabsf((float)i - (float)tof_samples);
            float amplitude = 2500.0f * gain / 100.0f;
            value = amplitude * expf(-dist * 0.8f);
            
            /* Apply distance-based attenuation */
            float attenuation = 1.0f / (1.0f + target_distance_m / 100.0f);
            value *= attenuation;
        }
        
        /* Add background noise */
        float noise = (float)(rand() % 100);
        value += noise;
        
        /* Clamp to ADC range */
        if (value > 4095.0f) value = 4095.0f;
        if (value < 0.0f) value = 0.0f;
        
        buf->samples[i] = (uint16_t)value;
    }
    
    buf->sample_count = GF_MARINE_ECHO_BUFFER_SIZE;
    buf->capturing = false;
}

/**
 * @brief Find peak in echo buffer (simple threshold detection)
 * 
 * @param threshold       Minimum amplitude to consider
 * @param skip_samples    Skip initial samples (blanking)
 * @param peak_index      Output: index of peak
 * @param peak_amplitude  Output: amplitude at peak
 * @return                True if peak found
 */
static bool marine_find_echo_peak(uint16_t threshold, 
                                   uint16_t skip_samples,
                                   uint16_t* peak_index,
                                   uint16_t* peak_amplitude) {
    gf_echo_buffer_t* buf = &g_marine.echo_buffer;
    
    if (buf->sample_count == 0) return false;
    
    uint16_t max_amp = 0;
    uint16_t max_idx = 0;
    
    for (uint16_t i = skip_samples; i < buf->sample_count; i++) {
        if (buf->samples[i] > max_amp) {
            max_amp = buf->samples[i];
            max_idx = i;
        }
    }
    
    if (max_amp >= threshold) {
        *peak_index = max_idx;
        *peak_amplitude = max_amp;
        return true;
    }
    
    return false;
}

/**
 * @brief Process echo buffer to extract detection
 * 
 * @param sound_speed     Speed of sound for distance calculation
 * @param echo            Output: echo detection result
 * @return                Status
 */
static gf_marine_status_t marine_process_echo(float sound_speed, 
                                                gf_marine_echo_t* echo) {
    /* Skip first few samples to avoid transmit pulse */
    uint16_t blanking_samples = 4;
    uint16_t threshold = 200;  /* Minimum amplitude for detection */
    
    uint16_t peak_idx, peak_amp;
    bool found = marine_find_echo_peak(threshold, blanking_samples, 
                                        &peak_idx, &peak_amp);
    
    if (!found) {
        memset(echo, 0, sizeof(*echo));
        echo->valid = false;
        return GF_MARINE_ERROR_NO_ECHO;
    }
    
    /* Calculate time of flight (1kHz sample rate = 1000us per sample) */
    float sample_period_us = 1000.0f;
    float tof_us = peak_idx * sample_period_us;
    
    /* Calculate distance: d = (c * t) / 2 */
    float distance_m = (sound_speed * tof_us / 1000000.0f) / 2.0f;
    
    /* Calculate confidence based on amplitude */
    float confidence = (float)peak_amp / 4095.0f;
    if (confidence > 1.0f) confidence = 1.0f;
    
    /* Populate result */
    echo->distance_m = distance_m;
    echo->confidence = confidence;
    echo->amplitude = peak_amp;
    echo->tof_us = (uint32_t)tof_us;
    echo->timestamp_ms = marine_get_timestamp_ms();
    echo->bearing_deg = 0;  /* Single beam */
    echo->valid = true;
    
    /* Weak echo warning */
    if (confidence < 0.2f) {
        return GF_MARINE_WARN_WEAK_ECHO;
    }
    
    return GF_MARINE_OK;
}

/* ===== Depth Calibration ===== */

/**
 * @brief Zero depth sensor at surface
 */
gf_marine_status_t gf_marine_depth_zero(void) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    /* Take current pressure as atmospheric reference */
    float current_pressure = marine_get_filtered_pressure();
    
    g_marine.depth_cal.zero_offset_mbar = current_pressure - GF_MARINE_ATM_PRESSURE_MBAR;
    g_marine.depth_cal.calibrated = true;
    g_marine.depth_cal.calibration_time = marine_get_timestamp_ms();
    
    return GF_MARINE_OK;
}

/**
 * @brief Full depth calibration with known reference
 */
gf_marine_status_t gf_marine_depth_calibrate(float known_depth_m, 
                                               float measured_pressure_mbar) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    /* Calculate expected pressure at known depth */
    float expected_pressure = GF_MARINE_ATM_PRESSURE_MBAR + 
                              (g_marine.environment.density_kgm3 * 
                               GF_MARINE_GRAVITY * known_depth_m / 100.0f);
    
    /* Calculate scale factor */
    float gauge_measured = measured_pressure_mbar - GF_MARINE_ATM_PRESSURE_MBAR;
    float gauge_expected = expected_pressure - GF_MARINE_ATM_PRESSURE_MBAR;
    
    if (fabsf(gauge_measured) < 1.0f) {
        return GF_MARINE_ERROR_CALIBRATION;
    }
    
    g_marine.depth_cal.scale_factor = gauge_expected / gauge_measured;
    g_marine.depth_cal.calibrated = true;
    g_marine.depth_cal.calibration_time = marine_get_timestamp_ms();
    
    return GF_MARINE_OK;
}

/* ===== Main API Functions ===== */

/**
 * @brief Initialize marine system
 */
gf_marine_status_t gf_marine_init(const gf_marine_config_t* config) {
    if (!config) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    if (g_marine.initialized) {
        /* Shutdown existing instance */
        gf_marine_shutdown();
    }
    
    memset(&g_marine, 0, sizeof(g_marine));
    g_marine.config = *config;
    
    /* Initialize environment with defaults */
    float salinity = config->depth.salinity_ppt;
    if (salinity <= 0.0f) {
        salinity = (config->depth.water_type == GF_WATER_SEAWATER) ? 35.0f : 0.0f;
    }
    
    g_marine.environment.temperature_c = 15.0f;
    g_marine.environment.salinity_ppt = salinity;
    g_marine.environment.density_kgm3 = gf_marine_calculate_density(15.0f, salinity);
    g_marine.environment.sound_speed_mps = gf_marine_calculate_sound_speed(15.0f, salinity, 0.0f);
    g_marine.environment.water_type = config->depth.water_type;
    g_marine.environment.light_zone = GF_LIGHTING_SURFACE;
    
    /* Initialize depth calibration */
    g_marine.depth_cal.zero_offset_mbar = 0.0f;
    g_marine.depth_cal.scale_factor = 1.0f;
    g_marine.depth_cal.temp_coefficient = 0.0f;
    g_marine.depth_cal.calibrated = false;
    
    /* Initialize sonar state */
    g_marine.sonar_state = GF_SONAR_STATE_IDLE;
    
    /* Auto-zero if configured */
    if (config->depth.auto_calibrate) {
        /* Add initial atmospheric pressure sample */
        marine_add_pressure_sample(GF_MARINE_ATM_PRESSURE_MBAR);
        gf_marine_depth_zero();
    }
    
    g_marine.initialized = true;
    
    return GF_MARINE_OK;
}

/**
 * @brief Shutdown marine system
 */
void gf_marine_shutdown(void) {
    if (!g_marine.initialized) return;
    
    g_marine.initialized = false;
    g_marine.sonar_state = GF_SONAR_STATE_IDLE;
}

/**
 * @brief Update environment parameters
 */
gf_marine_status_t gf_marine_set_environment(float temperature_c, 
                                               float salinity_ppt,
                                               float current_depth_m) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    g_marine.environment.temperature_c = temperature_c;
    g_marine.environment.salinity_ppt = salinity_ppt;
    g_marine.environment.density_kgm3 = gf_marine_calculate_density(temperature_c, salinity_ppt);
    g_marine.environment.sound_speed_mps = gf_marine_calculate_sound_speed(
        temperature_c, salinity_ppt, current_depth_m);
    
    /* Update light zone based on depth */
    if (current_depth_m < 200.0f) {
        g_marine.environment.light_zone = GF_LIGHTING_SURFACE;
    } else if (current_depth_m < 1000.0f) {
        g_marine.environment.light_zone = GF_LIGHTING_TWILIGHT;
    } else if (current_depth_m < 4000.0f) {
        g_marine.environment.light_zone = GF_LIGHTING_MIDNIGHT;
    } else {
        g_marine.environment.light_zone = GF_LIGHTING_ABYSSAL;
    }
    
    return GF_MARINE_OK;
}

/**
 * @brief Trigger sonar ping and wait for echo
 */
gf_marine_status_t gf_marine_sonar_ping(float simulate_distance_m,
                                          gf_marine_echo_t* echo) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!echo) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    /* Check range */
    if (simulate_distance_m > g_marine.config.sonar.max_range_m) {
        simulate_distance_m = g_marine.config.sonar.max_range_m;
    }
    
    /* Transition to PING state */
    g_marine.sonar_state = GF_SONAR_STATE_PING;
    g_marine.last_ping_time_ms = marine_get_timestamp_ms();
    g_marine.pings_sent++;
    
    /* Simulate echo signal */
    g_marine.sonar_state = GF_SONAR_STATE_LISTEN;
    marine_simulate_echo_signal(simulate_distance_m,
                                 g_marine.environment.sound_speed_mps,
                                 g_marine.config.sonar.gain);
    
    /* Process echo */
    g_marine.sonar_state = GF_SONAR_STATE_PROCESS;
    gf_marine_status_t status = marine_process_echo(
        g_marine.environment.sound_speed_mps, echo);
    
    if (status == GF_MARINE_OK || status == GF_MARINE_WARN_WEAK_ECHO) {
        g_marine.last_echo = *echo;
        g_marine.echoes_received++;
        
        /* Update detection rate */
        g_marine.detection_rate = (float)g_marine.echoes_received / 
                                   (float)g_marine.pings_sent;
    }
    
    /* Return to idle */
    g_marine.sonar_state = GF_SONAR_STATE_IDLE;
    
    return status;
}

/**
 * @brief Perform multi-beam scan
 */
gf_marine_status_t gf_marine_sonar_scan(const float* distances_m,
                                          uint8_t beam_count,
                                          gf_marine_scan_t* scan) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!distances_m || !scan || beam_count == 0) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    if (beam_count > GF_MARINE_MAX_BEAMS) {
        beam_count = GF_MARINE_MAX_BEAMS;
    }
    
    memset(scan, 0, sizeof(*scan));
    scan->beam_count = beam_count;
    scan->timestamp_ms = marine_get_timestamp_ms();
    
    float min_dist = 9999.0f;
    float max_dist = 0.0f;
    
    /* Calculate beam angles (symmetric around 0) */
    float swath_per_beam = 120.0f / beam_count;
    scan->swath_width_deg = 120.0f;
    
    for (uint8_t i = 0; i < beam_count; i++) {
        float angle = -60.0f + (i + 0.5f) * swath_per_beam;
        
        /* Simulate echo for this beam */
        gf_marine_status_t status = gf_marine_sonar_ping(distances_m[i], 
                                                           &scan->beams[i]);
        scan->beams[i].bearing_deg = (int16_t)angle;
        
        if (status == GF_MARINE_OK && scan->beams[i].valid) {
            if (scan->beams[i].distance_m < min_dist) {
                min_dist = scan->beams[i].distance_m;
            }
            if (scan->beams[i].distance_m > max_dist) {
                max_dist = scan->beams[i].distance_m;
            }
        }
    }
    
    scan->min_distance_m = min_dist;
    scan->max_distance_m = max_dist;
    
    return GF_MARINE_OK;
}

/**
 * @brief Read depth at simulated depth
 */
gf_marine_status_t gf_marine_depth_read(float simulate_depth_m,
                                          gf_marine_depth_t* reading) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!reading) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    /* Simulate pressure at this depth */
    float pressure = marine_simulate_pressure(simulate_depth_m,
                                               g_marine.environment.density_kgm3);
    
    /* Add to ring buffer */
    marine_add_pressure_sample(pressure);
    
    /* Apply calibration - zero offset removes atmospheric baseline */
    float gauge_pressure = pressure - GF_MARINE_ATM_PRESSURE_MBAR - g_marine.depth_cal.zero_offset_mbar;
    gauge_pressure *= g_marine.depth_cal.scale_factor;
    
    /* Calculate depth from gauge pressure */
    if (g_marine.environment.density_kgm3 <= 0.0f) {
        return GF_MARINE_ERROR_SENSOR_FAULT;
    }
    
    /* depth = gauge_pressure_Pa / (rho * g) */
    float gauge_pa = gauge_pressure * 100.0f;  /* mbar to Pa */
    float depth = gauge_pa / (g_marine.environment.density_kgm3 * GF_MARINE_GRAVITY);
    if (depth < 0.0f) depth = 0.0f;
    
    /* Calculate rate */
    static uint32_t last_time_ms = 0;
    uint32_t current_time = marine_get_timestamp_ms();
    float rate = marine_calculate_rate(depth, g_marine.last_depth.depth_m,
                                        current_time - last_time_ms);
    last_time_ms = current_time;
    
    /* Populate reading */
    reading->depth_m = depth;
    reading->pressure_mbar = pressure;
    reading->temperature_c = g_marine.environment.temperature_c;
    reading->rate_mps = rate;
    reading->quality = 95;  /* Simulated high quality */
    reading->timestamp_ms = current_time;
    
    /* Update state */
    (void)g_marine.last_depth.depth_m; /* Previously tracked for rate calc */
    g_marine.last_depth = *reading;
    
    /* Track min/max */
    if (g_marine.total_samples == 0) {
        g_marine.min_depth_m = depth;
        g_marine.max_depth_m = depth;
    } else {
        if (depth < g_marine.min_depth_m) g_marine.min_depth_m = depth;
        if (depth > g_marine.max_depth_m) g_marine.max_depth_m = depth;
    }
    g_marine.total_samples++;
    
    /* Check limits */
    gf_marine_status_t status = GF_MARINE_OK;
    
    if (depth >= g_marine.config.depth.warning_depth_m) {
        status = GF_MARINE_WARN_DEPTH_LIMIT;
        
        if (g_marine.depth_callback) {
            g_marine.depth_callback(status, reading, g_marine.depth_callback_data);
        }
    }
    
    return status;
}

/**
 * @brief Get filtered (averaged) depth reading
 */
gf_marine_status_t gf_marine_depth_read_filtered(gf_marine_depth_t* reading) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!reading) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    float filtered_pressure = marine_get_filtered_pressure();
    
    /* Apply calibration - zero offset removes atmospheric baseline */
    float gauge_pressure = filtered_pressure - GF_MARINE_ATM_PRESSURE_MBAR - g_marine.depth_cal.zero_offset_mbar;
    gauge_pressure *= g_marine.depth_cal.scale_factor;
    
    /* Calculate depth from gauge pressure */
    float gauge_pa = gauge_pressure * 100.0f;
    float depth = gauge_pa / (g_marine.environment.density_kgm3 * GF_MARINE_GRAVITY);
    if (depth < 0.0f) depth = 0.0f;
    
    reading->depth_m = depth;
    reading->pressure_mbar = filtered_pressure;
    reading->temperature_c = g_marine.environment.temperature_c;
    reading->rate_mps = g_marine.last_depth.rate_mps;
    reading->quality = 98;  /* Filtered = higher quality */
    reading->timestamp_ms = marine_get_timestamp_ms();
    
    return GF_MARINE_OK;
}

/**
 * @brief Register depth alert callback
 */
gf_marine_status_t gf_marine_register_depth_callback(gf_marine_depth_callback_t callback,
                                                       void* user_data) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    g_marine.depth_callback = callback;
    g_marine.depth_callback_data = user_data;
    return GF_MARINE_OK;
}

/**
 * @brief Register echo callback for async operation
 */
gf_marine_status_t gf_marine_register_echo_callback(gf_marine_echo_callback_t callback,
                                                       void* user_data) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    g_marine.echo_callback = callback;
    g_marine.echo_callback_data = user_data;
    return GF_MARINE_OK;
}

/* ===== Telemetry ===== */

/**
 * @brief Create telemetry packet from current state
 */
gf_marine_status_t gf_marine_create_telemetry(gf_marine_telemetry_t* telemetry) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!telemetry) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    telemetry->packet_id = ++g_marine.telemetry_counter;
    telemetry->timestamp_ms = marine_get_timestamp_ms();
    telemetry->depth = g_marine.last_depth;
    telemetry->bottom_echo = g_marine.last_echo;
    telemetry->environment = g_marine.environment;
    telemetry->battery_voltage = 12.6f;  /* Simulated */
    telemetry->internal_temp_c = 25.0f;  /* Simulated */
    telemetry->system_status = 0;        /* All OK */
    
    return GF_MARINE_OK;
}

/**
 * @brief Queue telemetry for transmission
 */
gf_marine_status_t gf_marine_queue_telemetry(const gf_marine_telemetry_t* telemetry) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!telemetry) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    if (g_marine.telem_count >= GF_MARINE_TELEMETRY_QUEUE_SIZE) {
        return GF_MARINE_ERROR_BUFFER_FULL;
    }
    
    g_marine.telemetry_queue[g_marine.telem_head] = *telemetry;
    g_marine.telem_head = (g_marine.telem_head + 1) % GF_MARINE_TELEMETRY_QUEUE_SIZE;
    g_marine.telem_count++;
    
    return GF_MARINE_OK;
}

/**
 * @brief Get next telemetry packet from queue
 */
gf_marine_status_t gf_marine_get_telemetry(gf_marine_telemetry_t* telemetry) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!telemetry) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    if (g_marine.telem_count == 0) {
        return GF_MARINE_ERROR_BUFFER_FULL;  /* Empty */
    }
    
    *telemetry = g_marine.telemetry_queue[g_marine.telem_tail];
    g_marine.telem_tail = (g_marine.telem_tail + 1) % GF_MARINE_TELEMETRY_QUEUE_SIZE;
    g_marine.telem_count--;
    
    return GF_MARINE_OK;
}

/* ===== Statistics ===== */

/**
 * @brief Get sonar statistics
 */
gf_marine_status_t gf_marine_get_sonar_stats(uint32_t* pings_sent,
                                               uint32_t* echoes_received,
                                               float* detection_rate) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (pings_sent) *pings_sent = g_marine.pings_sent;
    if (echoes_received) *echoes_received = g_marine.echoes_received;
    if (detection_rate) *detection_rate = g_marine.detection_rate;
    
    return GF_MARINE_OK;
}

/**
 * @brief Get depth statistics
 */
gf_marine_status_t gf_marine_get_depth_stats(float* min_depth,
                                               float* max_depth,
                                               float* current_depth,
                                               uint32_t* sample_count) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (min_depth) *min_depth = g_marine.min_depth_m;
    if (max_depth) *max_depth = g_marine.max_depth_m;
    if (current_depth) *current_depth = g_marine.last_depth.depth_m;
    if (sample_count) *sample_count = g_marine.total_samples;
    
    return GF_MARINE_OK;
}

/**
 * @brief Get current environment
 */
gf_marine_status_t gf_marine_get_environment(gf_marine_environment_t* env) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!env) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    *env = g_marine.environment;
    return GF_MARINE_OK;
}

/**
 * @brief Get last echo
 */
gf_marine_status_t gf_marine_get_last_echo(gf_marine_echo_t* echo) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!echo) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    *echo = g_marine.last_echo;
    return GF_MARINE_OK;
}

/**
 * @brief Get last depth
 */
gf_marine_status_t gf_marine_get_last_depth(gf_marine_depth_t* depth) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    if (!depth) {
        return GF_MARINE_ERROR_NULL_PTR;
    }
    
    *depth = g_marine.last_depth;
    return GF_MARINE_OK;
}

/**
 * @brief Periodic processing (call from scheduler)
 */
gf_marine_status_t gf_marine_process(void) {
    if (!g_marine.initialized) {
        return GF_MARINE_ERROR_NOT_INITIALIZED;
    }
    
    uint32_t current_time = marine_get_timestamp_ms();
    
    /* Check if telemetry is due */
    if (current_time - g_marine.last_telemetry_time >= 
        g_marine.config.telemetry_interval_ms) {
        
        gf_marine_telemetry_t telemetry;
        if (gf_marine_create_telemetry(&telemetry) == GF_MARINE_OK) {
            gf_marine_queue_telemetry(&telemetry);
        }
        
        g_marine.last_telemetry_time = current_time;
    }
    
    return GF_MARINE_OK;
}
