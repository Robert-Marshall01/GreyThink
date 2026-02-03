/**
 * @file haptic.h
 * @brief Haptic Feedback Driver
 * 
 * INDUSTRY RELEVANCE:
 * Haptic feedback is essential for wearables, gaming controllers, smartphones,
 * and automotive touch interfaces. Modern haptic drivers use Linear Resonant
 * Actuators (LRAs) or Eccentric Rotating Mass (ERM) motors with sophisticated
 * waveform generation. Apple's Taptic Engine and gaming controllers demonstrate
 * industry-leading haptic experiences.
 * 
 * WHY THIS MATTERS:
 * - LRA vs ERM actuator support
 * - Waveform generation and playback
 * - Pre-defined effects library
 * - Auto-resonance frequency detection
 * - Power management for battery life
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - PWM waveform generation
 * - Haptic effect sequencing
 * - Actuator calibration
 * - Power-efficient feedback patterns
 */

#ifndef GF_HAPTIC_H
#define GF_HAPTIC_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_HAPTIC_MAX_EFFECTS       32      /* Maximum custom effects */
#define GF_HAPTIC_MAX_SEQUENCE      8       /* Effects per sequence */
#define GF_HAPTIC_WAVEFORM_LEN      128     /* Max waveform points */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Actuator type */
typedef enum {
    GF_HAPTIC_LRA = 0,              /* Linear Resonant Actuator */
    GF_HAPTIC_ERM                   /* Eccentric Rotating Mass */
} gf_haptic_actuator_t;

/* Haptic driver state */
typedef enum {
    GF_HAPTIC_STATE_OFF = 0,        /* Driver powered off */
    GF_HAPTIC_STATE_IDLE,           /* Ready for playback */
    GF_HAPTIC_STATE_PLAYING,        /* Effect playing */
    GF_HAPTIC_STATE_CALIBRATING     /* Auto-calibration */
} gf_haptic_state_t;

/* Built-in effect library */
typedef enum {
    /* Basic effects */
    GF_HAPTIC_EFFECT_CLICK = 0,     /* Short click (button press) */
    GF_HAPTIC_EFFECT_DOUBLE_CLICK,  /* Double click */
    GF_HAPTIC_EFFECT_TICK,          /* Soft tick (scroll) */
    GF_HAPTIC_EFFECT_THUD,          /* Heavy thud */
    GF_HAPTIC_EFFECT_POP,           /* Pop sensation */
    
    /* Notification effects */
    GF_HAPTIC_EFFECT_ALERT,         /* Alert vibration */
    GF_HAPTIC_EFFECT_ALARM,         /* Alarm pattern */
    GF_HAPTIC_EFFECT_NOTIFICATION,  /* Notification buzz */
    GF_HAPTIC_EFFECT_RINGTONE,      /* Incoming call */
    GF_HAPTIC_EFFECT_SUCCESS,       /* Success confirmation */
    GF_HAPTIC_EFFECT_FAILURE,       /* Failure/error */
    GF_HAPTIC_EFFECT_WARNING,       /* Warning alert */
    
    /* Texture effects */
    GF_HAPTIC_EFFECT_ROUGH,         /* Rough texture */
    GF_HAPTIC_EFFECT_SMOOTH,        /* Smooth texture */
    GF_HAPTIC_EFFECT_BUMP,          /* Speed bump */
    GF_HAPTIC_EFFECT_RAMP_UP,       /* Increasing intensity */
    GF_HAPTIC_EFFECT_RAMP_DOWN,     /* Decreasing intensity */
    
    /* Game effects */
    GF_HAPTIC_EFFECT_EXPLOSION,     /* Explosion vibration */
    GF_HAPTIC_EFFECT_GUNSHOT,       /* Short impulse */
    GF_HAPTIC_EFFECT_ENGINE,        /* Engine rumble */
    GF_HAPTIC_EFFECT_HEARTBEAT,     /* Heartbeat pattern */
    
    /* Custom starts here */
    GF_HAPTIC_EFFECT_CUSTOM_BASE = 64
} gf_haptic_effect_t;

/* Waveform type */
typedef enum {
    GF_HAPTIC_WAVE_SINE = 0,        /* Sinusoidal */
    GF_HAPTIC_WAVE_SQUARE,          /* Square wave */
    GF_HAPTIC_WAVE_TRIANGLE,        /* Triangle wave */
    GF_HAPTIC_WAVE_CLICK,           /* Click impulse */
    GF_HAPTIC_WAVE_CUSTOM           /* Custom waveform data */
} gf_haptic_waveform_t;

/* Effect definition */
typedef struct {
    uint8_t             id;             /* Effect ID */
    gf_haptic_waveform_t waveform;      /* Wave type */
    uint16_t            duration_ms;    /* Effect duration */
    uint8_t             amplitude;      /* Intensity (0-255) */
    uint16_t            frequency_hz;   /* Drive frequency (for LRA) */
    uint8_t             attack_ms;      /* Attack time */
    uint8_t             release_ms;     /* Release time */
    int8_t              custom_wave[GF_HAPTIC_WAVEFORM_LEN]; /* Custom data */
    uint8_t             custom_len;     /* Custom waveform length */
} gf_haptic_effect_def_t;

/* Effect sequence */
typedef struct {
    uint8_t     effects[GF_HAPTIC_MAX_SEQUENCE];    /* Effect IDs */
    uint16_t    delays_ms[GF_HAPTIC_MAX_SEQUENCE];  /* Delay after each */
    uint8_t     count;                              /* Number of effects */
    uint8_t     repeat;                             /* Repeat count (0=once) */
} gf_haptic_sequence_t;

/* Driver configuration */
typedef struct {
    gf_haptic_actuator_t actuator;      /* Actuator type */
    uint16_t    resonant_freq_hz;       /* LRA resonant frequency */
    uint8_t     overdrive_percent;      /* Initial overdrive */
    uint8_t     brake_percent;          /* Braking strength */
    uint8_t     max_amplitude;          /* Maximum intensity limit */
    bool        auto_calibrate;         /* Enable auto-calibration */
} gf_haptic_config_t;

/* Playback callback */
typedef void (*gf_haptic_callback_t)(gf_haptic_effect_t effect,
                                     gf_haptic_state_t state, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize haptic driver
 * @param config Driver configuration (NULL for defaults)
 */
int gf_haptic_init(const gf_haptic_config_t *config);

/**
 * @brief Get driver state
 */
gf_haptic_state_t gf_haptic_get_state(void);

/**
 * @brief Play a built-in or custom effect
 * @param effect Effect ID to play
 */
int gf_haptic_play(gf_haptic_effect_t effect);

/**
 * @brief Play effect with custom amplitude
 * @param effect Effect ID
 * @param amplitude Override amplitude (0-255)
 */
int gf_haptic_play_with_amplitude(gf_haptic_effect_t effect, uint8_t amplitude);

/**
 * @brief Play effect sequence
 * @param sequence Sequence to play
 */
int gf_haptic_play_sequence(const gf_haptic_sequence_t *sequence);

/**
 * @brief Stop any playing effect
 */
int gf_haptic_stop(void);

/**
 * @brief Create custom effect
 * @param def Effect definition
 * @return Effect ID or error
 */
int gf_haptic_create_effect(const gf_haptic_effect_def_t *def);

/**
 * @brief Delete custom effect
 */
int gf_haptic_delete_effect(uint8_t id);

/**
 * @brief Set global amplitude scaling
 * @param scale Amplitude scale (0.0-1.0, 1.0 = full)
 */
int gf_haptic_set_amplitude_scale(float scale);

/**
 * @brief Enable/disable haptic feedback globally
 */
int gf_haptic_enable(bool enable);

/**
 * @brief Check if haptic is enabled
 */
bool gf_haptic_is_enabled(void);

/**
 * @brief Run auto-calibration for LRA
 * @return Detected resonant frequency in Hz
 */
int gf_haptic_calibrate(void);

/**
 * @brief Set resonant frequency for LRA
 */
int gf_haptic_set_resonant_freq(uint16_t freq_hz);

/**
 * @brief Get resonant frequency
 */
uint16_t gf_haptic_get_resonant_freq(void);

/**
 * @brief Register playback callback
 */
int gf_haptic_register_callback(gf_haptic_callback_t callback, void *ctx);

/**
 * @brief Process haptic driver (call from main loop or timer)
 */
int gf_haptic_process(void);

/**
 * @brief Play simple vibration
 * @param duration_ms Duration in milliseconds
 * @param amplitude Intensity (0-255)
 */
int gf_haptic_vibrate(uint16_t duration_ms, uint8_t amplitude);

/**
 * @brief Play buzzer pattern (for wearable notifications)
 * @param pattern Bit pattern (1=on, 0=off, LSB first)
 * @param period_ms Duration of each bit
 * @param repeat Number of repeats
 */
int gf_haptic_pattern(uint16_t pattern, uint16_t period_ms, uint8_t repeat);

/**
 * @brief Get power consumption
 * @return Current draw in milliamps when active
 */
uint16_t gf_haptic_get_power_ma(void);

#endif /* GF_HAPTIC_H */
