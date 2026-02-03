/**
 * @file encoder.h
 * @brief Quadrature Encoder Interface
 * 
 * WHAT: Hardware-accelerated quadrature encoder decoder for position
 *       and velocity measurement in motion control applications.
 * 
 * WHY: Encoders provide essential feedback for closed-loop motor control.
 *      Understanding quadrature decoding, index pulse handling, and
 *      velocity estimation from position data demonstrates motion control
 *      expertise valued in robotics and industrial automation.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Robotics: Joint position sensing, wheel odometry
 *   - Industrial: CNC axis position, conveyor tracking
 *   - Automotive: Steering angle, throttle position
 *   - Consumer: Scroll wheels, volume knobs
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Quadrature decoding (A/B/Z channels)
 *   - Hardware timer counter mode
 *   - Velocity calculation from position delta
 *   - Index pulse synchronization
 *   - Overflow/underflow handling for multi-turn
 */

#ifndef GF_ENCODER_H
#define GF_ENCODER_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_ENCODER_MAX_CHANNELS     4       /* Maximum encoder inputs */
#define GF_ENCODER_SAMPLE_RATE_HZ   1000    /* Velocity sample rate */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_ENC_MODE_X1 = 0,             /* Count on A rising only (1x) */
    GF_ENC_MODE_X2,                 /* Count on A edges (2x) */
    GF_ENC_MODE_X4                  /* Count on A and B edges (4x) */
} gf_encoder_mode_t;

typedef struct {
    gf_encoder_mode_t   mode;
    uint32_t            counts_per_rev;     /* Counts per revolution */
    bool                invert_direction;   /* Swap CW/CCW */
    bool                enable_index;       /* Use Z (index) channel */
    uint32_t            filter_ns;          /* Input glitch filter */
} gf_encoder_config_t;

typedef struct {
    int32_t             position;           /* Current position (counts) */
    int32_t             velocity;           /* Velocity (counts/sec) */
    uint32_t            revolutions;        /* Complete revolutions */
    bool                index_detected;     /* Index pulse seen */
    uint32_t            error_count;        /* Decoding errors */
} gf_encoder_status_t;

/* Callback for index pulse detection */
typedef void (*gf_encoder_index_cb)(uint8_t channel, int32_t position, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize encoder channel
 */
int gf_encoder_init(uint8_t channel, const gf_encoder_config_t *config);

/**
 * @brief Get current position (counts)
 */
int32_t gf_encoder_get_position(uint8_t channel);

/**
 * @brief Get current velocity (counts/second)
 */
int32_t gf_encoder_get_velocity(uint8_t channel);

/**
 * @brief Get full status
 */
void gf_encoder_get_status(uint8_t channel, gf_encoder_status_t *status);

/**
 * @brief Reset position to zero
 */
int gf_encoder_reset(uint8_t channel);

/**
 * @brief Set position offset
 */
int gf_encoder_set_position(uint8_t channel, int32_t position);

/**
 * @brief Register index pulse callback
 */
void gf_encoder_set_index_callback(gf_encoder_index_cb callback, void *ctx);

/**
 * @brief Convert counts to angle (degrees * 100)
 */
int32_t gf_encoder_to_angle(uint8_t channel, int32_t counts);

/**
 * @brief Get driver descriptor
 */
const void* gf_encoder_get_driver(void);

#endif /* GF_ENCODER_H */
