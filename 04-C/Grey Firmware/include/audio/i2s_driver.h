/**
 * @file i2s_driver.h
 * @brief I2S Audio Interface Driver Stub
 * 
 * INDUSTRY RELEVANCE:
 *   I2S (Inter-IC Sound) is the standard digital audio interface used in:
 *   - Consumer Electronics: Bluetooth speakers, headphones, smart displays
 *   - Automotive: Infotainment systems, hands-free calling, ADAS alerts
 *   - Industrial: PA systems, intercom, audio monitoring
 *   - Medical: Patient monitoring audio alerts, diagnostic equipment
 *   - IoT: Voice assistants, smart home speakers
 * 
 * This stub demonstrates understanding of audio peripheral integration,
 * DMA-based streaming, and sample rate/format configuration.
 */

#ifndef GF_I2S_DRIVER_H
#define GF_I2S_DRIVER_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_I2S_MODE_MASTER_TX = 0,  /* Master transmitter (speaker) */
    GF_I2S_MODE_MASTER_RX,      /* Master receiver (microphone) */
    GF_I2S_MODE_SLAVE_TX,       /* Slave transmitter */
    GF_I2S_MODE_SLAVE_RX,       /* Slave receiver */
    GF_I2S_MODE_FULL_DUPLEX     /* Simultaneous TX/RX */
} gf_i2s_mode_t;

typedef enum {
    GF_I2S_STANDARD_PHILIPS = 0,
    GF_I2S_STANDARD_MSB_JUSTIFIED,
    GF_I2S_STANDARD_LSB_JUSTIFIED,
    GF_I2S_STANDARD_PCM_SHORT,
    GF_I2S_STANDARD_PCM_LONG
} gf_i2s_standard_t;

typedef enum {
    GF_I2S_BITS_16 = 16,
    GF_I2S_BITS_24 = 24,
    GF_I2S_BITS_32 = 32
} gf_i2s_bits_t;

typedef enum {
    GF_I2S_RATE_8000   = 8000,
    GF_I2S_RATE_16000  = 16000,
    GF_I2S_RATE_22050  = 22050,
    GF_I2S_RATE_44100  = 44100,
    GF_I2S_RATE_48000  = 48000,
    GF_I2S_RATE_96000  = 96000,
    GF_I2S_RATE_192000 = 192000
} gf_i2s_rate_t;

typedef struct {
    gf_i2s_mode_t       mode;
    gf_i2s_standard_t   standard;
    gf_i2s_bits_t       bits_per_sample;
    gf_i2s_rate_t       sample_rate;
    uint8_t             channels;       /* 1=mono, 2=stereo */
    bool                use_dma;
    uint32_t            dma_buffer_size;
} gf_i2s_config_t;

typedef void (*gf_i2s_callback_t)(void *buffer, size_t samples, void *ctx);

typedef struct {
    bool        active;
    bool        tx_running;
    bool        rx_running;
    uint32_t    samples_transferred;
    uint32_t    buffer_underruns;
    uint32_t    buffer_overruns;
} gf_i2s_status_t;

/*===========================================================================*/
/* API                                                                        */
/*===========================================================================*/

/**
 * @brief Initialize I2S peripheral
 */
int gf_i2s_init(const gf_i2s_config_t *config);

/**
 * @brief Deinitialize I2S peripheral
 */
void gf_i2s_deinit(void);

/**
 * @brief Start audio transmission
 */
int gf_i2s_start_tx(void *buffer, size_t samples);

/**
 * @brief Start audio reception
 */
int gf_i2s_start_rx(void *buffer, size_t samples);

/**
 * @brief Stop audio transmission/reception
 */
void gf_i2s_stop(void);

/**
 * @brief Set TX callback (called when buffer needs refill)
 */
void gf_i2s_set_tx_callback(gf_i2s_callback_t cb, void *ctx);

/**
 * @brief Set RX callback (called when buffer is full)
 */
void gf_i2s_set_rx_callback(gf_i2s_callback_t cb, void *ctx);

/**
 * @brief Get current I2S status
 */
void gf_i2s_get_status(gf_i2s_status_t *status);

/**
 * @brief Set audio volume (0-100)
 */
void gf_i2s_set_volume(uint8_t volume);

#endif /* GF_I2S_DRIVER_H */
