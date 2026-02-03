/**
 * @file lora.h
 * @brief LoRa/LoRaWAN Communication Interface
 * 
 * WHAT: Long-range, low-power radio communication using LoRa modulation.
 *       Supports raw LoRa P2P mode and LoRaWAN network stack for
 *       battery-powered IoT sensors.
 * 
 * WHY: LoRaWAN is the dominant LPWAN technology for long-range IoT.
 *      Understanding spread factors, duty cycle limits, ADR, and
 *      class A/B/C devices demonstrates expertise in constrained
 *      networking valued in industrial IoT and smart city projects.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Smart Cities: Parking sensors, street lighting
 *   - Agriculture: Soil sensors, irrigation control
 *   - Utilities: Smart metering, leak detection
 *   - Logistics: Asset tracking, cold chain monitoring
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - LoRa radio configuration (SF, BW, CR)
 *   - LoRaWAN OTAA/ABP activation
 *   - Device classes (A/B/C)
 *   - Adaptive Data Rate (ADR)
 *   - Duty cycle management
 *   - MAC command handling
 */

#ifndef GF_LORA_H
#define GF_LORA_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_LORA_MAX_PAYLOAD     222     /* Max bytes per packet */
#define GF_LORA_DEV_EUI_LEN     8
#define GF_LORA_APP_EUI_LEN     8
#define GF_LORA_APP_KEY_LEN     16

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_LORA_SF7 = 7,            /* Fastest, shortest range */
    GF_LORA_SF8 = 8,
    GF_LORA_SF9 = 9,
    GF_LORA_SF10 = 10,
    GF_LORA_SF11 = 11,
    GF_LORA_SF12 = 12           /* Slowest, longest range */
} gf_lora_sf_t;

typedef enum {
    GF_LORA_BW_125 = 0,         /* 125 kHz bandwidth */
    GF_LORA_BW_250,             /* 250 kHz bandwidth */
    GF_LORA_BW_500              /* 500 kHz bandwidth */
} gf_lora_bw_t;

typedef enum {
    GF_LORAWAN_CLASS_A = 0,     /* Lowest power, uplink-initiated */
    GF_LORAWAN_CLASS_B,         /* Scheduled downlink windows */
    GF_LORAWAN_CLASS_C          /* Always listening */
} gf_lorawan_class_t;

typedef enum {
    GF_LORAWAN_JOIN_OTAA = 0,   /* Over-The-Air Activation */
    GF_LORAWAN_JOIN_ABP         /* Activation By Personalization */
} gf_lorawan_join_t;

typedef enum {
    GF_LORA_STATE_OFF = 0,
    GF_LORA_STATE_IDLE,
    GF_LORA_STATE_TX,
    GF_LORA_STATE_RX,
    GF_LORA_STATE_CAD,          /* Channel Activity Detection */
    GF_LORA_STATE_JOINING,
    GF_LORA_STATE_JOINED
} gf_lora_state_t;

/* Raw LoRa P2P configuration */
typedef struct {
    uint32_t        frequency_hz;       /* Center frequency */
    gf_lora_sf_t    spreading_factor;
    gf_lora_bw_t    bandwidth;
    uint8_t         coding_rate;        /* 5-8 (4/5 to 4/8) */
    int8_t          tx_power_dbm;
    uint16_t        preamble_len;
    bool            implicit_header;
    bool            crc_enabled;
} gf_lora_radio_config_t;

/* LoRaWAN OTAA credentials */
typedef struct {
    uint8_t         dev_eui[GF_LORA_DEV_EUI_LEN];
    uint8_t         app_eui[GF_LORA_APP_EUI_LEN];
    uint8_t         app_key[GF_LORA_APP_KEY_LEN];
} gf_lorawan_otaa_t;

/* LoRaWAN status */
typedef struct {
    gf_lora_state_t     state;
    bool                joined;
    int8_t              rssi_last;
    int8_t              snr_last;
    uint32_t            uplink_count;
    uint32_t            downlink_count;
    gf_lora_sf_t        current_sf;     /* After ADR */
    int8_t              current_power;  /* After ADR */
} gf_lorawan_status_t;

/* Callbacks */
typedef void (*gf_lora_rx_cb)(const uint8_t *data, uint16_t len, 
                               int8_t rssi, int8_t snr, void *ctx);
typedef void (*gf_lora_tx_done_cb)(bool success, void *ctx);
typedef void (*gf_lorawan_join_cb)(bool success, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize LoRa radio
 */
int gf_lora_init(const gf_lora_radio_config_t *config);

/**
 * @brief Send raw LoRa packet (P2P mode)
 */
int gf_lora_send(const uint8_t *data, uint16_t len);

/**
 * @brief Start continuous receive (P2P mode)
 */
int gf_lora_receive(void);

/**
 * @brief Initialize LoRaWAN stack
 */
int gf_lorawan_init(gf_lorawan_class_t device_class, const char *region);

/**
 * @brief Join network (OTAA)
 */
int gf_lorawan_join(gf_lorawan_join_t method, const gf_lorawan_otaa_t *creds);

/**
 * @brief Send confirmed uplink
 */
int gf_lorawan_send_confirmed(uint8_t port, const uint8_t *data, uint16_t len);

/**
 * @brief Send unconfirmed uplink
 */
int gf_lorawan_send(uint8_t port, const uint8_t *data, uint16_t len);

/**
 * @brief Get current status
 */
void gf_lorawan_get_status(gf_lorawan_status_t *status);

/**
 * @brief Enable/disable ADR
 */
void gf_lorawan_set_adr(bool enabled);

/**
 * @brief Process LoRaWAN stack (call from main loop)
 */
void gf_lorawan_process(void);

/**
 * @brief Set callbacks
 */
void gf_lora_set_callbacks(gf_lora_rx_cb rx_cb, 
                           gf_lora_tx_done_cb tx_cb,
                           void *ctx);

/**
 * @brief Get driver descriptor
 */
const void* gf_lora_get_driver(void);

#endif /* GF_LORA_H */
