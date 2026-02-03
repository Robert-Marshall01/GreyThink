/**
 * @file rfid_scanner.h
 * @brief RFID Scanner Interface for Smart Retail
 * 
 * INDUSTRY RELEVANCE:
 * RFID in retail enables:
 * - Automated inventory tracking
 * - Self-checkout systems
 * - Anti-theft (EAS)
 * - Supply chain visibility
 * - Smart shelving
 * 
 * Protocols: EPC Gen2, ISO 18000-6C, NFC
 * Companies: Zebra, Impinj, Avery Dennison, Checkpoint
 */

#ifndef GF_RFID_SCANNER_H
#define GF_RFID_SCANNER_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_RFID_MAX_TAGS            256
#define GF_RFID_EPC_SIZE            12      /* 96-bit EPC */
#define GF_RFID_TID_SIZE            12
#define GF_RFID_USER_DATA_SIZE      32

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_RFID_OK = 0,
    GF_RFID_ERROR_NULL_PTR,
    GF_RFID_ERROR_NOT_INITIALIZED,
    GF_RFID_ERROR_NO_TAG,
    GF_RFID_ERROR_COLLISION,
    GF_RFID_ERROR_READ_FAIL,
    GF_RFID_ERROR_WRITE_FAIL,
    GF_RFID_ERROR_LOCK_FAIL,
    GF_RFID_ERROR_ANTENNA_FAULT,
    GF_RFID_ERROR_POWER_TOO_LOW,
    GF_RFID_WARN_WEAK_SIGNAL
} gf_rfid_status_t;

typedef enum {
    GF_RFID_PROTO_EPC_GEN2,     /* UHF RFID */
    GF_RFID_PROTO_ISO14443A,    /* NFC Type A */
    GF_RFID_PROTO_ISO14443B,    /* NFC Type B */
    GF_RFID_PROTO_ISO15693,     /* HF RFID */
    GF_RFID_PROTO_ISO18000_6C   /* UHF standard */
} gf_rfid_protocol_t;

typedef enum {
    GF_RFID_FREQ_LF,            /* 125-134 kHz */
    GF_RFID_FREQ_HF,            /* 13.56 MHz */
    GF_RFID_FREQ_UHF,           /* 860-960 MHz */
    GF_RFID_FREQ_MICROWAVE      /* 2.45 GHz */
} gf_rfid_frequency_t;

typedef enum {
    GF_RFID_BANK_RESERVED,
    GF_RFID_BANK_EPC,
    GF_RFID_BANK_TID,
    GF_RFID_BANK_USER
} gf_rfid_memory_bank_t;

/**
 * @brief Scanner configuration
 */
typedef struct {
    gf_rfid_protocol_t protocol;
    gf_rfid_frequency_t frequency;
    float tx_power_dbm;
    uint8_t antenna_port;
    uint16_t scan_rate_hz;
    uint16_t session;           /* Gen2 session */
    bool continuous_mode;
} gf_rfid_config_t;

/**
 * @brief Tag data
 */
typedef struct {
    uint8_t epc[GF_RFID_EPC_SIZE];
    uint8_t epc_len;
    uint8_t tid[GF_RFID_TID_SIZE];
    uint8_t tid_len;
    uint8_t user_data[GF_RFID_USER_DATA_SIZE];
    uint8_t user_data_len;
    float rssi_dbm;
    uint16_t read_count;
    uint64_t first_seen_ms;
    uint64_t last_seen_ms;
    bool locked;
} gf_rfid_tag_t;

/**
 * @brief Inventory result
 */
typedef struct {
    uint32_t total_tags;
    uint32_t new_tags;
    uint32_t lost_tags;
    uint16_t scan_time_ms;
    float avg_rssi_dbm;
} gf_rfid_inventory_t;

/**
 * @brief Write command
 */
typedef struct {
    uint8_t epc[GF_RFID_EPC_SIZE];
    gf_rfid_memory_bank_t bank;
    uint16_t word_offset;
    uint8_t* data;
    uint8_t data_len;
    uint32_t access_password;
} gf_rfid_write_cmd_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_rfid_tag_cb_t)(const gf_rfid_tag_t* tag, bool is_new, void* user_data);

typedef void (*gf_rfid_inventory_cb_t)(const gf_rfid_inventory_t* inv, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_rfid_status_t gf_rfid_init(const gf_rfid_config_t* config);
void gf_rfid_shutdown(void);

/* Control */
gf_rfid_status_t gf_rfid_enable(bool enable);
gf_rfid_status_t gf_rfid_set_power(float tx_power_dbm);
gf_rfid_status_t gf_rfid_select_antenna(uint8_t port);

/* Scanning */
gf_rfid_status_t gf_rfid_start_inventory(void);
gf_rfid_status_t gf_rfid_stop_inventory(void);
gf_rfid_status_t gf_rfid_single_read(gf_rfid_tag_t* tag);
gf_rfid_status_t gf_rfid_get_tag_count(uint32_t* count);
gf_rfid_status_t gf_rfid_get_tag(uint32_t index, gf_rfid_tag_t* tag);
gf_rfid_status_t gf_rfid_clear_tags(void);

/* Read/Write operations */
gf_rfid_status_t gf_rfid_read_bank(const uint8_t* epc,
                                    gf_rfid_memory_bank_t bank,
                                    uint16_t word_offset,
                                    uint8_t word_count,
                                    uint8_t* data);
gf_rfid_status_t gf_rfid_write_bank(const gf_rfid_write_cmd_t* cmd);
gf_rfid_status_t gf_rfid_lock_tag(const uint8_t* epc, uint32_t lock_mask);

/* Callbacks */
gf_rfid_status_t gf_rfid_register_tag_callback(gf_rfid_tag_cb_t cb, void* user_data);
gf_rfid_status_t gf_rfid_register_inventory_callback(gf_rfid_inventory_cb_t cb, void* user_data);

/* Periodic processing */
gf_rfid_status_t gf_rfid_process(void);

#endif /* GF_RFID_SCANNER_H */
