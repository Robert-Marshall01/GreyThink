/**
 * @file fault_tolerant_comm.h
 * @brief Fault-Tolerant Space Communication Interface
 * 
 * INDUSTRY RELEVANCE:
 * Reliable communication in space environments requires:
 * - Deep Space Network (DSN) protocol support
 * - CCSDS (Consultative Committee for Space Data Systems) compliance
 * - Radiation-induced bit-flip error correction
 * - Store-and-forward for communication blackouts
 * - Multi-path redundancy (primary/backup transponders)
 * - Power-efficient burst transmission modes
 * 
 * This module demonstrates expertise in:
 * - Reed-Solomon and Turbo code error correction
 * - Interleaving for burst error mitigation
 * - Priority-based data scheduling
 * - Doppler shift compensation
 * - Delay-tolerant networking (DTN) concepts
 * - Mission-critical data preservation
 */

#ifndef GF_FAULT_TOLERANT_COMM_H
#define GF_FAULT_TOLERANT_COMM_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_FTC_MAX_PACKET_SIZE      4096
#define GF_FTC_MAX_QUEUE_DEPTH      128
#define GF_FTC_MAX_CHANNELS         4

typedef enum {
    GF_FTC_OK = 0,
    GF_FTC_ERROR_NOT_INITIALIZED,
    GF_FTC_ERROR_NO_LINK,
    GF_FTC_ERROR_BUFFER_FULL,
    GF_FTC_ERROR_ENCODE_FAILED,
    GF_FTC_ERROR_DECODE_FAILED,
    GF_FTC_ERROR_UNCORRECTABLE,
    GF_FTC_ERROR_TIMEOUT,
    GF_FTC_WARN_DEGRADED_LINK
} gf_ftc_status_t;

typedef enum {
    GF_FTC_CHANNEL_PRIMARY,
    GF_FTC_CHANNEL_BACKUP,
    GF_FTC_CHANNEL_EMERGENCY,
    GF_FTC_CHANNEL_RELAY            /* Via relay satellite */
} gf_ftc_channel_t;

typedef enum {
    GF_FTC_DATA_HOUSEKEEPING,
    GF_FTC_DATA_SCIENCE,
    GF_FTC_DATA_TELEMETRY,
    GF_FTC_DATA_COMMAND,
    GF_FTC_DATA_EMERGENCY
} gf_ftc_data_type_t;

typedef enum {
    GF_FTC_FEC_NONE,
    GF_FTC_FEC_REED_SOLOMON,
    GF_FTC_FEC_TURBO,
    GF_FTC_FEC_LDPC,
    GF_FTC_FEC_CONVOLUTIONAL
} gf_ftc_fec_t;

typedef struct {
    uint32_t packet_id;
    gf_ftc_data_type_t type;
    uint8_t priority;               /* 0 = lowest, 255 = highest */
    uint16_t length;
    uint8_t data[GF_FTC_MAX_PACKET_SIZE];
    uint32_t timestamp_ms;
    uint32_t deadline_ms;           /* Drop if not sent by deadline */
    bool acknowledged;
    uint8_t retry_count;
} gf_ftc_packet_t;

typedef struct {
    gf_ftc_channel_t channel;
    bool link_active;
    float signal_strength_dbm;
    float bit_error_rate;
    uint32_t bytes_sent;
    uint32_t bytes_received;
    uint32_t errors_corrected;
    uint32_t errors_uncorrectable;
    float link_margin_db;
} gf_ftc_link_status_t;

typedef struct {
    gf_ftc_fec_t fec_type;
    uint8_t fec_rate;               /* 1/2, 2/3, etc. as numerator */
    uint16_t interleave_depth;
    bool store_and_forward;
    uint32_t tx_power_mw;
    uint32_t bitrate_bps;
    bool auto_failover;
} gf_ftc_config_t;

gf_ftc_status_t gf_ftc_init(const gf_ftc_config_t* config);
void gf_ftc_shutdown(void);
gf_ftc_status_t gf_ftc_queue_packet(const gf_ftc_packet_t* packet);
gf_ftc_status_t gf_ftc_transmit(void);
gf_ftc_status_t gf_ftc_receive(gf_ftc_packet_t* packet);
gf_ftc_status_t gf_ftc_switch_channel(gf_ftc_channel_t channel);
gf_ftc_status_t gf_ftc_get_link_status(gf_ftc_channel_t channel, gf_ftc_link_status_t* status);
gf_ftc_status_t gf_ftc_set_fec(gf_ftc_fec_t fec, uint8_t rate);
gf_ftc_status_t gf_ftc_flush_queue(gf_ftc_data_type_t type);
uint16_t gf_ftc_get_queue_depth(void);
gf_ftc_status_t gf_ftc_send_emergency(const uint8_t* data, uint16_t length);

#ifdef __cplusplus
}
#endif

#endif /* GF_FAULT_TOLERANT_COMM_H */
