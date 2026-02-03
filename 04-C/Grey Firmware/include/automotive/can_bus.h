/**
 * @file can_bus.h
 * @brief CAN Bus Driver - SPOTLIGHT SUBSYSTEM
 * 
 * WHAT: Production-grade CAN 2.0B driver with protocol timing, error handling,
 *       fault tolerance, and bus-off recovery.
 * 
 * WHY: CAN (Controller Area Network) is the backbone of automotive and
 *      industrial systems. Every modern vehicle has 2-5 CAN buses connecting
 *      ECUs for engine, transmission, body, chassis, and infotainment.
 *      Deep CAN expertise is essential for automotive firmware roles.
 *      
 *      Key differentiators for senior roles:
 *      - Understanding bit timing and synchronization
 *      - Error counter management and bus-off handling
 *      - Gateway/bridging between CAN networks
 *      - CAN-FD support and migration strategies
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Complete bit timing calculation from oscillator to arbitration
 *      - Error state machine (active/passive/bus-off) per CAN spec
 *      - Hardware abstraction for multiple CAN controller types
 *      - Filter configuration for efficient message routing
 *      - TX queue with priority sorting and retry logic
 *      - Integration with message bus for system-wide event distribution
 * 
 * Industry applications: automotive ECUs, industrial automation, robotics,
 *                        medical devices, aerospace systems
 */

#ifndef GF_CAN_BUS_H
#define GF_CAN_BUS_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* CAN Protocol Constants (ISO 11898)                                         */
/*===========================================================================*/

/* Standard CAN bit rates */
#define GF_CAN_BAUD_1MBIT       1000000
#define GF_CAN_BAUD_500KBIT     500000
#define GF_CAN_BAUD_250KBIT     250000
#define GF_CAN_BAUD_125KBIT     125000
#define GF_CAN_BAUD_100KBIT     100000

/* Error counter thresholds (per CAN specification) */
#define GF_CAN_ERR_CNT_ACTIVE   96      /* Below: error active */
#define GF_CAN_ERR_CNT_PASSIVE  127     /* Below: error passive */
#define GF_CAN_ERR_CNT_BUSOFF   256     /* At or above: bus-off */

/* Maximum message counts */
#define GF_CAN_MAX_FILTERS      16
#define GF_CAN_TX_QUEUE_SIZE    16
#define GF_CAN_RX_BUFFER_SIZE   32

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/**
 * CAN message frame (standard and extended ID support)
 */
typedef struct {
    uint32_t    id;             /* 11-bit (std) or 29-bit (ext) identifier */
    uint8_t     data[8];        /* Payload (0-8 bytes) */
    uint8_t     dlc;            /* Data length code (0-8) */
    bool        extended;       /* true = 29-bit ID, false = 11-bit ID */
    bool        rtr;            /* Remote transmission request */
    uint32_t    timestamp;      /* RX timestamp (microseconds) */
} gf_can_frame_t;

/**
 * CAN error states (per ISO 11898-1)
 */
typedef enum {
    GF_CAN_ERR_ACTIVE = 0,      /* Normal operation */
    GF_CAN_ERR_PASSIVE,         /* Transmitting passive error flags */
    GF_CAN_ERR_BUSOFF,          /* Disconnected from bus */
    GF_CAN_ERR_RECOVERING       /* Bus-off recovery in progress */
} gf_can_err_state_t;

/**
 * CAN error types for diagnostics
 */
typedef enum {
    GF_CAN_ERR_NONE = 0,
    GF_CAN_ERR_STUFF,           /* Bit stuffing violation */
    GF_CAN_ERR_FORM,            /* Frame format error */
    GF_CAN_ERR_ACK,             /* No acknowledgment received */
    GF_CAN_ERR_BIT_RECESSIVE,   /* Bit error (recessive) */
    GF_CAN_ERR_BIT_DOMINANT,    /* Bit error (dominant) */
    GF_CAN_ERR_CRC,             /* CRC mismatch */
    GF_CAN_ERR_OVERLOAD         /* Overload frame received */
} gf_can_err_type_t;

/**
 * Bit timing configuration
 * 
 * CAN bit structure: SYNC_SEG | PROP_SEG | PHASE_SEG1 | PHASE_SEG2
 * Sample point is at end of PHASE_SEG1 (typically 75-87.5%)
 */
typedef struct {
    uint16_t    prescaler;      /* Baud rate prescaler (1-1024) */
    uint8_t     sync_jump_width; /* SJW: 1-4 TQ (resynchronization) */
    uint8_t     time_seg1;      /* PROP_SEG + PHASE_SEG1: 1-16 TQ */
    uint8_t     time_seg2;      /* PHASE_SEG2: 1-8 TQ */
    uint8_t     sample_point;   /* Sample point percentage (0-100) */
} gf_can_timing_t;

/**
 * CAN controller configuration
 */
typedef struct {
    uint32_t            baud_rate;      /* Target baud rate */
    gf_can_timing_t     timing;         /* Bit timing (auto-calculated if prescaler=0) */
    bool                loopback;       /* Enable loopback mode (testing) */
    bool                silent;         /* Silent mode (monitor only) */
    bool                auto_retransmit; /* Auto-retry failed transmissions */
    uint32_t            bus_off_recovery_ms; /* Time before bus-off recovery */
} gf_can_config_t;

/**
 * Acceptance filter configuration
 */
typedef struct {
    uint32_t    id;             /* ID to match */
    uint32_t    mask;           /* Mask (1 = must match, 0 = don't care) */
    bool        extended;       /* Match extended IDs */
    uint8_t     fifo;           /* Target RX FIFO (0 or 1) */
} gf_can_filter_t;

/**
 * CAN bus statistics
 */
typedef struct {
    uint32_t            tx_count;       /* Frames transmitted */
    uint32_t            rx_count;       /* Frames received */
    uint32_t            tx_errors;      /* TX error count */
    uint32_t            rx_errors;      /* RX error count */
    uint32_t            error_frames;   /* Error frame count */
    uint32_t            overruns;       /* RX buffer overruns */
    uint32_t            arbitration_lost; /* Lost arbitrations */
    uint8_t             tec;            /* Transmit error counter */
    uint8_t             rec;            /* Receive error counter */
    gf_can_err_state_t  error_state;    /* Current error state */
    gf_can_err_type_t   last_error;     /* Most recent error type */
    uint8_t             bus_load_percent; /* Approximate bus utilization */
} gf_can_stats_t;

/* Callback types */
typedef void (*gf_can_rx_cb)(const gf_can_frame_t *frame, void *ctx);
typedef void (*gf_can_err_cb)(gf_can_err_type_t error, gf_can_err_state_t state, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize CAN driver
 * @param config CAN configuration
 * @return GF_OK on success
 */
int gf_can_init(const gf_can_config_t *config);

/**
 * @brief Deinitialize CAN driver
 */
int gf_can_deinit(void);

/**
 * @brief Start CAN communications
 */
int gf_can_start(void);

/**
 * @brief Stop CAN communications
 */
int gf_can_stop(void);

/**
 * @brief Configure acceptance filter
 * @param filter_num Filter slot (0 to GF_CAN_MAX_FILTERS-1)
 * @param filter Filter configuration
 */
int gf_can_set_filter(uint8_t filter_num, const gf_can_filter_t *filter);

/**
 * @brief Transmit a CAN frame
 * @param frame Frame to transmit
 * @param timeout_ms Timeout (0 = non-blocking)
 * @return GF_OK if queued, GF_ERR_TIMEOUT if queue full
 */
int gf_can_transmit(const gf_can_frame_t *frame, uint32_t timeout_ms);

/**
 * @brief Receive a CAN frame (blocking)
 * @param frame Output buffer
 * @param timeout_ms Timeout (0 = poll, -1 = infinite)
 * @return GF_OK if received, GF_ERR_TIMEOUT if none available
 */
int gf_can_receive(gf_can_frame_t *frame, uint32_t timeout_ms);

/**
 * @brief Check if frames are available
 * @return Number of frames in RX buffer
 */
int gf_can_available(void);

/**
 * @brief Register RX callback (called from ISR context)
 */
void gf_can_set_rx_callback(gf_can_rx_cb callback, void *ctx);

/**
 * @brief Register error callback
 */
void gf_can_set_error_callback(gf_can_err_cb callback, void *ctx);

/**
 * @brief Get CAN statistics
 */
void gf_can_get_stats(gf_can_stats_t *stats);

/**
 * @brief Reset statistics counters
 */
void gf_can_reset_stats(void);

/**
 * @brief Get current error state
 */
gf_can_err_state_t gf_can_get_error_state(void);

/**
 * @brief Manually trigger bus-off recovery
 */
int gf_can_recover_bus_off(void);

/**
 * @brief Calculate bit timing for a given baud rate
 * @param clock_hz CAN peripheral clock frequency
 * @param baud_rate Target baud rate
 * @param timing Output: calculated timing parameters
 * @return GF_OK if valid timing found
 */
int gf_can_calc_timing(uint32_t clock_hz, uint32_t baud_rate, gf_can_timing_t *timing);

/**
 * @brief Process CAN events (call from main loop or task)
 */
void gf_can_process(void);

/**
 * @brief Get CAN driver descriptor for registration
 */
const void* gf_can_get_driver(void);

/*===========================================================================*/
/* Diagnostic Functions                                                       */
/*===========================================================================*/

/**
 * @brief Enable CAN bus traffic logger
 */
void gf_can_log_enable(bool enable);

/**
 * @brief Get error counter values
 */
void gf_can_get_error_counters(uint8_t *tec, uint8_t *rec);

/**
 * @brief Inject test error (for validation)
 */
void gf_can_inject_error(gf_can_err_type_t error);

#endif /* GF_CAN_BUS_H */
