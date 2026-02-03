/**
 * @file modbus.h
 * @brief Modbus RTU/TCP Protocol Driver
 *
 * INDUSTRY RELEVANCE:
 * Modbus is the de facto standard for industrial automation:
 * - RS-485 multi-drop networks in factories
 * - PLC and SCADA system integration
 * - Sensor/actuator communication
 * - Both RTU (serial) and TCP (Ethernet) variants
 * - Master/slave architecture
 *
 * Used in: Factory automation, building management, energy monitoring
 *
 * @note This is a stub demonstrating industrial protocol patterns.
 */

#ifndef GF_MODBUS_H
#define GF_MODBUS_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Modbus Definitions                                                        */
/*===========================================================================*/

/**
 * @brief Modbus protocol variants
 */
typedef enum {
    GF_MODBUS_RTU,              /**< Serial RTU mode */
    GF_MODBUS_ASCII,            /**< Serial ASCII mode */
    GF_MODBUS_TCP               /**< TCP/IP mode */
} gf_modbus_mode_t;

/**
 * @brief Modbus function codes
 */
typedef enum {
    GF_MODBUS_READ_COILS            = 0x01,
    GF_MODBUS_READ_DISCRETE_INPUTS  = 0x02,
    GF_MODBUS_READ_HOLDING_REGS     = 0x03,
    GF_MODBUS_READ_INPUT_REGS       = 0x04,
    GF_MODBUS_WRITE_SINGLE_COIL     = 0x05,
    GF_MODBUS_WRITE_SINGLE_REG      = 0x06,
    GF_MODBUS_WRITE_MULTIPLE_COILS  = 0x0F,
    GF_MODBUS_WRITE_MULTIPLE_REGS   = 0x10,
    GF_MODBUS_READ_WRITE_REGS       = 0x17,
    GF_MODBUS_MASK_WRITE_REG        = 0x16,
    GF_MODBUS_READ_FIFO             = 0x18,
    GF_MODBUS_READ_DEVICE_ID        = 0x2B
} gf_modbus_func_t;

/**
 * @brief Modbus exception codes
 */
typedef enum {
    GF_MODBUS_OK                    = 0x00,
    GF_MODBUS_EX_ILLEGAL_FUNCTION   = 0x01,
    GF_MODBUS_EX_ILLEGAL_ADDRESS    = 0x02,
    GF_MODBUS_EX_ILLEGAL_VALUE      = 0x03,
    GF_MODBUS_EX_SLAVE_FAILURE      = 0x04,
    GF_MODBUS_EX_ACKNOWLEDGE        = 0x05,
    GF_MODBUS_EX_SLAVE_BUSY         = 0x06,
    GF_MODBUS_EX_MEMORY_ERROR       = 0x08,
    GF_MODBUS_EX_GATEWAY_PATH       = 0x0A,
    GF_MODBUS_EX_GATEWAY_TARGET     = 0x0B
} gf_modbus_exception_t;

/**
 * @brief Serial port configuration (RTU/ASCII)
 */
typedef struct {
    uint32_t baud_rate;             /**< Baud rate (9600, 19200, etc.) */
    uint8_t data_bits;              /**< Data bits (7 or 8) */
    uint8_t parity;                 /**< 0=none, 1=odd, 2=even */
    uint8_t stop_bits;              /**< Stop bits (1 or 2) */
} gf_modbus_serial_t;

/**
 * @brief Modbus configuration
 */
typedef struct {
    gf_modbus_mode_t mode;
    
    /* Serial settings (RTU/ASCII) */
    gf_modbus_serial_t serial;
    
    /* TCP settings */
    uint8_t ip_address[4];
    uint16_t port;                  /**< Default: 502 */
    
    /* Common settings */
    uint8_t slave_address;          /**< Slave address (1-247) */
    uint32_t response_timeout_ms;
    uint8_t max_retries;
    
    /* Callbacks */
    int (*serial_write)(const uint8_t* data, size_t len);
    int (*serial_read)(uint8_t* data, size_t max_len, uint32_t timeout_ms);
} gf_modbus_config_t;

/**
 * @brief Modbus statistics
 */
typedef struct {
    uint32_t requests_sent;
    uint32_t responses_received;
    uint32_t timeouts;
    uint32_t crc_errors;
    uint32_t exceptions_received;
    uint32_t bus_errors;
} gf_modbus_stats_t;

/**
 * @brief Modbus handle
 */
typedef struct gf_modbus* gf_modbus_t;

/*===========================================================================*/
/* Modbus Master API                                                         */
/*===========================================================================*/

/**
 * @brief Initialize Modbus master
 * @param config Configuration
 * @param modbus Output handle
 * @return 0 on success
 */
int gf_modbus_init(const gf_modbus_config_t* config, gf_modbus_t* modbus);

/**
 * @brief Read coils (FC 01)
 * @param modbus Modbus handle
 * @param slave_addr Slave address
 * @param start_addr Starting coil address
 * @param count Number of coils to read
 * @param values Output buffer (bit-packed)
 * @return 0 on success, exception code on error
 */
int gf_modbus_read_coils(gf_modbus_t modbus,
                          uint8_t slave_addr,
                          uint16_t start_addr,
                          uint16_t count,
                          uint8_t* values);

/**
 * @brief Read discrete inputs (FC 02)
 */
int gf_modbus_read_discrete_inputs(gf_modbus_t modbus,
                                    uint8_t slave_addr,
                                    uint16_t start_addr,
                                    uint16_t count,
                                    uint8_t* values);

/**
 * @brief Read holding registers (FC 03)
 * @param modbus Modbus handle
 * @param slave_addr Slave address
 * @param start_addr Starting register address
 * @param count Number of registers
 * @param values Output buffer (16-bit values)
 * @return 0 on success
 */
int gf_modbus_read_holding_regs(gf_modbus_t modbus,
                                 uint8_t slave_addr,
                                 uint16_t start_addr,
                                 uint16_t count,
                                 uint16_t* values);

/**
 * @brief Read input registers (FC 04)
 */
int gf_modbus_read_input_regs(gf_modbus_t modbus,
                               uint8_t slave_addr,
                               uint16_t start_addr,
                               uint16_t count,
                               uint16_t* values);

/**
 * @brief Write single coil (FC 05)
 */
int gf_modbus_write_coil(gf_modbus_t modbus,
                          uint8_t slave_addr,
                          uint16_t addr,
                          bool value);

/**
 * @brief Write single register (FC 06)
 */
int gf_modbus_write_register(gf_modbus_t modbus,
                              uint8_t slave_addr,
                              uint16_t addr,
                              uint16_t value);

/**
 * @brief Write multiple registers (FC 16)
 */
int gf_modbus_write_registers(gf_modbus_t modbus,
                               uint8_t slave_addr,
                               uint16_t start_addr,
                               uint16_t count,
                               const uint16_t* values);

/**
 * @brief Get Modbus statistics
 */
int gf_modbus_get_stats(gf_modbus_t modbus, gf_modbus_stats_t* stats);

/**
 * @brief Reset statistics
 */
int gf_modbus_reset_stats(gf_modbus_t modbus);

/**
 * @brief Deinitialize Modbus
 */
void gf_modbus_deinit(gf_modbus_t modbus);

/*===========================================================================*/
/* Modbus CRC Utilities                                                      */
/*===========================================================================*/

/**
 * @brief Calculate Modbus RTU CRC-16
 * @param data Data buffer
 * @param len Data length
 * @return CRC-16 value
 */
uint16_t gf_modbus_crc16(const uint8_t* data, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* GF_MODBUS_H */
