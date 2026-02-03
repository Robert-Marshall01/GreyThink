/**
 * @file plc_interface.h
 * @brief PLC Communication Interface
 *
 * INDUSTRY RELEVANCE:
 * PLCs (Programmable Logic Controllers) are the backbone of industrial control:
 * - Standard protocols (EtherNet/IP, PROFINET, Modbus TCP)
 * - Tag-based data exchange
 * - Real-time I/O scanning
 * - Deterministic communication timing
 * - Integration with SCADA/HMI systems
 *
 * Used in: Factory automation, process control, packaging machines
 *
 * @note This is a stub demonstrating PLC integration patterns.
 */

#ifndef GF_PLC_INTERFACE_H
#define GF_PLC_INTERFACE_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* PLC Type Definitions                                                      */
/*===========================================================================*/

/**
 * @brief Supported PLC protocols
 */
typedef enum {
    GF_PLC_MODBUS_TCP,          /**< Modbus TCP */
    GF_PLC_ETHERNET_IP,         /**< EtherNet/IP (CIP) */
    GF_PLC_PROFINET,            /**< PROFINET */
    GF_PLC_OPCUA,               /**< OPC UA */
    GF_PLC_ADS                  /**< Beckhoff ADS */
} gf_plc_protocol_t;

/**
 * @brief PLC tag data types
 */
typedef enum {
    GF_TAG_BOOL,                /**< Boolean (BOOL) */
    GF_TAG_INT8,                /**< Signed 8-bit (SINT) */
    GF_TAG_UINT8,               /**< Unsigned 8-bit (USINT) */
    GF_TAG_INT16,               /**< Signed 16-bit (INT) */
    GF_TAG_UINT16,              /**< Unsigned 16-bit (UINT) */
    GF_TAG_INT32,               /**< Signed 32-bit (DINT) */
    GF_TAG_UINT32,              /**< Unsigned 32-bit (UDINT) */
    GF_TAG_FLOAT,               /**< 32-bit float (REAL) */
    GF_TAG_DOUBLE,              /**< 64-bit float (LREAL) */
    GF_TAG_STRING,              /**< String */
    GF_TAG_ARRAY                /**< Array type */
} gf_tag_type_t;

/**
 * @brief Tag access mode
 */
typedef enum {
    GF_TAG_READ_ONLY,
    GF_TAG_WRITE_ONLY,
    GF_TAG_READ_WRITE
} gf_tag_access_t;

/**
 * @brief PLC connection configuration
 */
typedef struct {
    gf_plc_protocol_t protocol;
    uint8_t ip_address[4];          /**< PLC IP address */
    uint16_t port;                  /**< Port (502, 44818, etc.) */
    uint16_t slot;                  /**< Slot/rack for some PLCs */
    uint32_t scan_rate_ms;          /**< I/O scan rate */
    uint32_t timeout_ms;            /**< Connection timeout */
    uint8_t retries;
} gf_plc_config_t;

/**
 * @brief Tag definition
 */
typedef struct {
    const char* name;               /**< Tag name/path */
    gf_tag_type_t type;
    gf_tag_access_t access;
    uint16_t array_length;          /**< Length if array */
    uint16_t address;               /**< Register address (Modbus) */
} gf_plc_tag_def_t;

/**
 * @brief Tag value union
 */
typedef union {
    bool b;
    int8_t i8;
    uint8_t u8;
    int16_t i16;
    uint16_t u16;
    int32_t i32;
    uint32_t u32;
    float f32;
    double f64;
    char str[64];
} gf_tag_value_t;

/**
 * @brief Tag read result
 */
typedef struct {
    const char* name;
    gf_tag_type_t type;
    gf_tag_value_t value;
    uint32_t timestamp_ms;
    uint8_t quality;                /**< 0=bad, 192=good */
    int error_code;
} gf_tag_result_t;

/**
 * @brief PLC connection status
 */
typedef struct {
    bool connected;
    uint32_t uptime_ms;
    uint32_t reads_completed;
    uint32_t writes_completed;
    uint32_t errors;
    uint32_t last_scan_time_us;
    uint32_t avg_scan_time_us;
} gf_plc_status_t;

/**
 * @brief PLC handle
 */
typedef struct gf_plc_interface* gf_plc_t;

/*===========================================================================*/
/* PLC Interface API                                                         */
/*===========================================================================*/

/**
 * @brief Initialize PLC interface
 * @param config Configuration
 * @param plc Output handle
 * @return 0 on success
 */
int gf_plc_init(const gf_plc_config_t* config, gf_plc_t* plc);

/**
 * @brief Connect to PLC
 * @param plc PLC handle
 * @return 0 on success
 */
int gf_plc_connect(gf_plc_t plc);

/**
 * @brief Register tag for scanning
 * @param plc PLC handle
 * @param tag Tag definition
 * @return Tag handle (>=0) or error (<0)
 */
int gf_plc_register_tag(gf_plc_t plc, const gf_plc_tag_def_t* tag);

/**
 * @brief Read single tag
 * @param plc PLC handle
 * @param tag_name Tag name
 * @param result Output result
 * @return 0 on success
 */
int gf_plc_read_tag(gf_plc_t plc,
                     const char* tag_name,
                     gf_tag_result_t* result);

/**
 * @brief Read multiple tags
 * @param plc PLC handle
 * @param tag_names Array of tag names
 * @param count Number of tags
 * @param results Output results
 * @return Number of successful reads
 */
int gf_plc_read_tags(gf_plc_t plc,
                      const char** tag_names,
                      size_t count,
                      gf_tag_result_t* results);

/**
 * @brief Write single tag
 * @param plc PLC handle
 * @param tag_name Tag name
 * @param value Value to write
 * @return 0 on success
 */
int gf_plc_write_tag(gf_plc_t plc,
                      const char* tag_name,
                      const gf_tag_value_t* value);

/**
 * @brief Run I/O scan cycle
 * @param plc PLC handle
 * @return 0 on success
 */
int gf_plc_scan(gf_plc_t plc);

/**
 * @brief Get connection status
 * @param plc PLC handle
 * @param status Output status
 * @return 0 on success
 */
int gf_plc_get_status(gf_plc_t plc, gf_plc_status_t* status);

/**
 * @brief Disconnect from PLC
 * @param plc PLC handle
 * @return 0 on success
 */
int gf_plc_disconnect(gf_plc_t plc);

/**
 * @brief Deinitialize PLC interface
 * @param plc PLC handle
 */
void gf_plc_deinit(gf_plc_t plc);

#ifdef __cplusplus
}
#endif

#endif /* GF_PLC_INTERFACE_H */
