/**
 * @file ble_gatt.h
 * @brief BLE GATT Service Framework
 * 
 * WHAT: Bluetooth Low Energy GATT service and characteristic definitions
 *       for embedded IoT devices. Provides registration API for custom
 *       services and handles read/write/notify operations.
 * 
 * WHY: BLE is the dominant protocol for battery-powered IoT devices.
 *      Understanding GATT architecture, characteristic properties,
 *      and connection management is essential for consumer electronics,
 *      wearables, and industrial sensor nodes.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Consumer: Fitness trackers, smart home devices
 *   - Medical: Glucose monitors, pulse oximeters
 *   - Industrial: Wireless sensor nodes, asset tracking
 *   - Automotive: Keyless entry, tire pressure monitors
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - GATT service/characteristic hierarchy
 *   - UUID management (16-bit and 128-bit)
 *   - Characteristic properties (R/W/N/I)
 *   - Connection state machine
 *   - Advertising configuration
 */

#ifndef GF_BLE_GATT_H
#define GF_BLE_GATT_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_BLE_MAX_SERVICES         8
#define GF_BLE_MAX_CHARACTERISTICS  16
#define GF_BLE_MAX_VALUE_LEN        512
#define GF_BLE_DEVICE_NAME_MAX      32

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef struct {
    union {
        uint16_t    uuid16;         /* SIG-assigned 16-bit UUID */
        uint8_t     uuid128[16];    /* Custom 128-bit UUID */
    };
    bool            is_uuid128;
} gf_ble_uuid_t;

typedef enum {
    GF_BLE_CHAR_READ        = (1 << 0),
    GF_BLE_CHAR_WRITE       = (1 << 1),
    GF_BLE_CHAR_WRITE_NR    = (1 << 2),     /* Write without response */
    GF_BLE_CHAR_NOTIFY      = (1 << 3),
    GF_BLE_CHAR_INDICATE    = (1 << 4)
} gf_ble_char_props_t;

typedef struct {
    gf_ble_uuid_t       uuid;
    gf_ble_char_props_t properties;
    uint16_t            max_len;
    void               *initial_value;
    uint16_t            initial_len;
} gf_ble_char_def_t;

typedef struct {
    gf_ble_uuid_t       uuid;
    uint8_t             char_count;
    gf_ble_char_def_t  *characteristics;
} gf_ble_service_def_t;

typedef enum {
    GF_BLE_STATE_OFF = 0,
    GF_BLE_STATE_ADVERTISING,
    GF_BLE_STATE_CONNECTED,
    GF_BLE_STATE_BONDED
} gf_ble_state_t;

/* Callbacks */
typedef void (*gf_ble_connect_cb)(uint16_t conn_handle, void *ctx);
typedef void (*gf_ble_disconnect_cb)(uint16_t conn_handle, uint8_t reason, void *ctx);
typedef void (*gf_ble_write_cb)(uint16_t char_handle, const uint8_t *data, 
                                 uint16_t len, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize BLE stack
 */
int gf_ble_init(const char *device_name);

/**
 * @brief Register a GATT service
 */
int gf_ble_register_service(const gf_ble_service_def_t *service);

/**
 * @brief Start advertising
 */
int gf_ble_start_advertising(uint16_t interval_ms, bool connectable);

/**
 * @brief Stop advertising
 */
int gf_ble_stop_advertising(void);

/**
 * @brief Get connection state
 */
gf_ble_state_t gf_ble_get_state(void);

/**
 * @brief Send notification to connected client
 */
int gf_ble_notify(uint16_t char_handle, const uint8_t *data, uint16_t len);

/**
 * @brief Disconnect client
 */
int gf_ble_disconnect(uint16_t conn_handle);

/**
 * @brief Set connection callbacks
 */
void gf_ble_set_callbacks(gf_ble_connect_cb connect,
                          gf_ble_disconnect_cb disconnect,
                          gf_ble_write_cb write,
                          void *ctx);

/**
 * @brief Get driver descriptor
 */
const void* gf_ble_get_driver(void);

#endif /* GF_BLE_GATT_H */
