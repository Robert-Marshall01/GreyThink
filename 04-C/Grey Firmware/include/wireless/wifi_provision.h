/**
 * @file wifi_provision.h
 * @brief Wi-Fi Provisioning State Machine
 * 
 * WHAT: Wi-Fi credential provisioning for headless IoT devices.
 *       Supports multiple provisioning methods: BLE, SoftAP, SmartConfig,
 *       and WPS. Manages secure credential storage and retry logic.
 * 
 * WHY: Safe, reliable provisioning is critical for IoT products.
 *      Understanding provisioning security, state machine design,
 *      and credential management demonstrates product-ready firmware
 *      development skills.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Smart Home: Thermostats, plugs, cameras
 *   - Industrial IoT: Gateways, sensors
 *   - Consumer: Speakers, appliances
 *   - Healthcare: Remote patient monitors
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Provisioning state machine
 *   - Secure credential storage
 *   - Connection retry with backoff
 *   - AP/STA mode switching
 *   - mDNS discovery
 */

#ifndef GF_WIFI_PROVISION_H
#define GF_WIFI_PROVISION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_WIFI_SSID_MAX        32
#define GF_WIFI_PASS_MAX        64
#define GF_WIFI_MAX_RETRIES     5
#define GF_WIFI_BACKOFF_BASE_MS 1000

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_PROV_METHOD_BLE = 0,     /* Provision via BLE characteristic */
    GF_PROV_METHOD_SOFTAP,      /* Device acts as AP, user connects */
    GF_PROV_METHOD_SMARTCONFIG, /* ESP-style SmartConfig */
    GF_PROV_METHOD_WPS          /* Wi-Fi Protected Setup */
} gf_prov_method_t;

typedef enum {
    GF_WIFI_STATE_IDLE = 0,
    GF_WIFI_STATE_PROVISIONING,     /* Waiting for credentials */
    GF_WIFI_STATE_CONNECTING,       /* Attempting connection */
    GF_WIFI_STATE_CONNECTED,        /* Connected to AP */
    GF_WIFI_STATE_DISCONNECTED,     /* Lost connection, retrying */
    GF_WIFI_STATE_FAILED            /* Max retries exceeded */
} gf_wifi_state_t;

typedef struct {
    char        ssid[GF_WIFI_SSID_MAX + 1];
    char        password[GF_WIFI_PASS_MAX + 1];
    bool        use_static_ip;
    uint32_t    static_ip;
    uint32_t    gateway;
    uint32_t    netmask;
} gf_wifi_credentials_t;

typedef struct {
    gf_wifi_state_t     state;
    int8_t              rssi;           /* Signal strength dBm */
    uint8_t             retry_count;
    uint32_t            ip_address;
    uint8_t             mac[6];
    bool                has_credentials;
} gf_wifi_status_t;

/* Callbacks */
typedef void (*gf_wifi_state_cb)(gf_wifi_state_t state, void *ctx);
typedef void (*gf_wifi_event_cb)(int event_id, void *event_data, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize Wi-Fi subsystem
 */
int gf_wifi_init(void);

/**
 * @brief Start provisioning mode
 */
int gf_wifi_start_provisioning(gf_prov_method_t method, const char *ap_name);

/**
 * @brief Stop provisioning mode
 */
int gf_wifi_stop_provisioning(void);

/**
 * @brief Set credentials directly (bypass provisioning)
 */
int gf_wifi_set_credentials(const gf_wifi_credentials_t *creds);

/**
 * @brief Connect to stored AP
 */
int gf_wifi_connect(void);

/**
 * @brief Disconnect from AP
 */
int gf_wifi_disconnect(void);

/**
 * @brief Get current status
 */
void gf_wifi_get_status(gf_wifi_status_t *status);

/**
 * @brief Clear stored credentials
 */
int gf_wifi_forget_credentials(void);

/**
 * @brief Register state change callback
 */
void gf_wifi_set_state_callback(gf_wifi_state_cb callback, void *ctx);

/**
 * @brief Scan for available networks
 */
int gf_wifi_scan(void);

/**
 * @brief Get scan results
 */
int gf_wifi_get_scan_results(void *buffer, uint16_t max_entries);

/**
 * @brief Get driver descriptor
 */
const void* gf_wifi_get_driver(void);

#endif /* GF_WIFI_PROVISION_H */
