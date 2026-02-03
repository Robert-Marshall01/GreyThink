/**
 * @file matter.h
 * @brief Matter Protocol Driver Stub
 * 
 * INDUSTRY RELEVANCE:
 * Matter (formerly Project CHIP) is the new unified smart home standard backed
 * by Apple, Google, Amazon, and Samsung. It runs over Thread, Wi-Fi, and Ethernet,
 * providing true cross-platform interoperability. Released in 2022, Matter is
 * rapidly becoming the industry standard for new smart home products.
 * 
 * WHY THIS MATTERS:
 * - Unified protocol across all major ecosystems (Apple Home, Google Home, Alexa)
 * - Thread mesh networking for low-power devices
 * - WiFi for high-bandwidth devices (cameras, displays)
 * - Strong security with device attestation
 * - IPv6-based addressing
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Multi-transport abstraction (Thread/WiFi/Ethernet)
 * - Commissioning and device pairing flows
 * - Data model with clusters and attributes
 * - Secure communication with certificates
 */

#ifndef GF_MATTER_H
#define GF_MATTER_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_MATTER_MAX_FABRICS       5       /* Max connected ecosystems */
#define GF_MATTER_MAX_ENDPOINTS     16      /* Device endpoints */
#define GF_MATTER_MAX_CLUSTERS      32      /* Clusters per device */
#define GF_MATTER_MAX_BINDINGS      16      /* Device bindings */
#define GF_MATTER_SETUP_CODE_LEN    11      /* Setup code length */
#define GF_MATTER_VENDOR_NAME_LEN   32      /* Vendor name max length */
#define GF_MATTER_PRODUCT_NAME_LEN  32      /* Product name max length */

/*===========================================================================*/
/* Matter Protocol Types                                                      */
/*===========================================================================*/

/* Transport types */
typedef enum {
    GF_MATTER_TRANSPORT_THREAD = 0,     /* Thread mesh (802.15.4) */
    GF_MATTER_TRANSPORT_WIFI,           /* WiFi (802.11) */
    GF_MATTER_TRANSPORT_ETHERNET,       /* Wired Ethernet */
    GF_MATTER_TRANSPORT_BLE             /* BLE for commissioning only */
} gf_matter_transport_t;

/* Device lifecycle state */
typedef enum {
    GF_MATTER_STATE_UNPAIRED = 0,       /* Not commissioned */
    GF_MATTER_STATE_COMMISSIONING,      /* Pairing in progress */
    GF_MATTER_STATE_PAIRED,             /* Commissioned to fabric */
    GF_MATTER_STATE_OPERATIONAL,        /* Fully operational */
    GF_MATTER_STATE_ERROR               /* Error state */
} gf_matter_state_t;

/* Standard Matter device types */
typedef enum {
    GF_MATTER_DEV_ON_OFF_LIGHT      = 0x0100,
    GF_MATTER_DEV_DIMMABLE_LIGHT    = 0x0101,
    GF_MATTER_DEV_COLOR_LIGHT       = 0x010D,
    GF_MATTER_DEV_ON_OFF_SWITCH     = 0x0103,
    GF_MATTER_DEV_DIMMER_SWITCH     = 0x0104,
    GF_MATTER_DEV_CONTACT_SENSOR    = 0x0015,
    GF_MATTER_DEV_OCCUPANCY_SENSOR  = 0x0107,
    GF_MATTER_DEV_TEMP_SENSOR       = 0x0302,
    GF_MATTER_DEV_HUMIDITY_SENSOR   = 0x0307,
    GF_MATTER_DEV_DOOR_LOCK         = 0x000A,
    GF_MATTER_DEV_THERMOSTAT        = 0x0301,
    GF_MATTER_DEV_FAN               = 0x002B,
    GF_MATTER_DEV_WINDOW_COVERING   = 0x0202,
    GF_MATTER_DEV_BRIDGE            = 0x000E
} gf_matter_device_type_t;

/* Standard Matter clusters */
typedef enum {
    GF_MATTER_CLUSTER_DESCRIPTOR        = 0x001D,   /* Device descriptor */
    GF_MATTER_CLUSTER_BINDING           = 0x001E,   /* Device bindings */
    GF_MATTER_CLUSTER_ACCESS_CTRL       = 0x001F,   /* Access control */
    GF_MATTER_CLUSTER_BASIC_INFO        = 0x0028,   /* Basic information */
    GF_MATTER_CLUSTER_OTA_PROVIDER      = 0x0029,   /* OTA software update */
    GF_MATTER_CLUSTER_NETWORK_COMM      = 0x0031,   /* Network commissioning */
    GF_MATTER_CLUSTER_GENERAL_COMM      = 0x0030,   /* General commissioning */
    GF_MATTER_CLUSTER_DIAG_NETWORK      = 0x0035,   /* Network diagnostics */
    GF_MATTER_CLUSTER_IDENTIFY          = 0x0003,   /* Device identify */
    GF_MATTER_CLUSTER_GROUPS            = 0x0004,   /* Group management */
    GF_MATTER_CLUSTER_SCENES            = 0x0005,   /* Scene management */
    GF_MATTER_CLUSTER_ON_OFF            = 0x0006,   /* On/Off control */
    GF_MATTER_CLUSTER_LEVEL             = 0x0008,   /* Level control */
    GF_MATTER_CLUSTER_COLOR             = 0x0300,   /* Color control */
    GF_MATTER_CLUSTER_TEMP_MEAS         = 0x0402,   /* Temperature */
    GF_MATTER_CLUSTER_HUMIDITY_MEAS     = 0x0405,   /* Humidity */
    GF_MATTER_CLUSTER_OCCUPANCY         = 0x0406,   /* Occupancy sensing */
    GF_MATTER_CLUSTER_DOOR_LOCK         = 0x0101    /* Door lock */
} gf_matter_cluster_id_t;

/*===========================================================================*/
/* Data Structures                                                            */
/*===========================================================================*/

/* Device attestation certificate (DAC) info */
typedef struct {
    uint16_t    vendor_id;
    uint16_t    product_id;
    uint8_t     certificate[512];       /* DER-encoded certificate */
    uint16_t    cert_len;
    uint8_t     private_key[32];        /* Device private key */
} gf_matter_dac_t;

/* Fabric information (connected ecosystem) */
typedef struct {
    uint64_t    fabric_id;              /* Fabric identifier */
    uint8_t     root_cert[512];         /* Root CA certificate */
    uint16_t    root_cert_len;
    uint8_t     noc[512];               /* Node operational certificate */
    uint16_t    noc_len;
    uint64_t    node_id;                /* Our node ID in this fabric */
    char        fabric_label[32];       /* e.g., "Apple Home", "Google" */
    bool        is_active;
} gf_matter_fabric_t;

/* Basic device info */
typedef struct {
    uint16_t    vendor_id;
    uint16_t    product_id;
    char        vendor_name[GF_MATTER_VENDOR_NAME_LEN];
    char        product_name[GF_MATTER_PRODUCT_NAME_LEN];
    char        serial_number[32];
    char        software_version[16];
    uint32_t    software_version_number;
    char        hardware_version[16];
    uint16_t    hardware_version_number;
} gf_matter_device_info_t;

/* Endpoint definition */
typedef struct {
    uint8_t                     endpoint_id;
    gf_matter_device_type_t     device_type;
    uint16_t                    clusters[GF_MATTER_MAX_CLUSTERS];
    uint8_t                     cluster_count;
} gf_matter_endpoint_t;

/* Setup payload for QR code/manual pairing */
typedef struct {
    uint32_t    setup_passcode;         /* 8-digit passcode */
    uint16_t    discriminator;          /* Device discriminator */
    uint8_t     version;                /* Payload version */
    uint8_t     commissioning_flow;     /* 0=standard, 1=user consent */
    uint16_t    vendor_id;
    uint16_t    product_id;
    uint8_t     rendezvous_flags;       /* BLE, SoftAP, OnNetwork */
} gf_matter_setup_payload_t;

/* Callbacks */
typedef void (*gf_matter_commission_cb_t)(gf_matter_state_t state, 
                                           const gf_matter_fabric_t *fabric,
                                           void *ctx);
typedef void (*gf_matter_cmd_cb_t)(uint8_t endpoint, uint32_t cluster,
                                   uint8_t command, const uint8_t *payload,
                                   uint16_t len, void *ctx);
typedef void (*gf_matter_attr_cb_t)(uint8_t endpoint, uint32_t cluster,
                                    uint32_t attr_id, const uint8_t *value,
                                    uint16_t len, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize Matter stack
 * @param device_info Device identification info
 * @param dac Device attestation credentials
 * @return GF_OK on success
 */
int gf_matter_init(const gf_matter_device_info_t *device_info,
                   const gf_matter_dac_t *dac);

/**
 * @brief Start Matter stack on specified transport
 * @param transport Transport layer to use
 */
int gf_matter_start(gf_matter_transport_t transport);

/**
 * @brief Stop Matter stack
 */
int gf_matter_stop(void);

/**
 * @brief Get current device state
 */
gf_matter_state_t gf_matter_get_state(void);

/**
 * @brief Add endpoint to device
 */
int gf_matter_add_endpoint(const gf_matter_endpoint_t *endpoint);

/**
 * @brief Start commissioning window
 * @param setup Setup payload for pairing
 * @param timeout_sec How long to stay in commissioning mode
 */
int gf_matter_open_commissioning(const gf_matter_setup_payload_t *setup,
                                  uint16_t timeout_sec);

/**
 * @brief Close commissioning window
 */
int gf_matter_close_commissioning(void);

/**
 * @brief Generate QR code content from setup payload
 * @param setup Setup payload
 * @param qr_buffer Output buffer for QR string
 * @param buffer_len Buffer size
 */
int gf_matter_generate_qr_code(const gf_matter_setup_payload_t *setup,
                                char *qr_buffer, uint16_t buffer_len);

/**
 * @brief Generate manual pairing code
 */
int gf_matter_generate_manual_code(const gf_matter_setup_payload_t *setup,
                                    char *code_buffer, uint16_t buffer_len);

/**
 * @brief Get fabric information
 * @param index Fabric index (0 to GF_MATTER_MAX_FABRICS-1)
 * @return Fabric info or NULL
 */
const gf_matter_fabric_t *gf_matter_get_fabric(uint8_t index);

/**
 * @brief Remove device from fabric
 * @param fabric_id Fabric to leave
 */
int gf_matter_leave_fabric(uint64_t fabric_id);

/**
 * @brief Factory reset device (remove all fabrics)
 */
int gf_matter_factory_reset(void);

/**
 * @brief Register commissioning event callback
 */
int gf_matter_register_commission_cb(gf_matter_commission_cb_t callback,
                                      void *ctx);

/**
 * @brief Register command receive callback
 */
int gf_matter_register_cmd_cb(gf_matter_cmd_cb_t callback, void *ctx);

/**
 * @brief Register attribute write callback
 */
int gf_matter_register_attr_cb(gf_matter_attr_cb_t callback, void *ctx);

/**
 * @brief Set attribute value
 * @param endpoint Endpoint ID
 * @param cluster Cluster ID
 * @param attr_id Attribute ID
 * @param value Attribute value bytes
 * @param len Value length
 */
int gf_matter_set_attribute(uint8_t endpoint, uint32_t cluster,
                            uint32_t attr_id, const void *value, uint16_t len);

/**
 * @brief Get attribute value
 */
int gf_matter_get_attribute(uint8_t endpoint, uint32_t cluster,
                            uint32_t attr_id, void *value, uint16_t *len);

/**
 * @brief Send command to bound device
 */
int gf_matter_send_command(uint8_t endpoint, uint32_t cluster,
                           uint8_t command, const void *payload, uint16_t len);

/**
 * @brief Process Matter stack (call from main loop)
 */
int gf_matter_process(uint32_t timeout_ms);

/*===========================================================================*/
/* Cluster-Specific Helpers                                                   */
/*===========================================================================*/

/**
 * @brief Set On/Off cluster state
 */
int gf_matter_set_on_off(uint8_t endpoint, bool on);

/**
 * @brief Set Level cluster value
 */
int gf_matter_set_level(uint8_t endpoint, uint8_t level);

/**
 * @brief Set Color cluster (hue/saturation)
 */
int gf_matter_set_color_hs(uint8_t endpoint, uint8_t hue, uint8_t saturation);

/**
 * @brief Set Color temperature
 */
int gf_matter_set_color_temp(uint8_t endpoint, uint16_t color_temp_mireds);

/**
 * @brief Report temperature measurement
 */
int gf_matter_report_temperature(uint8_t endpoint, int16_t temp_centi_c);

/**
 * @brief Report humidity measurement
 */
int gf_matter_report_humidity(uint8_t endpoint, uint16_t humidity_percent_100);

/**
 * @brief Report occupancy state
 */
int gf_matter_report_occupancy(uint8_t endpoint, bool occupied);

#endif /* GF_MATTER_H */
