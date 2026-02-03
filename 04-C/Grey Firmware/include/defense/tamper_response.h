/**
 * @file tamper_response.h
 * @brief Tamper Detection and Response Module Stub
 * 
 * INDUSTRY RELEVANCE:
 * High-security systems require active tamper detection and response to
 * protect cryptographic keys, sensitive data, and intellectual property.
 * Applications include payment terminals (PCI-DSS), military equipment,
 * and secure key storage devices. The security hardware market exceeds $5B.
 * Firmware must detect intrusion attempts and respond within milliseconds.
 * 
 * Key challenges:
 * - Sub-millisecond response to tamper events
 * - Anti-tamper mesh monitoring
 * - Zeroization of sensitive data
 * - Environmental attack detection (voltage, temperature, light)
 * - Secure audit logging of tamper events
 */

#ifndef GF_TAMPER_RESPONSE_H
#define GF_TAMPER_RESPONSE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Tamper detection status codes */
typedef enum {
    GF_TAMPER_OK = 0,
    GF_TAMPER_DETECTED,             /* Active tamper detected */
    GF_TAMPER_HISTORY,              /* Historical tamper recorded */
    GF_TAMPER_SENSOR_FAULT,         /* Sensor malfunction */
    GF_TAMPER_ZEROIZED,             /* Keys already zeroized */
    GF_TAMPER_BATTERY_LOW,          /* Backup battery low */
    GF_TAMPER_SELF_TEST_FAIL        /* Self-test failure */
} gf_tamper_status_t;

/* Tamper event types */
typedef enum {
    GF_TAMPER_ENCLOSURE_OPEN,       /* Case/enclosure opened */
    GF_TAMPER_MESH_BREAK,           /* Anti-tamper mesh damaged */
    GF_TAMPER_VOLTAGE_GLITCH,       /* Power supply glitch attack */
    GF_TAMPER_VOLTAGE_HIGH,         /* Overvoltage detected */
    GF_TAMPER_VOLTAGE_LOW,          /* Undervoltage detected */
    GF_TAMPER_TEMPERATURE_HIGH,     /* Overtemperature attack */
    GF_TAMPER_TEMPERATURE_LOW,      /* Low temperature attack */
    GF_TAMPER_LIGHT_DETECTED,       /* Light intrusion (decapping) */
    GF_TAMPER_PROBE_DETECTED,       /* Probe/FIB attack detected */
    GF_TAMPER_CLOCK_GLITCH,         /* Clock manipulation */
    GF_TAMPER_EMI_ATTACK,           /* Electromagnetic attack */
    GF_TAMPER_MOTION                /* Unauthorized movement */
} gf_tamper_event_t;

/* Response actions */
typedef enum {
    GF_RESPONSE_LOG_ONLY,           /* Log event only */
    GF_RESPONSE_ALERT,              /* Generate alert */
    GF_RESPONSE_LOCK,               /* Lock device */
    GF_RESPONSE_ZEROIZE,            /* Zeroize sensitive data */
    GF_RESPONSE_SHUTDOWN,           /* Complete shutdown */
    GF_RESPONSE_DESTRUCT            /* Physical destruction (fuse blow) */
} gf_tamper_response_t;

/* Tamper sensor configuration */
typedef struct {
    bool enable_enclosure;          /* Enclosure switch monitoring */
    bool enable_mesh;               /* Anti-tamper mesh active */
    bool enable_voltage;            /* Voltage monitoring */
    bool enable_temperature;        /* Temperature monitoring */
    bool enable_light;              /* Light sensor active */
    bool enable_motion;             /* Motion/accelerometer active */
    int8_t temp_low_threshold_c;    /* Low temperature threshold */
    int8_t temp_high_threshold_c;   /* High temperature threshold */
    uint16_t voltage_low_mv;        /* Low voltage threshold */
    uint16_t voltage_high_mv;       /* High voltage threshold */
} gf_tamper_config_t;

/* Response configuration per event type */
typedef struct {
    gf_tamper_event_t event;        /* Event type */
    gf_tamper_response_t response;  /* Response action */
    uint16_t delay_ms;              /* Response delay (0 = immediate) */
    bool log_event;                 /* Log to secure storage */
    bool notify_host;               /* Notify host system */
} gf_tamper_response_config_t;

/* Tamper event record */
typedef struct {
    uint64_t timestamp;             /* Event timestamp */
    gf_tamper_event_t event;        /* Event type */
    gf_tamper_response_t response;  /* Response taken */
    uint16_t sensor_value;          /* Sensor reading at event */
    bool data_zeroized;             /* Data was zeroized */
    uint32_t sequence;              /* Event sequence number */
} gf_tamper_record_t;

/* System status */
typedef struct {
    gf_tamper_status_t status;      /* Current status */
    uint32_t event_count;           /* Total tamper events */
    uint64_t last_event_time;       /* Last event timestamp */
    gf_tamper_event_t last_event;   /* Last event type */
    uint8_t battery_percent;        /* Backup battery level */
    bool keys_present;              /* Keys still valid */
    bool device_locked;             /* Device locked state */
} gf_tamper_system_status_t;

/**
 * @brief Initialize tamper detection subsystem
 * @param config Sensor configuration
 * @return Status code
 */
gf_tamper_status_t gf_tamper_init(const gf_tamper_config_t* config);

/**
 * @brief Configure response for specific event type
 * @param response_config Response configuration
 * @return Status code
 */
gf_tamper_status_t gf_tamper_set_response(const gf_tamper_response_config_t* response_config);

/**
 * @brief Arm tamper detection (enable monitoring)
 * @return Status code
 */
gf_tamper_status_t gf_tamper_arm(void);

/**
 * @brief Disarm tamper detection (requires authentication)
 * @param auth_token Authentication token
 * @param token_length Token length
 * @return Status code
 */
gf_tamper_status_t gf_tamper_disarm(const uint8_t* auth_token, size_t token_length);

/**
 * @brief Check current tamper status
 * @param status Output for system status
 * @return Status code
 */
gf_tamper_status_t gf_tamper_get_status(gf_tamper_system_status_t* status);

/**
 * @brief Retrieve tamper event record
 * @param index Event index (0 = most recent)
 * @param record Output for event record
 * @return Status code
 */
gf_tamper_status_t gf_tamper_get_record(uint32_t index, gf_tamper_record_t* record);

/**
 * @brief Manually trigger zeroization
 * @return Status code
 */
gf_tamper_status_t gf_tamper_zeroize_now(void);

/**
 * @brief Perform sensor self-test
 * @return Status code (OK if passed)
 */
gf_tamper_status_t gf_tamper_self_test(void);

/**
 * @brief Shutdown tamper subsystem
 */
void gf_tamper_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TAMPER_RESPONSE_H */
