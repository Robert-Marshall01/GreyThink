/**
 * @file quantum_tamper.h
 * @brief Quantum-Enhanced Tamper Detection Module
 * 
 * @details
 * This module provides quantum-enhanced tamper detection capabilities
 * including quantum random number generation for challenge-response,
 * quantum sensing for physical intrusion, and PUF integration.
 * 
 * INDUSTRY RELEVANCE:
 * - Hardware security modules (HSM)
 * - Secure element tamper protection
 * - Military/defense systems
 * - Critical infrastructure protection
 * - Cryptocurrency hardware wallets
 * - Secure manufacturing
 * 
 * KEY TECHNOLOGIES:
 * - Quantum Random Number Generation (QRNG)
 * - Physical Unclonable Functions (PUF)
 * - Quantum sensing (magnetometry, gravimetry)
 * - Tamper-evident packaging
 * - Zeroization on tamper
 * - Secure boot chain verification
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_QUANTUM_TAMPER_H
#define GF_QUANTUM_TAMPER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define GF_QT_PUF_RESPONSE_SIZE     32    /**< PUF response size (bytes) */
#define GF_QT_CHALLENGE_SIZE        16    /**< Challenge size (bytes) */
#define GF_QT_SENSOR_CHANNELS       8     /**< Max sensor channels */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

typedef enum {
    GF_QT_OK = 0,
    GF_QT_ERROR_NOT_INITIALIZED,
    GF_QT_ERROR_NULL_PTR,
    GF_QT_ERROR_ENTROPY,
    GF_QT_ERROR_PUF_UNSTABLE,
    GF_QT_ERROR_SENSOR,
    GF_QT_ERROR_TAMPER_DETECTED,
    GF_QT_ERROR_ZEROIZED,
    GF_QT_WARN_ENTROPY_LOW,
    GF_QT_WARN_SENSOR_DRIFT
} gf_qt_status_t;

typedef enum {
    GF_QT_SENSOR_MESH,            /**< Tamper mesh/traces */
    GF_QT_SENSOR_TEMPERATURE,     /**< Temperature anomaly */
    GF_QT_SENSOR_VOLTAGE,         /**< Voltage glitch detection */
    GF_QT_SENSOR_CLOCK,           /**< Clock fault detection */
    GF_QT_SENSOR_LIGHT,           /**< Decap light detection */
    GF_QT_SENSOR_MAGNETIC,        /**< Magnetic field */
    GF_QT_SENSOR_ACCELERATION,    /**< Physical shock/motion */
    GF_QT_SENSOR_ENCLOSURE        /**< Enclosure open detect */
} gf_qt_sensor_type_t;

typedef enum {
    GF_QT_RESPONSE_LOG,           /**< Log event only */
    GF_QT_RESPONSE_ALERT,         /**< Generate alert */
    GF_QT_RESPONSE_LOCK,          /**< Lock device */
    GF_QT_RESPONSE_ZEROIZE        /**< Zeroize all keys */
} gf_qt_response_t;

typedef enum {
    GF_QT_STATE_NORMAL,
    GF_QT_STATE_ALERT,
    GF_QT_STATE_LOCKED,
    GF_QT_STATE_ZEROIZED
} gf_qt_state_t;

typedef struct {
    gf_qt_sensor_type_t type;     /**< Sensor type */
    bool enabled;                 /**< Sensor enabled */
    float threshold;              /**< Trigger threshold */
    gf_qt_response_t response;    /**< Response on trigger */
    uint32_t debounce_ms;         /**< Debounce time */
} gf_qt_sensor_config_t;

typedef struct {
    gf_qt_sensor_type_t type;     /**< Sensor type */
    float value;                  /**< Current reading */
    bool triggered;               /**< Trigger state */
    uint32_t trigger_count;       /**< Total triggers */
    uint64_t last_trigger_ms;     /**< Last trigger time */
} gf_qt_sensor_reading_t;

typedef struct {
    gf_qt_state_t state;          /**< Current security state */
    uint8_t active_sensors;       /**< Number of active sensors */
    uint8_t triggered_sensors;    /**< Sensors currently triggered */
    uint32_t total_tamper_events; /**< Total tamper events */
    uint64_t last_event_ms;       /**< Last tamper event */
    float entropy_quality;        /**< QRNG entropy quality (0-1) */
    bool puf_stable;              /**< PUF stability status */
} gf_qt_status_report_t;

typedef struct {
    uint8_t data[GF_QT_PUF_RESPONSE_SIZE];  /**< PUF response */
    uint8_t reliability_pct;                 /**< Response reliability */
    uint64_t timestamp_ms;                   /**< Generation time */
} gf_qt_puf_response_t;

typedef void (*gf_qt_tamper_cb_t)(gf_qt_sensor_type_t sensor, float value,
                                   gf_qt_response_t response, void* user_data);
typedef void (*gf_qt_state_cb_t)(gf_qt_state_t old_state, gf_qt_state_t new_state,
                                  void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_qt_status_t gf_qt_init(void);
void gf_qt_shutdown(void);
gf_qt_status_t gf_qt_configure_sensor(const gf_qt_sensor_config_t* config);
gf_qt_status_t gf_qt_get_sensor_reading(gf_qt_sensor_type_t sensor,
                                         gf_qt_sensor_reading_t* reading);
gf_qt_status_t gf_qt_get_random(uint8_t* buffer, uint16_t length);
gf_qt_status_t gf_qt_puf_challenge(const uint8_t* challenge,
                                    gf_qt_puf_response_t* response);
gf_qt_status_t gf_qt_puf_enroll(uint16_t challenge_id, const uint8_t* challenge);
gf_qt_status_t gf_qt_puf_verify(uint16_t challenge_id, const gf_qt_puf_response_t* response,
                                 bool* match);
gf_qt_status_t gf_qt_get_status(gf_qt_status_report_t* status);
gf_qt_status_t gf_qt_arm(void);
gf_qt_status_t gf_qt_disarm(const uint8_t* auth_token, uint16_t token_len);
gf_qt_status_t gf_qt_zeroize_now(void);
gf_qt_status_t gf_qt_register_tamper_callback(gf_qt_tamper_cb_t callback, void* user_data);
gf_qt_status_t gf_qt_register_state_callback(gf_qt_state_cb_t callback, void* user_data);
gf_qt_status_t gf_qt_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_QUANTUM_TAMPER_H */
