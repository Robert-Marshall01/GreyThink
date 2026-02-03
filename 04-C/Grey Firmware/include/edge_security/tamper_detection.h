/**
 * @file tamper_detection.h
 * @brief Physical Tamper Detection Interface
 *
 * INDUSTRY RELEVANCE:
 * Physical security is essential for payment terminals, utility meters,
 * and security devices. Tamper detection prevents physical attacks.
 * This module demonstrates:
 * - Multi-zone tamper mesh monitoring
 * - Environmental attack detection (temperature, voltage glitching)
 * - Zeroization on tamper (secure key destruction)
 * - FIPS 140-2/3 Level 3+ physical security requirements
 *
 * These skills apply to payment security (NCR, Verifone, Square),
 * hardware security modules (Thales, Utimaco), smart meters, and any
 * device requiring physical security certification.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires certified tamper-evident enclosures.
 */

#ifndef GF_TAMPER_DETECTION_H
#define GF_TAMPER_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_TAMPER_MAX_ZONES         8       /**< Maximum tamper zones */
#define GF_TAMPER_MAX_EVENTS        64      /**< Maximum event log entries */
#define GF_TAMPER_MESH_CHANNELS     4       /**< Tamper mesh channels */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Tamper event type
 */
typedef enum {
    GF_TAMPER_CASE_OPEN,        /**< Enclosure opened */
    GF_TAMPER_MESH_BREAK,       /**< Tamper mesh wire broken */
    GF_TAMPER_MESH_SHORT,       /**< Tamper mesh shorted */
    GF_TAMPER_VOLTAGE_GLITCH,   /**< Voltage glitch detected */
    GF_TAMPER_TEMP_HIGH,        /**< Temperature too high */
    GF_TAMPER_TEMP_LOW,         /**< Temperature too low */
    GF_TAMPER_LIGHT,            /**< Light sensor triggered */
    GF_TAMPER_VIBRATION,        /**< Vibration/drilling detected */
    GF_TAMPER_EMI,              /**< EMI/EMP attack detected */
    GF_TAMPER_CLOCK_FAULT,      /**< Clock manipulation detected */
    GF_TAMPER_POWER_LOSS        /**< Power interruption */
} gf_tamper_type_t;

/**
 * @brief Tamper response action
 */
typedef enum {
    GF_TAMPER_ACTION_LOG,       /**< Log event only */
    GF_TAMPER_ACTION_ALERT,     /**< Generate alert */
    GF_TAMPER_ACTION_LOCKOUT,   /**< Lock security functions */
    GF_TAMPER_ACTION_ZEROIZE    /**< Destroy all keys */
} gf_tamper_action_t;

/**
 * @brief Tamper zone configuration
 */
typedef struct {
    uint8_t zone_id;                    /**< Zone identifier */
    char name[32];                      /**< Zone name */
    gf_tamper_type_t types_enabled;     /**< Enabled tamper types (bitmask) */
    gf_tamper_action_t action;          /**< Response action */
    uint16_t debounce_ms;               /**< Debounce time */
    bool armed;                         /**< Zone armed */
} gf_tamper_zone_t;

/**
 * @brief Tamper mesh configuration
 */
typedef struct {
    bool enabled;                       /**< Mesh enabled */
    uint16_t resistance_nominal;        /**< Nominal resistance (ohms) */
    uint16_t resistance_threshold;      /**< Threshold for break detection */
    uint16_t short_threshold;           /**< Threshold for short detection */
    uint16_t check_interval_ms;         /**< Check interval */
} gf_tamper_mesh_config_t;

/**
 * @brief Environmental limits
 */
typedef struct {
    float temp_min_c;                   /**< Minimum allowed temperature */
    float temp_max_c;                   /**< Maximum allowed temperature */
    float voltage_min_v;                /**< Minimum allowed voltage */
    float voltage_max_v;                /**< Maximum allowed voltage */
    float voltage_glitch_threshold;     /**< Glitch detection threshold (V) */
    uint16_t glitch_window_us;          /**< Glitch detection window (µs) */
} gf_tamper_env_limits_t;

/**
 * @brief Tamper event record
 */
typedef struct {
    uint32_t event_id;                  /**< Event sequence number */
    gf_tamper_type_t type;              /**< Tamper type */
    uint8_t zone_id;                    /**< Zone that triggered */
    uint64_t timestamp_ms;              /**< Event timestamp */
    gf_tamper_action_t action_taken;    /**< Action taken */
    bool cleared;                       /**< Event cleared */
    union {
        float temperature_c;            /**< Temperature when triggered */
        float voltage_v;                /**< Voltage when triggered */
        uint16_t mesh_resistance;       /**< Mesh resistance when triggered */
        uint16_t light_level;           /**< Light level when triggered */
    } value;
} gf_tamper_event_t;

/**
 * @brief Tamper status
 */
typedef struct {
    bool armed;                         /**< System armed */
    bool tampered;                      /**< Active tamper condition */
    bool zeroized;                      /**< Keys have been destroyed */
    uint8_t active_zones;               /**< Number of active tamper zones */
    uint32_t total_events;              /**< Total tamper events */
    float current_temp_c;               /**< Current temperature */
    float current_voltage_v;            /**< Current voltage */
    uint16_t mesh_resistance[GF_TAMPER_MESH_CHANNELS]; /**< Mesh resistances */
    uint64_t last_event_time;           /**< Last tamper event timestamp */
} gf_tamper_status_t;

/**
 * @brief Zeroization status
 */
typedef struct {
    bool keys_destroyed;                /**< Cryptographic keys destroyed */
    bool ram_cleared;                   /**< Sensitive RAM cleared */
    bool flash_erased;                  /**< Secure flash erased */
    uint32_t zeroize_time_ms;           /**< Time when zeroization occurred */
    uint8_t zeroize_trigger;            /**< What triggered zeroization */
} gf_zeroize_status_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_tamper_callback_t)(const gf_tamper_event_t* event, void* user_data);
typedef void (*gf_zeroize_callback_t)(void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize tamper detection subsystem
 * @return 0 on success, negative error code on failure
 */
int gf_tamper_init(void);

/**
 * @brief Shutdown tamper detection
 */
void gf_tamper_deinit(void);

/**
 * @brief Configure tamper zone
 * @param zone Zone configuration
 * @return 0 on success
 */
int gf_tamper_configure_zone(const gf_tamper_zone_t* zone);

/**
 * @brief Configure tamper mesh
 * @param config Mesh configuration
 * @return 0 on success
 */
int gf_tamper_configure_mesh(const gf_tamper_mesh_config_t* config);

/**
 * @brief Set environmental limits
 * @param limits Environmental limits
 * @return 0 on success
 */
int gf_tamper_set_env_limits(const gf_tamper_env_limits_t* limits);

/**
 * @brief Arm tamper detection
 * @return 0 on success
 */
int gf_tamper_arm(void);

/**
 * @brief Disarm tamper detection
 * @param auth Authentication token
 * @param auth_len Token length
 * @return 0 on success
 */
int gf_tamper_disarm(const uint8_t* auth, uint8_t auth_len);

/**
 * @brief Check current tamper status
 * @param[out] status Status output
 */
void gf_tamper_get_status(gf_tamper_status_t* status);

/**
 * @brief Get tamper event from log
 * @param index Event index (0 = newest)
 * @param[out] event Event output
 * @return 0 on success, -1 if index out of range
 */
int gf_tamper_get_event(uint16_t index, gf_tamper_event_t* event);

/**
 * @brief Clear tamper event
 * @param event_id Event ID to clear
 * @param auth Authentication token
 * @param auth_len Token length
 * @return 0 on success
 */
int gf_tamper_clear_event(uint32_t event_id, const uint8_t* auth, uint8_t auth_len);

/**
 * @brief Manually trigger zeroization
 * @param auth Authentication token
 * @param auth_len Token length
 * @return 0 on success
 */
int gf_tamper_zeroize(const uint8_t* auth, uint8_t auth_len);

/**
 * @brief Get zeroization status
 * @param[out] status Status output
 */
void gf_tamper_get_zeroize_status(gf_zeroize_status_t* status);

/**
 * @brief Set tamper callback
 * @param callback Tamper event callback
 * @param user_data User context
 */
void gf_tamper_set_callback(gf_tamper_callback_t callback, void* user_data);

/**
 * @brief Set zeroization callback (called before keys destroyed)
 * @param callback Zeroize callback
 * @param user_data User context
 */
void gf_tamper_set_zeroize_callback(gf_zeroize_callback_t callback, void* user_data);

/**
 * @brief Run tamper self-test
 * @return 0 if all tests pass, negative error code on failure
 */
int gf_tamper_self_test(void);

/**
 * @brief Process tamper monitoring (call periodically)
 */
void gf_tamper_process(void);

/**
 * @brief Battery backup status (for power-loss tamper detection)
 * @return Battery voltage in mV, or 0 if no battery
 */
uint16_t gf_tamper_battery_status(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TAMPER_DETECTION_H */
