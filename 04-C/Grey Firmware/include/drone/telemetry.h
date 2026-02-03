/**
 * @file telemetry.h
 * @brief Drone Remote Telemetry Uplink Interface
 *
 * INDUSTRY RELEVANCE:
 * Remote telemetry enables ground control, fleet management, and regulatory
 * compliance for commercial drone operations. This module demonstrates:
 * - MAVLink protocol implementation (industry standard)
 * - Long-range communication (LTE/4G, LoRa, satellite)
 * - Telemetry prioritization and bandwidth management
 * - Remote ID compliance (FAA/EASA requirements)
 *
 * These skills apply to drone fleet operators (Wing, Zipline), ground control
 * software (DroneLink, UgCS), and UTM systems for drone airspace management.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production requires certified radio implementations.
 */

#ifndef GF_DRONE_TELEMETRY_H
#define GF_DRONE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_TELEM_MAX_PACKET_SIZE    280     /**< Maximum packet size (MAVLink v2) */
#define GF_TELEM_MAX_CHANNELS       4       /**< Maximum simultaneous channels */
#define GF_TELEM_COMMAND_TIMEOUT_MS 5000    /**< Command acknowledgment timeout */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Telemetry link type
 */
typedef enum {
    GF_TELEM_LINK_SERIAL,       /**< Serial (USB/UART) */
    GF_TELEM_LINK_WIFI,         /**< WiFi (802.11) */
    GF_TELEM_LINK_LTE,          /**< Cellular LTE */
    GF_TELEM_LINK_LORA,         /**< Long-range LoRa */
    GF_TELEM_LINK_SATCOM,       /**< Satellite (Iridium, etc.) */
    GF_TELEM_LINK_SBUS          /**< SBUS RC link */
} gf_telem_link_t;

/**
 * @brief Message priority
 */
typedef enum {
    GF_TELEM_PRIORITY_CRITICAL, /**< Safety-critical (always send) */
    GF_TELEM_PRIORITY_HIGH,     /**< Important status updates */
    GF_TELEM_PRIORITY_NORMAL,   /**< Regular telemetry */
    GF_TELEM_PRIORITY_LOW       /**< Non-essential data */
} gf_telem_priority_t;

/**
 * @brief Telemetry message types
 */
typedef enum {
    GF_TELEM_MSG_HEARTBEAT,     /**< System heartbeat */
    GF_TELEM_MSG_ATTITUDE,      /**< Attitude quaternion/euler */
    GF_TELEM_MSG_POSITION,      /**< GPS position */
    GF_TELEM_MSG_VELOCITY,      /**< Velocity vector */
    GF_TELEM_MSG_BATTERY,       /**< Battery status */
    GF_TELEM_MSG_RC_CHANNELS,   /**< RC channel values */
    GF_TELEM_MSG_MISSION_ITEM,  /**< Mission waypoint */
    GF_TELEM_MSG_COMMAND_ACK,   /**< Command acknowledgment */
    GF_TELEM_MSG_PARAM_VALUE,   /**< Parameter value */
    GF_TELEM_MSG_STATUSTEXT,    /**< Text status message */
    GF_TELEM_MSG_REMOTE_ID      /**< Remote ID broadcast */
} gf_telem_msg_type_t;

/**
 * @brief Link statistics
 */
typedef struct {
    gf_telem_link_t type;       /**< Link type */
    bool connected;             /**< Link connected */
    int8_t rssi_dbm;            /**< Signal strength */
    int8_t noise_dbm;           /**< Noise floor */
    float packet_loss;          /**< Packet loss rate (0-1) */
    uint32_t tx_packets;        /**< Transmitted packets */
    uint32_t rx_packets;        /**< Received packets */
    uint32_t errors;            /**< Link errors */
    uint32_t latency_ms;        /**< Round-trip latency */
} gf_telem_link_stats_t;

/**
 * @brief Remote ID data (FAA compliance)
 */
typedef struct {
    char uas_id[24];            /**< UAS serial number */
    char operator_id[24];       /**< Operator ID */
    double latitude;            /**< Current latitude */
    double longitude;           /**< Current longitude */
    float altitude_geo;         /**< Geodetic altitude */
    float altitude_baro;        /**< Pressure altitude */
    float height_agl;           /**< Height above ground */
    float speed_h;              /**< Horizontal speed (m/s) */
    float speed_v;              /**< Vertical speed (m/s) */
    float heading;              /**< Track direction */
    uint32_t timestamp;         /**< Unix timestamp */
    uint8_t emergency_status;   /**< Emergency code */
} gf_remote_id_t;

/**
 * @brief Attitude telemetry
 */
typedef struct {
    float roll;                 /**< Roll (radians) */
    float pitch;                /**< Pitch (radians) */
    float yaw;                  /**< Yaw (radians) */
    float rollspeed;            /**< Roll rate (rad/s) */
    float pitchspeed;           /**< Pitch rate (rad/s) */
    float yawspeed;             /**< Yaw rate (rad/s) */
} gf_telem_attitude_t;

/**
 * @brief Position telemetry
 */
typedef struct {
    int32_t lat;                /**< Latitude (degE7) */
    int32_t lon;                /**< Longitude (degE7) */
    int32_t alt;                /**< Altitude MSL (mm) */
    int32_t alt_rel;            /**< Altitude relative (mm) */
    int16_t vx;                 /**< X velocity (cm/s) */
    int16_t vy;                 /**< Y velocity (cm/s) */
    int16_t vz;                 /**< Z velocity (cm/s) */
    uint16_t hdg;               /**< Heading (cdeg) */
} gf_telem_position_t;

/**
 * @brief Battery telemetry
 */
typedef struct {
    uint16_t voltage_mv;        /**< Voltage (mV) */
    int16_t current_ca;         /**< Current (cA, negative = discharging) */
    uint8_t remaining;          /**< Remaining percentage */
    int32_t consumed_mah;       /**< Consumed capacity (mAh) */
    int16_t temperature_c;      /**< Temperature (centi-degrees) */
    uint8_t status;             /**< Battery status flags */
} gf_telem_battery_t;

/**
 * @brief Command from ground control
 */
typedef struct {
    uint16_t command;           /**< MAVLink command ID */
    uint8_t target_system;      /**< Target system */
    uint8_t target_component;   /**< Target component */
    float params[7];            /**< Command parameters */
} gf_telem_command_t;

/**
 * @brief Telemetry configuration
 */
typedef struct {
    uint8_t system_id;          /**< MAVLink system ID */
    uint8_t component_id;       /**< MAVLink component ID */
    uint16_t heartbeat_rate_ms; /**< Heartbeat interval */
    uint16_t attitude_rate_ms;  /**< Attitude update interval */
    uint16_t position_rate_ms;  /**< Position update interval */
    bool remote_id_enabled;     /**< Enable Remote ID */
    uint16_t remote_id_rate_ms; /**< Remote ID broadcast interval */
} gf_telem_config_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * Callbacks
 * ───────────────────────────────────────────────────────────────────────────── */

typedef void (*gf_telem_cmd_cb_t)(const gf_telem_command_t* cmd, void* user_data);
typedef void (*gf_telem_link_cb_t)(gf_telem_link_t link, bool connected, void* user_data);

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize telemetry system
 * @param config Telemetry configuration
 * @return 0 on success, negative error code on failure
 */
int gf_telem_init(const gf_telem_config_t* config);

/**
 * @brief Shutdown telemetry system
 */
void gf_telem_deinit(void);

/**
 * @brief Add telemetry link
 * @param type Link type
 * @param config Link-specific configuration
 * @return Link handle, or negative error code
 */
int gf_telem_add_link(gf_telem_link_t type, const void* config);

/**
 * @brief Remove telemetry link
 * @param link_handle Link handle from add_link
 */
void gf_telem_remove_link(int link_handle);

/**
 * @brief Send attitude telemetry
 * @param attitude Attitude data
 */
void gf_telem_send_attitude(const gf_telem_attitude_t* attitude);

/**
 * @brief Send position telemetry
 * @param position Position data
 */
void gf_telem_send_position(const gf_telem_position_t* position);

/**
 * @brief Send battery telemetry
 * @param battery Battery data
 */
void gf_telem_send_battery(const gf_telem_battery_t* battery);

/**
 * @brief Send status text
 * @param severity Message severity (0-7)
 * @param text Status text
 */
void gf_telem_send_statustext(uint8_t severity, const char* text);

/**
 * @brief Send Remote ID broadcast
 * @param rid Remote ID data
 */
void gf_telem_send_remote_id(const gf_remote_id_t* rid);

/**
 * @brief Set command callback
 * @param callback Command received callback
 * @param user_data User context
 */
void gf_telem_set_command_callback(gf_telem_cmd_cb_t callback, void* user_data);

/**
 * @brief Set link status callback
 * @param callback Link status change callback
 * @param user_data User context
 */
void gf_telem_set_link_callback(gf_telem_link_cb_t callback, void* user_data);

/**
 * @brief Get link statistics
 * @param link_handle Link handle
 * @param[out] stats Output statistics
 * @return 0 on success
 */
int gf_telem_get_link_stats(int link_handle, gf_telem_link_stats_t* stats);

/**
 * @brief Process telemetry (call periodically)
 */
void gf_telem_process(void);

/**
 * @brief Send mavlink heartbeat
 */
void gf_telem_heartbeat(void);

/**
 * @brief Request parameter from ground control
 * @param param_id Parameter name
 */
void gf_telem_request_param(const char* param_id);

#ifdef __cplusplus
}
#endif

#endif /* GF_DRONE_TELEMETRY_H */
