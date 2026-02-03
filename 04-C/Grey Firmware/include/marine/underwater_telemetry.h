/**
 * @file underwater_telemetry.h
 * @brief Underwater Telemetry Module Interface
 * 
 * INDUSTRY RELEVANCE:
 * Underwater telemetry enables real-time monitoring and communication in:
 * - Autonomous Underwater Vehicles (AUVs) for mission data relay
 * - Remotely Operated Vehicles (ROVs) for live operator feedback
 * - Oceanographic buoys and moorings for continuous environmental data
 * - Subsea oil/gas infrastructure for pipeline and wellhead monitoring
 * - Marine research for biological and geological data collection
 * - Naval operations for submarine communication and situational awareness
 * 
 * This module demonstrates expertise in:
 * - Acoustic modem communication protocols
 * - Data compression for bandwidth-limited channels
 * - Store-and-forward for intermittent connectivity
 * - Multi-sensor data aggregation and timestamping
 * - Protocol design for high-latency, low-bandwidth environments
 * 
 * Challenges addressed:
 * - Limited bandwidth (typically 1-10 kbps acoustic)
 * - High latency (seconds for acoustic propagation)
 * - Multipath interference and signal attenuation
 * - Power constraints in battery-operated systems
 */

#ifndef GF_UNDERWATER_TELEMETRY_H
#define GF_UNDERWATER_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ===== Configuration ===== */

#define GF_UWTELEM_MAX_PACKET_SIZE      256     /* Max packet size in bytes */
#define GF_UWTELEM_MAX_SENSORS          16      /* Max sensors to aggregate */
#define GF_UWTELEM_BUFFER_SIZE          4096    /* Telemetry buffer size */
#define GF_UWTELEM_MAX_QUEUE_DEPTH      32      /* Max queued packets */

/* ===== Status Codes ===== */

typedef enum {
    GF_UWTELEM_OK = 0,
    GF_UWTELEM_ERROR_NOT_INITIALIZED,
    GF_UWTELEM_ERROR_BUFFER_FULL,
    GF_UWTELEM_ERROR_INVALID_PARAM,
    GF_UWTELEM_ERROR_TX_FAILED,
    GF_UWTELEM_ERROR_NO_ACK,
    GF_UWTELEM_ERROR_TIMEOUT,
    GF_UWTELEM_ERROR_CHANNEL_BUSY
} gf_uwtelem_status_t;

/* ===== Telemetry Types ===== */

typedef enum {
    GF_UWTELEM_CHANNEL_ACOUSTIC,    /* Acoustic modem */
    GF_UWTELEM_CHANNEL_RF,          /* Surface RF (when surfaced) */
    GF_UWTELEM_CHANNEL_OPTICAL,     /* Optical modem (short range) */
    GF_UWTELEM_CHANNEL_TETHER       /* Tethered connection */
} gf_uwtelem_channel_t;

typedef enum {
    GF_UWTELEM_PRIORITY_LOW = 0,
    GF_UWTELEM_PRIORITY_NORMAL,
    GF_UWTELEM_PRIORITY_HIGH,
    GF_UWTELEM_PRIORITY_CRITICAL    /* Emergency/safety data */
} gf_uwtelem_priority_t;

/* ===== Data Structures ===== */

/**
 * @brief Sensor reading for telemetry
 */
typedef struct {
    uint8_t sensor_id;
    uint8_t sensor_type;
    float value;
    uint8_t quality;            /* Data quality (0-100) */
    uint32_t timestamp_ms;
} gf_uwtelem_sensor_t;

/**
 * @brief Navigation data packet
 */
typedef struct {
    float latitude;             /* Degrees */
    float longitude;            /* Degrees */
    float depth_m;              /* Depth in meters */
    float heading_deg;          /* Heading in degrees */
    float pitch_deg;            /* Pitch in degrees */
    float roll_deg;             /* Roll in degrees */
    float velocity_mps;         /* Velocity in m/s */
    uint32_t timestamp_ms;
} gf_uwtelem_nav_t;

/**
 * @brief Telemetry packet
 */
typedef struct {
    uint16_t packet_id;
    gf_uwtelem_priority_t priority;
    uint8_t sensor_count;
    gf_uwtelem_sensor_t sensors[GF_UWTELEM_MAX_SENSORS];
    gf_uwtelem_nav_t nav;
    uint32_t mission_time_s;
    uint8_t battery_pct;
    uint8_t system_status;
} gf_uwtelem_packet_t;

/**
 * @brief Telemetry configuration
 */
typedef struct {
    gf_uwtelem_channel_t primary_channel;
    gf_uwtelem_channel_t backup_channel;
    uint16_t report_interval_ms;
    uint8_t compression_level;      /* 0=none, 1-9=compression */
    bool store_and_forward;         /* Buffer when no link */
    bool acknowledge_required;      /* Require ACKs */
    uint8_t retry_count;
    uint16_t timeout_ms;
} gf_uwtelem_config_t;

/**
 * @brief Telemetry statistics
 */
typedef struct {
    uint32_t packets_sent;
    uint32_t packets_acked;
    uint32_t packets_failed;
    uint32_t bytes_sent;
    uint32_t retries;
    float avg_latency_ms;
    float link_quality;             /* 0.0 - 1.0 */
} gf_uwtelem_stats_t;

/* ===== API Functions ===== */

/**
 * @brief Initialize telemetry system
 */
gf_uwtelem_status_t gf_uwtelem_init(const gf_uwtelem_config_t* config);

/**
 * @brief Shutdown telemetry system
 */
void gf_uwtelem_shutdown(void);

/**
 * @brief Queue sensor data for transmission
 */
gf_uwtelem_status_t gf_uwtelem_queue_sensor(const gf_uwtelem_sensor_t* sensor);

/**
 * @brief Queue navigation data for transmission
 */
gf_uwtelem_status_t gf_uwtelem_queue_nav(const gf_uwtelem_nav_t* nav);

/**
 * @brief Send telemetry packet immediately
 */
gf_uwtelem_status_t gf_uwtelem_send(const gf_uwtelem_packet_t* packet);

/**
 * @brief Process telemetry queue (call periodically)
 */
gf_uwtelem_status_t gf_uwtelem_process(void);

/**
 * @brief Get telemetry statistics
 */
gf_uwtelem_status_t gf_uwtelem_get_stats(gf_uwtelem_stats_t* stats);

/**
 * @brief Set communication channel
 */
gf_uwtelem_status_t gf_uwtelem_set_channel(gf_uwtelem_channel_t channel);

/**
 * @brief Check link status
 */
bool gf_uwtelem_link_available(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_UNDERWATER_TELEMETRY_H */
