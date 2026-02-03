/**
 * @file public_safety.h
 * @brief Public Safety Telemetry Collector Interface
 * 
 * INDUSTRY RELEVANCE:
 * Public safety infrastructure enables rapid emergency response:
 * - ShotSpotter-style gunshot detection and localization
 * - Emergency call box and panic button networks
 * - Video analytics for crowd monitoring and incident detection
 * - First responder location tracking and dispatch
 * - Critical infrastructure protection (utilities, transportation)
 * - Natural disaster early warning and evacuation systems
 * 
 * This module demonstrates expertise in:
 * - Real-time event detection and classification
 * - Multi-sensor fusion for localization
 * - Priority-based alert routing
 * - Integration with CAD (Computer-Aided Dispatch) systems
 * - Compliance with public safety standards (NENA, APCO)
 */

#ifndef GF_PUBLIC_SAFETY_H
#define GF_PUBLIC_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

#define GF_PS_MAX_SENSORS           128
#define GF_PS_MAX_ALERTS            32
#define GF_PS_MAX_ZONES             16

typedef enum {
    GF_PS_OK = 0,
    GF_PS_ERROR_NOT_INITIALIZED,
    GF_PS_ERROR_SENSOR_OFFLINE,
    GF_PS_ERROR_ALERT_QUEUE_FULL,
    GF_PS_ERROR_DISPATCH_FAILED,
    GF_PS_ERROR_LOCALIZATION_FAILED
} gf_ps_status_t;

typedef enum {
    GF_PS_ALERT_GUNSHOT,
    GF_PS_ALERT_EXPLOSION,
    GF_PS_ALERT_GLASS_BREAK,
    GF_PS_ALERT_PANIC_BUTTON,
    GF_PS_ALERT_CROWD_ANOMALY,
    GF_PS_ALERT_FIRE_DETECTED,
    GF_PS_ALERT_FLOOD_WARNING,
    GF_PS_ALERT_SEISMIC,
    GF_PS_ALERT_GAS_LEAK,
    GF_PS_ALERT_SUSPICIOUS_ACTIVITY
} gf_ps_alert_type_t;

typedef enum {
    GF_PS_PRIORITY_ROUTINE = 0,
    GF_PS_PRIORITY_ELEVATED,
    GF_PS_PRIORITY_HIGH,
    GF_PS_PRIORITY_CRITICAL,
    GF_PS_PRIORITY_EMERGENCY
} gf_ps_priority_t;

typedef struct {
    float latitude;
    float longitude;
    float accuracy_m;
    uint8_t confidence;
} gf_ps_location_t;

typedef struct {
    uint32_t alert_id;
    gf_ps_alert_type_t type;
    gf_ps_priority_t priority;
    gf_ps_location_t location;
    uint32_t timestamp_ms;
    uint8_t sensor_count;           /* Confirming sensors */
    char description[64];
    bool acknowledged;
    bool dispatched;
} gf_ps_alert_t;

typedef struct {
    uint16_t sensor_id;
    gf_ps_location_t location;
    uint8_t sensor_types;           /* Bitmask of capabilities */
    bool online;
    uint8_t battery_pct;
    uint32_t last_heartbeat_ms;
} gf_ps_sensor_t;

typedef struct {
    uint16_t zone_count;
    uint16_t sensors_per_zone;
    uint16_t alert_timeout_ms;
    uint8_t min_confirming_sensors;
    bool auto_dispatch;
    char dispatch_endpoint[128];
} gf_ps_config_t;

gf_ps_status_t gf_ps_init(const gf_ps_config_t* config);
void gf_ps_shutdown(void);
gf_ps_status_t gf_ps_register_sensor(const gf_ps_sensor_t* sensor);
gf_ps_status_t gf_ps_report_event(uint16_t sensor_id, gf_ps_alert_type_t type, float amplitude);
gf_ps_status_t gf_ps_get_pending_alerts(gf_ps_alert_t* alerts, uint8_t* count, uint8_t max);
gf_ps_status_t gf_ps_acknowledge_alert(uint32_t alert_id);
gf_ps_status_t gf_ps_dispatch_alert(uint32_t alert_id, const char* unit_id);
gf_ps_status_t gf_ps_get_zone_status(uint8_t zone_id, uint16_t* sensors_online, uint16_t* alerts_active);
gf_ps_status_t gf_ps_localize_event(gf_ps_alert_type_t type, gf_ps_location_t* location);

#ifdef __cplusplus
}
#endif

#endif /* GF_PUBLIC_SAFETY_H */
