/**
 * @file emergency_telemetry.h
 * @brief Emergency Telemetry Collector for Disaster Systems
 * 
 * INDUSTRY RELEVANCE:
 * Emergency management systems require reliable, low-latency telemetry for
 * alerting populations and coordinating response. FEMA, local EMAs, and
 * private alert services need firmware that can deliver critical warnings
 * within seconds. Cell broadcast, sirens, and multi-channel alerts all
 * require embedded systems expertise.
 * 
 * This module provides telemetry collection, prioritization, and delivery
 * for emergency alert systems.
 * 
 * KEY CAPABILITIES:
 * - Priority message queuing
 * - Multi-channel delivery (CAP format)
 * - Geotargeted alerting
 * - Message redundancy
 * - Acknowledgment tracking
 * - Alert escalation
 * - Heartbeat monitoring
 * - Audit logging
 * 
 * STANDARDS COMPLIANCE:
 * - CAP 1.2 (Common Alerting Protocol)
 * - SAME (Specific Area Message Encoding)
 * - WEA (Wireless Emergency Alerts)
 * - IPAWS (Integrated Public Alert and Warning)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_EMERGENCY_TELEMETRY_H
#define GF_EMERGENCY_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define EM_MAX_ALERTS          32    /**< Active alert queue */
#define EM_MESSAGE_MAX_LEN     256   /**< Alert message length */
#define EM_MAX_RECIPIENTS      1024  /**< Geotarget recipients */
#define EM_HEARTBEAT_INTERVAL_S 10   /**< Heartbeat period */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Alert urgency (CAP) */
typedef enum {
    EM_URGENCY_UNKNOWN,
    EM_URGENCY_PAST,
    EM_URGENCY_FUTURE,
    EM_URGENCY_EXPECTED,
    EM_URGENCY_IMMEDIATE
} em_urgency_t;

/** Alert severity (CAP) */
typedef enum {
    EM_SEVERITY_UNKNOWN,
    EM_SEVERITY_MINOR,
    EM_SEVERITY_MODERATE,
    EM_SEVERITY_SEVERE,
    EM_SEVERITY_EXTREME
} em_severity_t;

/** Alert certainty (CAP) */
typedef enum {
    EM_CERTAINTY_UNKNOWN,
    EM_CERTAINTY_POSSIBLE,
    EM_CERTAINTY_LIKELY,
    EM_CERTAINTY_OBSERVED
} em_certainty_t;

/** Delivery channel */
typedef enum {
    EM_CHANNEL_SIREN,
    EM_CHANNEL_SMS,
    EM_CHANNEL_CELL_BROADCAST,
    EM_CHANNEL_RADIO,
    EM_CHANNEL_TV,
    EM_CHANNEL_APP_PUSH,
    EM_CHANNEL_SOCIAL
} em_channel_t;

/** Alert message */
typedef struct {
    uint32_t alert_id;
    char event_type[32];
    char headline[EM_MESSAGE_MAX_LEN];
    char description[EM_MESSAGE_MAX_LEN];
    em_urgency_t urgency;
    em_severity_t severity;
    em_certainty_t certainty;
    double area_lat;
    double area_lon;
    float area_radius_km;
    uint32_t effective_time;
    uint32_t expires_time;
} em_alert_t;

/** Delivery status */
typedef struct {
    uint32_t alert_id;
    em_channel_t channel;
    uint32_t sent_count;
    uint32_t ack_count;
    uint32_t fail_count;
    uint32_t send_time;
    bool completed;
} em_delivery_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize emergency telemetry
 * @param station_id Alerting authority ID
 * @return 0 on success
 */
int em_init(uint32_t station_id);

/**
 * @brief Queue emergency alert
 * @param alert Alert to send
 * @param channels Bitmask of channels
 * @return Alert ID on success, negative on error
 */
int32_t em_queue_alert(const em_alert_t* alert, uint8_t channels);

/**
 * @brief Cancel active alert
 * @param alert_id Alert to cancel
 * @return 0 on success
 */
int em_cancel_alert(uint32_t alert_id);

/**
 * @brief Get delivery status
 * @param alert_id Alert to query
 * @param status Output status
 * @return 0 on success
 */
int em_get_status(uint32_t alert_id, em_delivery_status_t* status);

/**
 * @brief Process telemetry (call periodically)
 * @param delta_ms Time since last call
 * @return 0 on success
 */
int em_process(uint32_t delta_ms);

/**
 * @brief Send heartbeat
 * @return 0 on success
 */
int em_send_heartbeat(void);

/**
 * @brief Shutdown telemetry
 */
void em_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_EMERGENCY_TELEMETRY_H */
