/**
 * @file health_telemetry.h
 * @brief Smart Textile Health Telemetry Module
 * 
 * INDUSTRY RELEVANCE:
 * Remote patient monitoring and telehealth using wearable textiles
 * enables continuous health tracking. Applications include cardiac
 * monitoring, fall detection for elderly, athletic performance, and
 * occupational health for firefighters and industrial workers.
 * 
 * STANDARDS:
 * - HL7 FHIR (Health data interoperability)
 * - IEEE 11073 (Personal health data)
 * - HIPAA (Privacy compliance)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TEXTILE_HEALTH_TELEMETRY_H
#define GF_TEXTILE_HEALTH_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    HEALTH_METRIC_HEART_RATE,
    HEALTH_METRIC_HRV,
    HEALTH_METRIC_RESPIRATION,
    HEALTH_METRIC_TEMPERATURE,
    HEALTH_METRIC_ACTIVITY,
    HEALTH_METRIC_STRESS,
    HEALTH_METRIC_HYDRATION,
    HEALTH_METRIC_POSTURE
} health_metric_t;

typedef struct {
    health_metric_t metric;
    float value;
    float confidence;
    uint64_t timestamp_us;
    bool alert_triggered;
} health_data_t;

int textile_health_init(void);
int textile_health_compute(health_metric_t metric, health_data_t *result);
int textile_health_get_telemetry(uint8_t *buffer, size_t max_len);
int textile_health_set_alert(health_metric_t metric, float low, float high);

#ifdef __cplusplus
}
#endif

#endif /* GF_TEXTILE_HEALTH_TELEMETRY_H */
