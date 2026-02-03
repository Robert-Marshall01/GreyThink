/**
 * @file reactor_telemetry.h
 * @brief Nuclear Reactor Health Telemetry
 * 
 * INDUSTRY RELEVANCE:
 * Plant data historians and monitoring systems track thousands of
 * parameters for performance optimization, regulatory compliance,
 * and predictive maintenance. Modern plants use OPC-UA and MQTT.
 * 
 * STANDARDS:
 * - IEC 62541 (OPC Unified Architecture)
 * - IEEE 1451 (Smart transducer interface)
 * - NRC RG 1.152 (Criteria for digital computers)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_REACTOR_TELEMETRY_H
#define GF_REACTOR_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    REACTOR_TELEM_POWER,
    REACTOR_TELEM_TEMPERATURE,
    REACTOR_TELEM_PRESSURE,
    REACTOR_TELEM_FLOW,
    REACTOR_TELEM_RADIATION,
    REACTOR_TELEM_SAFETY,
    REACTOR_TELEM_ALARM
} reactor_telem_type_t;

typedef struct {
    float power_mw;
    float power_percent;
    float rx_period;           /**< Reactor period (seconds) */
    float avg_temp_c;
    float pressure_mpa;
    float flow_kg_s;
    uint32_t alarms_active;
    bool limiting_condition;   /**< LCO violation */
} reactor_telem_t;

int reactor_telem_init(void);
int reactor_telem_get(reactor_telem_t *telem);
int reactor_telem_generate(uint8_t *buffer, size_t max_len);
int reactor_telem_log_alarm(uint16_t alarm_id, const char *description);

#ifdef __cplusplus
}
#endif

#endif /* GF_REACTOR_TELEMETRY_H */
