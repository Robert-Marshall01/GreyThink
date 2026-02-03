/**
 * @file reactor_sensor.h
 * @brief Nuclear Reactor Sensor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Nuclear power provides ~20% of US electricity and is expanding
 * globally for clean energy. Next-gen Small Modular Reactors (SMRs)
 * from NuScale, TerraPower, and X-energy require modern digital I&C.
 * Reactor instrumentation is safety-critical (NQA-1, IEEE 603).
 * 
 * This stub demonstrates:
 * - Neutron flux monitoring
 * - Temperature/pressure sensing
 * - Coolant flow measurement
 * - Radiation monitoring
 * 
 * STANDARDS:
 * - IEEE 603 (Nuclear safety systems)
 * - NQA-1 (Quality assurance)
 * - 10 CFR 50 Appendix B
 * - IEC 61513 (Nuclear I&C)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_REACTOR_SENSOR_H
#define GF_REACTOR_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef enum {
    REACTOR_SENSOR_NEUTRON_FLUX,
    REACTOR_SENSOR_TEMPERATURE,
    REACTOR_SENSOR_PRESSURE,
    REACTOR_SENSOR_COOLANT_FLOW,
    REACTOR_SENSOR_LEVEL,
    REACTOR_SENSOR_RADIATION,
    REACTOR_SENSOR_CONTAINMENT
} reactor_sensor_type_t;

typedef enum {
    REACTOR_DIV_A,
    REACTOR_DIV_B,
    REACTOR_DIV_C,
    REACTOR_DIV_D
} reactor_division_t;

typedef struct {
    reactor_sensor_type_t type;
    reactor_division_t division;
    uint8_t channel;
    float value;
    float uncertainty;
    bool valid;
    bool bypassed;
    uint64_t timestamp_us;
} reactor_reading_t;

typedef struct {
    reactor_sensor_type_t type;
    reactor_division_t division;
    float cal_gain;
    float cal_offset;
    float trip_setpoint;
    float warning_setpoint;
} reactor_sensor_config_t;

int reactor_sensor_init(void);
int reactor_sensor_configure(uint8_t sensor_id, const reactor_sensor_config_t *config);
int reactor_sensor_read(uint8_t sensor_id, reactor_reading_t *reading);
int reactor_sensor_self_test(uint8_t sensor_id);
bool reactor_sensor_is_valid(uint8_t sensor_id);

#ifdef __cplusplus
}
#endif

#endif /* GF_REACTOR_SENSOR_H */
