/**
 * @file thermal.h
 * @brief Thermal Monitoring and Management
 * 
 * WHAT: Temperature monitoring and thermal throttling to protect components
 *       from overheating while maximizing performance.
 * 
 * WHY: Thermal management is essential for reliability. Understanding
 *      thermal zones, throttling strategies, and hysteresis demonstrates
 *      systems-level thinking valued in consumer and industrial products.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Consumer: Smartphones, laptops, gaming devices
 *   - Industrial: Motor drives, power supplies
 *   - Automotive: ECUs, battery systems
 *   - Data centers: Server management
 */

#ifndef GF_THERMAL_H
#define GF_THERMAL_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_THERMAL_MAX_ZONES        8

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_THERMAL_ZONE_CPU = 0,
    GF_THERMAL_ZONE_GPU,
    GF_THERMAL_ZONE_BATTERY,
    GF_THERMAL_ZONE_AMBIENT,
    GF_THERMAL_ZONE_RADIO,
    GF_THERMAL_ZONE_CHARGER
} gf_thermal_zone_t;

typedef enum {
    GF_THERMAL_NORMAL = 0,      /* Below warning threshold */
    GF_THERMAL_WARNING,         /* Approaching limit */
    GF_THERMAL_THROTTLE,        /* Active throttling */
    GF_THERMAL_CRITICAL,        /* Emergency shutdown pending */
    GF_THERMAL_SHUTDOWN         /* Thermal shutdown initiated */
} gf_thermal_state_t;

typedef struct {
    int8_t              warning_c;
    int8_t              throttle_c;
    int8_t              critical_c;
    int8_t              shutdown_c;
    int8_t              hysteresis_c;
} gf_thermal_limits_t;

typedef struct {
    int8_t              temperature_c;
    gf_thermal_state_t  state;
    uint8_t             throttle_percent;   /* 0-100%, 100 = no throttle */
    uint32_t            time_in_throttle_s;
} gf_thermal_zone_status_t;

typedef void (*gf_thermal_callback)(gf_thermal_zone_t zone, 
                                     gf_thermal_state_t state, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

int gf_thermal_init(void);
int gf_thermal_configure_zone(gf_thermal_zone_t zone, 
                               const gf_thermal_limits_t *limits);
int8_t gf_thermal_get_temp(gf_thermal_zone_t zone);
void gf_thermal_get_zone_status(gf_thermal_zone_t zone, 
                                 gf_thermal_zone_status_t *status);
gf_thermal_state_t gf_thermal_get_state(gf_thermal_zone_t zone);
void gf_thermal_set_callback(gf_thermal_callback callback, void *ctx);
void gf_thermal_process(void);     /* Call periodically */
const void* gf_thermal_get_driver(void);

#endif /* GF_THERMAL_H */
