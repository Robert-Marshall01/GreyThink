/**
 * @file health.h
 * @brief System Health Monitor
 * 
 * WHAT: Continuous system health checking with heartbeats, self-tests,
 *       and anomaly detection for proactive fault management.
 * 
 * WHY: Health monitoring enables predictive maintenance and improves
 *      reliability. Understanding health checks and self-diagnostics
 *      is essential for mission-critical embedded systems.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Industrial: PLC health, motor diagnostics
 *   - Medical: Device self-tests
 *   - Automotive: ECU diagnostics
 *   - Infrastructure: Gateway monitoring
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Heartbeat/watchdog patterns
 *   - Self-test frameworks
 *   - Health aggregation
 *   - Anomaly detection
 *   - Recovery actions
 */

#ifndef GF_HEALTH_H
#define GF_HEALTH_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_HEALTH_MAX_CHECKS        16
#define GF_HEALTH_NAME_MAX          24

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_HEALTH_OK = 0,
    GF_HEALTH_DEGRADED,     /* Functioning but impaired */
    GF_HEALTH_UNHEALTHY,    /* Not functioning correctly */
    GF_HEALTH_CRITICAL      /* Requires immediate attention */
} gf_health_status_t;

typedef enum {
    GF_CHECK_HEARTBEAT = 0, /* Periodic liveness check */
    GF_CHECK_SELFTEST,      /* On-demand diagnostic */
    GF_CHECK_THRESHOLD,     /* Value within range */
    GF_CHECK_WATCHDOG       /* Must be fed periodically */
} gf_check_type_t;

typedef gf_health_status_t (*gf_health_check_fn)(void *ctx);

typedef struct {
    char                name[GF_HEALTH_NAME_MAX];
    gf_check_type_t     type;
    gf_health_check_fn  check_fn;
    void               *ctx;
    uint32_t            interval_ms;    /* Check frequency */
    uint32_t            timeout_ms;     /* For watchdog type */
} gf_health_check_t;

typedef uint32_t gf_health_handle_t;

typedef struct {
    gf_health_status_t  overall;
    uint8_t             ok_count;
    uint8_t             degraded_count;
    uint8_t             unhealthy_count;
    uint8_t             critical_count;
    uint32_t            total_checks;
    uint32_t            last_check_time;
    uint32_t            uptime_sec;
} gf_health_summary_t;

typedef void (*gf_health_callback)(gf_health_handle_t handle,
                                    gf_health_status_t status, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize health monitor
 */
int gf_health_init(void);

/**
 * @brief Register health check
 */
gf_health_handle_t gf_health_register(const gf_health_check_t *check);

/**
 * @brief Unregister health check
 */
int gf_health_unregister(gf_health_handle_t handle);

/**
 * @brief Feed watchdog (for watchdog-type checks)
 */
void gf_health_feed(gf_health_handle_t handle);

/**
 * @brief Run all checks now
 */
int gf_health_check_all(void);

/**
 * @brief Get check status
 */
gf_health_status_t gf_health_get_status(gf_health_handle_t handle);

/**
 * @brief Get overall health summary
 */
void gf_health_get_summary(gf_health_summary_t *summary);

/**
 * @brief Set status change callback
 */
void gf_health_set_callback(gf_health_callback callback, void *ctx);

/**
 * @brief Process health checks (call from main loop)
 */
void gf_health_process(void);

/**
 * @brief Export health status as JSON
 */
int gf_health_export_json(char *buffer, size_t buffer_size, size_t *written);

const void* gf_health_get_driver(void);

#endif /* GF_HEALTH_H */
