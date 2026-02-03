/**
 * @file metrics.h
 * @brief Runtime Metrics Collection
 * 
 * WHAT: Structured metrics collection for system health monitoring.
 *       Supports counters, gauges, histograms with efficient storage
 *       and export capabilities.
 * 
 * WHY: Observability is essential for production devices. Understanding
 *      metrics design, aggregation, and efficient collection demonstrates
 *      production-quality firmware development skills.
 * 
 * INDUSTRY APPLICATIONS:
 *   - All connected devices
 *   - Fleet monitoring
 *   - Predictive maintenance
 *   - SLA compliance
 *   - Performance optimization
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Metric types (counter, gauge, histogram)
 *   - Label/tag dimensions
 *   - Efficient aggregation
 *   - Export formats (Prometheus, OpenMetrics)
 */

#ifndef GF_METRICS_H
#define GF_METRICS_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_METRIC_NAME_MAX      32
#define GF_METRIC_LABEL_MAX     4
#define GF_METRIC_MAX_METRICS   32

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_METRIC_COUNTER = 0,      /* Monotonically increasing */
    GF_METRIC_GAUGE,            /* Point-in-time value */
    GF_METRIC_HISTOGRAM         /* Value distribution */
} gf_metric_type_t;

typedef struct {
    char    name[16];
    char    value[16];
} gf_metric_label_t;

typedef uint32_t gf_metric_handle_t;

typedef struct {
    char                name[GF_METRIC_NAME_MAX];
    gf_metric_type_t    type;
    const char         *description;
    uint8_t             label_count;
    gf_metric_label_t   labels[GF_METRIC_LABEL_MAX];
} gf_metric_def_t;

typedef struct {
    gf_metric_type_t    type;
    union {
        uint64_t        counter;
        int64_t         gauge;
        struct {
            uint64_t    count;
            int64_t     sum;
            uint64_t    buckets[8];     /* Configurable bucket boundaries */
        } histogram;
    } value;
} gf_metric_value_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize metrics collection
 */
int gf_metrics_init(void);

/**
 * @brief Register a new metric
 */
gf_metric_handle_t gf_metric_register(const gf_metric_def_t *def);

/**
 * @brief Increment counter
 */
void gf_metric_counter_inc(gf_metric_handle_t handle);
void gf_metric_counter_add(gf_metric_handle_t handle, uint64_t delta);

/**
 * @brief Set gauge value
 */
void gf_metric_gauge_set(gf_metric_handle_t handle, int64_t value);
void gf_metric_gauge_inc(gf_metric_handle_t handle);
void gf_metric_gauge_dec(gf_metric_handle_t handle);

/**
 * @brief Record histogram observation
 */
void gf_metric_histogram_observe(gf_metric_handle_t handle, int64_t value);

/**
 * @brief Get metric value
 */
int gf_metric_get(gf_metric_handle_t handle, gf_metric_value_t *value);

/**
 * @brief Export all metrics in Prometheus format
 */
int gf_metrics_export_prometheus(char *buffer, size_t buffer_size, size_t *written);

/**
 * @brief Reset all metrics
 */
void gf_metrics_reset(void);

const void* gf_metrics_get_driver(void);

#endif /* GF_METRICS_H */
