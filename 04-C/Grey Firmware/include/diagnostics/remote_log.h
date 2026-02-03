/**
 * @file remote_log.h
 * @brief Remote Logging and Telemetry
 * 
 * WHAT: Efficient log collection and transmission to remote endpoints.
 *       Supports buffering, compression, and multiple transport protocols.
 * 
 * WHY: Remote logging is essential for fleet management and debugging
 *      deployed devices. Understanding log aggregation, filtering, and
 *      bandwidth management demonstrates production firmware skills.
 * 
 * INDUSTRY APPLICATIONS:
 *   - Fleet monitoring
 *   - Remote debugging
 *   - Compliance logging
 *   - Security audit trails
 * 
 * KEY CONCEPTS DEMONSTRATED:
 *   - Log buffering and batching
 *   - Priority-based filtering
 *   - Bandwidth management
 *   - Reliable delivery
 */

#ifndef GF_REMOTE_LOG_H
#define GF_REMOTE_LOG_H

#include <stdint.h>
#include <stddef.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_RLOG_MSG_MAX_LEN         256
#define GF_RLOG_BUFFER_SIZE         4096
#define GF_RLOG_BATCH_MAX           32

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_RLOG_TRANSPORT_MQTT = 0,
    GF_RLOG_TRANSPORT_HTTP,
    GF_RLOG_TRANSPORT_UDP,
    GF_RLOG_TRANSPORT_TCP
} gf_rlog_transport_t;

typedef enum {
    GF_RLOG_LEVEL_TRACE = 0,
    GF_RLOG_LEVEL_DEBUG,
    GF_RLOG_LEVEL_INFO,
    GF_RLOG_LEVEL_WARN,
    GF_RLOG_LEVEL_ERROR,
    GF_RLOG_LEVEL_FATAL
} gf_rlog_level_t;

typedef struct {
    gf_rlog_transport_t transport;
    const char         *endpoint;       /* URL or MQTT topic */
    uint16_t            port;
    uint16_t            batch_size;     /* Logs per transmission */
    uint32_t            flush_interval_ms;
    gf_rlog_level_t     min_level;      /* Filter below this level */
    bool                compress;       /* Enable compression */
    bool                reliable;       /* Require ACK */
} gf_rlog_config_t;

typedef struct {
    uint32_t            logs_buffered;
    uint32_t            logs_sent;
    uint32_t            logs_dropped;
    uint32_t            bytes_sent;
    uint32_t            send_errors;
    uint32_t            last_send_time;
    bool                connected;
} gf_rlog_stats_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize remote logging
 */
int gf_rlog_init(const gf_rlog_config_t *config);

/**
 * @brief Log message to remote
 */
void gf_rlog(gf_rlog_level_t level, const char *module, const char *fmt, ...);

/**
 * @brief Log with structured data
 */
void gf_rlog_structured(gf_rlog_level_t level,
                        const char *module,
                        const char *message,
                        const char *json_data);

/**
 * @brief Force flush buffered logs
 */
int gf_rlog_flush(void);

/**
 * @brief Set minimum log level
 */
void gf_rlog_set_level(gf_rlog_level_t level);

/**
 * @brief Get statistics
 */
void gf_rlog_get_stats(gf_rlog_stats_t *stats);

/**
 * @brief Process pending transmissions (call from main loop)
 */
void gf_rlog_process(void);

const void* gf_rlog_get_driver(void);

#endif /* GF_REMOTE_LOG_H */
