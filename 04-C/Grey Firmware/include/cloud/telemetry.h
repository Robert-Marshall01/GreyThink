/**
 * @file telemetry.h
 * @brief Cloud Telemetry Uploader
 * 
 * INDUSTRY RELEVANCE:
 * Telemetry is the foundation of IoT value - devices send sensor data, status,
 * and events to cloud platforms for analytics, alerting, and visualization.
 * AWS IoT Core, Azure IoT Hub, Google Cloud IoT, and countless startups handle
 * billions of telemetry messages daily. The market for IoT data analytics
 * exceeds $20B annually.
 * 
 * WHY THIS MATTERS:
 * - Efficient data serialization (JSON, CBOR, Protobuf)
 * - Batching for network efficiency
 * - Compression for bandwidth savings
 * - Offline buffering for connectivity gaps
 * - Rate limiting and backoff
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Telemetry data modeling
 * - Batching and compression
 * - Offline queue management
 * - Upload scheduling
 */

#ifndef GF_TELEMETRY_H
#define GF_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_TELEM_MAX_POINTS         256     /* Maximum points in batch */
#define GF_TELEM_MAX_TAGS           8       /* Tags per data point */
#define GF_TELEM_QUEUE_SIZE         1024    /* Offline queue size */
#define GF_TELEM_TAG_NAME_LEN       24      /* Tag name length */
#define GF_TELEM_TAG_VALUE_LEN      32      /* Tag value length */
#define GF_TELEM_METRIC_NAME_LEN    48      /* Metric name length */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Data point value type */
typedef enum {
    GF_TELEM_TYPE_INT = 0,          /* Integer value */
    GF_TELEM_TYPE_FLOAT,            /* Floating point */
    GF_TELEM_TYPE_BOOL,             /* Boolean */
    GF_TELEM_TYPE_STRING,           /* String value */
    GF_TELEM_TYPE_BINARY            /* Binary blob */
} gf_telem_type_t;

/* Serialization format */
typedef enum {
    GF_TELEM_FORMAT_JSON = 0,       /* JSON (human-readable) */
    GF_TELEM_FORMAT_CBOR,           /* CBOR (compact binary) */
    GF_TELEM_FORMAT_PROTOBUF,       /* Protocol Buffers */
    GF_TELEM_FORMAT_MSGPACK,        /* MessagePack */
    GF_TELEM_FORMAT_LINE_PROTOCOL   /* InfluxDB line protocol */
} gf_telem_format_t;

/* Compression type */
typedef enum {
    GF_TELEM_COMPRESS_NONE = 0,
    GF_TELEM_COMPRESS_GZIP,
    GF_TELEM_COMPRESS_LZ4,
    GF_TELEM_COMPRESS_ZSTD
} gf_telem_compress_t;

/* Connection state */
typedef enum {
    GF_TELEM_STATE_OFFLINE = 0,
    GF_TELEM_STATE_CONNECTING,
    GF_TELEM_STATE_ONLINE,
    GF_TELEM_STATE_ERROR
} gf_telem_state_t;

/* Quality of service */
typedef enum {
    GF_TELEM_QOS_AT_MOST_ONCE = 0,  /* Fire and forget */
    GF_TELEM_QOS_AT_LEAST_ONCE,     /* Retry until ack */
    GF_TELEM_QOS_EXACTLY_ONCE       /* Deduplication */
} gf_telem_qos_t;

/* Tag (metadata key-value) */
typedef struct {
    char    name[GF_TELEM_TAG_NAME_LEN];
    char    value[GF_TELEM_TAG_VALUE_LEN];
} gf_telem_tag_t;

/* Data point */
typedef struct {
    char                metric[GF_TELEM_METRIC_NAME_LEN];
    gf_telem_type_t     type;
    union {
        int64_t         i;
        double          f;
        bool            b;
        char            s[64];
        struct {
            uint8_t    *data;
            uint16_t    len;
        }               bin;
    } value;
    gf_telem_tag_t      tags[GF_TELEM_MAX_TAGS];
    uint8_t             tag_count;
    uint64_t            timestamp_ms;       /* Unix timestamp in ms */
} gf_telem_point_t;

/* Batch of data points */
typedef struct {
    gf_telem_point_t    points[GF_TELEM_MAX_POINTS];
    int                 count;
    gf_telem_tag_t      common_tags[GF_TELEM_MAX_TAGS]; /* Applied to all */
    uint8_t             common_tag_count;
} gf_telem_batch_t;

/* Client configuration */
typedef struct {
    char                endpoint[256];      /* Cloud endpoint URL */
    char                device_id[64];      /* Device identifier */
    char                auth_token[256];    /* Authentication token */
    gf_telem_format_t   format;             /* Serialization format */
    gf_telem_compress_t compression;        /* Compression type */
    gf_telem_qos_t      default_qos;        /* Default QoS level */
    uint16_t            batch_size;         /* Points per batch upload */
    uint16_t            batch_interval_ms;  /* Max time before flush */
    uint16_t            retry_interval_ms;  /* Retry delay on failure */
    uint8_t             max_retries;        /* Maximum upload retries */
} gf_telem_config_t;

/* Statistics */
typedef struct {
    uint32_t            points_sent;
    uint32_t            points_queued;
    uint32_t            points_dropped;
    uint32_t            batches_sent;
    uint32_t            bytes_sent;
    uint32_t            bytes_compressed;
    uint32_t            errors;
    uint32_t            retries;
    uint16_t            queue_fill_pct;
    uint16_t            avg_latency_ms;
} gf_telem_stats_t;

/* Callback for upload completion */
typedef void (*gf_telem_callback_t)(bool success, int points_sent, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize telemetry client
 * @param config Client configuration
 */
int gf_telem_init(const gf_telem_config_t *config);

/**
 * @brief Start telemetry client
 */
int gf_telem_start(void);

/**
 * @brief Stop telemetry client
 */
int gf_telem_stop(void);

/**
 * @brief Get connection state
 */
gf_telem_state_t gf_telem_get_state(void);

/**
 * @brief Record a data point
 * @param metric Metric name
 * @param value Integer value
 */
int gf_telem_record_int(const char *metric, int64_t value);

/**
 * @brief Record float value
 */
int gf_telem_record_float(const char *metric, double value);

/**
 * @brief Record boolean value
 */
int gf_telem_record_bool(const char *metric, bool value);

/**
 * @brief Record string value
 */
int gf_telem_record_string(const char *metric, const char *value);

/**
 * @brief Record data point with tags
 */
int gf_telem_record(const gf_telem_point_t *point);

/**
 * @brief Record batch of points
 */
int gf_telem_record_batch(const gf_telem_batch_t *batch);

/**
 * @brief Add common tag to all future points
 */
int gf_telem_add_common_tag(const char *name, const char *value);

/**
 * @brief Remove common tag
 */
int gf_telem_remove_common_tag(const char *name);

/**
 * @brief Force flush pending data
 */
int gf_telem_flush(void);

/**
 * @brief Flush synchronously (blocking)
 * @param timeout_ms Maximum wait time
 */
int gf_telem_flush_sync(uint32_t timeout_ms);

/**
 * @brief Register completion callback
 */
int gf_telem_register_callback(gf_telem_callback_t callback, void *ctx);

/**
 * @brief Get queue depth
 * @return Number of points waiting to send
 */
int gf_telem_queue_depth(void);

/**
 * @brief Clear offline queue (discard unsent data)
 */
int gf_telem_clear_queue(void);

/**
 * @brief Get statistics
 */
int gf_telem_get_stats(gf_telem_stats_t *stats);

/**
 * @brief Reset statistics
 */
int gf_telem_reset_stats(void);

/**
 * @brief Set rate limit (points per second)
 */
int gf_telem_set_rate_limit(uint16_t points_per_second);

/**
 * @brief Process telemetry (call from main loop)
 */
int gf_telem_process(void);

/**
 * @brief Create point helper
 */
int gf_telem_point_init(gf_telem_point_t *point, const char *metric);

/**
 * @brief Add tag to point
 */
int gf_telem_point_add_tag(gf_telem_point_t *point, const char *name,
                           const char *value);

/**
 * @brief Set point timestamp (0 = now)
 */
int gf_telem_point_set_time(gf_telem_point_t *point, uint64_t timestamp_ms);

#endif /* GF_TELEMETRY_H */
