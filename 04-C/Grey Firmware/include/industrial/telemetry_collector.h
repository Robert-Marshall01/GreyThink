/**
 * @file telemetry_collector.h
 * @brief Industrial Telemetry Data Collector
 *
 * INDUSTRY RELEVANCE:
 * Industrial IoT requires robust data collection infrastructure:
 * - High-frequency sensor sampling with buffering
 * - Edge aggregation and compression
 * - Time-series data formatting (InfluxDB, TimescaleDB)
 * - Reliable transmission with store-and-forward
 * - Historian integration for compliance
 *
 * Used in: Predictive maintenance, process monitoring, energy management
 *
 * @note This is a stub demonstrating IIoT telemetry patterns.
 */

#ifndef GF_TELEMETRY_COLLECTOR_H
#define GF_TELEMETRY_COLLECTOR_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Telemetry Type Definitions                                                */
/*===========================================================================*/

/**
 * @brief Maximum number of telemetry channels
 */
#define GF_TELEMETRY_MAX_CHANNELS   64

/**
 * @brief Telemetry data types
 */
typedef enum {
    GF_TELEM_INT32,
    GF_TELEM_UINT32,
    GF_TELEM_INT64,
    GF_TELEM_UINT64,
    GF_TELEM_FLOAT,
    GF_TELEM_DOUBLE,
    GF_TELEM_BOOL,
    GF_TELEM_STRING
} gf_telem_type_t;

/**
 * @brief Aggregation methods
 */
typedef enum {
    GF_AGG_NONE,                /**< No aggregation (raw values) */
    GF_AGG_AVERAGE,             /**< Arithmetic mean */
    GF_AGG_MIN,                 /**< Minimum value */
    GF_AGG_MAX,                 /**< Maximum value */
    GF_AGG_SUM,                 /**< Sum of values */
    GF_AGG_COUNT,               /**< Count of samples */
    GF_AGG_FIRST,               /**< First value in window */
    GF_AGG_LAST,                /**< Last value in window */
    GF_AGG_DELTA                /**< Change from previous */
} gf_aggregation_t;

/**
 * @brief Data quality flags
 */
typedef enum {
    GF_QUALITY_GOOD         = 0x00,
    GF_QUALITY_UNCERTAIN    = 0x40,
    GF_QUALITY_BAD          = 0x80,
    GF_QUALITY_STALE        = 0x81,
    GF_QUALITY_SENSOR_FAULT = 0x82,
    GF_QUALITY_OVERFLOW     = 0x83
} gf_data_quality_t;

/**
 * @brief Telemetry channel configuration
 */
typedef struct {
    const char* name;               /**< Channel name */
    const char* unit;               /**< Engineering unit */
    gf_telem_type_t type;
    gf_aggregation_t aggregation;
    uint32_t sample_period_ms;      /**< Sampling period */
    uint32_t report_period_ms;      /**< Reporting period */
    float deadband;                 /**< Change threshold for reporting */
    float scale_factor;             /**< Scaling multiplier */
    float offset;                   /**< Scaling offset */
    bool enabled;
} gf_telem_channel_t;

/**
 * @brief Telemetry sample
 */
typedef union {
    int32_t i32;
    uint32_t u32;
    int64_t i64;
    uint64_t u64;
    float f32;
    double f64;
    bool b;
    char str[32];
} gf_telem_value_t;

/**
 * @brief Timestamped data point
 */
typedef struct {
    uint16_t channel_id;
    gf_telem_value_t value;
    uint64_t timestamp_ms;          /**< Unix timestamp (ms) */
    gf_data_quality_t quality;
} gf_telem_point_t;

/**
 * @brief Telemetry batch for transmission
 */
typedef struct {
    char device_id[32];             /**< Device identifier */
    uint64_t batch_timestamp;       /**< Batch creation time */
    uint32_t sequence_number;       /**< For ordering/dedup */
    gf_telem_point_t* points;       /**< Data points */
    size_t point_count;
    size_t compressed_size;         /**< If compression applied */
    bool is_compressed;
} gf_telem_batch_t;

/**
 * @brief Collector configuration
 */
typedef struct {
    const char* device_id;
    size_t buffer_size;             /**< Ring buffer size (points) */
    size_t batch_size;              /**< Points per transmission */
    uint32_t flush_interval_ms;     /**< Max time before flush */
    bool enable_compression;        /**< Compress batches */
    bool store_and_forward;         /**< Local persistence */
    const char* storage_path;       /**< Path for S&F storage */
} gf_collector_config_t;

/**
 * @brief Collector statistics
 */
typedef struct {
    uint32_t samples_collected;
    uint32_t samples_dropped;
    uint32_t batches_sent;
    uint32_t batches_failed;
    uint32_t batches_pending;       /**< In store-and-forward queue */
    size_t buffer_used;
    size_t buffer_capacity;
    uint64_t bytes_transmitted;
    uint64_t bytes_compressed;
} gf_collector_stats_t;

/**
 * @brief Telemetry collector handle
 */
typedef struct gf_telemetry_collector* gf_collector_t;

/*===========================================================================*/
/* Telemetry Collector API                                                   */
/*===========================================================================*/

/**
 * @brief Initialize telemetry collector
 * @param config Configuration
 * @param collector Output handle
 * @return 0 on success
 */
int gf_collector_init(const gf_collector_config_t* config,
                       gf_collector_t* collector);

/**
 * @brief Add telemetry channel
 * @param collector Collector handle
 * @param channel Channel configuration
 * @return Channel ID (>=0) or error (<0)
 */
int gf_collector_add_channel(gf_collector_t collector,
                              const gf_telem_channel_t* channel);

/**
 * @brief Record a sample
 * @param collector Collector handle
 * @param channel_id Channel identifier
 * @param value Sample value
 * @param quality Data quality
 * @return 0 on success
 */
int gf_collector_record(gf_collector_t collector,
                         uint16_t channel_id,
                         gf_telem_value_t value,
                         gf_data_quality_t quality);

/**
 * @brief Record float sample (convenience)
 */
int gf_collector_record_float(gf_collector_t collector,
                               uint16_t channel_id,
                               float value);

/**
 * @brief Record integer sample (convenience)
 */
int gf_collector_record_int(gf_collector_t collector,
                             uint16_t channel_id,
                             int32_t value);

/**
 * @brief Run aggregation/flush cycle
 * @param collector Collector handle
 * @param current_time_ms Current system time
 * @return Number of batches ready
 */
int gf_collector_process(gf_collector_t collector,
                          uint64_t current_time_ms);

/**
 * @brief Get next batch for transmission
 * @param collector Collector handle
 * @param batch Output batch (caller must free points)
 * @return 0 on success, 1 if no batch ready
 */
int gf_collector_get_batch(gf_collector_t collector,
                            gf_telem_batch_t* batch);

/**
 * @brief Acknowledge batch transmission
 * @param collector Collector handle
 * @param sequence_number Sequence number of sent batch
 * @param success true if sent successfully
 * @return 0 on success
 */
int gf_collector_ack_batch(gf_collector_t collector,
                            uint32_t sequence_number,
                            bool success);

/**
 * @brief Get collector statistics
 * @param collector Collector handle
 * @param stats Output statistics
 * @return 0 on success
 */
int gf_collector_get_stats(gf_collector_t collector,
                            gf_collector_stats_t* stats);

/**
 * @brief Flush all pending data
 * @param collector Collector handle
 * @return 0 on success
 */
int gf_collector_flush(gf_collector_t collector);

/**
 * @brief Deinitialize collector
 * @param collector Collector handle
 */
void gf_collector_deinit(gf_collector_t collector);

/*===========================================================================*/
/* Serialization Helpers                                                     */
/*===========================================================================*/

/**
 * @brief Serialize batch to JSON (InfluxDB line protocol compatible)
 * @param batch Batch to serialize
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written, or negative on error
 */
int gf_telem_batch_to_json(const gf_telem_batch_t* batch,
                            char* buffer,
                            size_t max_len);

/**
 * @brief Serialize batch to binary format
 * @param batch Batch to serialize
 * @param buffer Output buffer
 * @param max_len Buffer size
 * @return Bytes written, or negative on error
 */
int gf_telem_batch_to_binary(const gf_telem_batch_t* batch,
                              uint8_t* buffer,
                              size_t max_len);

/**
 * @brief Compress data using simple RLE/delta encoding
 * @param input Input data
 * @param input_len Input length
 * @param output Output buffer
 * @param output_len Output buffer size
 * @return Compressed size, or negative on error
 */
int gf_telem_compress(const uint8_t* input,
                       size_t input_len,
                       uint8_t* output,
                       size_t output_len);

#ifdef __cplusplus
}
#endif

#endif /* GF_TELEMETRY_COLLECTOR_H */
