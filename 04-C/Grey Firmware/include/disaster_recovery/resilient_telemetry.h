/**
 * @file resilient_telemetry.h
 * @brief Resilient Telemetry Collector for Disaster Recovery Networks
 * 
 * @details
 * Fault-tolerant telemetry collection for disaster recovery and
 * emergency operations. Implements local buffering, data aggregation,
 * and opportunistic upload for intermittent connectivity.
 * 
 * INDUSTRY RELEVANCE:
 * - Disaster response systems
 * - Remote infrastructure monitoring
 * - Military field operations
 * - Mining and offshore platforms
 * - Developing region connectivity
 * 
 * KEY FEATURES:
 * - Persistent local storage
 * - Data compression
 * - Opportunistic forwarding
 * - Conflict-free merging (CRDTs)
 * - Bandwidth-aware transmission
 * - Data prioritization
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_RESILIENT_TELEMETRY_H
#define GF_RESILIENT_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum local buffer size (bytes) */
#define GF_RTEL_BUFFER_SIZE         65536

/** Maximum telemetry channels */
#define GF_RTEL_MAX_CHANNELS        32

/** Compression threshold (bytes) */
#define GF_RTEL_COMPRESS_THRESHOLD  256

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Telemetry status codes
 */
typedef enum {
    GF_RTEL_OK = 0,
    GF_RTEL_ERROR_NOT_INIT,
    GF_RTEL_ERROR_NULL_PTR,
    GF_RTEL_ERROR_BUFFER_FULL,
    GF_RTEL_ERROR_STORAGE_FAIL,
    GF_RTEL_ERROR_COMPRESS_FAIL,
    GF_RTEL_WARN_OFFLINE,
    GF_RTEL_WARN_DEGRADED
} gf_rtel_status_t;

/**
 * @brief Telemetry data types
 */
typedef enum {
    GF_RTEL_TYPE_SENSOR,          /**< Sensor readings */
    GF_RTEL_TYPE_STATUS,          /**< Status updates */
    GF_RTEL_TYPE_EVENT,           /**< Discrete events */
    GF_RTEL_TYPE_ALERT,           /**< Alert/alarm */
    GF_RTEL_TYPE_LOCATION,        /**< GPS location */
    GF_RTEL_TYPE_DIAGNOSTIC       /**< Diagnostic data */
} gf_rtel_type_t;

/**
 * @brief Data priority
 */
typedef enum {
    GF_RTEL_PRIO_BULK,            /**< Bulk upload when available */
    GF_RTEL_PRIO_NORMAL,          /**< Normal priority */
    GF_RTEL_PRIO_HIGH,            /**< High priority */
    GF_RTEL_PRIO_CRITICAL         /**< Critical - immediate */
} gf_rtel_priority_t;

/**
 * @brief Connectivity state
 */
typedef enum {
    GF_RTEL_CONN_OFFLINE,         /**< No connectivity */
    GF_RTEL_CONN_DEGRADED,        /**< Limited connectivity */
    GF_RTEL_CONN_NORMAL           /**< Full connectivity */
} gf_rtel_connectivity_t;

/**
 * @brief Telemetry sample
 */
typedef struct {
    uint16_t channel_id;          /**< Channel identifier */
    gf_rtel_type_t type;          /**< Data type */
    gf_rtel_priority_t priority;  /**< Data priority */
    uint32_t timestamp;           /**< Unix timestamp */
    uint32_t sequence;            /**< Sequence number */
    uint8_t* data;                /**< Sample data */
    uint16_t data_len;            /**< Data length */
} gf_rtel_sample_t;

/**
 * @brief Channel configuration
 */
typedef struct {
    uint16_t channel_id;          /**< Channel identifier */
    char name[32];                /**< Channel name */
    gf_rtel_type_t type;          /**< Default type */
    gf_rtel_priority_t priority;  /**< Default priority */
    uint16_t sample_interval_s;   /**< Sample interval */
    bool compress;                /**< Enable compression */
    uint16_t aggregation_count;   /**< Samples to aggregate */
} gf_rtel_channel_config_t;

/**
 * @brief Buffer statistics
 */
typedef struct {
    uint32_t bytes_buffered;      /**< Bytes in buffer */
    uint32_t bytes_capacity;      /**< Buffer capacity */
    uint32_t samples_pending;     /**< Pending samples */
    uint32_t samples_uploaded;    /**< Uploaded samples */
    uint32_t samples_dropped;     /**< Dropped samples */
    uint32_t upload_failures;     /**< Upload failures */
    gf_rtel_connectivity_t conn;  /**< Connectivity state */
} gf_rtel_stats_t;

/**
 * @brief Upload complete callback
 */
typedef void (*gf_rtel_upload_cb_t)(uint32_t samples_uploaded,
                                     uint32_t bytes_sent, void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize resilient telemetry
 * @return Status code
 */
gf_rtel_status_t gf_rtel_init(void);

/**
 * @brief Shutdown resilient telemetry
 * @return Status code
 */
gf_rtel_status_t gf_rtel_shutdown(void);

/**
 * @brief Configure channel
 * @param config Channel configuration
 * @return Status code
 */
gf_rtel_status_t gf_rtel_configure_channel(const gf_rtel_channel_config_t* config);

/**
 * @brief Record telemetry sample
 * @param sample Telemetry sample
 * @return Status code
 */
gf_rtel_status_t gf_rtel_record(const gf_rtel_sample_t* sample);

/**
 * @brief Update connectivity state
 * @param state Connectivity state
 * @return Status code
 */
gf_rtel_status_t gf_rtel_set_connectivity(gf_rtel_connectivity_t state);

/**
 * @brief Trigger immediate upload
 * @param priority_threshold Minimum priority to upload
 * @return Status code
 */
gf_rtel_status_t gf_rtel_flush(gf_rtel_priority_t priority_threshold);

/**
 * @brief Get buffer statistics
 * @param stats Output statistics
 * @return Status code
 */
gf_rtel_status_t gf_rtel_get_stats(gf_rtel_stats_t* stats);

/**
 * @brief Drop buffered data by priority
 * @param priority Priority threshold
 * @return Number of samples dropped
 */
uint32_t gf_rtel_drop_below_priority(gf_rtel_priority_t priority);

/**
 * @brief Register upload callback
 * @param callback Upload callback
 * @param user_data User context
 * @return Status code
 */
gf_rtel_status_t gf_rtel_register_callback(gf_rtel_upload_cb_t callback,
                                            void* user_data);

/**
 * @brief Process telemetry (call periodically)
 * @return Status code
 */
gf_rtel_status_t gf_rtel_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_RESILIENT_TELEMETRY_H */
