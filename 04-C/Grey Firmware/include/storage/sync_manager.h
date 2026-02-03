/**
 * @file sync_manager.h
 * @brief Cloud/Offline Sync Manager for Edge Storage
 * 
 * Provides bidirectional synchronization between local edge storage
 * and cloud backends, handling connectivity transitions gracefully.
 * 
 * Key Features:
 * - Offline-first operation with local queue
 * - Conflict resolution strategies
 * - Bandwidth-aware sync scheduling
 * - Priority-based sync ordering
 * - Incremental/delta sync support
 * - Sync progress tracking and resumption
 * 
 * Industry Relevance:
 * - IoT devices in low-connectivity environments
 * - Field data collection with periodic uploads
 * - Two-way configuration synchronization
 * - OTA update delivery with resume support
 */

#ifndef GF_SYNC_MANAGER_H
#define GF_SYNC_MANAGER_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define GF_SYNC_MAX_PENDING         256
#define GF_SYNC_MAX_ID_LEN          64
#define GF_SYNC_MAX_PATH_LEN        128
#define GF_SYNC_MAX_ENDPOINTS       4

/*===========================================================================*/
/* Enumerations                                                               */
/*===========================================================================*/

/**
 * @brief Sync direction modes
 */
typedef enum {
    GF_SYNC_DIR_UPLOAD       = 0x01,  /** Local -> Cloud only */
    GF_SYNC_DIR_DOWNLOAD     = 0x02,  /** Cloud -> Local only */
    GF_SYNC_DIR_BIDIRECTIONAL = 0x03, /** Both directions */
} gf_sync_direction_t;

/**
 * @brief Sync operation types
 */
typedef enum {
    GF_SYNC_OP_CREATE        = 0x01,  /** Create new item */
    GF_SYNC_OP_UPDATE        = 0x02,  /** Update existing */
    GF_SYNC_OP_DELETE        = 0x03,  /** Delete item */
    GF_SYNC_OP_REPLACE       = 0x04,  /** Full replacement */
} gf_sync_operation_t;

/**
 * @brief Sync item priority
 */
typedef enum {
    GF_SYNC_PRIORITY_LOW     = 0,
    GF_SYNC_PRIORITY_NORMAL  = 1,
    GF_SYNC_PRIORITY_HIGH    = 2,
    GF_SYNC_PRIORITY_URGENT  = 3,     /** Sync immediately on connectivity */
} gf_sync_priority_t;

/**
 * @brief Conflict resolution strategies
 */
typedef enum {
    GF_SYNC_CONFLICT_LOCAL_WINS   = 0, /** Local changes override remote */
    GF_SYNC_CONFLICT_REMOTE_WINS  = 1, /** Remote changes override local */
    GF_SYNC_CONFLICT_NEWEST_WINS  = 2, /** Most recent timestamp wins */
    GF_SYNC_CONFLICT_MERGE        = 3, /** Attempt automatic merge */
    GF_SYNC_CONFLICT_CALLBACK     = 4, /** Defer to callback for resolution */
    GF_SYNC_CONFLICT_DUPLICATE    = 5, /** Keep both with suffix */
} gf_sync_conflict_t;

/**
 * @brief Connectivity states
 */
typedef enum {
    GF_SYNC_CONN_OFFLINE     = 0,     /** No connectivity */
    GF_SYNC_CONN_METERED     = 1,     /** Limited/expensive connection */
    GF_SYNC_CONN_ONLINE      = 2,     /** Full connectivity */
} gf_sync_connectivity_t;

/**
 * @brief Sync item states
 */
typedef enum {
    GF_SYNC_STATE_PENDING    = 0,     /** Awaiting sync */
    GF_SYNC_STATE_SYNCING    = 1,     /** Currently syncing */
    GF_SYNC_STATE_COMPLETE   = 2,     /** Successfully synced */
    GF_SYNC_STATE_CONFLICT   = 3,     /** Unresolved conflict */
    GF_SYNC_STATE_ERROR      = 4,     /** Sync failed */
    GF_SYNC_STATE_CANCELLED  = 5,     /** User cancelled */
} gf_sync_state_t;

/**
 * @brief Sync error codes
 */
typedef enum {
    GF_SYNC_OK               = 0,
    GF_SYNC_ERR_OFFLINE      = -1,
    GF_SYNC_ERR_CONFLICT     = -2,
    GF_SYNC_ERR_NO_SPACE     = -3,
    GF_SYNC_ERR_AUTH         = -4,
    GF_SYNC_ERR_TIMEOUT      = -5,
    GF_SYNC_ERR_NOT_FOUND    = -6,
    GF_SYNC_ERR_INVALID      = -7,
    GF_SYNC_ERR_CANCELLED    = -8,
} gf_sync_result_t;

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Sync manager configuration
 */
typedef struct {
    char                    endpoint[GF_SYNC_MAX_PATH_LEN];
    char                    device_id[GF_SYNC_MAX_ID_LEN];
    char                    api_key[64];
    gf_sync_direction_t     direction;
    gf_sync_conflict_t      conflict_strategy;
    uint32_t                sync_interval_ms;     /** Auto-sync interval (0=manual) */
    uint32_t                retry_interval_ms;    /** Retry on failure */
    uint32_t                max_retries;
    bool                    sync_on_metered;      /** Sync on metered connection */
    bool                    compress;             /** Compress sync payloads */
    bool                    delta_sync;           /** Use delta/incremental sync */
    uint32_t                batch_size;           /** Items per sync batch */
} gf_sync_config_t;

/**
 * @brief Sync item descriptor
 */
typedef struct {
    char                    id[GF_SYNC_MAX_ID_LEN];
    char                    collection[GF_SYNC_MAX_PATH_LEN];
    gf_sync_operation_t     operation;
    gf_sync_priority_t      priority;
    gf_sync_state_t         state;
    uint64_t                local_modified;       /** Local modification time */
    uint64_t                remote_modified;      /** Remote modification time */
    uint32_t                local_version;        /** Local version counter */
    uint32_t                remote_version;       /** Remote version counter */
    size_t                  data_size;            /** Payload size */
    uint32_t                retry_count;          /** Current retry count */
    int                     last_error;           /** Last sync error */
} gf_sync_item_t;

/**
 * @brief Sync progress information
 */
typedef struct {
    uint32_t                pending_upload;       /** Items waiting to upload */
    uint32_t                pending_download;     /** Items waiting to download */
    uint32_t                in_progress;          /** Currently syncing */
    uint32_t                completed;            /** Successfully synced */
    uint32_t                failed;               /** Failed items */
    uint32_t                conflicts;            /** Unresolved conflicts */
    uint64_t                bytes_uploaded;       /** Total bytes uploaded */
    uint64_t                bytes_downloaded;     /** Total bytes downloaded */
    float                   progress_pct;         /** Overall progress percentage */
    bool                    is_syncing;           /** Sync in progress */
} gf_sync_progress_t;

/**
 * @brief Sync statistics
 */
typedef struct {
    uint64_t                total_syncs;          /** Total sync operations */
    uint64_t                items_uploaded;       /** Total items uploaded */
    uint64_t                items_downloaded;     /** Total items downloaded */
    uint64_t                conflicts_resolved;   /** Auto-resolved conflicts */
    uint64_t                bytes_transferred;    /** Total bytes transferred */
    uint32_t                avg_sync_time_ms;     /** Average sync time */
    uint32_t                last_sync_time_ms;    /** Last sync duration */
    uint64_t                last_sync_timestamp;  /** Last successful sync */
    uint32_t                consecutive_failures; /** Consecutive failures */
} gf_sync_stats_t;

/**
 * @brief Conflict details for callback resolution
 */
typedef struct {
    gf_sync_item_t          local;
    gf_sync_item_t          remote;
    const void             *local_data;
    size_t                  local_size;
    const void             *remote_data;
    size_t                  remote_size;
} gf_sync_conflict_info_t;

/*===========================================================================*/
/* Callback Types                                                             */
/*===========================================================================*/

/**
 * @brief Conflict resolution callback
 * @return 0=use local, 1=use remote, 2=merge (provide merged data), -1=skip
 */
typedef int (*gf_sync_conflict_cb_t)(const gf_sync_conflict_info_t *conflict,
                                     void *merge_output, size_t *merge_size,
                                     void *ctx);

/**
 * @brief Sync progress callback
 */
typedef void (*gf_sync_progress_cb_t)(const gf_sync_progress_t *progress,
                                      void *ctx);

/**
 * @brief Sync completion callback
 */
typedef void (*gf_sync_complete_cb_t)(gf_sync_result_t result, int items_synced,
                                      void *ctx);

/**
 * @brief Connectivity change callback
 */
typedef void (*gf_sync_conn_cb_t)(gf_sync_connectivity_t connectivity,
                                  void *ctx);

/*===========================================================================*/
/* API Functions - Initialization                                             */
/*===========================================================================*/

/**
 * @brief Initialize sync manager
 */
int gf_sync_init(const gf_sync_config_t *config);

/**
 * @brief Shutdown sync manager
 */
int gf_sync_shutdown(void);

/**
 * @brief Update connectivity state
 */
int gf_sync_set_connectivity(gf_sync_connectivity_t connectivity);

/**
 * @brief Get current connectivity state
 */
gf_sync_connectivity_t gf_sync_get_connectivity(void);

/*===========================================================================*/
/* API Functions - Queue Management                                           */
/*===========================================================================*/

/**
 * @brief Queue item for sync
 */
int gf_sync_queue(const char *collection, const char *id,
                  gf_sync_operation_t operation, const void *data, size_t size);

/**
 * @brief Queue item with priority
 */
int gf_sync_queue_priority(const char *collection, const char *id,
                           gf_sync_operation_t operation, const void *data,
                           size_t size, gf_sync_priority_t priority);

/**
 * @brief Remove item from sync queue
 */
int gf_sync_dequeue(const char *collection, const char *id);

/**
 * @brief Get pending sync items
 */
int gf_sync_get_pending(gf_sync_item_t *items, int max_items, int *count);

/**
 * @brief Get item sync status
 */
int gf_sync_get_item_status(const char *collection, const char *id,
                            gf_sync_item_t *item);

/*===========================================================================*/
/* API Functions - Sync Control                                               */
/*===========================================================================*/

/**
 * @brief Start synchronization
 */
int gf_sync_start(void);

/**
 * @brief Stop synchronization
 */
int gf_sync_stop(void);

/**
 * @brief Force immediate sync
 */
int gf_sync_now(void);

/**
 * @brief Sync specific collection
 */
int gf_sync_collection(const char *collection);

/**
 * @brief Sync single item
 */
int gf_sync_item(const char *collection, const char *id);

/**
 * @brief Pause sync (maintain state)
 */
int gf_sync_pause(void);

/**
 * @brief Resume paused sync
 */
int gf_sync_resume(void);

/**
 * @brief Cancel pending sync for item
 */
int gf_sync_cancel_item(const char *collection, const char *id);

/**
 * @brief Retry failed items
 */
int gf_sync_retry_failed(void);

/*===========================================================================*/
/* API Functions - Conflict Resolution                                        */
/*===========================================================================*/

/**
 * @brief Set conflict resolution strategy
 */
int gf_sync_set_conflict_strategy(gf_sync_conflict_t strategy);

/**
 * @brief Set conflict resolution callback
 */
int gf_sync_set_conflict_callback(gf_sync_conflict_cb_t callback, void *ctx);

/**
 * @brief Get list of unresolved conflicts
 */
int gf_sync_get_conflicts(gf_sync_item_t *items, int max_items, int *count);

/**
 * @brief Manually resolve conflict
 */
int gf_sync_resolve_conflict(const char *collection, const char *id,
                             bool use_local);

/**
 * @brief Resolve conflict with merged data
 */
int gf_sync_resolve_with_merge(const char *collection, const char *id,
                               const void *merged_data, size_t size);

/*===========================================================================*/
/* API Functions - Callbacks                                                  */
/*===========================================================================*/

/**
 * @brief Set progress callback
 */
int gf_sync_set_progress_callback(gf_sync_progress_cb_t callback, void *ctx);

/**
 * @brief Set completion callback
 */
int gf_sync_set_complete_callback(gf_sync_complete_cb_t callback, void *ctx);

/**
 * @brief Set connectivity change callback
 */
int gf_sync_set_conn_callback(gf_sync_conn_cb_t callback, void *ctx);

/*===========================================================================*/
/* API Functions - Status                                                     */
/*===========================================================================*/

/**
 * @brief Get sync progress
 */
int gf_sync_get_progress(gf_sync_progress_t *progress);

/**
 * @brief Get sync statistics
 */
int gf_sync_get_stats(gf_sync_stats_t *stats);

/**
 * @brief Reset sync statistics
 */
int gf_sync_reset_stats(void);

/**
 * @brief Check if sync is active
 */
bool gf_sync_is_syncing(void);

/**
 * @brief Check if all items are synced
 */
bool gf_sync_is_complete(void);

/**
 * @brief Sync periodic processing
 */
int gf_sync_process(void);

#endif /* GF_SYNC_MANAGER_H */
