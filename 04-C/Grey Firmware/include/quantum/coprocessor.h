/**
 * @file coprocessor.h
 * @brief Specialized Co-Processor Interface
 * 
 * INDUSTRY RELEVANCE:
 * Modern embedded systems offload compute-intensive tasks to specialized
 * accelerators (NPUs, DSPs, FPGAs, cryptographic engines). This module
 * demonstrates:
 * - Heterogeneous computing with shared memory management
 * - Asynchronous job submission and completion handling
 * - Power-aware accelerator lifecycle management
 * - DMA-based data transfer with cache coherency
 * 
 * Applications: AI inference accelerators, cryptographic offload,
 *               signal processing, hardware security modules
 * Standards: OpenCL embedded profile, ARM CMSIS-NN, NVIDIA CUDA
 * 
 * @copyright Grey Firmware Project
 */

#ifndef GF_COPROCESSOR_H
#define GF_COPROCESSOR_H

#include <stdint.h>
#include <stdbool.h>
#include "core/error_handler.h"

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Co-Processor Types
 ******************************************************************************/

/** Co-processor types */
typedef enum {
    GF_COPROC_NPU,           /**< Neural Processing Unit */
    GF_COPROC_DSP,           /**< Digital Signal Processor */
    GF_COPROC_FPGA,          /**< Field-Programmable Gate Array */
    GF_COPROC_CRYPTO,        /**< Cryptographic accelerator */
    GF_COPROC_GPU,           /**< Graphics Processing Unit */
    GF_COPROC_TPU,           /**< Tensor Processing Unit */
    GF_COPROC_CUSTOM         /**< Vendor-specific accelerator */
} gf_coproc_type_t;

/** Co-processor state */
typedef enum {
    GF_COPROC_STATE_OFF,
    GF_COPROC_STATE_BOOTING,
    GF_COPROC_STATE_IDLE,
    GF_COPROC_STATE_BUSY,
    GF_COPROC_STATE_ERROR,
    GF_COPROC_STATE_SLEEPING  /**< Low-power retention mode */
} gf_coproc_state_t;

/** Job priority */
typedef enum {
    GF_COPROC_PRIORITY_LOW,
    GF_COPROC_PRIORITY_NORMAL,
    GF_COPROC_PRIORITY_HIGH,
    GF_COPROC_PRIORITY_REALTIME
} gf_coproc_priority_t;

/** Job status */
typedef enum {
    GF_JOB_PENDING,
    GF_JOB_RUNNING,
    GF_JOB_COMPLETED,
    GF_JOB_FAILED,
    GF_JOB_CANCELLED,
    GF_JOB_TIMEOUT
} gf_job_status_t;

/*******************************************************************************
 * Co-Processor Configuration
 ******************************************************************************/

/** Co-processor capabilities */
typedef struct {
    gf_coproc_type_t type;
    char name[32];
    uint32_t max_clock_mhz;
    uint32_t memory_kb;
    uint32_t compute_units;
    uint32_t max_concurrent_jobs;
    bool supports_dma;
    bool supports_interrupts;
    bool supports_sleep;
    uint32_t power_mw_active;
    uint32_t power_mw_idle;
    uint32_t power_mw_sleep;
} gf_coproc_caps_t;

/** Co-processor configuration */
typedef struct {
    gf_coproc_type_t type;
    uint32_t clock_mhz;           /**< Operating frequency */
    uint32_t memory_allocation_kb;/**< Dedicated memory */
    uint8_t interrupt_priority;
    bool enable_dma;
    bool enable_cache_coherency;
    uint32_t job_timeout_ms;
    void (*completion_callback)(uint32_t job_id, gf_job_status_t status);
} gf_coproc_config_t;

/** Job descriptor */
typedef struct {
    uint32_t job_id;
    gf_coproc_priority_t priority;
    void *input_buffer;
    uint32_t input_size;
    void *output_buffer;
    uint32_t output_size;
    uint32_t kernel_id;           /**< Operation to perform */
    uint32_t param[8];            /**< Kernel parameters */
    uint32_t timeout_ms;
} gf_coproc_job_t;

/*******************************************************************************
 * Co-Processor Statistics
 ******************************************************************************/

typedef struct {
    uint32_t jobs_submitted;
    uint32_t jobs_completed;
    uint32_t jobs_failed;
    uint32_t jobs_cancelled;
    uint64_t compute_time_us;
    uint64_t idle_time_us;
    uint64_t bytes_transferred;
    uint32_t dma_transfers;
    uint32_t cache_flushes;
    uint32_t power_state_changes;
    gf_coproc_state_t current_state;
} gf_coproc_stats_t;

/*******************************************************************************
 * API Functions
 ******************************************************************************/

/**
 * @brief Initialize co-processor
 * @param config Co-processor configuration
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_coproc_init(const gf_coproc_config_t *config);

/**
 * @brief Get co-processor capabilities
 * @param caps Output capabilities
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_coproc_get_caps(gf_coproc_caps_t *caps);

/**
 * @brief Submit job to co-processor
 * @param job Job descriptor
 * @return Job ID or negative error code
 */
int32_t gf_coproc_submit_job(const gf_coproc_job_t *job);

/**
 * @brief Wait for job completion
 * @param job_id Job to wait for
 * @param timeout_ms Timeout in milliseconds
 * @return Job status
 */
gf_job_status_t gf_coproc_wait_job(uint32_t job_id, uint32_t timeout_ms);

/**
 * @brief Cancel pending job
 * @param job_id Job to cancel
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_coproc_cancel_job(uint32_t job_id);

/**
 * @brief Get job status
 * @param job_id Job to query
 * @return Job status
 */
gf_job_status_t gf_coproc_get_job_status(uint32_t job_id);

/**
 * @brief Enter low-power sleep mode
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_coproc_sleep(void);

/**
 * @brief Wake from sleep mode
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_coproc_wake(void);

/**
 * @brief Load firmware/bitstream to co-processor
 * @param firmware Firmware data
 * @param size Firmware size
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_coproc_load_firmware(const uint8_t *firmware, size_t size);

/**
 * @brief Get co-processor statistics
 * @return Current statistics
 */
gf_coproc_stats_t gf_coproc_get_stats(void);

/**
 * @brief Shutdown co-processor
 * @return GF_SUCCESS or error code
 */
gf_error_t gf_coproc_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_COPROCESSOR_H */
