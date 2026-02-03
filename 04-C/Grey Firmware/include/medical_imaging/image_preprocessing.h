/**
 * @file image_preprocessing.h
 * @brief Medical Image Preprocessing Pipeline
 *
 * INDUSTRY RELEVANCE:
 * Medical image preprocessing is critical for diagnostic quality and regulatory
 * compliance. This module demonstrates expertise in:
 * - Real-time signal processing on resource-constrained devices
 * - FDA/CE-compliant image processing algorithms
 * - DICOM-compatible image pipelines
 * - GPU/FPGA acceleration interfaces
 *
 * Skills demonstrated here apply to medical imaging companies, AI-assisted
 * diagnostics (Aidoc, Viz.ai), and portable imaging devices that require
 * edge-based processing for low-latency display.
 *
 * @note This is a stub module demonstrating interface design.
 *       Production algorithms require clinical validation.
 */

#ifndef GF_IMAGE_PREPROCESSING_H
#define GF_IMAGE_PREPROCESSING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ─────────────────────────────────────────────────────────────────────────────
 * Constants
 * ───────────────────────────────────────────────────────────────────────────── */

#define GF_IMG_MAX_WIDTH            4096    /**< Maximum image width */
#define GF_IMG_MAX_HEIGHT           4096    /**< Maximum image height */
#define GF_IMG_MAX_PIPELINE_STAGES  16      /**< Maximum processing stages */

/* ─────────────────────────────────────────────────────────────────────────────
 * Type Definitions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Image pixel format
 */
typedef enum {
    GF_IMG_FORMAT_GRAY8,        /**< 8-bit grayscale */
    GF_IMG_FORMAT_GRAY16,       /**< 16-bit grayscale */
    GF_IMG_FORMAT_RGB24,        /**< 24-bit RGB */
    GF_IMG_FORMAT_RGBA32,       /**< 32-bit RGBA */
    GF_IMG_FORMAT_YUV420,       /**< YUV 4:2:0 planar */
    GF_IMG_FORMAT_RF_INT16,     /**< Raw RF data (signed 16-bit) */
    GF_IMG_FORMAT_FLOAT32       /**< 32-bit floating point */
} gf_img_format_t;

/**
 * @brief Processing stage types
 */
typedef enum {
    GF_IMG_STAGE_ENVELOPE_DETECT,   /**< RF to envelope (Hilbert transform) */
    GF_IMG_STAGE_LOG_COMPRESS,      /**< Log compression for display */
    GF_IMG_STAGE_SCAN_CONVERT,      /**< Polar to Cartesian conversion */
    GF_IMG_STAGE_SPECKLE_REDUCE,    /**< Speckle reduction filter */
    GF_IMG_STAGE_EDGE_ENHANCE,      /**< Edge enhancement */
    GF_IMG_STAGE_DENOISE,           /**< Noise reduction */
    GF_IMG_STAGE_HISTOGRAM_EQ,      /**< Histogram equalization */
    GF_IMG_STAGE_GAMMA_CORRECT,     /**< Gamma correction */
    GF_IMG_STAGE_COLORMAP,          /**< Grayscale to color mapping */
    GF_IMG_STAGE_SPATIAL_COMPOUND,  /**< Spatial compounding */
    GF_IMG_STAGE_PERSISTENCE,       /**< Frame averaging/persistence */
    GF_IMG_STAGE_ANNOTATION         /**< Text/graphics overlay */
} gf_img_stage_type_t;

/**
 * @brief Image buffer descriptor
 */
typedef struct {
    void* data;                     /**< Pixel data pointer */
    uint16_t width;                 /**< Width in pixels */
    uint16_t height;                /**< Height in pixels */
    uint16_t stride;                /**< Row stride in bytes */
    gf_img_format_t format;         /**< Pixel format */
    uint32_t frame_number;          /**< Frame counter */
    uint32_t timestamp_us;          /**< Frame timestamp */
} gf_img_buffer_t;

/**
 * @brief Speckle reduction parameters
 */
typedef struct {
    uint8_t strength;               /**< Filter strength (0-10) */
    uint8_t kernel_size;            /**< Spatial kernel size */
    bool preserve_edges;            /**< Edge preservation mode */
} gf_img_speckle_params_t;

/**
 * @brief Scan conversion parameters
 */
typedef struct {
    float sector_angle_deg;         /**< Sector angle */
    float depth_mm;                 /**< Imaging depth */
    uint16_t num_lines;             /**< Input scan lines */
    uint16_t output_width;          /**< Output image width */
    uint16_t output_height;         /**< Output image height */
    bool bilinear_interp;           /**< Use bilinear interpolation */
} gf_img_scan_convert_params_t;

/**
 * @brief Processing stage configuration
 */
typedef struct {
    gf_img_stage_type_t type;       /**< Stage type */
    bool enabled;                   /**< Stage enabled */
    union {
        gf_img_speckle_params_t speckle;
        gf_img_scan_convert_params_t scan_convert;
        float gamma;                /**< Gamma value */
        uint8_t persistence;        /**< Persistence frames (1-16) */
        uint8_t edge_strength;      /**< Edge enhancement (0-10) */
        float log_dynamic_range;    /**< Log compression range in dB */
    } params;
} gf_img_stage_config_t;

/**
 * @brief Processing pipeline configuration
 */
typedef struct {
    gf_img_stage_config_t stages[GF_IMG_MAX_PIPELINE_STAGES];
    uint8_t num_stages;             /**< Number of configured stages */
    gf_img_format_t input_format;   /**< Expected input format */
    gf_img_format_t output_format;  /**< Desired output format */
} gf_img_pipeline_t;

/**
 * @brief Processing statistics
 */
typedef struct {
    uint32_t frames_processed;      /**< Total frames processed */
    uint32_t processing_time_us;    /**< Last frame processing time */
    uint32_t avg_processing_us;     /**< Average processing time */
    uint32_t max_processing_us;     /**< Maximum processing time */
    uint32_t dropped_frames;        /**< Frames dropped due to overrun */
} gf_img_stats_t;

/* ─────────────────────────────────────────────────────────────────────────────
 * API Functions
 * ───────────────────────────────────────────────────────────────────────────── */

/**
 * @brief Initialize image preprocessing subsystem
 * @return 0 on success, negative error code on failure
 */
int gf_img_init(void);

/**
 * @brief Shutdown preprocessing subsystem
 */
void gf_img_deinit(void);

/**
 * @brief Create processing pipeline
 * @param[out] pipeline Pipeline configuration to initialize
 * @return 0 on success
 */
int gf_img_create_pipeline(gf_img_pipeline_t* pipeline);

/**
 * @brief Add processing stage to pipeline
 * @param pipeline Pipeline to modify
 * @param stage Stage configuration
 * @return 0 on success, -1 if pipeline full
 */
int gf_img_add_stage(gf_img_pipeline_t* pipeline, const gf_img_stage_config_t* stage);

/**
 * @brief Process image through pipeline
 * @param pipeline Processing pipeline
 * @param input Input image buffer
 * @param output Output image buffer (pre-allocated)
 * @return 0 on success, negative error code on failure
 */
int gf_img_process(const gf_img_pipeline_t* pipeline,
                   const gf_img_buffer_t* input,
                   gf_img_buffer_t* output);

/**
 * @brief Process in-place (where supported)
 * @param pipeline Processing pipeline
 * @param image Image buffer to process in-place
 * @return 0 on success
 */
int gf_img_process_inplace(const gf_img_pipeline_t* pipeline, gf_img_buffer_t* image);

/**
 * @brief Allocate image buffer
 * @param width Image width
 * @param height Image height
 * @param format Pixel format
 * @return Allocated buffer, or NULL on failure
 */
gf_img_buffer_t* gf_img_alloc_buffer(uint16_t width, uint16_t height, gf_img_format_t format);

/**
 * @brief Free image buffer
 * @param buffer Buffer to free
 */
void gf_img_free_buffer(gf_img_buffer_t* buffer);

/**
 * @brief Get processing statistics
 * @param[out] stats Statistics output
 */
void gf_img_get_stats(gf_img_stats_t* stats);

/**
 * @brief Reset processing statistics
 */
void gf_img_reset_stats(void);

/**
 * @brief Enable hardware acceleration (if available)
 * @param enable True to enable
 * @return 0 on success, -1 if not supported
 */
int gf_img_enable_hw_accel(bool enable);

#ifdef __cplusplus
}
#endif

#endif /* GF_IMAGE_PREPROCESSING_H */
