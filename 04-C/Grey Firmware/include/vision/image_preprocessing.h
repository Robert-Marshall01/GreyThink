/**
 * @file image_preprocessing.h
 * @brief Image Preprocessing Pipeline Stub
 * 
 * INDUSTRY RELEVANCE:
 * Image preprocessing is the foundation of every computer vision pipeline,
 * from industrial inspection to autonomous vehicles. On embedded systems,
 * efficient preprocessing determines overall system latency and power
 * consumption. Firmware engineers must optimize pixel operations using
 * SIMD, DSP accelerators, or dedicated hardware blocks.
 * 
 * Key challenges:
 * - Real-time processing (30+ fps at VGA+)
 * - Memory-efficient operations (in-place when possible)
 * - Fixed-point optimization for processors without FPU
 * - Hardware accelerator integration (GPU, NPU, DSP)
 * - Quality vs. speed trade-offs
 */

#ifndef GF_IMAGE_PREPROCESSING_H
#define GF_IMAGE_PREPROCESSING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Preprocessing status codes */
typedef enum {
    GF_IMG_OK = 0,
    GF_IMG_NULL_POINTER,            /* Null buffer pointer */
    GF_IMG_INVALID_SIZE,            /* Invalid dimensions */
    GF_IMG_UNSUPPORTED_FORMAT,      /* Unsupported pixel format */
    GF_IMG_MEMORY_ERROR,            /* Memory allocation failed */
    GF_IMG_ACCEL_ERROR,             /* Hardware accelerator error */
    GF_IMG_PARAM_ERROR              /* Invalid parameter */
} gf_img_status_t;

/* Supported pixel formats */
typedef enum {
    GF_IMG_FMT_GRAY8,               /* 8-bit grayscale */
    GF_IMG_FMT_RGB888,              /* 24-bit RGB */
    GF_IMG_FMT_BGR888,              /* 24-bit BGR */
    GF_IMG_FMT_RGB565,              /* 16-bit RGB */
    GF_IMG_FMT_YUV422,              /* YUV 4:2:2 */
    GF_IMG_FMT_FLOAT32              /* 32-bit float (normalized) */
} gf_img_format_t;

/* Interpolation methods for resize */
typedef enum {
    GF_INTERP_NEAREST,              /* Nearest neighbor */
    GF_INTERP_BILINEAR,             /* Bilinear interpolation */
    GF_INTERP_BICUBIC,              /* Bicubic interpolation */
    GF_INTERP_AREA                  /* Area averaging (for downscale) */
} gf_img_interp_t;

/* Convolution kernel types */
typedef enum {
    GF_KERNEL_BLUR_3X3,             /* 3x3 box blur */
    GF_KERNEL_BLUR_5X5,             /* 5x5 Gaussian blur */
    GF_KERNEL_SHARPEN,              /* Sharpening kernel */
    GF_KERNEL_EDGE_SOBEL_X,         /* Sobel X gradient */
    GF_KERNEL_EDGE_SOBEL_Y,         /* Sobel Y gradient */
    GF_KERNEL_EDGE_LAPLACIAN,       /* Laplacian edge detect */
    GF_KERNEL_EMBOSS,               /* Emboss effect */
    GF_KERNEL_CUSTOM                /* User-defined kernel */
} gf_img_kernel_t;

/* Threshold methods */
typedef enum {
    GF_THRESH_BINARY,               /* Binary threshold */
    GF_THRESH_BINARY_INV,           /* Inverse binary */
    GF_THRESH_OTSU,                 /* Otsu's adaptive */
    GF_THRESH_ADAPTIVE_MEAN,        /* Adaptive mean */
    GF_THRESH_ADAPTIVE_GAUSSIAN     /* Adaptive Gaussian */
} gf_img_thresh_t;

/* Image buffer descriptor */
typedef struct {
    void* data;                     /* Pixel data pointer */
    uint16_t width;                 /* Image width */
    uint16_t height;                /* Image height */
    uint16_t stride;                /* Row stride in bytes */
    gf_img_format_t format;         /* Pixel format */
} gf_img_buffer_t;

/* Preprocessing configuration */
typedef struct {
    bool use_hw_accel;              /* Use hardware acceleration */
    uint8_t num_threads;            /* Thread count (0 = auto) */
    bool allow_in_place;            /* Allow in-place operations */
    size_t scratch_size;            /* Scratch buffer size */
    void* scratch_buffer;           /* Pre-allocated scratch memory */
} gf_img_config_t;

/* Histogram data */
typedef struct {
    uint32_t bins[256];             /* Histogram bins */
    uint8_t min_value;              /* Minimum pixel value */
    uint8_t max_value;              /* Maximum pixel value */
    float mean;                     /* Mean value */
    float std_dev;                  /* Standard deviation */
} gf_img_histogram_t;

/* Region of Interest */
typedef struct {
    uint16_t x;                     /* Top-left X */
    uint16_t y;                     /* Top-left Y */
    uint16_t width;                 /* ROI width */
    uint16_t height;                /* ROI height */
} gf_img_roi_t;

/**
 * @brief Initialize image preprocessing subsystem
 * @param config Processing configuration
 * @return Status code
 */
gf_img_status_t gf_img_init(const gf_img_config_t* config);

/**
 * @brief Convert image to grayscale
 * @param src Source image (RGB/BGR)
 * @param dst Destination image (grayscale)
 * @return Status code
 */
gf_img_status_t gf_img_to_grayscale(const gf_img_buffer_t* src, gf_img_buffer_t* dst);

/**
 * @brief Normalize pixel values to 0.0-1.0 range
 * @param src Source image
 * @param dst Destination image (float32)
 * @return Status code
 */
gf_img_status_t gf_img_normalize(const gf_img_buffer_t* src, gf_img_buffer_t* dst);

/**
 * @brief Resize image
 * @param src Source image
 * @param dst Destination image
 * @param interp Interpolation method
 * @return Status code
 */
gf_img_status_t gf_img_resize(const gf_img_buffer_t* src, gf_img_buffer_t* dst, gf_img_interp_t interp);

/**
 * @brief Apply convolution kernel
 * @param src Source image
 * @param dst Destination image
 * @param kernel Kernel type
 * @param custom_kernel Custom kernel data (for GF_KERNEL_CUSTOM)
 * @param kernel_size Kernel size (3, 5, or 7)
 * @return Status code
 */
gf_img_status_t gf_img_convolve(const gf_img_buffer_t* src, gf_img_buffer_t* dst, 
                                 gf_img_kernel_t kernel, const float* custom_kernel, uint8_t kernel_size);

/**
 * @brief Apply threshold
 * @param src Source image (grayscale)
 * @param dst Destination image
 * @param method Threshold method
 * @param threshold Threshold value (for fixed methods)
 * @return Status code
 */
gf_img_status_t gf_img_threshold(const gf_img_buffer_t* src, gf_img_buffer_t* dst,
                                  gf_img_thresh_t method, uint8_t threshold);

/**
 * @brief Apply histogram equalization
 * @param src Source image (grayscale)
 * @param dst Destination image
 * @return Status code
 */
gf_img_status_t gf_img_equalize_hist(const gf_img_buffer_t* src, gf_img_buffer_t* dst);

/**
 * @brief Calculate image histogram
 * @param src Source image (grayscale)
 * @param histogram Output histogram
 * @return Status code
 */
gf_img_status_t gf_img_calc_histogram(const gf_img_buffer_t* src, gf_img_histogram_t* histogram);

/**
 * @brief Extract Region of Interest
 * @param src Source image
 * @param dst Destination image
 * @param roi Region of interest
 * @return Status code
 */
gf_img_status_t gf_img_extract_roi(const gf_img_buffer_t* src, gf_img_buffer_t* dst, 
                                    const gf_img_roi_t* roi);

/**
 * @brief Flip image horizontally or vertically
 * @param src Source image
 * @param dst Destination image
 * @param flip_horizontal Flip horizontally
 * @param flip_vertical Flip vertically
 * @return Status code
 */
gf_img_status_t gf_img_flip(const gf_img_buffer_t* src, gf_img_buffer_t* dst,
                             bool flip_horizontal, bool flip_vertical);

/**
 * @brief Rotate image by 90-degree increments
 * @param src Source image
 * @param dst Destination image
 * @param degrees Rotation (90, 180, or 270)
 * @return Status code
 */
gf_img_status_t gf_img_rotate(const gf_img_buffer_t* src, gf_img_buffer_t* dst, uint16_t degrees);

/**
 * @brief Apply Gaussian blur
 * @param src Source image
 * @param dst Destination image
 * @param sigma Blur sigma (0 = auto from kernel size)
 * @param kernel_size Kernel size (3, 5, or 7)
 * @return Status code
 */
gf_img_status_t gf_img_gaussian_blur(const gf_img_buffer_t* src, gf_img_buffer_t* dst,
                                      float sigma, uint8_t kernel_size);

/**
 * @brief Shutdown and release resources
 */
void gf_img_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_IMAGE_PREPROCESSING_H */
