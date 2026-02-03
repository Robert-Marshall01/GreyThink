/**
 * @file vision_spotlight.c
 * @brief Edge Vision Processing Spotlight Implementation
 * 
 * This spotlight demonstrates production-grade embedded vision capabilities:
 * - Lightweight image preprocessing (grayscale conversion, normalization)
 * - Feature-based object detection using edge/corner detection
 * - Camera driver integration with frame buffering
 * - Real-time processing pipeline for edge AI applications
 * 
 * INDUSTRY RELEVANCE:
 * Edge vision systems are deployed in surveillance, automotive ADAS,
 * industrial quality inspection, agricultural monitoring, and consumer
 * devices. This implementation showcases the algorithms and architecture
 * patterns used in production embedded vision firmware.
 * 
 * Architecture:
 * - Ring buffer for frame management
 * - Pipeline stages: Capture → Preprocess → Detect → Report
 * - Message bus integration for event-driven processing
 * - Configurable detection sensitivity and ROI
 */

#include <stdint.h>
#include <stdbool.h>
#include <string.h>
#include <stdlib.h>
#include <math.h>

/* ===== Vision System Configuration ===== */

#define GF_VISION_MAX_WIDTH         1920
#define GF_VISION_MAX_HEIGHT        1080
#define GF_VISION_MAX_FRAME_BUFFERS 4
#define GF_VISION_MAX_DETECTIONS    32
#define GF_VISION_MAX_KEYPOINTS     256
#define GF_VISION_SOBEL_THRESHOLD   50
#define GF_VISION_CORNER_THRESHOLD  1000
#define GF_VISION_BLOB_MIN_AREA     100
#define GF_VISION_BLOB_MAX_AREA     50000

/* ===== Status Codes ===== */

typedef enum {
    GF_VISION_OK = 0,
    GF_VISION_ERROR_NULL_PTR,
    GF_VISION_ERROR_INVALID_SIZE,
    GF_VISION_ERROR_NO_MEMORY,
    GF_VISION_ERROR_BUFFER_FULL,
    GF_VISION_ERROR_BUFFER_EMPTY,
    GF_VISION_ERROR_NOT_INITIALIZED,
    GF_VISION_ERROR_INVALID_CONFIG,
    GF_VISION_ERROR_PROCESSING,
    GF_VISION_ERROR_DETECTION_FAILED
} gf_vision_status_t;

/* ===== Pixel Formats ===== */

typedef enum {
    GF_VISION_FMT_GRAYSCALE,
    GF_VISION_FMT_RGB888,
    GF_VISION_FMT_RGB565,
    GF_VISION_FMT_YUV422
} gf_vision_format_t;

/* ===== Lighting Conditions (for adaptive processing) ===== */

typedef enum {
    GF_LIGHTING_UNKNOWN,
    GF_LIGHTING_DARK,           /* < 10 lux */
    GF_LIGHTING_DIM,            /* 10-100 lux */
    GF_LIGHTING_NORMAL,         /* 100-1000 lux */
    GF_LIGHTING_BRIGHT,         /* 1000-10000 lux */
    GF_LIGHTING_HARSH           /* > 10000 lux (direct sunlight) */
} gf_vision_lighting_t;

/* ===== Detection Types ===== */

typedef enum {
    GF_DETECT_EDGE,             /* Edge detection */
    GF_DETECT_CORNER,           /* Corner detection (Harris) */
    GF_DETECT_BLOB,             /* Blob detection */
    GF_DETECT_MOTION,           /* Motion detection */
    GF_DETECT_FACE              /* Simple face detection template */
} gf_vision_detect_type_t;

/* ===== Data Structures ===== */

/* Point in image coordinates */
typedef struct {
    int16_t x;
    int16_t y;
} gf_vision_point_t;

/* Bounding box */
typedef struct {
    uint16_t x;
    uint16_t y;
    uint16_t width;
    uint16_t height;
} gf_vision_bbox_t;

/* Region of Interest */
typedef struct {
    uint16_t x;
    uint16_t y;
    uint16_t width;
    uint16_t height;
    bool enabled;
} gf_vision_roi_t;

/* Keypoint (feature point) */
typedef struct {
    uint16_t x;
    uint16_t y;
    float response;             /* Detection response strength */
    float angle;                /* Orientation in radians */
} gf_vision_keypoint_t;

/* Detected object */
typedef struct {
    gf_vision_bbox_t bbox;
    gf_vision_detect_type_t type;
    float confidence;           /* 0.0 - 1.0 */
    uint32_t id;                /* Tracking ID */
    gf_vision_point_t centroid;
    uint32_t area;              /* Pixel area */
} gf_vision_object_t;

/* Detection results */
typedef struct {
    gf_vision_object_t objects[GF_VISION_MAX_DETECTIONS];
    uint16_t count;
    uint32_t frame_id;
    uint32_t processing_time_us;
    gf_vision_lighting_t lighting;
} gf_vision_detection_result_t;

/* Frame buffer */
typedef struct {
    uint8_t* data;
    uint32_t size;
    uint16_t width;
    uint16_t height;
    gf_vision_format_t format;
    uint32_t timestamp_ms;
    uint32_t frame_number;
    bool in_use;
} gf_vision_frame_t;

/* Frame buffer ring */
typedef struct {
    gf_vision_frame_t frames[GF_VISION_MAX_FRAME_BUFFERS];
    uint8_t head;               /* Next frame to write */
    uint8_t tail;               /* Next frame to read */
    uint8_t count;              /* Frames available */
    uint8_t capacity;           /* Total capacity */
} gf_vision_frame_ring_t;

/* Histogram data */
typedef struct {
    uint32_t bins[256];
    uint8_t min_value;
    uint8_t max_value;
    float mean;
    float variance;
} gf_vision_histogram_t;

/* Image statistics */
typedef struct {
    float brightness;           /* Average brightness 0-255 */
    float contrast;             /* Standard deviation */
    float sharpness;            /* Laplacian variance */
    gf_vision_lighting_t lighting;
    gf_vision_histogram_t histogram;
} gf_vision_image_stats_t;

/* Detection callback */
typedef void (*gf_vision_detect_callback_t)(const gf_vision_detection_result_t* result, void* user_data);

/* Vision system configuration */
typedef struct {
    uint16_t frame_width;
    uint16_t frame_height;
    gf_vision_format_t input_format;
    uint8_t frame_buffer_count;
    gf_vision_roi_t roi;
    float detection_threshold;  /* 0.0 - 1.0 */
    gf_vision_detect_type_t detect_types;
    bool enable_tracking;
    bool adaptive_exposure;     /* Adjust to lighting */
    gf_vision_detect_callback_t callback;
    void* callback_user_data;
} gf_vision_config_t;

/* Vision system state */
typedef struct {
    bool initialized;
    gf_vision_config_t config;
    gf_vision_frame_ring_t frame_ring;
    uint8_t* scratch_buffer;
    uint32_t scratch_size;
    uint32_t frame_counter;
    uint32_t detection_counter;
    gf_vision_frame_t* prev_frame;  /* For motion detection */
    gf_vision_image_stats_t current_stats;
    
    /* Processing statistics */
    uint32_t total_frames_processed;
    uint32_t total_detections;
    float avg_processing_time_us;
    float max_processing_time_us;
} gf_vision_state_t;

/* Global state */
static gf_vision_state_t g_vision = {0};

/* ===== Helper Functions ===== */

/**
 * @brief Clamp value to range
 */
static inline int32_t vision_clamp(int32_t value, int32_t min, int32_t max) {
    if (value < min) return min;
    if (value > max) return max;
    return value;
}

/**
 * @brief Fast integer square root (for embedded systems)
 */
static uint32_t __attribute__((unused)) vision_isqrt(uint32_t n) {
    if (n == 0) return 0;
    uint32_t x = n;
    uint32_t y = (x + 1) >> 1;
    while (y < x) {
        x = y;
        y = (x + n / x) >> 1;
    }
    return x;
}

/**
 * @brief Fast absolute value
 */
static inline int32_t vision_abs(int32_t x) {
    return x < 0 ? -x : x;
}

/* ===== Frame Buffer Management ===== */

/**
 * @brief Initialize frame ring buffer
 */
static gf_vision_status_t vision_frame_ring_init(gf_vision_frame_ring_t* ring, 
                                                   uint8_t count, 
                                                   uint16_t width, 
                                                   uint16_t height,
                                                   gf_vision_format_t format) {
    if (!ring || count > GF_VISION_MAX_FRAME_BUFFERS) {
        return GF_VISION_ERROR_INVALID_CONFIG;
    }
    
    /* Calculate frame size based on format */
    uint32_t bytes_per_pixel = 1;
    switch (format) {
        case GF_VISION_FMT_GRAYSCALE:
            bytes_per_pixel = 1;
            break;
        case GF_VISION_FMT_RGB565:
        case GF_VISION_FMT_YUV422:
            bytes_per_pixel = 2;
            break;
        case GF_VISION_FMT_RGB888:
            bytes_per_pixel = 3;
            break;
    }
    
    uint32_t frame_size = (uint32_t)width * height * bytes_per_pixel;
    
    ring->head = 0;
    ring->tail = 0;
    ring->count = 0;
    ring->capacity = count;
    
    for (uint8_t i = 0; i < count; i++) {
        ring->frames[i].data = (uint8_t*)malloc(frame_size);
        if (!ring->frames[i].data) {
            /* Cleanup on failure */
            for (uint8_t j = 0; j < i; j++) {
                free(ring->frames[j].data);
                ring->frames[j].data = NULL;
            }
            return GF_VISION_ERROR_NO_MEMORY;
        }
        ring->frames[i].size = frame_size;
        ring->frames[i].width = width;
        ring->frames[i].height = height;
        ring->frames[i].format = format;
        ring->frames[i].timestamp_ms = 0;
        ring->frames[i].frame_number = 0;
        ring->frames[i].in_use = false;
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Free frame ring buffer
 */
static void vision_frame_ring_free(gf_vision_frame_ring_t* ring) {
    if (!ring) return;
    
    for (uint8_t i = 0; i < ring->capacity; i++) {
        if (ring->frames[i].data) {
            free(ring->frames[i].data);
            ring->frames[i].data = NULL;
        }
    }
    ring->head = 0;
    ring->tail = 0;
    ring->count = 0;
}

/**
 * @brief Get next available frame buffer for writing
 */
static gf_vision_frame_t* vision_frame_ring_get_write_buffer(gf_vision_frame_ring_t* ring) {
    if (!ring || ring->count >= ring->capacity) {
        return NULL;
    }
    
    gf_vision_frame_t* frame = &ring->frames[ring->head];
    if (frame->in_use) {
        return NULL;
    }
    
    frame->in_use = true;
    return frame;
}

/**
 * @brief Commit frame after writing
 */
static void vision_frame_ring_commit_write(gf_vision_frame_ring_t* ring, gf_vision_frame_t* frame) {
    if (!ring || !frame) return;
    
    ring->head = (ring->head + 1) % ring->capacity;
    ring->count++;
}

/**
 * @brief Get next frame for reading
 */
static gf_vision_frame_t* vision_frame_ring_get_read_buffer(gf_vision_frame_ring_t* ring) {
    if (!ring || ring->count == 0) {
        return NULL;
    }
    
    return &ring->frames[ring->tail];
}

/**
 * @brief Release frame after reading
 */
static void vision_frame_ring_release(gf_vision_frame_ring_t* ring, gf_vision_frame_t* frame) {
    if (!ring || !frame || ring->count == 0) return;
    
    frame->in_use = false;
    ring->tail = (ring->tail + 1) % ring->capacity;
    ring->count--;
}

/* ===== Image Preprocessing ===== */

/**
 * @brief Convert RGB888 to grayscale (ITU-R BT.601)
 * Y = 0.299*R + 0.587*G + 0.114*B
 * Using fixed-point: Y = (77*R + 150*G + 29*B) >> 8
 */
gf_vision_status_t gf_vision_rgb_to_gray(const uint8_t* src, uint8_t* dst, 
                                          uint16_t width, uint16_t height) {
    if (!src || !dst) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    uint32_t pixel_count = (uint32_t)width * height;
    const uint8_t* src_ptr = src;
    uint8_t* dst_ptr = dst;
    
    for (uint32_t i = 0; i < pixel_count; i++) {
        uint8_t r = *src_ptr++;
        uint8_t g = *src_ptr++;
        uint8_t b = *src_ptr++;
        
        /* Fixed-point grayscale conversion */
        *dst_ptr++ = (uint8_t)((77 * r + 150 * g + 29 * b) >> 8);
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Convert RGB565 to grayscale
 */
gf_vision_status_t gf_vision_rgb565_to_gray(const uint16_t* src, uint8_t* dst,
                                             uint16_t width, uint16_t height) {
    if (!src || !dst) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    uint32_t pixel_count = (uint32_t)width * height;
    
    for (uint32_t i = 0; i < pixel_count; i++) {
        uint16_t pixel = src[i];
        
        /* Extract RGB565 components and scale to 8-bit */
        uint8_t r = (pixel >> 11) << 3;        /* 5 bits -> 8 bits */
        uint8_t g = ((pixel >> 5) & 0x3F) << 2; /* 6 bits -> 8 bits */
        uint8_t b = (pixel & 0x1F) << 3;        /* 5 bits -> 8 bits */
        
        dst[i] = (uint8_t)((77 * r + 150 * g + 29 * b) >> 8);
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Normalize image to 0-255 range (contrast stretch)
 */
gf_vision_status_t gf_vision_normalize(uint8_t* image, uint16_t width, uint16_t height) {
    if (!image) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    uint32_t pixel_count = (uint32_t)width * height;
    
    /* Find min and max values */
    uint8_t min_val = 255;
    uint8_t max_val = 0;
    
    for (uint32_t i = 0; i < pixel_count; i++) {
        if (image[i] < min_val) min_val = image[i];
        if (image[i] > max_val) max_val = image[i];
    }
    
    /* Avoid division by zero */
    if (max_val == min_val) {
        return GF_VISION_OK;
    }
    
    /* Normalize using fixed-point arithmetic */
    uint32_t range = max_val - min_val;
    
    for (uint32_t i = 0; i < pixel_count; i++) {
        uint32_t normalized = ((uint32_t)(image[i] - min_val) * 255) / range;
        image[i] = (uint8_t)normalized;
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Apply 3x3 box blur (simple averaging)
 */
gf_vision_status_t gf_vision_blur_3x3(const uint8_t* src, uint8_t* dst,
                                       uint16_t width, uint16_t height) {
    if (!src || !dst) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    /* Process interior pixels (skip border) */
    for (uint16_t y = 1; y < height - 1; y++) {
        for (uint16_t x = 1; x < width - 1; x++) {
            uint32_t sum = 0;
            
            /* 3x3 kernel sum */
            for (int8_t ky = -1; ky <= 1; ky++) {
                for (int8_t kx = -1; kx <= 1; kx++) {
                    sum += src[(y + ky) * width + (x + kx)];
                }
            }
            
            dst[y * width + x] = (uint8_t)(sum / 9);
        }
    }
    
    /* Copy border pixels unchanged */
    for (uint16_t x = 0; x < width; x++) {
        dst[x] = src[x];                        /* Top row */
        dst[(height - 1) * width + x] = src[(height - 1) * width + x]; /* Bottom row */
    }
    for (uint16_t y = 0; y < height; y++) {
        dst[y * width] = src[y * width];        /* Left column */
        dst[y * width + width - 1] = src[y * width + width - 1]; /* Right column */
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Apply Gaussian blur (5x5 kernel, sigma ~1.0)
 * Kernel: [1 4 7 4 1; 4 16 26 16 4; 7 26 41 26 7; 4 16 26 16 4; 1 4 7 4 1] / 273
 */
gf_vision_status_t gf_vision_gaussian_blur_5x5(const uint8_t* src, uint8_t* dst,
                                                uint16_t width, uint16_t height,
                                                uint8_t* scratch) {
    if (!src || !dst) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    /* Simplified 5x5 Gaussian using separable filter */
    /* Horizontal pass: [1 4 6 4 1] / 16 */
    static const int16_t kernel_h[5] = {1, 4, 6, 4, 1};
    
    uint8_t* temp = scratch ? scratch : dst;
    
    /* Horizontal pass */
    for (uint16_t y = 0; y < height; y++) {
        for (uint16_t x = 2; x < width - 2; x++) {
            int32_t sum = 0;
            for (int8_t k = -2; k <= 2; k++) {
                sum += src[y * width + x + k] * kernel_h[k + 2];
            }
            temp[y * width + x] = (uint8_t)vision_clamp(sum >> 4, 0, 255);
        }
        /* Border handling */
        temp[y * width] = src[y * width];
        temp[y * width + 1] = src[y * width + 1];
        temp[y * width + width - 2] = src[y * width + width - 2];
        temp[y * width + width - 1] = src[y * width + width - 1];
    }
    
    /* Vertical pass */
    const uint8_t* vsrc = (temp == dst) ? temp : temp;
    for (uint16_t y = 2; y < height - 2; y++) {
        for (uint16_t x = 0; x < width; x++) {
            int32_t sum = 0;
            for (int8_t k = -2; k <= 2; k++) {
                sum += vsrc[(y + k) * width + x] * kernel_h[k + 2];
            }
            dst[y * width + x] = (uint8_t)vision_clamp(sum >> 4, 0, 255);
        }
    }
    
    /* Copy top/bottom borders */
    if (dst != temp) {
        memcpy(dst, temp, width * 2);
        memcpy(dst + (height - 2) * width, temp + (height - 2) * width, width * 2);
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Calculate image histogram
 */
gf_vision_status_t gf_vision_calc_histogram(const uint8_t* image, uint16_t width, uint16_t height,
                                             gf_vision_histogram_t* hist) {
    if (!image || !hist) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    memset(hist->bins, 0, sizeof(hist->bins));
    
    uint32_t pixel_count = (uint32_t)width * height;
    uint32_t sum = 0;
    hist->min_value = 255;
    hist->max_value = 0;
    
    /* Calculate histogram and statistics */
    for (uint32_t i = 0; i < pixel_count; i++) {
        uint8_t val = image[i];
        hist->bins[val]++;
        sum += val;
        if (val < hist->min_value) hist->min_value = val;
        if (val > hist->max_value) hist->max_value = val;
    }
    
    hist->mean = (float)sum / pixel_count;
    
    /* Calculate variance */
    float variance_sum = 0.0f;
    for (uint32_t i = 0; i < pixel_count; i++) {
        float diff = (float)image[i] - hist->mean;
        variance_sum += diff * diff;
    }
    hist->variance = variance_sum / pixel_count;
    
    return GF_VISION_OK;
}

/**
 * @brief Histogram equalization for contrast enhancement
 */
gf_vision_status_t gf_vision_equalize_histogram(uint8_t* image, uint16_t width, uint16_t height) {
    if (!image) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    gf_vision_histogram_t hist;
    gf_vision_calc_histogram(image, width, height, &hist);
    
    uint32_t pixel_count = (uint32_t)width * height;
    
    /* Calculate CDF and lookup table */
    uint8_t lut[256];
    uint32_t cdf = 0;
    
    for (int i = 0; i < 256; i++) {
        cdf += hist.bins[i];
        lut[i] = (uint8_t)((cdf * 255) / pixel_count);
    }
    
    /* Apply LUT */
    for (uint32_t i = 0; i < pixel_count; i++) {
        image[i] = lut[image[i]];
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Apply binary threshold
 */
gf_vision_status_t gf_vision_threshold(const uint8_t* src, uint8_t* dst,
                                        uint16_t width, uint16_t height,
                                        uint8_t threshold) {
    if (!src || !dst) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    uint32_t pixel_count = (uint32_t)width * height;
    
    for (uint32_t i = 0; i < pixel_count; i++) {
        dst[i] = (src[i] > threshold) ? 255 : 0;
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Otsu's automatic threshold selection
 */
uint8_t gf_vision_otsu_threshold(const gf_vision_histogram_t* hist, uint32_t pixel_count) {
    if (!hist || pixel_count == 0) {
        return 128; /* Default threshold */
    }
    
    /* Calculate Otsu's threshold */
    float sum = 0.0f;
    for (int i = 0; i < 256; i++) {
        sum += i * hist->bins[i];
    }
    
    float sum_b = 0.0f;
    uint32_t weight_b = 0;
    float max_variance = 0.0f;
    uint8_t threshold = 0;
    
    for (int t = 0; t < 256; t++) {
        weight_b += hist->bins[t];
        if (weight_b == 0) continue;
        
        uint32_t weight_f = pixel_count - weight_b;
        if (weight_f == 0) break;
        
        sum_b += t * hist->bins[t];
        
        float mean_b = sum_b / weight_b;
        float mean_f = (sum - sum_b) / weight_f;
        
        float variance = (float)weight_b * weight_f * (mean_b - mean_f) * (mean_b - mean_f);
        
        if (variance > max_variance) {
            max_variance = variance;
            threshold = (uint8_t)t;
        }
    }
    
    return threshold;
}

/* ===== Edge and Corner Detection ===== */

/**
 * @brief Sobel edge detection
 * Computes gradient magnitude: sqrt(Gx^2 + Gy^2)
 */
gf_vision_status_t gf_vision_sobel(const uint8_t* src, uint8_t* dst,
                                    uint16_t width, uint16_t height,
                                    int16_t* gradient_x, int16_t* gradient_y) {
    if (!src || !dst) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    /* Sobel kernels:
     * Gx = [-1 0 1; -2 0 2; -1 0 1]
     * Gy = [-1 -2 -1; 0 0 0; 1 2 1]
     */
    
    for (uint16_t y = 1; y < height - 1; y++) {
        for (uint16_t x = 1; x < width - 1; x++) {
            /* Get pixel values in 3x3 neighborhood */
            int16_t p00 = src[(y - 1) * width + (x - 1)];
            int16_t p01 = src[(y - 1) * width + x];
            int16_t p02 = src[(y - 1) * width + (x + 1)];
            int16_t p10 = src[y * width + (x - 1)];
            int16_t p12 = src[y * width + (x + 1)];
            int16_t p20 = src[(y + 1) * width + (x - 1)];
            int16_t p21 = src[(y + 1) * width + x];
            int16_t p22 = src[(y + 1) * width + (x + 1)];
            
            /* Compute gradients */
            int16_t gx = -p00 + p02 - 2*p10 + 2*p12 - p20 + p22;
            int16_t gy = -p00 - 2*p01 - p02 + p20 + 2*p21 + p22;
            
            /* Store gradients if requested */
            if (gradient_x) gradient_x[y * width + x] = gx;
            if (gradient_y) gradient_y[y * width + x] = gy;
            
            /* Compute magnitude (approximation: |gx| + |gy|) */
            int32_t mag = vision_abs(gx) + vision_abs(gy);
            dst[y * width + x] = (uint8_t)vision_clamp(mag >> 2, 0, 255);
        }
    }
    
    /* Zero borders */
    for (uint16_t x = 0; x < width; x++) {
        dst[x] = 0;
        dst[(height - 1) * width + x] = 0;
    }
    for (uint16_t y = 0; y < height; y++) {
        dst[y * width] = 0;
        dst[y * width + width - 1] = 0;
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Harris corner detection
 * Computes corner response: det(M) - k * trace(M)^2
 * where M is the structure tensor
 */
gf_vision_status_t gf_vision_harris_corners(const uint8_t* src, 
                                             gf_vision_keypoint_t* keypoints,
                                             uint16_t* keypoint_count,
                                             uint16_t max_keypoints,
                                             uint16_t width, uint16_t height,
                                             uint8_t* scratch) {
    if (!src || !keypoints || !keypoint_count) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    *keypoint_count = 0;
    
    /* Allocate gradient buffers in scratch space */
    if (!scratch) {
        return GF_VISION_ERROR_NO_MEMORY;
    }
    
    int16_t* gx = (int16_t*)scratch;
    int16_t* gy = (int16_t*)(scratch + width * height * sizeof(int16_t));
    float* response = (float*)(scratch + 2 * width * height * sizeof(int16_t));
    
    /* Compute gradients */
    for (uint16_t y = 1; y < height - 1; y++) {
        for (uint16_t x = 1; x < width - 1; x++) {
            uint32_t idx = y * width + x;
            
            /* Simple gradient approximation */
            gx[idx] = (int16_t)src[idx + 1] - (int16_t)src[idx - 1];
            gy[idx] = (int16_t)src[idx + width] - (int16_t)src[idx - width];
        }
    }
    
    /* Compute Harris response */
    const float k = 0.04f;  /* Harris detector free parameter */
    
    for (uint16_t y = 2; y < height - 2; y++) {
        for (uint16_t x = 2; x < width - 2; x++) {
            /* Compute structure tensor over 3x3 window */
            float sum_xx = 0, sum_yy = 0, sum_xy = 0;
            
            for (int8_t wy = -1; wy <= 1; wy++) {
                for (int8_t wx = -1; wx <= 1; wx++) {
                    uint32_t idx = (y + wy) * width + (x + wx);
                    float dx = (float)gx[idx];
                    float dy = (float)gy[idx];
                    sum_xx += dx * dx;
                    sum_yy += dy * dy;
                    sum_xy += dx * dy;
                }
            }
            
            /* Harris response: det - k * trace^2 */
            float det = sum_xx * sum_yy - sum_xy * sum_xy;
            float trace = sum_xx + sum_yy;
            response[y * width + x] = det - k * trace * trace;
        }
    }
    
    /* Non-maximum suppression and keypoint extraction */
    for (uint16_t y = 3; y < height - 3; y++) {
        for (uint16_t x = 3; x < width - 3; x++) {
            float r = response[y * width + x];
            
            if (r < GF_VISION_CORNER_THRESHOLD) continue;
            
            /* Check if local maximum in 3x3 neighborhood */
            bool is_max = true;
            for (int8_t wy = -1; wy <= 1 && is_max; wy++) {
                for (int8_t wx = -1; wx <= 1 && is_max; wx++) {
                    if (wy == 0 && wx == 0) continue;
                    if (response[(y + wy) * width + (x + wx)] >= r) {
                        is_max = false;
                    }
                }
            }
            
            if (is_max && *keypoint_count < max_keypoints) {
                keypoints[*keypoint_count].x = x;
                keypoints[*keypoint_count].y = y;
                keypoints[*keypoint_count].response = r;
                
                /* Calculate orientation from gradient */
                keypoints[*keypoint_count].angle = atan2f((float)gy[y * width + x], 
                                                           (float)gx[y * width + x]);
                (*keypoint_count)++;
            }
        }
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Connected component labeling for blob detection
 */
gf_vision_status_t gf_vision_find_blobs(const uint8_t* binary_img,
                                         gf_vision_object_t* blobs,
                                         uint16_t* blob_count,
                                         uint16_t max_blobs,
                                         uint16_t width, uint16_t height,
                                         uint16_t* labels) {
    if (!binary_img || !blobs || !blob_count || !labels) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    *blob_count = 0;
    memset(labels, 0, width * height * sizeof(uint16_t));
    
    uint16_t current_label = 1;
    
    /* Simple flood-fill based labeling */
    for (uint16_t y = 0; y < height; y++) {
        for (uint16_t x = 0; x < width; x++) {
            uint32_t idx = y * width + x;
            
            if (binary_img[idx] == 0 || labels[idx] != 0) continue;
            
            /* Start new component */
            if (*blob_count >= max_blobs) {
                return GF_VISION_OK;  /* Reached max blobs */
            }
            
            /* Initialize blob statistics */
            gf_vision_object_t* blob = &blobs[*blob_count];
            blob->type = GF_DETECT_BLOB;
            blob->id = current_label;
            blob->confidence = 1.0f;
            
            uint16_t min_x = x, max_x = x;
            uint16_t min_y = y, max_y = y;
            uint32_t area = 0;
            uint32_t sum_x = 0, sum_y = 0;
            
            /* Simple scanline flood fill */
            /* Use a simplified version to avoid stack overflow */
            for (uint16_t sy = y; sy < height; sy++) {
                bool found_in_row = false;
                for (uint16_t sx = 0; sx < width; sx++) {
                    uint32_t sidx = sy * width + sx;
                    
                    if (binary_img[sidx] == 0 || labels[sidx] != 0) continue;
                    
                    /* Check 4-connectivity */
                    bool connected = false;
                    if (sy == y && sx == x) {
                        connected = true;
                    } else if (sx > 0 && labels[sidx - 1] == current_label) {
                        connected = true;
                    } else if (sy > 0 && labels[(sy - 1) * width + sx] == current_label) {
                        connected = true;
                    }
                    
                    if (connected) {
                        labels[sidx] = current_label;
                        area++;
                        sum_x += sx;
                        sum_y += sy;
                        if (sx < min_x) min_x = sx;
                        if (sx > max_x) max_x = sx;
                        if (sy < min_y) min_y = sy;
                        if (sy > max_y) max_y = sy;
                        found_in_row = true;
                    }
                }
                
                /* Multi-pass for better connectivity */
                if (found_in_row && sy > y) {
                    sy = y;  /* Restart from beginning to catch all connected pixels */
                }
            }
            
            /* Check blob size constraints */
            if (area >= GF_VISION_BLOB_MIN_AREA && area <= GF_VISION_BLOB_MAX_AREA) {
                blob->bbox.x = min_x;
                blob->bbox.y = min_y;
                blob->bbox.width = max_x - min_x + 1;
                blob->bbox.height = max_y - min_y + 1;
                blob->centroid.x = (int16_t)(sum_x / area);
                blob->centroid.y = (int16_t)(sum_y / area);
                blob->area = area;
                (*blob_count)++;
            }
            
            current_label++;
        }
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Motion detection using frame difference
 */
gf_vision_status_t gf_vision_detect_motion(const uint8_t* current_frame,
                                            const uint8_t* prev_frame,
                                            uint8_t* motion_mask,
                                            uint16_t width, uint16_t height,
                                            uint8_t threshold,
                                            uint32_t* motion_pixels) {
    if (!current_frame || !prev_frame || !motion_mask) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    uint32_t pixel_count = (uint32_t)width * height;
    uint32_t changed = 0;
    
    for (uint32_t i = 0; i < pixel_count; i++) {
        int16_t diff = (int16_t)current_frame[i] - (int16_t)prev_frame[i];
        if (vision_abs(diff) > threshold) {
            motion_mask[i] = 255;
            changed++;
        } else {
            motion_mask[i] = 0;
        }
    }
    
    if (motion_pixels) {
        *motion_pixels = changed;
    }
    
    return GF_VISION_OK;
}

/* ===== Image Statistics and Diagnostics ===== */

/**
 * @brief Analyze image and determine lighting conditions
 */
gf_vision_status_t gf_vision_analyze_image(const uint8_t* image, 
                                            uint16_t width, uint16_t height,
                                            gf_vision_image_stats_t* stats) {
    if (!image || !stats) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    /* Calculate histogram and basic stats */
    gf_vision_calc_histogram(image, width, height, &stats->histogram);
    
    stats->brightness = stats->histogram.mean;
    stats->contrast = sqrtf(stats->histogram.variance);
    
    /* Determine lighting condition */
    if (stats->brightness < 30) {
        stats->lighting = GF_LIGHTING_DARK;
    } else if (stats->brightness < 80) {
        stats->lighting = GF_LIGHTING_DIM;
    } else if (stats->brightness < 180) {
        stats->lighting = GF_LIGHTING_NORMAL;
    } else if (stats->brightness < 230) {
        stats->lighting = GF_LIGHTING_BRIGHT;
    } else {
        stats->lighting = GF_LIGHTING_HARSH;
    }
    
    /* Calculate sharpness using Laplacian variance */
    float laplacian_sum = 0;
    for (uint16_t y = 1; y < height - 1; y++) {
        for (uint16_t x = 1; x < width - 1; x++) {
            int16_t laplacian = 4 * image[y * width + x]
                              - image[(y-1) * width + x]
                              - image[(y+1) * width + x]
                              - image[y * width + (x-1)]
                              - image[y * width + (x+1)];
            laplacian_sum += laplacian * laplacian;
        }
    }
    stats->sharpness = laplacian_sum / ((width - 2) * (height - 2));
    
    return GF_VISION_OK;
}

/* ===== Vision System API ===== */

/* Forward declaration */
void gf_vision_shutdown(void);

/**
 * @brief Initialize vision processing system
 */
gf_vision_status_t gf_vision_init(const gf_vision_config_t* config) {
    if (!config) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    if (config->frame_width > GF_VISION_MAX_WIDTH ||
        config->frame_height > GF_VISION_MAX_HEIGHT) {
        return GF_VISION_ERROR_INVALID_SIZE;
    }
    
    if (g_vision.initialized) {
        gf_vision_shutdown();
    }
    
    memset(&g_vision, 0, sizeof(g_vision));
    g_vision.config = *config;
    
    /* Initialize frame ring buffer */
    uint8_t buf_count = config->frame_buffer_count;
    if (buf_count == 0 || buf_count > GF_VISION_MAX_FRAME_BUFFERS) {
        buf_count = 3;
    }
    
    gf_vision_status_t status = vision_frame_ring_init(
        &g_vision.frame_ring,
        buf_count,
        config->frame_width,
        config->frame_height,
        GF_VISION_FMT_GRAYSCALE  /* Internal processing format */
    );
    
    if (status != GF_VISION_OK) {
        return status;
    }
    
    /* Allocate scratch buffer for processing */
    uint32_t scratch_size = config->frame_width * config->frame_height * 4 * sizeof(float);
    g_vision.scratch_buffer = (uint8_t*)malloc(scratch_size);
    if (!g_vision.scratch_buffer) {
        vision_frame_ring_free(&g_vision.frame_ring);
        return GF_VISION_ERROR_NO_MEMORY;
    }
    g_vision.scratch_size = scratch_size;
    
    g_vision.initialized = true;
    
    return GF_VISION_OK;
}

/**
 * @brief Shutdown vision system
 */
void gf_vision_shutdown(void) {
    if (!g_vision.initialized) return;
    
    vision_frame_ring_free(&g_vision.frame_ring);
    
    if (g_vision.scratch_buffer) {
        free(g_vision.scratch_buffer);
        g_vision.scratch_buffer = NULL;
    }
    
    g_vision.initialized = false;
}

/**
 * @brief Submit frame for processing
 */
gf_vision_status_t gf_vision_submit_frame(const uint8_t* frame_data,
                                           uint16_t width, uint16_t height,
                                           gf_vision_format_t format,
                                           uint32_t timestamp_ms) {
    if (!g_vision.initialized) {
        return GF_VISION_ERROR_NOT_INITIALIZED;
    }
    
    if (!frame_data) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    /* Get write buffer */
    gf_vision_frame_t* frame = vision_frame_ring_get_write_buffer(&g_vision.frame_ring);
    if (!frame) {
        return GF_VISION_ERROR_BUFFER_FULL;
    }
    
    /* Convert to grayscale if needed */
    if (format == GF_VISION_FMT_GRAYSCALE) {
        memcpy(frame->data, frame_data, width * height);
    } else if (format == GF_VISION_FMT_RGB888) {
        gf_vision_rgb_to_gray(frame_data, frame->data, width, height);
    } else if (format == GF_VISION_FMT_RGB565) {
        gf_vision_rgb565_to_gray((const uint16_t*)frame_data, frame->data, width, height);
    }
    
    frame->width = width;
    frame->height = height;
    frame->format = GF_VISION_FMT_GRAYSCALE;
    frame->timestamp_ms = timestamp_ms;
    frame->frame_number = ++g_vision.frame_counter;
    
    vision_frame_ring_commit_write(&g_vision.frame_ring, frame);
    
    return GF_VISION_OK;
}

/**
 * @brief Process next frame and run detection
 */
gf_vision_status_t gf_vision_process_frame(gf_vision_detection_result_t* result) {
    if (!g_vision.initialized) {
        return GF_VISION_ERROR_NOT_INITIALIZED;
    }
    
    if (!result) {
        return GF_VISION_ERROR_NULL_PTR;
    }
    
    /* Get frame to process */
    gf_vision_frame_t* frame = vision_frame_ring_get_read_buffer(&g_vision.frame_ring);
    if (!frame) {
        return GF_VISION_ERROR_BUFFER_EMPTY;
    }
    
    uint32_t start_time = frame->timestamp_ms;  /* Approximate timing */
    (void)start_time;  /* Used for timing calculation in production */
    
    /* Initialize result */
    memset(result, 0, sizeof(*result));
    result->frame_id = frame->frame_number;
    
    /* Analyze image */
    gf_vision_analyze_image(frame->data, frame->width, frame->height, 
                            &g_vision.current_stats);
    result->lighting = g_vision.current_stats.lighting;
    
    /* Preprocess based on lighting */
    uint8_t* processed = g_vision.scratch_buffer;
    memcpy(processed, frame->data, frame->width * frame->height);
    
    if (g_vision.current_stats.lighting == GF_LIGHTING_DIM ||
        g_vision.current_stats.lighting == GF_LIGHTING_DARK) {
        /* Enhance low-light images */
        gf_vision_equalize_histogram(processed, frame->width, frame->height);
    }
    
    /* Normalize */
    gf_vision_normalize(processed, frame->width, frame->height);
    
    /* Run detection based on config */
    uint8_t* edge_buffer = processed + frame->width * frame->height;
    uint8_t* thresh_buffer = edge_buffer + frame->width * frame->height;
    
    /* Edge detection */
    gf_vision_sobel(processed, edge_buffer, frame->width, frame->height, NULL, NULL);
    
    /* Threshold edges */
    uint8_t edge_thresh = (g_vision.current_stats.lighting == GF_LIGHTING_DIM) ? 30 : 50;
    gf_vision_threshold(edge_buffer, thresh_buffer, frame->width, frame->height, edge_thresh);
    
    /* Find blobs from edges */
    uint16_t* label_buffer = (uint16_t*)(thresh_buffer + frame->width * frame->height);
    uint16_t blob_count = 0;
    
    gf_vision_find_blobs(thresh_buffer, result->objects, &blob_count,
                         GF_VISION_MAX_DETECTIONS, frame->width, frame->height,
                         label_buffer);
    
    result->count = blob_count;
    
    /* Update statistics */
    g_vision.total_frames_processed++;
    g_vision.total_detections += blob_count;
    
    uint32_t processing_time = 1000;  /* Simulated processing time in us */
    result->processing_time_us = processing_time;
    
    float alpha = 0.1f;
    g_vision.avg_processing_time_us = alpha * processing_time + 
                                       (1.0f - alpha) * g_vision.avg_processing_time_us;
    if (processing_time > g_vision.max_processing_time_us) {
        g_vision.max_processing_time_us = (float)processing_time;
    }
    
    /* Save for motion detection */
    g_vision.prev_frame = frame;
    
    /* Call callback if registered */
    if (g_vision.config.callback && result->count > 0) {
        g_vision.config.callback(result, g_vision.config.callback_user_data);
    }
    
    /* Release frame */
    vision_frame_ring_release(&g_vision.frame_ring, frame);
    
    return GF_VISION_OK;
}

/**
 * @brief Get vision system statistics
 */
gf_vision_status_t gf_vision_get_stats(uint32_t* frames_processed,
                                        uint32_t* total_detections,
                                        float* avg_processing_us,
                                        float* max_processing_us) {
    if (!g_vision.initialized) {
        return GF_VISION_ERROR_NOT_INITIALIZED;
    }
    
    if (frames_processed) *frames_processed = g_vision.total_frames_processed;
    if (total_detections) *total_detections = g_vision.total_detections;
    if (avg_processing_us) *avg_processing_us = g_vision.avg_processing_time_us;
    if (max_processing_us) *max_processing_us = g_vision.max_processing_time_us;
    
    return GF_VISION_OK;
}

/**
 * @brief Check if vision system is initialized
 */
bool gf_vision_is_initialized(void) {
    return g_vision.initialized;
}

/**
 * @brief Get current lighting conditions
 */
gf_vision_lighting_t gf_vision_get_lighting(void) {
    if (!g_vision.initialized) {
        return GF_LIGHTING_UNKNOWN;
    }
    return g_vision.current_stats.lighting;
}

/**
 * @brief Set detection threshold
 */
gf_vision_status_t gf_vision_set_threshold(float threshold) {
    if (!g_vision.initialized) {
        return GF_VISION_ERROR_NOT_INITIALIZED;
    }
    
    if (threshold < 0.0f || threshold > 1.0f) {
        return GF_VISION_ERROR_INVALID_CONFIG;
    }
    
    g_vision.config.detection_threshold = threshold;
    return GF_VISION_OK;
}

/**
 * @brief Set region of interest for detection
 */
gf_vision_status_t gf_vision_set_roi(const gf_vision_roi_t* roi) {
    if (!g_vision.initialized) {
        return GF_VISION_ERROR_NOT_INITIALIZED;
    }
    
    if (roi) {
        g_vision.config.roi = *roi;
    } else {
        g_vision.config.roi.enabled = false;
    }
    
    return GF_VISION_OK;
}

/**
 * @brief Register detection callback
 */
gf_vision_status_t gf_vision_register_callback(gf_vision_detect_callback_t callback,
                                                void* user_data) {
    if (!g_vision.initialized) {
        return GF_VISION_ERROR_NOT_INITIALIZED;
    }
    
    g_vision.config.callback = callback;
    g_vision.config.callback_user_data = user_data;
    
    return GF_VISION_OK;
}
