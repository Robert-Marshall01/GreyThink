/**
 * @file object_detection.h
 * @brief Object Detection Stub
 * 
 * INDUSTRY RELEVANCE:
 * Object detection is fundamental to autonomous vehicles, security systems,
 * retail analytics, and industrial automation. The edge AI market exceeds
 * $15B with embedded object detection as a key driver. Firmware engineers
 * work with neural network inference engines, model quantization, and
 * hardware accelerators (NPU, GPU, TPU) to achieve real-time performance.
 * 
 * Key challenges:
 * - Real-time inference (10+ fps minimum)
 * - Model optimization for memory-constrained devices
 * - Multiple object tracking across frames
 * - Confidence calibration and threshold tuning
 * - Power efficiency for always-on detection
 */

#ifndef GF_OBJECT_DETECTION_H
#define GF_OBJECT_DETECTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Object detection status codes */
typedef enum {
    GF_DET_OK = 0,
    GF_DET_MODEL_ERROR,             /* Model load/inference error */
    GF_DET_MEMORY_ERROR,            /* Memory allocation failed */
    GF_DET_INPUT_ERROR,             /* Invalid input format */
    GF_DET_NO_DETECTIONS,           /* No objects detected */
    GF_DET_ACCEL_ERROR,             /* Hardware accelerator error */
    GF_DET_TIMEOUT,                 /* Inference timeout */
    GF_DET_NOT_INITIALIZED          /* Subsystem not initialized */
} gf_det_status_t;

/* Detection model types */
typedef enum {
    GF_MODEL_FEATURE_BASED,         /* Traditional feature extraction */
    GF_MODEL_HOG_SVM,               /* HOG + SVM classifier */
    GF_MODEL_MOBILENET_SSD,         /* MobileNet SSD */
    GF_MODEL_YOLO_TINY,             /* YOLO Tiny */
    GF_MODEL_CUSTOM                 /* Custom model */
} gf_det_model_t;

/* Object class categories */
typedef enum {
    GF_CLASS_UNKNOWN = 0,
    GF_CLASS_PERSON,
    GF_CLASS_FACE,
    GF_CLASS_VEHICLE,
    GF_CLASS_ANIMAL,
    GF_CLASS_HAND,
    GF_CLASS_TEXT,
    GF_CLASS_QR_CODE,
    GF_CLASS_CUSTOM_START = 100     /* Custom classes start here */
} gf_det_class_t;

/* Bounding box */
typedef struct {
    uint16_t x;                     /* Top-left X coordinate */
    uint16_t y;                     /* Top-left Y coordinate */
    uint16_t width;                 /* Box width */
    uint16_t height;                /* Box height */
} gf_det_bbox_t;

/* Single detection result */
typedef struct {
    gf_det_bbox_t bbox;             /* Bounding box */
    gf_det_class_t class_id;        /* Object class */
    float confidence;               /* Detection confidence (0.0-1.0) */
    uint32_t track_id;              /* Tracking ID (0 if not tracking) */
    char label[32];                 /* Class label string */
} gf_det_object_t;

/* Detection results */
typedef struct {
    gf_det_object_t* objects;       /* Array of detected objects */
    uint16_t count;                 /* Number of detections */
    uint16_t max_count;             /* Maximum capacity */
    uint32_t frame_id;              /* Source frame ID */
    uint32_t inference_time_ms;     /* Inference duration */
} gf_det_result_t;

/* Detector configuration */
typedef struct {
    gf_det_model_t model;           /* Detection model type */
    const void* model_data;         /* Model weights/parameters */
    size_t model_size;              /* Model data size */
    float confidence_threshold;     /* Minimum confidence (default 0.5) */
    float nms_threshold;            /* Non-max suppression threshold */
    uint16_t max_detections;        /* Maximum detections per frame */
    bool enable_tracking;           /* Enable object tracking */
    bool use_hw_accel;              /* Use hardware acceleration */
    uint8_t num_threads;            /* Inference threads (0 = auto) */
} gf_det_config_t;

/* Feature point for feature-based detection */
typedef struct {
    uint16_t x;                     /* X coordinate */
    uint16_t y;                     /* Y coordinate */
    float response;                 /* Feature response strength */
    float angle;                    /* Feature orientation */
    uint8_t octave;                 /* Scale octave */
} gf_det_keypoint_t;

/* Feature extraction result */
typedef struct {
    gf_det_keypoint_t* keypoints;   /* Detected keypoints */
    uint16_t count;                 /* Number of keypoints */
    uint8_t* descriptors;           /* Feature descriptors */
    uint16_t desc_size;             /* Descriptor size per keypoint */
} gf_det_features_t;

/* Performance statistics */
typedef struct {
    uint32_t frames_processed;      /* Total frames processed */
    uint32_t total_detections;      /* Total objects detected */
    float avg_inference_ms;         /* Average inference time */
    float max_inference_ms;         /* Maximum inference time */
    float fps;                      /* Current processing FPS */
    uint32_t memory_used_kb;        /* Memory usage in KB */
} gf_det_stats_t;

/**
 * @brief Initialize object detector
 * @param config Detector configuration
 * @return Status code
 */
gf_det_status_t gf_det_init(const gf_det_config_t* config);

/**
 * @brief Run object detection on image
 * @param image Input image data
 * @param width Image width
 * @param height Image height
 * @param result Detection results
 * @return Status code
 */
gf_det_status_t gf_det_detect(const uint8_t* image, uint16_t width, uint16_t height,
                               gf_det_result_t* result);

/**
 * @brief Extract feature keypoints from image
 * @param image Input image (grayscale)
 * @param width Image width
 * @param height Image height
 * @param features Output features
 * @return Status code
 */
gf_det_status_t gf_det_extract_features(const uint8_t* image, uint16_t width, uint16_t height,
                                         gf_det_features_t* features);

/**
 * @brief Set detection confidence threshold
 * @param threshold Confidence threshold (0.0-1.0)
 * @return Status code
 */
gf_det_status_t gf_det_set_threshold(float threshold);

/**
 * @brief Enable/disable specific object class
 * @param class_id Class to enable/disable
 * @param enabled Enable flag
 * @return Status code
 */
gf_det_status_t gf_det_set_class_enabled(gf_det_class_t class_id, bool enabled);

/**
 * @brief Register callback for detection events
 * @param callback Function to call on detection
 * @param user_data User data for callback
 * @return Status code
 */
gf_det_status_t gf_det_register_callback(void (*callback)(const gf_det_result_t*, void*), 
                                          void* user_data);

/**
 * @brief Get detection statistics
 * @param stats Output for statistics
 * @return Status code
 */
gf_det_status_t gf_det_get_stats(gf_det_stats_t* stats);

/**
 * @brief Reset object tracker state
 * @return Status code
 */
gf_det_status_t gf_det_reset_tracking(void);

/**
 * @brief Free detection result memory
 * @param result Result to free
 */
void gf_det_free_result(gf_det_result_t* result);

/**
 * @brief Free features memory
 * @param features Features to free
 */
void gf_det_free_features(gf_det_features_t* features);

/**
 * @brief Shutdown and release resources
 */
void gf_det_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_OBJECT_DETECTION_H */
