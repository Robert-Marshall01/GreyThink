# Edge Vision Processing System

## Overview

The Edge Vision Processing Spotlight demonstrates production-grade embedded computer vision capabilities for resource-constrained devices. This implementation showcases the algorithms and architecture patterns used in industrial vision systems, surveillance, automotive ADAS, and IoT vision applications.

## Architecture

```
┌─────────────────────────────────────────────────────────────────┐
│                    Vision Processing Pipeline                    │
├─────────────────────────────────────────────────────────────────┤
│                                                                  │
│  ┌──────────┐    ┌─────────────┐    ┌──────────┐    ┌────────┐ │
│  │  Camera  │───▶│ Preprocess  │───▶│ Detector │───▶│ Report │ │
│  │  Driver  │    │             │    │          │    │        │ │
│  └──────────┘    └─────────────┘    └──────────┘    └────────┘ │
│       │                │                  │              │      │
│       ▼                ▼                  ▼              ▼      │
│  ┌──────────┐    ┌─────────────┐    ┌──────────┐    ┌────────┐ │
│  │  Ring    │    │ Grayscale   │    │ Sobel    │    │ Msg    │ │
│  │  Buffer  │    │ Normalize   │    │ Corners  │    │ Bus    │ │
│  │          │    │ Blur        │    │ Blobs    │    │        │ │
│  └──────────┘    └─────────────┘    └──────────┘    └────────┘ │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

## Features

### 1. Frame Buffer Management
- **Ring Buffer**: Configurable multi-frame ring buffer (up to 4 frames)
- **Zero-Copy**: Efficient buffer cycling for real-time performance
- **Frame Metadata**: Timestamp, frame number, format tracking

### 2. Image Preprocessing

| Algorithm | Description | Use Case |
|-----------|-------------|----------|
| RGB to Grayscale | BT.601 luminance conversion (0.299R + 0.587G + 0.114B) | Color camera input |
| RGB565 to Grayscale | 16-bit color format conversion | Embedded displays |
| Normalization | Min-max contrast stretch to 0-255 | Lighting compensation |
| Standardization | Z-score normalization (mean=0, std=1) | ML preprocessing |
| Blur 3x3 | Box blur for noise reduction | High-frequency noise |
| Gaussian 5x5 | Weighted gaussian kernel blur | Gentle smoothing |
| Histogram Equalization | Adaptive contrast enhancement | Low-light imaging |

### 3. Edge Detection

**Sobel Operator**:
- Horizontal edge detection (Gx kernel)
- Vertical edge detection (Gy kernel)
- Gradient magnitude: √(Gx² + Gy²)

**Thresholding**:
- Binary threshold for edge maps
- Otsu's method for automatic threshold selection

### 4. Feature Detection

**Harris Corner Detection**:
- Gradient-based corner response
- Non-maximum suppression
- Configurable detection threshold

**Blob Detection**:
- Connected component labeling
- Feature extraction (area, centroid, bounding box)
- Size-based filtering (min/max area)

### 5. Object Detection

**Detection Pipeline**:
1. Edge detection on preprocessed frame
2. Threshold to binary edge map
3. Find connected components (blobs)
4. Calculate bounding boxes
5. Assign confidence scores

**Detection Result Structure**:
```c
typedef struct {
    uint16_t x, y;           /* Top-left corner */
    uint16_t width, height;  /* Bounding box dimensions */
    uint16_t area;           /* Pixel area */
    uint8_t class_id;        /* Object class */
    uint8_t confidence;      /* Detection confidence (0-255) */
    uint32_t track_id;       /* Tracking ID (if enabled) */
} gf_vision_detection_t;
```

### 6. Motion Detection

**Frame Differencing**:
- Previous frame comparison
- Configurable motion threshold
- Motion region bounding boxes

### 7. Lighting Adaptation

**Automatic Detection**:
| State | Mean Intensity | Action |
|-------|----------------|--------|
| DARK | < 30 | Maximum histogram equalization |
| DIM | 30-80 | Moderate enhancement |
| NORMAL | 80-180 | No enhancement |
| BRIGHT | 180-230 | Slight reduction |
| HARSH | > 230 | Saturation handling |

## API Reference

### Initialization

```c
gf_vision_config_t config = {
    .frame_width = 320,
    .frame_height = 240,
    .input_format = GF_VISION_FMT_RGB888,
    .frame_buffer_count = 3,
    .detection_threshold = 0.5f,
    .detect_types = GF_DETECT_BLOB | GF_DETECT_EDGE,
    .enable_tracking = false,
    .adaptive_exposure = true,
    .callback = my_detection_callback,
    .callback_user_data = &my_context
};

gf_vision_status_t status = gf_vision_init(&config);
```

### Frame Processing

```c
/* Submit a frame for processing */
status = gf_vision_submit_frame(
    frame_data,
    320, 240,
    GF_VISION_FMT_RGB888,
    timestamp_ms
);

/* Process and get detection results */
gf_vision_detection_result_t result;
status = gf_vision_process_frame(&result);

/* Access detections */
for (uint8_t i = 0; i < result.count; i++) {
    printf("Object at (%d,%d) size %dx%d, conf=%d%%\n",
           result.objects[i].x,
           result.objects[i].y,
           result.objects[i].width,
           result.objects[i].height,
           result.objects[i].confidence * 100 / 255);
}
```

### Preprocessing Functions

```c
/* Color conversion */
gf_vision_rgb_to_gray(rgb_data, gray_data, width, height);
gf_vision_rgb565_to_gray(rgb565_data, gray_data, width, height);

/* Filtering */
gf_vision_blur_3x3(src, dst, width, height);
gf_vision_gaussian_blur_5x5(src, dst, width, height, scratch);

/* Enhancement */
gf_vision_normalize(image, width, height);
gf_vision_equalize_histogram(image, width, height);
```

### Edge Detection

```c
/* Sobel edge detection */
gf_vision_sobel(src, dst, width, height, grad_x, grad_y);

/* Binary thresholding */
gf_vision_threshold(src, dst, width, height, threshold_value);

/* Automatic threshold using Otsu's method */
uint8_t otsu_thresh = gf_vision_otsu_threshold(&histogram, pixel_count);
```

### Feature Detection

```c
/* Harris corner detection */
gf_vision_keypoint_t keypoints[256];
uint16_t keypoint_count = 0;

gf_vision_harris_corners(
    image, keypoints, &keypoint_count, 256,
    width, height, scratch_buffer
);

/* Blob detection */
gf_vision_detection_t blobs[32];
uint16_t blob_count = 0;

gf_vision_find_blobs(
    binary_image, blobs, &blob_count, 32,
    width, height, label_buffer
);
```

### Statistics and Monitoring

```c
/* Get processing statistics */
uint32_t frames_processed, total_detections;
float avg_time_us, max_time_us;

gf_vision_get_stats(
    &frames_processed,
    &total_detections,
    &avg_time_us,
    &max_time_us
);

/* Analyze image quality */
gf_vision_image_stats_t stats;
gf_vision_analyze_image(image, width, height, &stats);
printf("Lighting: %s, Sharpness: %.1f\n",
       lighting_names[stats.lighting], stats.sharpness);
```

## Message Bus Integration

The vision system publishes events to the Grey Firmware message bus:

### Vision Topics

| Topic | Payload Type | Description |
|-------|--------------|-------------|
| `vision/frame` | `gf_vision_frame_payload_t` | New frame captured |
| `vision/detect` | `gf_vision_detect_payload_t` | Detection result |
| `vision/event` | varies | System events (start/stop) |
| `vision/error` | string | Error notifications |
| `vision/stats` | `gf_vision_stats_payload_t` | Processing statistics |

### Frame Payload
```c
typedef struct {
    uint32_t frame_id;
    uint16_t width;
    uint16_t height;
    gf_vision_format_t format;
    uint32_t timestamp_ms;
} gf_vision_frame_payload_t;
```

### Detection Payload
```c
typedef struct {
    uint32_t frame_id;
    uint8_t detection_count;
    gf_vision_detection_t detections[8];
    gf_vision_lighting_t lighting;
    uint32_t processing_time_us;
} gf_vision_detect_payload_t;
```

## Performance Characteristics

### Memory Requirements

| Resolution | Frame Buffer (3x) | Scratch Buffer | Total |
|------------|-------------------|----------------|-------|
| 160x120 | 57.6 KB | 76.8 KB | ~135 KB |
| 320x240 | 230.4 KB | 307.2 KB | ~540 KB |
| 640x480 | 921.6 KB | 1.2 MB | ~2.1 MB |

### Processing Time (Estimated)

| Operation | 160x120 | 320x240 | 640x480 |
|-----------|---------|---------|---------|
| Grayscale | ~0.2ms | ~0.8ms | ~3ms |
| Blur 3x3 | ~0.3ms | ~1.2ms | ~5ms |
| Sobel Edge | ~0.5ms | ~2ms | ~8ms |
| Blob Detection | ~1ms | ~4ms | ~16ms |
| Full Pipeline | ~3ms | ~12ms | ~48ms |

*Times based on 100MHz ARM Cortex-M4. Actual performance varies by platform.*

## Test Coverage

The vision spotlight includes 31 comprehensive tests:

### Preprocessing Tests (7)
- RGB to grayscale conversion with BT.601 weights
- Normalization (min-max contrast stretch)
- Blur operations (3x3 and 5x5 Gaussian)
- Histogram calculation
- Binary thresholding
- Otsu's automatic thresholding

### Edge Detection Tests (2)
- Sobel vertical edge detection
- Sobel horizontal edge detection

### Corner Detection Tests (2)
- Harris corner detection on checkerboard
- No corners on uniform image

### Blob Detection Tests (3)
- Single blob detection
- Multiple blob detection
- Size filtering

### Motion Detection Tests (2)
- Static scene (no motion)
- Moving object detection

### Lighting Tests (4)
- Dark lighting detection
- Normal lighting detection
- Bright lighting detection
- Histogram equalization enhancement

### Frame Buffer Tests (3)
- Ring buffer initialization
- Write/read operations
- Buffer full handling

### Integration Tests (8)
- System init/shutdown
- Full frame pipeline
- Dark lighting enhancement
- ROI configuration
- Threshold adjustment
- Callback registration
- Multiple frame processing
- RGB input format handling

Run vision tests with:
```bash
make test-vision
```

## Use Cases

### 1. Industrial Quality Inspection
```c
/* Configure for high-precision detection */
config.frame_width = 640;
config.frame_height = 480;
config.detection_threshold = 0.8f;  /* High confidence required */
config.detect_types = GF_DETECT_EDGE | GF_DETECT_BLOB;
```

### 2. Surveillance Motion Detection
```c
/* Configure for motion sensitivity */
config.frame_width = 320;
config.frame_height = 240;
config.enable_tracking = true;
config.adaptive_exposure = true;  /* Handle day/night */
```

### 3. ADAS Lane Detection
```c
/* Configure for road marking detection */
config.roi = { .x = 0, .y = 120, .width = 320, .height = 120 };
config.detect_types = GF_DETECT_EDGE;
```

### 4. Agricultural Crop Monitoring
```c
/* Configure for plant detection */
config.adaptive_exposure = true;  /* Outdoor lighting variation */
config.detect_types = GF_DETECT_BLOB;
```

## Limitations

1. **No Deep Learning**: Uses classical CV algorithms (suitable for MCU)
2. **Single Object Class**: Blob detection without classification
3. **No GPU Acceleration**: CPU-only processing
4. **Memory Constrained**: Limited by available RAM for buffers

## Future Enhancements

Planned improvements for production deployment:

1. **TinyML Integration**: Connect with Edge AI spotlight for neural network classification
2. **Hardware Acceleration**: DMA-based pixel processing
3. **Multi-Object Tracking**: Kalman filter-based object tracking
4. **Color Detection**: HSV color space segmentation
5. **Optical Flow**: Lucas-Kanade for motion vectors

## Related Documentation

- [Edge AI/ML Guide](ai_inference.md) - Neural network inference for classification
- [Drone Flight Control](drone_systems.md) - Vision-assisted navigation
- [Message Bus](overview.md#message-bus) - Event-driven communication
- [Scheduler](overview.md#scheduler) - Periodic frame capture scheduling

---

*Grey Firmware Vision Spotlight - Production-grade embedded computer vision for resource-constrained devices*
