/**
 * @file camera_driver.h
 * @brief Camera Driver Stub
 * 
 * INDUSTRY RELEVANCE:
 * Embedded vision systems are deployed across surveillance, automotive
 * (ADAS), industrial inspection, and consumer devices. The embedded vision
 * market exceeds $25B growing at 15%+ CAGR. Firmware engineers work with
 * MIPI CSI-2 interfaces, image sensor configuration (CMOS sensors), and
 * efficient frame buffer management for real-time processing.
 * 
 * Key challenges:
 * - High-bandwidth data handling (100+ MB/s for HD)
 * - Frame synchronization and timestamping
 * - Power management for always-on cameras
 * - Lens correction and sensor calibration
 * - Privacy and security (secure streaming)
 */

#ifndef GF_CAMERA_DRIVER_H
#define GF_CAMERA_DRIVER_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Camera driver status codes */
typedef enum {
    GF_CAM_OK = 0,
    GF_CAM_NOT_DETECTED,            /* Camera not found */
    GF_CAM_INIT_FAILED,             /* Initialization failure */
    GF_CAM_BUFFER_FULL,             /* Frame buffer overflow */
    GF_CAM_FRAME_ERROR,             /* Frame capture error */
    GF_CAM_CONFIG_ERROR,            /* Configuration invalid */
    GF_CAM_I2C_ERROR,               /* Sensor I2C communication error */
    GF_CAM_TIMEOUT,                 /* Frame capture timeout */
    GF_CAM_POWER_OFF                /* Camera powered down */
} gf_cam_status_t;

/* Pixel formats */
typedef enum {
    GF_PIXEL_RGB888,                /* 24-bit RGB */
    GF_PIXEL_RGB565,                /* 16-bit RGB */
    GF_PIXEL_YUV422,                /* YUV 4:2:2 */
    GF_PIXEL_YUV420,                /* YUV 4:2:0 (NV12) */
    GF_PIXEL_GRAYSCALE,             /* 8-bit grayscale */
    GF_PIXEL_BAYER_RGGB,            /* Raw Bayer RGGB */
    GF_PIXEL_BAYER_BGGR,            /* Raw Bayer BGGR */
    GF_PIXEL_JPEG                   /* Compressed JPEG */
} gf_cam_pixel_format_t;

/* Common resolutions */
typedef enum {
    GF_RES_QQVGA,                   /* 160x120 */
    GF_RES_QVGA,                    /* 320x240 */
    GF_RES_VGA,                     /* 640x480 */
    GF_RES_SVGA,                    /* 800x600 */
    GF_RES_720P,                    /* 1280x720 */
    GF_RES_1080P,                   /* 1920x1080 */
    GF_RES_CUSTOM                   /* Custom resolution */
} gf_cam_resolution_t;

/* Camera interface types */
typedef enum {
    GF_CAM_IF_PARALLEL,             /* Parallel interface */
    GF_CAM_IF_MIPI_CSI2,            /* MIPI CSI-2 */
    GF_CAM_IF_USB,                  /* USB camera */
    GF_CAM_IF_SPI                   /* SPI camera (low-res) */
} gf_cam_interface_t;

/* Camera configuration */
typedef struct {
    gf_cam_resolution_t resolution; /* Capture resolution */
    uint16_t width;                 /* Width (for custom) */
    uint16_t height;                /* Height (for custom) */
    gf_cam_pixel_format_t format;   /* Pixel format */
    gf_cam_interface_t interface;   /* Hardware interface */
    uint8_t fps;                    /* Frames per second */
    uint8_t buffer_count;           /* Number of frame buffers */
    bool enable_auto_exposure;      /* Auto exposure control */
    bool enable_auto_wb;            /* Auto white balance */
    bool enable_flip_h;             /* Horizontal flip */
    bool enable_flip_v;             /* Vertical flip */
} gf_cam_config_t;

/* Frame buffer descriptor */
typedef struct {
    uint8_t* data;                  /* Pointer to pixel data */
    uint32_t size;                  /* Buffer size in bytes */
    uint16_t width;                 /* Frame width */
    uint16_t height;                /* Frame height */
    gf_cam_pixel_format_t format;   /* Pixel format */
    uint32_t timestamp;             /* Capture timestamp (ms) */
    uint32_t frame_number;          /* Frame sequence number */
    uint16_t stride;                /* Row stride in bytes */
} gf_cam_frame_t;

/* Camera sensor information */
typedef struct {
    char sensor_name[32];           /* Sensor model name */
    uint16_t max_width;             /* Maximum resolution width */
    uint16_t max_height;            /* Maximum resolution height */
    uint8_t max_fps;                /* Maximum frame rate */
    uint8_t bits_per_pixel;         /* Sensor bit depth */
    float pixel_size_um;            /* Pixel size in micrometers */
    bool has_autofocus;             /* Autofocus capability */
} gf_cam_sensor_info_t;

/* Exposure control */
typedef struct {
    uint32_t exposure_us;           /* Exposure time (microseconds) */
    uint16_t gain;                  /* Analog gain (1x = 1024) */
    uint16_t digital_gain;          /* Digital gain */
    int8_t ev_compensation;         /* EV compensation (-3 to +3) */
} gf_cam_exposure_t;

/**
 * @brief Initialize camera driver
 * @param config Camera configuration
 * @return Status code
 */
gf_cam_status_t gf_cam_init(const gf_cam_config_t* config);

/**
 * @brief Start continuous frame capture
 * @return Status code
 */
gf_cam_status_t gf_cam_start(void);

/**
 * @brief Stop frame capture
 * @return Status code
 */
gf_cam_status_t gf_cam_stop(void);

/**
 * @brief Capture single frame (blocking)
 * @param frame Output frame descriptor
 * @return Status code
 */
gf_cam_status_t gf_cam_capture(gf_cam_frame_t* frame);

/**
 * @brief Get next available frame (non-blocking)
 * @param frame Output frame descriptor
 * @return Status code (GF_CAM_BUFFER_FULL if no frame ready)
 */
gf_cam_status_t gf_cam_get_frame(gf_cam_frame_t* frame);

/**
 * @brief Release frame buffer back to driver
 * @param frame Frame to release
 * @return Status code
 */
gf_cam_status_t gf_cam_release_frame(gf_cam_frame_t* frame);

/**
 * @brief Get sensor information
 * @param info Output for sensor info
 * @return Status code
 */
gf_cam_status_t gf_cam_get_sensor_info(gf_cam_sensor_info_t* info);

/**
 * @brief Set exposure parameters
 * @param exposure Exposure settings
 * @return Status code
 */
gf_cam_status_t gf_cam_set_exposure(const gf_cam_exposure_t* exposure);

/**
 * @brief Get current exposure parameters
 * @param exposure Output for exposure settings
 * @return Status code
 */
gf_cam_status_t gf_cam_get_exposure(gf_cam_exposure_t* exposure);

/**
 * @brief Set camera resolution at runtime
 * @param resolution Resolution preset
 * @param width Custom width (if GF_RES_CUSTOM)
 * @param height Custom height (if GF_RES_CUSTOM)
 * @return Status code
 */
gf_cam_status_t gf_cam_set_resolution(gf_cam_resolution_t resolution, uint16_t width, uint16_t height);

/**
 * @brief Power down camera to save power
 * @return Status code
 */
gf_cam_status_t gf_cam_power_down(void);

/**
 * @brief Power up camera from sleep
 * @return Status code
 */
gf_cam_status_t gf_cam_power_up(void);

/**
 * @brief Perform camera self-test
 * @return Status code (OK if passed)
 */
gf_cam_status_t gf_cam_self_test(void);

/**
 * @brief Shutdown and release resources
 */
void gf_cam_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CAMERA_DRIVER_H */
