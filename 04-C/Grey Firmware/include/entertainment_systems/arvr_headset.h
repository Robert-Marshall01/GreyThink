/**
 * @file arvr_headset.h
 * @brief AR/VR Headset Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Augmented and virtual reality headsets require sophisticated firmware for
 * display timing, motion tracking, eye tracking, and low-latency rendering.
 * Companies like Meta, Apple, Sony, HTC, Magic Leap, and Varjo build XR
 * devices that demand firmware expertise in IMU fusion, display pipelines,
 * and real-time systems.
 * 
 * This module provides interfaces for VR/AR headset subsystems including
 * displays, tracking, and input devices.
 * 
 * KEY CAPABILITIES:
 * - Display timing and sync
 * - IMU-based head tracking
 * - Inside-out tracking support
 * - Eye tracking interface
 * - Hand tracking integration
 * - Passthrough camera control
 * - Audio spatialization hooks
 * - Thermal management
 * 
 * DISPLAY TECHNOLOGIES:
 * - OLED/LCD panels (90-120Hz+)
 * - Pancake optics support
 * - Variable focus displays
 * - Foveated rendering hints
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_ARVR_HEADSET_H
#define GF_ARVR_HEADSET_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

#define XR_MAX_DISPLAYS        2     /**< Stereo displays */
#define XR_REFRESH_RATE_HZ     90    /**< Default refresh rate */
#define XR_TRACKING_RATE_HZ    1000  /**< IMU sample rate */
#define XR_MOTION_TO_PHOTON_MS 20    /**< Target latency */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/** Headset type */
typedef enum {
    XR_TYPE_VR,            /**< Fully immersive VR */
    XR_TYPE_AR,            /**< See-through AR */
    XR_TYPE_MR             /**< Mixed reality (passthrough) */
} xr_type_t;

/** Display eye */
typedef enum {
    XR_EYE_LEFT,
    XR_EYE_RIGHT,
    XR_EYE_BOTH
} xr_eye_t;

/** Tracking state */
typedef enum {
    XR_TRACKING_NONE,
    XR_TRACKING_LIMITED,
    XR_TRACKING_NORMAL,
    XR_TRACKING_HIGH_ACCURACY
} xr_tracking_state_t;

/** Pose (position + orientation) */
typedef struct {
    float position[3];     /**< XYZ in meters */
    float orientation[4];  /**< Quaternion XYZW */
    float velocity[3];     /**< Linear velocity m/s */
    float angular_vel[3];  /**< Angular velocity rad/s */
} xr_pose_t;

/** Eye tracking data */
typedef struct {
    float gaze_direction[3];  /**< Unit vector */
    float pupil_diameter_mm;
    float openness;           /**< 0.0-1.0 */
    bool tracking_valid;
} xr_eye_tracking_t;

/** Display configuration */
typedef struct {
    uint16_t width_px;
    uint16_t height_px;
    uint8_t refresh_rate_hz;
    float ppd;                /**< Pixels per degree */
    float fov_horizontal_deg;
    float fov_vertical_deg;
} xr_display_config_t;

/** Headset status */
typedef struct {
    xr_type_t type;
    xr_tracking_state_t tracking;
    xr_pose_t head_pose;
    xr_eye_tracking_t left_eye;
    xr_eye_tracking_t right_eye;
    float battery_pct;
    float temperature_c;
    bool proximity_detected;  /**< User wearing headset */
    bool passthrough_active;
} xr_status_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

/**
 * @brief Initialize XR headset
 * @param type Headset type
 * @return 0 on success
 */
int xr_init(xr_type_t type);

/**
 * @brief Configure display
 * @param eye Target eye
 * @param config Display configuration
 * @return 0 on success
 */
int xr_configure_display(xr_eye_t eye, const xr_display_config_t* config);

/**
 * @brief Get current head pose
 * @param pose Output pose
 * @param prediction_ms Future prediction time
 * @return 0 on success
 */
int xr_get_pose(xr_pose_t* pose, float prediction_ms);

/**
 * @brief Get eye tracking data
 * @param eye Which eye
 * @param tracking Output tracking data
 * @return 0 on success
 */
int xr_get_eye_tracking(xr_eye_t eye, xr_eye_tracking_t* tracking);

/**
 * @brief Set display brightness
 * @param eye Target eye
 * @param brightness 0.0-1.0
 * @return 0 on success
 */
int xr_set_brightness(xr_eye_t eye, float brightness);

/**
 * @brief Enable passthrough (MR mode)
 * @param enable Enable flag
 * @return 0 on success
 */
int xr_enable_passthrough(bool enable);

/**
 * @brief Get headset status
 * @param status Output status
 * @return 0 on success
 */
int xr_get_status(xr_status_t* status);

/**
 * @brief Signal frame ready for display
 * @param eye Target eye
 * @return 0 on success
 */
int xr_present_frame(xr_eye_t eye);

/**
 * @brief Shutdown headset
 */
void xr_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_ARVR_HEADSET_H */
