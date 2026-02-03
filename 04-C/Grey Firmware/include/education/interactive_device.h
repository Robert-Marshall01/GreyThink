/**
 * @file interactive_device.h
 * @brief Interactive Device Interface for Smart Education Technology
 * 
 * @details
 * This module provides interfaces for interactive educational devices,
 * including smart boards, student response systems, and hands-on
 * learning equipment.
 * 
 * INDUSTRY RELEVANCE:
 * - Interactive Displays: SMART Board, Promethean, ViewSonic
 * - Student Response: Clicker systems, tablet-based polling
 * - STEM Education: Lab equipment, robotics kits, science sensors
 * - Special Education: Adaptive input devices, accessibility tools
 * - Training Simulators: Flight sims, medical training equipment
 * 
 * DEVICE CATEGORIES:
 * - Interactive whiteboards and displays
 * - Student tablets and Chromebooks
 * - Response systems (clickers)
 * - Science lab sensors and probes
 * - Maker/robotics equipment
 * - AR/VR headsets for education
 * 
 * COMPLIANCE:
 * - COPPA: Children's Online Privacy Protection
 * - FERPA: Student data privacy
 * - ADA Section 508: Accessibility
 * - UL/CE: Device safety certifications
 * 
 * @version 1.0
 * @date 2024
 */

#ifndef GF_INTERACTIVE_DEVICE_H
#define GF_INTERACTIVE_DEVICE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*******************************************************************************
 * Constants
 ******************************************************************************/

/** Maximum connected devices */
#define GF_IDEV_MAX_DEVICES             64

/** Maximum simultaneous touch points */
#define GF_IDEV_MAX_TOUCH_POINTS        20

/** Device ID length */
#define GF_IDEV_ID_LENGTH               16

/*******************************************************************************
 * Type Definitions
 ******************************************************************************/

/**
 * @brief Device type
 */
typedef enum {
    GF_IDEV_DISPLAY,              /**< Interactive display */
    GF_IDEV_TABLET,               /**< Student tablet */
    GF_IDEV_CLICKER,              /**< Response clicker */
    GF_IDEV_SENSOR_PROBE,         /**< Science sensor */
    GF_IDEV_ROBOT,                /**< Educational robot */
    GF_IDEV_VR_HEADSET,           /**< VR headset */
    GF_IDEV_CAMERA,               /**< Document camera */
    GF_IDEV_MICROPHONE            /**< Classroom microphone */
} gf_idev_type_t;

/**
 * @brief Connection status
 */
typedef enum {
    GF_IDEV_DISCONNECTED,         /**< Not connected */
    GF_IDEV_CONNECTING,           /**< Connecting */
    GF_IDEV_CONNECTED,            /**< Connected */
    GF_IDEV_PAIRED,               /**< Paired to student */
    GF_IDEV_ERROR                 /**< Connection error */
} gf_idev_status_t;

/**
 * @brief Touch event
 */
typedef struct {
    uint8_t point_id;             /**< Touch point ID */
    uint16_t x;                   /**< X coordinate */
    uint16_t y;                   /**< Y coordinate */
    float pressure;               /**< Touch pressure */
    bool active;                  /**< Point active */
} gf_touch_point_t;

/**
 * @brief Clicker response
 */
typedef struct {
    char device_id[GF_IDEV_ID_LENGTH];
    char student_id[GF_IDEV_ID_LENGTH];
    uint8_t response;             /**< Response (A=1, B=2, etc.) */
    uint32_t response_time_ms;    /**< Time to respond */
    uint32_t timestamp;           /**< Response timestamp */
} gf_clicker_response_t;

/**
 * @brief Sensor probe reading
 */
typedef struct {
    char sensor_type[16];         /**< Sensor type name */
    float value;                  /**< Reading value */
    char unit[8];                 /**< Unit string */
    float min_range;              /**< Sensor min range */
    float max_range;              /**< Sensor max range */
    uint32_t timestamp;           /**< Reading timestamp */
} gf_sensor_probe_t;

/**
 * @brief Device info
 */
typedef struct {
    char device_id[GF_IDEV_ID_LENGTH];
    gf_idev_type_t type;          /**< Device type */
    gf_idev_status_t status;      /**< Connection status */
    char student_id[GF_IDEV_ID_LENGTH]; /**< Assigned student */
    uint8_t battery_pct;          /**< Battery level */
    int8_t rssi_dbm;              /**< Signal strength */
    uint32_t last_activity;       /**< Last activity timestamp */
} gf_idev_info_t;

/*******************************************************************************
 * Function Prototypes
 ******************************************************************************/

/**
 * @brief Initialize interactive device system
 * @return 0 on success
 */
int gf_idev_init(void);

/**
 * @brief Shutdown interactive device system
 */
void gf_idev_shutdown(void);

/**
 * @brief Scan for devices
 * @param type Device type (or -1 for all)
 * @return Number of devices found
 */
int gf_idev_scan(int type);

/**
 * @brief Connect to device
 * @param device_id Device identifier
 * @return 0 on success
 */
int gf_idev_connect(const char* device_id);

/**
 * @brief Pair device to student
 * @param device_id Device identifier
 * @param student_id Student identifier
 * @return 0 on success
 */
int gf_idev_pair_student(const char* device_id, const char* student_id);

/**
 * @brief Get device info
 * @param device_id Device identifier
 * @param info Output info
 * @return 0 on success
 */
int gf_idev_get_info(const char* device_id, gf_idev_info_t* info);

/**
 * @brief Get clicker responses
 * @param responses Output array
 * @param max_responses Maximum responses
 * @return Number of responses
 */
int gf_idev_get_clicker_responses(gf_clicker_response_t* responses,
                                   uint8_t max_responses);

/**
 * @brief Clear clicker responses
 */
void gf_idev_clear_responses(void);

/**
 * @brief Get sensor probe reading
 * @param device_id Device identifier
 * @param reading Output reading
 * @return 0 on success
 */
int gf_idev_get_sensor_reading(const char* device_id,
                                gf_sensor_probe_t* reading);

/**
 * @brief Process interactive devices (call periodically)
 * @param delta_ms Time since last call
 */
void gf_idev_process(uint32_t delta_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_INTERACTIVE_DEVICE_H */
