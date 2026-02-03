/**
 * @file ar_shopping_interface.h
 * @brief Augmented Reality Shopping Interface Driver
 * 
 * INDUSTRY RELEVANCE:
 * Retailers are deploying AR experiences for product visualization,
 * wayfinding, and personalized recommendations. This interface enables
 * embedded AR functionality in smart mirrors, kiosks, and mobile devices
 * with real-time product recognition and overlay rendering.
 * 
 * TECHNICAL SCOPE:
 * - Product recognition via visual markers/ML
 * - 3D product overlay rendering
 * - Virtual try-on support (apparel, cosmetics)
 * - Price tag and promotion overlays
 * - Wayfinding arrows and navigation
 * - Gesture recognition for UI control
 * 
 * USE CASES:
 * - Smart mirror clothing try-on
 * - Furniture placement visualization
 * - Product information overlays
 * - In-store navigation
 * - Interactive displays
 * 
 * STANDARDS COMPLIANCE:
 * - OpenXR compatibility
 * - ARCore/ARKit interoperability
 * - ADA accessibility guidelines
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_AR_SHOPPING_INTERFACE_H
#define GF_AR_SHOPPING_INTERFACE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define AR_MAX_TRACKED_OBJECTS  16    /**< Simultaneous tracked items */
#define AR_MAX_OVERLAYS         32    /**< Maximum overlay elements */
#define AR_FRAME_RATE           30    /**< Target frame rate */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** AR experience type */
typedef enum {
    AR_EXP_PRODUCT_INFO,           /**< Info card overlay */
    AR_EXP_VIRTUAL_TRYON,          /**< Try-on experience */
    AR_EXP_PLACEMENT,              /**< 3D placement preview */
    AR_EXP_NAVIGATION,             /**< Wayfinding */
    AR_EXP_PROMOTION,              /**< Dynamic ads/promos */
    AR_EXP_GAMIFICATION            /**< Interactive engagement */
} ar_experience_t;

/** Gesture type */
typedef enum {
    AR_GESTURE_NONE,
    AR_GESTURE_TAP,
    AR_GESTURE_SWIPE_LEFT,
    AR_GESTURE_SWIPE_RIGHT,
    AR_GESTURE_SWIPE_UP,
    AR_GESTURE_SWIPE_DOWN,
    AR_GESTURE_PINCH,
    AR_GESTURE_SPREAD,
    AR_GESTURE_ROTATE
} ar_gesture_t;

/** Tracking status */
typedef enum {
    AR_TRACK_NONE,
    AR_TRACK_LIMITED,
    AR_TRACK_NORMAL,
    AR_TRACK_EXCELLENT
} ar_tracking_t;

/** Detected product */
typedef struct {
    uint32_t product_id;
    char sku[32];
    char name[64];
    float price;
    float discount_pct;
    bool in_stock;
    uint16_t stock_count;
    float position_x, position_y, position_z;
    float confidence;
} ar_product_t;

/** Overlay element */
typedef struct {
    uint8_t overlay_id;
    ar_experience_t type;
    float anchor_x, anchor_y, anchor_z;
    float scale;
    float rotation_deg;
    bool visible;
    uint32_t content_id;
} ar_overlay_t;

/** AR session status */
typedef struct {
    ar_tracking_t tracking;
    uint8_t products_detected;
    uint8_t overlays_active;
    ar_gesture_t last_gesture;
    float frame_rate_fps;
    float cpu_usage_pct;
    float gpu_usage_pct;
    bool camera_active;
    bool depth_available;
} ar_session_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize AR interface */
int ar_init(void);

/** Start AR session */
int ar_start_session(ar_experience_t experience);

/** Stop AR session */
int ar_stop_session(void);

/** Get detected products */
int ar_get_products(ar_product_t *products, uint8_t max_count);

/** Add overlay element */
int ar_add_overlay(const ar_overlay_t *overlay);

/** Remove overlay */
int ar_remove_overlay(uint8_t overlay_id);

/** Get last gesture */
ar_gesture_t ar_get_gesture(void);

/** Get session status */
int ar_get_status(ar_session_status_t *status);

/** Process AR frame */
int ar_process_frame(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_AR_SHOPPING_INTERFACE_H */
