/**
 * @file body_camera.h
 * @brief Body-Worn Camera Driver Interface for Law Enforcement
 * 
 * INDUSTRY RELEVANCE:
 * Body-worn cameras are critical for transparency and accountability in law
 * enforcement. Modern systems require tamper-evident recording, GPS tagging,
 * and real-time streaming capabilities while maintaining chain-of-custody
 * integrity for legal proceedings.
 * 
 * KEY CAPABILITIES:
 * - High-definition video capture (1080p/4K @ 30fps)
 * - GPS location tagging for all footage
 * - Pre-event buffering (30-120 seconds)
 * - Tamper-evident storage with cryptographic hashing
 * - Real-time streaming to command center
 * - Battery-efficient operation for 12+ hour shifts
 * 
 * STANDARDS COMPLIANCE:
 * - CJIS Security Policy (FBI Criminal Justice Information Services)
 * - AXON Evidence.com integration patterns
 * - ISO 27001 information security
 * 
 * @note Stub header for portfolio demonstration
 */

#ifndef GF_BODY_CAMERA_H
#define GF_BODY_CAMERA_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Recording resolution */
typedef enum {
    BC_RES_720P,       /**< 1280x720 @ 30fps */
    BC_RES_1080P,      /**< 1920x1080 @ 30fps */
    BC_RES_4K          /**< 3840x2160 @ 30fps */
} bc_resolution_t;

/** Recording mode */
typedef enum {
    BC_MODE_STANDBY,   /**< Pre-event buffer only */
    BC_MODE_RECORDING, /**< Active recording */
    BC_MODE_STREAMING, /**< Live streaming to server */
    BC_MODE_MUTED      /**< Video only, no audio */
} bc_mode_t;

/** Camera status */
typedef enum {
    BC_STATUS_OK,
    BC_STATUS_LOW_BATTERY,
    BC_STATUS_STORAGE_FULL,
    BC_STATUS_TAMPER_DETECTED,
    BC_STATUS_GPS_LOST,
    BC_STATUS_STREAM_FAILED
} bc_status_t;

/** Recording metadata */
typedef struct {
    uint32_t timestamp;        /**< Unix timestamp */
    uint32_t officer_id;       /**< Badge/ID number */
    double latitude;           /**< GPS latitude */
    double longitude;          /**< GPS longitude */
    uint8_t hash[32];          /**< SHA-256 segment hash */
    bool audio_enabled;        /**< Audio capture flag */
    bc_resolution_t resolution;
} bc_metadata_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize body camera subsystem
 * @param officer_id Badge/ID for metadata tagging
 * @return 0 on success, negative on error
 */
int bc_init(uint32_t officer_id);

/**
 * @brief Start recording with pre-event buffer
 * @param mode Recording mode
 * @return 0 on success, negative on error
 */
int bc_start_recording(bc_mode_t mode);

/**
 * @brief Stop recording and finalize segment
 * @return 0 on success, negative on error
 */
int bc_stop_recording(void);

/**
 * @brief Tag current recording with incident marker
 * @param incident_type Type code for incident
 * @return 0 on success, negative on error
 */
int bc_tag_incident(uint16_t incident_type);

/**
 * @brief Get current camera status
 * @param status Output status structure
 * @return 0 on success, negative on error
 */
int bc_get_status(bc_status_t* status);

/**
 * @brief Enable/disable live streaming
 * @param server_url Command center stream URL
 * @return 0 on success, negative on error
 */
int bc_enable_streaming(const char* server_url);

/**
 * @brief Shutdown camera subsystem
 * @return 0 on success, negative on error
 */
int bc_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_BODY_CAMERA_H */
