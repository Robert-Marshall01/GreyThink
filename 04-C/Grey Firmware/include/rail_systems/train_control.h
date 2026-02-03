/**
 * @file train_control.h
 * @brief Train Control Loop for Smart Rail Systems
 * 
 * @details
 * Real-time train control system implementing speed regulation, traction
 * control, and automatic train protection (ATP). Designed for commuter
 * rail, metro, and high-speed rail applications.
 * 
 * INDUSTRY RELEVANCE:
 * - Automatic Train Operation (ATO)
 * - European Train Control System (ETCS)
 * - Positive Train Control (PTC)
 * - CBTC (Communication-Based Train Control)
 * - Rolling stock manufacturers (Siemens, Alstom, Hitachi)
 * 
 * KEY FEATURES:
 * - Speed profile following
 * - Traction/braking control
 * - Jerk-limited acceleration
 * - Station stop control
 * - Emergency braking triggering
 * - Energy-efficient driving
 * 
 * @version 1.0
 * @copyright Grey Firmware Project
 */

#ifndef GF_TRAIN_CONTROL_H
#define GF_TRAIN_CONTROL_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Constants                                                                  */
/*===========================================================================*/

/** Maximum speed (km/h) */
#define GF_TRAIN_MAX_SPEED_KMH      400

/** Emergency brake deceleration (m/s²) */
#define GF_TRAIN_EMERGENCY_DECEL    1.2f

/** Control loop period (ms) */
#define GF_TRAIN_CONTROL_PERIOD_MS  50

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/**
 * @brief Train control status codes
 */
typedef enum {
    GF_TRAIN_OK = 0,
    GF_TRAIN_ERROR_NOT_INIT,
    GF_TRAIN_ERROR_NULL_PTR,
    GF_TRAIN_ERROR_INVALID_STATE,
    GF_TRAIN_ERROR_COMM_FAULT,
    GF_TRAIN_ERROR_SENSOR_FAULT,
    GF_TRAIN_WARN_SPEED_LIMIT,
    GF_TRAIN_WARN_WHEELSLIP,
    GF_TRAIN_CRITICAL_EMERGENCY
} gf_train_status_t;

/**
 * @brief Operating mode
 */
typedef enum {
    GF_TRAIN_MODE_OFF,            /**< System off */
    GF_TRAIN_MODE_MANUAL,         /**< Manual driving */
    GF_TRAIN_MODE_ATO,            /**< Automatic operation */
    GF_TRAIN_MODE_ATP_SUPERVISE,  /**< ATP supervision only */
    GF_TRAIN_MODE_SHUNTING,       /**< Yard movement */
    GF_TRAIN_MODE_EMERGENCY       /**< Emergency brake active */
} gf_train_mode_t;

/**
 * @brief Braking mode
 */
typedef enum {
    GF_TRAIN_BRAKE_NONE,
    GF_TRAIN_BRAKE_SERVICE,       /**< Normal service brake */
    GF_TRAIN_BRAKE_FULL_SERVICE,  /**< Maximum service brake */
    GF_TRAIN_BRAKE_EMERGENCY      /**< Emergency brake */
} gf_train_brake_t;

/**
 * @brief Train state
 */
typedef struct {
    int32_t speed_mm_s;           /**< Current speed (mm/s) */
    int32_t target_speed_mm_s;    /**< Target speed (mm/s) */
    int32_t position_mm;          /**< Track position (mm) */
    int16_t acceleration_mg;      /**< Acceleration (milli-g) */
    int16_t gradient_permille;    /**< Track gradient (0.1%) */
    uint8_t traction_pct;         /**< Traction demand (%) */
    uint8_t brake_pct;            /**< Brake demand (%) */
    gf_train_mode_t mode;         /**< Operating mode */
} gf_train_state_t;

/**
 * @brief Speed restriction
 */
typedef struct {
    int32_t start_position_mm;    /**< Restriction start */
    int32_t end_position_mm;      /**< Restriction end */
    uint16_t speed_limit_kmh;     /**< Speed limit (km/h) */
    bool permanent;               /**< Permanent restriction */
} gf_train_speed_restriction_t;

/**
 * @brief Station stop point
 */
typedef struct {
    int32_t stop_position_mm;     /**< Target stop position */
    uint16_t tolerance_mm;        /**< Stopping tolerance */
    uint32_t dwell_time_s;        /**< Door open time */
    bool platform_left;           /**< Platform side left */
    bool platform_right;          /**< Platform side right */
} gf_train_station_stop_t;

/**
 * @brief Control parameters
 */
typedef struct {
    float max_accel_ms2;          /**< Maximum acceleration */
    float max_decel_ms2;          /**< Maximum deceleration */
    float max_jerk_ms3;           /**< Maximum jerk */
    uint16_t speed_tolerance_kmh; /**< Speed tolerance */
    bool eco_mode;                /**< Energy-efficient mode */
} gf_train_control_params_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize train control
 * @return Status code
 */
gf_train_status_t gf_train_init(void);

/**
 * @brief Shutdown train control
 */
void gf_train_shutdown(void);

/**
 * @brief Set operating mode
 * @param mode Operating mode
 * @return Status code
 */
gf_train_status_t gf_train_set_mode(gf_train_mode_t mode);

/**
 * @brief Set target speed
 * @param speed_kmh Target speed (km/h)
 * @return Status code
 */
gf_train_status_t gf_train_set_target_speed(uint16_t speed_kmh);

/**
 * @brief Apply braking
 * @param mode Brake mode
 * @param force_pct Brake force (% for service brake)
 * @return Status code
 */
gf_train_status_t gf_train_apply_brake(gf_train_brake_t mode, uint8_t force_pct);

/**
 * @brief Trigger emergency brake
 * @param reason Emergency brake reason
 * @return Status code
 */
gf_train_status_t gf_train_emergency_brake(const char* reason);

/**
 * @brief Get current state
 * @param state Output state
 * @return Status code
 */
gf_train_status_t gf_train_get_state(gf_train_state_t* state);

/**
 * @brief Set speed restriction
 * @param restriction Speed restriction
 * @return Status code
 */
gf_train_status_t gf_train_set_restriction(const gf_train_speed_restriction_t* restriction);

/**
 * @brief Program station stop
 * @param stop Station stop parameters
 * @return Status code
 */
gf_train_status_t gf_train_set_station_stop(const gf_train_station_stop_t* stop);

/**
 * @brief Configure control parameters
 * @param params Control parameters
 * @return Status code
 */
gf_train_status_t gf_train_configure(const gf_train_control_params_t* params);

/**
 * @brief Process control loop (call at CONTROL_PERIOD_MS)
 * @return Status code
 */
gf_train_status_t gf_train_process(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TRAIN_CONTROL_H */
