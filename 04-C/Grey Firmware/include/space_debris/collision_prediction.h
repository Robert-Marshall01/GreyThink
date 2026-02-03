/**
 * @file collision_prediction.h
 * @brief Orbital Collision Prediction Module Interface
 * 
 * INDUSTRY RELEVANCE:
 * Collision avoidance is essential for satellite operators managing
 * multi-billion dollar assets. NASA's Conjunction Assessment team
 * processes thousands of close approaches daily. Commercial operators
 * like SpaceX Starlink, OneWeb, and Amazon Kuiper require automated
 * collision prediction for mega-constellation management.
 * 
 * This module provides:
 * - Conjunction screening (TCA, miss distance calculation)
 * - Collision probability computation (Pc)
 * - Maneuver planning support
 * - Automated alert generation
 * 
 * STANDARDS:
 * - CCSDS 508.0-B (Orbit Data Messages)
 * - NASA Conjunction Data Messages (CDM)
 * - AFSPC STK/Aegis integration
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_COLLISION_PREDICTION_H
#define GF_COLLISION_PREDICTION_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration
 * ============================================================================ */

#define PRED_MAX_CONJUNCTIONS     256    /**< Max simultaneous conjunctions */
#define PRED_HORIZON_DAYS         7      /**< Prediction horizon */
#define PRED_SCREEN_DISTANCE_KM   10.0f  /**< Default screening threshold */
#define PRED_PC_THRESHOLD         1e-4f  /**< Collision probability threshold */

/* ============================================================================
 * Types
 * ============================================================================ */

/** Conjunction severity level */
typedef enum {
    CONJ_SEVERITY_ROUTINE,     /**< Miss distance > 5 km */
    CONJ_SEVERITY_WATCH,       /**< 1-5 km, Pc < 1e-5 */
    CONJ_SEVERITY_WARNING,     /**< 0.5-1 km, Pc < 1e-4 */
    CONJ_SEVERITY_ALERT,       /**< < 500m, Pc >= 1e-4 */
    CONJ_SEVERITY_CRITICAL     /**< Maneuver required */
} conjunction_severity_t;

/** Maneuver decision */
typedef enum {
    MANEUVER_NONE,
    MANEUVER_MONITOR,
    MANEUVER_PLANNING,
    MANEUVER_COMMITTED,
    MANEUVER_EXECUTED
} maneuver_status_t;

/** Orbital state vector (ECI J2000) */
typedef struct {
    double x_km, y_km, z_km;       /**< Position */
    double vx_km_s, vy_km_s, vz_km_s; /**< Velocity */
    double epoch_jd;               /**< Julian date epoch */
} state_vector_t;

/** Covariance matrix (6x6 position-velocity) */
typedef struct {
    double elements[21];           /**< Upper triangular, row-major */
} covariance_t;

/** Conjunction event */
typedef struct {
    uint32_t conjunction_id;
    uint32_t primary_id;           /**< Protected asset NORAD ID */
    uint32_t secondary_id;         /**< Threat object NORAD ID */
    double tca_jd;                 /**< Time of closest approach */
    float miss_distance_km;
    float radial_miss_km;
    float in_track_miss_km;
    float cross_track_miss_km;
    float relative_velocity_km_s;
    float collision_probability;
    conjunction_severity_t severity;
    maneuver_status_t maneuver;
    state_vector_t primary_state;
    state_vector_t secondary_state;
    covariance_t primary_cov;
    covariance_t secondary_cov;
} conjunction_event_t;

/** Prediction configuration */
typedef struct {
    float screen_distance_km;
    float pc_threshold;
    uint8_t prediction_days;
    bool auto_maneuver_planning;
    bool cdm_generation_enabled;
} prediction_config_t;

/* ============================================================================
 * API Functions
 * ============================================================================ */

/**
 * @brief Initialize prediction engine
 * @return 0 on success
 */
int pred_init(void);

/**
 * @brief Configure prediction parameters
 * @param config Configuration structure
 * @return 0 on success
 */
int pred_configure(const prediction_config_t *config);

/**
 * @brief Screen for conjunctions
 * @param primary Protected asset state
 * @param primary_cov Position covariance
 * @return Number of conjunctions found
 */
int pred_screen_conjunctions(const state_vector_t *primary, 
                             const covariance_t *primary_cov);

/**
 * @brief Get active conjunctions
 * @param events Output buffer
 * @param max_count Maximum events
 * @return Number of active conjunctions
 */
int pred_get_conjunctions(conjunction_event_t *events, uint16_t max_count);

/**
 * @brief Calculate collision probability
 * @param event Conjunction to analyze
 * @return Collision probability (0-1)
 */
float pred_calculate_pc(const conjunction_event_t *event);

/**
 * @brief Process prediction updates
 * @param time_ms Current time
 * @return 0 on success
 */
int pred_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_COLLISION_PREDICTION_H */
