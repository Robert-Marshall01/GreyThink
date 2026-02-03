/**
 * @file step_counter.h
 * @brief Pedometer / Step Counter Module
 * 
 * INDUSTRY RELEVANCE:
 * Step counting is a core feature of fitness trackers and smartwatches.
 * Modern algorithms use accelerometer data to detect gait patterns while
 * rejecting false positives from arm movements, driving, etc. The global
 * fitness tracker market exceeds $40B, with step counting being the most
 * requested feature.
 * 
 * WHY THIS MATTERS:
 * - 3-axis accelerometer processing
 * - Peak detection algorithms for step identification
 * - Activity recognition (walking, running, stairs)
 * - Cadence and distance estimation
 * - Low-power always-on operation
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - MEMS sensor data processing
 * - Signal filtering and peak detection
 * - Activity classification
 * - Power-optimized algorithms
 */

#ifndef GF_STEP_COUNTER_H
#define GF_STEP_COUNTER_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_STEP_SAMPLE_RATE_HZ      50      /* Accelerometer sample rate */
#define GF_STEP_BUFFER_SIZE         128     /* Circular buffer for processing */
#define GF_STEP_HISTORY_HOURS       24      /* Hours of step history */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Activity type */
typedef enum {
    GF_ACTIVITY_STATIONARY = 0,     /* Not moving */
    GF_ACTIVITY_WALKING,            /* Normal walking */
    GF_ACTIVITY_RUNNING,            /* Running/jogging */
    GF_ACTIVITY_CYCLING,            /* Cycling (filtered out) */
    GF_ACTIVITY_STAIRS_UP,          /* Climbing stairs */
    GF_ACTIVITY_STAIRS_DOWN,        /* Descending stairs */
    GF_ACTIVITY_UNKNOWN             /* Unclassified */
} gf_activity_t;

/* Step detection sensitivity */
typedef enum {
    GF_STEP_SENS_LOW = 0,           /* Few false positives, may miss steps */
    GF_STEP_SENS_NORMAL,            /* Balanced detection */
    GF_STEP_SENS_HIGH               /* Catch all steps, more false positives */
} gf_step_sensitivity_t;

/* 3-axis accelerometer sample */
typedef struct {
    int16_t     x;                  /* X-axis (milli-g) */
    int16_t     y;                  /* Y-axis (milli-g) */
    int16_t     z;                  /* Z-axis (milli-g) */
    uint32_t    timestamp_ms;
} gf_accel_sample_t;

/* Step counter configuration */
typedef struct {
    uint16_t    sample_rate_hz;     /* Accelerometer ODR */
    uint16_t    step_threshold;     /* Minimum acceleration for step */
    uint16_t    min_step_interval_ms; /* Minimum time between steps */
    uint16_t    max_step_interval_ms; /* Maximum time between steps */
    uint16_t    stride_length_cm;   /* User stride length for distance */
    uint16_t    user_height_cm;     /* Height for stride estimation */
    uint8_t     user_weight_kg;     /* Weight for calorie estimation */
    gf_step_sensitivity_t sensitivity;
} gf_step_config_t;

/* Current step counter state */
typedef struct {
    uint32_t    steps_today;        /* Steps since midnight */
    uint32_t    steps_session;      /* Steps since start() */
    uint32_t    steps_lifetime;     /* Total steps ever */
    uint32_t    distance_m;         /* Distance in meters */
    uint16_t    calories;           /* Estimated calories burned */
    uint16_t    cadence;            /* Steps per minute */
    gf_activity_t activity;         /* Current activity type */
    uint16_t    active_minutes;     /* Minutes of activity today */
    uint32_t    last_step_time;     /* Timestamp of last step */
} gf_step_state_t;

/* Hourly step history */
typedef struct {
    uint16_t    steps[24];          /* Steps per hour */
    uint16_t    active_minutes[24]; /* Active minutes per hour */
    uint8_t     current_hour;       /* Current hour index */
} gf_step_history_t;

/* Goal tracking */
typedef struct {
    uint32_t    daily_step_goal;
    uint16_t    daily_distance_goal_m;
    uint16_t    daily_calorie_goal;
    uint16_t    daily_active_minutes_goal;
    bool        goal_reached_steps;
    bool        goal_reached_distance;
    bool        goal_reached_calories;
    bool        goal_reached_active;
} gf_step_goals_t;

/* Step event callback */
typedef void (*gf_step_callback_t)(const gf_step_state_t *state, void *ctx);

/* Activity change callback */
typedef void (*gf_activity_callback_t)(gf_activity_t old_activity,
                                       gf_activity_t new_activity, void *ctx);

/* Goal reached callback */
typedef void (*gf_goal_callback_t)(const gf_step_goals_t *goals, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize step counter
 * @param config Configuration (NULL for defaults)
 */
int gf_step_init(const gf_step_config_t *config);

/**
 * @brief Start step counting
 */
int gf_step_start(void);

/**
 * @brief Stop step counting
 */
int gf_step_stop(void);

/**
 * @brief Reset step count for new session
 */
int gf_step_reset_session(void);

/**
 * @brief Reset daily step count (call at midnight)
 */
int gf_step_reset_daily(void);

/**
 * @brief Get current step counter state
 */
int gf_step_get_state(gf_step_state_t *state);

/**
 * @brief Get today's step count
 */
uint32_t gf_step_get_today(void);

/**
 * @brief Get hourly step history
 */
int gf_step_get_history(gf_step_history_t *history);

/**
 * @brief Set daily goals
 */
int gf_step_set_goals(const gf_step_goals_t *goals);

/**
 * @brief Get goal status
 */
int gf_step_get_goals(gf_step_goals_t *goals);

/**
 * @brief Update user profile for calorie estimation
 * @param height_cm User height
 * @param weight_kg User weight
 * @param stride_cm Stride length (0 for auto-estimate)
 */
int gf_step_set_user_profile(uint16_t height_cm, uint8_t weight_kg,
                             uint16_t stride_cm);

/**
 * @brief Register step callback
 * @param callback Called on each step or periodically
 * @param ctx User context
 */
int gf_step_register_callback(gf_step_callback_t callback, void *ctx);

/**
 * @brief Register activity change callback
 */
int gf_step_register_activity_cb(gf_activity_callback_t callback, void *ctx);

/**
 * @brief Register goal reached callback
 */
int gf_step_register_goal_cb(gf_goal_callback_t callback, void *ctx);

/**
 * @brief Feed accelerometer sample to step counter
 * @param sample Raw accelerometer data
 * @return 1 if step detected, 0 otherwise
 */
int gf_step_feed_sample(const gf_accel_sample_t *sample);

/**
 * @brief Process step counter (call from main loop)
 * @return Number of steps detected this call
 */
int gf_step_process(void);

/**
 * @brief Get current activity type
 */
gf_activity_t gf_step_get_activity(void);

/**
 * @brief Get estimated walking speed
 * @return Speed in meters per second (x100)
 */
uint16_t gf_step_get_speed(void);

/**
 * @brief Get estimated floors climbed
 */
uint16_t gf_step_get_floors(void);

/**
 * @brief Get power consumption estimate
 * @return Current draw in microamps
 */
uint32_t gf_step_get_power_ua(void);

#endif /* GF_STEP_COUNTER_H */
