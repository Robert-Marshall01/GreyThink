/**
 * @file wind_turbine.h
 * @brief Wind Turbine Control Interface
 * 
 * INDUSTRY RELEVANCE:
 * Wind energy is the fastest-growing renewable source globally. Firmware engineers
 * in this space work on turbine control systems at companies like Vestas, GE Renewable,
 * Siemens Gamesa, and Goldwind. Key challenges include:
 * - Variable-speed turbine control for optimal power extraction (MPPT)
 * - Pitch control for power limiting and emergency braking
 * - Yaw control for wind direction tracking
 * - Structural load monitoring and fatigue management
 * - Grid code compliance (frequency/voltage ride-through)
 * 
 * This stub demonstrates pitch/yaw control, rotor speed regulation, and
 * integration with grid synchronization systems.
 */

#ifndef GF_WIND_TURBINE_H
#define GF_WIND_TURBINE_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_TURBINE_MAX_BLADES           3
#define GF_TURBINE_PITCH_RANGE_DEG      90.0f
#define GF_TURBINE_YAW_RANGE_DEG        360.0f
#define GF_TURBINE_MAX_ROTOR_RPM        20.0f
#define GF_TURBINE_RATED_POWER_KW       2000.0f

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_TURBINE_OK = 0,
    GF_TURBINE_ERROR_NULL_PTR,
    GF_TURBINE_ERROR_NOT_INITIALIZED,
    GF_TURBINE_ERROR_INVALID_PARAM,
    GF_TURBINE_ERROR_PITCH_FAULT,
    GF_TURBINE_ERROR_YAW_FAULT,
    GF_TURBINE_ERROR_OVERSPEED,
    GF_TURBINE_ERROR_GRID_FAULT,
    GF_TURBINE_ERROR_SENSOR_FAULT,
    GF_TURBINE_WARN_HIGH_WIND,
    GF_TURBINE_WARN_LOW_WIND
} gf_turbine_status_t;

typedef enum {
    GF_TURBINE_STATE_STOPPED,
    GF_TURBINE_STATE_STARTING,
    GF_TURBINE_STATE_RUNNING,
    GF_TURBINE_STATE_POWER_PRODUCTION,
    GF_TURBINE_STATE_STOPPING,
    GF_TURBINE_STATE_EMERGENCY_STOP,
    GF_TURBINE_STATE_MAINTENANCE,
    GF_TURBINE_STATE_FAULT
} gf_turbine_state_t;

typedef enum {
    GF_TURBINE_CONTROL_MPPT,        /* Maximum power point tracking */
    GF_TURBINE_CONTROL_POWER_LIMIT, /* Power curtailment mode */
    GF_TURBINE_CONTROL_FREQUENCY,   /* Grid frequency support */
    GF_TURBINE_CONTROL_MANUAL       /* Manual control for testing */
} gf_turbine_control_mode_t;

/**
 * @brief Turbine configuration
 */
typedef struct {
    float rated_power_kw;           /* Rated power output */
    float cut_in_wind_mps;          /* Minimum wind speed to start */
    float cut_out_wind_mps;         /* Maximum operating wind speed */
    float rated_wind_mps;           /* Wind speed at rated power */
    float rotor_diameter_m;         /* Rotor diameter */
    uint8_t num_blades;             /* Number of blades */
    float gear_ratio;               /* Gearbox ratio */
    float generator_poles;          /* Generator pole pairs */
    gf_turbine_control_mode_t mode; /* Control mode */
} gf_turbine_config_t;

/**
 * @brief Blade pitch state
 */
typedef struct {
    float angle_deg[GF_TURBINE_MAX_BLADES];  /* Current pitch angles */
    float setpoint_deg;                       /* Target pitch angle */
    float rate_dps;                           /* Pitch rate (deg/s) */
    bool synchronized;                        /* All blades synchronized */
} gf_turbine_pitch_t;

/**
 * @brief Nacelle yaw state
 */
typedef struct {
    float angle_deg;                /* Current yaw angle */
    float wind_direction_deg;       /* Wind direction */
    float error_deg;                /* Yaw error */
    bool tracking;                  /* Active tracking flag */
} gf_turbine_yaw_t;

/**
 * @brief Rotor state
 */
typedef struct {
    float speed_rpm;                /* Rotor speed */
    float speed_setpoint_rpm;       /* Target rotor speed */
    float torque_nm;                /* Shaft torque */
    float power_kw;                 /* Mechanical power */
    float tip_speed_ratio;          /* Optimal ~6-8 for 3-blade */
} gf_turbine_rotor_t;

/**
 * @brief Generator state
 */
typedef struct {
    float power_kw;                 /* Electrical power output */
    float voltage_v;                /* Generator voltage */
    float current_a;                /* Generator current */
    float frequency_hz;             /* Generator frequency */
    float power_factor;             /* Power factor */
    float efficiency;               /* Generator efficiency */
    int8_t temperature_c;           /* Winding temperature */
} gf_turbine_generator_t;

/**
 * @brief Wind measurement
 */
typedef struct {
    float speed_mps;                /* Wind speed */
    float direction_deg;            /* Wind direction */
    float gust_mps;                 /* Gust speed (3s average) */
    float turbulence_intensity;     /* TI = σ/mean */
    int8_t temperature_c;           /* Air temperature */
    float density_kgm3;             /* Air density */
} gf_turbine_wind_t;

/**
 * @brief Turbine telemetry
 */
typedef struct {
    uint32_t timestamp_ms;
    gf_turbine_state_t state;
    gf_turbine_wind_t wind;
    gf_turbine_rotor_t rotor;
    gf_turbine_generator_t generator;
    gf_turbine_pitch_t pitch;
    gf_turbine_yaw_t yaw;
    float capacity_factor;          /* Actual/rated power ratio */
    uint32_t runtime_hours;
    uint32_t energy_produced_kwh;
} gf_turbine_telemetry_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_turbine_state_cb_t)(gf_turbine_state_t old_state,
                                       gf_turbine_state_t new_state,
                                       void* user_data);

typedef void (*gf_turbine_alarm_cb_t)(gf_turbine_status_t alarm,
                                       const char* message,
                                       void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_turbine_status_t gf_turbine_init(const gf_turbine_config_t* config);
void gf_turbine_shutdown(void);

/* State control */
gf_turbine_status_t gf_turbine_start(void);
gf_turbine_status_t gf_turbine_stop(void);
gf_turbine_status_t gf_turbine_emergency_stop(void);
gf_turbine_state_t gf_turbine_get_state(void);

/* Pitch control */
gf_turbine_status_t gf_turbine_set_pitch(float angle_deg);
gf_turbine_status_t gf_turbine_get_pitch(gf_turbine_pitch_t* pitch);

/* Yaw control */
gf_turbine_status_t gf_turbine_set_yaw(float angle_deg);
gf_turbine_status_t gf_turbine_enable_yaw_tracking(bool enable);

/* Power control */
gf_turbine_status_t gf_turbine_set_power_limit(float power_kw);
gf_turbine_status_t gf_turbine_set_control_mode(gf_turbine_control_mode_t mode);

/* Wind input (from anemometer) */
gf_turbine_status_t gf_turbine_update_wind(const gf_turbine_wind_t* wind);

/* Telemetry */
gf_turbine_status_t gf_turbine_get_telemetry(gf_turbine_telemetry_t* telemetry);

/* Callbacks */
gf_turbine_status_t gf_turbine_register_state_callback(gf_turbine_state_cb_t cb,
                                                        void* user_data);
gf_turbine_status_t gf_turbine_register_alarm_callback(gf_turbine_alarm_cb_t cb,
                                                        void* user_data);

/* Periodic processing */
gf_turbine_status_t gf_turbine_process(void);

#endif /* GF_WIND_TURBINE_H */
