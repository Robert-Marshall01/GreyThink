/**
 * @file grid_sync.h
 * @brief Grid Synchronization Module for Renewable Energy
 * 
 * INDUSTRY RELEVANCE:
 * Grid-tied renewable systems must synchronize with utility grids before
 * exporting power. This is critical for wind farms, solar installations,
 * and battery storage systems. Firmware engineers work on:
 * - Phase-locked loop (PLL) for grid angle tracking
 * - Frequency matching and droop control
 * - Voltage regulation and reactive power control
 * - Anti-islanding protection (IEEE 1547)
 * - Grid fault ride-through (LVRT/HVRT)
 * 
 * Companies: ABB, Siemens Energy, Schneider Electric, SMA Solar
 */

#ifndef GF_GRID_SYNC_H
#define GF_GRID_SYNC_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_GRID_NOMINAL_FREQ_HZ     50.0f   /* 50Hz (EU) or 60Hz (US) */
#define GF_GRID_NOMINAL_VOLTAGE_V   690.0f  /* Turbine output voltage */
#define GF_GRID_FREQ_TOLERANCE_HZ   0.5f
#define GF_GRID_VOLT_TOLERANCE_PCT  10.0f
#define GF_GRID_PHASE_TOLERANCE_DEG 5.0f

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_GRID_OK = 0,
    GF_GRID_ERROR_NULL_PTR,
    GF_GRID_ERROR_NOT_INITIALIZED,
    GF_GRID_ERROR_FREQ_OUT_OF_RANGE,
    GF_GRID_ERROR_VOLTAGE_OUT_OF_RANGE,
    GF_GRID_ERROR_PHASE_MISMATCH,
    GF_GRID_ERROR_ANTI_ISLAND_TRIP,
    GF_GRID_ERROR_FAULT_DETECTED,
    GF_GRID_ERROR_SYNC_TIMEOUT,
    GF_GRID_WARN_FREQ_DEVIATION,
    GF_GRID_WARN_VOLTAGE_SAG,
    GF_GRID_WARN_VOLTAGE_SWELL
} gf_grid_status_t;

typedef enum {
    GF_GRID_STATE_DISCONNECTED,
    GF_GRID_STATE_MONITORING,
    GF_GRID_STATE_PRE_SYNC,
    GF_GRID_STATE_SYNCHRONIZING,
    GF_GRID_STATE_CONNECTED,
    GF_GRID_STATE_EXPORTING,
    GF_GRID_STATE_FAULT_RIDE_THROUGH,
    GF_GRID_STATE_ISLANDED,
    GF_GRID_STATE_FAULT
} gf_grid_state_t;

typedef enum {
    GF_GRID_FAULT_NONE,
    GF_GRID_FAULT_UNDERVOLTAGE,
    GF_GRID_FAULT_OVERVOLTAGE,
    GF_GRID_FAULT_UNDERFREQ,
    GF_GRID_FAULT_OVERFREQ,
    GF_GRID_FAULT_PHASE_LOSS,
    GF_GRID_FAULT_SHORT_CIRCUIT
} gf_grid_fault_t;

/**
 * @brief Grid configuration
 */
typedef struct {
    float nominal_frequency_hz;     /* 50 or 60 Hz */
    float nominal_voltage_v;        /* Line voltage */
    float freq_tolerance_hz;        /* Frequency window */
    float voltage_tolerance_pct;    /* Voltage window */
    float power_factor_setpoint;    /* Target PF */
    float droop_coefficient;        /* Frequency droop % */
    bool anti_islanding_enabled;
    bool lvrt_enabled;              /* Low voltage ride through */
    uint16_t sync_timeout_ms;
} gf_grid_config_t;

/**
 * @brief Grid measurements
 */
typedef struct {
    float voltage_v[3];             /* Phase voltages */
    float current_a[3];             /* Phase currents */
    float frequency_hz;             /* Grid frequency */
    float phase_angle_deg;          /* Grid phase angle */
    float active_power_kw;          /* Real power */
    float reactive_power_kvar;      /* Reactive power */
    float power_factor;             /* Power factor */
    float thd_voltage_pct;          /* Voltage THD */
    float thd_current_pct;          /* Current THD */
} gf_grid_measurement_t;

/**
 * @brief Synchronization status
 */
typedef struct {
    float freq_error_hz;            /* Frequency difference */
    float voltage_error_v;          /* Voltage difference */
    float phase_error_deg;          /* Phase difference */
    bool freq_locked;               /* Frequency in window */
    bool voltage_matched;           /* Voltage in window */
    bool phase_locked;              /* Phase locked */
    bool ready_to_close;            /* All conditions met */
    uint32_t lock_time_ms;          /* Time in sync */
} gf_grid_sync_status_t;

/**
 * @brief Power export setpoints
 */
typedef struct {
    float active_power_kw;          /* Target P */
    float reactive_power_kvar;      /* Target Q */
    float power_factor;             /* Target PF */
    float ramp_rate_kw_s;           /* Power ramp rate */
    float max_export_kw;            /* Export limit */
} gf_grid_power_setpoint_t;

/*===========================================================================*/
/* Callbacks                                                                  */
/*===========================================================================*/

typedef void (*gf_grid_state_cb_t)(gf_grid_state_t old_state,
                                    gf_grid_state_t new_state,
                                    void* user_data);

typedef void (*gf_grid_fault_cb_t)(gf_grid_fault_t fault,
                                    const gf_grid_measurement_t* meas,
                                    void* user_data);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

gf_grid_status_t gf_grid_init(const gf_grid_config_t* config);
void gf_grid_shutdown(void);

/* State control */
gf_grid_status_t gf_grid_connect(void);
gf_grid_status_t gf_grid_disconnect(void);
gf_grid_state_t gf_grid_get_state(void);

/* Measurements */
gf_grid_status_t gf_grid_update_measurements(const gf_grid_measurement_t* meas);
gf_grid_status_t gf_grid_get_measurements(gf_grid_measurement_t* meas);

/* Synchronization */
gf_grid_status_t gf_grid_start_sync(void);
gf_grid_status_t gf_grid_get_sync_status(gf_grid_sync_status_t* status);
bool gf_grid_is_synchronized(void);

/* Power control */
gf_grid_status_t gf_grid_set_power(const gf_grid_power_setpoint_t* setpoint);
gf_grid_status_t gf_grid_set_power_factor(float pf);
gf_grid_status_t gf_grid_enable_export(bool enable);

/* Fault handling */
gf_grid_status_t gf_grid_get_fault(gf_grid_fault_t* fault);
gf_grid_status_t gf_grid_clear_fault(void);

/* Callbacks */
gf_grid_status_t gf_grid_register_state_callback(gf_grid_state_cb_t cb,
                                                   void* user_data);
gf_grid_status_t gf_grid_register_fault_callback(gf_grid_fault_cb_t cb,
                                                   void* user_data);

/* Periodic processing */
gf_grid_status_t gf_grid_process(void);

#endif /* GF_GRID_SYNC_H */
