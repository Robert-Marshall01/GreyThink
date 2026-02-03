/**
 * @file gas_sensor.h
 * @brief Industrial Gas Sensor Driver Stub
 * 
 * INDUSTRY RELEVANCE:
 * Gas detection is critical for industrial safety, mining, oil & gas, chemical
 * plants, and building safety systems. Sensors detect toxic gases (CO, H2S, NO2),
 * combustible gases (CH4, propane), and oxygen levels. This is a $3B+ market
 * with strict regulatory requirements (ATEX, IECEx, SIL certification).
 * 
 * WHY THIS MATTERS:
 * - Electrochemical and catalytic sensor interfaces
 * - Temperature compensation for accurate readings
 * - Alarm thresholds (TWA, STEL, IDLH)
 * - Fault detection and sensor lifetime management
 * - Hazardous area compliance
 * 
 * GREY FIRMWARE DEMONSTRATES:
 * - Multi-gas sensor management
 * - Alarm system integration
 * - Calibration and compensation
 * - Safety-critical data handling
 */

#ifndef GF_GAS_SENSOR_H
#define GF_GAS_SENSOR_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_GAS_MAX_SENSORS          8       /* Maximum sensor channels */
#define GF_GAS_HISTORY_SIZE         128     /* Readings history */
#define GF_GAS_CAL_POINTS           3       /* Calibration points */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

/* Gas types */
typedef enum {
    GF_GAS_O2 = 0,              /* Oxygen (19.5-23.5% normal) */
    GF_GAS_CO,                  /* Carbon monoxide */
    GF_GAS_H2S,                 /* Hydrogen sulfide */
    GF_GAS_CH4,                 /* Methane (LEL) */
    GF_GAS_NO2,                 /* Nitrogen dioxide */
    GF_GAS_SO2,                 /* Sulfur dioxide */
    GF_GAS_NH3,                 /* Ammonia */
    GF_GAS_CL2,                 /* Chlorine */
    GF_GAS_CO2,                 /* Carbon dioxide */
    GF_GAS_VOC,                 /* Volatile organic compounds */
    GF_GAS_COMBUSTIBLE          /* General combustible (LEL) */
} gf_gas_type_t;

/* Sensor technology */
typedef enum {
    GF_GAS_TECH_ELECTROCHEMICAL = 0,    /* Electrochemical cell */
    GF_GAS_TECH_CATALYTIC,              /* Catalytic bead */
    GF_GAS_TECH_INFRARED,               /* NDIR */
    GF_GAS_TECH_PHOTOIONIZATION,        /* PID */
    GF_GAS_TECH_SEMICONDUCTER           /* MOS */
} gf_gas_tech_t;

/* Sensor state */
typedef enum {
    GF_GAS_STATE_OFF = 0,
    GF_GAS_STATE_WARMING,       /* Sensor warmup period */
    GF_GAS_STATE_READY,         /* Normal operation */
    GF_GAS_STATE_CALIBRATING,   /* Calibration in progress */
    GF_GAS_STATE_FAULT,         /* Sensor fault */
    GF_GAS_STATE_EXPIRED        /* Sensor end-of-life */
} gf_gas_state_t;

/* Alarm level */
typedef enum {
    GF_GAS_ALARM_NONE = 0,
    GF_GAS_ALARM_LOW,           /* Low alarm (TWA) */
    GF_GAS_ALARM_HIGH,          /* High alarm (STEL) */
    GF_GAS_ALARM_CRITICAL,      /* Critical/IDLH */
    GF_GAS_ALARM_OVER_RANGE     /* Sensor saturated */
} gf_gas_alarm_t;

/* Sensor configuration */
typedef struct {
    uint8_t         channel;        /* Sensor channel ID */
    gf_gas_type_t   gas_type;       /* Gas being detected */
    gf_gas_tech_t   technology;     /* Sensor technology */
    float           range_min;      /* Minimum measurement (ppm or %) */
    float           range_max;      /* Maximum measurement */
    float           resolution;     /* Resolution */
    float           alarm_low;      /* Low alarm threshold */
    float           alarm_high;     /* High alarm threshold */
    float           alarm_critical; /* Critical alarm threshold */
    uint16_t        warmup_sec;     /* Warmup time in seconds */
    uint16_t        response_sec;   /* T90 response time */
    uint16_t        lifetime_days;  /* Expected sensor lifetime */
    char            serial[16];     /* Sensor serial number */
} gf_gas_config_t;

/* Sensor reading */
typedef struct {
    uint8_t         channel;
    gf_gas_type_t   gas_type;
    float           concentration;  /* Current reading (ppm or %) */
    float           twa_8hr;        /* 8-hour time-weighted average */
    float           stel_15min;     /* 15-min short-term exposure */
    float           peak;           /* Peak value since reset */
    gf_gas_alarm_t  alarm;          /* Current alarm state */
    gf_gas_state_t  state;          /* Sensor state */
    float           temperature_c;  /* Sensor temperature */
    uint32_t        timestamp;      /* Reading timestamp */
    uint8_t         confidence;     /* Reading confidence (0-100) */
} gf_gas_reading_t;

/* Calibration data */
typedef struct {
    float           zero_offset;    /* Zero gas offset */
    float           span_factor;    /* Span calibration factor */
    float           temp_coeff;     /* Temperature coefficient */
    uint32_t        cal_date;       /* Last calibration date */
    uint32_t        next_cal_date;  /* Next calibration due */
    uint8_t         cal_gas_conc;   /* Calibration gas concentration */
} gf_gas_cal_t;

/* Alarm callback */
typedef void (*gf_gas_alarm_cb_t)(uint8_t channel, gf_gas_type_t gas,
                                   gf_gas_alarm_t alarm, float concentration,
                                   void *ctx);

/* Fault callback */
typedef void (*gf_gas_fault_cb_t)(uint8_t channel, uint16_t fault_code,
                                   const char *message, void *ctx);

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize gas sensor subsystem
 */
int gf_gas_init(void);

/**
 * @brief Register a gas sensor channel
 * @param config Sensor configuration
 * @return Channel ID or error
 */
int gf_gas_register(const gf_gas_config_t *config);

/**
 * @brief Start sensor operation
 */
int gf_gas_start(uint8_t channel);

/**
 * @brief Stop sensor
 */
int gf_gas_stop(uint8_t channel);

/**
 * @brief Get current reading
 */
int gf_gas_get_reading(uint8_t channel, gf_gas_reading_t *reading);

/**
 * @brief Get all sensor readings
 */
int gf_gas_get_all_readings(gf_gas_reading_t *readings, int max_readings);

/**
 * @brief Set alarm thresholds
 */
int gf_gas_set_alarms(uint8_t channel, float low, float high, float critical);

/**
 * @brief Acknowledge alarm
 */
int gf_gas_ack_alarm(uint8_t channel);

/**
 * @brief Start zero calibration (in clean air)
 */
int gf_gas_calibrate_zero(uint8_t channel);

/**
 * @brief Start span calibration (with known gas)
 * @param channel Sensor channel
 * @param gas_concentration Known calibration gas concentration
 */
int gf_gas_calibrate_span(uint8_t channel, float gas_concentration);

/**
 * @brief Get calibration data
 */
int gf_gas_get_calibration(uint8_t channel, gf_gas_cal_t *cal);

/**
 * @brief Set calibration data (from storage)
 */
int gf_gas_set_calibration(uint8_t channel, const gf_gas_cal_t *cal);

/**
 * @brief Register alarm callback
 */
int gf_gas_register_alarm_cb(gf_gas_alarm_cb_t callback, void *ctx);

/**
 * @brief Register fault callback
 */
int gf_gas_register_fault_cb(gf_gas_fault_cb_t callback, void *ctx);

/**
 * @brief Reset peak value
 */
int gf_gas_reset_peak(uint8_t channel);

/**
 * @brief Reset TWA/STEL accumulators
 */
int gf_gas_reset_twa(uint8_t channel);

/**
 * @brief Run sensor self-test
 */
int gf_gas_self_test(uint8_t channel);

/**
 * @brief Get sensor remaining lifetime
 * @return Days remaining or negative for expired
 */
int gf_gas_get_lifetime_days(uint8_t channel);

/**
 * @brief Process sensor readings (call from main loop)
 */
int gf_gas_process(void);

/**
 * @brief Get sensor state
 */
gf_gas_state_t gf_gas_get_state(uint8_t channel);

#endif /* GF_GAS_SENSOR_H */
