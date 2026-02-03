/**
 * @file climate_data.h
 * @brief Climate Data Collector Stub
 * 
 * INDUSTRY RELEVANCE:
 * Climate monitoring systems are deployed globally for weather forecasting,
 * agricultural optimization, and climate research. The weather monitoring
 * market exceeds $2B with IoT-connected stations growing rapidly. Firmware
 * engineers work with precision sensors, solar power management, and
 * long-range communication (LoRa, satellite) for remote deployments.
 * 
 * Key challenges:
 * - Extreme environment operation (-40°C to +60°C)
 * - Ultra-low power for multi-year battery life
 * - Precision measurement (WMO standards)
 * - Remote firmware updates via satellite
 * - Data integrity across communication gaps
 */

#ifndef GF_CLIMATE_DATA_H
#define GF_CLIMATE_DATA_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Climate collector status codes */
typedef enum {
    GF_CLIMATE_OK = 0,
    GF_CLIMATE_SENSOR_FAULT,        /* Sensor hardware failure */
    GF_CLIMATE_COMM_ERROR,          /* Communication failure */
    GF_CLIMATE_POWER_LOW,           /* Low power warning */
    GF_CLIMATE_STORAGE_FULL,        /* Local storage full */
    GF_CLIMATE_GPS_ERROR,           /* GPS fix lost */
    GF_CLIMATE_ICING,               /* Sensor icing detected */
    GF_CLIMATE_MAINTENANCE          /* Maintenance required */
} gf_climate_status_t;

/* Weather sensor types */
typedef enum {
    GF_CLIMATE_SENSOR_TEMP,         /* Air temperature */
    GF_CLIMATE_SENSOR_HUMIDITY,     /* Relative humidity */
    GF_CLIMATE_SENSOR_PRESSURE,     /* Barometric pressure */
    GF_CLIMATE_SENSOR_WIND_SPEED,   /* Wind speed */
    GF_CLIMATE_SENSOR_WIND_DIR,     /* Wind direction */
    GF_CLIMATE_SENSOR_RAIN,         /* Precipitation */
    GF_CLIMATE_SENSOR_SOLAR,        /* Solar radiation */
    GF_CLIMATE_SENSOR_UV,           /* UV index */
    GF_CLIMATE_SENSOR_SOIL_TEMP,    /* Soil temperature */
    GF_CLIMATE_SENSOR_SOIL_MOIST,   /* Soil moisture */
    GF_CLIMATE_SENSOR_COUNT
} gf_climate_sensor_t;

/* Communication modes */
typedef enum {
    GF_CLIMATE_COMM_CELLULAR,       /* Cellular (LTE-M/NB-IoT) */
    GF_CLIMATE_COMM_LORA,           /* LoRaWAN */
    GF_CLIMATE_COMM_SATELLITE,      /* Satellite (Iridium) */
    GF_CLIMATE_COMM_WIFI,           /* WiFi */
    GF_CLIMATE_COMM_OFFLINE         /* Store locally only */
} gf_climate_comm_t;

/* Station configuration */
typedef struct {
    char station_id[16];            /* Unique station ID */
    float latitude;                 /* GPS latitude */
    float longitude;                /* GPS longitude */
    float elevation_m;              /* Elevation in meters */
    uint16_t sample_interval_sec;   /* Sampling interval */
    uint16_t report_interval_sec;   /* Reporting interval */
    gf_climate_comm_t comm_mode;    /* Communication mode */
    uint16_t enabled_sensors;       /* Bitmask of enabled sensors */
    bool enable_store_forward;      /* Store when offline */
} gf_climate_config_t;

/* Wind measurement */
typedef struct {
    float speed_ms;                 /* Wind speed (m/s) */
    float gust_ms;                  /* Gust speed (m/s) */
    uint16_t direction_deg;         /* Direction (0-359°) */
    float avg_speed_10min;          /* 10-minute average */
    float max_gust_10min;           /* 10-minute max gust */
} gf_climate_wind_t;

/* Precipitation measurement */
typedef struct {
    float rain_mm_hour;             /* Hourly rainfall (mm) */
    float rain_mm_day;              /* Daily rainfall (mm) */
    float rain_mm_month;            /* Monthly rainfall (mm) */
    uint32_t tips_count;            /* Rain gauge tips */
    bool is_raining;                /* Currently raining */
} gf_climate_precip_t;

/* Complete weather observation */
typedef struct {
    float temperature_c;            /* Air temperature */
    float humidity_percent;         /* Relative humidity */
    float pressure_hpa;             /* Sea-level pressure */
    float dewpoint_c;               /* Dew point temperature */
    gf_climate_wind_t wind;         /* Wind measurements */
    gf_climate_precip_t precip;     /* Precipitation data */
    float solar_wm2;                /* Solar radiation (W/m²) */
    float uv_index;                 /* UV index (0-11+) */
    float soil_temp_c;              /* Soil temperature */
    float soil_moisture_pct;        /* Soil moisture % */
    uint32_t timestamp;             /* Observation timestamp */
    gf_climate_status_t status;     /* Station status */
    float battery_volts;            /* Battery voltage */
    int8_t signal_strength;         /* Comm signal strength */
} gf_climate_observation_t;

/* Daily summary statistics */
typedef struct {
    float temp_min_c;               /* Minimum temperature */
    float temp_max_c;               /* Maximum temperature */
    float temp_avg_c;               /* Average temperature */
    float humidity_avg;             /* Average humidity */
    float pressure_min_hpa;         /* Minimum pressure */
    float pressure_max_hpa;         /* Maximum pressure */
    float wind_max_gust_ms;         /* Maximum wind gust */
    float rain_total_mm;            /* Total precipitation */
    float solar_total_mj;           /* Total solar (MJ/m²) */
    uint32_t sample_count;          /* Samples in period */
} gf_climate_daily_summary_t;

/**
 * @brief Initialize climate data collector
 * @param config Station configuration
 * @return Status code
 */
gf_climate_status_t gf_climate_init(const gf_climate_config_t* config);

/**
 * @brief Start data collection
 * @return Status code
 */
gf_climate_status_t gf_climate_start(void);

/**
 * @brief Stop data collection
 * @return Status code
 */
gf_climate_status_t gf_climate_stop(void);

/**
 * @brief Get current observation
 * @param obs Output for observation data
 * @return Status code
 */
gf_climate_status_t gf_climate_get_observation(gf_climate_observation_t* obs);

/**
 * @brief Get specific sensor reading
 * @param sensor Sensor type
 * @param value Output for value
 * @return Status code
 */
gf_climate_status_t gf_climate_get_sensor(gf_climate_sensor_t sensor, float* value);

/**
 * @brief Get daily summary
 * @param summary Output for daily summary
 * @return Status code
 */
gf_climate_status_t gf_climate_get_daily_summary(gf_climate_daily_summary_t* summary);

/**
 * @brief Transmit observation to server
 * @param obs Observation to transmit
 * @return Status code
 */
gf_climate_status_t gf_climate_transmit(const gf_climate_observation_t* obs);

/**
 * @brief Get stored observations count (when offline)
 * @param count Output for count
 * @return Status code
 */
gf_climate_status_t gf_climate_get_stored_count(uint32_t* count);

/**
 * @brief Flush stored observations
 * @return Status code
 */
gf_climate_status_t gf_climate_flush_stored(void);

/**
 * @brief Reset rain gauge totals
 * @return Status code
 */
gf_climate_status_t gf_climate_reset_rain(void);

/**
 * @brief Perform sensor self-test
 * @return Status code (OK if passed)
 */
gf_climate_status_t gf_climate_self_test(void);

/**
 * @brief Enter low-power mode
 * @param duration_sec Sleep duration
 * @return Status code
 */
gf_climate_status_t gf_climate_sleep(uint32_t duration_sec);

/**
 * @brief Shutdown and release resources
 */
void gf_climate_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_CLIMATE_DATA_H */
