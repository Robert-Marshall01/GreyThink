/**
 * @file climate_telemetry.h
 * @brief Climate Data Telemetry Collector
 * 
 * INDUSTRY RELEVANCE:
 * Climate research networks require standardized telemetry for data fusion
 * across heterogeneous sensor platforms. This module collects, aggregates,
 * and transmits climate data using internationally recognized formats
 * for interoperability with major climate databases.
 * 
 * TECHNICAL SCOPE:
 * - Multi-sensor data aggregation
 * - WMO BUFR/GRIB format encoding
 * - Quality control flags
 * - Temporal averaging (1-min, hourly, daily)
 * - Store-and-forward for intermittent connectivity
 * - Metadata management (station info, calibration)
 * 
 * DATA PRODUCTS:
 * - METAR-format weather observations
 * - SYNOP messages
 * - Climate Data Records (CDR)
 * - Air quality indices
 * 
 * STANDARDS COMPLIANCE:
 * - WMO BUFR FM 94 (Binary format)
 * - WMO Manual on Codes
 * - CF Conventions (NetCDF)
 * - ISO 19115 (Metadata)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_CLIMATE_TELEMETRY_H
#define GF_CLIMATE_TELEMETRY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define CLIMATE_MAX_STATIONS    8     /**< Multi-station support */
#define CLIMATE_BUFFER_SIZE     4096  /**< Telemetry buffer size */
#define CLIMATE_QUEUE_DEPTH     256   /**< Store-forward queue */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Telemetry format */
typedef enum {
    CLIMATE_FMT_BUFR,              /**< WMO BUFR binary */
    CLIMATE_FMT_JSON,              /**< JSON for REST APIs */
    CLIMATE_FMT_CSV,               /**< Simple CSV */
    CLIMATE_FMT_METAR,             /**< Aviation weather */
    CLIMATE_FMT_PROTOBUF           /**< Protocol Buffers */
} climate_format_t;

/** Data quality flag */
typedef enum {
    QC_GOOD,                       /**< Passed all checks */
    QC_SUSPECT,                    /**< Questionable value */
    QC_BAD,                        /**< Failed QC */
    QC_MISSING,                    /**< No data available */
    QC_ESTIMATED                   /**< Gap-filled estimate */
} qc_flag_t;

/** Averaging period */
typedef enum {
    AVG_INSTANTANEOUS,
    AVG_1_MINUTE,
    AVG_5_MINUTE,
    AVG_10_MINUTE,
    AVG_HOURLY,
    AVG_DAILY
} avg_period_t;

/** Station metadata */
typedef struct {
    char station_id[16];
    char station_name[64];
    float latitude;
    float longitude;
    float elevation_m;
    char wmo_id[8];
    char country_code[4];
    uint32_t sensors_installed;
} station_metadata_t;

/** Telemetry packet */
typedef struct {
    station_metadata_t station;
    avg_period_t period;
    uint64_t observation_time;
    float temperature_c;
    qc_flag_t temp_qc;
    float humidity_pct;
    qc_flag_t humid_qc;
    float pressure_hpa;
    qc_flag_t press_qc;
    float wind_speed_m_s;
    float wind_dir_deg;
    qc_flag_t wind_qc;
    float precipitation_mm;
    qc_flag_t precip_qc;
    float solar_w_m2;
    qc_flag_t solar_qc;
    uint16_t aqi;
    qc_flag_t aqi_qc;
} climate_observation_t;

/** Telemetry status */
typedef struct {
    uint32_t observations_sent;
    uint32_t observations_queued;
    uint32_t failed_transmissions;
    uint64_t last_successful_tx;
    bool connected;
    float queue_fill_pct;
} climate_tx_status_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize climate telemetry */
int climate_telemetry_init(void);

/** Set station metadata */
int climate_set_station(const station_metadata_t *meta);

/** Record observation */
int climate_record_observation(const climate_observation_t *obs);

/** Generate formatted telemetry */
int climate_generate_packet(climate_format_t format, uint8_t *buffer, size_t max_len);

/** Transmit queued data */
int climate_transmit(void);

/** Get transmission status */
int climate_get_tx_status(climate_tx_status_t *status);

/** Flush queue to storage */
int climate_flush_to_storage(void);

/** Process telemetry cycle */
int climate_process(uint32_t time_ms);

#ifdef __cplusplus
}
#endif

#endif /* GF_CLIMATE_TELEMETRY_H */
