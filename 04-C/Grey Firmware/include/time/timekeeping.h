/**
 * @file timekeeping.h
 * @brief Timekeeping Module (RTC, NTP, Timestamp Service)
 * 
 * INDUSTRY RELEVANCE:
 *   Accurate timekeeping is critical for embedded systems:
 *   - Industrial: Event logging, scheduling, process synchronization
 *   - IoT: Data timestamping, certificate validation, log correlation
 *   - Automotive: Diagnostic timestamp, fleet management
 *   - Medical: Audit trails, medication scheduling
 *   - Building Automation: Scheduling, energy management
 * 
 * This module demonstrates:
 *   - RTC driver abstraction (I2C/SPI RTC chips)
 *   - NTP synchronization stub
 *   - Unified timestamp service with monotonic/wall-clock time
 */

#ifndef GF_TIMEKEEPING_H
#define GF_TIMEKEEPING_H

#include <stdint.h>
#include <stdbool.h>
#include <stddef.h>

/*===========================================================================*/
/* RTC Driver                                                                 */
/*===========================================================================*/

typedef enum {
    GF_RTC_TYPE_INTERNAL = 0,       /* MCU internal RTC */
    GF_RTC_TYPE_DS1307,             /* I2C Dallas DS1307 */
    GF_RTC_TYPE_DS3231,             /* I2C Dallas DS3231 (TCXO) */
    GF_RTC_TYPE_PCF8563,            /* I2C NXP PCF8563 */
    GF_RTC_TYPE_MCP79410,           /* I2C Microchip MCP79410 */
    GF_RTC_TYPE_RV3028              /* I2C Micro Crystal RV-3028 */
} gf_rtc_type_t;

typedef enum {
    GF_RTC_STATUS_OK = 0,
    GF_RTC_STATUS_NOT_INITIALIZED,
    GF_RTC_STATUS_BATTERY_LOW,
    GF_RTC_STATUS_OSCILLATOR_STOP,
    GF_RTC_STATUS_COMM_ERROR
} gf_rtc_status_t;

typedef struct {
    uint16_t year;      /* 2000-2099 */
    uint8_t  month;     /* 1-12 */
    uint8_t  day;       /* 1-31 */
    uint8_t  hour;      /* 0-23 */
    uint8_t  minute;    /* 0-59 */
    uint8_t  second;    /* 0-59 */
    uint8_t  weekday;   /* 0-6 (Sunday=0) */
} gf_rtc_datetime_t;

typedef struct {
    gf_rtc_type_t   type;
    uint8_t         i2c_addr;       /* I2C address (default: chip-specific) */
    bool            use_24h;        /* 24-hour format */
    bool            enable_alarm;   /* Enable alarm interrupts */
} gf_rtc_config_t;

/**
 * @brief Initialize RTC driver
 */
int gf_rtc_init(const gf_rtc_config_t *config);

/**
 * @brief Get current date/time
 */
int gf_rtc_get_datetime(gf_rtc_datetime_t *dt);

/**
 * @brief Set date/time
 */
int gf_rtc_set_datetime(const gf_rtc_datetime_t *dt);

/**
 * @brief Get RTC status
 */
gf_rtc_status_t gf_rtc_get_status(void);

/**
 * @brief Get Unix timestamp (seconds since 1970-01-01)
 */
int gf_rtc_get_unix_time(uint32_t *timestamp);

/**
 * @brief Set alarm
 */
int gf_rtc_set_alarm(const gf_rtc_datetime_t *alarm_time);

/**
 * @brief Clear alarm flag
 */
void gf_rtc_clear_alarm(void);

/*===========================================================================*/
/* NTP Synchronization                                                        */
/*===========================================================================*/

typedef enum {
    GF_NTP_STATUS_NOT_SYNCED = 0,
    GF_NTP_STATUS_SYNCING,
    GF_NTP_STATUS_SYNCED,
    GF_NTP_STATUS_STALE,            /* Last sync too old */
    GF_NTP_STATUS_ERROR
} gf_ntp_status_t;

typedef struct {
    const char     *server_primary;
    const char     *server_secondary;
    uint32_t        sync_interval_s;    /* How often to sync (default: 3600) */
    uint32_t        timeout_ms;         /* NTP request timeout */
    int16_t         timezone_offset_min;/* Minutes from UTC */
    bool            enable_dst;         /* Daylight saving time */
} gf_ntp_config_t;

typedef struct {
    gf_ntp_status_t status;
    uint32_t        last_sync_time;     /* Unix timestamp of last sync */
    int32_t         offset_ms;          /* Measured offset from server */
    uint32_t        rtt_ms;             /* Round-trip time */
    uint8_t         stratum;            /* NTP stratum level */
    uint32_t        sync_count;         /* Successful syncs */
    uint32_t        fail_count;         /* Failed attempts */
} gf_ntp_stats_t;

/**
 * @brief Initialize NTP client
 */
int gf_ntp_init(const gf_ntp_config_t *config);

/**
 * @brief Start NTP synchronization
 */
int gf_ntp_start(void);

/**
 * @brief Stop NTP synchronization
 */
void gf_ntp_stop(void);

/**
 * @brief Force immediate sync
 */
int gf_ntp_sync_now(void);

/**
 * @brief Get NTP statistics
 */
void gf_ntp_get_stats(gf_ntp_stats_t *stats);

/**
 * @brief Check if time is synchronized
 */
bool gf_ntp_is_synced(void);

/*===========================================================================*/
/* Timestamp Service                                                          */
/*===========================================================================*/

typedef enum {
    GF_TIME_SOURCE_UNKNOWN = 0,
    GF_TIME_SOURCE_RTC,
    GF_TIME_SOURCE_NTP,
    GF_TIME_SOURCE_GPS,
    GF_TIME_SOURCE_MANUAL
} gf_time_source_t;

typedef struct {
    uint32_t        unix_timestamp;     /* Seconds since epoch */
    uint32_t        milliseconds;       /* Fractional milliseconds */
    gf_time_source_t source;
    bool            is_valid;
} gf_timestamp_t;

typedef struct {
    bool            use_rtc;
    bool            use_ntp;
    bool            fallback_to_monotonic;
    uint32_t        max_drift_ppm;      /* Max acceptable drift */
} gf_timestamp_config_t;

/**
 * @brief Initialize timestamp service
 */
int gf_timestamp_init(const gf_timestamp_config_t *config);

/**
 * @brief Get current timestamp
 */
int gf_timestamp_get(gf_timestamp_t *ts);

/**
 * @brief Get monotonic time (guaranteed increasing, survives rollover)
 */
uint64_t gf_timestamp_monotonic_ms(void);

/**
 * @brief Get uptime in milliseconds
 */
uint64_t gf_timestamp_uptime_ms(void);

/**
 * @brief Format timestamp as ISO 8601 string
 */
int gf_timestamp_format_iso8601(const gf_timestamp_t *ts, char *buf, size_t len);

/**
 * @brief Compare two timestamps
 * @return <0 if a<b, 0 if a==b, >0 if a>b
 */
int gf_timestamp_compare(const gf_timestamp_t *a, const gf_timestamp_t *b);

/**
 * @brief Get elapsed time between two timestamps in milliseconds
 */
int64_t gf_timestamp_diff_ms(const gf_timestamp_t *start, const gf_timestamp_t *end);

#endif /* GF_TIMEKEEPING_H */
