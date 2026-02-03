/**
 * @file timekeeping.c
 * @brief Timekeeping Module Implementation
 */

#include "time/timekeeping.h"
#include "core/error_handler.h"
#include <string.h>
#include <stdio.h>

/* Local error codes (if error_handler.h not available) */
#ifndef GF_OK
#define GF_OK                       0
#define GF_ERROR_INVALID_PARAM      -1
#define GF_ERROR_NOT_INITIALIZED    -2
#define GF_ERROR_NOT_SUPPORTED      -3
#define GF_ERROR_NOT_AVAILABLE      -4
#endif

/*===========================================================================*/
/* RTC Driver Implementation                                                  */
/*===========================================================================*/

static gf_rtc_config_t s_rtc_config;
static gf_rtc_status_t s_rtc_status = GF_RTC_STATUS_NOT_INITIALIZED;
static gf_rtc_datetime_t s_rtc_time;
static bool s_alarm_pending;

/* Days in each month (non-leap year) */
static const uint8_t s_days_in_month[] = {
    31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};

static bool is_leap_year(uint16_t year)
{
    return (year % 4 == 0 && year % 100 != 0) || (year % 400 == 0);
}

int gf_rtc_init(const gf_rtc_config_t *config)
{
    if (!config) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    memcpy(&s_rtc_config, config, sizeof(gf_rtc_config_t));
    
    /* Initialize with default time (2024-01-01 00:00:00) */
    s_rtc_time.year = 2024;
    s_rtc_time.month = 1;
    s_rtc_time.day = 1;
    s_rtc_time.hour = 0;
    s_rtc_time.minute = 0;
    s_rtc_time.second = 0;
    s_rtc_time.weekday = 1; /* Monday */
    
    s_rtc_status = GF_RTC_STATUS_OK;
    s_alarm_pending = false;
    
    return GF_OK;
}

int gf_rtc_get_datetime(gf_rtc_datetime_t *dt)
{
    if (!dt) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    if (s_rtc_status == GF_RTC_STATUS_NOT_INITIALIZED) {
        return GF_ERROR_NOT_INITIALIZED;
    }
    
    memcpy(dt, &s_rtc_time, sizeof(gf_rtc_datetime_t));
    return GF_OK;
}

int gf_rtc_set_datetime(const gf_rtc_datetime_t *dt)
{
    if (!dt) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    /* Validate date/time */
    if (dt->year < 2000 || dt->year > 2099 ||
        dt->month < 1 || dt->month > 12 ||
        dt->day < 1 || dt->day > 31 ||
        dt->hour > 23 || dt->minute > 59 || dt->second > 59) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    /* Validate day of month */
    uint8_t max_day = s_days_in_month[dt->month - 1];
    if (dt->month == 2 && is_leap_year(dt->year)) {
        max_day = 29;
    }
    if (dt->day > max_day) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    memcpy(&s_rtc_time, dt, sizeof(gf_rtc_datetime_t));
    return GF_OK;
}

gf_rtc_status_t gf_rtc_get_status(void)
{
    return s_rtc_status;
}

int gf_rtc_get_unix_time(uint32_t *timestamp)
{
    if (!timestamp) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    if (s_rtc_status == GF_RTC_STATUS_NOT_INITIALIZED) {
        return GF_ERROR_NOT_INITIALIZED;
    }
    
    /* Calculate Unix timestamp */
    uint32_t days = 0;
    
    /* Days from 1970 to year */
    for (uint16_t y = 1970; y < s_rtc_time.year; y++) {
        days += is_leap_year(y) ? 366 : 365;
    }
    
    /* Days from January to month */
    for (uint8_t m = 1; m < s_rtc_time.month; m++) {
        days += s_days_in_month[m - 1];
        if (m == 2 && is_leap_year(s_rtc_time.year)) {
            days++;
        }
    }
    
    /* Days in current month */
    days += s_rtc_time.day - 1;
    
    *timestamp = days * 86400 + s_rtc_time.hour * 3600 + 
                 s_rtc_time.minute * 60 + s_rtc_time.second;
    
    return GF_OK;
}

int gf_rtc_set_alarm(const gf_rtc_datetime_t *alarm_time)
{
    if (!alarm_time) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    if (!s_rtc_config.enable_alarm) {
        return GF_ERROR_NOT_SUPPORTED;
    }
    
    /* Stub: would program RTC alarm registers */
    s_alarm_pending = true;
    return GF_OK;
}

void gf_rtc_clear_alarm(void)
{
    s_alarm_pending = false;
}

/*===========================================================================*/
/* NTP Synchronization Implementation                                         */
/*===========================================================================*/

static gf_ntp_config_t s_ntp_config;
static gf_ntp_stats_t s_ntp_stats;
static bool s_ntp_running;

int gf_ntp_init(const gf_ntp_config_t *config)
{
    if (!config) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    memcpy(&s_ntp_config, config, sizeof(gf_ntp_config_t));
    memset(&s_ntp_stats, 0, sizeof(gf_ntp_stats_t));
    s_ntp_stats.status = GF_NTP_STATUS_NOT_SYNCED;
    s_ntp_running = false;
    
    return GF_OK;
}

int gf_ntp_start(void)
{
    if (!s_ntp_config.server_primary) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    s_ntp_running = true;
    s_ntp_stats.status = GF_NTP_STATUS_SYNCING;
    
    /* Stub: would start NTP task */
    return GF_OK;
}

void gf_ntp_stop(void)
{
    s_ntp_running = false;
}

int gf_ntp_sync_now(void)
{
    if (!s_ntp_running) {
        return GF_ERROR_NOT_INITIALIZED;
    }
    
    /* Stub: would trigger immediate NTP request */
    s_ntp_stats.status = GF_NTP_STATUS_SYNCED;
    s_ntp_stats.sync_count++;
    
    return GF_OK;
}

void gf_ntp_get_stats(gf_ntp_stats_t *stats)
{
    if (stats) {
        memcpy(stats, &s_ntp_stats, sizeof(gf_ntp_stats_t));
    }
}

bool gf_ntp_is_synced(void)
{
    return s_ntp_stats.status == GF_NTP_STATUS_SYNCED;
}

/*===========================================================================*/
/* Timestamp Service Implementation                                           */
/*===========================================================================*/

static gf_timestamp_config_t s_ts_config;
static uint64_t s_monotonic_base;
static uint64_t s_uptime_start;
static bool s_ts_initialized;

int gf_timestamp_init(const gf_timestamp_config_t *config)
{
    if (!config) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    memcpy(&s_ts_config, config, sizeof(gf_timestamp_config_t));
    s_monotonic_base = 0;
    s_uptime_start = 0;
    s_ts_initialized = true;
    
    return GF_OK;
}

int gf_timestamp_get(gf_timestamp_t *ts)
{
    if (!ts) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    if (!s_ts_initialized) {
        return GF_ERROR_NOT_INITIALIZED;
    }
    
    /* Try NTP first, then RTC */
    if (s_ts_config.use_ntp && gf_ntp_is_synced()) {
        uint32_t unix_time;
        if (gf_rtc_get_unix_time(&unix_time) == GF_OK) {
            ts->unix_timestamp = unix_time + (s_ntp_stats.offset_ms / 1000);
            ts->milliseconds = (s_ntp_stats.offset_ms % 1000);
            ts->source = GF_TIME_SOURCE_NTP;
            ts->is_valid = true;
            return GF_OK;
        }
    }
    
    if (s_ts_config.use_rtc) {
        uint32_t unix_time;
        if (gf_rtc_get_unix_time(&unix_time) == GF_OK) {
            ts->unix_timestamp = unix_time;
            ts->milliseconds = 0;
            ts->source = GF_TIME_SOURCE_RTC;
            ts->is_valid = true;
            return GF_OK;
        }
    }
    
    /* Fallback to monotonic */
    if (s_ts_config.fallback_to_monotonic) {
        ts->unix_timestamp = 0;
        ts->milliseconds = (uint32_t)(gf_timestamp_monotonic_ms() % 1000);
        ts->source = GF_TIME_SOURCE_UNKNOWN;
        ts->is_valid = false;
        return GF_OK;
    }
    
    return GF_ERROR_NOT_AVAILABLE;
}

uint64_t gf_timestamp_monotonic_ms(void)
{
    /* Stub: would read HAL timer */
    return s_monotonic_base;
}

uint64_t gf_timestamp_uptime_ms(void)
{
    return gf_timestamp_monotonic_ms() - s_uptime_start;
}

int gf_timestamp_format_iso8601(const gf_timestamp_t *ts, char *buf, size_t len)
{
    if (!ts || !buf || len < 25) {
        return GF_ERROR_INVALID_PARAM;
    }
    
    /* Convert Unix timestamp to datetime */
    uint32_t unix = ts->unix_timestamp;
    uint32_t days = unix / 86400;
    uint32_t secs = unix % 86400;
    
    uint16_t year = 1970;
    while (days >= (is_leap_year(year) ? 366u : 365u)) {
        days -= is_leap_year(year) ? 366u : 365u;
        year++;
    }
    
    uint8_t month = 1;
    while (days >= s_days_in_month[month - 1]) {
        uint8_t dm = s_days_in_month[month - 1];
        if (month == 2 && is_leap_year(year)) dm++;
        if (days < dm) break;
        days -= dm;
        month++;
    }
    
    uint8_t day = (uint8_t)(days + 1);
    uint8_t hour = (uint8_t)(secs / 3600);
    uint8_t minute = (uint8_t)((secs % 3600) / 60);
    uint8_t second = (uint8_t)(secs % 60);
    
    snprintf(buf, len, "%04u-%02u-%02uT%02u:%02u:%02uZ",
             year, month, day, hour, minute, second);
    
    return GF_OK;
}

int gf_timestamp_compare(const gf_timestamp_t *a, const gf_timestamp_t *b)
{
    if (!a || !b) return 0;
    
    if (a->unix_timestamp != b->unix_timestamp) {
        return (a->unix_timestamp < b->unix_timestamp) ? -1 : 1;
    }
    
    if (a->milliseconds != b->milliseconds) {
        return (a->milliseconds < b->milliseconds) ? -1 : 1;
    }
    
    return 0;
}

int64_t gf_timestamp_diff_ms(const gf_timestamp_t *start, const gf_timestamp_t *end)
{
    if (!start || !end) return 0;
    
    int64_t diff_sec = (int64_t)end->unix_timestamp - (int64_t)start->unix_timestamp;
    int64_t diff_ms = (int64_t)end->milliseconds - (int64_t)start->milliseconds;
    
    return diff_sec * 1000 + diff_ms;
}
