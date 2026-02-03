/**
 * @file calibration.c
 * @brief Medical Device Calibration Stub Implementation
 */

#include "medical/calibration.h"
#include "core/driver_registry.h"
#include <string.h>
#include <math.h>

static struct {
    gf_cal_data_t   data[GF_CAL_MAX_CHANNELS];
    bool            in_progress[GF_CAL_MAX_CHANNELS];
    uint8_t         pending_points[GF_CAL_MAX_CHANNELS];
    char            operator_id[16];
    char            equipment_id[16];
    bool            initialized;
} s_cal;

/* Simple CRC32 for data integrity */
static uint32_t calc_crc32(const void *data, size_t len) {
    uint32_t crc = 0xFFFFFFFF;
    const uint8_t *p = data;
    while (len--) {
        crc ^= *p++;
        for (int i = 0; i < 8; i++)
            crc = (crc >> 1) ^ (0xEDB88320 & -(crc & 1));
    }
    return ~crc;
}

/* Calculate linear fit coefficients */
static void calc_linear_fit(gf_cal_data_t *d) {
    float sum_x = 0, sum_y = 0, sum_xy = 0, sum_x2 = 0;
    int n = d->point_count;
    
    for (int i = 0; i < n; i++) {
        float x = d->points[i].measured;
        float y = d->points[i].reference;
        sum_x += x; sum_y += y;
        sum_xy += x * y;
        sum_x2 += x * x;
    }
    
    float slope = (n * sum_xy - sum_x * sum_y) / (n * sum_x2 - sum_x * sum_x);
    float intercept = (sum_y - slope * sum_x) / n;
    
    d->coefficients[0] = intercept;  /* b */
    d->coefficients[1] = slope;      /* m */
}

int gf_cal_init(void) {
    memset(&s_cal, 0, sizeof(s_cal));
    s_cal.initialized = true;
    
    /* Load all calibrations from NVS */
    for (int i = 0; i < GF_CAL_MAX_CHANNELS; i++) {
        gf_cal_load(i);
    }
    
    return 0;
}

int gf_cal_load(uint8_t ch) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    /* Would read from flash/EEPROM */
    /* Verify CRC before accepting */
    return 0;
}

int gf_cal_save(uint8_t ch) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    
    /* Update CRC */
    s_cal.data[ch].crc32 = calc_crc32(&s_cal.data[ch], 
        sizeof(gf_cal_data_t) - sizeof(uint32_t));
    
    /* Would write to flash/EEPROM */
    return 0;
}

gf_cal_status_t gf_cal_get_status(uint8_t ch) {
    if (ch >= GF_CAL_MAX_CHANNELS) return GF_CAL_STATUS_INVALID;
    
    if (s_cal.in_progress[ch]) return GF_CAL_STATUS_PENDING;
    if (s_cal.data[ch].magic != GF_CAL_MAGIC) return GF_CAL_STATUS_INVALID;
    
    /* Check expiry */
    extern uint32_t gf_sched_get_ticks(void);
    uint32_t now = gf_sched_get_ticks() / 1000; /* Approximate seconds */
    if (s_cal.data[ch].expiry_date > 0 && now > s_cal.data[ch].expiry_date) {
        return GF_CAL_STATUS_EXPIRED;
    }
    
    return GF_CAL_STATUS_VALID;
}

int gf_cal_start(uint8_t ch, gf_cal_type_t type) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    
    memset(&s_cal.data[ch], 0, sizeof(gf_cal_data_t));
    s_cal.data[ch].channel = ch;
    s_cal.data[ch].type = type;
    s_cal.in_progress[ch] = true;
    s_cal.pending_points[ch] = 0;
    
    return 0;
}

int gf_cal_add_point(uint8_t ch, float reference, float measured) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    if (!s_cal.in_progress[ch]) return -2;
    if (s_cal.pending_points[ch] >= GF_CAL_MAX_POINTS) return -3;
    
    int idx = s_cal.pending_points[ch]++;
    s_cal.data[ch].points[idx].reference = reference;
    s_cal.data[ch].points[idx].measured = measured;
    
    return 0;
}

int gf_cal_complete(uint8_t ch) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    if (!s_cal.in_progress[ch]) return -2;
    if (s_cal.pending_points[ch] < 2) return -3;
    
    gf_cal_data_t *d = &s_cal.data[ch];
    d->point_count = s_cal.pending_points[ch];
    
    /* Calculate coefficients based on type */
    switch (d->type) {
        case GF_CAL_TYPE_LINEAR:
        case GF_CAL_TYPE_GAIN_OFFSET:
            calc_linear_fit(d);
            break;
        case GF_CAL_TYPE_POLYNOMIAL:
            /* Would use least squares polynomial fit */
            break;
        case GF_CAL_TYPE_LOOKUP:
            /* Points are used directly for interpolation */
            break;
    }
    
    /* Set metadata */
    d->magic = GF_CAL_MAGIC;
    d->cal_date = 0; /* Would get current timestamp */
    strncpy(d->operator_id, s_cal.operator_id, 15);
    strncpy(d->equipment_id, s_cal.equipment_id, 15);
    
    s_cal.in_progress[ch] = false;
    
    return gf_cal_save(ch);
}

int gf_cal_cancel(uint8_t ch) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    s_cal.in_progress[ch] = false;
    return 0;
}

float gf_cal_apply(uint8_t ch, float raw) {
    if (ch >= GF_CAL_MAX_CHANNELS) return raw;
    if (gf_cal_get_status(ch) != GF_CAL_STATUS_VALID) return raw;
    
    gf_cal_data_t *d = &s_cal.data[ch];
    
    switch (d->type) {
        case GF_CAL_TYPE_LINEAR:
        case GF_CAL_TYPE_GAIN_OFFSET:
            return d->coefficients[0] + d->coefficients[1] * raw;
            
        case GF_CAL_TYPE_POLYNOMIAL: {
            float result = d->coefficients[0];
            float x = raw;
            for (int i = 1; i < 8 && d->coefficients[i] != 0; i++) {
                result += d->coefficients[i] * x;
                x *= raw;
            }
            return result;
        }
        
        case GF_CAL_TYPE_LOOKUP: {
            /* Linear interpolation */
            for (int i = 0; i < d->point_count - 1; i++) {
                if (raw >= d->points[i].measured && 
                    raw <= d->points[i+1].measured) {
                    float t = (raw - d->points[i].measured) /
                              (d->points[i+1].measured - d->points[i].measured);
                    return d->points[i].reference + 
                           t * (d->points[i+1].reference - d->points[i].reference);
                }
            }
            return raw; /* Out of range */
        }
    }
    
    return raw;
}

int gf_cal_verify(uint8_t ch, float reference, float measured,
                   float tolerance, gf_cal_verify_result_t *result) {
    if (!result) return -1;
    
    float corrected = gf_cal_apply(ch, measured);
    float error = fabsf(corrected - reference);
    float error_pct = fabsf(error / reference) * 100.0f;
    
    result->passed = (error_pct <= tolerance);
    result->error = error;
    result->error_percent = error_pct;
    result->reference = reference;
    result->measured = measured;
    result->corrected = corrected;
    
    return 0;
}

int gf_cal_set_data(uint8_t ch, const gf_cal_data_t *data) {
    if (ch >= GF_CAL_MAX_CHANNELS || !data) return -1;
    memcpy(&s_cal.data[ch], data, sizeof(gf_cal_data_t));
    return gf_cal_save(ch);
}

int gf_cal_get_data(uint8_t ch, gf_cal_data_t *data) {
    if (ch >= GF_CAL_MAX_CHANNELS || !data) return -1;
    memcpy(data, &s_cal.data[ch], sizeof(gf_cal_data_t));
    return 0;
}

int gf_cal_clear(uint8_t ch) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    memset(&s_cal.data[ch], 0, sizeof(gf_cal_data_t));
    return gf_cal_save(ch);
}

bool gf_cal_any_expired(void) {
    for (int i = 0; i < GF_CAL_MAX_CHANNELS; i++) {
        if (gf_cal_get_status(i) == GF_CAL_STATUS_EXPIRED) return true;
    }
    return false;
}

int gf_cal_days_until_expiry(uint8_t ch) {
    if (ch >= GF_CAL_MAX_CHANNELS) return -1;
    if (s_cal.data[ch].expiry_date == 0) return 9999;
    
    extern uint32_t gf_sched_get_ticks(void);
    uint32_t now = gf_sched_get_ticks() / 1000;
    int32_t diff = s_cal.data[ch].expiry_date - now;
    return diff / 86400;
}

void gf_cal_set_operator(const char *id) {
    if (id) strncpy(s_cal.operator_id, id, 15);
}

void gf_cal_set_equipment(const char *id) {
    if (id) strncpy(s_cal.equipment_id, id, 15);
}

static int cal_drv_init(void *cfg) { (void)cfg; return gf_cal_init(); }
static gf_driver_t s_cal_driver = {
    .name = "calibration", .version = 0x0100,
    .ops = { .init = cal_drv_init }
};
const void* gf_cal_get_driver(void) { return &s_cal_driver; }
