/**
 * @file calibration.h
 * @brief Medical Device Calibration Routine Stub
 * 
 * WHAT: Calibration management for medical sensors with multi-point
 *       calibration, compensation algorithms, and calibration verification.
 * 
 * WHY: Accurate measurements require calibration to compensate for component
 *      tolerances, aging, and environmental factors. Medical devices must
 *      demonstrate traceability to national standards and maintain calibration
 *      certificates. This is a core regulatory requirement.
 * 
 * HOW GREY FIRMWARE DEMONSTRATES THIS:
 *      - Multi-point calibration curves
 *      - Linear, polynomial, and lookup interpolation
 *      - Calibration storage in non-volatile memory
 *      - Calibration drift detection
 *      - Recalibration scheduling
 * 
 * Industry applications: blood analyzers, imaging systems, diagnostic devices
 * 
 * NOTE: Annotated stub. Production requires ISO 13485 compliance documentation.
 */

#ifndef GF_CALIBRATION_H
#define GF_CALIBRATION_H

#include <stdint.h>
#include <stdbool.h>

/*===========================================================================*/
/* Configuration                                                              */
/*===========================================================================*/

#define GF_CAL_MAX_POINTS           16      /* Max calibration points */
#define GF_CAL_MAX_CHANNELS         8       /* Max calibrated channels */
#define GF_CAL_MAGIC                0x43414C31  /* "CAL1" */

/*===========================================================================*/
/* Types                                                                      */
/*===========================================================================*/

typedef enum {
    GF_CAL_TYPE_LINEAR = 0,     /* y = mx + b */
    GF_CAL_TYPE_POLYNOMIAL,     /* y = a0 + a1*x + a2*x^2 + ... */
    GF_CAL_TYPE_LOOKUP,         /* Linear interpolation between points */
    GF_CAL_TYPE_GAIN_OFFSET     /* Simple gain and offset */
} gf_cal_type_t;

typedef enum {
    GF_CAL_STATUS_VALID = 0,
    GF_CAL_STATUS_EXPIRED,
    GF_CAL_STATUS_INVALID,
    GF_CAL_STATUS_PENDING
} gf_cal_status_t;

/* Single calibration point */
typedef struct {
    float   reference;      /* Known/reference value */
    float   measured;       /* Raw measured value */
} gf_cal_point_t;

/* Calibration data for a channel */
typedef struct {
    uint32_t            magic;          /* Validation magic number */
    uint8_t             channel;
    gf_cal_type_t       type;
    uint8_t             point_count;
    gf_cal_point_t      points[GF_CAL_MAX_POINTS];
    float               coefficients[8]; /* For polynomial */
    uint32_t            cal_date;       /* Unix timestamp */
    uint32_t            expiry_date;    /* Unix timestamp or 0 */
    uint32_t            crc32;          /* Data integrity check */
    char                operator_id[16];
    char                equipment_id[16];
} gf_cal_data_t;

/* Calibration verification result */
typedef struct {
    bool    passed;
    float   error;          /* Absolute error at test point */
    float   error_percent;  /* Relative error */
    float   reference;
    float   measured;
    float   corrected;
} gf_cal_verify_result_t;

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize calibration subsystem
 */
int gf_cal_init(void);

/**
 * @brief Load calibration data from non-volatile storage
 */
int gf_cal_load(uint8_t channel);

/**
 * @brief Save calibration data to non-volatile storage
 */
int gf_cal_save(uint8_t channel);

/**
 * @brief Check if channel has valid calibration
 */
gf_cal_status_t gf_cal_get_status(uint8_t channel);

/**
 * @brief Start calibration procedure
 */
int gf_cal_start(uint8_t channel, gf_cal_type_t type);

/**
 * @brief Add calibration point
 * @param channel Channel being calibrated
 * @param reference Known reference value
 * @param measured Current raw measurement
 */
int gf_cal_add_point(uint8_t channel, float reference, float measured);

/**
 * @brief Complete calibration and calculate coefficients
 */
int gf_cal_complete(uint8_t channel);

/**
 * @brief Cancel calibration in progress
 */
int gf_cal_cancel(uint8_t channel);

/**
 * @brief Apply calibration to a raw value
 * @param channel Channel
 * @param raw Raw measured value
 * @return Calibrated value
 */
float gf_cal_apply(uint8_t channel, float raw);

/**
 * @brief Verify calibration at a reference point
 * @param channel Channel to verify
 * @param reference Known reference value
 * @param measured Current measurement at reference
 * @param tolerance Acceptable error (percent)
 * @param result Verification result
 */
int gf_cal_verify(uint8_t channel, float reference, float measured,
                   float tolerance, gf_cal_verify_result_t *result);

/**
 * @brief Set calibration directly (from file or external source)
 */
int gf_cal_set_data(uint8_t channel, const gf_cal_data_t *data);

/**
 * @brief Get calibration data
 */
int gf_cal_get_data(uint8_t channel, gf_cal_data_t *data);

/**
 * @brief Clear calibration for a channel
 */
int gf_cal_clear(uint8_t channel);

/**
 * @brief Check if any calibrations are expired
 */
bool gf_cal_any_expired(void);

/**
 * @brief Get days until calibration expires
 */
int gf_cal_days_until_expiry(uint8_t channel);

/**
 * @brief Set operator/equipment IDs for audit trail
 */
void gf_cal_set_operator(const char *operator_id);
void gf_cal_set_equipment(const char *equipment_id);

/**
 * @brief Get calibration driver descriptor
 */
const void* gf_cal_get_driver(void);

#endif /* GF_CALIBRATION_H */
