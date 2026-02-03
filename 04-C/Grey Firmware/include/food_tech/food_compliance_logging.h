/**
 * @file food_compliance_logging.h
 * @brief Food Safety Compliance Logging Module
 * 
 * INDUSTRY RELEVANCE:
 * Commercial food operations require comprehensive logging for HACCP, FDA,
 * and local health department audits. This module provides immutable logging
 * with cryptographic integrity for temperature records, cleaning schedules,
 * and critical control point documentation.
 * 
 * TECHNICAL SCOPE:
 * - Temperature logging at configurable intervals
 * - Critical Control Point (CCP) documentation
 * - Corrective action recording
 * - Digital signature for log integrity
 * - Automated report generation
 * - Cloud backup with retention policies
 * 
 * LOG CATEGORIES:
 * - Temperature monitoring (refrigeration, cooking, holding)
 * - Time control (rapid cooling, hot holding limits)
 * - Sanitation verification
 * - Receiving logs (supplier, temperature, inspection)
 * - Employee health checks
 * 
 * STANDARDS COMPLIANCE:
 * - HACCP (Hazard Analysis Critical Control Points)
 * - FDA Food Code 2022
 * - FSMA (Food Safety Modernization Act)
 * - ISO 22000 (Food safety management)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_FOOD_COMPLIANCE_LOGGING_H
#define GF_FOOD_COMPLIANCE_LOGGING_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Configuration Constants
 * ============================================================================ */

#define FOOD_LOG_MAX_ENTRIES    10000  /**< Maximum log entries */
#define FOOD_LOG_MAX_CCPS       16     /**< Maximum CCPs monitored */
#define FOOD_LOG_RETENTION_DAYS 365    /**< Log retention period */

/* ============================================================================
 * Type Definitions
 * ============================================================================ */

/** Log entry type */
typedef enum {
    FOOD_LOG_TEMPERATURE,
    FOOD_LOG_TIME_CONTROL,
    FOOD_LOG_SANITATION,
    FOOD_LOG_RECEIVING,
    FOOD_LOG_CCP_CHECK,
    FOOD_LOG_CORRECTIVE_ACTION,
    FOOD_LOG_EMPLOYEE_HEALTH,
    FOOD_LOG_EQUIPMENT_CAL
} food_log_type_t;

/** Temperature zone type */
typedef enum {
    TEMP_ZONE_WALK_IN_COOLER,
    TEMP_ZONE_WALK_IN_FREEZER,
    TEMP_ZONE_REACH_IN_COOLER,
    TEMP_ZONE_PREP_TABLE,
    TEMP_ZONE_HOT_HOLDING,
    TEMP_ZONE_COOKING,
    TEMP_ZONE_RECEIVING
} temp_zone_t;

/** Compliance status */
typedef enum {
    COMPLIANCE_OK,
    COMPLIANCE_WARNING,           /**< Approaching limit */
    COMPLIANCE_VIOLATION,         /**< Limit exceeded */
    COMPLIANCE_CRITICAL           /**< Immediate action required */
} compliance_status_t;

/** Temperature log entry */
typedef struct {
    temp_zone_t zone;
    float temperature_c;
    float limit_low_c;
    float limit_high_c;
    compliance_status_t status;
    uint64_t timestamp;
    uint32_t sensor_id;
    char operator_id[16];
} temp_log_entry_t;

/** CCP verification entry */
typedef struct {
    uint8_t ccp_id;
    char ccp_name[32];
    bool verified;
    char verification_method[64];
    compliance_status_t status;
    uint64_t timestamp;
    char operator_id[16];
    uint8_t signature[32];        /**< SHA-256 of entry */
} ccp_log_entry_t;

/* ============================================================================
 * Function Prototypes (Stub)
 * ============================================================================ */

/** Initialize compliance logging */
int food_log_init(void);

/** Log temperature reading */
int food_log_temperature(temp_zone_t zone, float temp_c, const char *operator_id);

/** Log CCP verification */
int food_log_ccp_check(uint8_t ccp_id, bool verified, const char *method, const char *operator_id);

/** Log corrective action */
int food_log_corrective_action(const char *description, const char *operator_id);

/** Generate compliance report */
int food_log_generate_report(uint8_t *buffer, size_t max_len, uint64_t start_time, uint64_t end_time);

/** Verify log integrity */
bool food_log_verify_integrity(void);

/** Export logs for audit */
int food_log_export(const char *filename);

#ifdef __cplusplus
}
#endif

#endif /* GF_FOOD_COMPLIANCE_LOGGING_H */
