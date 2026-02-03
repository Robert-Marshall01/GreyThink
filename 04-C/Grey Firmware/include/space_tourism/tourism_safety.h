/**
 * @file tourism_safety.h
 * @brief Space Tourism Safety Compliance Module
 * 
 * INDUSTRY RELEVANCE:
 * Commercial space operators must comply with FAA/AST regulations for
 * passenger safety. This module provides:
 * - Pre-flight safety checklist verification
 * - Real-time safety parameter monitoring
 * - Abort decision support
 * - Regulatory compliance logging
 * - Incident recording for post-flight analysis
 * 
 * Essential for obtaining and maintaining commercial spaceflight licenses.
 * 
 * STANDARDS: 14 CFR Part 460, FAA-AST Safety Guidelines
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_TOURISM_SAFETY_H
#define GF_TOURISM_SAFETY_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/* Safety checklist items */
typedef enum {
    SAFETY_CHECK_CABIN_PRESSURE,
    SAFETY_CHECK_O2_SUPPLY,
    SAFETY_CHECK_FIRE_SUPPRESSION,
    SAFETY_CHECK_EMERGENCY_EGRESS,
    SAFETY_CHECK_COMM_SYSTEMS,
    SAFETY_CHECK_RESTRAINTS,
    SAFETY_CHECK_ABORT_CAPABILITY,
    SAFETY_CHECK_COUNT
} safety_check_t;

/* Abort modes */
typedef enum {
    ABORT_NONE,
    ABORT_PAD,
    ABORT_ASCENT,
    ABORT_ORBIT,
    ABORT_REENTRY
} abort_mode_t;

/* Safety event severity */
typedef enum {
    SAFETY_SEV_INFO,
    SAFETY_SEV_CAUTION,
    SAFETY_SEV_WARNING,
    SAFETY_SEV_CRITICAL
} safety_severity_t;

/**
 * @brief Initialize safety compliance module
 * @return 0 on success
 */
int tourism_safety_init(void);

/**
 * @brief Execute pre-flight safety checklist
 * @param results Array to store check results (true = pass)
 * @return Number of failed checks, 0 = all pass
 */
int tourism_safety_preflight(bool results[SAFETY_CHECK_COUNT]);

/**
 * @brief Check if abort is recommended
 * @param mode Output recommended abort mode
 * @return true if abort is recommended
 */
bool tourism_safety_check_abort(abort_mode_t *mode);

/**
 * @brief Log safety event for compliance
 * @param severity Event severity
 * @param event_code Unique event identifier
 * @param description Brief description
 * @return 0 on success
 */
int tourism_safety_log_event(safety_severity_t severity, 
                             uint16_t event_code,
                             const char *description);

/**
 * @brief Get compliance report for regulatory filing
 * @param buffer Output buffer for report data
 * @param max_len Buffer size
 * @return Bytes written
 */
int tourism_safety_get_report(uint8_t *buffer, size_t max_len);

/**
 * @brief Check overall go/no-go status
 * @return true if safe to proceed
 */
bool tourism_safety_is_go(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_TOURISM_SAFETY_H */
