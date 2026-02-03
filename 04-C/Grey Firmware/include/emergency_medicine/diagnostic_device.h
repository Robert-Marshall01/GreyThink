/**
 * @file diagnostic_device.h
 * @brief Portable Diagnostic Device Driver Interface
 * 
 * INDUSTRY RELEVANCE:
 * Point-of-care (POC) diagnostics enable rapid medical testing outside
 * traditional labs. The $50B+ market includes:
 * - Cardiac markers (troponin, BNP)
 * - Infectious disease (COVID, flu, malaria)
 * - Blood chemistry (glucose, electrolytes)
 * - Pregnancy/fertility testing
 * 
 * Companies like Abbott (i-STAT), Roche, and Siemens develop portable
 * analyzers. Embedded engineers need expertise in:
 * - Biosensor interface (electrochemical, optical)
 * - Sample handling and fluid control
 * - Result interpretation algorithms
 * - FDA/CE certification requirements
 * 
 * STANDARDS:
 * - IEC 62304 (Medical Device Software)
 * - ISO 13485 (Medical Device QMS)
 * - FDA 21 CFR Part 820 (Quality System Regulation)
 * - IVDR (In Vitro Diagnostic Regulation - EU)
 * 
 * @author Grey Firmware Project
 */

#ifndef GF_DIAGNOSTIC_DEVICE_H
#define GF_DIAGNOSTIC_DEVICE_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* Configuration Constants                                                    */
/*===========================================================================*/

#define DIAG_MAX_TESTS              32   /**< Maximum concurrent tests */
#define DIAG_RESULT_TIMEOUT_MS      300000  /**< 5 min max test time */

/*===========================================================================*/
/* Type Definitions                                                           */
/*===========================================================================*/

/** Test type */
typedef enum {
    TEST_CARDIAC_TROPONIN,
    TEST_CARDIAC_BNP,
    TEST_BLOOD_GLUCOSE,
    TEST_BLOOD_GAS,
    TEST_ELECTROLYTES,
    TEST_COAGULATION,
    TEST_HEMOGLOBIN,
    TEST_INFECTIOUS_DISEASE,
    TEST_PREGNANCY,
    TEST_DRUG_SCREEN
} test_type_t;

/** Test status */
typedef enum {
    TEST_STATUS_IDLE,
    TEST_STATUS_SAMPLE_INSERTED,
    TEST_STATUS_ANALYZING,
    TEST_STATUS_COMPLETE,
    TEST_STATUS_ERROR,
    TEST_STATUS_EXPIRED_CARTRIDGE
} test_status_t;

/** Result quality */
typedef enum {
    QUALITY_VALID,
    QUALITY_HEMOLYSIS_DETECTED,
    QUALITY_INSUFFICIENT_SAMPLE,
    QUALITY_INTERFERENCE_DETECTED,
    QUALITY_OUT_OF_RANGE
} result_quality_t;

/** Test result */
typedef struct {
    test_type_t type;
    test_status_t status;
    float value;
    char unit[16];
    float reference_low;
    float reference_high;
    result_quality_t quality;
    uint32_t test_duration_ms;
    uint32_t timestamp;
    char cartridge_lot[20];
    bool abnormal;
} test_result_t;

/** Device configuration */
typedef struct {
    char device_serial[20];
    char operator_id[32];
    char patient_id[32];
    bool qc_passed;
    uint32_t last_qc_time;
} diag_config_t;

/*===========================================================================*/
/* Function Prototypes                                                        */
/*===========================================================================*/

int diagnostic_device_init(void);
int diagnostic_config(const diag_config_t *config);
int diagnostic_insert_cartridge(void);
int diagnostic_start_test(test_type_t type);
int diagnostic_get_status(test_status_t *status);
int diagnostic_get_result(test_result_t *result);
int diagnostic_run_qc(bool *passed);
int diagnostic_eject_cartridge(void);
void diagnostic_device_shutdown(void);

#ifdef __cplusplus
}
#endif

#endif /* GF_DIAGNOSTIC_DEVICE_H */
