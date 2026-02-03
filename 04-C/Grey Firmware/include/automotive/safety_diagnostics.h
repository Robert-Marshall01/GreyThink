/**
 * @file safety_diagnostics.h
 * @brief Automotive Safety Diagnostics Module (ISO 26262)
 *
 * INDUSTRY RELEVANCE:
 * Safety-critical automotive systems require continuous diagnostics:
 * - Runtime integrity checks per ISO 26262 requirements
 * - Memory (RAM/ROM) verification
 * - CPU core tests and watchdog monitoring
 * - Communication link monitoring (CAN, LIN, FlexRay)
 * - Sensor plausibility checks
 * - ASIL decomposition and safety goal tracking
 *
 * Used in: ECUs, powertrain controllers, chassis systems
 *
 * @note This is a stub demonstrating functional safety patterns.
 */

#ifndef GF_SAFETY_DIAGNOSTICS_H
#define GF_SAFETY_DIAGNOSTICS_H

#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/*===========================================================================*/
/* ASIL Level Definitions                                                    */
/*===========================================================================*/

/**
 * @brief Automotive Safety Integrity Levels
 */
typedef enum {
    GF_ASIL_QM,                 /**< Quality Management (non-safety) */
    GF_ASIL_A,                  /**< Lowest safety level */
    GF_ASIL_B,                  /**< Low-medium safety */
    GF_ASIL_C,                  /**< Medium-high safety */
    GF_ASIL_D                   /**< Highest safety level */
} gf_asil_level_t;

/**
 * @brief Diagnostic test types
 */
typedef enum {
    /* Memory Tests */
    GF_DIAG_ROM_CRC,            /**< ROM checksum verification */
    GF_DIAG_RAM_MARCH,          /**< RAM march test */
    GF_DIAG_RAM_PATTERN,        /**< RAM pattern test */
    GF_DIAG_STACK_MONITOR,      /**< Stack overflow detection */
    
    /* CPU Tests */
    GF_DIAG_CPU_REGISTER,       /**< CPU register test */
    GF_DIAG_CPU_ALU,            /**< ALU operation test */
    GF_DIAG_CPU_FLOW,           /**< Program flow monitoring */
    GF_DIAG_WATCHDOG,           /**< Watchdog test */
    
    /* Clock Tests */
    GF_DIAG_CLOCK_MONITOR,      /**< Clock frequency check */
    GF_DIAG_CLOCK_CROSS,        /**< Cross-clock check */
    
    /* I/O Tests */
    GF_DIAG_ADC_REFERENCE,      /**< ADC reference check */
    GF_DIAG_ADC_CONVERSION,     /**< ADC conversion test */
    GF_DIAG_GPIO_LOOPBACK,      /**< GPIO loopback test */
    
    /* Communication Tests */
    GF_DIAG_CAN_CONTROLLER,     /**< CAN controller test */
    GF_DIAG_CAN_BUS,            /**< CAN bus connectivity */
    GF_DIAG_SPI_LOOPBACK,       /**< SPI loopback */
    
    /* Sensor Tests */
    GF_DIAG_SENSOR_RANGE,       /**< Sensor value range */
    GF_DIAG_SENSOR_PLAUSIBLE,   /**< Sensor plausibility */
    GF_DIAG_SENSOR_STUCK,       /**< Sensor stuck detection */
    GF_DIAG_SENSOR_GRADIENT,    /**< Rate of change check */
    
    /* Actuator Tests */
    GF_DIAG_ACTUATOR_FEEDBACK,  /**< Actuator feedback loop */
    GF_DIAG_ACTUATOR_CURRENT,   /**< Actuator current check */
    
    GF_DIAG_TEST_COUNT
} gf_diag_test_t;

/**
 * @brief Diagnostic test result
 */
typedef enum {
    GF_DIAG_PASS,               /**< Test passed */
    GF_DIAG_FAIL,               /**< Test failed */
    GF_DIAG_DEGRADED,           /**< Partial pass */
    GF_DIAG_NOT_RUN,            /**< Test not executed */
    GF_DIAG_BLOCKED             /**< Test blocked by fault */
} gf_diag_result_t;

/**
 * @brief Safety goal definition
 */
typedef struct {
    uint16_t goal_id;           /**< Safety goal identifier */
    const char* description;    /**< Goal description */
    gf_asil_level_t asil;       /**< Required ASIL level */
    uint16_t ftti_ms;           /**< Fault Tolerant Time Interval */
    bool (*check_function)(void);  /**< Verification function */
} gf_safety_goal_t;

/**
 * @brief Diagnostic test configuration
 */
typedef struct {
    gf_diag_test_t test;
    gf_asil_level_t required_asil;
    uint32_t interval_ms;       /**< Test execution interval */
    uint8_t max_failures;       /**< Failures before fault */
    bool run_at_startup;        /**< Run in startup sequence */
    bool run_periodic;          /**< Run periodically */
    void* context;              /**< Test-specific context */
} gf_diag_config_t;

/**
 * @brief Diagnostic status for single test
 */
typedef struct {
    gf_diag_test_t test;
    gf_diag_result_t result;
    uint32_t last_run_time;
    uint32_t run_count;
    uint32_t fail_count;
    uint32_t execution_time_us;
    uint8_t data[16];           /**< Test-specific data */
} gf_diag_status_t;

/**
 * @brief Overall system diagnostic summary
 */
typedef struct {
    bool system_ok;
    gf_asil_level_t max_achievable_asil;
    uint16_t tests_passed;
    uint16_t tests_failed;
    uint16_t tests_not_run;
    uint16_t active_faults;
    uint32_t total_faults_logged;
    uint32_t uptime_ms;
    uint8_t safety_goals_met;
    uint8_t safety_goals_total;
} gf_diag_summary_t;

/**
 * @brief Safety diagnostics handle
 */
typedef struct gf_safety_diag* gf_safety_diag_t;

/*===========================================================================*/
/* Safety Diagnostics API                                                    */
/*===========================================================================*/

/**
 * @brief Initialize safety diagnostics
 * @param asil Target ASIL level
 * @param handle Output handle
 * @return 0 on success
 */
int gf_safety_diag_init(gf_asil_level_t asil, gf_safety_diag_t* handle);

/**
 * @brief Register a diagnostic test
 * @param handle Diagnostics handle
 * @param config Test configuration
 * @return 0 on success
 */
int gf_safety_diag_register(gf_safety_diag_t handle,
                            const gf_diag_config_t* config);

/**
 * @brief Register a safety goal
 * @param handle Diagnostics handle
 * @param goal Safety goal definition
 * @return 0 on success
 */
int gf_safety_diag_add_goal(gf_safety_diag_t handle,
                            const gf_safety_goal_t* goal);

/**
 * @brief Run startup diagnostic sequence
 * @param handle Diagnostics handle
 * @return 0 if all startup tests pass
 */
int gf_safety_diag_startup(gf_safety_diag_t handle);

/**
 * @brief Run periodic diagnostic tests (call from scheduler)
 * @param handle Diagnostics handle
 * @param current_time_ms Current system time
 * @return Number of tests executed
 */
int gf_safety_diag_periodic(gf_safety_diag_t handle,
                            uint32_t current_time_ms);

/**
 * @brief Run specific diagnostic test
 * @param handle Diagnostics handle
 * @param test Test to run
 * @return Test result
 */
gf_diag_result_t gf_safety_diag_run_test(gf_safety_diag_t handle,
                                          gf_diag_test_t test);

/**
 * @brief Get status of specific test
 * @param handle Diagnostics handle
 * @param test Test identifier
 * @param status Output status
 * @return 0 on success
 */
int gf_safety_diag_get_status(gf_safety_diag_t handle,
                               gf_diag_test_t test,
                               gf_diag_status_t* status);

/**
 * @brief Get overall diagnostic summary
 * @param handle Diagnostics handle
 * @param summary Output summary
 * @return 0 on success
 */
int gf_safety_diag_get_summary(gf_safety_diag_t handle,
                                gf_diag_summary_t* summary);

/**
 * @brief Check if system meets ASIL level
 * @param handle Diagnostics handle
 * @param asil Required ASIL level
 * @return true if system meets level
 */
bool gf_safety_diag_check_asil(gf_safety_diag_t handle,
                                gf_asil_level_t asil);

/**
 * @brief Report diagnostic failure
 * @param handle Diagnostics handle
 * @param test Failed test
 * @param data Failure data
 * @return 0 on success
 */
int gf_safety_diag_report_failure(gf_safety_diag_t handle,
                                   gf_diag_test_t test,
                                   const uint8_t* data);

/**
 * @brief Deinitialize diagnostics
 * @param handle Diagnostics handle
 */
void gf_safety_diag_deinit(gf_safety_diag_t handle);

/*===========================================================================*/
/* Built-in Diagnostic Tests                                                 */
/*===========================================================================*/

/**
 * @brief ROM CRC verification
 * @param start_addr ROM start address
 * @param length ROM length
 * @param expected_crc Expected CRC32
 * @return true if CRC matches
 */
bool gf_diag_rom_crc(const void* start_addr, uint32_t length,
                     uint32_t expected_crc);

/**
 * @brief RAM march test (non-destructive)
 * @param start_addr RAM start address
 * @param length RAM length to test
 * @return true if test passes
 */
bool gf_diag_ram_march(void* start_addr, uint32_t length);

/**
 * @brief CPU register test
 * @return true if all registers functional
 */
bool gf_diag_cpu_register(void);

/**
 * @brief Stack overflow check
 * @param stack_bottom Stack bottom address
 * @param pattern Guard pattern
 * @return true if stack intact
 */
bool gf_diag_stack_check(const void* stack_bottom, uint32_t pattern);

#ifdef __cplusplus
}
#endif

#endif /* GF_SAFETY_DIAGNOSTICS_H */
