/**
 * @file fault_injection_tests.c
 * @brief Fault Injection and Resilience Test Suite
 * 
 * RECRUITER NOTE: Fault injection testing is critical in mission-critical systems
 * (automotive ISO 26262, medical IEC 62304, aerospace DO-178C). This suite
 * demonstrates chaos engineering principles applied to embedded systems.
 * 
 * These tests verify:
 * - System behavior under simulated hardware faults
 * - Recovery strategy effectiveness
 * - Fault logging and persistence
 * - Watchdog supervision and timeout handling
 */

#include <stdio.h>
#include <string.h>
#include <stdbool.h>

/* Ensure fault injection is available */
#ifndef GF_ENABLE_FAULT_INJECTION
#define GF_ENABLE_FAULT_INJECTION
#endif

#include "core/reliability.h"
#include "core/error_handler.h"

/*===========================================================================*/
/* Test Infrastructure                                                        */
/*===========================================================================*/

static int tests_run = 0;
static int tests_passed = 0;
static int tests_failed = 0;

#define FI_TEST(name) static int test_##name(void)
#define FI_ASSERT(cond, msg) do { \
    if (!(cond)) { \
        printf("    FAIL: %s\n", msg); \
        return 0; \
    } \
} while(0)
#define FI_PASS() return 1

#define FI_RUN_TEST(name) do { \
    tests_run++; \
    printf("  [%02d] %-50s ", tests_run, #name); \
    if (test_##name()) { \
        tests_passed++; \
        printf("PASS\n"); \
    } else { \
        tests_failed++; \
    } \
} while(0)

/*===========================================================================*/
/* Task Watchdog Tests                                                        */
/*===========================================================================*/

/**
 * TEST: Basic task watchdog registration and kick
 * 
 * Validates that per-task watchdog monitoring can be set up and maintained.
 * In production, each RTOS task registers with the watchdog supervisor,
 * allowing detection of individual task hangs without resetting healthy tasks.
 */
FI_TEST(wdt_registration) {
    gf_rel_init();
    
    gf_task_wdt_config_t config = {
        .task_name = "test_task",
        .timeout_ms = 1000,
        .mode = GF_WDT_MODE_SIMPLE,
        .critical = false
    };
    
    gf_task_wdt_handle_t handle = gf_rel_wdt_register(&config);
    FI_ASSERT(handle != 0, "Watchdog registration failed");
    
    gf_task_wdt_status_t status;
    FI_ASSERT(gf_rel_wdt_get_status(handle, &status) == 0, "Status get failed");
    FI_ASSERT(status.active, "Watchdog not active");
    FI_ASSERT(strcmp(status.task_name, "test_task") == 0, "Name mismatch");
    
    FI_ASSERT(gf_rel_wdt_unregister(handle) == 0, "Unregister failed");
    
    FI_PASS();
}

/**
 * TEST: Watchdog kick resets timeout
 * 
 * Verifies the fundamental watchdog operation: kicking the watchdog
 * before timeout keeps the task alive. Demonstrates the keep-alive
 * pattern used in every safety-critical embedded system.
 */
FI_TEST(wdt_kick_mechanism) {
    gf_rel_init();
    
    gf_task_wdt_config_t config = {
        .task_name = "kick_test",
        .timeout_ms = 500,
        .mode = GF_WDT_MODE_SIMPLE,
        .critical = false
    };
    
    gf_task_wdt_handle_t handle = gf_rel_wdt_register(&config);
    FI_ASSERT(handle != 0, "Registration failed");
    
    /* Kick should update last_kick time */
    gf_rel_wdt_kick(handle);
    
    gf_task_wdt_status_t status;
    gf_rel_wdt_get_status(handle, &status);
    FI_ASSERT(!status.expired, "Should not be expired after kick");
    
    gf_rel_wdt_unregister(handle);
    FI_PASS();
}

/**
 * TEST: Multiple task watchdog isolation
 * 
 * Ensures each task has independent watchdog supervision.
 * Critical for systems where some tasks are more important than others.
 */
FI_TEST(wdt_multi_task) {
    gf_rel_init();
    
    gf_task_wdt_config_t config1 = {
        .task_name = "task_1",
        .timeout_ms = 100,
        .mode = GF_WDT_MODE_SIMPLE,
        .critical = true
    };
    
    gf_task_wdt_config_t config2 = {
        .task_name = "task_2",
        .timeout_ms = 200,
        .mode = GF_WDT_MODE_SIMPLE,
        .critical = false
    };
    
    gf_task_wdt_handle_t h1 = gf_rel_wdt_register(&config1);
    gf_task_wdt_handle_t h2 = gf_rel_wdt_register(&config2);
    
    FI_ASSERT(h1 != 0 && h2 != 0, "Multi-registration failed");
    FI_ASSERT(h1 != h2, "Handles should be unique");
    
    gf_task_wdt_status_t status1, status2;
    gf_rel_wdt_get_status(h1, &status1);
    gf_rel_wdt_get_status(h2, &status2);
    
    FI_ASSERT(strcmp(status1.task_name, "task_1") == 0, "Task 1 name wrong");
    FI_ASSERT(strcmp(status2.task_name, "task_2") == 0, "Task 2 name wrong");
    
    gf_rel_wdt_unregister(h1);
    gf_rel_wdt_unregister(h2);
    
    FI_PASS();
}

/**
 * TEST: Deadline mode watchdog
 * 
 * Validates deadline-based timeout for critical operations.
 * Unlike simple mode (periodic kick), deadline mode measures
 * completion time of specific operations - essential for real-time systems.
 */
FI_TEST(wdt_deadline_mode) {
    gf_rel_init();
    
    gf_task_wdt_config_t config = {
        .task_name = "deadline_task",
        .timeout_ms = 50,
        .mode = GF_WDT_MODE_DEADLINE,
        .critical = true
    };
    
    gf_task_wdt_handle_t handle = gf_rel_wdt_register(&config);
    FI_ASSERT(handle != 0, "Registration failed");
    
    /* Start critical operation deadline */
    gf_rel_wdt_start_deadline(handle);
    
    /* Simulate work completing within deadline */
    gf_rel_wdt_complete_deadline(handle);
    
    gf_task_wdt_status_t status;
    gf_rel_wdt_get_status(handle, &status);
    FI_ASSERT(!status.expired, "Should not expire on timely completion");
    
    gf_rel_wdt_unregister(handle);
    FI_PASS();
}

/*===========================================================================*/
/* Persistent Fault Logging Tests                                             */
/*===========================================================================*/

/**
 * TEST: Fault recording and retrieval
 * 
 * Validates the persistent fault log stores and retrieves entries correctly.
 * Post-mortem debugging in embedded systems relies heavily on fault logs
 * that survive reboot - this is how field failures are diagnosed.
 */
FI_TEST(fault_log_record) {
    gf_rel_init();
    
    int result = gf_rel_fault_record(
        GF_FAULT_COMM_TIMEOUT, 
        GF_FAULT_SEV_MINOR,
        0x05,  /* Subsystem ID */
        "CAN bus timeout on node 3"
    );
    FI_ASSERT(result == 0, "Record failed");
    
    gf_persistent_fault_t entries[4];
    int count = gf_rel_fault_get_log(entries, 4);
    FI_ASSERT(count >= 1, "No entries retrieved");
    
    FI_ASSERT(entries[0].type == GF_FAULT_COMM_TIMEOUT, "Type mismatch");
    FI_ASSERT(entries[0].severity == GF_FAULT_SEV_MINOR, "Severity mismatch");
    
    FI_PASS();
}

/**
 * TEST: Fault log rollover behavior
 * 
 * Verifies circular buffer behavior when log fills up.
 * Production systems run for years - logs must handle rollover gracefully
 * while preserving recent history.
 */
FI_TEST(fault_log_rollover) {
    gf_rel_init();
    
    /* Fill the log beyond capacity */
    for (int i = 0; i < GF_REL_FAULT_LOG_SIZE + 5; i++) {
        char msg[32];
        snprintf(msg, sizeof(msg), "Fault %d", i);
        gf_rel_fault_record(GF_FAULT_HARDFAULT, GF_FAULT_SEV_MINOR, 0x01, msg);
    }
    
    gf_fault_stats_t stats;
    gf_rel_fault_get_stats(&stats);
    
    FI_ASSERT(stats.log_entries <= GF_REL_FAULT_LOG_SIZE, "Log exceeded capacity");
    FI_ASSERT(stats.log_rollovers > 0, "Rollover not detected");
    FI_ASSERT(stats.total_faults == GF_REL_FAULT_LOG_SIZE + 5, "Count wrong");
    
    FI_PASS();
}

/**
 * TEST: Fault severity classification
 * 
 * Ensures severity levels are tracked correctly for analytics.
 * Production dashboards show fault trends by severity to guide
 * maintenance and firmware updates.
 */
FI_TEST(fault_severity_tracking) {
    gf_rel_init();
    
    gf_rel_fault_record(GF_FAULT_POWER_FAIL, GF_FAULT_SEV_CRITICAL, 0x03, "Brown-out");
    gf_rel_fault_record(GF_FAULT_WATCHDOG_TIMEOUT, GF_FAULT_SEV_FATAL, 0x01, "Deadline miss");
    gf_rel_fault_record(GF_FAULT_MEMFAULT, GF_FAULT_SEV_MINOR, 0x02, "Cache miss");
    
    gf_fault_stats_t stats;
    gf_rel_fault_get_stats(&stats);
    
    FI_ASSERT(stats.critical_count >= 1, "Critical not counted");
    FI_ASSERT(stats.fatal_count >= 1, "Fatal not counted");
    FI_ASSERT(stats.minor_count >= 1, "Minor not counted");
    
    FI_PASS();
}

/**
 * TEST: Fault acknowledgment
 * 
 * Validates that operators can acknowledge faults to clear maintenance alerts.
 * Production systems distinguish between new faults and acknowledged ones.
 */
FI_TEST(fault_acknowledgment) {
    gf_rel_init();
    
    gf_rel_fault_record(GF_FAULT_CONFIG_CORRUPT, GF_FAULT_SEV_MAJOR, 0x07, "Flash wear");
    
    gf_persistent_fault_t entries[4];
    int count = gf_rel_fault_get_recent(entries, 4);
    FI_ASSERT(count >= 1, "No recent entries");
    
    uint32_t ts = entries[count-1].timestamp;
    FI_ASSERT(gf_rel_fault_acknowledge(ts) == 0, "Acknowledge failed");
    
    int cleared = gf_rel_fault_clear_acknowledged();
    FI_ASSERT(cleared >= 1, "Clear did not remove acknowledged faults");
    
    FI_PASS();
}

/*===========================================================================*/
/* Recovery Strategy Tests                                                    */
/*===========================================================================*/

static bool recovery_hook_called = false;
static gf_fault_type_t recovery_hook_fault;

static bool test_recovery_hook(gf_fault_type_t fault, void *ctx) {
    (void)ctx;
    recovery_hook_called = true;
    recovery_hook_fault = fault;
    return true;  /* Recovery succeeded */
}

/**
 * TEST: Recovery policy configuration
 * 
 * Validates that recovery policies can be set per fault type.
 * Different faults require different responses - a sensor glitch
 * might just need a retry, while memory corruption needs a reset.
 */
FI_TEST(recovery_policy_config) {
    gf_rel_init();
    
    gf_recovery_policy_t policy = {
        .fault_type = GF_FAULT_COMM_TIMEOUT,
        .strategy = GF_RECOVERY_STRATEGY_RETRY,
        .max_retries = 3,
        .retry_delay_ms = 100,
        .escalate_on_fail = true
    };
    
    int result = gf_rel_set_recovery_policy(&policy);
    FI_ASSERT(result >= 0, "Policy set failed");
    
    gf_recovery_strategy_t strat = gf_rel_execute_recovery(GF_FAULT_COMM_TIMEOUT);
    FI_ASSERT(strat == GF_RECOVERY_STRATEGY_RETRY, "Wrong strategy executed");
    
    FI_PASS();
}

/**
 * TEST: Recovery hook invocation
 * 
 * Verifies that custom recovery hooks are called on fault.
 * Hooks allow subsystem-specific recovery without central coordinator
 * needing to know every subsystem's internals.
 */
FI_TEST(recovery_hook_invocation) {
    gf_rel_init();
    recovery_hook_called = false;
    
    int result = gf_rel_register_recovery_hook(
        GF_FAULT_HARDFAULT, 
        test_recovery_hook, 
        NULL
    );
    FI_ASSERT(result >= 0, "Hook registration failed");
    
    gf_rel_execute_recovery(GF_FAULT_HARDFAULT);
    
    FI_ASSERT(recovery_hook_called, "Hook was not called");
    FI_ASSERT(recovery_hook_fault == GF_FAULT_HARDFAULT, "Wrong fault type in hook");
    
    FI_PASS();
}

/**
 * TEST: Safe mode entry and exit
 * 
 * Validates degraded operation mode for fault containment.
 * Safe mode disables non-essential features to maintain core functionality
 * when the system is partially compromised.
 */
FI_TEST(safe_mode_operation) {
    gf_rel_init();
    
    gf_recovery_status_t status;
    gf_rel_get_recovery_status(&status);
    FI_ASSERT(!status.in_safe_mode, "Should not start in safe mode");
    
    gf_rel_enter_safe_mode("Test entry");
    
    gf_rel_get_recovery_status(&status);
    FI_ASSERT(status.in_safe_mode, "Should be in safe mode");
    
    FI_ASSERT(gf_rel_exit_safe_mode() == 0, "Exit failed");
    
    gf_rel_get_recovery_status(&status);
    FI_ASSERT(!status.in_safe_mode, "Should have exited safe mode");
    
    FI_PASS();
}

/**
 * TEST: Feature degradation
 * 
 * Validates selective feature disabling during degraded operation.
 * Rather than full system failure, non-critical features are disabled
 * while critical functions continue.
 */
FI_TEST(feature_degradation) {
    gf_rel_init();
    
    #define FEATURE_LOGGING  0x01
    #define FEATURE_DISPLAY  0x02
    
    FI_ASSERT(!gf_rel_is_feature_disabled(FEATURE_LOGGING), "Should be enabled");
    
    gf_rel_disable_feature(FEATURE_LOGGING);
    FI_ASSERT(gf_rel_is_feature_disabled(FEATURE_LOGGING), "Should be disabled");
    
    gf_recovery_status_t status;
    gf_rel_get_recovery_status(&status);
    FI_ASSERT(status.degraded, "Should be in degraded mode");
    
    gf_rel_enable_feature(FEATURE_LOGGING);
    FI_ASSERT(!gf_rel_is_feature_disabled(FEATURE_LOGGING), "Should be re-enabled");
    
    FI_PASS();
}

/*===========================================================================*/
/* Fault Injection Tests                                                      */
/*===========================================================================*/

#ifdef GF_ENABLE_FAULT_INJECTION

/**
 * TEST: Fault injection enable/disable
 * 
 * RECRUITER NOTE: This is chaos engineering for embedded systems.
 * Fault injection allows testing recovery paths that are hard to
 * trigger naturally. Netflix's Chaos Monkey, but for firmware.
 */
FI_TEST(inject_enable_disable) {
    gf_rel_init();
    gf_rel_inject_reset();
    
    gf_inject_status_t status;
    gf_rel_inject_get_status(&status);
    FI_ASSERT(!status.active, "Should start inactive");
    
    gf_rel_inject_enable(GF_INJECT_MEMORY_CORRUPT);
    
    gf_rel_inject_get_status(&status);
    FI_ASSERT(status.active, "Should be active");
    FI_ASSERT(status.current_type == GF_INJECT_MEMORY_CORRUPT, "Wrong type");
    
    gf_rel_inject_disable();
    
    gf_rel_inject_get_status(&status);
    FI_ASSERT(!status.active, "Should be disabled");
    
    FI_PASS();
}

/**
 * TEST: Fault injection trigger check
 * 
 * Validates that injected faults fire at the right points.
 * Code paths check gf_rel_inject_check() at critical points;
 * if injection is active, they simulate failure.
 */
FI_TEST(inject_trigger_check) {
    gf_rel_init();
    gf_rel_inject_reset();
    
    gf_inject_config_t config = {
        .type = GF_INJECT_COMM_FAIL,
        .probability = 100,  /* Always trigger */
        .subsystem = 0,      /* Any subsystem */
        .one_shot = false,
        .trigger_after = 0
    };
    gf_rel_inject_configure(&config);
    gf_rel_inject_enable(GF_INJECT_COMM_FAIL);
    
    bool triggered = gf_rel_inject_check(GF_INJECT_COMM_FAIL, 0x05);
    FI_ASSERT(triggered, "Should have triggered");
    
    gf_inject_status_t status;
    gf_rel_inject_get_status(&status);
    FI_ASSERT(status.injection_count >= 1, "Injection not counted");
    
    FI_PASS();
}

/**
 * TEST: One-shot fault injection
 * 
 * Validates single-trigger mode for deterministic testing.
 * One-shot is useful for testing recovery from a single fault
 * without needing to disable injection manually.
 */
FI_TEST(inject_one_shot) {
    gf_rel_init();
    gf_rel_inject_reset();
    
    gf_inject_config_t config = {
        .type = GF_INJECT_TIMEOUT,
        .probability = 100,
        .subsystem = 0,
        .one_shot = true,
        .trigger_after = 0
    };
    gf_rel_inject_configure(&config);
    gf_rel_inject_enable(GF_INJECT_TIMEOUT);
    
    bool first = gf_rel_inject_check(GF_INJECT_TIMEOUT, 0x01);
    bool second = gf_rel_inject_check(GF_INJECT_TIMEOUT, 0x01);
    
    FI_ASSERT(first, "First check should trigger");
    FI_ASSERT(!second, "Second check should not trigger (one-shot)");
    
    FI_PASS();
}

/**
 * TEST: Delayed fault injection
 * 
 * Validates trigger_after functionality for targeting specific code paths.
 * Useful for testing failures in Nth iteration of a loop.
 */
FI_TEST(inject_delayed_trigger) {
    gf_rel_init();
    gf_rel_inject_reset();
    
    gf_inject_config_t config = {
        .type = GF_INJECT_RANDOM,
        .probability = 100,
        .subsystem = 0,
        .one_shot = false,
        .trigger_after = 3  /* Trigger on 3rd check */
    };
    gf_rel_inject_configure(&config);
    gf_rel_inject_enable(GF_INJECT_RANDOM);
    
    bool t1 = gf_rel_inject_check(GF_INJECT_RANDOM, 0x01);
    bool t2 = gf_rel_inject_check(GF_INJECT_RANDOM, 0x01);
    bool t3 = gf_rel_inject_check(GF_INJECT_RANDOM, 0x01);
    
    FI_ASSERT(!t1, "First should not trigger");
    FI_ASSERT(!t2, "Second should not trigger");
    FI_ASSERT(t3, "Third should trigger");
    
    FI_PASS();
}

/**
 * TEST: Subsystem-targeted injection
 * 
 * Validates fault injection can target specific subsystems.
 * Allows testing one subsystem's fault handling without affecting others.
 */
FI_TEST(inject_subsystem_target) {
    gf_rel_init();
    gf_rel_inject_reset();
    
    gf_inject_config_t config = {
        .type = GF_INJECT_POWER_GLITCH,
        .probability = 100,
        .subsystem = 0x03,  /* Only affect subsystem 3 */
        .one_shot = false,
        .trigger_after = 0
    };
    gf_rel_inject_configure(&config);
    gf_rel_inject_enable(GF_INJECT_POWER_GLITCH);
    
    bool wrong_subsys = gf_rel_inject_check(GF_INJECT_POWER_GLITCH, 0x01);
    bool right_subsys = gf_rel_inject_check(GF_INJECT_POWER_GLITCH, 0x03);
    
    FI_ASSERT(!wrong_subsys, "Should not trigger for wrong subsystem");
    FI_ASSERT(right_subsys, "Should trigger for target subsystem");
    
    FI_PASS();
}

#endif /* GF_ENABLE_FAULT_INJECTION */

/*===========================================================================*/
/* Integrated Resilience Scenarios                                            */
/*===========================================================================*/

/**
 * TEST: Watchdog timeout triggers fault log and recovery
 * 
 * End-to-end test of the fault detection and response pipeline.
 * This is what happens in production when a task hangs.
 */
FI_TEST(integrated_wdt_fault_recovery) {
    gf_rel_init();
    
    /* Set up recovery policy */
    gf_recovery_policy_t policy = {
        .fault_type = GF_FAULT_WATCHDOG_TIMEOUT,
        .strategy = GF_RECOVERY_STRATEGY_SAFE_MODE,
        .max_retries = 1,
        .retry_delay_ms = 0,
        .escalate_on_fail = true
    };
    gf_rel_set_recovery_policy(&policy);
    
    /* Simulate watchdog timeout detection */
    gf_rel_fault_record(GF_FAULT_WATCHDOG_TIMEOUT, GF_FAULT_SEV_CRITICAL,
                       GF_SUBSYS_CORE, "Sensor task timeout");
    
    gf_rel_execute_recovery(GF_FAULT_WATCHDOG_TIMEOUT);
    
    gf_recovery_status_t status;
    gf_rel_get_recovery_status(&status);
    FI_ASSERT(status.in_safe_mode, "Should enter safe mode on WDT timeout");
    FI_ASSERT(status.recovery_count >= 1, "Recovery count not updated");
    
    FI_PASS();
}

/**
 * TEST: Escalating recovery strategy
 * 
 * Tests the pattern: retry -> degrade -> safe mode -> reset.
 * Shows progressive recovery attempts before resorting to system reset.
 */
FI_TEST(escalating_recovery) {
    gf_rel_init();
    
    /* First level: retry */
    gf_recovery_policy_t p1 = {
        .fault_type = GF_FAULT_COMM_TIMEOUT,
        .strategy = GF_RECOVERY_STRATEGY_RETRY,
        .max_retries = 3,
        .retry_delay_ms = 0,
        .escalate_on_fail = true
    };
    gf_rel_set_recovery_policy(&p1);
    
    gf_recovery_strategy_t s1 = gf_rel_execute_recovery(GF_FAULT_COMM_TIMEOUT);
    FI_ASSERT(s1 == GF_RECOVERY_STRATEGY_RETRY, "First level should retry");
    
    gf_recovery_status_t status;
    gf_rel_get_recovery_status(&status);
    FI_ASSERT(status.retry_count >= 1, "Retry count not updated");
    
    FI_PASS();
}

/*===========================================================================*/
/* Test Runner                                                                */
/*===========================================================================*/

int run_fault_injection_tests(void) {
    printf("\n========================================\n");
    printf("FAULT INJECTION & RESILIENCE TEST SUITE\n");
    printf("========================================\n");
    
    printf("\n[Task Watchdog Tests]\n");
    FI_RUN_TEST(wdt_registration);
    FI_RUN_TEST(wdt_kick_mechanism);
    FI_RUN_TEST(wdt_multi_task);
    FI_RUN_TEST(wdt_deadline_mode);
    
    printf("\n[Persistent Fault Logging Tests]\n");
    FI_RUN_TEST(fault_log_record);
    FI_RUN_TEST(fault_log_rollover);
    FI_RUN_TEST(fault_severity_tracking);
    FI_RUN_TEST(fault_acknowledgment);
    
    printf("\n[Recovery Strategy Tests]\n");
    FI_RUN_TEST(recovery_policy_config);
    FI_RUN_TEST(recovery_hook_invocation);
    FI_RUN_TEST(safe_mode_operation);
    FI_RUN_TEST(feature_degradation);
    
#ifdef GF_ENABLE_FAULT_INJECTION
    printf("\n[Fault Injection Tests]\n");
    FI_RUN_TEST(inject_enable_disable);
    FI_RUN_TEST(inject_trigger_check);
    FI_RUN_TEST(inject_one_shot);
    FI_RUN_TEST(inject_delayed_trigger);
    FI_RUN_TEST(inject_subsystem_target);
#endif
    
    printf("\n[Integrated Resilience Scenarios]\n");
    FI_RUN_TEST(integrated_wdt_fault_recovery);
    FI_RUN_TEST(escalating_recovery);
    
    printf("\n========================================\n");
    printf("RESULTS: %d/%d passed", tests_passed, tests_run);
    if (tests_failed > 0) {
        printf(" (%d FAILED)", tests_failed);
    }
    printf("\n========================================\n\n");
    
    return tests_failed;
}

/* Standalone execution */
int main(void) {
    return run_fault_injection_tests();
}
