/**
 * @file test_framework.c
 * @brief Unit Test Framework Implementation
 */

#include "test_framework.h"
#include <stdio.h>

/*===========================================================================*/
/* Global State                                                               */
/*===========================================================================*/

gf_test_stats_t g_test_stats;

/*===========================================================================*/
/* API Implementation                                                         */
/*===========================================================================*/

void gf_test_init(void)
{
    memset(&g_test_stats, 0, sizeof(g_test_stats));
    printf("\n");
    printf("================================================================\n");
    printf("  Grey Firmware Test Framework v1.0\n");
    printf("================================================================\n\n");
}

bool gf_test_run_case(const char *name, gf_test_fn fn)
{
    g_test_stats.tests_run++;
    g_test_stats.current_test = name;
    g_test_stats.current_passed = true;
    
    printf("  [TEST] %-40s ", name);
    fflush(stdout);
    
    /* Run the test */
    fn();
    
    if (g_test_stats.current_passed) {
        g_test_stats.tests_passed++;
        printf("PASS\n");
        return true;
    } else {
        g_test_stats.tests_failed++;
        printf("FAIL\n");
        return false;
    }
}

void gf_test_run_suite(gf_test_suite_t *suite)
{
    if (!suite) return;
    
    g_test_stats.current_suite = suite->name;
    
    printf("\n[SUITE] %s\n", suite->name);
    printf("----------------------------------------------------------------\n");
    
    for (uint8_t i = 0; i < suite->test_count && i < GF_TEST_MAX_TESTS; i++) {
        gf_test_case_t *tc = &suite->tests[i];
        
        if (!tc->fn) continue;
        
        /* Setup */
        if (suite->setup) {
            suite->setup();
        }
        
        /* Run test */
        gf_test_run_case(tc->name, tc->fn);
        
        /* Teardown */
        if (suite->teardown) {
            suite->teardown();
        }
    }
    
    printf("\n");
}

void gf_test_print_summary(void)
{
    printf("================================================================\n");
    printf("  TEST SUMMARY\n");
    printf("================================================================\n");
    printf("  Tests Run:        %u\n", g_test_stats.tests_run);
    printf("  Tests Passed:     %u\n", g_test_stats.tests_passed);
    printf("  Tests Failed:     %u\n", g_test_stats.tests_failed);
    printf("  Assertions Run:   %u\n", g_test_stats.assertions_run);
    printf("  Assertions Failed:%u\n", g_test_stats.assertions_failed);
    printf("----------------------------------------------------------------\n");
    
    if (g_test_stats.tests_failed == 0) {
        printf("  RESULT: ALL TESTS PASSED\n");
    } else {
        printf("  RESULT: %u TEST(S) FAILED\n", g_test_stats.tests_failed);
    }
    printf("================================================================\n\n");
}

void gf_test_get_stats(gf_test_stats_t *stats)
{
    if (stats) {
        memcpy(stats, &g_test_stats, sizeof(gf_test_stats_t));
    }
}

void gf_test_reset(void)
{
    memset(&g_test_stats, 0, sizeof(g_test_stats));
}
