/**
 * @file test_framework.h
 * @brief Lightweight Unit Test Framework for Embedded Systems
 * 
 * Minimal test framework designed to run on target or in QEMU/simulation.
 * No dynamic memory allocation, suitable for memory-constrained systems.
 */

#ifndef GF_TEST_FRAMEWORK_H
#define GF_TEST_FRAMEWORK_H

#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>

/*===========================================================================*/
/* Test Framework Macros                                                      */
/*===========================================================================*/

#define GF_TEST_MAX_TESTS       64
#define GF_TEST_MAX_SUITES      16
#define GF_TEST_NAME_MAX        32

/* Test result tracking */
typedef struct {
    uint32_t    tests_run;
    uint32_t    tests_passed;
    uint32_t    tests_failed;
    uint32_t    assertions_run;
    uint32_t    assertions_failed;
    const char *current_test;
    const char *current_suite;
    bool        current_passed;
} gf_test_stats_t;

extern gf_test_stats_t g_test_stats;

/*===========================================================================*/
/* Assertion Macros                                                           */
/*===========================================================================*/

#define GF_TEST_ASSERT(cond) \
    do { \
        g_test_stats.assertions_run++; \
        if (!(cond)) { \
            g_test_stats.assertions_failed++; \
            g_test_stats.current_passed = false; \
            printf("    FAIL: %s:%d: %s\n", __FILE__, __LINE__, #cond); \
        } \
    } while (0)

#define GF_TEST_ASSERT_MSG(cond, msg) \
    do { \
        g_test_stats.assertions_run++; \
        if (!(cond)) { \
            g_test_stats.assertions_failed++; \
            g_test_stats.current_passed = false; \
            printf("    FAIL: %s:%d: %s - %s\n", __FILE__, __LINE__, #cond, msg); \
        } \
    } while (0)

#define GF_TEST_ASSERT_EQ(a, b) \
    do { \
        g_test_stats.assertions_run++; \
        if ((a) != (b)) { \
            g_test_stats.assertions_failed++; \
            g_test_stats.current_passed = false; \
            printf("    FAIL: %s:%d: %s == %s (got %d, expected %d)\n", \
                   __FILE__, __LINE__, #a, #b, (int)(a), (int)(b)); \
        } \
    } while (0)

#define GF_TEST_ASSERT_NE(a, b) \
    do { \
        g_test_stats.assertions_run++; \
        if ((a) == (b)) { \
            g_test_stats.assertions_failed++; \
            g_test_stats.current_passed = false; \
            printf("    FAIL: %s:%d: %s != %s\n", __FILE__, __LINE__, #a, #b); \
        } \
    } while (0)

#define GF_TEST_ASSERT_NULL(ptr) GF_TEST_ASSERT((ptr) == NULL)
#define GF_TEST_ASSERT_NOT_NULL(ptr) GF_TEST_ASSERT((ptr) != NULL)

#define GF_TEST_ASSERT_STR_EQ(a, b) \
    do { \
        g_test_stats.assertions_run++; \
        if (strcmp((a), (b)) != 0) { \
            g_test_stats.assertions_failed++; \
            g_test_stats.current_passed = false; \
            printf("    FAIL: %s:%d: '%s' == '%s'\n", __FILE__, __LINE__, (a), (b)); \
        } \
    } while (0)

#define GF_TEST_ASSERT_MEM_EQ(a, b, len) \
    do { \
        g_test_stats.assertions_run++; \
        if (memcmp((a), (b), (len)) != 0) { \
            g_test_stats.assertions_failed++; \
            g_test_stats.current_passed = false; \
            printf("    FAIL: %s:%d: memory mismatch\n", __FILE__, __LINE__); \
        } \
    } while (0)

#define GF_TEST_ASSERT_FLOAT_EQ(a, b, eps) \
    do { \
        g_test_stats.assertions_run++; \
        float diff = (a) - (b); \
        if (diff < 0) diff = -diff; \
        if (diff > (eps)) { \
            g_test_stats.assertions_failed++; \
            g_test_stats.current_passed = false; \
            printf("    FAIL: %s:%d: %f != %f (eps=%f)\n", \
                   __FILE__, __LINE__, (double)(a), (double)(b), (double)(eps)); \
        } \
    } while (0)

/*===========================================================================*/
/* Test Definition Macros                                                     */
/*===========================================================================*/

typedef void (*gf_test_fn)(void);

typedef struct {
    const char *name;
    gf_test_fn  fn;
} gf_test_case_t;

typedef struct {
    const char      *name;
    gf_test_fn       setup;
    gf_test_fn       teardown;
    gf_test_case_t   tests[GF_TEST_MAX_TESTS];
    uint8_t          test_count;
} gf_test_suite_t;

#define GF_TEST_SUITE_BEGIN(suite_name) \
    static gf_test_suite_t suite_name = { .name = #suite_name }

#define GF_TEST_SUITE_SETUP(fn) \
    suite.setup = fn

#define GF_TEST_SUITE_TEARDOWN(fn) \
    suite.teardown = fn

#define GF_TEST_CASE(test_fn) \
    { .name = #test_fn, .fn = test_fn }

/*===========================================================================*/
/* API Functions                                                              */
/*===========================================================================*/

/**
 * @brief Initialize test framework
 */
void gf_test_init(void);

/**
 * @brief Run a single test case
 */
bool gf_test_run_case(const char *name, gf_test_fn fn);

/**
 * @brief Run an entire test suite
 */
void gf_test_run_suite(gf_test_suite_t *suite);

/**
 * @brief Print test summary
 */
void gf_test_print_summary(void);

/**
 * @brief Get test results
 */
void gf_test_get_stats(gf_test_stats_t *stats);

/**
 * @brief Reset test statistics
 */
void gf_test_reset(void);

/*===========================================================================*/
/* Mock/Stub Helpers                                                          */
/*===========================================================================*/

/* Track function call counts for verification */
typedef struct {
    const char *name;
    uint32_t    call_count;
    int         last_result;
    void       *last_arg;
} gf_mock_t;

#define GF_MOCK_INIT(mock_name) \
    static gf_mock_t mock_name = { .name = #mock_name }

#define GF_MOCK_RESET(mock) \
    do { (mock).call_count = 0; (mock).last_result = 0; (mock).last_arg = NULL; } while(0)

#define GF_MOCK_CALLED(mock) \
    ((mock).call_count++)

#define GF_MOCK_VERIFY_CALLED(mock, expected) \
    GF_TEST_ASSERT_EQ((mock).call_count, expected)

#endif /* GF_TEST_FRAMEWORK_H */
