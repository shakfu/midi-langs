/* test_framework.h - Simple C testing framework
 *
 * A minimal testing framework with no external dependencies.
 * Provides assertion macros and test runner infrastructure.
 */

/* Feature test macros - must be before any system includes.
 * These enable POSIX functions like mkdtemp() and nftw() in test utilities. */
#ifndef _XOPEN_SOURCE
#define _XOPEN_SOURCE 700
#endif
#ifndef _DEFAULT_SOURCE
#define _DEFAULT_SOURCE 1
#endif
#if defined(__APPLE__) && !defined(_DARWIN_C_SOURCE)
#define _DARWIN_C_SOURCE
#endif

#ifndef TEST_FRAMEWORK_H
#define TEST_FRAMEWORK_H

#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/* Test statistics */
typedef struct {
    int total_tests;
    int passed_tests;
    int failed_tests;
    int current_test_failed;
    const char *current_test_name;
} test_stats_t;

extern test_stats_t test_stats;

/* Color output for terminals */
#define COLOR_RED     "\x1b[31m"
#define COLOR_GREEN   "\x1b[32m"
#define COLOR_YELLOW  "\x1b[33m"
#define COLOR_RESET   "\x1b[0m"

/* Test macros */
#define TEST(name) \
    static void test_##name(void); \
    static void test_##name##_wrapper(void) { \
        test_stats.current_test_name = #name; \
        test_stats.current_test_failed = 0; \
        test_##name(); \
        if (!test_stats.current_test_failed) { \
            test_stats.passed_tests++; \
            printf(COLOR_GREEN "  ✓ " COLOR_RESET "%s\n", #name); \
        } \
    } \
    static void test_##name(void)

#define RUN_TEST(name) do { \
    test_stats.total_tests++; \
    test_##name##_wrapper(); \
} while(0)

/* Assertion macros */
#define ASSERT_TRUE(condition) do { \
    if (!(condition)) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Assertion failed: %s\n", \
               __FILE__, __LINE__, #condition); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_FALSE(condition) ASSERT_TRUE(!(condition))

#define ASSERT_EQ(actual, expected) do { \
    if ((actual) != (expected)) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected %d, got %d\n", \
               __FILE__, __LINE__, (int)(expected), (int)(actual)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_PTR_EQ(actual, expected) do { \
    if ((actual) != (expected)) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected pointer %p, got %p\n", \
               __FILE__, __LINE__, (const void*)(expected), (const void*)(actual)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_NEQ(actual, expected) do { \
    if ((actual) == (expected)) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected not equal to %d\n", \
               __FILE__, __LINE__, (int)(expected)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_GT(actual, expected) do { \
    if (!((actual) > (expected))) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected %d > %d\n", \
               __FILE__, __LINE__, (int)(actual), (int)(expected)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_LT(actual, expected) do { \
    if (!((actual) < (expected))) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected %d < %d\n", \
               __FILE__, __LINE__, (int)(actual), (int)(expected)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_GTE(actual, expected) do { \
    if (!((actual) >= (expected))) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected %d >= %d\n", \
               __FILE__, __LINE__, (int)(actual), (int)(expected)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_LTE(actual, expected) do { \
    if (!((actual) <= (expected))) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected %d <= %d\n", \
               __FILE__, __LINE__, (int)(actual), (int)(expected)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_STR_EQ(actual, expected) do { \
    if (strcmp((actual), (expected)) != 0) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected \"%s\", got \"%s\"\n", \
               __FILE__, __LINE__, (expected), (actual)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_NULL(ptr) do { \
    if ((ptr) != NULL) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected NULL pointer\n", \
               __FILE__, __LINE__); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_NOT_NULL(ptr) do { \
    if ((ptr) == NULL) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected non-NULL pointer\n", \
               __FILE__, __LINE__); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

#define ASSERT_NEAR(actual, expected, epsilon) do { \
    if (fabs((actual) - (expected)) > (epsilon)) { \
        printf(COLOR_RED "  ✗ " COLOR_RESET "%s:%d: Expected %g, got %g (epsilon=%g)\n", \
               __FILE__, __LINE__, (double)(expected), (double)(actual), (double)(epsilon)); \
        test_stats.current_test_failed = 1; \
        test_stats.failed_tests++; \
        return; \
    } \
} while(0)

/* Compatibility aliases */
#define ASSERT(cond) ASSERT_TRUE(cond)
#define ASSERT_STREQ(a, b) ASSERT_STR_EQ(a, b)

/* Legacy test result macros (for backward compatibility) */
#define TEST_PASS() do { \
    /* No-op: pass is recorded automatically by RUN_TEST */ \
} while(0)

#define TEST_SUMMARY() do { \
    printf("\n%d tests, %d passed, %d failed\n", \
           test_stats.total_tests, test_stats.passed_tests, test_stats.failed_tests); \
} while(0)

#define TEST_EXIT_CODE() (test_stats.failed_tests > 0 ? 1 : 0)

/* Test suite infrastructure */
#define BEGIN_TEST_SUITE(name) \
    int main(void) { \
        printf("\n" COLOR_YELLOW "Running test suite: " COLOR_RESET "%s\n\n", name); \
        test_stats.total_tests = 0; \
        test_stats.passed_tests = 0; \
        test_stats.failed_tests = 0;

#define END_TEST_SUITE() \
        printf("\n" COLOR_YELLOW "Results: " COLOR_RESET); \
        if (test_stats.failed_tests == 0) { \
            printf(COLOR_GREEN "%d/%d tests passed\n" COLOR_RESET, \
                   test_stats.passed_tests, test_stats.total_tests); \
            return 0; \
        } else { \
            printf(COLOR_RED "%d/%d tests passed, %d failed\n" COLOR_RESET, \
                   test_stats.passed_tests, test_stats.total_tests, \
                   test_stats.failed_tests); \
            return 1; \
        } \
    }

/* =============================================================================
 * Test Fixtures
 * =============================================================================
 * Fixtures provide setup/teardown functions that run before/after each test.
 *
 * Usage:
 *   FIXTURE(my_fixture, {
 *       // fixture state
 *       int counter;
 *       char *buffer;
 *   });
 *
 *   FIXTURE_SETUP(my_fixture) {
 *       fixture->counter = 0;
 *       fixture->buffer = malloc(256);
 *   }
 *
 *   FIXTURE_TEARDOWN(my_fixture) {
 *       free(fixture->buffer);
 *   }
 *
 *   TEST_F(my_fixture, test_name) {
 *       fixture->counter++;
 *       ASSERT_NOT_NULL(fixture->buffer);
 *   }
 *
 *   // In main:
 *   RUN_TEST_F(my_fixture, test_name);
 */

/* Define a fixture with its state structure */
#define FIXTURE(name, state_struct) \
    typedef struct name##_fixture_state state_struct name##_fixture_state_t; \
    static name##_fixture_state_t name##_fixture_instance; \
    static void name##_setup(name##_fixture_state_t *fixture); \
    static void name##_teardown(name##_fixture_state_t *fixture)

/* Define the setup function for a fixture */
#define FIXTURE_SETUP(name) \
    static void name##_setup(name##_fixture_state_t *fixture)

/* Define the teardown function for a fixture */
#define FIXTURE_TEARDOWN(name) \
    static void name##_teardown(name##_fixture_state_t *fixture)

/* Define a test that uses a fixture */
#define TEST_F(fixture_name, test_name) \
    static void test_##fixture_name##_##test_name(fixture_name##_fixture_state_t *fixture); \
    static void test_##fixture_name##_##test_name##_wrapper(void) { \
        test_stats.current_test_name = #fixture_name "_" #test_name; \
        test_stats.current_test_failed = 0; \
        fixture_name##_setup(&fixture_name##_fixture_instance); \
        test_##fixture_name##_##test_name(&fixture_name##_fixture_instance); \
        fixture_name##_teardown(&fixture_name##_fixture_instance); \
        if (!test_stats.current_test_failed) { \
            test_stats.passed_tests++; \
            printf(COLOR_GREEN "  ✓ " COLOR_RESET "%s_%s\n", #fixture_name, #test_name); \
        } \
    } \
    static void test_##fixture_name##_##test_name(fixture_name##_fixture_state_t *fixture)

/* Run a fixture-based test */
#define RUN_TEST_F(fixture_name, test_name) do { \
    test_stats.total_tests++; \
    test_##fixture_name##_##test_name##_wrapper(); \
} while(0)

/* =============================================================================
 * Suite-level Setup/Teardown
 * =============================================================================
 * For setup/teardown that runs once per test suite (not per test).
 *
 * Usage:
 *   SUITE_SETUP(my_suite) {
 *       // runs once before all tests
 *   }
 *
 *   SUITE_TEARDOWN(my_suite) {
 *       // runs once after all tests
 *   }
 *
 *   BEGIN_TEST_SUITE_WITH_FIXTURE("Suite Name", my_suite)
 *       RUN_TEST(test1);
 *       RUN_TEST(test2);
 *   END_TEST_SUITE()
 */

#define SUITE_SETUP(name) \
    static void name##_suite_setup(void)

#define SUITE_TEARDOWN(name) \
    static void name##_suite_teardown(void)

#define BEGIN_TEST_SUITE_WITH_FIXTURE(suite_name, fixture_name) \
    int main(void) { \
        printf("\n" COLOR_YELLOW "Running test suite: " COLOR_RESET "%s\n\n", suite_name); \
        test_stats.total_tests = 0; \
        test_stats.passed_tests = 0; \
        test_stats.failed_tests = 0; \
        fixture_name##_suite_setup();

#define END_TEST_SUITE_WITH_FIXTURE(fixture_name) \
        fixture_name##_suite_teardown(); \
        printf("\n" COLOR_YELLOW "Results: " COLOR_RESET); \
        if (test_stats.failed_tests == 0) { \
            printf(COLOR_GREEN "%d/%d tests passed\n" COLOR_RESET, \
                   test_stats.passed_tests, test_stats.total_tests); \
            return 0; \
        } else { \
            printf(COLOR_RED "%d/%d tests passed, %d failed\n" COLOR_RESET, \
                   test_stats.passed_tests, test_stats.total_tests, \
                   test_stats.failed_tests); \
            return 1; \
        } \
    }

/* Legacy helper for setup/teardown (deprecated, use fixtures instead) */
typedef void (*test_func_t)(void);

void run_test_with_setup(test_func_t setup, test_func_t test, test_func_t teardown);

/* =============================================================================
 * Memory Leak Detection
 * =============================================================================
 * Include test_memcheck.h for memory leak detection in tests.
 *
 * Usage:
 *   #include "test_memcheck.h"
 *
 *   TEST(my_test) {
 *       void *ptr = MEMCHECK_MALLOC(100);
 *       // ... use ptr ...
 *       MEMCHECK_FREE(ptr);
 *       ASSERT_NO_LEAKS();
 *   }
 *
 *   BEGIN_TEST_SUITE_MEMCHECK("Suite")
 *       RUN_TEST_MEMCHECK(my_test);  // Fails if test leaks memory
 *   END_TEST_SUITE_MEMCHECK()
 */

#endif /* TEST_FRAMEWORK_H */
