/* test_framework.c - Test framework implementation */

#include "test_framework.h"

/* Global test statistics */
test_stats_t test_stats = {0, 0, 0, 0, NULL};

/* Run a test with optional setup and teardown */
void run_test_with_setup(test_func_t setup, test_func_t test, test_func_t teardown) {
    if (setup) setup();
    if (test) test();
    if (teardown) teardown();
}
