/* The platform's extra.c, with the runtime clock replaced by script time (midi_clock.h),
 * in microseconds. threadDelay then waits in script time, and a runtime whose threads
 * all sleep advances script time to the earliest wake instead of sleeping.
 * Both functions are defined in mhs_plugin_runtime.c. */
#include "unix/extra.c"
#undef CLOCK_GET
#undef CLOCK_SLEEP
#define CLOCK_GET mhs_plugin_clock_get
#define CLOCK_SLEEP mhs_plugin_clock_sleep
int64_t mhs_plugin_clock_get(void);
void mhs_plugin_clock_sleep(useconds_t usecs);
