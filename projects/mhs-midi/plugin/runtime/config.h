/* MicroHs configuration for the mhs-midi plugin: the platform's, without IO polling.
 * With polling, a runtime whose threads all sleep waits in poll() on the wall clock;
 * without it, it waits in CLOCK_SLEEP, which extra.c turns into script time.
 * This directory precedes the runtime's on the include path. */
#if defined(__APPLE__) || defined(__unix__)
#include "unix/config.h"
#else
#error "the mhs-midi plugin is built for Unix only"
#endif
#undef WANT_IO_POLL
#define WANT_IO_POLL 0
