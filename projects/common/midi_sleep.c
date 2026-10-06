/* midi_sleep.c - see midi_sleep.h */

#include "midi_sleep.h"

#ifdef _WIN32
#include <windows.h>
#else
#include <unistd.h>
#endif

static int g_no_sleep = 0;

void midi_set_no_sleep(int on) {
    g_no_sleep = on;
}

int midi_no_sleep(void) {
    return g_no_sleep;
}

void midi_sleep(int ms) {
    if (ms <= 0 || g_no_sleep) {
        return;
    }
#ifdef _WIN32
    Sleep((DWORD)ms);
#else
    usleep((useconds_t)ms * 1000);
#endif
}

int midi_wait_ms(int ms) {
    return g_no_sleep ? 0 : ms;
}
