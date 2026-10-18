/* pf_io_midi.c - terminal I/O and sleep for pforth-midi.
 *
 * pForth's posix/pf_io_posix.c is compiled with sdTerminalIn,
 * sdTerminalEcho and sdTerminalInit renamed to pf_posix_*; these wrappers
 * replace them.
 * Upstream treats getchar()'s EOF as character 0xFF and loops forever on
 * piped input, and echoes input even when stdin is not a terminal. It also
 * sets raw mode on a terminal it does not own, which stops a background
 * process with SIGTTOU (e.g. under `timeout` or `&`).
 * sdTerminalOut and sdSleepMillis are wrapped too, for the CLAP plugin: its
 * output goes to the plugin's log, and MS waits in script time. */

#include "pf_all.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

#include "midi_clock.h"
#include "pf_io_midi.h"

int pf_posix_sdTerminalIn(void);
int pf_posix_sdTerminalOut(char c);
cell_t pf_posix_sdSleepMillis(cell_t msec);
int pf_posix_sdTerminalEcho(char c);
void pf_posix_sdTerminalInit(void);

static void (*g_output)(char c, void* user) = NULL;
static void* g_output_user = NULL;

void pfm_set_output(void (*write)(char c, void* user), void* user) {
    g_output = write;
    g_output_user = user;
}

int sdTerminalOut(char c) {
    if (g_output) {
        g_output(c, g_output_user);
        return c;
    }
    return pf_posix_sdTerminalOut(c);
}

cell_t sdSleepMillis(cell_t msec) {
    if (midi_sink_active()) {
        midi_clock_wait_until(midi_clock_ms() + (msec > 0 ? (double)msec : 0.0));
        return 0;
    }
    return pf_posix_sdSleepMillis(msec);
}

void sdTerminalInit(void) {
    /* Only the terminal's foreground process group may change its modes */
    if (isatty(STDIN_FILENO) && tcgetpgrp(STDIN_FILENO) != getpgrp()) return;
    pf_posix_sdTerminalInit();
}

int sdTerminalIn(void) {
    /* A host's stdin is not ours: KEY and ACCEPT get an empty line */
    if (g_output) return '\n';
    int c = pf_posix_sdTerminalIn();
    if (c == EOF) {
        /* End of piped input or a closed terminal: leave like BYE */
        if (isatty(STDIN_FILENO)) putchar('\n');
        sdTerminalTerm();
        exit(0);
    }
    return c;
}

int sdTerminalEcho(char c) {
    /* The terminal is in no-echo mode only when stdin is a tty */
    if (!isatty(STDIN_FILENO)) return 0;
    return pf_posix_sdTerminalEcho(c);
}
