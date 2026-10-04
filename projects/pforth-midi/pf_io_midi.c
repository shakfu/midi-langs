/* pf_io_midi.c - terminal input for pforth-midi.
 *
 * pForth's posix/pf_io_posix.c is compiled with sdTerminalIn and
 * sdTerminalEcho renamed to pf_posix_*; these wrappers replace them.
 * Upstream treats getchar()'s EOF as character 0xFF and loops forever on
 * piped input, and echoes input even when stdin is not a terminal. */

#include "pf_all.h"

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

int pf_posix_sdTerminalIn(void);
int pf_posix_sdTerminalEcho(char c);

int sdTerminalIn(void) {
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
