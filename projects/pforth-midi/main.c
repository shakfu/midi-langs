/* main.c - pforth-midi: pForth with MIDI words in a static dictionary */

#include "pforth.h"
#include "midi_ffi.h"

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

static void usage(const char* prog) {
    printf("Usage: %s [-q] [file.fs]\n", prog);
    printf("\n");
    printf("pForth with MIDI words. With no file, starts the interactive prompt.\n");
    printf("Type MIDI-HELP at the prompt for the MIDI words, BYE to exit.\n");
    printf("\n");
    printf("  -q    No banner\n");
}

int main(int argc, char** argv) {
    const char* source = NULL;
    int quiet = 0;

    for (int i = 1; i < argc; i++) {
        if (strcmp(argv[i], "-h") == 0 || strcmp(argv[i], "--help") == 0) {
            usage(argv[0]);
            return 0;
        } else if (strcmp(argv[i], "-q") == 0) {
            quiet = 1;
        } else if (argv[i][0] == '-') {
            fprintf(stderr, "Unknown option: %s\n", argv[i]);
            usage(argv[0]);
            return 1;
        } else {
            source = argv[i];
        }
    }

    /* Runs on BYE and on EOF (see pf_io_midi.c); midi_cleanup sends all-notes-off */
    atexit(midi_cleanup);

    /* pForth's banner and "Including:" line are noise when running a file */
    pfSetQuiet(quiet || source != NULL);
    return pfDoForth(NULL, source, 0) == 0 ? 0 : 1;
}
