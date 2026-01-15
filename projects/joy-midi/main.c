/*
 * Joy-MIDI - Joy interpreter with MIDI extensions
 *
 * This embeds the pyjoy-runtime and extends it with MIDI primitives.
 */

#include "joy_runtime.h"
#include "joy_parser.h"
#include "joy_midi.h"
#include "music_notation.h"
#include "midi_primitives.h"
#include <string.h>
#include <stdlib.h>

static void banner(void) {
    printf("Joy-MIDI  -  Joy interpreter with MIDI extensions\n");
    printf("Type 'quit' to exit, 'help' for MIDI words\n");
    fflush(stdout);
}

static void usage(const char* prog) {
    printf("Usage: %s [options] [file.joy]\n", prog);
    printf("Options:\n");
    printf("  -h        Show this help\n");
    printf("  -v        Show version\n");
    printf("\nMIDI words:\n");
    printf("  midi-list    - List MIDI output ports\n");
    printf("  midi-virtual - Create virtual MIDI port 'JoyMIDI'\n");
    printf("  midi-open    - Open port by index (n -- )\n");
    printf("  midi-note    - Play note (pitch vel dur -- )\n");
    printf("  midi-chord   - Play chord ([pitches] vel dur -- )\n");
    printf("  pitch        - Parse pitch name (\"C4\" -- 60)\n");
    printf("  major        - Build major triad (root -- [pitches])\n");
    printf("  minor        - Build minor triad (root -- [pitches])\n");
}

int main(int argc, char** argv) {
    int show_banner = 1;
    char* filename = NULL;

    /* Set argv for Joy */
    joy_set_argv(argc, argv);

    /* Parse command line */
    for (int i = 1; i < argc; i++) {
        if (argv[i][0] == '-') {
            for (int j = 1; argv[i][j]; j++) {
                switch (argv[i][j]) {
                case 'h':
                    usage(argv[0]);
                    return 0;
                case 'v':
                    banner();
                    return 0;
                default:
                    fprintf(stderr, "Unknown option: -%c\n", argv[i][j]);
                    return 1;
                }
            }
        } else {
            filename = argv[i];
            show_banner = 0;
        }
    }

    /* Create execution context */
    JoyContext* ctx = joy_context_new();
    if (!ctx) {
        fprintf(stderr, "Failed to create Joy context\n");
        return 1;
    }

    /* Initialize Joy runtime (registers builtin primitives) */
    joy_runtime_init(ctx);

    /* Initialize music notation system (Alda-like syntax) */
    music_notation_init(ctx);

    /* Register MIDI primitives */
    joy_midi_register_primitives(ctx);

    /* Enable autoput (show stack after each line) */
    ctx->autoput = 1;

    /* If a file was specified, run it */
    if (filename) {
        if (joy_load_file(ctx, filename) != 0) {
            fprintf(stderr, "Failed to open file: %s\n", filename);
            joy_midi_cleanup();
            joy_context_free(ctx);
            return 1;
        }
    } else {
        /* Interactive mode */
        if (show_banner) {
            banner();
        }

        /* Auto-create virtual MIDI port for convenience */
        midi_virtual_(ctx);

        /* Run REPL */
        joy_repl(ctx);
    }

    /* Cleanup */
    music_notation_cleanup(ctx);
    joy_midi_cleanup();
    joy_context_free(ctx);

    return 0;
}
