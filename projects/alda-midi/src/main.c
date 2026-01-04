/**
 * @file main.c
 * @brief Alda-MIDI main executable - CLI and file playback.
 */

#include "alda/alda.h"
#include "alda/context.h"
#include "alda/midi_backend.h"
#include "alda/scheduler.h"
#include "alda/interpreter.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <getopt.h>

/* ============================================================================
 * Usage and Help
 * ============================================================================ */

static void print_usage(const char* prog) {
    printf("Usage: %s [options] [file.alda]\n", prog);
    printf("\n");
    printf("Alda music language interpreter with MIDI output.\n");
    printf("\n");
    printf("Options:\n");
    printf("  -h, --help        Show this help message\n");
    printf("  -v, --verbose     Enable verbose output\n");
    printf("  -l, --list        List available MIDI ports\n");
    printf("  -p, --port N      Use MIDI port N (0-based index)\n");
    printf("  -o, --output NAME Use MIDI port matching NAME\n");
    printf("  --virtual NAME    Create virtual MIDI port with NAME\n");
    printf("  --no-sleep        Disable timing delays (for testing)\n");
    printf("\n");
    printf("Examples:\n");
    printf("  %s song.alda              Play an Alda file\n", prog);
    printf("  %s -l                     List MIDI ports\n", prog);
    printf("  %s -p 0 song.alda         Play using port 0\n", prog);
    printf("  %s --virtual myMIDI song.alda  Create virtual port 'Alda'\n", prog);
    printf("\n");
}

/* ============================================================================
 * Main
 * ============================================================================ */

int main(int argc, char* argv[]) {
    int verbose = 0;
    int list_ports = 0;
    int port_index = -1;
    const char* port_name = NULL;
    const char* virtual_name = NULL;
    int no_sleep = 0;
    const char* input_file = NULL;

    /* Long options */
    static struct option long_options[] = {
        {"help",     no_argument,       0, 'h'},
        {"verbose",  no_argument,       0, 'v'},
        {"list",     no_argument,       0, 'l'},
        {"port",     required_argument, 0, 'p'},
        {"output",   required_argument, 0, 'o'},
        {"virtual",  required_argument, 0, 'V'},
        {"no-sleep", no_argument,       0, 'S'},
        {0, 0, 0, 0}
    };

    /* Parse options */
    int opt;
    int option_index = 0;
    while ((opt = getopt_long(argc, argv, "hvlp:o:", long_options, &option_index)) != -1) {
        switch (opt) {
            case 'h':
                print_usage(argv[0]);
                return 0;

            case 'v':
                verbose = 1;
                break;

            case 'l':
                list_ports = 1;
                break;

            case 'p':
                port_index = atoi(optarg);
                break;

            case 'o':
                port_name = optarg;
                break;

            case 'V':
                virtual_name = optarg;
                break;

            case 'S':
                no_sleep = 1;
                break;

            default:
                print_usage(argv[0]);
                return 1;
        }
    }

    /* Get input file if provided */
    if (optind < argc) {
        input_file = argv[optind];
    }

    /* Initialize context */
    AldaContext ctx;
    alda_context_init(&ctx);
    ctx.verbose_mode = verbose;
    ctx.no_sleep_mode = no_sleep;

    /* Initialize MIDI observer */
    alda_midi_init_observer(&ctx);

    /* Handle --list */
    if (list_ports) {
        alda_midi_list_ports(&ctx);
        alda_midi_cleanup(&ctx);
        alda_context_cleanup(&ctx);
        return 0;
    }

    /* Open MIDI output */
    int midi_opened = 0;

    if (virtual_name) {
        /* Create virtual port */
        if (alda_midi_open_virtual(&ctx, virtual_name) == 0) {
            midi_opened = 1;
            if (verbose) {
                printf("Created virtual MIDI output: %s\n", virtual_name);
            }
        }
    } else if (port_name) {
        /* Open by name */
        if (alda_midi_open_by_name(&ctx, port_name) == 0) {
            midi_opened = 1;
        }
    } else if (port_index >= 0) {
        /* Open by index */
        if (alda_midi_open_port(&ctx, port_index) == 0) {
            midi_opened = 1;
        }
    } else {
        /* Default: create virtual port */
        if (alda_midi_open_virtual(&ctx, "AldaMIDI") == 0) {
            midi_opened = 1;
            if (verbose) {
                printf("Created virtual MIDI output: AldaMIDI\n");
            }
        }
    }

    if (!midi_opened) {
        fprintf(stderr, "Warning: Failed to open MIDI output\n");
    }

    /* Check if we have an input file */
    if (!input_file) {
        fprintf(stderr, "Error: No input file specified\n");
        print_usage(argv[0]);
        alda_midi_cleanup(&ctx);
        alda_context_cleanup(&ctx);
        return 1;
    }

    /* Interpret the file */
    if (verbose) {
        printf("Playing: %s\n", input_file);
    }

    int result = alda_interpret_file(&ctx, input_file);

    if (result < 0) {
        fprintf(stderr, "Error: Failed to interpret file\n");
        alda_midi_cleanup(&ctx);
        alda_context_cleanup(&ctx);
        return 1;
    }

    /* Play the scheduled events */
    if (verbose) {
        printf("Scheduled %d events\n", ctx.event_count);
    }

    result = alda_events_play(&ctx);

    if (result < 0) {
        fprintf(stderr, "Error: Failed to play events\n");
    }

    /* Cleanup */
    alda_midi_cleanup(&ctx);
    alda_context_cleanup(&ctx);

    return result < 0 ? 1 : 0;
}
