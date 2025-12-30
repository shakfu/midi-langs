/* main.c - Main entry point for MIDI Forth interpreter */

#include "forth_midi.h"
#include <readline/readline.h>
#include <readline/history.h>
#include <getopt.h>

/* Print usage */
static void print_usage(const char* prog) {
    printf("Usage: %s [options] [file.4th ...]\n", prog);
    printf("Options:\n");
    printf("  --script FILE   Run FILE in batch mode (no REPL, exit on error)\n");
    printf("  --no-sleep      Disable all sleep/delay calls (for testing)\n");
    printf("  --help          Show this help\n");
    printf("\nWithout --script, files are loaded then REPL starts.\n");
}

/* Interactive interpreter loop */
static void interpreter_loop(void) {
    char* input;

    printf("MIDI Forth (type 'help' for commands, 'quit' to exit)\n");

    while (1) {
        input = readline("> ");

        if (input == NULL) {
            /* EOF (Ctrl-D) */
            printf("\n");
            break;
        }

        /* Skip empty lines */
        if (input[0] == '\0') {
            free(input);
            continue;
        }

        /* Add to history */
        add_history(input);

        if (strcmp(input, "quit") == 0) {
            free(input);
            break;
        }

        if (strcmp(input, "help") == 0) {
            op_help(&stack);
            free(input);
            continue;
        }

        /* Record input if recording is active */
        if (recording_active) {
            /* Check if first word is a control command */
            char first_word[32];
            int j = 0;
            const char* p = input;
            while (*p && isspace(*p)) p++;  /* Skip leading whitespace */
            while (*p && !isspace(*p) && j < 31) {
                first_word[j++] = *p++;
            }
            first_word[j] = '\0';

            /* Only record if not a control command */
            if (strcmp(first_word, "rec") != 0 &&
                strcmp(first_word, "stop") != 0 &&
                strcmp(first_word, "save") != 0 &&
                strcmp(first_word, "save-midi") != 0 &&
                strcmp(first_word, "rec-midi") != 0 &&
                strcmp(first_word, "write-mid") != 0 &&
                strcmp(first_word, "read-mid") != 0 &&
                strcmp(first_word, "load") != 0) {
                recording_add_line(input);
            }
        }

        interpret(input);
        printf(" ok\n");

        free(input);
    }
}

/* Cleanup function */
static void cleanup(void) {
    midi_cleanup_observer();
    recording_clear();
    capture_clear();

    /* Free block storage */
    for (int i = 0; i < block_count; i++) {
        if (block_storage[i]) {
            free(block_storage[i]);
            block_storage[i] = NULL;
        }
    }

    /* Free user-defined word bodies */
    for (int i = 0; i < dict_count; i++) {
        if (!dictionary[i].is_primitive && dictionary[i].body) {
            free(dictionary[i].body);
            dictionary[i].body = NULL;
        }
    }
}

int main(int argc, char* argv[]) {
    const char* script_file = NULL;
    int show_help = 0;

    static struct option long_options[] = {
        {"script",   required_argument, 0, 's'},
        {"no-sleep", no_argument,       0, 'n'},
        {"help",     no_argument,       0, 'h'},
        {0, 0, 0, 0}
    };

    int opt;
    while ((opt = getopt_long(argc, argv, "s:nh", long_options, NULL)) != -1) {
        switch (opt) {
            case 's':
                script_file = optarg;
                break;
            case 'n':
                forth_set_no_sleep(1);
                break;
            case 'h':
                show_help = 1;
                break;
            default:
                print_usage(argv[0]);
                return 1;
        }
    }

    if (show_help) {
        print_usage(argv[0]);
        return 0;
    }

    /* Seed random number generator */
    srand((unsigned int)time(NULL));

    /* Initialize global context (includes stack, MIDI defaults, etc.) */
    forth_context_init(&g_ctx);

    /* Initialize dictionary with primitives */
    init_dictionary();

    /* Script mode: run file and exit */
    if (script_file) {
        int result = load_file(script_file);
        cleanup();
        return result == 0 ? 0 : 1;
    }

    /* Initialize readline autocomplete (only for interactive mode) */
    init_readline_completion();

    /* Load any additional files provided as arguments */
    for (int i = optind; i < argc; i++) {
        load_file(argv[i]);
    }

    /* Start interactive interpreter */
    interpreter_loop();

    /* Cleanup */
    cleanup();

    return 0;
}
