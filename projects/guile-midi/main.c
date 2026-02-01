/*
 * guile_midi - Scheme-based MIDI language using GNU Guile
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <libguile.h>

#ifdef USE_READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

/* External functions from midi_module.c */
extern void guile_midi_init(void);
extern void guile_midi_cleanup(void);

#ifdef USE_READLINE
/* ============================================================================
 * Readline autocomplete
 * ============================================================================ */

/* MIDI functions */
static const char* midi_functions[] = {
    "midi-list-ports", "midi-open", "midi-close", "midi-open?", "midi-out?",
    "midi-note", "midi-note-on", "midi-note-off", "midi-chord",
    "midi-cc", "midi-program", "midi-all-notes-off", "midi-sleep",
    "midi-pitch-bend",
    NULL
};

/* Scheme core functions */
static const char* scheme_keywords[] = {
    "define", "lambda", "let", "let*", "letrec", "if", "cond", "case",
    "begin", "set!", "quote", "quasiquote", "unquote", "and", "or", "not",
    "car", "cdr", "cons", "list", "append", "map", "for-each", "apply",
    "length", "reverse", "null?", "pair?", "list?", "number?", "string?",
    "symbol?", "boolean?", "procedure?", "eq?", "eqv?", "equal?",
    "+", "-", "*", "/", "=", "<", ">", "<=", ">=",
    "floor", "ceiling", "round", "truncate", "modulo", "remainder",
    "abs", "min", "max", "sqrt", "expt", "sin", "cos", "tan", "random",
    "display", "newline", "write", "read", "load", "error",
    NULL
};

/* Prelude functions and values */
static const char* prelude_functions[] = {
    /* Convenience functions */
    "note", "help", "open", "close", "n", "ch", "arp", "rest",
    "play-notes", "set-tempo!", "get-tempo", "bpm", "dotted",
    /* Chord builders */
    "major", "minor", "dim", "aug", "dom7", "maj7", "min7",
    /* Scale functions */
    "build-scale", "scale-degree", "in-scale?", "quantize-to-scale",
    "get-scale", "scale", "degree", "in-scale-named?", "quantize",
    /* Transpose */
    "transpose", "octave-up", "octave-down",
    /* Arpeggio */
    "midi-arpeggio", "cents-to-note",
    /* Scheduler */
    "spawn", "run", "poll", "stop", "voices", "scheduler-status",
    NULL
};

/* Dynamics */
static const char* dynamics[] = {
    "ppp", "pp", "p", "mp", "mf", "f", "ff", "fff",
    NULL
};

/* Durations */
static const char* durations[] = {
    "whole", "half", "quarter", "eighth", "sixteenth",
    NULL
};

/* Pitch names (c0-c8 with sharps and flats) */
static const char* pitch_names[] = {
    "c0", "cs0", "db0", "d0", "ds0", "eb0", "e0", "f0", "fs0", "gb0", "g0", "gs0", "ab0", "a0", "as0", "bb0", "b0",
    "c1", "cs1", "db1", "d1", "ds1", "eb1", "e1", "f1", "fs1", "gb1", "g1", "gs1", "ab1", "a1", "as1", "bb1", "b1",
    "c2", "cs2", "db2", "d2", "ds2", "eb2", "e2", "f2", "fs2", "gb2", "g2", "gs2", "ab2", "a2", "as2", "bb2", "b2",
    "c3", "cs3", "db3", "d3", "ds3", "eb3", "e3", "f3", "fs3", "gb3", "g3", "gs3", "ab3", "a3", "as3", "bb3", "b3",
    "c4", "cs4", "db4", "d4", "ds4", "eb4", "e4", "f4", "fs4", "gb4", "g4", "gs4", "ab4", "a4", "as4", "bb4", "b4",
    "c5", "cs5", "db5", "d5", "ds5", "eb5", "e5", "f5", "fs5", "gb5", "g5", "gs5", "ab5", "a5", "as5", "bb5", "b5",
    "c6", "cs6", "db6", "d6", "ds6", "eb6", "e6", "f6", "fs6", "gb6", "g6", "gs6", "ab6", "a6", "as6", "bb6", "b6",
    "c7", "cs7", "db7", "d7", "ds7", "eb7", "e7", "f7", "fs7", "gb7", "g7", "gs7", "ab7", "a7", "as7", "bb7", "b7",
    "c8", "cs8", "db8", "d8", "ds8", "eb8", "e8", "f8", "fs8", "gb8", "g8", "gs8", "ab8", "a8", "as8", "bb8", "b8",
    NULL
};

/* Scale definitions */
static const char* scale_names[] = {
    "scale-major", "scale-ionian", "scale-dorian", "scale-phrygian",
    "scale-lydian", "scale-mixolydian", "scale-minor", "scale-aeolian",
    "scale-locrian", "scale-harmonic-minor", "scale-melodic-minor",
    "scale-pentatonic", "scale-pentatonic-major", "scale-pentatonic-minor",
    "scale-blues", "scale-whole-tone", "scale-chromatic",
    "scale-diminished-hw", "scale-diminished-wh", "scale-augmented",
    "scale-bebop-dominant", "scale-bebop-major", "scale-bebop-minor",
    "scale-hungarian-minor", "scale-double-harmonic",
    "scale-neapolitan-major", "scale-neapolitan-minor",
    "scale-phrygian-dominant", "scale-persian", "scale-altered", "scale-enigmatic",
    "scale-hirajoshi", "scale-in-sen", "scale-iwato", "scale-kumoi",
    "scale-egyptian", "scale-romanian-minor", "scale-spanish-8-tone",
    "scale-maqam-hijaz", "scale-maqam-nahawand", "scale-maqam-nikriz",
    "scale-raga-bhairav", "scale-raga-todi", "scale-raga-marwa",
    NULL
};

/* Generator function for readline completion */
static char* completion_generator(const char* text, int state) {
    static int midi_idx, scheme_idx, prelude_idx, dyn_idx, dur_idx, pitch_idx, scale_idx;
    static size_t len;

    if (state == 0) {
        midi_idx = 0;
        scheme_idx = 0;
        prelude_idx = 0;
        dyn_idx = 0;
        dur_idx = 0;
        pitch_idx = 0;
        scale_idx = 0;
        len = strlen(text);
    }

    /* Search MIDI functions */
    while (midi_functions[midi_idx] != NULL) {
        const char* name = midi_functions[midi_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search Scheme keywords */
    while (scheme_keywords[scheme_idx] != NULL) {
        const char* name = scheme_keywords[scheme_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search prelude functions */
    while (prelude_functions[prelude_idx] != NULL) {
        const char* name = prelude_functions[prelude_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search dynamics */
    while (dynamics[dyn_idx] != NULL) {
        const char* name = dynamics[dyn_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search durations */
    while (durations[dur_idx] != NULL) {
        const char* name = durations[dur_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search pitch names */
    while (pitch_names[pitch_idx] != NULL) {
        const char* name = pitch_names[pitch_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    /* Search scale names */
    while (scale_names[scale_idx] != NULL) {
        const char* name = scale_names[scale_idx++];
        if (strncmp(name, text, len) == 0) {
            return strdup(name);
        }
    }

    return NULL;
}

/* Custom completion function */
static char** guile_completion(const char* text, int start, int end) {
    (void)start;
    (void)end;

    /* Disable default filename completion */
    rl_attempted_completion_over = 1;

    return rl_completion_matches(text, completion_generator);
}

/* Initialize readline completion */
static void init_readline_completion(void) {
    rl_attempted_completion_function = guile_completion;
    /* Scheme uses parens and spaces as word boundaries */
    rl_completer_word_break_characters = " \t\n()[]'`,";
}
#endif /* USE_READLINE */

static void print_usage(const char *prog) {
    fprintf(stderr, "Usage: %s [options] [file.scm]\n", prog);
    fprintf(stderr, "Options:\n");
    fprintf(stderr, "  -e EXPR    Evaluate expression and print result\n");
    fprintf(stderr, "  --version  Show version\n");
    fprintf(stderr, "  --help     Show this help\n");
    fprintf(stderr, "\nWithout arguments, starts an interactive REPL.\n");
}

/* Arguments passed to inner_main */
typedef struct {
    int argc;
    char **argv;
} MainArgs;

/* Inner main function called via scm_with_guile */
static void* inner_main(void *data) {
    MainArgs *args = (MainArgs *)data;
    int argc = args->argc;
    char **argv = args->argv;

    /* Initialize MIDI module */
    guile_midi_init();

    /* Process arguments */
    if (argc >= 2) {
        for (int i = 1; i < argc; i++) {
            if (strcmp(argv[i], "-e") == 0) {
                /* Evaluate expression */
                if (i + 1 >= argc) {
                    fprintf(stderr, "Error: -e requires an expression\n");
                    guile_midi_cleanup();
                    return (void*)1;
                }
                SCM result = scm_c_eval_string(argv[++i]);
                if (!scm_is_eq(result, SCM_UNSPECIFIED)) {
                    scm_display(result, scm_current_output_port());
                    scm_newline(scm_current_output_port());
                }
            } else if (strcmp(argv[i], "--version") == 0) {
                printf("guile_midi using Guile %s\n", scm_to_locale_string(scm_version()));
            } else if (strcmp(argv[i], "--help") == 0 || strcmp(argv[i], "-h") == 0) {
                print_usage(argv[0]);
            } else {
                /* Load file */
                scm_c_primitive_load(argv[i]);
            }
        }
    } else {
        /* Interactive REPL */
        printf("guile_midi - Scheme MIDI language (GNU Guile)\n");
        printf("Type (help) for available functions, (quit) to exit\n\n");

#ifdef USE_READLINE
        init_readline_completion();

        char *input;
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

            /* Check for quit */
            if (strncmp(input, "(quit)", 6) == 0 ||
                strncmp(input, "(exit)", 6) == 0) {
                free(input);
                break;
            }

            /* Evaluate and print result */
            SCM result = scm_c_eval_string(input);
            if (!scm_is_eq(result, SCM_UNSPECIFIED)) {
                scm_display(result, scm_current_output_port());
                scm_newline(scm_current_output_port());
            }

            free(input);
        }
#else
        /* Fallback REPL without readline */
        char buffer[4096];
        while (1) {
            printf("> ");
            fflush(stdout);

            if (!fgets(buffer, sizeof(buffer), stdin)) {
                break;  /* EOF or error */
            }

            /* Skip empty lines */
            if (buffer[0] == '\n' || buffer[0] == '\0') {
                continue;
            }

            /* Check for quit */
            if (strncmp(buffer, "(quit)", 6) == 0 ||
                strncmp(buffer, "(exit)", 6) == 0) {
                break;
            }

            /* Evaluate and print result */
            SCM result = scm_c_eval_string(buffer);
            if (!scm_is_eq(result, SCM_UNSPECIFIED)) {
                scm_display(result, scm_current_output_port());
                scm_newline(scm_current_output_port());
            }
        }
#endif
    }

    /* Cleanup */
    guile_midi_cleanup();

    return NULL;
}

int main(int argc, char **argv) {
    MainArgs args = { argc, argv };

    /* Initialize Guile and run inner_main */
    scm_with_guile(inner_main, &args);

    return 0;
}
