/*
 * music_notation.c - Alda-like musical notation parser for Joy-MIDI
 *
 * Notes push MIDI pitch integers onto the stack instead of playing directly.
 * Use 'play' to play notes sequentially or 'chord' to play simultaneously.
 *
 * Examples:
 *   c d e           - pushes 60, 62, 64 onto stack
 *   [c d e] play    - plays C D E sequentially
 *   [c e g] chord   - plays C major chord
 *   [c d e] reverse play - plays E D C
 *   [c d e] [7 +] map play - transpose up a fifth
 */

#include "music_notation.h"
#include "music_context.h"
#include "midi_primitives.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

/* Special value for rest */
#define REST_MARKER (-1)

/* Forward declarations for MIDI functions */
extern void midi_note_(JoyContext* ctx);
extern void midi_chord_(JoyContext* ctx);

/* ============================================================================
 * Internal Helpers
 * ============================================================================ */

/* Note offset from C within an octave */
static int note_offset(char note) {
    switch (tolower(note)) {
        case 'c': return 0;
        case 'd': return 2;
        case 'e': return 4;
        case 'f': return 5;
        case 'g': return 7;
        case 'a': return 9;
        case 'b': return 11;
        default: return -1;
    }
}

/* ============================================================================
 * Parsing Functions - Push data onto stack instead of playing
 * ============================================================================ */

/* Parse octave commands: o0-o9, >>, << */
static bool parse_octave_command(MusicContext* mctx, const char* name) {
    if (strcmp(name, ">>") == 0) {
        if (mctx->octave < 9) mctx->octave++;
        return true;
    }
    if (strcmp(name, "<<") == 0) {
        if (mctx->octave > 0) mctx->octave--;
        return true;
    }
    if (name[0] == 'o' && name[1] >= '0' && name[1] <= '9' && name[2] == '\0') {
        mctx->octave = name[1] - '0';
        return true;
    }
    return false;
}

/* Parse rest: r, r4, r8, etc. - pushes REST_MARKER */
static bool parse_rest(JoyContext* ctx, MusicContext* mctx, const char* name) {
    if (name[0] != 'r') return false;

    /* Check it's not a Joy word starting with 'r' */
    if (name[1] != '\0' && !isdigit(name[1]) && name[1] != '.') return false;

    const char* p = name + 1;
    int duration = 0;
    int dots = 0;

    /* Duration: 1, 2, 4, 8, 16, 32 */
    if (isdigit(*p)) {
        duration = atoi(p);
        while (isdigit(*p)) p++;
        mctx->duration_ms = music_duration_to_ms(duration, mctx->tempo);
    }

    /* Dots */
    while (*p == '.') {
        dots++;
        p++;
    }

    /* Must have consumed entire string */
    if (*p != '\0') return false;

    /* Calculate actual duration with dots (stored for play to use) */
    int dur = mctx->duration_ms;
    for (int i = 0; i < dots; i++) {
        dur = dur * 3 / 2;
    }
    mctx->duration_ms = dur;

    /* Push rest marker */
    joy_stack_push(ctx->stack, joy_integer(REST_MARKER));
    return true;
}

/* Parse dynamics: ppp, pp, p, mp, mf, f, ff, fff - sets velocity state */
static bool parse_dynamic(MusicContext* mctx, const char* name) {
    int velocity = -1;

    if (strcmp(name, "ppp") == 0) velocity = 16;
    else if (strcmp(name, "pp") == 0) velocity = 33;
    else if (strcmp(name, "p") == 0) velocity = 49;
    else if (strcmp(name, "mp") == 0) velocity = 64;
    else if (strcmp(name, "mf") == 0) velocity = 80;
    else if (strcmp(name, "f") == 0) velocity = 96;
    else if (strcmp(name, "ff") == 0) velocity = 112;
    else if (strcmp(name, "fff") == 0) velocity = 127;

    if (velocity >= 0) {
        mctx->velocity = velocity;
        return true;
    }
    return false;
}

/* Parse note: c, c4, c+, c4., c+4. - pushes MIDI pitch */
static bool parse_note(JoyContext* ctx, MusicContext* mctx, const char* name) {
    const char* p = name;
    int offset = -1;
    int accidental = 0;
    int duration = 0;
    int dots = 0;

    /* Note letter: c d e f g a b */
    offset = note_offset(*p);
    if (offset < 0) return false;
    p++;

    /* Accidentals: + (sharp), - (flat), _ (natural) */
    while (*p == '+' || *p == '-' || *p == '_') {
        if (*p == '+') accidental++;
        else if (*p == '-') accidental--;
        /* _ resets to natural (for future key signature support) */
        p++;
    }

    /* Duration: 1, 2, 4, 8, 16, 32 */
    if (isdigit(*p)) {
        duration = atoi(p);
        while (isdigit(*p)) p++;
        /* Update default duration */
        mctx->duration_ms = music_duration_to_ms(duration, mctx->tempo);
    }

    /* Dots: . or .. */
    while (*p == '.') {
        dots++;
        p++;
    }

    /* Must have consumed entire string */
    if (*p != '\0') return false;

    /* Calculate MIDI pitch */
    int pitch = (mctx->octave + 1) * 12 + offset + accidental;
    if (pitch < 0) pitch = 0;
    if (pitch > 127) pitch = 127;

    /* Update duration with dots */
    if (dots > 0) {
        int dur = mctx->duration_ms;
        for (int i = 0; i < dots; i++) {
            dur = dur * 3 / 2;
        }
        mctx->duration_ms = dur;
    }

    /* Push the pitch onto the stack */
    joy_stack_push(ctx->stack, joy_integer(pitch));

    mctx->last_pitch = pitch;
    return true;
}

/* Chord intervals for named chords */
static const int CHORD_MAJ[] = {0, 4, 7};
static const int CHORD_MIN[] = {0, 3, 7};
static const int CHORD_DIM[] = {0, 3, 6};
static const int CHORD_AUG[] = {0, 4, 8};
static const int CHORD_DOM7[] = {0, 4, 7, 10};
static const int CHORD_MAJ7[] = {0, 4, 7, 11};
static const int CHORD_MIN7[] = {0, 3, 7, 10};
static const int CHORD_DIM7[] = {0, 3, 6, 9};

/* Parse named chord: c:maj, c:min7 - pushes list of pitches */
static bool parse_named_chord(JoyContext* ctx, MusicContext* mctx, const char* name) {
    /* Must contain a colon */
    const char* colon = strchr(name, ':');
    if (!colon) return false;

    /* Get root note */
    const char* p = name;
    int offset = note_offset(*p);
    if (offset < 0) return false;
    p++;

    /* Accidentals */
    int accidental = 0;
    while (*p == '+' || *p == '-' || *p == '_') {
        if (*p == '+') accidental++;
        else if (*p == '-') accidental--;
        p++;
    }

    /* Duration before colon */
    int duration = 0;
    if (isdigit(*p)) {
        duration = atoi(p);
        while (isdigit(*p)) p++;
        mctx->duration_ms = music_duration_to_ms(duration, mctx->tempo);
    }

    /* Should be at colon now */
    if (*p != ':') return false;
    p++;  /* Skip colon */

    /* Parse chord type */
    const int* intervals = NULL;
    int chord_size = 0;

    if (strcmp(p, "maj") == 0) {
        intervals = CHORD_MAJ; chord_size = 3;
    } else if (strcmp(p, "min") == 0 || strcmp(p, "m") == 0) {
        intervals = CHORD_MIN; chord_size = 3;
    } else if (strcmp(p, "dim") == 0) {
        intervals = CHORD_DIM; chord_size = 3;
    } else if (strcmp(p, "aug") == 0) {
        intervals = CHORD_AUG; chord_size = 3;
    } else if (strcmp(p, "7") == 0 || strcmp(p, "dom7") == 0) {
        intervals = CHORD_DOM7; chord_size = 4;
    } else if (strcmp(p, "maj7") == 0) {
        intervals = CHORD_MAJ7; chord_size = 4;
    } else if (strcmp(p, "min7") == 0 || strcmp(p, "m7") == 0) {
        intervals = CHORD_MIN7; chord_size = 4;
    } else if (strcmp(p, "dim7") == 0) {
        intervals = CHORD_DIM7; chord_size = 4;
    } else {
        return false;
    }

    /* Calculate root pitch */
    int root = (mctx->octave + 1) * 12 + offset + accidental;

    /* Build list of pitches */
    JoyList* list = joy_list_new(chord_size);
    for (int i = 0; i < chord_size; i++) {
        int pitch = root + intervals[i];
        if (pitch < 0) pitch = 0;
        if (pitch > 127) pitch = 127;
        joy_list_push(list, joy_integer(pitch));
    }

    /* Push the list onto the stack */
    JoyValue list_val = {.type = JOY_LIST, .data.list = list};
    joy_stack_push(ctx->stack, list_val);

    return true;
}

/* ============================================================================
 * Play and Chord Primitives
 * ============================================================================ */

/* Helper: play a single note with current settings */
static void play_single_note(MusicContext* mctx, int pitch) {
    if (pitch == REST_MARKER) {
        /* Rest: just wait */
        usleep(mctx->duration_ms * 1000);
        return;
    }

    /* Apply quantization */
    int play_dur = mctx->duration_ms * mctx->quantization / 100;
    int rest_dur = mctx->duration_ms - play_dur;

    /* Use midi_primitives functions directly */
    extern void send_note_on(int pitch, int velocity);
    extern void send_note_off(int pitch);

    send_note_on(pitch, mctx->velocity);
    if (play_dur > 0) {
        usleep(play_dur * 1000);
    }
    send_note_off(pitch);

    if (rest_dur > 0) {
        usleep(rest_dur * 1000);
    }
}

/* play - pop and play notes sequentially */
void music_play_(JoyContext* ctx) {
    if (ctx->stack->depth < 1) {
        joy_error_underflow("play", 1, ctx->stack->depth);
        return;
    }

    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (!mctx) {
        joy_error("play: no music context");
        return;
    }

    JoyValue val = joy_stack_pop(ctx->stack);

    if (val.type == JOY_INTEGER) {
        /* Single note */
        play_single_note(mctx, (int)val.data.integer);
    } else if (val.type == JOY_LIST) {
        /* List of notes - play sequentially */
        JoyList* list = val.data.list;
        for (size_t i = 0; i < list->length; i++) {
            if (list->items[i].type == JOY_INTEGER) {
                play_single_note(mctx, (int)list->items[i].data.integer);
            }
        }
        joy_value_free(&val);
    } else if (val.type == JOY_QUOTATION) {
        /* Quotation - execute it first to get notes, then play them */
        /* For now, just execute and let notes accumulate, then play from stack */
        joy_execute_quotation(ctx, val.data.quotation);
        joy_value_free(&val);
        /* Play whatever is now on the stack */
        while (ctx->stack->depth > 0) {
            JoyValue note = joy_stack_pop(ctx->stack);
            if (note.type == JOY_INTEGER) {
                play_single_note(mctx, (int)note.data.integer);
            } else {
                joy_value_free(&note);
                break;  /* Stop if we hit a non-integer */
            }
        }
    } else {
        joy_value_free(&val);
        joy_error("play: expected integer, list, or quotation");
    }
}

/* chord - pop and play notes simultaneously */
void music_chord_(JoyContext* ctx) {
    if (ctx->stack->depth < 1) {
        joy_error_underflow("chord", 1, ctx->stack->depth);
        return;
    }

    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (!mctx) {
        joy_error("chord: no music context");
        return;
    }

    JoyValue val = joy_stack_pop(ctx->stack);

    extern void send_note_on(int pitch, int velocity);
    extern void send_note_off(int pitch);

    if (val.type == JOY_INTEGER) {
        /* Single note - just play it */
        play_single_note(mctx, (int)val.data.integer);
    } else if (val.type == JOY_LIST) {
        /* List of notes - play simultaneously */
        JoyList* list = val.data.list;
        int pitches[16];
        int count = 0;

        /* Collect pitches */
        for (size_t i = 0; i < list->length && count < 16; i++) {
            if (list->items[i].type == JOY_INTEGER) {
                int p = (int)list->items[i].data.integer;
                if (p != REST_MARKER) {
                    pitches[count++] = p;
                }
            }
        }

        /* Note on for all */
        for (int i = 0; i < count; i++) {
            send_note_on(pitches[i], mctx->velocity);
        }

        /* Wait for duration */
        int play_dur = mctx->duration_ms * mctx->quantization / 100;
        if (play_dur > 0) {
            usleep(play_dur * 1000);
        }

        /* Note off for all */
        for (int i = 0; i < count; i++) {
            send_note_off(pitches[i]);
        }

        /* Rest gap */
        int rest_dur = mctx->duration_ms - play_dur;
        if (rest_dur > 0) {
            usleep(rest_dur * 1000);
        }

        joy_value_free(&val);
    } else {
        joy_value_free(&val);
        joy_error("chord: expected integer or list");
    }
}

/* notes - execute quotation/list and collect results into a list */
void music_notes_(JoyContext* ctx) {
    if (ctx->stack->depth < 1) {
        joy_error_underflow("notes", 1, ctx->stack->depth);
        return;
    }

    JoyValue val = joy_stack_pop(ctx->stack);

    /* Record stack depth before execution */
    size_t depth_before = ctx->stack->depth;

    /* Execute based on type */
    if (val.type == JOY_QUOTATION) {
        joy_execute_quotation(ctx, val.data.quotation);
        joy_value_free(&val);
    } else if (val.type == JOY_LIST) {
        /* Execute each item in the list */
        JoyList* list = val.data.list;
        for (size_t i = 0; i < list->length; i++) {
            joy_execute_value(ctx, joy_value_copy(list->items[i]));
        }
        joy_value_free(&val);
    } else {
        joy_value_free(&val);
        joy_error("notes: expected list or quotation");
        return;
    }

    /* Collect new values into a list */
    size_t new_count = ctx->stack->depth - depth_before;
    JoyList* list = joy_list_new(new_count > 0 ? new_count : 1);

    /* Pop new values in reverse order and prepend to build correct order */
    for (size_t i = 0; i < new_count; i++) {
        JoyValue item = joy_stack_pop(ctx->stack);
        /* Insert at beginning to maintain order */
        joy_list_push(list, item);
    }

    /* Reverse the list to get correct order (first pushed = first in list) */
    for (size_t i = 0; i < list->length / 2; i++) {
        JoyValue tmp = list->items[i];
        list->items[i] = list->items[list->length - 1 - i];
        list->items[list->length - 1 - i] = tmp;
    }

    /* Push the list */
    JoyValue list_val = {.type = JOY_LIST, .data.list = list};
    joy_stack_push(ctx->stack, list_val);
}

/* ============================================================================
 * Public API
 * ============================================================================ */

bool music_handle_symbol(JoyContext* ctx, const char* name) {
    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (!mctx) return false;

    /* 1. Octave commands: o0-o9, >>, << (modify state, push nothing) */
    if (parse_octave_command(mctx, name)) return true;

    /* 2. Dynamic: ppp, pp, p, mp, mf, f, ff, fff (modify state, push nothing) */
    if (parse_dynamic(mctx, name)) return true;

    /* 3. Rest: r, r4, r8, etc. (push REST_MARKER) */
    if (parse_rest(ctx, mctx, name)) return true;

    /* 4. Named chord: c:maj, c:min7 (push list of pitches) */
    if (parse_named_chord(ctx, mctx, name)) return true;

    /* 5. Note: c, c4, c+, c4., c+4. (push MIDI pitch) */
    if (parse_note(ctx, mctx, name)) return true;

    return false;  /* Not music notation */
}

void music_notation_init(JoyContext* ctx) {
    /* Create music context */
    MusicContext* mctx = music_context_new();
    if (!mctx) {
        fprintf(stderr, "Failed to create music context\n");
        return;
    }

    /* Store in Joy context */
    ctx->user_data = mctx;
    ctx->undef_handler = music_handle_symbol;
}

void music_notation_cleanup(JoyContext* ctx) {
    if (ctx->user_data) {
        music_context_free((MusicContext*)ctx->user_data);
        ctx->user_data = NULL;
    }
    ctx->undef_handler = NULL;
}

MusicContext* music_get_context(JoyContext* ctx) {
    return (MusicContext*)ctx->user_data;
}
