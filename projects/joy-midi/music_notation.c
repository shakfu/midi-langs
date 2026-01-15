/*
 * music_notation.c - Alda-like musical notation parser for Joy-MIDI
 */

#include "music_notation.h"
#include "music_context.h"
#include "midi_primitives.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>

/* Forward declarations for internal MIDI functions */
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

/* Play a single note using the MIDI subsystem */
static void play_note(JoyContext* ctx, int pitch, int velocity, int duration_ms) {
    /* Push pitch, velocity, duration and call midi-note */
    joy_stack_push(ctx->stack, joy_integer(pitch));
    joy_stack_push(ctx->stack, joy_integer(velocity));
    joy_stack_push(ctx->stack, joy_integer(duration_ms));
    midi_note_(ctx);
}

/* Play a chord using the MIDI subsystem */
static void play_chord(JoyContext* ctx, int* pitches, int count, int velocity, int duration_ms) {
    /* Build a list of pitches */
    JoyList* list = joy_list_new(count);
    for (int i = 0; i < count; i++) {
        joy_list_push(list, joy_integer(pitches[i]));
    }
    JoyValue list_val = {.type = JOY_LIST, .data.list = list};
    joy_stack_push(ctx->stack, list_val);
    joy_stack_push(ctx->stack, joy_integer(velocity));
    joy_stack_push(ctx->stack, joy_integer(duration_ms));
    midi_chord_(ctx);
}

/* Sleep for a duration (for rests) */
static void do_rest(int duration_ms) {
    if (duration_ms > 0) {
        usleep(duration_ms * 1000);
    }
}

/* ============================================================================
 * Parsing Functions
 * ============================================================================ */

/* Parse octave commands: o0-o9, >>, << (avoiding conflict with Joy's > < operators) */
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

/* Parse rest: r, r4, r8, etc. */
static bool parse_rest(JoyContext* ctx, MusicContext* mctx, const char* name) {
    if (name[0] != 'r') return false;

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

    /* Calculate actual duration with dots */
    int dur = mctx->duration_ms;
    for (int i = 0; i < dots; i++) {
        dur = dur * 3 / 2;
    }

    (void)ctx;  /* ctx not needed for rest */
    do_rest(dur);
    return true;
}

/* Parse dynamics: ppp, pp, p, mp, mf, f, ff, fff */
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

/* Parse note with optional duration/accidental/dot: c, c4, c+, c4., c+4. */
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

    /* Calculate actual duration with dots */
    int dur = mctx->duration_ms;
    for (int i = 0; i < dots; i++) {
        dur = dur * 3 / 2;  /* Each dot adds 50% */
    }

    /* Apply quantization */
    int play_dur = dur * mctx->quantization / 100;

    /* Play the note */
    play_note(ctx, pitch, mctx->velocity, play_dur);

    /* Wait remaining duration (gap between notes) */
    int rest_dur = dur - play_dur;
    if (rest_dur > 0) {
        usleep(rest_dur * 1000);
    }

    mctx->last_pitch = pitch;
    return true;
}

/* Parse slash chord notation: c/e/g */
static bool parse_slash_chord(JoyContext* ctx, MusicContext* mctx, const char* name) {
    /* Must contain at least one slash */
    if (!strchr(name, '/')) return false;

    /* Copy for tokenizing */
    char* copy = strdup(name);
    if (!copy) return false;

    int pitches[MUSIC_MAX_CHORD_NOTES];
    int count = 0;
    int duration = 0;
    int dots = 0;

    /* Parse each note separated by / */
    char* token = strtok(copy, "/");
    while (token && count < MUSIC_MAX_CHORD_NOTES) {
        const char* p = token;

        /* Note letter */
        int offset = note_offset(*p);
        if (offset < 0) {
            free(copy);
            return false;
        }
        p++;

        /* Accidentals */
        int accidental = 0;
        while (*p == '+' || *p == '-' || *p == '_') {
            if (*p == '+') accidental++;
            else if (*p == '-') accidental--;
            p++;
        }

        /* Duration (only from first note) */
        if (count == 0 && isdigit(*p)) {
            duration = atoi(p);
            while (isdigit(*p)) p++;
            mctx->duration_ms = music_duration_to_ms(duration, mctx->tempo);
        } else {
            /* Skip any duration on subsequent notes */
            while (isdigit(*p)) p++;
        }

        /* Dots (only from first note) */
        if (count == 0) {
            while (*p == '.') {
                dots++;
                p++;
            }
        }

        /* Calculate pitch */
        int pitch = (mctx->octave + 1) * 12 + offset + accidental;
        if (pitch < 0) pitch = 0;
        if (pitch > 127) pitch = 127;
        pitches[count++] = pitch;

        token = strtok(NULL, "/");
    }

    free(copy);

    if (count < 2) return false;  /* Need at least 2 notes for a chord */

    /* Calculate actual duration with dots */
    int dur = mctx->duration_ms;
    for (int i = 0; i < dots; i++) {
        dur = dur * 3 / 2;
    }

    /* Apply quantization */
    int play_dur = dur * mctx->quantization / 100;

    /* Play the chord */
    play_chord(ctx, pitches, count, mctx->velocity, play_dur);

    /* Wait remaining duration */
    int rest_dur = dur - play_dur;
    if (rest_dur > 0) {
        usleep(rest_dur * 1000);
    }

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

/* Parse named chord notation: c:maj, c:min7 */
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
    int dots = 0;
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

    /* Build chord pitches */
    int pitches[MUSIC_MAX_CHORD_NOTES];
    for (int i = 0; i < chord_size; i++) {
        int pitch = root + intervals[i];
        if (pitch < 0) pitch = 0;
        if (pitch > 127) pitch = 127;
        pitches[i] = pitch;
    }

    /* Calculate actual duration with dots */
    int dur = mctx->duration_ms;
    for (int i = 0; i < dots; i++) {
        dur = dur * 3 / 2;
    }

    /* Apply quantization */
    int play_dur = dur * mctx->quantization / 100;

    /* Play the chord */
    play_chord(ctx, pitches, chord_size, mctx->velocity, play_dur);

    /* Wait remaining duration */
    int rest_dur = dur - play_dur;
    if (rest_dur > 0) {
        usleep(rest_dur * 1000);
    }

    return true;
}

/* ============================================================================
 * Public API
 * ============================================================================ */

bool music_handle_symbol(JoyContext* ctx, const char* name) {
    MusicContext* mctx = (MusicContext*)ctx->user_data;
    if (!mctx) return false;

    /* 1. Octave commands: o0-o9, >, < */
    if (parse_octave_command(mctx, name)) return true;

    /* 2. Rest: r, r4, r8, etc. */
    if (parse_rest(ctx, mctx, name)) return true;

    /* 3. Dynamic: ppp, pp, p, mp, mf, f, ff, fff */
    if (parse_dynamic(mctx, name)) return true;

    /* 4. Named chord: c:maj, c:min7 */
    if (parse_named_chord(ctx, mctx, name)) return true;

    /* 5. Slash chord: c/e/g */
    if (parse_slash_chord(ctx, mctx, name)) return true;

    /* 6. Note: c, c4, c+, c4., c+4. */
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
