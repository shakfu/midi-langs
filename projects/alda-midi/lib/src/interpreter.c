/**
 * @file interpreter.c
 * @brief Alda AST interpreter - walks AST and generates MIDI events.
 */

#include "alda/interpreter.h"
#include "alda/scheduler.h"
#include "alda/midi_backend.h"
#include "alda/instruments.h"
#include "alda/parser.h"
#include "finalize.h"
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <ctype.h>

/* Forward declarations for visitor functions */
static int visit_node(AldaContext* ctx, AldaNode* node);
static int visit_root(AldaContext* ctx, AldaNode* node);
static int visit_part_decl(AldaContext* ctx, AldaNode* node);
static int visit_event_seq(AldaContext* ctx, AldaNode* node);
static int visit_note(AldaContext* ctx, AldaNode* node);
static int visit_rest(AldaContext* ctx, AldaNode* node);
static int visit_chord(AldaContext* ctx, AldaNode* node);
static int visit_octave_set(AldaContext* ctx, AldaNode* node);
static int visit_octave_up(AldaContext* ctx, AldaNode* node);
static int visit_octave_down(AldaContext* ctx, AldaNode* node);
static int visit_lisp_list(AldaContext* ctx, AldaNode* node);
static int visit_repeat(AldaContext* ctx, AldaNode* node);
static int visit_bracket_seq(AldaContext* ctx, AldaNode* node);
static int visit_voice_group(AldaContext* ctx, AldaNode* node);
static int visit_voice(AldaContext* ctx, AldaNode* node);
static int visit_var_def(AldaContext* ctx, AldaNode* node);
static int visit_var_ref(AldaContext* ctx, AldaNode* node);
static int visit_cram(AldaContext* ctx, AldaNode* node);
static int visit_marker(AldaContext* ctx, AldaNode* node);
static int visit_at_marker(AldaContext* ctx, AldaNode* node);
static int visit_on_reps(AldaContext* ctx, AldaNode* node);

/* ============================================================================
 * Pitch Calculation
 * ============================================================================ */

/* Note letter to semitone offset (C=0, D=2, E=4, F=5, G=7, A=9, B=11) */
static const int NOTE_OFFSETS[] = {
    9,  /* A */
    11, /* B */
    0,  /* C */
    2,  /* D */
    4,  /* E */
    5,  /* F */
    7   /* G */
};

int alda_calculate_pitch(char letter, const char* accidentals, int octave, const int* key_sig) {
    /* Validate letter */
    int c = tolower((unsigned char)letter);
    if (c < 'a' || c > 'g') {
        return -1;
    }

    /* Get base semitone offset */
    int semitone = NOTE_OFFSETS[c - 'a'];

    /* Check for explicit accidentals or natural sign */
    int has_explicit_accidental = 0;
    int has_natural = 0;
    int accidental_offset = 0;

    if (accidentals) {
        for (const char* p = accidentals; *p; p++) {
            if (*p == '+') {
                accidental_offset++;
                has_explicit_accidental = 1;
            } else if (*p == '-') {
                accidental_offset--;
                has_explicit_accidental = 1;
            } else if (*p == '_') {
                has_natural = 1;  /* Natural sign overrides key signature */
            }
        }
    }

    /* Apply accidentals: explicit > natural > key signature */
    if (has_explicit_accidental) {
        semitone += accidental_offset;
    } else if (!has_natural && key_sig) {
        /* Apply key signature (C=0, D=1, E=2, F=3, G=4, A=5, B=6) */
        /* Map letter to key_sig index */
        static const int LETTER_TO_INDEX[] = {
            5,  /* a -> index 5 (A) */
            6,  /* b -> index 6 (B) */
            0,  /* c -> index 0 (C) */
            1,  /* d -> index 1 (D) */
            2,  /* e -> index 2 (E) */
            3,  /* f -> index 3 (F) */
            4   /* g -> index 4 (G) */
        };
        int idx = LETTER_TO_INDEX[c - 'a'];
        semitone += key_sig[idx];
    }
    /* If has_natural and no explicit accidental, keep semitone as-is (natural) */

    /* Calculate MIDI pitch: C4 = 60 */
    /* octave 4, C = 0 -> 60 */
    /* Formula: (octave + 1) * 12 + semitone */
    int pitch = (octave + 1) * 12 + semitone;

    /* Clamp to valid MIDI range */
    if (pitch < 0) pitch = 0;
    if (pitch > 127) pitch = 127;

    return pitch;
}

/* ============================================================================
 * Duration Calculation from AST
 * ============================================================================ */

/* A duration as Alda keeps it: beats (quarter note = 1) plus milliseconds.
 * A tie such as c4~500ms has both. */
typedef struct {
    double beats;
    double ms;
} AldaLength;

static AldaLength component_length(AldaNode* comp) {
    AldaLength len = {0.0, 0.0};
    switch (comp->type) {
        case ALDA_NODE_NOTE_LENGTH: {
            double denom = comp->data.note_length.denominator;
            if (!(denom > 0.0)) denom = 4.0;
            double beats = 4.0 / denom;
            double add = beats;
            for (int d = 0; d < comp->data.note_length.dots; d++) {
                add /= 2.0;
                beats += add;
            }
            len.beats = beats;
            break;
        }
        case ALDA_NODE_NOTE_LENGTH_MS:
            len.ms = comp->data.note_length_ms.ms;
            break;
        case ALDA_NODE_NOTE_LENGTH_S:
            len.ms = comp->data.note_length_s.seconds * 1000.0;
            break;
        default:
            break;
    }
    return len;
}

/* The length a duration node gives, or the default when it is absent. */
static AldaLength resolve_length(double default_beats, double default_ms,
                                 AldaNode* duration) {
    AldaLength len = {default_beats, default_ms};
    if (!duration) return len;

    len.beats = 0.0;
    len.ms = 0.0;
    if (duration->type == ALDA_NODE_DURATION) {
        for (AldaNode* c = duration->data.duration.components; c; c = c->next) {
            AldaLength part = component_length(c);
            len.beats += part.beats;
            len.ms += part.ms;
        }
    } else {
        len = component_length(duration);
    }
    return len;
}

static double length_seconds(AldaLength len, int tempo) {
    if (tempo <= 0) tempo = ALDA_DEFAULT_TEMPO;
    return len.beats * 60.0 / tempo + len.ms / 1000.0;
}

/* A note or rest with a duration sets the default for what follows, the whole
 * tied length included (Alda's updateDefaultDuration). */
static AldaLength take_length(AldaPartState* part, AldaNode* duration) {
    AldaLength len = resolve_length(part->default_beats, part->default_ms, duration);
    if (duration) {
        part->default_beats = len.beats;
        part->default_ms = len.ms;
    }
    return len;
}

/* Seconds a note or rest of this length lasts in this part, cram scaling
 * included. */
static double part_seconds(AldaContext* ctx, AldaPartState* part, AldaLength len) {
    return length_seconds(len, alda_effective_tempo(ctx, part)) * part->time_scale;
}

int alda_ast_duration_to_ticks(AldaContext* ctx, AldaPartState* part, AldaNode* duration) {
    AldaLength len = resolve_length(part->default_beats, part->default_ms, duration);
    int tempo = alda_effective_tempo(ctx, part);
    double ticks = len.beats * ALDA_TICKS_PER_QUARTER
                 + len.ms / 1000.0 * tempo / 60.0 * ALDA_TICKS_PER_QUARTER;
    return (int)(ticks + 0.5);
}

/* ============================================================================
 * AST Visitor Functions
 * ============================================================================ */

static int visit_node(AldaContext* ctx, AldaNode* node) {
    if (!node) return 0;

    switch (node->type) {
        case ALDA_NODE_ROOT:
            return visit_root(ctx, node);

        case ALDA_NODE_PART_DECL:
            return visit_part_decl(ctx, node);

        case ALDA_NODE_EVENT_SEQ:
            return visit_event_seq(ctx, node);

        case ALDA_NODE_NOTE:
            return visit_note(ctx, node);

        case ALDA_NODE_REST:
            return visit_rest(ctx, node);

        case ALDA_NODE_CHORD:
            return visit_chord(ctx, node);

        case ALDA_NODE_OCTAVE_SET:
            return visit_octave_set(ctx, node);

        case ALDA_NODE_OCTAVE_UP:
            return visit_octave_up(ctx, node);

        case ALDA_NODE_OCTAVE_DOWN:
            return visit_octave_down(ctx, node);

        case ALDA_NODE_LISP_LIST:
            return visit_lisp_list(ctx, node);

        case ALDA_NODE_REPEAT:
            return visit_repeat(ctx, node);

        case ALDA_NODE_BRACKET_SEQ:
            return visit_bracket_seq(ctx, node);

        case ALDA_NODE_VOICE_GROUP:
            return visit_voice_group(ctx, node);

        case ALDA_NODE_VOICE:
            return visit_voice(ctx, node);

        case ALDA_NODE_BARLINE:
            /* Barlines are visual-only, no action needed */
            return 0;

        case ALDA_NODE_VAR_DEF:
            return visit_var_def(ctx, node);

        case ALDA_NODE_VAR_REF:
            return visit_var_ref(ctx, node);

        case ALDA_NODE_CRAM:
            return visit_cram(ctx, node);

        case ALDA_NODE_MARKER:
            return visit_marker(ctx, node);

        case ALDA_NODE_AT_MARKER:
            return visit_at_marker(ctx, node);

        case ALDA_NODE_ON_REPS:
            return visit_on_reps(ctx, node);

        default:
            return 0;
    }
}

static int visit_list(AldaContext* ctx, AldaNode* node) {
    for (; node; node = node->next) {
        if (visit_node(ctx, node) < 0) return -1;
    }
    return 0;
}

static int visit_root(AldaContext* ctx, AldaNode* node) {
    return visit_list(ctx, node->data.root.children);
}

static int visit_part_decl(AldaContext* ctx, AldaNode* node) {
    /* Set current parts from declaration */
    char** names = node->data.part_decl.names;
    int count = (int)node->data.part_decl.name_count;

    if (alda_set_current_parts_aliased(ctx, names, count,
                                       node->data.part_decl.alias) < 0) {
        return -1;
    }

    /* Apply the alias to every part in the declaration. For a group such as
     * violin/viola/cello "strings" the alias names the whole group, so each
     * member carries it - that is what lets "strings.cello" resolve later. */
    if (node->data.part_decl.alias) {
        for (int i = 0; i < ctx->current_part_count; i++) {
            int idx = ctx->current_part_indices[i];
            if (idx < 0 || idx >= ctx->part_count) continue;
            AldaPartState* part = &ctx->parts[idx];
            strncpy(part->alias, node->data.part_decl.alias,
                    sizeof(part->alias) - 1);
            part->alias[sizeof(part->alias) - 1] = '\0';
        }
    }

    /* Program changes are sent with each part's notes, once channels are
     * known (alda_events_finalize). */
    return 0;
}

static int visit_event_seq(AldaContext* ctx, AldaNode* node) {
    return visit_list(ctx, node->data.event_seq.events);
}

/* The MIDI pitch of a note in this part, or -1 for an invalid letter. */
static int note_pitch(AldaPartState* p, AldaNode* note) {
    int pitch = alda_calculate_pitch(note->data.note.letter,
                                     note->data.note.accidentals,
                                     p->octave, p->key_signature);
    if (pitch < 0) return -1;
    pitch += p->transpose;
    if (pitch < 0) pitch = 0;
    if (pitch > 127) pitch = 127;
    return pitch;
}

/* Sound a note at the part's position and return its length in seconds,
 * without advancing the position. */
static double play_note(AldaContext* ctx, AldaPartState* p, AldaNode* node, int pitch) {
    AldaLength len = take_length(p, node->data.note.duration);
    double seconds = part_seconds(ctx, p, len);

    /* Slurred notes sound their full length */
    double sounding = node->data.note.slurred
                    ? seconds
                    : seconds * alda_effective_quant(ctx, p) / 100.0;

    alda_record_note(ctx, p, p->current_time, sounding, pitch,
                     alda_effective_velocity(ctx, p));
    return seconds;
}

static int visit_note(AldaContext* ctx, AldaNode* node) {
    if (!alda_current_part(ctx)) {
        fprintf(stderr, "Error: No current part for note\n");
        return -1;
    }

    /* Set source line for event tracking */
    ALDA_SET_SOURCE_LINE(ctx, node->pos.line);

    /* Everything below is resolved per part. When a group is active - say
     * "guitar/sax:" - its members can differ in octave, key signature,
     * transposition, dynamics, tempo and default duration. */
    for (int i = 0; i < ctx->current_part_count; i++) {
        AldaPartState* p = &ctx->parts[ctx->current_part_indices[i]];

        int pitch = note_pitch(p, node);
        if (pitch < 0) {
            fprintf(stderr, "Error: Invalid note\n");
            return -1;
        }
        p->current_time += play_note(ctx, p, node, pitch);
    }

    return 0;
}

static int visit_rest(AldaContext* ctx, AldaNode* node) {
    if (!alda_current_part(ctx)) {
        fprintf(stderr, "Error: No current part for rest\n");
        return -1;
    }

    /* A rest with a duration sets the default, as a note does. */
    for (int i = 0; i < ctx->current_part_count; i++) {
        AldaPartState* p = &ctx->parts[ctx->current_part_indices[i]];
        p->current_time += part_seconds(ctx, p, take_length(p, node->data.rest.duration));
    }

    return 0;
}

static int visit_chord(AldaContext* ctx, AldaNode* node) {
    if (!alda_current_part(ctx)) {
        fprintf(stderr, "Error: No current part for chord\n");
        return -1;
    }

    /* Set source line for event tracking */
    ALDA_SET_SOURCE_LINE(ctx, node->pos.line);

    /* Each note keeps its own duration and sets the default for the next, and
     * the part moves on after the shortest note or rest in the chord
     * (alda-language/chords.md). Resolved per part: octave changes inside a
     * chord, tempo and default duration all belong to the individual part. */
    for (int i = 0; i < ctx->current_part_count; i++) {
        AldaPartState* p = &ctx->parts[ctx->current_part_indices[i]];
        double shortest = -1.0;

        for (AldaNode* item = node->data.chord.notes; item; item = item->next) {
            double seconds;
            if (item->type == ALDA_NODE_OCTAVE_UP) {
                if (p->octave < 9) p->octave++;
                continue;
            } else if (item->type == ALDA_NODE_OCTAVE_DOWN) {
                if (p->octave > 0) p->octave--;
                continue;
            } else if (item->type == ALDA_NODE_NOTE) {
                int pitch = note_pitch(p, item);
                if (pitch < 0) {
                    fprintf(stderr, "Error: Invalid note\n");
                    return -1;
                }
                seconds = play_note(ctx, p, item, pitch);
            } else if (item->type == ALDA_NODE_REST) {
                seconds = part_seconds(ctx, p, take_length(p, item->data.rest.duration));
            } else {
                continue;
            }
            if (shortest < 0.0 || seconds < shortest) shortest = seconds;
        }

        if (shortest > 0.0) p->current_time += shortest;
    }

    return 0;
}

static int visit_octave_set(AldaContext* ctx, AldaNode* node) {
    int octave = node->data.octave_set.octave;

    /* Set octave for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        ctx->parts[idx].octave = octave;
    }

    return 0;
}

static int visit_octave_up(AldaContext* ctx, AldaNode* node) {
    (void)node;

    /* Increment octave for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        if (ctx->parts[idx].octave < 9) {
            ctx->parts[idx].octave++;
        }
    }

    return 0;
}

static int visit_octave_down(AldaContext* ctx, AldaNode* node) {
    (void)node;

    /* Decrement octave for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        if (ctx->parts[idx].octave > 0) {
            ctx->parts[idx].octave--;
        }
    }

    return 0;
}

/* Forward declaration */
int alda_eval_attribute(AldaContext* ctx, AldaPartState* part, AldaNode* lisp_list);

static int visit_lisp_list(AldaContext* ctx, AldaNode* node) {
    /* Set source line for event tracking (tempo, pan, program changes) */
    ALDA_SET_SOURCE_LINE(ctx, node->pos.line);

    /* With no part selected the attribute is still evaluated once, with a NULL
     * part, so that a global directive written before the first part
     * declaration - the usual place for (tempo! 120) or (key-sig! ...) - is not
     * silently discarded. */
    if (ctx->current_part_count == 0) {
        return alda_eval_attribute(ctx, NULL, node);
    }

    /* Evaluate attribute for all active parts */
    for (int i = 0; i < ctx->current_part_count; i++) {
        int idx = ctx->current_part_indices[i];
        AldaPartState* part = &ctx->parts[idx];
        if (alda_eval_attribute(ctx, part, node) < 0) {
            return -1;
        }
    }
    return 0;
}

static int visit_repeat(AldaContext* ctx, AldaNode* node) {
    int count = node->data.repeat.count;
    AldaNode* event = node->data.repeat.event;

    /* Save outer repetition context (for nested repeats) */
    int saved_rep = ctx->current_repetition;

    for (int i = 0; i < count; i++) {
        /* Set current repetition (1-indexed) */
        ctx->current_repetition = i + 1;

        if (visit_node(ctx, event) < 0) {
            ctx->current_repetition = saved_rep;
            return -1;
        }
    }

    /* Restore outer repetition context */
    ctx->current_repetition = saved_rep;

    return 0;
}

static int visit_bracket_seq(AldaContext* ctx, AldaNode* node) {
    /* Bracket sequences are just event sequences */
    return visit_list(ctx, node->data.bracket_seq.events);
}

/* Voices fork a part and merge it again, as Alda does (client/model/voice.go):
 *
 * - every voice starts from a copy of the part as it was at the start of the
 *   group, so octave, volume, key signature or tempo set in one voice do not
 *   reach another;
 * - a voice number used again continues that voice where it left off;
 * - at the end the voice that finished last becomes the part, with all of its
 *   state, ties going to the voice created last.
 *
 * The active part's slot in ctx->parts holds whichever voice is being
 * interpreted, so everything that works on parts works on voices unchanged. */
static int visit_voice_group(AldaContext* ctx, AldaNode* node) {
    int nparts = ctx->current_part_count;
    if (nparts == 0) {
        fprintf(stderr, "Error: No current part for voices\n");
        return -1;
    }

    int indices[ALDA_MAX_PARTS];
    for (int i = 0; i < nparts; i++) indices[i] = ctx->current_part_indices[i];

    /* forks[i * ALDA_MAX_VOICES + v] is voice v of part i, in creation order */
    AldaPartState* templates = malloc(sizeof(AldaPartState) * (size_t)nparts);
    AldaPartState* forks = malloc(sizeof(AldaPartState) * (size_t)nparts * ALDA_MAX_VOICES);
    int numbers[ALDA_MAX_VOICES];
    int fork_count = 0;
    int result = 0;

    if (!templates || !forks) {
        free(templates);
        free(forks);
        fprintf(stderr, "Error: Out of memory\n");
        return -1;
    }
    for (int i = 0; i < nparts; i++) templates[i] = ctx->parts[indices[i]];

    for (AldaNode* voice = node->data.voice_group.voices; voice; voice = voice->next) {
        if (voice->type != ALDA_NODE_VOICE) continue;
        int number = voice->data.voice.number;
        if (number == 0) continue;  /* V0: ends the group */

        int v = 0;
        while (v < fork_count && numbers[v] != number) v++;
        if (v == fork_count) {
            if (fork_count == ALDA_MAX_VOICES) {
                fprintf(stderr, "Error: Too many voices (max %d)\n", ALDA_MAX_VOICES);
                result = -1;
                break;
            }
            numbers[fork_count++] = number;
            for (int i = 0; i < nparts; i++) {
                forks[i * ALDA_MAX_VOICES + v] = templates[i];
            }
        }

        for (int i = 0; i < nparts; i++) {
            ctx->parts[indices[i]] = forks[i * ALDA_MAX_VOICES + v];
        }
        ctx->current_part_count = nparts;
        for (int i = 0; i < nparts; i++) ctx->current_part_indices[i] = indices[i];

        result = visit_list(ctx, voice->data.voice.events);

        for (int i = 0; i < nparts; i++) {
            forks[i * ALDA_MAX_VOICES + v] = ctx->parts[indices[i]];
        }
        if (result < 0) break;
    }

    if (fork_count > 0) {
        for (int i = 0; i < nparts; i++) {
            AldaPartState* winner = &forks[i * ALDA_MAX_VOICES + fork_count - 1];
            for (int v = 0; v < fork_count - 1; v++) {
                AldaPartState* fork = &forks[i * ALDA_MAX_VOICES + v];
                if (fork->current_time > winner->current_time) winner = fork;
            }
            ctx->parts[indices[i]] = *winner;
            if (alda_record_voice_group_end(ctx, indices[i]) < 0) result = -1;
        }
    }
    ctx->current_part_count = nparts;
    for (int i = 0; i < nparts; i++) ctx->current_part_indices[i] = indices[i];

    free(templates);
    free(forks);
    return result;
}

/* A voice outside a group (not produced by the parser) plays its events. */
static int visit_voice(AldaContext* ctx, AldaNode* node) {
    return visit_list(ctx, node->data.voice.events);
}

/* ============================================================================
 * Variable Handling
 * ============================================================================ */

static AldaVariable* find_variable(AldaContext* ctx, const char* name) {
    for (int i = 0; i < ctx->variable_count; i++) {
        if (strcmp(ctx->variables[i].name, name) == 0) {
            return &ctx->variables[i];
        }
    }
    return NULL;
}

static int store_variable(AldaContext* ctx, const char* name, AldaNode* events) {
    /* Check if variable already exists */
    AldaVariable* existing = find_variable(ctx, name);
    if (existing) {
        /* Update existing variable */
        existing->events = events;
        return 0;
    }

    /* Add new variable */
    if (ctx->variable_count >= ALDA_MAX_VARIABLES) {
        fprintf(stderr, "Error: Too many variables (max %d)\n", ALDA_MAX_VARIABLES);
        return -1;
    }

    AldaVariable* var = &ctx->variables[ctx->variable_count++];
    strncpy(var->name, name, sizeof(var->name) - 1);
    var->name[sizeof(var->name) - 1] = '\0';
    var->events = events;

    return 0;
}

static int visit_var_def(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.var_def.name;
    AldaNode* events = node->data.var_def.events;

    if (ctx->verbose_mode) {
        fprintf(stderr, "Defining variable: %s\n", name);
    }

    /* Store the variable (we store the AST node, not a copy) */
    return store_variable(ctx, name, events);
}

static int visit_var_ref(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.var_ref.name;

    AldaVariable* var = find_variable(ctx, name);
    if (!var) {
        fprintf(stderr, "Error: Undefined variable '%s'\n", name);
        return -1;
    }

    if (ctx->verbose_mode) {
        fprintf(stderr, "Expanding variable: %s\n", name);
    }

    /* Visit all stored events (linked list) */
    return visit_list(ctx, var->events);
}

/* ============================================================================
 * Marker Handling
 * ============================================================================ */

static AldaMarker* find_marker(AldaContext* ctx, const char* name) {
    for (int i = 0; i < ctx->marker_count; i++) {
        if (strcmp(ctx->markers[i].name, name) == 0) {
            return &ctx->markers[i];
        }
    }
    return NULL;
}

static int store_marker(AldaContext* ctx, const char* name, double time) {
    /* Check if marker already exists */
    AldaMarker* existing = find_marker(ctx, name);
    if (existing) {
        existing->time = time;
        return 0;
    }

    /* Add new marker */
    if (ctx->marker_count >= ALDA_MAX_MARKERS) {
        fprintf(stderr, "Error: Too many markers (max %d)\n", ALDA_MAX_MARKERS);
        return -1;
    }

    AldaMarker* marker = &ctx->markers[ctx->marker_count++];
    strncpy(marker->name, name, sizeof(marker->name) - 1);
    marker->name[sizeof(marker->name) - 1] = '\0';
    marker->time = time;

    return 0;
}

static int visit_marker(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.marker.name;

    /* The marker is placed at the first active part's position */
    AldaPartState* part = alda_current_part(ctx);
    if (!part) {
        fprintf(stderr, "Error: No current part for marker\n");
        return -1;
    }

    if (ctx->verbose_mode) {
        fprintf(stderr, "Setting marker '%s' at %.3fs\n", name, part->current_time);
    }

    return store_marker(ctx, name, part->current_time);
}

static int visit_at_marker(AldaContext* ctx, AldaNode* node) {
    const char* name = node->data.at_marker.name;

    AldaMarker* marker = find_marker(ctx, name);
    if (!marker) {
        fprintf(stderr, "Error: Undefined marker '%s'\n", name);
        return -1;
    }

    if (ctx->verbose_mode) {
        fprintf(stderr, "Jumping to marker '%s' at %.3fs\n", name, marker->time);
    }

    for (int i = 0; i < ctx->current_part_count; i++) {
        ctx->parts[ctx->current_part_indices[i]].current_time = marker->time;
    }

    return 0;
}

/* ============================================================================
 * On-Repetitions Handling
 * ============================================================================ */

static int on_reps_applies(AldaNode* node, int repetition) {
    /* Outside a repeat, always play */
    if (repetition == 0) return 1;
    for (size_t i = 0; i < node->data.on_reps.rep_count; i++) {
        if (node->data.on_reps.reps[i] == repetition) return 1;
    }
    return 0;
}

static int visit_on_reps(AldaContext* ctx, AldaNode* node) {
    if (!on_reps_applies(node, ctx->current_repetition)) return 0;
    return visit_node(ctx, node->data.on_reps.event);
}

/* ============================================================================
 * Cram Expression Handling
 * ============================================================================ */

/* The unscaled length in seconds of a list of events, as Alda's DurationMs
 * computes it for a cram's "inner duration": lengths carry over as defaults
 * from note to note, a chord counts its shortest note, a nested cram its own
 * duration, and attributes nothing. Works on a copy of the default so the
 * part is not changed. */
static double inner_seconds(AldaContext* ctx, AldaNode* node, AldaLength* dflt,
                            int tempo, int repetition);

static double inner_one(AldaContext* ctx, AldaNode* node, AldaLength* dflt,
                        int tempo, int repetition) {
    switch (node->type) {
        case ALDA_NODE_NOTE:
        case ALDA_NODE_REST: {
            AldaNode* dur = node->type == ALDA_NODE_NOTE
                          ? node->data.note.duration : node->data.rest.duration;
            AldaLength len = resolve_length(dflt->beats, dflt->ms, dur);
            if (dur) *dflt = len;
            return length_seconds(len, tempo);
        }
        case ALDA_NODE_CHORD: {
            double shortest = 0.0;
            for (AldaNode* n = node->data.chord.notes; n; n = n->next) {
                double s = inner_one(ctx, n, dflt, tempo, repetition);
                if (s > 0.0 && (shortest == 0.0 || s < shortest)) shortest = s;
            }
            return shortest;
        }
        case ALDA_NODE_CRAM:
            return length_seconds(
                resolve_length(dflt->beats, dflt->ms, node->data.cram.duration), tempo);
        case ALDA_NODE_EVENT_SEQ:
            return inner_seconds(ctx, node->data.event_seq.events, dflt, tempo, repetition);
        case ALDA_NODE_BRACKET_SEQ:
            return inner_seconds(ctx, node->data.bracket_seq.events, dflt, tempo, repetition);
        case ALDA_NODE_REPEAT: {
            double total = 0.0;
            for (int r = 1; r <= node->data.repeat.count; r++) {
                total += inner_one(ctx, node->data.repeat.event, dflt, tempo, r);
            }
            return total;
        }
        case ALDA_NODE_ON_REPS:
            return on_reps_applies(node, repetition)
                 ? inner_one(ctx, node->data.on_reps.event, dflt, tempo, repetition)
                 : 0.0;
        case ALDA_NODE_VAR_REF: {
            AldaVariable* var = find_variable(ctx, node->data.var_ref.name);
            return var ? inner_seconds(ctx, var->events, dflt, tempo, repetition) : 0.0;
        }
        default:
            return 0.0;
    }
}

static double inner_seconds(AldaContext* ctx, AldaNode* node, AldaLength* dflt,
                            int tempo, int repetition) {
    double total = 0.0;
    for (; node; node = node->next) {
        total += inner_one(ctx, node, dflt, tempo, repetition);
    }
    return total;
}

/* A cram fits its events into its duration, keeping their proportions: each
 * event is scaled by the cram's duration over the events' total, as Alda does
 * (client/model/cram.go). A nested cram multiplies the scales. Afterwards the
 * cram's own duration, if it has one, becomes the default. */
static int visit_cram(AldaContext* ctx, AldaNode* node) {
    if (!alda_current_part(ctx)) {
        fprintf(stderr, "Error: No current part for cram\n");
        return -1;
    }

    /* Run the cram once per active part, with that part temporarily the only
     * one selected: the scale depends on the part's tempo and default
     * duration, which group members need not share. */
    int saved_count = ctx->current_part_count;
    int saved_indices[ALDA_MAX_PARTS];
    for (int i = 0; i < saved_count; i++) {
        saved_indices[i] = ctx->current_part_indices[i];
    }

    int result = 0;
    for (int i = 0; i < saved_count && result == 0; i++) {
        ctx->current_part_count = 1;
        ctx->current_part_indices[0] = saved_indices[i];
        AldaPartState* part = &ctx->parts[saved_indices[i]];

        int tempo = alda_effective_tempo(ctx, part);
        AldaLength outer = resolve_length(part->default_beats, part->default_ms,
                                          node->data.cram.duration);
        AldaLength dflt = {part->default_beats, part->default_ms};
        double inner = inner_seconds(ctx, node->data.cram.events, &dflt, tempo,
                                     ctx->current_repetition);
        if (inner <= 0.0) continue;  /* Nothing in the cram takes time */

        double saved_scale = part->time_scale;
        AldaLength saved_default = {part->default_beats, part->default_ms};
        part->time_scale = saved_scale * length_seconds(outer, tempo) / inner;

        result = visit_list(ctx, node->data.cram.events);

        /* The cram may have run a voice group, which replaces the slot */
        part = &ctx->parts[saved_indices[i]];
        part->time_scale = saved_scale;
        if (node->data.cram.duration) {
            part->default_beats = outer.beats;
            part->default_ms = outer.ms;
        } else {
            part->default_beats = saved_default.beats;
            part->default_ms = saved_default.ms;
        }
    }

    ctx->current_part_count = saved_count;
    for (int i = 0; i < saved_count; i++) {
        ctx->current_part_indices[i] = saved_indices[i];
    }

    return result;
}

/* ============================================================================
 * Main Interpreter Functions
 * ============================================================================ */

int alda_interpret_ast(AldaContext* ctx, AldaNode* root) {
    if (!ctx || !root) return -1;

    /* Clear any previous events */
    alda_events_clear(ctx);
    ctx->note_count = 0;
    ctx->tempo_change_count = 0;
    ctx->voice_group_end_count = 0;
    ctx->base_tempo = ctx->global_tempo;

    /* Reset part positions */
    for (int i = 0; i < ctx->part_count; i++) {
        ctx->parts[i].current_time = 0.0;
        ctx->parts[i].time_scale = 1.0;
    }

    if (visit_node(ctx, root) < 0) return -1;

    /* Channels, controller state and ticks need the whole score */
    return alda_events_finalize(ctx);
}

int alda_interpret_string(AldaContext* ctx, const char* source, const char* filename) {
    if (!ctx || !source) return -1;

    /* Parse the source */
    char* error = NULL;
    AldaNode* ast = alda_parse(source, filename, &error);

    if (error) {
        fprintf(stderr, "%s\n", error);
        free(error);
        return -1;
    }

    if (!ast) {
        fprintf(stderr, "Error: Failed to parse source\n");
        return -1;
    }

    /* Interpret the AST */
    int result = alda_interpret_ast(ctx, ast);

    /* Free the AST */
    alda_ast_free(ast);

    return result;
}

int alda_interpret_file(AldaContext* ctx, const char* filename) {
    if (!ctx || !filename) return -1;

    /* Read file contents */
    FILE* f = fopen(filename, "r");
    if (!f) {
        fprintf(stderr, "Error: Cannot open file '%s'\n", filename);
        return -1;
    }

    /* Get file size */
    fseek(f, 0, SEEK_END);
    long size = ftell(f);
    if (size < 0) {
        fclose(f);
        fprintf(stderr, "Error: Cannot determine file size for '%s'\n", filename);
        return -1;
    }
    fseek(f, 0, SEEK_SET);

    /* Allocate buffer */
    char* source = malloc((size_t)size + 1);
    if (!source) {
        fclose(f);
        fprintf(stderr, "Error: Out of memory\n");
        return -1;
    }

    /* Read file */
    size_t bytes_requested = (size_t)size;
    size_t bytes_read = fread(source, 1, bytes_requested, f);
    /* Defensive bounds check: fread returns at most bytes_requested */
    size_t null_pos = (bytes_read < bytes_requested) ? bytes_read : bytes_requested;
    source[null_pos] = '\0';
    fclose(f);

    /* Interpret */
    ctx->current_file = filename;
    int result = alda_interpret_string(ctx, source, filename);
    ctx->current_file = NULL;

    free(source);
    return result;
}
