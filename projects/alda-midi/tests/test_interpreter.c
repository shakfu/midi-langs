/**
 * @file test_interpreter.c
 * @brief Unit tests for Alda interpreter.
 *
 * Tests MIDI event generation including tempo, volume, polyphony,
 * markers, variables, and other core interpreter functionality.
 */

#include "test_framework.h"
#include <alda/alda.h>
#include <alda/context.h>
#include <alda/interpreter.h>
#include <alda/scheduler.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

/* Initialize context for testing */
static void test_context_init(AldaContext* ctx) {
    alda_context_init(ctx);
    alda_set_no_sleep(ctx, 1);  /* Disable timing for tests */
}

/* Count events of a specific type */
static int count_events(AldaContext* ctx, AldaEventType type) {
    int count = 0;
    for (int i = 0; i < ctx->event_count; i++) {
        if (ctx->events[i].type == type) {
            count++;
        }
    }
    return count;
}

/* Find first event of type */
static AldaScheduledEvent* find_event(AldaContext* ctx, AldaEventType type, int skip) {
    for (int i = 0; i < ctx->event_count; i++) {
        if (ctx->events[i].type == type) {
            if (skip == 0) {
                return &ctx->events[i];
            }
            skip--;
        }
    }
    return NULL;
}

/* Find note-on event with specific pitch */
static AldaScheduledEvent* find_note_on(AldaContext* ctx, int pitch) {
    for (int i = 0; i < ctx->event_count; i++) {
        if (ctx->events[i].type == ALDA_EVT_NOTE_ON &&
            ctx->events[i].data1 == pitch) {
            return &ctx->events[i];
        }
    }
    return NULL;
}

/* ============================================================================
 * Basic Note Tests
 * ============================================================================ */

TEST(interpret_single_note) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: c", "test");
    ASSERT_EQ(result, 0);

    /* Should have note-on and note-off events */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 1);
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_OFF), 1);

    /* C4 = MIDI pitch 60 */
    AldaScheduledEvent* note_on = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note_on);
    ASSERT_EQ(note_on->data1, 60);

    alda_context_cleanup(&ctx);
}

TEST(interpret_note_with_accidentals) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* C# (sharp) */
    int result = alda_interpret_string(&ctx, "piano: c+", "test");
    ASSERT_EQ(result, 0);

    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data1, 61);  /* C#4 = 61 */

    alda_context_cleanup(&ctx);

    /* Db (flat) */
    test_context_init(&ctx);
    result = alda_interpret_string(&ctx, "piano: d-", "test");
    ASSERT_EQ(result, 0);

    note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data1, 61);  /* Db4 = 61 */

    alda_context_cleanup(&ctx);
}

TEST(interpret_note_with_octave) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: o5 c", "test");
    ASSERT_EQ(result, 0);

    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data1, 72);  /* C5 = 72 */

    alda_context_cleanup(&ctx);
}

TEST(interpret_octave_up_down) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Default octave is 4, > goes up, < goes down */
    int result = alda_interpret_string(&ctx, "piano: c > c < < c", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 3);

    /* First C at octave 4 */
    AldaScheduledEvent* note1 = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note1);
    ASSERT_EQ(note1->data1, 60);

    /* Second C at octave 5 */
    AldaScheduledEvent* note2 = find_event(&ctx, ALDA_EVT_NOTE_ON, 1);
    ASSERT_NOT_NULL(note2);
    ASSERT_EQ(note2->data1, 72);

    /* Third C at octave 3 */
    AldaScheduledEvent* note3 = find_event(&ctx, ALDA_EVT_NOTE_ON, 2);
    ASSERT_NOT_NULL(note3);
    ASSERT_EQ(note3->data1, 48);

    alda_context_cleanup(&ctx);
}

TEST(interpret_note_sequence) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: c d e f g", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 5);

    /* Verify pitches: C=60, D=62, E=64, F=65, G=67 */
    AldaScheduledEvent* c = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    AldaScheduledEvent* d = find_event(&ctx, ALDA_EVT_NOTE_ON, 1);
    AldaScheduledEvent* e = find_event(&ctx, ALDA_EVT_NOTE_ON, 2);
    AldaScheduledEvent* f = find_event(&ctx, ALDA_EVT_NOTE_ON, 3);
    AldaScheduledEvent* g = find_event(&ctx, ALDA_EVT_NOTE_ON, 4);

    ASSERT_EQ(c->data1, 60);
    ASSERT_EQ(d->data1, 62);
    ASSERT_EQ(e->data1, 64);
    ASSERT_EQ(f->data1, 65);
    ASSERT_EQ(g->data1, 67);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Duration Tests
 * ============================================================================ */

TEST(interpret_note_durations) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Quarter note (4), half note (2), whole note (1) */
    int result = alda_interpret_string(&ctx, "piano: c4 c2 c1", "test");
    ASSERT_EQ(result, 0);

    alda_events_sort(&ctx);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 3);

    /* Verify timing: quarter = 480 ticks, half = 960, whole = 1920 */
    AldaScheduledEvent* n1 = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    AldaScheduledEvent* n2 = find_event(&ctx, ALDA_EVT_NOTE_ON, 1);
    AldaScheduledEvent* n3 = find_event(&ctx, ALDA_EVT_NOTE_ON, 2);

    ASSERT_EQ(n1->tick, 0);
    ASSERT_EQ(n2->tick, 480);     /* After quarter note */
    ASSERT_EQ(n3->tick, 1440);    /* After quarter + half */

    alda_context_cleanup(&ctx);
}

TEST(interpret_dotted_duration) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Dotted quarter = 480 + 240 = 720 ticks */
    int result = alda_interpret_string(&ctx, "piano: c4. c4", "test");
    ASSERT_EQ(result, 0);

    alda_events_sort(&ctx);

    AldaScheduledEvent* n1 = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    AldaScheduledEvent* n2 = find_event(&ctx, ALDA_EVT_NOTE_ON, 1);

    ASSERT_EQ(n1->tick, 0);
    ASSERT_EQ(n2->tick, 720);  /* After dotted quarter */

    alda_context_cleanup(&ctx);
}

TEST(interpret_tied_duration) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Tied quarter + eighth = 480 + 240 = 720 ticks */
    int result = alda_interpret_string(&ctx, "piano: c4~8 c4", "test");
    ASSERT_EQ(result, 0);

    alda_events_sort(&ctx);

    AldaScheduledEvent* n1 = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    AldaScheduledEvent* n2 = find_event(&ctx, ALDA_EVT_NOTE_ON, 1);

    ASSERT_EQ(n1->tick, 0);
    ASSERT_EQ(n2->tick, 720);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Rest Tests
 * ============================================================================ */

TEST(interpret_rest) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Note, rest, note - second note should be offset */
    int result = alda_interpret_string(&ctx, "piano: c4 r4 c4", "test");
    ASSERT_EQ(result, 0);

    alda_events_sort(&ctx);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 2);

    AldaScheduledEvent* n1 = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    AldaScheduledEvent* n2 = find_event(&ctx, ALDA_EVT_NOTE_ON, 1);

    ASSERT_EQ(n1->tick, 0);
    ASSERT_EQ(n2->tick, 960);  /* After quarter note + quarter rest */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Chord Tests
 * ============================================================================ */

TEST(interpret_chord) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* C major chord */
    int result = alda_interpret_string(&ctx, "piano: c/e/g", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 3);

    /* All notes should start at same tick */
    AldaScheduledEvent* c = find_note_on(&ctx, 60);
    AldaScheduledEvent* e = find_note_on(&ctx, 64);
    AldaScheduledEvent* g = find_note_on(&ctx, 67);

    ASSERT_NOT_NULL(c);
    ASSERT_NOT_NULL(e);
    ASSERT_NOT_NULL(g);

    ASSERT_EQ(c->tick, e->tick);
    ASSERT_EQ(e->tick, g->tick);

    alda_context_cleanup(&ctx);
}

TEST(interpret_chord_with_octave_change) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Chord spanning octaves */
    int result = alda_interpret_string(&ctx, "piano: c/>e/>g", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 3);

    /* C4=60, E5=76, G6=91 */
    AldaScheduledEvent* c = find_note_on(&ctx, 60);
    AldaScheduledEvent* e = find_note_on(&ctx, 76);
    AldaScheduledEvent* g = find_note_on(&ctx, 91);

    ASSERT_NOT_NULL(c);
    ASSERT_NOT_NULL(e);
    ASSERT_NOT_NULL(g);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Tempo Tests
 * ============================================================================ */

TEST(interpret_tempo_attribute) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: (tempo 180) c", "test");
    ASSERT_EQ(result, 0);

    /* Should have a tempo change event */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_TEMPO), 1);

    AldaScheduledEvent* tempo = find_event(&ctx, ALDA_EVT_TEMPO, 0);
    ASSERT_NOT_NULL(tempo);
    ASSERT_EQ(tempo->data1, 180);

    alda_context_cleanup(&ctx);
}

TEST(interpret_tempo_change_mid_score) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: c4 (tempo 180) c4", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_TEMPO), 1);

    AldaScheduledEvent* tempo = find_event(&ctx, ALDA_EVT_TEMPO, 0);
    ASSERT_NOT_NULL(tempo);
    ASSERT_EQ(tempo->tick, 480);  /* After first quarter note */
    ASSERT_EQ(tempo->data1, 180);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Volume/Dynamics Tests
 * ============================================================================ */

TEST(interpret_volume_attribute) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: (volume 100) c", "test");
    ASSERT_EQ(result, 0);

    /* Note velocity should reflect volume */
    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data2, 127);  /* 100% volume = max velocity */

    alda_context_cleanup(&ctx);
}

TEST(interpret_dynamics) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* ff (fortissimo): velocity 88, as Alda sends it */
    int result = alda_interpret_string(&ctx, "piano: (ff) c", "test");
    ASSERT_EQ(result, 0);

    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data2, 88);

    alda_context_cleanup(&ctx);

    /* pp (pianissimo): Alda's 0.31314 * 127 rounds to 40 (client/model/attributes.go) */
    test_context_init(&ctx);
    result = alda_interpret_string(&ctx, "piano: (pp) c", "test");
    ASSERT_EQ(result, 0);

    note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data2, 40);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Repeat Tests
 * ============================================================================ */

TEST(interpret_simple_repeat) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: c *3", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 3);

    alda_context_cleanup(&ctx);
}

TEST(interpret_repeat_sequence) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: [c d] *2", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 4);  /* c d c d */

    alda_context_cleanup(&ctx);
}

TEST(interpret_alternate_endings) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* '1 plays on first rep, '2 plays on second rep */
    int result = alda_interpret_string(&ctx, "piano: [c d '1 e '2 f] *2", "test");
    ASSERT_EQ(result, 0);

    /* First rep: c d e, Second rep: c d f = 6 notes */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 6);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Variable Tests
 * ============================================================================ */

TEST(interpret_variable_definition_and_reference) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: motif = [c d e] motif motif", "test");
    ASSERT_EQ(result, 0);

    /* motif (c d e) played twice = 6 notes */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 6);

    alda_context_cleanup(&ctx);
}

TEST(interpret_variable_redefine) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Use newlines to separate variable definitions and uses */
    int result = alda_interpret_string(&ctx, 
        "piano:\n"
        "x = c\n"
        "x\n"
        "x = d\n"
        "x\n"
        "x = e\n"
        "x", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 3);

    /* Pitches should be c, d, e */
    AldaScheduledEvent* n1 = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    AldaScheduledEvent* n2 = find_event(&ctx, ALDA_EVT_NOTE_ON, 1);
    AldaScheduledEvent* n3 = find_event(&ctx, ALDA_EVT_NOTE_ON, 2);

    ASSERT_EQ(n1->data1, 60);  /* C */
    ASSERT_EQ(n2->data1, 62);  /* D */
    ASSERT_EQ(n3->data1, 64);  /* E */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Marker Tests
 * ============================================================================ */

TEST(interpret_marker_and_at_marker) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Define marker after c, continue with d e, then jump back and play f */
    /* Marker is placed after c, so @here goes back to tick 480 */
    int result = alda_interpret_string(&ctx, 
        "piano: c4 %here d4 e4 @here f4", "test");
    ASSERT_EQ(result, 0);

    alda_events_sort(&ctx);

    /* Should have 4 notes: c, d, e, f */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 4);

    /* c starts at 0 */
    /* %here marker placed at tick 480 (after c) */
    /* d starts at 480 */
    /* e starts at 960 */
    /* @here jumps back to tick 480 */
    /* f starts at 480 (same time as d) */

    /* Find all note-ons */
    int c_tick = -1, d_tick = -1, e_tick = -1, f_tick = -1;

    for (int i = 0; i < ctx.event_count; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            int pitch = ctx.events[i].data1;
            int tick = ctx.events[i].tick;

            if (pitch == 60) { c_tick = tick; }
            else if (pitch == 62) { d_tick = tick; }
            else if (pitch == 64) { e_tick = tick; }
            else if (pitch == 65) { f_tick = tick; }
        }
    }

    ASSERT_EQ(c_tick, 0);
    ASSERT_EQ(d_tick, 480);
    ASSERT_EQ(e_tick, 960);
    ASSERT_EQ(f_tick, 480);  /* f at same time as d (jumped back via marker) */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Voice Tests (Polyphony)
 * ============================================================================ */

TEST(interpret_voices) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, 
        "piano: V1: c d e V2: g a b V0:", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 6);

    /* Both voices start at same tick (0) */
    AldaScheduledEvent* c = find_note_on(&ctx, 60);  /* C in V1 */
    AldaScheduledEvent* g = find_note_on(&ctx, 67);  /* G in V2 */

    ASSERT_NOT_NULL(c);
    ASSERT_NOT_NULL(g);
    ASSERT_EQ(c->tick, 0);
    ASSERT_EQ(g->tick, 0);

    alda_context_cleanup(&ctx);
}

TEST(interpret_voice_timing) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Voice 1 has 3 quarter notes, voice 2 has 2 */
    int result = alda_interpret_string(&ctx, 
        "piano: V1: c4 d4 e4 V2: g2 V0:", "test");
    ASSERT_EQ(result, 0);

    alda_events_sort(&ctx);

    /* After V0:, part tick should be at max voice tick (3*480 = 1440) */
    /* Verify c, d, e are at 0, 480, 960 */
    /* Verify g is at 0 */

    int found_ticks[4] = {-1, -1, -1, -1};
    for (int i = 0; i < ctx.event_count; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            switch (ctx.events[i].data1) {
                case 60: found_ticks[0] = ctx.events[i].tick; break;  /* c */
                case 62: found_ticks[1] = ctx.events[i].tick; break;  /* d */
                case 64: found_ticks[2] = ctx.events[i].tick; break;  /* e */
                case 67: found_ticks[3] = ctx.events[i].tick; break;  /* g */
            }
        }
    }

    ASSERT_EQ(found_ticks[0], 0);     /* c */
    ASSERT_EQ(found_ticks[1], 480);   /* d */
    ASSERT_EQ(found_ticks[2], 960);   /* e */
    ASSERT_EQ(found_ticks[3], 0);     /* g */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Cram Expression Tests
 * ============================================================================ */

TEST(interpret_cram_basic) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* 4 notes crammed into 1 quarter note duration */
    int result = alda_interpret_string(&ctx, "piano: {c d e f}4", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 4);

    /* Total duration should be 480 (quarter note), each note 120 */
    alda_events_sort(&ctx);

    int ticks[4] = {-1, -1, -1, -1};
    int idx = 0;
    for (int i = 0; i < ctx.event_count && idx < 4; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            ticks[idx++] = ctx.events[i].tick;
        }
    }

    ASSERT_EQ(ticks[0], 0);
    ASSERT_EQ(ticks[1], 120);
    ASSERT_EQ(ticks[2], 240);
    ASSERT_EQ(ticks[3], 360);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Key Signature Tests
 * ============================================================================ */

TEST(interpret_key_signature) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* G major: F is sharp (using key-sig with tonic and mode) */
    int result = alda_interpret_string(&ctx, "piano: (key-sig '(g major)) f", "test");
    ASSERT_EQ(result, 0);

    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data1, 66);  /* F# = 66 in G major */

    alda_context_cleanup(&ctx);
}

TEST(interpret_natural_overrides_key_sig) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* G major has F#, but f_ means natural */
    int result = alda_interpret_string(&ctx, "piano: (key-sig '(g major)) f_", "test");
    ASSERT_EQ(result, 0);

    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data1, 65);  /* F natural = 65 */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Transpose Tests
 * ============================================================================ */

TEST(interpret_transpose) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Transpose up 2 semitones */
    int result = alda_interpret_string(&ctx, "piano: (transpose 2) c", "test");
    ASSERT_EQ(result, 0);

    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data1, 62);  /* C + 2 = D */

    alda_context_cleanup(&ctx);
}

TEST(interpret_transpose_negative) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Transpose down 3 semitones */
    int result = alda_interpret_string(&ctx, "piano: (transpose -3) c", "test");
    ASSERT_EQ(result, 0);

    AldaScheduledEvent* note = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->data1, 57);  /* C - 3 = A */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Pan Tests
 * ============================================================================ */

TEST(interpret_pan) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: (pan 0) c", "test");
    ASSERT_EQ(result, 0);

    /* Should have a pan event */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_PAN), 1);

    AldaScheduledEvent* pan = find_event(&ctx, ALDA_EVT_PAN, 0);
    ASSERT_NOT_NULL(pan);
    /* For ALDA_EVT_PAN, data1 stores the pan value (0-127) */
    ASSERT_EQ(pan->data1, 0);    /* Hard left (0% -> 0) */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Quantization Tests
 * ============================================================================ */

TEST(interpret_quantization) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* quant 50 means note plays for 50% of its duration */
    int result = alda_interpret_string(&ctx, "piano: (quant 50) c4", "test");
    ASSERT_EQ(result, 0);

    alda_events_sort(&ctx);

    AldaScheduledEvent* note_on = find_event(&ctx, ALDA_EVT_NOTE_ON, 0);
    AldaScheduledEvent* note_off = find_event(&ctx, ALDA_EVT_NOTE_OFF, 0);

    ASSERT_NOT_NULL(note_on);
    ASSERT_NOT_NULL(note_off);

    /* Quarter note = 480 ticks, 50% quant = 240 ticks sounding */
    int duration = note_off->tick - note_on->tick;
    ASSERT_EQ(duration, 240);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Multiple Parts Tests
 * ============================================================================ */

TEST(interpret_multiple_parts) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, 
        "piano: c d e\nviolin: g a b", "test");
    ASSERT_EQ(result, 0);

    /* Should have program changes for both instruments */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_PROGRAM), 2);
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 6);

    alda_context_cleanup(&ctx);
}

TEST(interpret_part_group) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Two instruments playing same notes */
    int result = alda_interpret_string(&ctx, "piano/violin: c d", "test");
    ASSERT_EQ(result, 0);

    /* Each instrument plays both notes = 4 note-ons */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_NOTE_ON), 4);

    /* Should have 2 program changes (one per instrument) */
    ASSERT_EQ(count_events(&ctx, ALDA_EVT_PROGRAM), 2);

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Program Change Tests
 * ============================================================================ */

TEST(interpret_program_change) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: c", "test");
    ASSERT_EQ(result, 0);

    ASSERT_EQ(count_events(&ctx, ALDA_EVT_PROGRAM), 1);

    AldaScheduledEvent* prog = find_event(&ctx, ALDA_EVT_PROGRAM, 0);
    ASSERT_NOT_NULL(prog);
    ASSERT_EQ(prog->data1, 0);  /* Piano = GM program 0 */

    alda_context_cleanup(&ctx);

    /* Test violin */
    test_context_init(&ctx);
    result = alda_interpret_string(&ctx, "violin: c", "test");
    ASSERT_EQ(result, 0);

    prog = find_event(&ctx, ALDA_EVT_PROGRAM, 0);
    ASSERT_NOT_NULL(prog);
    ASSERT_EQ(prog->data1, 40);  /* Violin = GM program 40 */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Error Handling Tests
 * ============================================================================ */

TEST(interpret_undefined_variable_error) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: undefined_var", "test");
    ASSERT_EQ(result, -1);  /* Should fail */

    alda_context_cleanup(&ctx);
}

TEST(interpret_undefined_marker_error) {
    AldaContext ctx;
    test_context_init(&ctx);

    int result = alda_interpret_string(&ctx, "piano: @nonexistent", "test");
    ASSERT_EQ(result, -1);  /* Should fail */

    alda_context_cleanup(&ctx);
}

TEST(interpret_no_part_error) {
    AldaContext ctx;
    test_context_init(&ctx);

    /* Notes without declaring a part first */
    int result = alda_interpret_string(&ctx, "c d e", "test");
    ASSERT_EQ(result, -1);  /* Should fail */

    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Pitch Calculation Unit Tests
 * ============================================================================ */

TEST(calculate_pitch_basic) {
    /* C4 = 60 */
    ASSERT_EQ(alda_calculate_pitch('c', NULL, 4, NULL), 60);
    ASSERT_EQ(alda_calculate_pitch('C', NULL, 4, NULL), 60);

    /* D4 = 62 */
    ASSERT_EQ(alda_calculate_pitch('d', NULL, 4, NULL), 62);

    /* A4 = 69 */
    ASSERT_EQ(alda_calculate_pitch('a', NULL, 4, NULL), 69);
}

TEST(calculate_pitch_octaves) {
    /* C0 = 12 */
    ASSERT_EQ(alda_calculate_pitch('c', NULL, 0, NULL), 12);

    /* C5 = 72 */
    ASSERT_EQ(alda_calculate_pitch('c', NULL, 5, NULL), 72);

    /* C8 = 108 */
    ASSERT_EQ(alda_calculate_pitch('c', NULL, 8, NULL), 108);
}

TEST(calculate_pitch_accidentals) {
    /* C#4 = 61 */
    ASSERT_EQ(alda_calculate_pitch('c', "+", 4, NULL), 61);

    /* Db4 = 61 */
    ASSERT_EQ(alda_calculate_pitch('d', "-", 4, NULL), 61);

    /* C##4 (double sharp) = 62 */
    ASSERT_EQ(alda_calculate_pitch('c', "++", 4, NULL), 62);

    /* Dbb4 (double flat) = 60 */
    ASSERT_EQ(alda_calculate_pitch('d', "--", 4, NULL), 60);
}

TEST(calculate_pitch_with_key_sig) {
    /* G major: F# */
    int key_g_major[7] = {0, 0, 0, 1, 0, 0, 0};  /* F is sharp */

    /* F in G major should be F# = 66 */
    ASSERT_EQ(alda_calculate_pitch('f', NULL, 4, key_g_major), 66);

    /* F natural (_) in G major should still be F = 65 */
    ASSERT_EQ(alda_calculate_pitch('f', "_", 4, key_g_major), 65);

    /* F# explicitly in G major = 66 (explicit overrides) */
    ASSERT_EQ(alda_calculate_pitch('f', "+", 4, key_g_major), 66);
}

/* ============================================================================
 * Duration Calculation Unit Tests
 * ============================================================================ */

TEST(duration_to_ticks_basic) {
    /* Whole note = 1920 */
    ASSERT_EQ(alda_duration_to_ticks(1, 0), 1920);

    /* Half note = 960 */
    ASSERT_EQ(alda_duration_to_ticks(2, 0), 960);

    /* Quarter note = 480 */
    ASSERT_EQ(alda_duration_to_ticks(4, 0), 480);

    /* Eighth note = 240 */
    ASSERT_EQ(alda_duration_to_ticks(8, 0), 240);

    /* Sixteenth = 120 */
    ASSERT_EQ(alda_duration_to_ticks(16, 0), 120);
}

TEST(duration_to_ticks_dotted) {
    /* Dotted quarter = 480 + 240 = 720 */
    ASSERT_EQ(alda_duration_to_ticks(4, 1), 720);

    /* Double dotted quarter = 480 + 240 + 120 = 840 */
    ASSERT_EQ(alda_duration_to_ticks(4, 2), 840);
}

TEST(ms_to_ticks) {
    /* At 120 BPM: 1 beat = 500ms, 1 beat = 480 ticks */
    /* So 1000ms = 2 beats = 960 ticks */
    ASSERT_EQ(alda_ms_to_ticks(1000, 120), 960);

    /* 500ms at 120 BPM = 480 ticks */
    ASSERT_EQ(alda_ms_to_ticks(500, 120), 480);
}

TEST(apply_quant) {
    /* 100% quant = full duration */
    ASSERT_EQ(alda_apply_quant(480, 100), 480);

    /* 50% quant = half duration */
    ASSERT_EQ(alda_apply_quant(480, 50), 240);

    /* 90% quant (default) */
    ASSERT_EQ(alda_apply_quant(480, 90), 432);
}

/* ============================================================================
 * Test Suite Main
 * ============================================================================ */


/* ============================================================================
 * Interpreter Divergences Found Against aldakit
 *
 * Each of these produced the wrong notes until fixed. Found by comparing
 * scheduled events with the aldakit implementation over both projects' example
 * corpora; see test_examples.c for the corpus-level backstop.
 * ============================================================================ */

/* Pitch of the first note-on for a given part index, or -1. */
static int first_pitch_for_part(AldaContext* ctx, int part_index) {
    for (int i = 0; i < ctx->event_count; i++) {
        if (ctx->events[i].type == ALDA_EVT_NOTE_ON &&
            ctx->events[i].part_index == part_index) {
            return ctx->events[i].data1;
        }
    }
    return -1;
}

TEST(interp_key_sig_quoted_list_with_accidental_word) {
    /* '(a flat major) is Ab major: B, E, A and D are flattened. */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: (key-sig '(a flat major)) a b c d e f g",
                          "test");
    int expect[] = {68, 70, 60, 61, 63, 65, 67};
    int n = 0;
    for (int i = 0; i < ctx.event_count && n < 7; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            ASSERT_EQ(ctx.events[i].data1, expect[n]);
            n++;
        }
    }
    ASSERT_EQ(n, 7);
    alda_context_cleanup(&ctx);
}

TEST(interp_key_sig_key_name_with_accidental_word) {
    /* '(e flat minor) names Eb minor - six flats - not "E is flat". The
     * trailing mode word is what distinguishes it from the per-note form. */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: (key-sig '(e flat minor)) c d e f g a b",
                          "test");
    int expect[] = {59, 61, 63, 65, 66, 68, 70};  /* Cb Db Eb F Gb Ab Bb */
    int n = 0;
    for (int i = 0; i < ctx.event_count && n < 7; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            ASSERT_EQ(ctx.events[i].data1, expect[n]);
            n++;
        }
    }
    ASSERT_EQ(n, 7);
    alda_context_cleanup(&ctx);
}

TEST(interp_key_sig_bare_accidental_words) {
    /* '(e flat b flat) has no trailing mode, so it is the per-note form and
     * means the same as '(e (flat) b (flat)) and "e- b-". */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: (key-sig '(e flat b flat)) e b", "test");
    int expect[] = {63, 70};
    int n = 0;
    for (int i = 0; i < ctx.event_count && n < 2; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            ASSERT_EQ(ctx.events[i].data1, expect[n]);
            n++;
        }
    }
    ASSERT_EQ(n, 2);
    alda_context_cleanup(&ctx);
}

TEST(interp_key_sig_per_note_accidental_form) {
    /* '(e (flat) b (flat)) is equivalent to "e- b-". */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: (key-sig '(e (flat) b (flat))) e b", "test");
    int expect[] = {63, 70};
    int n = 0;
    for (int i = 0; i < ctx.event_count && n < 2; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            ASSERT_EQ(ctx.events[i].data1, expect[n]);
            n++;
        }
    }
    ASSERT_EQ(n, 2);
    alda_context_cleanup(&ctx);
}

TEST(interp_key_sig_sharp_tonic_scans_as_one_symbol) {
    /* "c#" must not be split by the comment scanner. C# minor has F#. */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: (key-sig '(c# minor)) f", "test");
    int n = 0;
    for (int i = 0; i < ctx.event_count; i++) {
        if (ctx.events[i].type == ALDA_EVT_NOTE_ON) {
            ASSERT_EQ(ctx.events[i].data1, 66);  /* F# */
            n++;
        }
    }
    ASSERT_EQ(n, 1);
    alda_context_cleanup(&ctx);
}

TEST(interp_global_key_sig_before_any_part) {
    /* (key-sig! ...) at the top of a score, before any part exists, must not be
     * discarded - and later parts inherit it. */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx,
        "(key-sig! \"f+ c+\")\npiano: f\nviolin: f", "test");
    ASSERT_EQ(first_pitch_for_part(&ctx, 0), 66);  /* F# */
    ASSERT_EQ(first_pitch_for_part(&ctx, 1), 66);  /* F# */
    alda_context_cleanup(&ctx);
}

TEST(interp_group_reuses_existing_parts) {
    /* An un-aliased group addresses the existing parts and keeps their state,
     * rather than creating fresh ones at the default octave. */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx,
        "violin \"vln\": o2\nviola \"vla\": o6\nvln/vla: c", "test");
    ASSERT_EQ(first_pitch_for_part(&ctx, 0), 36);  /* C2 */
    ASSERT_EQ(first_pitch_for_part(&ctx, 1), 84);  /* C6 */
    alda_context_cleanup(&ctx);
}

TEST(interp_aliased_group_creates_new_parts) {
    /* An alias names a distinct instance: "violin/viola \"s\"" is separate from
     * a plain "violin:" earlier in the score, so it starts at the default
     * octave rather than inheriting o2. */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "violin: o2 c\nviolin/viola \"s\": c", "test");
    ASSERT_EQ(first_pitch_for_part(&ctx, 0), 36);  /* the original violin, o2 */
    ASSERT_EQ(first_pitch_for_part(&ctx, 1), 60);  /* new instance, default o4 */
    alda_context_cleanup(&ctx);
}

TEST(interp_group_members_keep_own_octave) {
    /* Notes are resolved per part: a group whose members sit at different
     * octaves must not all play the first member's pitch. */
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: o2\nharp: o5\npiano/harp: c", "test");
    ASSERT_EQ(first_pitch_for_part(&ctx, 0), 36);  /* C2 */
    ASSERT_EQ(first_pitch_for_part(&ctx, 1), 72);  /* C5 */
    alda_context_cleanup(&ctx);
}

TEST(interp_group_chord_keeps_own_octave) {
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: o2\nharp: o5\npiano/harp: c/e", "test");
    ASSERT_EQ(first_pitch_for_part(&ctx, 0), 36);  /* C2 */
    ASSERT_EQ(first_pitch_for_part(&ctx, 1), 72);  /* C5 */
    alda_context_cleanup(&ctx);
}

TEST(interp_group_cram_keeps_own_octave) {
    AldaContext ctx;
    test_context_init(&ctx);
    alda_interpret_string(&ctx, "piano: o2\nharp: o5\npiano/harp: {c d e}", "test");
    ASSERT_EQ(first_pitch_for_part(&ctx, 0), 36);  /* C2 */
    ASSERT_EQ(first_pitch_for_part(&ctx, 1), 72);  /* C5 */
    alda_context_cleanup(&ctx);
}

/* ============================================================================
 * Conformance with Alda (docs/dev/conformance.md)
 *
 * Expected values are what `alda export` 2.4.7 produces for the same score,
 * at 480 ticks per quarter note and 120 BPM unless stated.
 * ============================================================================ */

/* Tick of the nth note-on of a pitch, or -1 */
static int note_on_tick(AldaContext* ctx, int pitch, int skip) {
    for (int i = 0; i < ctx->event_count; i++) {
        AldaScheduledEvent* e = &ctx->events[i];
        if (e->type == ALDA_EVT_NOTE_ON && e->data1 == pitch && skip-- == 0) {
            return e->tick;
        }
    }
    return -1;
}

/* Sounding length in ticks of the nth note of a pitch, or -1 */
static int note_length(AldaContext* ctx, int pitch, int skip) {
    for (int i = 0; i < ctx->event_count; i++) {
        AldaScheduledEvent* on = &ctx->events[i];
        if (on->type != ALDA_EVT_NOTE_ON || on->data1 != pitch || skip-- != 0) continue;
        for (int j = i + 1; j < ctx->event_count; j++) {
            AldaScheduledEvent* off = &ctx->events[j];
            if (off->type == ALDA_EVT_NOTE_OFF && off->data1 == pitch &&
                off->channel == on->channel) {
                return off->tick - on->tick;
            }
        }
    }
    return -1;
}

/* Program, or controller value, in effect on a channel at a tick; -1 if none.
 * Events are sorted, and settings precede note-ons at the same tick. */
static int setting_at(AldaContext* ctx, int channel, AldaEventType type,
                      int control, int tick) {
    int value = -1;
    for (int i = 0; i < ctx->event_count; i++) {
        AldaScheduledEvent* e = &ctx->events[i];
        if (e->tick > tick) break;
        if (e->type != type || e->channel != channel) continue;
        if (type == ALDA_EVT_CC && e->data1 != control) continue;
        value = (type == ALDA_EVT_CC) ? e->data2 : e->data1;
    }
    return value;
}

static AldaContext* conformance_run(const char* source) {
    AldaContext* ctx = malloc(sizeof(AldaContext));
    test_context_init(ctx);
    if (alda_interpret_string(ctx, source, "test") != 0) {
        alda_context_cleanup(ctx);
        free(ctx);
        return NULL;
    }
    return ctx;
}

static void conformance_free(AldaContext* ctx) {
    alda_context_cleanup(ctx);
    free(ctx);
}

/* P1: each part keeps its own tempo; the tempo map is the first part's */
TEST(conf_parts_keep_their_own_tempo) {
    AldaContext* ctx = conformance_run("violin: (tempo 100) c d\nviola: (tempo 200) c d");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(count_events(ctx, ALDA_EVT_TEMPO), 1);
    ASSERT_EQ(find_event(ctx, ALDA_EVT_TEMPO, 0)->data1, 100);
    /* At 100 BPM a tick is 1/800 s: viola's d at 0.3s, violin's at 0.6s */
    ASSERT_EQ(note_on_tick(ctx, 62, 0), 240);
    ASSERT_EQ(note_on_tick(ctx, 62, 1), 480);
    conformance_free(ctx);
}

TEST(conf_global_tempo_wins_over_a_local_one) {
    AldaContext* ctx = conformance_run("(tempo! 60) banjo: (tempo 180) c d");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(count_events(ctx, ALDA_EVT_TEMPO), 1);
    ASSERT_EQ(find_event(ctx, ALDA_EVT_TEMPO, 0)->data1, 60);
    /* The banjo still plays at 180: d after 1/3 s, 160 ticks at 60 BPM */
    ASSERT_EQ(note_on_tick(ctx, 62, 0), 160);
    conformance_free(ctx);
}

/* P2: chord notes keep their durations; the next event follows the shortest */
TEST(conf_chord_follows_shortest_note) {
    AldaContext* ctx = conformance_run("piano: c4./e4 g");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_length(ctx, 60, 0), 648);
    ASSERT_EQ(note_length(ctx, 64, 0), 432);
    ASSERT_EQ(note_on_tick(ctx, 67, 0), 480);
    ASSERT_EQ(note_length(ctx, 67, 0), 432);
    conformance_free(ctx);
}

TEST(conf_rest_in_chord_counts) {
    AldaContext* ctx = conformance_run("piano: c1/e/g/r4 b");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_on_tick(ctx, 71, 0), 480);
    conformance_free(ctx);
}

/* P3: voices start from a copy of the part and continue by number */
TEST(conf_voices_do_not_share_octave) {
    AldaContext* ctx = conformance_run("piano: V1: c > d V2: e f");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_on_tick(ctx, 64, 0), 0);
    ASSERT_EQ(note_on_tick(ctx, 65, 0), 480);
    ASSERT_EQ(note_on_tick(ctx, 74, 0), 480);
    conformance_free(ctx);
}

TEST(conf_repeated_voice_continues) {
    AldaContext* ctx = conformance_run("piano: V1: c1 V2: e2 V1: d1 V2: f2");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_on_tick(ctx, 62, 0), 1920);
    ASSERT_EQ(note_on_tick(ctx, 65, 0), 960);
    conformance_free(ctx);
}

TEST(conf_last_voice_to_finish_carries_on) {
    AldaContext* ctx = conformance_run(
        "piano: V1: c4 V2: (key-sig \"b-\") b2 V3: b4 V0: b");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_on_tick(ctx, 71, 0), 0);    /* V3 has no key signature */
    ASSERT_EQ(note_on_tick(ctx, 70, 1), 960);  /* V2 finished last */
    conformance_free(ctx);
}

/* P4: a rest sets the default duration */
TEST(conf_rest_sets_default_duration) {
    AldaContext* ctx = conformance_run("piano: r1 r c");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_on_tick(ctx, 60, 0), 3840);
    conformance_free(ctx);
}

/* P5, P6: a cram weighs repeats and brackets, and does not drift */
TEST(conf_cram_counts_repeats) {
    AldaContext* ctx = conformance_run("piano: {c [d e]*2}2 f");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_on_tick(ctx, 62, 0), 192);
    ASSERT_EQ(note_on_tick(ctx, 65, 0), 960);
    conformance_free(ctx);
}

TEST(conf_cram_positions_are_exact) {
    AldaContext* ctx = conformance_run("piano: {c c c}4 {c c c}4 {c c c}4 d");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_on_tick(ctx, 60, 4), 640);
    ASSERT_EQ(note_on_tick(ctx, 62, 0), 1440);
    conformance_free(ctx);
}

/* P7: a cram's duration becomes the default */
TEST(conf_cram_duration_becomes_default) {
    AldaContext* ctx = conformance_run("piano: c2 {d e}4 f");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_length(ctx, 65, 0), 432);
    conformance_free(ctx);
}

/* P8: slurred notes in a cram sound their full length */
TEST(conf_slur_in_cram_is_not_quantized) {
    AldaContext* ctx = conformance_run("piano: {a-~ b~ a-}4");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_length(ctx, 71, 0), 160);
    conformance_free(ctx);
}

/* P9: quant above 100 */
TEST(conf_quant_above_100) {
    AldaContext* ctx = conformance_run("piano: (quant 200) c8 d");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(note_length(ctx, 60, 0), 480);
    conformance_free(ctx);
}

/* P10: every channel starts with pan 64 and track volume 100 */
TEST(conf_channel_starts_with_pan_and_track_volume) {
    AldaContext* ctx = conformance_run("piano: c");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(setting_at(ctx, 0, ALDA_EVT_PAN, 0, 0), 64);
    ASSERT_EQ(setting_at(ctx, 0, ALDA_EVT_CC, 11, 0), 100);
    conformance_free(ctx);
}

TEST(conf_track_volume_is_cc11) {
    AldaContext* ctx = conformance_run("piano: c (track-volume 50) d");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(setting_at(ctx, 0, ALDA_EVT_CC, 11, 0), 100);
    ASSERT_EQ(setting_at(ctx, 0, ALDA_EVT_CC, 11, 480), 64);
    ASSERT_EQ(count_events(ctx, ALDA_EVT_CC), 2);
    conformance_free(ctx);
}

/* P11: Alda's instrument names and aliases */
TEST(conf_instrument_aliases) {
    const char* scores[] = {"guitar: c", "vibes: c", "upright-bass: c", "midi-bass+lead: c"};
    const int programs[] = {24, 11, 32, 87};
    for (int i = 0; i < 4; i++) {
        AldaContext* ctx = conformance_run(scores[i]);
        ASSERT_NOT_NULL(ctx);
        ASSERT_EQ(find_event(ctx, ALDA_EVT_PROGRAM, 0)->data1, programs[i]);
        conformance_free(ctx);
    }
}

/* P12: Alda rounds half away from zero */
TEST(conf_rounding) {
    AldaContext* ctx = conformance_run("piano: (pan 50) (vol 50) c");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(setting_at(ctx, 0, ALDA_EVT_PAN, 0, 0), 64);
    ASSERT_EQ(find_event(ctx, ALDA_EVT_NOTE_ON, 0)->data2, 64);
    conformance_free(ctx);
}

/* P13: beyond 15 parts, channels pass between parts and each note keeps its
 * instrument */
TEST(conf_channels_are_reused_over_time) {
    static const char* names[] = {
        "piano", "violin", "viola", "cello", "flute", "oboe", "clarinet",
        "bassoon", "trumpet", "trombone", "tuba", "harp", "celesta",
        "marimba", "vibraphone", "xylophone", "glockenspiel", "organ"
    };
    static const int programs[] = {0, 40, 41, 42, 73, 68, 71, 70, 56, 57, 58,
                                   46, 8, 12, 11, 13, 9, 19};
    char source[2048] = "";
    for (int i = 0; i < 18; i++) {
        char line[128];
        snprintf(line, sizeof(line), "%s: (set-note-length 1)", names[i]);
        strcat(source, line);
        for (int r = 0; r < i; r++) strcat(source, " r");
        strcat(source, " c\n");
    }
    AldaContext* ctx = conformance_run(source);
    ASSERT_NOT_NULL(ctx);
    for (int i = 0; i < 18; i++) {
        int tick = 1920 * i;
        int channel = -1;
        for (int e = 0; e < ctx->event_count; e++) {
            if (ctx->events[e].type == ALDA_EVT_NOTE_ON && ctx->events[e].tick == tick) {
                channel = ctx->events[e].channel;
            }
        }
        ASSERT_NEQ(channel, -1);
        ASSERT_EQ(setting_at(ctx, channel, ALDA_EVT_PROGRAM, 0, tick), programs[i]);
    }
    conformance_free(ctx);
}

/* P14: parts pinned to one channel each get their own instrument */
TEST(conf_midi_channel_shared_by_two_parts) {
    AldaContext* ctx = conformance_run(
        "piano: (midi-channel 2) c8 d\nguitar: (midi-channel 2) r4 e");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(find_event(ctx, ALDA_EVT_NOTE_ON, 0)->channel, 2);
    ASSERT_EQ(setting_at(ctx, 2, ALDA_EVT_PROGRAM, 0, note_on_tick(ctx, 62, 0)), 0);
    ASSERT_EQ(setting_at(ctx, 2, ALDA_EVT_PROGRAM, 0, note_on_tick(ctx, 64, 0)), 24);
    conformance_free(ctx);
}

/* After a voice group, a note still sounding keeps sounding when the part
 * plays its pitch again: the part moves to a channel of its own, as in Alda */
TEST(conf_note_outlasting_a_voice_group_is_not_cut) {
    AldaContext* ctx = conformance_run("piano: (quant 200) V1: c2 V2: e2 V0: d4 c4");
    ASSERT_NOT_NULL(ctx);
    int first = find_note_on(ctx, 60)->channel;
    AldaScheduledEvent* second = NULL;
    for (int i = 0; i < ctx->event_count; i++) {
        AldaScheduledEvent* e = &ctx->events[i];
        if (e->type == ALDA_EVT_NOTE_ON && e->data1 == 60 && e->tick == 1440) second = e;
    }
    ASSERT_NOT_NULL(second);
    ASSERT_NEQ(second->channel, first);
    ASSERT_EQ(note_length(ctx, 60, 0), 1920);  /* The first c sounds its full 2s */
    ASSERT_EQ(setting_at(ctx, second->channel, ALDA_EVT_PROGRAM, 0, 1440), 0);
    conformance_free(ctx);
}

/* Without such an overlap the part keeps its channel, and a unison inside a
 * group stays on one channel, as in Alda */
TEST(conf_part_keeps_channel_after_voices_when_it_can) {
    const char* scores[] = {"piano: V1: c2 V2: e2 V0: c4",
                            "piano: (quant 200) V1: c2 V2: c2 V0: d4"};
    for (int s = 0; s < 2; s++) {
        AldaContext* ctx = conformance_run(scores[s]);
        ASSERT_NOT_NULL(ctx);
        for (int i = 0; i < ctx->event_count; i++) {
            if (ctx->events[i].type == ALDA_EVT_NOTE_ON) ASSERT_EQ(ctx->events[i].channel, 0);
        }
        conformance_free(ctx);
    }
}

/* P15: no program change on the drum channel */
TEST(conf_no_program_change_for_percussion) {
    AldaContext* ctx = conformance_run("midi-percussion: c d");
    ASSERT_NOT_NULL(ctx);
    ASSERT_EQ(count_events(ctx, ALDA_EVT_PROGRAM), 0);
    ASSERT_EQ(find_event(ctx, ALDA_EVT_NOTE_ON, 0)->channel, 9);
    conformance_free(ctx);
}

BEGIN_TEST_SUITE("Alda Interpreter")
    /* Basic notes */
    RUN_TEST(interpret_single_note);
    RUN_TEST(interpret_note_with_accidentals);
    RUN_TEST(interpret_note_with_octave);
    RUN_TEST(interpret_octave_up_down);
    RUN_TEST(interpret_note_sequence);

    /* Durations */
    RUN_TEST(interpret_note_durations);
    RUN_TEST(interpret_dotted_duration);
    RUN_TEST(interpret_tied_duration);

    /* Rests */
    RUN_TEST(interpret_rest);

    /* Chords */
    RUN_TEST(interpret_chord);
    RUN_TEST(interpret_chord_with_octave_change);

    /* Tempo */
    RUN_TEST(interpret_tempo_attribute);
    RUN_TEST(interpret_tempo_change_mid_score);

    /* Volume/Dynamics */
    RUN_TEST(interpret_volume_attribute);
    RUN_TEST(interpret_dynamics);

    /* Repeats */
    RUN_TEST(interpret_simple_repeat);
    RUN_TEST(interpret_repeat_sequence);
    RUN_TEST(interpret_alternate_endings);

    /* Variables */
    RUN_TEST(interpret_variable_definition_and_reference);
    RUN_TEST(interpret_variable_redefine);

    /* Markers */
    RUN_TEST(interpret_marker_and_at_marker);

    /* Voices (Polyphony) */
    RUN_TEST(interpret_voices);
    RUN_TEST(interpret_voice_timing);

    /* Cram */
    RUN_TEST(interpret_cram_basic);

    /* Key Signature */
    RUN_TEST(interpret_key_signature);
    RUN_TEST(interpret_natural_overrides_key_sig);

    /* Transpose */
    RUN_TEST(interpret_transpose);
    RUN_TEST(interpret_transpose_negative);

    /* Pan */
    RUN_TEST(interpret_pan);

    /* Quantization */
    RUN_TEST(interpret_quantization);

    /* Multiple Parts */
    RUN_TEST(interpret_multiple_parts);
    RUN_TEST(interpret_part_group);

    /* Program Changes */
    RUN_TEST(interpret_program_change);

    /* Error handling */
    RUN_TEST(interpret_undefined_variable_error);
    RUN_TEST(interpret_undefined_marker_error);
    RUN_TEST(interpret_no_part_error);

    /* Pitch calculation unit tests */
    RUN_TEST(calculate_pitch_basic);
    RUN_TEST(calculate_pitch_octaves);
    RUN_TEST(calculate_pitch_accidentals);
    RUN_TEST(calculate_pitch_with_key_sig);

    /* Duration calculation unit tests */
    RUN_TEST(duration_to_ticks_basic);
    RUN_TEST(duration_to_ticks_dotted);
    RUN_TEST(ms_to_ticks);
    RUN_TEST(apply_quant);
    /* Divergences found against aldakit */
    RUN_TEST(interp_key_sig_quoted_list_with_accidental_word);
    RUN_TEST(interp_key_sig_key_name_with_accidental_word);
    RUN_TEST(interp_key_sig_bare_accidental_words);
    RUN_TEST(interp_key_sig_per_note_accidental_form);
    RUN_TEST(interp_key_sig_sharp_tonic_scans_as_one_symbol);
    RUN_TEST(interp_global_key_sig_before_any_part);
    RUN_TEST(interp_group_reuses_existing_parts);
    RUN_TEST(interp_aliased_group_creates_new_parts);
    RUN_TEST(interp_group_members_keep_own_octave);
    RUN_TEST(interp_group_chord_keeps_own_octave);
    RUN_TEST(interp_group_cram_keeps_own_octave);
    RUN_TEST(conf_parts_keep_their_own_tempo);
    RUN_TEST(conf_global_tempo_wins_over_a_local_one);
    RUN_TEST(conf_chord_follows_shortest_note);
    RUN_TEST(conf_rest_in_chord_counts);
    RUN_TEST(conf_voices_do_not_share_octave);
    RUN_TEST(conf_repeated_voice_continues);
    RUN_TEST(conf_last_voice_to_finish_carries_on);
    RUN_TEST(conf_rest_sets_default_duration);
    RUN_TEST(conf_cram_counts_repeats);
    RUN_TEST(conf_cram_positions_are_exact);
    RUN_TEST(conf_cram_duration_becomes_default);
    RUN_TEST(conf_slur_in_cram_is_not_quantized);
    RUN_TEST(conf_quant_above_100);
    RUN_TEST(conf_channel_starts_with_pan_and_track_volume);
    RUN_TEST(conf_track_volume_is_cc11);
    RUN_TEST(conf_instrument_aliases);
    RUN_TEST(conf_rounding);
    RUN_TEST(conf_channels_are_reused_over_time);
    RUN_TEST(conf_midi_channel_shared_by_two_parts);
    RUN_TEST(conf_no_program_change_for_percussion);
    RUN_TEST(conf_note_outlasting_a_voice_group_is_not_cut);
    RUN_TEST(conf_part_keeps_channel_after_voices_when_it_can);
END_TEST_SUITE()
