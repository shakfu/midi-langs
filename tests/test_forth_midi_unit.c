/* test_forth_midi_unit.c - Unit tests for MIDI Forth interpreter */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

/* Include the header to get access to functions */
#include "../projects/forth-midi/forth_midi.h"

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name) static void test_##name(void)
#define RUN_TEST(name) do { \
    printf("  %s... ", #name); \
    tests_run++; \
    test_##name(); \
    tests_passed++; \
    printf("ok\n"); \
} while(0)

#define ASSERT_EQ(a, b) do { \
    if ((a) != (b)) { \
        printf("FAILED: %s != %s (%d != %d)\n", #a, #b, (int)(a), (int)(b)); \
        exit(1); \
    } \
} while(0)

#define ASSERT_STR_EQ(a, b) do { \
    if (strcmp((a), (b)) != 0) { \
        printf("FAILED: %s != %s (\"%s\" != \"%s\")\n", #a, #b, (a), (b)); \
        exit(1); \
    } \
} while(0)

/* ============================================================================
 * parse_pitch tests
 * ============================================================================ */

TEST(parse_pitch_c4) {
    ASSERT_EQ(parse_pitch("c4"), 60);
}

TEST(parse_pitch_C4) {
    ASSERT_EQ(parse_pitch("C4"), 60);
}

TEST(parse_pitch_a4) {
    ASSERT_EQ(parse_pitch("a4"), 69);
}

TEST(parse_pitch_c0) {
    ASSERT_EQ(parse_pitch("c0"), 12);
}

TEST(parse_pitch_c5) {
    ASSERT_EQ(parse_pitch("c5"), 72);
}

TEST(parse_pitch_sharp) {
    ASSERT_EQ(parse_pitch("c#4"), 61);
    ASSERT_EQ(parse_pitch("C#4"), 61);
    ASSERT_EQ(parse_pitch("f#3"), 54);
}

TEST(parse_pitch_flat) {
    ASSERT_EQ(parse_pitch("db4"), 61);
    ASSERT_EQ(parse_pitch("Db4"), 61);
    ASSERT_EQ(parse_pitch("bb3"), 58);
}

TEST(parse_pitch_articulation_staccato) {
    /* Staccato suffix should still parse the pitch */
    int pitch = parse_pitch("c4.");
    ASSERT_EQ(pitch, 60);
}

TEST(parse_pitch_articulation_accent) {
    int pitch = parse_pitch("e4>");
    ASSERT_EQ(pitch, 64);
}

TEST(parse_pitch_invalid) {
    ASSERT_EQ(parse_pitch("xyz"), -1);
    ASSERT_EQ(parse_pitch(""), -1);
    ASSERT_EQ(parse_pitch("c"), -1);  /* No octave */
}

/* ============================================================================
 * Packed note tests
 * ============================================================================ */

TEST(pack_note_basic) {
    int32_t n = pack_note(60, 80, 1, 500);
    ASSERT_EQ(note_pitch(n), 60);
    ASSERT_EQ(note_vel(n), 80);
    ASSERT_EQ(note_ch(n), 1);
    ASSERT_EQ(note_dur(n), 500);
}

TEST(pack_note_edge_values) {
    /* Test with edge values */
    int32_t n = pack_note(127, 127, 15, 16383);
    ASSERT_EQ(note_pitch(n), 127);
    ASSERT_EQ(note_vel(n), 127);
    ASSERT_EQ(note_ch(n), 15);
    ASSERT_EQ(note_dur(n), 16383);
}

TEST(pack_note_zero_values) {
    int32_t n = pack_note(0, 0, 0, 0);
    ASSERT_EQ(note_pitch(n), 0);
    ASSERT_EQ(note_vel(n), 0);
    ASSERT_EQ(note_ch(n), 0);
    ASSERT_EQ(note_dur(n), 0);
}

/* ============================================================================
 * Stack tests
 * ============================================================================ */

TEST(stack_push_pop) {
    Stack s;
    s.top = -1;

    push(&s, 42);
    ASSERT_EQ(s.top, 0);
    ASSERT_EQ(peek(&s), 42);

    push(&s, 100);
    ASSERT_EQ(s.top, 1);
    ASSERT_EQ(peek(&s), 100);

    int32_t val = pop(&s);
    ASSERT_EQ(val, 100);
    ASSERT_EQ(s.top, 0);

    val = pop(&s);
    ASSERT_EQ(val, 42);
    ASSERT_EQ(s.top, -1);
}

TEST(stack_operations) {
    Stack s;
    s.top = -1;

    /* Test dup */
    push(&s, 5);
    push(&s, peek(&s));  /* dup */
    ASSERT_EQ(s.top, 1);
    ASSERT_EQ(s.data[0], 5);
    ASSERT_EQ(s.data[1], 5);

    /* Clear */
    s.top = -1;

    /* Test swap */
    push(&s, 1);
    push(&s, 2);
    int32_t b = pop(&s);
    int32_t a = pop(&s);
    push(&s, b);
    push(&s, a);
    ASSERT_EQ(s.data[0], 2);
    ASSERT_EQ(s.data[1], 1);
}

/* ============================================================================
 * Timing/tick conversion tests
 * ============================================================================ */

TEST(tick_to_ms_conversion) {
    /* Test tick to ms conversion at 120 BPM with 480 PPQN */
    /* 480 ticks = 1 quarter note = 500ms at 120 BPM */
    int bpm = 120;
    int ppqn = TICKS_PER_QUARTER;  /* 480 */
    int ticks = 480;
    int ms = (ticks * 60000) / (ppqn * bpm);
    ASSERT_EQ(ms, 500);

    /* At 60 BPM, 480 ticks = 1000ms */
    bpm = 60;
    ms = (ticks * 60000) / (ppqn * bpm);
    ASSERT_EQ(ms, 1000);
}

TEST(ms_to_tick_conversion) {
    /* Test ms to tick conversion */
    int bpm = 120;
    int ppqn = TICKS_PER_QUARTER;
    int ms = 500;
    int ticks = (ms * bpm * ppqn) / 60000;
    ASSERT_EQ(ticks, 480);
}

/* ============================================================================
 * Main
 * ============================================================================ */

int main(void) {
    printf("Running MIDI Forth unit tests...\n\n");

    /* Initialize context for tests that need it */
    forth_context_init(&g_ctx);

    printf("parse_pitch tests:\n");
    RUN_TEST(parse_pitch_c4);
    RUN_TEST(parse_pitch_C4);
    RUN_TEST(parse_pitch_a4);
    RUN_TEST(parse_pitch_c0);
    RUN_TEST(parse_pitch_c5);
    RUN_TEST(parse_pitch_sharp);
    RUN_TEST(parse_pitch_flat);
    RUN_TEST(parse_pitch_articulation_staccato);
    RUN_TEST(parse_pitch_articulation_accent);
    RUN_TEST(parse_pitch_invalid);

    printf("\npacked note tests:\n");
    RUN_TEST(pack_note_basic);
    RUN_TEST(pack_note_edge_values);
    RUN_TEST(pack_note_zero_values);

    printf("\nstack tests:\n");
    RUN_TEST(stack_push_pop);
    RUN_TEST(stack_operations);

    printf("\ntiming tests:\n");
    RUN_TEST(tick_to_ms_conversion);
    RUN_TEST(ms_to_tick_conversion);

    printf("\n%d/%d tests passed\n", tests_passed, tests_run);

    return tests_passed == tests_run ? 0 : 1;
}
