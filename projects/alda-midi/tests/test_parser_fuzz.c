/**
 * @file test_parser_fuzz.c
 * @brief Truncation fuzz tests for Alda parser EOF recovery.
 *
 * Feeds valid Alda strings to the parser, truncated at every byte position,
 * and asserts no crash. Validates that the parser handles unexpected EOF
 * gracefully at all points in the input.
 */

#include "test_framework.h"
#include <alda/ast.h>
#include <alda/parser.h>
#include <string.h>
#include <stdlib.h>

/* Parse truncated input and assert no crash. Returns 1 if no crash. */
static int parse_truncated(const char* source, size_t len) {
    char* buf = (char*)malloc(len + 1);
    if (!buf) return 0;
    memcpy(buf, source, len);
    buf[len] = '\0';

    char* error = NULL;
    AldaNode* ast = alda_parse(buf, "fuzz", &error);

    /* We don't care whether it parsed successfully or returned an error.
     * We only care that it didn't crash. */
    if (ast) alda_node_free(ast);
    if (error) free(error);
    free(buf);
    return 1;
}

/* Truncate source at every byte position and parse each truncation. */
static void fuzz_all_truncations(const char* source) {
    size_t len = strlen(source);
    for (size_t i = 0; i <= len; i++) {
        ASSERT_TRUE(parse_truncated(source, i));
    }
}

/* ============================================================================
 * Basic constructs
 * ============================================================================ */

TEST(fuzz_simple_note) {
    fuzz_all_truncations("piano: c d e f g a b");
}

TEST(fuzz_note_with_octave) {
    fuzz_all_truncations("piano: o4 c o5 d > e < f");
}

TEST(fuzz_note_with_duration) {
    fuzz_all_truncations("piano: c4 d8 e2. f16~16");
}

TEST(fuzz_note_with_accidentals) {
    fuzz_all_truncations("piano: c+ d- e++ f-- g_");
}

TEST(fuzz_rest) {
    fuzz_all_truncations("piano: c r4 d r8 e");
}

/* ============================================================================
 * Chords and voices
 * ============================================================================ */

TEST(fuzz_chord) {
    fuzz_all_truncations("piano: c/e/g c+/e/g+");
}

TEST(fuzz_voices) {
    fuzz_all_truncations("piano: V1: c d e V2: e f g");
}

/* ============================================================================
 * S-expressions (lisp calls)
 * ============================================================================ */

TEST(fuzz_sexp_tempo) {
    fuzz_all_truncations("piano: (tempo 120) c d e");
}

TEST(fuzz_sexp_nested) {
    fuzz_all_truncations("piano: (volume 80) (panning 50)");
}

TEST(fuzz_sexp_with_string) {
    fuzz_all_truncations("piano: (key-sig \"f+ c+ g+\")");
}

/* ============================================================================
 * Variables and markers
 * ============================================================================ */

TEST(fuzz_variable_def) {
    fuzz_all_truncations("motif = c d e f g");
}

TEST(fuzz_variable_ref) {
    fuzz_all_truncations("piano: motif");
}

TEST(fuzz_marker) {
    fuzz_all_truncations("piano: %chorus c d e @chorus f g a");
}

/* ============================================================================
 * Cram and brackets
 * ============================================================================ */

TEST(fuzz_cram) {
    fuzz_all_truncations("piano: {c d e}2");
}

TEST(fuzz_brackets) {
    fuzz_all_truncations("piano: [c d e]*3");
}

TEST(fuzz_nested_cram) {
    fuzz_all_truncations("piano: {c {d e} f}4");
}

/* ============================================================================
 * Repetition and on-reps
 * ============================================================================ */

TEST(fuzz_repeat) {
    fuzz_all_truncations("piano: [c d e]*4");
}

TEST(fuzz_on_reps) {
    fuzz_all_truncations("piano: [c d e'1 f'2]*2");
}

/* ============================================================================
 * Multi-part and complex expressions
 * ============================================================================ */

TEST(fuzz_multi_part) {
    fuzz_all_truncations("piano: c d e\nviolin: f g a");
}

TEST(fuzz_complex_expression) {
    fuzz_all_truncations("piano/violin: (tempo 120) o4 c8 d e+ f2. r4 {g a b}2 [c d]*3");
}

TEST(fuzz_multiline_complex) {
    fuzz_all_truncations(
        "piano:\n"
        "  o4 c8 d e f | g a b > c\n"
        "  (volume 80)\n"
        "  < c2 r4 d\n"
    );
}

/* ============================================================================
 * Duration edge cases
 * ============================================================================ */

TEST(fuzz_duration_ms) {
    fuzz_all_truncations("piano: c500ms d1000ms");
}

TEST(fuzz_duration_seconds) {
    fuzz_all_truncations("piano: c2s d0.5s");
}

TEST(fuzz_tied_duration) {
    fuzz_all_truncations("piano: c4~8~16");
}

/* ============================================================================
 * Comments and whitespace
 * ============================================================================ */

TEST(fuzz_with_comments) {
    fuzz_all_truncations("piano: c d e # this is a comment\nf g a");
}

TEST(fuzz_empty_and_whitespace) {
    fuzz_all_truncations("   \n\n  \t  \n");
}

/* ============================================================================
 * Test Suite
 * ============================================================================ */

BEGIN_TEST_SUITE("Alda Parser Fuzz Tests")
    RUN_TEST(fuzz_simple_note);
    RUN_TEST(fuzz_note_with_octave);
    RUN_TEST(fuzz_note_with_duration);
    RUN_TEST(fuzz_note_with_accidentals);
    RUN_TEST(fuzz_rest);
    RUN_TEST(fuzz_chord);
    RUN_TEST(fuzz_voices);
    RUN_TEST(fuzz_sexp_tempo);
    RUN_TEST(fuzz_sexp_nested);
    RUN_TEST(fuzz_sexp_with_string);
    RUN_TEST(fuzz_variable_def);
    RUN_TEST(fuzz_variable_ref);
    RUN_TEST(fuzz_marker);
    RUN_TEST(fuzz_cram);
    RUN_TEST(fuzz_brackets);
    RUN_TEST(fuzz_nested_cram);
    RUN_TEST(fuzz_repeat);
    RUN_TEST(fuzz_on_reps);
    RUN_TEST(fuzz_multi_part);
    RUN_TEST(fuzz_complex_expression);
    RUN_TEST(fuzz_multiline_complex);
    RUN_TEST(fuzz_duration_ms);
    RUN_TEST(fuzz_duration_seconds);
    RUN_TEST(fuzz_tied_duration);
    RUN_TEST(fuzz_with_comments);
    RUN_TEST(fuzz_empty_and_whitespace);
END_TEST_SUITE()
