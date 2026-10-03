/**
 * @file test_scanner.c
 * @brief Unit tests for Alda scanner/lexer.
 *
 * Tests tokenization for all token types and vulnerability edge cases.
 */

#include "test_framework.h"
#include <alda/scanner.h>
#include <alda/tokens.h>
#include <string.h>
#include <stdlib.h>
#include <stdio.h>

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

/* Find first token of given type */
static AldaToken* find_token_type(AldaToken* tokens, size_t count, AldaTokenType type) {
    for (size_t i = 0; i < count; i++) {
        if (tokens[i].type == type) {
            return &tokens[i];
        }
    }
    return NULL;
}

/* ============================================================================
 * Note Letter Tests
 * ============================================================================ */

TEST(scan_note_letter_c) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_TRUE(count >= 1);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[0].literal.char_val, 'c');
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_all_note_letters) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c d e f g a b", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);

    int note_count = 0;
    const char expected[] = {'c', 'd', 'e', 'f', 'g', 'a', 'b'};
    int expected_idx = 0;

    for (size_t i = 0; i < count; i++) {
        if (tokens[i].type == ALDA_TOK_NOTE_LETTER) {
            ASSERT_EQ(tokens[i].literal.char_val, expected[expected_idx++]);
            note_count++;
        }
    }
    ASSERT_EQ(note_count, 7);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Rest Tests
 * ============================================================================ */

TEST(scan_rest_letter) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("r", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_TRUE(count >= 1);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_REST_LETTER);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_rest_with_duration) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("r4", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_TRUE(count >= 2);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_REST_LETTER);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_NOTE_LENGTH);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Accidental Tests
 * ============================================================================ */

TEST(scan_sharp_plus) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c+", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_SHARP);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_flat) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c-", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_FLAT);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_natural) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c_", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_NATURAL);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_double_sharp) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c++", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_SHARP);
    ASSERT_EQ(tokens[2].type, ALDA_TOK_SHARP);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_double_flat) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c--", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_FLAT);
    ASSERT_EQ(tokens[2].type, ALDA_TOK_FLAT);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_suffix_sharp_s) {
    /* Suffix accidental 's' for sharp (e.g., cs = c sharp)
     * Note: needs trailing content for pending token to be emitted */
    size_t count;
    AldaScanner* scanner = alda_scanner_new("cs d", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[0].literal.char_val, 'c');
    ASSERT_EQ(tokens[1].type, ALDA_TOK_SHARP);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_suffix_flat_b) {
    /* Suffix accidental 'b' for flat (e.g., db = d flat)
     * Note: needs trailing content for pending token to be emitted */
    size_t count;
    AldaScanner* scanner = alda_scanner_new("db e", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LETTER);
    ASSERT_EQ(tokens[0].literal.char_val, 'd');
    ASSERT_EQ(tokens[1].type, ALDA_TOK_FLAT);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Octave Tests
 * ============================================================================ */

TEST(scan_octave_set) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("o4", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_OCTAVE_SET);
    ASSERT_EQ(tokens[0].literal.int_val, 4);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_octave_up) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new(">", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_OCTAVE_UP);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_octave_down) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("<", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_OCTAVE_DOWN);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Note Length Tests
 * ============================================================================ */

TEST(scan_note_length_basic) {
    /* Note-length literals are carried as float_val - the literal is a union,
     * and lengths may be fractional (see scan_note_length_fractional). */
    const int lengths[] = {1, 2, 4, 8, 16, 32};
    for (int i = 0; i < 6; i++) {
        char source[8];
        snprintf(source, sizeof(source), "%d", lengths[i]);
        size_t count;
        AldaScanner* scanner = alda_scanner_new(source, "test");
        ASSERT_NOT_NULL(scanner);
        AldaToken* tokens = alda_scanner_scan(scanner, &count);
        ASSERT_FALSE(alda_scanner_has_error(scanner));
        ASSERT_NOT_NULL(tokens);
        ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LENGTH);
        ASSERT_EQ((int)tokens[0].literal.float_val, lengths[i]);
        alda_tokens_free(tokens, count);
        alda_scanner_free(scanner);
    }
}

TEST(scan_note_length_fractional) {
    /* "c0.25" is a quadruple-whole note. The '.' is part of the number because
     * a digit follows it. */
    size_t count;
    AldaScanner* scanner = alda_scanner_new("0.25", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LENGTH);
    ASSERT_TRUE(tokens[0].literal.float_val > 0.24 &&
                tokens[0].literal.float_val < 0.26);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_dot_after_length_is_augmentation) {
    /* "4." is a dotted quarter: no digit follows the dot, so it stays a
     * separate DOT token rather than being absorbed into the number. */
    size_t count;
    AldaScanner* scanner = alda_scanner_new("4.", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LENGTH);
    ASSERT_EQ((int)tokens[0].literal.float_val, 4);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_DOT);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_hash_is_always_a_comment) {
    /* A banner comment must not scan as a sharp accidental. */
    size_t count;
    AldaScanner* scanner = alda_scanner_new("######\nc", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NEWLINE);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_NOTE_LETTER);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_dot_accessor_name) {
    /* "strings.cello" is one NAME, not NAME DOT NAME. */
    size_t count;
    AldaScanner* scanner = alda_scanner_new("strings.cello", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NAME);
    ASSERT_STR_EQ(tokens[0].lexeme, "strings.cello");
    ASSERT_EQ(tokens[1].type, ALDA_TOK_EOF);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_note_length_ms) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("500ms", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LENGTH_MS);
    ASSERT_EQ(tokens[0].literal.int_val, 500);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_note_length_s) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("2s", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LENGTH_S);
    ASSERT_TRUE(tokens[0].literal.float_val > 1.9 && tokens[0].literal.float_val < 2.1);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_dotted_note) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("4.", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NOTE_LENGTH);
    ASSERT_EQ(tokens[1].type, ALDA_TOK_DOT);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Structural Token Tests
 * ============================================================================ */

TEST(scan_tie) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("~", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_TIE);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_barline) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("|", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_BARLINE);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_separator) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("/", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_SEPARATOR);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_colon) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new(":", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_COLON);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_equals) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("=", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_EQUALS);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Bracket Tests
 * ============================================================================ */

TEST(scan_cram_brackets) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("{c d e}", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_CRAM_OPEN);
    AldaToken* close = find_token_type(tokens, count, ALDA_TOK_CRAM_CLOSE);
    ASSERT_NOT_NULL(close);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_square_brackets) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("[c d e]", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_BRACKET_OPEN);
    AldaToken* close = find_token_type(tokens, count, ALDA_TOK_BRACKET_CLOSE);
    ASSERT_NOT_NULL(close);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Marker Tests
 * ============================================================================ */

TEST(scan_marker) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("%verse", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_MARKER);
    ASSERT_STR_EQ(tokens[0].lexeme, "%verse");
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_at_marker) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("@chorus", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_AT_MARKER);
    ASSERT_STR_EQ(tokens[0].lexeme, "@chorus");
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_voice_marker) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("V1:", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_VOICE_MARKER);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Repeat Tests
 * ============================================================================ */

TEST(scan_repeat) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("*3", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_REPEAT);
    ASSERT_EQ(tokens[0].literal.int_val, 3);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_repetitions) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("'1-3,5", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_REPETITIONS);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Name/Identifier Tests
 * ============================================================================ */

TEST(scan_instrument_name) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("piano", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_NAME);
    ASSERT_STR_EQ(tokens[0].lexeme, "piano");
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_alias) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("\"left-hand\"", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_ALIAS);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * S-expression Mode Tests
 * ============================================================================ */

TEST(scan_sexp_simple) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("(tempo 120)", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_EQ(tokens[0].type, ALDA_TOK_LEFT_PAREN);
    AldaToken* rparen = find_token_type(tokens, count, ALDA_TOK_RIGHT_PAREN);
    ASSERT_NOT_NULL(rparen);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_sexp_symbol) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("(tempo)", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    AldaToken* sym = find_token_type(tokens, count, ALDA_TOK_SYMBOL);
    ASSERT_NOT_NULL(sym);
    ASSERT_STR_EQ(sym->lexeme, "tempo");
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_sexp_number) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("(tempo 120)", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    AldaToken* num = find_token_type(tokens, count, ALDA_TOK_NUMBER);
    ASSERT_NOT_NULL(num);
    ASSERT_TRUE(num->literal.float_val > 119.9 && num->literal.float_val < 120.1);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_sexp_string) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("(key \"c major\")", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    AldaToken* str = find_token_type(tokens, count, ALDA_TOK_STRING);
    ASSERT_NOT_NULL(str);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_sexp_nested) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("(volume (+ 50 25))", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    int lparen_count = 0, rparen_count = 0;
    for (size_t i = 0; i < count; i++) {
        if (tokens[i].type == ALDA_TOK_LEFT_PAREN) lparen_count++;
        if (tokens[i].type == ALDA_TOK_RIGHT_PAREN) rparen_count++;
    }
    ASSERT_EQ(lparen_count, 2);
    ASSERT_EQ(rparen_count, 2);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Empty/Whitespace Tests
 * ============================================================================ */

TEST(scan_empty) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_NOT_NULL(tokens);
    ASSERT_TRUE(count >= 1);
    ASSERT_EQ(tokens[count - 1].type, ALDA_TOK_EOF);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_whitespace_only) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("   \t  \n  ", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_NOT_NULL(tokens);
    ASSERT_TRUE(count >= 1);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Vulnerability Tests
 * ============================================================================ */

TEST(scan_unmatched_close_paren) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("c ) d", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    /* Should not crash */
    alda_scanner_free(scanner);
    if (tokens) alda_tokens_free(tokens, count);
}

TEST(scan_very_long_number) {
    char source[1024];
    memset(source, '9', 1000);
    source[1000] = '\0';

    size_t count;
    AldaScanner* scanner = alda_scanner_new(source, "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    /* Should not crash */
    alda_scanner_free(scanner);
    if (tokens) alda_tokens_free(tokens, count);
}

TEST(scan_very_long_identifier) {
    char source[2048];
    source[0] = 'x';
    for (int i = 1; i < 2000; i++) {
        source[i] = 'a';
    }
    source[2000] = '\0';

    size_t count;
    AldaScanner* scanner = alda_scanner_new(source, "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    /* Should not crash */
    alda_scanner_free(scanner);
    if (tokens) alda_tokens_free(tokens, count);
}

TEST(scan_unterminated_string) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("\"hello", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_TRUE(alda_scanner_has_error(scanner));
    alda_scanner_free(scanner);
    if (tokens) alda_tokens_free(tokens, count);
}

TEST(scan_deeply_nested_sexp) {
    char source[512];
    int pos = 0;
    for (int i = 0; i < 50; i++) {
        source[pos++] = '(';
    }
    source[pos++] = 'x';
    for (int i = 0; i < 50; i++) {
        source[pos++] = ')';
    }
    source[pos] = '\0';

    size_t count;
    AldaScanner* scanner = alda_scanner_new(source, "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    /* Should not crash */
    alda_scanner_free(scanner);
    if (tokens) alda_tokens_free(tokens, count);
}

/* ============================================================================
 * Complex Expression Tests
 * ============================================================================ */

TEST(scan_complete_measure) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("o4 c4 d8 e16 r4 | f/a/c1", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    ASSERT_TRUE(count > 10);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

TEST(scan_part_declaration) {
    size_t count;
    AldaScanner* scanner = alda_scanner_new("piano \"melody\":", "test");
    ASSERT_NOT_NULL(scanner);
    AldaToken* tokens = alda_scanner_scan(scanner, &count);
    ASSERT_FALSE(alda_scanner_has_error(scanner));
    ASSERT_NOT_NULL(tokens);
    AldaToken* name = find_token_type(tokens, count, ALDA_TOK_NAME);
    ASSERT_NOT_NULL(name);
    ASSERT_STR_EQ(name->lexeme, "piano");
    AldaToken* alias = find_token_type(tokens, count, ALDA_TOK_ALIAS);
    ASSERT_NOT_NULL(alias);
    AldaToken* colon = find_token_type(tokens, count, ALDA_TOK_COLON);
    ASSERT_NOT_NULL(colon);
    alda_tokens_free(tokens, count);
    alda_scanner_free(scanner);
}

/* ============================================================================
 * Test Runner
 * ============================================================================ */

BEGIN_TEST_SUITE("Alda Scanner Tests")

    /* Note letters */
    RUN_TEST(scan_note_letter_c);
    RUN_TEST(scan_all_note_letters);

    /* Rests */
    RUN_TEST(scan_rest_letter);
    RUN_TEST(scan_rest_with_duration);

    /* Accidentals */
    RUN_TEST(scan_sharp_plus);
    RUN_TEST(scan_flat);
    RUN_TEST(scan_natural);
    RUN_TEST(scan_double_sharp);
    RUN_TEST(scan_double_flat);
    RUN_TEST(scan_suffix_sharp_s);
    RUN_TEST(scan_suffix_flat_b);

    /* Octaves */
    RUN_TEST(scan_octave_set);
    RUN_TEST(scan_octave_up);
    RUN_TEST(scan_octave_down);

    /* Note lengths */
    RUN_TEST(scan_note_length_basic);
    RUN_TEST(scan_note_length_fractional);
    RUN_TEST(scan_dot_after_length_is_augmentation);
    RUN_TEST(scan_hash_is_always_a_comment);
    RUN_TEST(scan_dot_accessor_name);
    RUN_TEST(scan_note_length_ms);
    RUN_TEST(scan_note_length_s);
    RUN_TEST(scan_dotted_note);

    /* Structural tokens */
    RUN_TEST(scan_tie);
    RUN_TEST(scan_barline);
    RUN_TEST(scan_separator);
    RUN_TEST(scan_colon);
    RUN_TEST(scan_equals);

    /* Brackets */
    RUN_TEST(scan_cram_brackets);
    RUN_TEST(scan_square_brackets);

    /* Markers */
    RUN_TEST(scan_marker);
    RUN_TEST(scan_at_marker);
    RUN_TEST(scan_voice_marker);

    /* Repeat */
    RUN_TEST(scan_repeat);
    RUN_TEST(scan_repetitions);

    /* Names */
    RUN_TEST(scan_instrument_name);
    RUN_TEST(scan_alias);

    /* S-expressions */
    RUN_TEST(scan_sexp_simple);
    RUN_TEST(scan_sexp_symbol);
    RUN_TEST(scan_sexp_number);
    RUN_TEST(scan_sexp_string);
    RUN_TEST(scan_sexp_nested);

    /* Empty/whitespace */
    RUN_TEST(scan_empty);
    RUN_TEST(scan_whitespace_only);

    /* Vulnerability tests */
    RUN_TEST(scan_unmatched_close_paren);
    RUN_TEST(scan_very_long_number);
    RUN_TEST(scan_very_long_identifier);
    RUN_TEST(scan_unterminated_string);
    RUN_TEST(scan_deeply_nested_sexp);

    /* Complex expressions */
    RUN_TEST(scan_complete_measure);
    RUN_TEST(scan_part_declaration);

END_TEST_SUITE()
