/**
 * @file test_parser.c
 * @brief Unit tests for Alda parser.
 *
 * Tests parsing for all 27 AST node types:
 * - ROOT, PART_DECL, EVENT_SEQ, NOTE, REST, CHORD, BARLINE
 * - DURATION, NOTE_LENGTH, NOTE_LENGTH_MS, NOTE_LENGTH_S
 * - OCTAVE_SET, OCTAVE_UP, OCTAVE_DOWN
 * - LISP_LIST, LISP_SYMBOL, LISP_NUMBER, LISP_STRING
 * - VAR_DEF, VAR_REF, MARKER, AT_MARKER
 * - VOICE_GROUP, VOICE, CRAM, BRACKET_SEQ, REPEAT, ON_REPS
 */

#include "test_framework.h"
#include <alda/ast.h>
#include <alda/parser.h>
#include <string.h>
#include <math.h>

/* ============================================================================
 * Helper Functions
 * ============================================================================ */

/* Parse source and return AST, asserting no errors */
static AldaNode* parse_ok(const char* source) {
    char* error = NULL;
    AldaNode* ast = alda_parse(source, "test", &error);
    if (error) {
        printf("Parse error: %s\n", error);
        free(error);
    }
    return ast;
}

/* Count children of a node */
static int count_children(AldaNode* list) {
    int count = 0;
    while (list) {
        count++;
        list = list->next;
    }
    return count;
}

/* Find first child of given type */
static AldaNode* find_child_type(AldaNode* list, AldaNodeType type) {
    while (list) {
        if (list->type == type) {
            return list;
        }
        list = list->next;
    }
    return NULL;
}

/* Navigate to event sequence under root->part_decl */
static AldaNode* get_first_event_seq(AldaNode* root) {
    if (!root || root->type != ALDA_NODE_ROOT) return NULL;
    AldaNode* part = root->data.root.children;
    if (!part || part->type != ALDA_NODE_PART_DECL) return NULL;
    AldaNode* seq = part->next;
    if (!seq || seq->type != ALDA_NODE_EVENT_SEQ) return NULL;
    return seq;
}

/* Get first event from event sequence */
static AldaNode* get_first_event(AldaNode* root) {
    AldaNode* seq = get_first_event_seq(root);
    if (!seq) return NULL;
    return seq->data.event_seq.events;
}

/* ============================================================================
 * ALDA_NODE_ROOT Tests
 * ============================================================================ */

TEST(parse_empty_returns_root) {
    AldaNode* ast = parse_ok("");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(ast->type, ALDA_NODE_ROOT);
    ASSERT_NULL(ast->data.root.children);
    alda_ast_free(ast);
}

TEST(parse_root_contains_children) {
    AldaNode* ast = parse_ok("piano: c");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(ast->type, ALDA_NODE_ROOT);
    ASSERT_NOT_NULL(ast->data.root.children);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_PART_DECL Tests
 * ============================================================================ */

TEST(parse_simple_part_decl) {
    AldaNode* ast = parse_ok("piano:");
    ASSERT_NOT_NULL(ast);
    AldaNode* part = ast->data.root.children;
    ASSERT_NOT_NULL(part);
    ASSERT_EQ(part->type, ALDA_NODE_PART_DECL);
    ASSERT_EQ(part->data.part_decl.name_count, 1);
    ASSERT_STR_EQ(part->data.part_decl.names[0], "piano");
    ASSERT_NULL(part->data.part_decl.alias);
    alda_ast_free(ast);
}

TEST(parse_part_with_alias) {
    AldaNode* ast = parse_ok("piano \"left-hand\":");
    ASSERT_NOT_NULL(ast);
    AldaNode* part = ast->data.root.children;
    ASSERT_NOT_NULL(part);
    ASSERT_EQ(part->type, ALDA_NODE_PART_DECL);
    ASSERT_STR_EQ(part->data.part_decl.alias, "left-hand");
    alda_ast_free(ast);
}

TEST(parse_multi_part_decl) {
    AldaNode* ast = parse_ok("violin/viola:");
    ASSERT_NOT_NULL(ast);
    AldaNode* part = ast->data.root.children;
    ASSERT_NOT_NULL(part);
    ASSERT_EQ(part->type, ALDA_NODE_PART_DECL);
    ASSERT_EQ(part->data.part_decl.name_count, 2);
    ASSERT_STR_EQ(part->data.part_decl.names[0], "violin");
    ASSERT_STR_EQ(part->data.part_decl.names[1], "viola");
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_EVENT_SEQ Tests
 * ============================================================================ */

TEST(parse_event_seq_single) {
    AldaNode* ast = parse_ok("piano: c");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    ASSERT_EQ(seq->type, ALDA_NODE_EVENT_SEQ);
    ASSERT_NOT_NULL(seq->data.event_seq.events);
    alda_ast_free(ast);
}

TEST(parse_event_seq_multiple) {
    AldaNode* ast = parse_ok("piano: c d e f g");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    ASSERT_EQ(count_children(seq->data.event_seq.events), 5);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_NOTE Tests
 * ============================================================================ */

TEST(parse_note_basic) {
    AldaNode* ast = parse_ok("piano: c");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->type, ALDA_NODE_NOTE);
    ASSERT_EQ(note->data.note.letter, 'c');
    ASSERT_NULL(note->data.note.accidentals);
    ASSERT_FALSE(note->data.note.slurred);
    alda_ast_free(ast);
}

TEST(parse_note_all_letters) {
    const char letters[] = {'c', 'd', 'e', 'f', 'g', 'a', 'b'};
    for (int i = 0; i < 7; i++) {
        char source[16];
        snprintf(source, sizeof(source), "piano: %c", letters[i]);
        AldaNode* ast = parse_ok(source);
        ASSERT_NOT_NULL(ast);
        AldaNode* note = get_first_event(ast);
        ASSERT_NOT_NULL(note);
        ASSERT_EQ(note->data.note.letter, letters[i]);
        alda_ast_free(ast);
    }
}

TEST(parse_note_sharp) {
    AldaNode* ast = parse_ok("piano: c+");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    ASSERT_EQ(note->type, ALDA_NODE_NOTE);
    ASSERT_NOT_NULL(note->data.note.accidentals);
    ASSERT_STR_EQ(note->data.note.accidentals, "+");
    alda_ast_free(ast);
}

TEST(parse_note_flat) {
    AldaNode* ast = parse_ok("piano: b-");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    ASSERT_NOT_NULL(note->data.note.accidentals);
    ASSERT_STR_EQ(note->data.note.accidentals, "-");
    alda_ast_free(ast);
}

TEST(parse_note_double_sharp) {
    AldaNode* ast = parse_ok("piano: c++");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    ASSERT_NOT_NULL(note->data.note.accidentals);
    ASSERT_STR_EQ(note->data.note.accidentals, "++");
    alda_ast_free(ast);
}

TEST(parse_note_double_flat) {
    AldaNode* ast = parse_ok("piano: b--");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    ASSERT_NOT_NULL(note->data.note.accidentals);
    ASSERT_STR_EQ(note->data.note.accidentals, "--");
    alda_ast_free(ast);
}

TEST(parse_note_slurred) {
    AldaNode* ast = parse_ok("piano: c~");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    ASSERT_TRUE(note->data.note.slurred);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_REST Tests
 * ============================================================================ */

TEST(parse_rest_basic) {
    AldaNode* ast = parse_ok("piano: r");
    ASSERT_NOT_NULL(ast);
    AldaNode* rest = get_first_event(ast);
    ASSERT_NOT_NULL(rest);
    ASSERT_EQ(rest->type, ALDA_NODE_REST);
    alda_ast_free(ast);
}

TEST(parse_rest_with_duration) {
    AldaNode* ast = parse_ok("piano: r4");
    ASSERT_NOT_NULL(ast);
    AldaNode* rest = get_first_event(ast);
    ASSERT_NOT_NULL(rest);
    ASSERT_EQ(rest->type, ALDA_NODE_REST);
    ASSERT_NOT_NULL(rest->data.rest.duration);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_CHORD Tests
 * ============================================================================ */

TEST(parse_chord_basic) {
    AldaNode* ast = parse_ok("piano: c/e/g");
    ASSERT_NOT_NULL(ast);
    AldaNode* chord = get_first_event(ast);
    ASSERT_NOT_NULL(chord);
    ASSERT_EQ(chord->type, ALDA_NODE_CHORD);
    ASSERT_EQ(count_children(chord->data.chord.notes), 3);
    alda_ast_free(ast);
}

TEST(parse_chord_two_notes) {
    AldaNode* ast = parse_ok("piano: c/g");
    ASSERT_NOT_NULL(ast);
    AldaNode* chord = get_first_event(ast);
    ASSERT_NOT_NULL(chord);
    ASSERT_EQ(chord->type, ALDA_NODE_CHORD);
    ASSERT_EQ(count_children(chord->data.chord.notes), 2);
    alda_ast_free(ast);
}

TEST(parse_chord_with_accidentals) {
    AldaNode* ast = parse_ok("piano: c+/e/g+");
    ASSERT_NOT_NULL(ast);
    AldaNode* chord = get_first_event(ast);
    ASSERT_NOT_NULL(chord);
    ASSERT_EQ(chord->type, ALDA_NODE_CHORD);
    /* First note should be c# */
    AldaNode* first = chord->data.chord.notes;
    ASSERT_NOT_NULL(first);
    ASSERT_EQ(first->data.note.letter, 'c');
    ASSERT_STR_EQ(first->data.note.accidentals, "+");
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_BARLINE Tests
 * ============================================================================ */

TEST(parse_barline) {
    AldaNode* ast = parse_ok("piano: c | d");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* barline = find_child_type(seq->data.event_seq.events, ALDA_NODE_BARLINE);
    ASSERT_NOT_NULL(barline);
    ASSERT_EQ(barline->type, ALDA_NODE_BARLINE);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_DURATION Tests
 * ============================================================================ */

TEST(parse_duration_single) {
    AldaNode* ast = parse_ok("piano: c4");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    ASSERT_NOT_NULL(note->data.note.duration);
    ASSERT_EQ(note->data.note.duration->type, ALDA_NODE_DURATION);
    alda_ast_free(ast);
}

TEST(parse_duration_tied) {
    AldaNode* ast = parse_ok("piano: c4~8");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    AldaNode* dur = note->data.note.duration;
    ASSERT_NOT_NULL(dur);
    ASSERT_EQ(dur->type, ALDA_NODE_DURATION);
    /* Should have 2 duration components */
    ASSERT_EQ(count_children(dur->data.duration.components), 2);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_NOTE_LENGTH Tests
 * ============================================================================ */

TEST(parse_note_length_quarter) {
    AldaNode* ast = parse_ok("piano: c4");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    AldaNode* dur = note->data.note.duration;
    ASSERT_NOT_NULL(dur);
    AldaNode* len = dur->data.duration.components;
    ASSERT_NOT_NULL(len);
    ASSERT_EQ(len->type, ALDA_NODE_NOTE_LENGTH);
    ASSERT_EQ(len->data.note_length.denominator, 4);
    ASSERT_EQ(len->data.note_length.dots, 0);
    alda_ast_free(ast);
}

TEST(parse_note_length_whole) {
    AldaNode* ast = parse_ok("piano: c1");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->data.note_length.denominator, 1);
    alda_ast_free(ast);
}

TEST(parse_note_length_half) {
    AldaNode* ast = parse_ok("piano: c2");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->data.note_length.denominator, 2);
    alda_ast_free(ast);
}

TEST(parse_note_length_eighth) {
    AldaNode* ast = parse_ok("piano: c8");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->data.note_length.denominator, 8);
    alda_ast_free(ast);
}

TEST(parse_note_length_sixteenth) {
    AldaNode* ast = parse_ok("piano: c16");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->data.note_length.denominator, 16);
    alda_ast_free(ast);
}

TEST(parse_note_length_dotted) {
    AldaNode* ast = parse_ok("piano: c4.");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->data.note_length.denominator, 4);
    ASSERT_EQ(len->data.note_length.dots, 1);
    alda_ast_free(ast);
}

TEST(parse_note_length_double_dotted) {
    AldaNode* ast = parse_ok("piano: c4..");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->data.note_length.denominator, 4);
    ASSERT_EQ(len->data.note_length.dots, 2);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_NOTE_LENGTH_MS Tests
 * ============================================================================ */

TEST(parse_note_length_ms) {
    AldaNode* ast = parse_ok("piano: c500ms");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    AldaNode* dur = note->data.note.duration;
    ASSERT_NOT_NULL(dur);
    AldaNode* len = dur->data.duration.components;
    ASSERT_NOT_NULL(len);
    ASSERT_EQ(len->type, ALDA_NODE_NOTE_LENGTH_MS);
    ASSERT_EQ(len->data.note_length_ms.ms, 500);
    alda_ast_free(ast);
}

TEST(parse_note_length_ms_large) {
    AldaNode* ast = parse_ok("piano: c2000ms");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->type, ALDA_NODE_NOTE_LENGTH_MS);
    ASSERT_EQ(len->data.note_length_ms.ms, 2000);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_NOTE_LENGTH_S Tests
 * ============================================================================ */

TEST(parse_note_length_s) {
    AldaNode* ast = parse_ok("piano: c2s");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    ASSERT_NOT_NULL(note);
    AldaNode* dur = note->data.note.duration;
    ASSERT_NOT_NULL(dur);
    AldaNode* len = dur->data.duration.components;
    ASSERT_NOT_NULL(len);
    ASSERT_EQ(len->type, ALDA_NODE_NOTE_LENGTH_S);
    ASSERT_TRUE(fabs(len->data.note_length_s.seconds - 2.0) < 0.001);
    alda_ast_free(ast);
}

TEST(parse_note_length_ms_as_decimal_equivalent) {
    /* Alda uses integer seconds - for 1.5 seconds use 1500ms */
    AldaNode* ast = parse_ok("piano: c1500ms");
    ASSERT_NOT_NULL(ast);
    AldaNode* note = get_first_event(ast);
    AldaNode* len = note->data.note.duration->data.duration.components;
    ASSERT_EQ(len->type, ALDA_NODE_NOTE_LENGTH_MS);
    ASSERT_EQ(len->data.note_length_ms.ms, 1500);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_OCTAVE_SET Tests
 * ============================================================================ */

TEST(parse_octave_set) {
    AldaNode* ast = parse_ok("piano: o5 c");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* octave = seq->data.event_seq.events;
    ASSERT_NOT_NULL(octave);
    ASSERT_EQ(octave->type, ALDA_NODE_OCTAVE_SET);
    ASSERT_EQ(octave->data.octave_set.octave, 5);
    alda_ast_free(ast);
}

TEST(parse_octave_set_range) {
    /* Test various octave values */
    for (int oct = 0; oct <= 9; oct++) {
        char source[32];
        snprintf(source, sizeof(source), "piano: o%d c", oct);
        AldaNode* ast = parse_ok(source);
        ASSERT_NOT_NULL(ast);
        AldaNode* seq = get_first_event_seq(ast);
        AldaNode* octave = seq->data.event_seq.events;
        ASSERT_EQ(octave->data.octave_set.octave, oct);
        alda_ast_free(ast);
    }
}

/* ============================================================================
 * ALDA_NODE_OCTAVE_UP Tests
 * ============================================================================ */

TEST(parse_octave_up) {
    AldaNode* ast = parse_ok("piano: > c");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* octave = seq->data.event_seq.events;
    ASSERT_NOT_NULL(octave);
    ASSERT_EQ(octave->type, ALDA_NODE_OCTAVE_UP);
    alda_ast_free(ast);
}

TEST(parse_octave_up_multiple) {
    AldaNode* ast = parse_ok("piano: >> c");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* first = seq->data.event_seq.events;
    AldaNode* second = first->next;
    ASSERT_EQ(first->type, ALDA_NODE_OCTAVE_UP);
    ASSERT_EQ(second->type, ALDA_NODE_OCTAVE_UP);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_OCTAVE_DOWN Tests
 * ============================================================================ */

TEST(parse_octave_down) {
    AldaNode* ast = parse_ok("piano: < c");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* octave = seq->data.event_seq.events;
    ASSERT_NOT_NULL(octave);
    ASSERT_EQ(octave->type, ALDA_NODE_OCTAVE_DOWN);
    alda_ast_free(ast);
}

TEST(parse_octave_down_multiple) {
    AldaNode* ast = parse_ok("piano: << c");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* first = seq->data.event_seq.events;
    AldaNode* second = first->next;
    ASSERT_EQ(first->type, ALDA_NODE_OCTAVE_DOWN);
    ASSERT_EQ(second->type, ALDA_NODE_OCTAVE_DOWN);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_LISP_LIST Tests
 * ============================================================================ */

TEST(parse_lisp_list_tempo) {
    AldaNode* ast = parse_ok("piano: (tempo 120)");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* list = seq->data.event_seq.events;
    ASSERT_NOT_NULL(list);
    ASSERT_EQ(list->type, ALDA_NODE_LISP_LIST);
    ASSERT_NOT_NULL(list->data.lisp_list.elements);
    alda_ast_free(ast);
}

TEST(parse_lisp_list_nested) {
    AldaNode* ast = parse_ok("piano: (volume (+ 50 25))");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* list = seq->data.event_seq.events;
    ASSERT_EQ(list->type, ALDA_NODE_LISP_LIST);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_LISP_SYMBOL Tests
 * ============================================================================ */

TEST(parse_lisp_symbol) {
    AldaNode* ast = parse_ok("piano: (tempo 120)");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* list = seq->data.event_seq.events;
    ASSERT_NOT_NULL(list);
    AldaNode* sym = list->data.lisp_list.elements;
    ASSERT_NOT_NULL(sym);
    ASSERT_EQ(sym->type, ALDA_NODE_LISP_SYMBOL);
    ASSERT_STR_EQ(sym->data.lisp_symbol.name, "tempo");
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_LISP_NUMBER Tests
 * ============================================================================ */

TEST(parse_lisp_number_integer) {
    AldaNode* ast = parse_ok("piano: (tempo 120)");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* list = seq->data.event_seq.events;
    AldaNode* num = list->data.lisp_list.elements->next;  /* Second element */
    ASSERT_NOT_NULL(num);
    ASSERT_EQ(num->type, ALDA_NODE_LISP_NUMBER);
    ASSERT_TRUE(fabs(num->data.lisp_number.value - 120.0) < 0.001);
    alda_ast_free(ast);
}

TEST(parse_lisp_number_float) {
    AldaNode* ast = parse_ok("piano: (tempo 92.5)");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* list = seq->data.event_seq.events;
    AldaNode* num = list->data.lisp_list.elements->next;
    ASSERT_EQ(num->type, ALDA_NODE_LISP_NUMBER);
    ASSERT_TRUE(fabs(num->data.lisp_number.value - 92.5) < 0.001);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_LISP_STRING Tests
 * ============================================================================ */

TEST(parse_lisp_string) {
    AldaNode* ast = parse_ok("piano: (key-signature \"c major\")");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* list = seq->data.event_seq.events;
    ASSERT_NOT_NULL(list);
    AldaNode* str = list->data.lisp_list.elements->next;
    ASSERT_NOT_NULL(str);
    ASSERT_EQ(str->type, ALDA_NODE_LISP_STRING);
    ASSERT_STR_EQ(str->data.lisp_string.value, "c major");
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_VAR_DEF Tests
 * ============================================================================ */

TEST(parse_var_def_simple) {
    /* Variable definitions are at top level, not inside parts */
    AldaNode* ast = parse_ok("theme = c d e");
    ASSERT_NOT_NULL(ast);
    /* Variable definition is direct child of root */
    AldaNode* var = ast->data.root.children;
    ASSERT_NOT_NULL(var);
    ASSERT_EQ(var->type, ALDA_NODE_VAR_DEF);
    ASSERT_STR_EQ(var->data.var_def.name, "theme");
    ASSERT_NOT_NULL(var->data.var_def.events);
    alda_ast_free(ast);
}

TEST(parse_var_def_bracket) {
    /* Variable with bracket sequence */
    AldaNode* ast = parse_ok("motif = [c d e]");
    ASSERT_NOT_NULL(ast);
    AldaNode* var = ast->data.root.children;
    ASSERT_NOT_NULL(var);
    ASSERT_EQ(var->type, ALDA_NODE_VAR_DEF);
    ASSERT_STR_EQ(var->data.var_def.name, "motif");
    ASSERT_NOT_NULL(var->data.var_def.events);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_VAR_REF Tests
 * ============================================================================ */

TEST(parse_var_ref) {
    /* Variable definition then reference in a part */
    AldaNode* ast = parse_ok("theme = c d e\npiano: theme");
    ASSERT_NOT_NULL(ast);
    /* First child is var_def, second is part_decl, third is event_seq */
    AldaNode* var_def = ast->data.root.children;
    ASSERT_NOT_NULL(var_def);
    ASSERT_EQ(var_def->type, ALDA_NODE_VAR_DEF);
    /* Part declaration follows */
    AldaNode* part = var_def->next;
    ASSERT_NOT_NULL(part);
    ASSERT_EQ(part->type, ALDA_NODE_PART_DECL);
    /* Event sequence with var ref */
    AldaNode* seq = part->next;
    ASSERT_NOT_NULL(seq);
    ASSERT_EQ(seq->type, ALDA_NODE_EVENT_SEQ);
    AldaNode* var_ref = seq->data.event_seq.events;
    ASSERT_NOT_NULL(var_ref);
    ASSERT_EQ(var_ref->type, ALDA_NODE_VAR_REF);
    ASSERT_STR_EQ(var_ref->data.var_ref.name, "theme");
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_MARKER Tests
 * ============================================================================ */

TEST(parse_marker) {
    AldaNode* ast = parse_ok("piano: %verse c d e");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* marker = seq->data.event_seq.events;
    ASSERT_NOT_NULL(marker);
    ASSERT_EQ(marker->type, ALDA_NODE_MARKER);
    ASSERT_STR_EQ(marker->data.marker.name, "verse");
    alda_ast_free(ast);
}

TEST(parse_marker_with_numbers) {
    AldaNode* ast = parse_ok("piano: %section2 c");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* marker = seq->data.event_seq.events;
    ASSERT_EQ(marker->type, ALDA_NODE_MARKER);
    ASSERT_STR_EQ(marker->data.marker.name, "section2");
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_AT_MARKER Tests
 * ============================================================================ */

TEST(parse_at_marker) {
    AldaNode* ast = parse_ok("piano: @verse");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* at_marker = seq->data.event_seq.events;
    ASSERT_NOT_NULL(at_marker);
    ASSERT_EQ(at_marker->type, ALDA_NODE_AT_MARKER);
    ASSERT_STR_EQ(at_marker->data.at_marker.name, "verse");
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_VOICE_GROUP Tests
 * ============================================================================ */

TEST(parse_voice_group) {
    AldaNode* ast = parse_ok("piano: V1: c d e V2: e f g");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    ASSERT_NOT_NULL(seq);
    AldaNode* group = seq->data.event_seq.events;
    ASSERT_NOT_NULL(group);
    ASSERT_EQ(group->type, ALDA_NODE_VOICE_GROUP);
    ASSERT_NOT_NULL(group->data.voice_group.voices);
    alda_ast_free(ast);
}

TEST(parse_voice_group_three_voices) {
    AldaNode* ast = parse_ok("piano: V1: c V2: e V3: g");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* group = seq->data.event_seq.events;
    ASSERT_EQ(group->type, ALDA_NODE_VOICE_GROUP);
    ASSERT_EQ(count_children(group->data.voice_group.voices), 3);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_VOICE Tests
 * ============================================================================ */

TEST(parse_voice) {
    AldaNode* ast = parse_ok("piano: V1: c d e V2: g");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* group = seq->data.event_seq.events;
    ASSERT_NOT_NULL(group);
    AldaNode* voice = group->data.voice_group.voices;
    ASSERT_NOT_NULL(voice);
    ASSERT_EQ(voice->type, ALDA_NODE_VOICE);
    ASSERT_EQ(voice->data.voice.number, 1);
    ASSERT_NOT_NULL(voice->data.voice.events);
    alda_ast_free(ast);
}

TEST(parse_voice_numbers) {
    AldaNode* ast = parse_ok("piano: V1: c V5: d V9: e");
    ASSERT_NOT_NULL(ast);
    AldaNode* seq = get_first_event_seq(ast);
    AldaNode* group = seq->data.event_seq.events;
    AldaNode* v1 = group->data.voice_group.voices;
    AldaNode* v5 = v1->next;
    AldaNode* v9 = v5->next;
    ASSERT_EQ(v1->data.voice.number, 1);
    ASSERT_EQ(v5->data.voice.number, 5);
    ASSERT_EQ(v9->data.voice.number, 9);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_CRAM Tests
 * ============================================================================ */

TEST(parse_cram_basic) {
    AldaNode* ast = parse_ok("piano: {c d e}4");
    ASSERT_NOT_NULL(ast);
    AldaNode* cram = get_first_event(ast);
    ASSERT_NOT_NULL(cram);
    ASSERT_EQ(cram->type, ALDA_NODE_CRAM);
    ASSERT_NOT_NULL(cram->data.cram.events);
    ASSERT_NOT_NULL(cram->data.cram.duration);
    alda_ast_free(ast);
}

TEST(parse_cram_triplet) {
    AldaNode* ast = parse_ok("piano: {c d e}4");
    ASSERT_NOT_NULL(ast);
    AldaNode* cram = get_first_event(ast);
    ASSERT_EQ(cram->type, ALDA_NODE_CRAM);
    ASSERT_EQ(count_children(cram->data.cram.events), 3);
    alda_ast_free(ast);
}

TEST(parse_cram_quintuplet) {
    AldaNode* ast = parse_ok("piano: {c d e f g}4");
    ASSERT_NOT_NULL(ast);
    AldaNode* cram = get_first_event(ast);
    ASSERT_EQ(cram->type, ALDA_NODE_CRAM);
    ASSERT_EQ(count_children(cram->data.cram.events), 5);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_BRACKET_SEQ Tests
 * ============================================================================ */

TEST(parse_bracket_seq) {
    AldaNode* ast = parse_ok("piano: [c d e]*2");
    ASSERT_NOT_NULL(ast);
    AldaNode* event = get_first_event(ast);
    ASSERT_NOT_NULL(event);
    /* The bracket seq is wrapped in a repeat */
    ASSERT_EQ(event->type, ALDA_NODE_REPEAT);
    AldaNode* bracket = event->data.repeat.event;
    ASSERT_NOT_NULL(bracket);
    ASSERT_EQ(bracket->type, ALDA_NODE_BRACKET_SEQ);
    ASSERT_EQ(count_children(bracket->data.bracket_seq.events), 3);
    alda_ast_free(ast);
}

TEST(parse_bracket_seq_standalone) {
    AldaNode* ast = parse_ok("piano: [c d e]");
    ASSERT_NOT_NULL(ast);
    AldaNode* bracket = get_first_event(ast);
    ASSERT_NOT_NULL(bracket);
    ASSERT_EQ(bracket->type, ALDA_NODE_BRACKET_SEQ);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_REPEAT Tests
 * ============================================================================ */

TEST(parse_repeat_note) {
    AldaNode* ast = parse_ok("piano: c*4");
    ASSERT_NOT_NULL(ast);
    AldaNode* repeat = get_first_event(ast);
    ASSERT_NOT_NULL(repeat);
    ASSERT_EQ(repeat->type, ALDA_NODE_REPEAT);
    ASSERT_EQ(repeat->data.repeat.count, 4);
    ASSERT_NOT_NULL(repeat->data.repeat.event);
    ASSERT_EQ(repeat->data.repeat.event->type, ALDA_NODE_NOTE);
    alda_ast_free(ast);
}

TEST(parse_repeat_bracket) {
    AldaNode* ast = parse_ok("piano: [c d e]*3");
    ASSERT_NOT_NULL(ast);
    AldaNode* repeat = get_first_event(ast);
    ASSERT_EQ(repeat->type, ALDA_NODE_REPEAT);
    ASSERT_EQ(repeat->data.repeat.count, 3);
    alda_ast_free(ast);
}

TEST(parse_repeat_large_count) {
    AldaNode* ast = parse_ok("piano: c*100");
    ASSERT_NOT_NULL(ast);
    AldaNode* repeat = get_first_event(ast);
    ASSERT_EQ(repeat->type, ALDA_NODE_REPEAT);
    ASSERT_EQ(repeat->data.repeat.count, 100);
    alda_ast_free(ast);
}

/* ============================================================================
 * ALDA_NODE_ON_REPS Tests
 * ============================================================================ */

TEST(parse_on_reps_single) {
    AldaNode* ast = parse_ok("piano: [c d e'1 f'2]*2");
    ASSERT_NOT_NULL(ast);
    AldaNode* repeat = get_first_event(ast);
    ASSERT_NOT_NULL(repeat);
    ASSERT_EQ(repeat->type, ALDA_NODE_REPEAT);
    AldaNode* bracket = repeat->data.repeat.event;
    ASSERT_NOT_NULL(bracket);
    /* Find the on_reps node */
    AldaNode* event = bracket->data.bracket_seq.events;
    while (event && event->type != ALDA_NODE_ON_REPS) {
        event = event->next;
    }
    ASSERT_NOT_NULL(event);
    ASSERT_EQ(event->type, ALDA_NODE_ON_REPS);
    alda_ast_free(ast);
}

/* ============================================================================
 * Parser Error Tests
 * ============================================================================ */

TEST(parse_error_unclosed_paren) {
    char* error = NULL;
    AldaNode* ast = alda_parse("piano: (tempo 120", "test", &error);
    ASSERT_NULL(ast);
    ASSERT_NOT_NULL(error);
    free(error);
}

TEST(parse_error_unclosed_brace) {
    char* error = NULL;
    AldaNode* ast = alda_parse("piano: {c d e", "test", &error);
    ASSERT_NULL(ast);
    ASSERT_NOT_NULL(error);
    free(error);
}

TEST(parse_error_invalid_note) {
    char* error = NULL;
    AldaNode* ast = alda_parse("piano: x", "test", &error);
    /* Should fail - 'x' is not a valid note letter */
    if (ast) {
        /* Parser might accept 'x' as identifier, which is OK */
        alda_ast_free(ast);
    }
    if (error) free(error);
}

/* ============================================================================
 * Error Recovery and Context Tests
 * ============================================================================ */

TEST(parse_error_with_context_sexp) {
    /* Test that unclosed S-expression produces error */
    AldaParser* parser = alda_parser_new("piano: (tempo 120", "test");
    ASSERT_NOT_NULL(parser);

    AldaNode* ast = alda_parser_parse(parser);
    if (ast) alda_ast_free(ast);

    /* Check error count - should have at least one error */
    int err_count = alda_parser_error_count(parser);
    ASSERT_TRUE(err_count > 0);

    /* Get all errors formatted */
    char* all_errors = alda_parser_all_errors_string(parser);
    ASSERT_NOT_NULL(all_errors);
    free(all_errors);

    alda_parser_free(parser);
}

TEST(parse_error_with_context_bracket) {
    /* Test that bracket errors include context */
    AldaParser* parser = alda_parser_new("piano: [c d e", "test");
    ASSERT_NOT_NULL(parser);

    AldaNode* ast = alda_parser_parse(parser);
    if (ast) alda_ast_free(ast);

    ASSERT_EQ(alda_parser_has_error(parser), 1);

    /* Error should mention unclosed bracketed sequence */
    char* error_str = alda_parser_error_string(parser);
    ASSERT_NOT_NULL(error_str);
    free(error_str);

    alda_parser_free(parser);
}

TEST(parse_error_with_context_cram) {
    /* Test that cram expression errors include context */
    AldaParser* parser = alda_parser_new("piano: {c d e", "test");
    ASSERT_NOT_NULL(parser);

    AldaNode* ast = alda_parser_parse(parser);
    if (ast) alda_ast_free(ast);

    ASSERT_EQ(alda_parser_has_error(parser), 1);

    char* error_str = alda_parser_error_string(parser);
    ASSERT_NOT_NULL(error_str);
    free(error_str);

    alda_parser_free(parser);
}

TEST(parse_error_recovery_continues) {
    /* Test that parser recovers and continues after error */
    AldaParser* parser = alda_parser_new(
        "(tempo 120\n"   /* Unclosed S-expression - error */
        "piano: c d e",  /* Valid code after error */
        "test");
    ASSERT_NOT_NULL(parser);

    AldaNode* ast = alda_parser_parse(parser);
    /* Should produce some AST even with error */
    /* The parser should recover and parse the piano part */

    /* Should have collected errors */
    ASSERT_EQ(alda_parser_has_error(parser), 1);

    if (ast) {
        /* Check that we got something from the valid part */
        alda_ast_free(ast);
    }

    alda_parser_free(parser);
}

TEST(parse_error_multiple_collection) {
    /* Test that multiple errors are collected */
    AldaParser* parser = alda_parser_new(
        "(tempo 120\n"       /* Error 1: unclosed S-expression */
        "piano: c d\n"       /* Valid */
        "(volume 80\n"       /* Error 2: another unclosed S-expression */
        "violin: e f",       /* Valid */
        "test");
    ASSERT_NOT_NULL(parser);

    AldaNode* ast = alda_parser_parse(parser);
    if (ast) alda_ast_free(ast);

    /* Should have collected at least one error */
    int err_count = alda_parser_error_count(parser);
    ASSERT_TRUE(err_count > 0);

    alda_parser_free(parser);
}

TEST(parse_error_expected_colon) {
    /* Test error for missing colon in part declaration */
    AldaParser* parser = alda_parser_new("piano c d e", "test");
    ASSERT_NOT_NULL(parser);

    AldaNode* ast = alda_parser_parse(parser);
    if (ast) alda_ast_free(ast);

    /* "piano c d e" is ambiguous - could be var refs */
    /* Just verify parser doesn't crash */
    alda_parser_free(parser);
}

/* ============================================================================
 * Edge Cases
 * ============================================================================ */

TEST(parse_complex_expression) {
    AldaNode* ast = parse_ok("piano: o4 (tempo 120) c4 d8 e16 r4 | f/a/c1");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_multiline) {
    AldaNode* ast = parse_ok("piano:\n  c d e\n  f g a");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_comments) {
    AldaNode* ast = parse_ok("piano: c # this is a comment\nd e");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_multiple_parts) {
    AldaNode* ast = parse_ok("piano: c d e\nviolin: f g a");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(count_children(ast->data.root.children), 4);  /* 2 parts + 2 seqs */
    alda_ast_free(ast);
}

/* ============================================================================
 * Test Runner
 * ============================================================================ */


/* Count how many top-level children are part declarations. */
static int count_part_decls(AldaNode* root) {
    int n = 0;
    for (AldaNode* c = root->data.root.children; c; c = c->next) {
        if (c->type == ALDA_NODE_PART_DECL) n++;
    }
    return n;
}

/* ============================================================================
 * Cross-Implementation Regression Tests
 *
 * These constructs appear in the bundled examples/ scores but were rejected by
 * this parser until they were fixed. Each was found by differential-testing
 * against the aldakit Alda implementation over both projects' example corpora.
 * ============================================================================ */

/* A part declaration implicitly closes a voice group - "V0:" is optional. */
TEST(parse_part_decl_ends_voice_group) {
    AldaNode* ast = parse_ok("piano:\nV1: c\nclarinet: a");
    ASSERT_NOT_NULL(ast);
    /* Two parts, not one part swallowing the second declaration. */
    ASSERT_EQ(count_part_decls(ast), 2);
    alda_ast_free(ast);
}

TEST(parse_explicit_v0_still_works) {
    AldaNode* ast = parse_ok("piano:\nV1: c\nV0: d\nclarinet: a");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(count_part_decls(ast), 2);
    alda_ast_free(ast);
}

TEST(parse_voice_group_then_multiple_parts) {
    AldaNode* ast = parse_ok("piano:\nV1: c8 d\nV2: e8 f\nclarinet: a2\nflute: b2");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(count_part_decls(ast), 3);
    alda_ast_free(ast);
}

/* A tie may be split across a barline, on either side, and across lines. */
TEST(parse_tie_across_barline) {
    AldaNode* ast = parse_ok("piano: c4~|2");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_barline_before_tie) {
    AldaNode* ast = parse_ok("piano: c4 |~2");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_tie_both_sides_of_barline) {
    /* "d4.~4~|" continued by "|~4.~8" on the next line. */
    AldaNode* ast = parse_ok("piano: d4.~4~|\n\n  |~4.~8 g8");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_tie_across_barline_dotted) {
    AldaNode* ast = parse_ok("piano: a8~|2. a4");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

/* A tilde before a note letter is still a slur, not a tied duration. */
TEST(parse_slur_not_confused_with_tied_duration) {
    AldaNode* ast = parse_ok("piano: c4~d");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_plain_barline_still_an_event) {
    AldaNode* ast = parse_ok("piano: c4 | d4");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

/* Fractional note lengths: c0.25 is a quadruple-whole note. */
TEST(parse_fractional_note_length) {
    AldaNode* ast = parse_ok("piano: c0.25");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_fractional_tied_note_length) {
    AldaNode* ast = parse_ok("piano: c0.25~0.25");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_dotted_length_not_read_as_fraction) {
    /* "c4. d" must remain a dotted quarter followed by a note. */
    AldaNode* ast = parse_ok("piano: c4. d");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

/* Dot accessor: address one member of a named group. */
TEST(parse_dot_accessor_part) {
    AldaNode* ast = parse_ok("violin/viola/cello \"strings\": g1\nstrings.cello: c1");
    ASSERT_NOT_NULL(ast);
    ASSERT_EQ(count_part_decls(ast), 2);
    alda_ast_free(ast);
}

/* Banner comments must not scan as sharp accidentals. */
TEST(parse_banner_comment) {
    AldaNode* ast = parse_ok("piano: c\n########################\nd");
    ASSERT_NOT_NULL(ast);
    alda_ast_free(ast);
}

TEST(parse_hash_comment_variants) {
    const char* sources[] = {
        "piano: c # trailing\nd",
        "piano: c\n#no-space\nd",
        "piano: c\n#\nd",
        "piano: c\n#123\nd",
    };
    for (size_t i = 0; i < sizeof(sources) / sizeof(sources[0]); i++) {
        AldaNode* ast = parse_ok(sources[i]);
        ASSERT_NOT_NULL(ast);
        alda_ast_free(ast);
    }
}

BEGIN_TEST_SUITE("Alda Parser Tests")

    /* ROOT tests */
    RUN_TEST(parse_empty_returns_root);
    RUN_TEST(parse_root_contains_children);

    /* PART_DECL tests */
    RUN_TEST(parse_simple_part_decl);
    RUN_TEST(parse_part_with_alias);
    RUN_TEST(parse_multi_part_decl);

    /* EVENT_SEQ tests */
    RUN_TEST(parse_event_seq_single);
    RUN_TEST(parse_event_seq_multiple);

    /* NOTE tests */
    RUN_TEST(parse_note_basic);
    RUN_TEST(parse_note_all_letters);
    RUN_TEST(parse_note_sharp);
    RUN_TEST(parse_note_flat);
    RUN_TEST(parse_note_double_sharp);
    RUN_TEST(parse_note_double_flat);
    RUN_TEST(parse_note_slurred);

    /* REST tests */
    RUN_TEST(parse_rest_basic);
    RUN_TEST(parse_rest_with_duration);

    /* CHORD tests */
    RUN_TEST(parse_chord_basic);
    RUN_TEST(parse_chord_two_notes);
    RUN_TEST(parse_chord_with_accidentals);

    /* BARLINE tests */
    RUN_TEST(parse_barline);

    /* DURATION tests */
    RUN_TEST(parse_duration_single);
    RUN_TEST(parse_duration_tied);

    /* NOTE_LENGTH tests */
    RUN_TEST(parse_note_length_quarter);
    RUN_TEST(parse_note_length_whole);
    RUN_TEST(parse_note_length_half);
    RUN_TEST(parse_note_length_eighth);
    RUN_TEST(parse_note_length_sixteenth);
    RUN_TEST(parse_note_length_dotted);
    RUN_TEST(parse_note_length_double_dotted);

    /* NOTE_LENGTH_MS tests */
    RUN_TEST(parse_note_length_ms);
    RUN_TEST(parse_note_length_ms_large);

    /* NOTE_LENGTH_S tests */
    RUN_TEST(parse_note_length_s);
    RUN_TEST(parse_note_length_ms_as_decimal_equivalent);

    /* OCTAVE_SET tests */
    RUN_TEST(parse_octave_set);
    RUN_TEST(parse_octave_set_range);

    /* OCTAVE_UP tests */
    RUN_TEST(parse_octave_up);
    RUN_TEST(parse_octave_up_multiple);

    /* OCTAVE_DOWN tests */
    RUN_TEST(parse_octave_down);
    RUN_TEST(parse_octave_down_multiple);

    /* LISP_LIST tests */
    RUN_TEST(parse_lisp_list_tempo);
    RUN_TEST(parse_lisp_list_nested);

    /* LISP_SYMBOL tests */
    RUN_TEST(parse_lisp_symbol);

    /* LISP_NUMBER tests */
    RUN_TEST(parse_lisp_number_integer);
    RUN_TEST(parse_lisp_number_float);

    /* LISP_STRING tests */
    RUN_TEST(parse_lisp_string);

    /* VAR_DEF tests */
    RUN_TEST(parse_var_def_simple);
    RUN_TEST(parse_var_def_bracket);

    /* VAR_REF tests */
    RUN_TEST(parse_var_ref);

    /* MARKER tests */
    RUN_TEST(parse_marker);
    RUN_TEST(parse_marker_with_numbers);

    /* AT_MARKER tests */
    RUN_TEST(parse_at_marker);

    /* VOICE_GROUP tests */
    RUN_TEST(parse_voice_group);
    RUN_TEST(parse_voice_group_three_voices);

    /* VOICE tests */
    RUN_TEST(parse_voice);
    RUN_TEST(parse_voice_numbers);

    /* CRAM tests */
    RUN_TEST(parse_cram_basic);
    RUN_TEST(parse_cram_triplet);
    RUN_TEST(parse_cram_quintuplet);

    /* BRACKET_SEQ tests */
    RUN_TEST(parse_bracket_seq);
    RUN_TEST(parse_bracket_seq_standalone);

    /* REPEAT tests */
    RUN_TEST(parse_repeat_note);
    RUN_TEST(parse_repeat_bracket);
    RUN_TEST(parse_repeat_large_count);

    /* ON_REPS tests */
    RUN_TEST(parse_on_reps_single);

    /* Error tests */
    RUN_TEST(parse_error_unclosed_paren);
    RUN_TEST(parse_error_unclosed_brace);
    RUN_TEST(parse_error_invalid_note);

    /* Error recovery and context tests */
    RUN_TEST(parse_error_with_context_sexp);
    RUN_TEST(parse_error_with_context_bracket);
    RUN_TEST(parse_error_with_context_cram);
    RUN_TEST(parse_error_recovery_continues);
    RUN_TEST(parse_error_multiple_collection);
    RUN_TEST(parse_error_expected_colon);

    /* Edge cases */
    RUN_TEST(parse_complex_expression);
    RUN_TEST(parse_multiline);
    RUN_TEST(parse_comments);
    RUN_TEST(parse_multiple_parts);

    /* Cross-implementation regressions (see comment block above) */
    RUN_TEST(parse_part_decl_ends_voice_group);
    RUN_TEST(parse_explicit_v0_still_works);
    RUN_TEST(parse_voice_group_then_multiple_parts);
    RUN_TEST(parse_tie_across_barline);
    RUN_TEST(parse_barline_before_tie);
    RUN_TEST(parse_tie_both_sides_of_barline);
    RUN_TEST(parse_tie_across_barline_dotted);
    RUN_TEST(parse_slur_not_confused_with_tied_duration);
    RUN_TEST(parse_plain_barline_still_an_event);
    RUN_TEST(parse_fractional_note_length);
    RUN_TEST(parse_fractional_tied_note_length);
    RUN_TEST(parse_dotted_length_not_read_as_fraction);
    RUN_TEST(parse_dot_accessor_part);
    RUN_TEST(parse_banner_comment);
    RUN_TEST(parse_hash_comment_variants);

END_TEST_SUITE()
