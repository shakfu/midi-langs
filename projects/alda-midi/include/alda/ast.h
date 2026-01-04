/**
 * @file ast.h
 * @brief Abstract Syntax Tree node types for the Alda parser.
 */

#ifndef ALDA_AST_H
#define ALDA_AST_H

#include "tokens.h"
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief AST node types.
 */
typedef enum {
    ALDA_NODE_ROOT,
    ALDA_NODE_PART_DECL,
    ALDA_NODE_EVENT_SEQ,
    ALDA_NODE_NOTE,
    ALDA_NODE_REST,
    ALDA_NODE_CHORD,
    ALDA_NODE_BARLINE,
    ALDA_NODE_DURATION,
    ALDA_NODE_NOTE_LENGTH,
    ALDA_NODE_NOTE_LENGTH_MS,
    ALDA_NODE_NOTE_LENGTH_S,
    ALDA_NODE_OCTAVE_SET,
    ALDA_NODE_OCTAVE_UP,
    ALDA_NODE_OCTAVE_DOWN,
    ALDA_NODE_LISP_LIST,
    ALDA_NODE_LISP_SYMBOL,
    ALDA_NODE_LISP_NUMBER,
    ALDA_NODE_LISP_STRING,
    ALDA_NODE_VAR_DEF,
    ALDA_NODE_VAR_REF,
    ALDA_NODE_MARKER,
    ALDA_NODE_AT_MARKER,
    ALDA_NODE_VOICE_GROUP,
    ALDA_NODE_VOICE,
    ALDA_NODE_CRAM,
    ALDA_NODE_BRACKET_SEQ,
    ALDA_NODE_REPEAT,
    ALDA_NODE_ON_REPS,

    ALDA_NODE_COUNT
} AldaNodeType;

/**
 * @brief Forward declaration of AST node.
 */
typedef struct AldaNode AldaNode;

/**
 * @brief AST node structure.
 */
struct AldaNode {
    AldaNodeType type;
    AldaSourcePos pos;
    AldaNode* next;             /* Sibling link for lists */

    union {
        /* ALDA_NODE_ROOT */
        struct {
            AldaNode* children;
        } root;

        /* ALDA_NODE_PART_DECL */
        struct {
            char** names;
            size_t name_count;
            char* alias;
        } part_decl;

        /* ALDA_NODE_EVENT_SEQ */
        struct {
            AldaNode* events;
        } event_seq;

        /* ALDA_NODE_NOTE */
        struct {
            char letter;
            char* accidentals;
            AldaNode* duration;
            int slurred;
        } note;

        /* ALDA_NODE_REST */
        struct {
            AldaNode* duration;
        } rest;

        /* ALDA_NODE_CHORD */
        struct {
            AldaNode* notes;
        } chord;

        /* ALDA_NODE_DURATION */
        struct {
            AldaNode* components;
        } duration;

        /* ALDA_NODE_NOTE_LENGTH */
        struct {
            int denominator;
            int dots;
        } note_length;

        /* ALDA_NODE_NOTE_LENGTH_MS */
        struct {
            int ms;
        } note_length_ms;

        /* ALDA_NODE_NOTE_LENGTH_S */
        struct {
            double seconds;
        } note_length_s;

        /* ALDA_NODE_OCTAVE_SET */
        struct {
            int octave;
        } octave_set;

        /* ALDA_NODE_LISP_LIST */
        struct {
            AldaNode* elements;
        } lisp_list;

        /* ALDA_NODE_LISP_SYMBOL */
        struct {
            char* name;
        } lisp_symbol;

        /* ALDA_NODE_LISP_NUMBER */
        struct {
            double value;
        } lisp_number;

        /* ALDA_NODE_LISP_STRING */
        struct {
            char* value;
        } lisp_string;

        /* ALDA_NODE_VAR_DEF */
        struct {
            char* name;
            AldaNode* events;
        } var_def;

        /* ALDA_NODE_VAR_REF */
        struct {
            char* name;
        } var_ref;

        /* ALDA_NODE_MARKER */
        struct {
            char* name;
        } marker;

        /* ALDA_NODE_AT_MARKER */
        struct {
            char* name;
        } at_marker;

        /* ALDA_NODE_VOICE_GROUP */
        struct {
            AldaNode* voices;
        } voice_group;

        /* ALDA_NODE_VOICE */
        struct {
            int number;
            AldaNode* events;
        } voice;

        /* ALDA_NODE_CRAM */
        struct {
            AldaNode* events;
            AldaNode* duration;
        } cram;

        /* ALDA_NODE_BRACKET_SEQ */
        struct {
            AldaNode* events;
        } bracket_seq;

        /* ALDA_NODE_REPEAT */
        struct {
            AldaNode* event;
            int count;
        } repeat;

        /* ALDA_NODE_ON_REPS */
        struct {
            AldaNode* event;
            int* reps;
            size_t rep_count;
        } on_reps;
    } data;
};

/**
 * @brief Get the name of a node type.
 * @param type The node type.
 * @return A string representation of the node type.
 */
const char* alda_node_type_name(AldaNodeType type);

/**
 * @brief Create a new AST node.
 * @param type Node type.
 * @param pos Source position.
 * @return Newly allocated node. Caller must free with alda_node_free().
 */
AldaNode* alda_node_new(AldaNodeType type, AldaSourcePos pos);

/**
 * @brief Free a single AST node (not recursive).
 * @param node Node to free.
 */
void alda_node_free(AldaNode* node);

/**
 * @brief Free an entire AST tree recursively.
 * @param root Root node of the tree.
 */
void alda_ast_free(AldaNode* root);

/**
 * @brief Append a node to a linked list.
 * @param list Pointer to list head.
 * @param node Node to append.
 */
void alda_node_append(AldaNode** list, AldaNode* node);

/**
 * @brief Count nodes in a linked list.
 * @param list List head.
 * @return Number of nodes.
 */
size_t alda_node_count(AldaNode* list);

/* Node creation helpers */

AldaNode* alda_node_root(AldaSourcePos pos);
AldaNode* alda_node_part_decl(char** names, size_t name_count, char* alias, AldaSourcePos pos);
AldaNode* alda_node_event_seq(AldaNode* events, AldaSourcePos pos);
AldaNode* alda_node_note(char letter, char* accidentals, AldaNode* duration, int slurred, AldaSourcePos pos);
AldaNode* alda_node_rest(AldaNode* duration, AldaSourcePos pos);
AldaNode* alda_node_chord(AldaNode* notes, AldaSourcePos pos);
AldaNode* alda_node_duration(AldaNode* components, AldaSourcePos pos);
AldaNode* alda_node_note_length(int denominator, int dots, AldaSourcePos pos);
AldaNode* alda_node_note_length_ms(int ms, AldaSourcePos pos);
AldaNode* alda_node_note_length_s(double seconds, AldaSourcePos pos);
AldaNode* alda_node_octave_set(int octave, AldaSourcePos pos);
AldaNode* alda_node_octave_up(AldaSourcePos pos);
AldaNode* alda_node_octave_down(AldaSourcePos pos);
AldaNode* alda_node_lisp_list(AldaNode* elements, AldaSourcePos pos);
AldaNode* alda_node_lisp_symbol(char* name, AldaSourcePos pos);
AldaNode* alda_node_lisp_number(double value, AldaSourcePos pos);
AldaNode* alda_node_lisp_string(char* value, AldaSourcePos pos);
AldaNode* alda_node_var_def(char* name, AldaNode* events, AldaSourcePos pos);
AldaNode* alda_node_var_ref(char* name, AldaSourcePos pos);
AldaNode* alda_node_marker(char* name, AldaSourcePos pos);
AldaNode* alda_node_at_marker(char* name, AldaSourcePos pos);
AldaNode* alda_node_voice_group(AldaNode* voices, AldaSourcePos pos);
AldaNode* alda_node_voice(int number, AldaNode* events, AldaSourcePos pos);
AldaNode* alda_node_cram(AldaNode* events, AldaNode* duration, AldaSourcePos pos);
AldaNode* alda_node_bracket_seq(AldaNode* events, AldaSourcePos pos);
AldaNode* alda_node_repeat(AldaNode* event, int count, AldaSourcePos pos);
AldaNode* alda_node_on_reps(AldaNode* event, int* reps, size_t rep_count, AldaSourcePos pos);
AldaNode* alda_node_barline(AldaSourcePos pos);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_AST_H */
