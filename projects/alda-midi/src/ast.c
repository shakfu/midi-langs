/**
 * @file ast.c
 * @brief AST node implementation for the Alda parser.
 */

#include "alda/ast.h"
#include <stdlib.h>
#include <string.h>

static const char* NODE_TYPE_NAMES[] = {
    "ROOT",
    "PART_DECL",
    "EVENT_SEQ",
    "NOTE",
    "REST",
    "CHORD",
    "BARLINE",
    "DURATION",
    "NOTE_LENGTH",
    "NOTE_LENGTH_MS",
    "NOTE_LENGTH_S",
    "OCTAVE_SET",
    "OCTAVE_UP",
    "OCTAVE_DOWN",
    "LISP_LIST",
    "LISP_SYMBOL",
    "LISP_NUMBER",
    "LISP_STRING",
    "VAR_DEF",
    "VAR_REF",
    "MARKER",
    "AT_MARKER",
    "VOICE_GROUP",
    "VOICE",
    "CRAM",
    "BRACKET_SEQ",
    "REPEAT",
    "ON_REPS",
};

const char* alda_node_type_name(AldaNodeType type) {
    if (type >= 0 && type < ALDA_NODE_COUNT) {
        return NODE_TYPE_NAMES[type];
    }
    return "UNKNOWN";
}

AldaNode* alda_node_new(AldaNodeType type, AldaSourcePos pos) {
    AldaNode* node = (AldaNode*)calloc(1, sizeof(AldaNode));
    if (!node) return NULL;

    node->type = type;
    node->pos = pos;
    node->next = NULL;

    return node;
}

void alda_node_free(AldaNode* node) {
    if (!node) return;

    switch (node->type) {
        case ALDA_NODE_PART_DECL:
            for (size_t i = 0; i < node->data.part_decl.name_count; i++) {
                free(node->data.part_decl.names[i]);
            }
            free((void*)node->data.part_decl.names);
            free(node->data.part_decl.alias);
            break;

        case ALDA_NODE_NOTE:
            free(node->data.note.accidentals);
            alda_ast_free(node->data.note.duration);
            break;

        case ALDA_NODE_REST:
            alda_ast_free(node->data.rest.duration);
            break;

        case ALDA_NODE_CHORD:
            alda_ast_free(node->data.chord.notes);
            break;

        case ALDA_NODE_DURATION:
            alda_ast_free(node->data.duration.components);
            break;

        case ALDA_NODE_LISP_LIST:
            alda_ast_free(node->data.lisp_list.elements);
            break;

        case ALDA_NODE_LISP_SYMBOL:
            free(node->data.lisp_symbol.name);
            break;

        case ALDA_NODE_LISP_STRING:
            free(node->data.lisp_string.value);
            break;

        case ALDA_NODE_VAR_DEF:
            free(node->data.var_def.name);
            alda_ast_free(node->data.var_def.events);
            break;

        case ALDA_NODE_VAR_REF:
            free(node->data.var_ref.name);
            break;

        case ALDA_NODE_MARKER:
            free(node->data.marker.name);
            break;

        case ALDA_NODE_AT_MARKER:
            free(node->data.at_marker.name);
            break;

        case ALDA_NODE_VOICE_GROUP:
            alda_ast_free(node->data.voice_group.voices);
            break;

        case ALDA_NODE_VOICE:
            alda_ast_free(node->data.voice.events);
            break;

        case ALDA_NODE_CRAM:
            alda_ast_free(node->data.cram.events);
            alda_ast_free(node->data.cram.duration);
            break;

        case ALDA_NODE_BRACKET_SEQ:
            alda_ast_free(node->data.bracket_seq.events);
            break;

        case ALDA_NODE_REPEAT:
            alda_ast_free(node->data.repeat.event);
            break;

        case ALDA_NODE_ON_REPS:
            alda_ast_free(node->data.on_reps.event);
            free(node->data.on_reps.reps);
            break;

        case ALDA_NODE_ROOT:
            alda_ast_free(node->data.root.children);
            break;

        case ALDA_NODE_EVENT_SEQ:
            alda_ast_free(node->data.event_seq.events);
            break;

        default:
            break;
    }

    free(node);
}

void alda_ast_free(AldaNode* root) {
    while (root) {
        AldaNode* next = root->next;
        alda_node_free(root);
        root = next;
    }
}

void alda_node_append(AldaNode** list, AldaNode* node) {
    if (!node) return;

    if (!*list) {
        *list = node;
    } else {
        AldaNode* tail = *list;
        while (tail->next) {
            tail = tail->next;
        }
        tail->next = node;
    }
}

size_t alda_node_count(AldaNode* list) {
    size_t count = 0;
    while (list) {
        count++;
        list = list->next;
    }
    return count;
}

/* Node creation helpers */

AldaNode* alda_node_root(AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_ROOT, pos);
    if (node) {
        node->data.root.children = NULL;
    }
    return node;
}

AldaNode* alda_node_part_decl(char** names, size_t name_count, char* alias, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_PART_DECL, pos);
    if (node) {
        node->data.part_decl.names = names;
        node->data.part_decl.name_count = name_count;
        node->data.part_decl.alias = alias;
    }
    return node;
}

AldaNode* alda_node_event_seq(AldaNode* events, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_EVENT_SEQ, pos);
    if (node) {
        node->data.event_seq.events = events;
    }
    return node;
}

AldaNode* alda_node_note(char letter, char* accidentals, AldaNode* duration, int slurred, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_NOTE, pos);
    if (node) {
        node->data.note.letter = letter;
        node->data.note.accidentals = accidentals;
        node->data.note.duration = duration;
        node->data.note.slurred = slurred;
    }
    return node;
}

AldaNode* alda_node_rest(AldaNode* duration, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_REST, pos);
    if (node) {
        node->data.rest.duration = duration;
    }
    return node;
}

AldaNode* alda_node_chord(AldaNode* notes, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_CHORD, pos);
    if (node) {
        node->data.chord.notes = notes;
    }
    return node;
}

AldaNode* alda_node_duration(AldaNode* components, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_DURATION, pos);
    if (node) {
        node->data.duration.components = components;
    }
    return node;
}

AldaNode* alda_node_note_length(int denominator, int dots, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_NOTE_LENGTH, pos);
    if (node) {
        node->data.note_length.denominator = denominator;
        node->data.note_length.dots = dots;
    }
    return node;
}

AldaNode* alda_node_note_length_ms(int ms, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_NOTE_LENGTH_MS, pos);
    if (node) {
        node->data.note_length_ms.ms = ms;
    }
    return node;
}

AldaNode* alda_node_note_length_s(double seconds, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_NOTE_LENGTH_S, pos);
    if (node) {
        node->data.note_length_s.seconds = seconds;
    }
    return node;
}

AldaNode* alda_node_octave_set(int octave, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_OCTAVE_SET, pos);
    if (node) {
        node->data.octave_set.octave = octave;
    }
    return node;
}

AldaNode* alda_node_octave_up(AldaSourcePos pos) {
    return alda_node_new(ALDA_NODE_OCTAVE_UP, pos);
}

AldaNode* alda_node_octave_down(AldaSourcePos pos) {
    return alda_node_new(ALDA_NODE_OCTAVE_DOWN, pos);
}

AldaNode* alda_node_lisp_list(AldaNode* elements, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_LISP_LIST, pos);
    if (node) {
        node->data.lisp_list.elements = elements;
    }
    return node;
}

AldaNode* alda_node_lisp_symbol(char* name, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_LISP_SYMBOL, pos);
    if (node) {
        node->data.lisp_symbol.name = name;
    }
    return node;
}

AldaNode* alda_node_lisp_number(double value, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_LISP_NUMBER, pos);
    if (node) {
        node->data.lisp_number.value = value;
    }
    return node;
}

AldaNode* alda_node_lisp_string(char* value, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_LISP_STRING, pos);
    if (node) {
        node->data.lisp_string.value = value;
    }
    return node;
}

AldaNode* alda_node_var_def(char* name, AldaNode* events, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_VAR_DEF, pos);
    if (node) {
        node->data.var_def.name = name;
        node->data.var_def.events = events;
    }
    return node;
}

AldaNode* alda_node_var_ref(char* name, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_VAR_REF, pos);
    if (node) {
        node->data.var_ref.name = name;
    }
    return node;
}

AldaNode* alda_node_marker(char* name, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_MARKER, pos);
    if (node) {
        node->data.marker.name = name;
    }
    return node;
}

AldaNode* alda_node_at_marker(char* name, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_AT_MARKER, pos);
    if (node) {
        node->data.at_marker.name = name;
    }
    return node;
}

AldaNode* alda_node_voice_group(AldaNode* voices, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_VOICE_GROUP, pos);
    if (node) {
        node->data.voice_group.voices = voices;
    }
    return node;
}

AldaNode* alda_node_voice(int number, AldaNode* events, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_VOICE, pos);
    if (node) {
        node->data.voice.number = number;
        node->data.voice.events = events;
    }
    return node;
}

AldaNode* alda_node_cram(AldaNode* events, AldaNode* duration, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_CRAM, pos);
    if (node) {
        node->data.cram.events = events;
        node->data.cram.duration = duration;
    }
    return node;
}

AldaNode* alda_node_bracket_seq(AldaNode* events, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_BRACKET_SEQ, pos);
    if (node) {
        node->data.bracket_seq.events = events;
    }
    return node;
}

AldaNode* alda_node_repeat(AldaNode* event, int count, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_REPEAT, pos);
    if (node) {
        node->data.repeat.event = event;
        node->data.repeat.count = count;
    }
    return node;
}

AldaNode* alda_node_on_reps(AldaNode* event, int* reps, size_t rep_count, AldaSourcePos pos) {
    AldaNode* node = alda_node_new(ALDA_NODE_ON_REPS, pos);
    if (node) {
        node->data.on_reps.event = event;
        node->data.on_reps.reps = reps;
        node->data.on_reps.rep_count = rep_count;
    }
    return node;
}

AldaNode* alda_node_barline(AldaSourcePos pos) {
    return alda_node_new(ALDA_NODE_BARLINE, pos);
}
