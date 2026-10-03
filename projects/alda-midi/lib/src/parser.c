/**
 * @file parser.c
 * @brief Recursive descent parser for the Alda language.
 */

#include "alda/parser.h"
#include "alda/scanner.h"
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <stdio.h>

struct AldaParser {
    const char* source;
    const char* filename;
    AldaToken* tokens;
    size_t token_count;
    size_t current;
    AldaError* error;           /* First error (for compatibility) */
    AldaErrorList error_list;   /* All collected errors */
    const char* parse_context;  /* Current parsing context for error messages */
};

/* Helper functions */

static int is_at_end(AldaParser* p) {
    return p->current >= p->token_count ||
           p->tokens[p->current].type == ALDA_TOK_EOF;
}

static AldaToken _alda_eof_sentinel = {
    .type = ALDA_TOK_EOF,
    .lexeme = "",
    .lexeme_len = 0,
    .literal = {0},
    .pos = {0, 0, NULL},
};

static AldaToken* peek(AldaParser* p) {
    if (p->current >= p->token_count) return &_alda_eof_sentinel;
    return &p->tokens[p->current];
}

static AldaToken* peek_next(AldaParser* p) {
    if (p->current + 1 >= p->token_count) return NULL;
    return &p->tokens[p->current + 1];
}

static AldaToken* advance(AldaParser* p) {
    if (!is_at_end(p)) {
        return &p->tokens[p->current++];
    }
    return peek(p);
}

static int check(AldaParser* p, AldaTokenType type) {
    if (is_at_end(p)) return 0;
    return peek(p)->type == type;
}

static int match(AldaParser* p, AldaTokenType type) {
    if (check(p, type)) {
        advance(p);
        return 1;
    }
    return 0;
}

static void skip_newlines(AldaParser* p) {
    while (match(p, ALDA_TOK_NEWLINE)) {
        /* skip */
    }
}

static const char* token_type_name(AldaTokenType type) {
    switch (type) {
        case ALDA_TOK_NOTE_LETTER: return "note letter";
        case ALDA_TOK_REST_LETTER: return "rest";
        case ALDA_TOK_NOTE_LENGTH: return "note length";
        case ALDA_TOK_NOTE_LENGTH_MS: return "milliseconds duration";
        case ALDA_TOK_NOTE_LENGTH_S: return "seconds duration";
        case ALDA_TOK_LEFT_PAREN: return "'('";
        case ALDA_TOK_RIGHT_PAREN: return "')'";
        case ALDA_TOK_BRACKET_OPEN: return "'['";
        case ALDA_TOK_BRACKET_CLOSE: return "']'";
        case ALDA_TOK_CRAM_OPEN: return "'{'";
        case ALDA_TOK_CRAM_CLOSE: return "'}'";
        case ALDA_TOK_COLON: return "':'";
        case ALDA_TOK_EQUALS: return "'='";
        case ALDA_TOK_BARLINE: return "barline";
        case ALDA_TOK_NAME: return "identifier";
        case ALDA_TOK_SYMBOL: return "symbol";
        case ALDA_TOK_NUMBER: return "number";
        case ALDA_TOK_STRING: return "string";
        case ALDA_TOK_NEWLINE: return "newline";
        case ALDA_TOK_EOF: return "end of input";
        default: return "token";
    }
}

static void set_error(AldaParser* p, const char* msg) {
    AldaToken* tok = peek(p);
    AldaSourcePos pos = tok ? tok->pos : alda_pos_new(1, 1, p->filename);

    /* Build "found" description from current token */
    char found_buf[64];
    if (tok && tok->type != ALDA_TOK_EOF) {
        snprintf(found_buf, sizeof(found_buf), "%s", token_type_name(tok->type));
    } else {
        snprintf(found_buf, sizeof(found_buf), "end of input");
    }

    AldaError* err = alda_error_new_detailed(
        ALDA_ERR_SYNTAX, msg, pos, p->source,
        p->parse_context,  /* context */
        NULL,              /* expected - set by caller if needed */
        found_buf          /* found */
    );

    /* Add to error list */
    if (err && !alda_error_list_full(&p->error_list)) {
        alda_error_list_add(&p->error_list, err);
    }

    /* Keep first error for backward compatibility */
    if (!p->error) {
        p->error = alda_error_new_detailed(
            ALDA_ERR_SYNTAX, msg, pos, p->source,
            p->parse_context, NULL, found_buf
        );
    }
}

static void set_error_expected(AldaParser* p, const char* msg, const char* expected) {
    AldaToken* tok = peek(p);
    AldaSourcePos pos = tok ? tok->pos : alda_pos_new(1, 1, p->filename);

    /* Build "found" description */
    char found_buf[64];
    if (tok && tok->type != ALDA_TOK_EOF) {
        snprintf(found_buf, sizeof(found_buf), "%s", token_type_name(tok->type));
    } else {
        snprintf(found_buf, sizeof(found_buf), "end of input");
    }

    AldaError* err = alda_error_new_detailed(
        ALDA_ERR_SYNTAX, msg, pos, p->source,
        p->parse_context, expected, found_buf
    );

    if (err && !alda_error_list_full(&p->error_list)) {
        alda_error_list_add(&p->error_list, err);
    }

    if (!p->error) {
        p->error = alda_error_new_detailed(
            ALDA_ERR_SYNTAX, msg, pos, p->source,
            p->parse_context, expected, found_buf
        );
    }
}

/**
 * @brief Synchronize parser state after an error.
 *
 * Advances tokens until we reach a synchronization point where parsing
 * can safely resume. Recovery points include:
 * - Newlines (statement boundaries)
 * - Closing delimiters (for nested contexts)
 * - Part declarations (top-level structure)
 * - Variable definitions (top-level structure)
 */
static void synchronize(AldaParser* p) {
    advance(p);

    while (!is_at_end(p)) {
        /* Stop at newlines - natural statement boundaries */
        if (check(p, ALDA_TOK_NEWLINE)) {
            advance(p);
            return;
        }

        /* Stop at structural delimiters */
        AldaToken* tok = peek(p);
        if (!tok) return;

        switch (tok->type) {
            /* Closing delimiters - let caller handle */
            case ALDA_TOK_RIGHT_PAREN:
            case ALDA_TOK_BRACKET_CLOSE:
            case ALDA_TOK_CRAM_CLOSE:
                return;

            /* Part declaration markers */
            case ALDA_TOK_NAME: {
                AldaToken* next = peek_next(p);
                if (next && (next->type == ALDA_TOK_COLON ||
                             next->type == ALDA_TOK_EQUALS)) {
                    return;
                }
                break;
            }

            default:
                break;
        }

        advance(p);
    }
}

static char* strdup_safe(const char* s) {
    if (!s) return NULL;
    size_t len = strlen(s);
    char* copy = (char*)malloc(len + 1);
    if (copy) memcpy(copy, s, len + 1);
    return copy;
}

/* Forward declarations */
static AldaNode* parse_event(AldaParser* p);
static AldaNode* parse_event_sequence(AldaParser* p, AldaTokenType stop);
static AldaNode* parse_duration(AldaParser* p);
static int is_part_declaration(AldaParser* p);

/* Parse a note or chord */
static AldaNode* parse_note(AldaParser* p) {
    AldaToken* tok = advance(p);
    char letter = tok->literal.char_val;
    AldaSourcePos pos = tok->pos;

    /* Collect accidentals */
    char accidentals[16];
    int acc_count = 0;
    while (acc_count < 15 && (check(p, ALDA_TOK_SHARP) ||
                               check(p, ALDA_TOK_FLAT) ||
                               check(p, ALDA_TOK_NATURAL))) {
        AldaToken* acc = advance(p);
        accidentals[acc_count++] = acc->lexeme[0];
    }
    accidentals[acc_count] = '\0';

    char* acc_str = NULL;
    if (acc_count > 0) {
        acc_str = strdup_safe(accidentals);
    }

    /* Parse duration if present */
    AldaNode* duration = NULL;
    if (check(p, ALDA_TOK_NOTE_LENGTH) ||
        check(p, ALDA_TOK_NOTE_LENGTH_MS) ||
        check(p, ALDA_TOK_NOTE_LENGTH_S)) {
        duration = parse_duration(p);
    }

    /* Check for slur/tie */
    int slurred = match(p, ALDA_TOK_TIE);

    return alda_node_note(letter, acc_str, duration, slurred, pos);
}

static AldaNode* parse_rest(AldaParser* p) {
    AldaToken* tok = advance(p);
    AldaSourcePos pos = tok->pos;

    AldaNode* duration = NULL;
    if (check(p, ALDA_TOK_NOTE_LENGTH) ||
        check(p, ALDA_TOK_NOTE_LENGTH_MS) ||
        check(p, ALDA_TOK_NOTE_LENGTH_S)) {
        duration = parse_duration(p);
    }

    return alda_node_rest(duration, pos);
}

static AldaNode* parse_duration_component(AldaParser* p) {
    AldaToken* tok = advance(p);
    AldaSourcePos pos = tok->pos;

    if (tok->type == ALDA_TOK_NOTE_LENGTH) {
        double denominator = tok->literal.float_val;
        int dots = 0;
        while (match(p, ALDA_TOK_DOT)) {
            dots++;
        }
        return alda_node_note_length(denominator, dots, pos);
    } else if (tok->type == ALDA_TOK_NOTE_LENGTH_MS) {
        return alda_node_note_length_ms(tok->literal.int_val, pos);
    } else if (tok->type == ALDA_TOK_NOTE_LENGTH_S) {
        return alda_node_note_length_s(tok->literal.float_val, pos);
    }

    return NULL;
}

static AldaNode* parse_duration(AldaParser* p) {
    AldaSourcePos pos = peek(p)->pos;
    AldaNode* components = NULL;

    AldaNode* comp = parse_duration_component(p);
    if (comp) {
        alda_node_append(&components, comp);
    }

    /* Handle tied durations (e.g., c4~4).
     *
     * A tie may be split across a barline, and the barline may sit on either
     * side of the tilde and be separated by newlines - all of these are one
     * tied duration:
     *
     *     c4~2        c4~|2        c4 |~2        c4~|
     *                                            |2
     *
     * So scan past any barlines and newlines looking for a TIE, then past any
     * more of them looking for a duration component. Only commit (advance the
     * real cursor) once both are found; otherwise the tilde is a slur onto the
     * next note and belongs to parse_note, and the barline is a standalone
     * event. */
    while (1) {
        size_t probe = p->current;

        while (probe < p->token_count &&
               (p->tokens[probe].type == ALDA_TOK_BARLINE ||
                p->tokens[probe].type == ALDA_TOK_NEWLINE)) {
            probe++;
        }
        if (probe >= p->token_count || p->tokens[probe].type != ALDA_TOK_TIE) break;
        probe++;  /* the TIE itself */

        /* The tie may also be written on both sides of the barline, as in
         * "d4.~4~|" at the end of one bar continued by "|~4.~8" at the start of
         * the next, so absorb any further tildes here too. */
        while (probe < p->token_count &&
               (p->tokens[probe].type == ALDA_TOK_BARLINE ||
                p->tokens[probe].type == ALDA_TOK_NEWLINE ||
                p->tokens[probe].type == ALDA_TOK_TIE)) {
            probe++;
        }
        if (probe >= p->token_count) break;

        AldaTokenType t = p->tokens[probe].type;
        if (t != ALDA_TOK_NOTE_LENGTH &&
            t != ALDA_TOK_NOTE_LENGTH_MS &&
            t != ALDA_TOK_NOTE_LENGTH_S) {
            /* Slur onto a following note, not a tied duration. */
            break;
        }

        p->current = probe;
        comp = parse_duration_component(p);
        if (comp) {
            alda_node_append(&components, comp);
        }
    }

    return alda_node_duration(components, pos);
}

static AldaNode* parse_note_or_chord(AldaParser* p) {
    AldaNode* first = parse_note(p);
    if (!first) return NULL;

    /* Check for chord (separated by /) */
    if (!check(p, ALDA_TOK_SEPARATOR)) {
        return first;
    }

    AldaSourcePos pos = first->pos;
    AldaNode* notes = first;

    while (match(p, ALDA_TOK_SEPARATOR)) {
        skip_newlines(p);

        /* Handle octave changes within chord (e.g., c/e/g/>c) */
        while (check(p, ALDA_TOK_OCTAVE_UP) || check(p, ALDA_TOK_OCTAVE_DOWN)) {
            AldaToken* tok = advance(p);
            AldaNode* octave_node;
            if (tok->type == ALDA_TOK_OCTAVE_UP) {
                octave_node = alda_node_octave_up(tok->pos);
            } else {
                octave_node = alda_node_octave_down(tok->pos);
            }
            if (octave_node) {
                alda_node_append(&notes, octave_node);
            }
        }

        if (check(p, ALDA_TOK_NOTE_LETTER)) {
            AldaNode* note = parse_note(p);
            if (note) {
                alda_node_append(&notes, note);
            }
        } else if (check(p, ALDA_TOK_REST_LETTER)) {
            AldaNode* rest = parse_rest(p);
            if (rest) {
                alda_node_append(&notes, rest);
            }
        } else {
            break;
        }
    }

    if (alda_node_count(notes) > 1) {
        return alda_node_chord(notes, pos);
    }
    return notes;
}

static AldaNode* parse_sexp(AldaParser* p) {
    AldaToken* tok = advance(p); /* consume ( */
    AldaSourcePos pos = tok->pos;
    AldaNode* elements = NULL;

    /* Save and set context */
    const char* prev_context = p->parse_context;
    p->parse_context = "in S-expression";

    skip_newlines(p);

    while (!is_at_end(p) && !check(p, ALDA_TOK_RIGHT_PAREN)) {
        AldaNode* elem = NULL;

        if (check(p, ALDA_TOK_LEFT_PAREN)) {
            elem = parse_sexp(p);
        } else if (check(p, ALDA_TOK_QUOTE)) {
            /* Quoted list like '(g major) - parse as regular list */
            advance(p);  /* consume ' */
            if (check(p, ALDA_TOK_LEFT_PAREN)) {
                elem = parse_sexp(p);
            } else {
                set_error_expected(p, "Expected '(' after quote", "'('");
                /* Try to recover - skip to closing paren or newline */
                while (!is_at_end(p) &&
                       !check(p, ALDA_TOK_RIGHT_PAREN) &&
                       !check(p, ALDA_TOK_NEWLINE)) {
                    advance(p);
                }
                break;
            }
        } else if (check(p, ALDA_TOK_SYMBOL)) {
            AldaToken* sym = advance(p);
            elem = alda_node_lisp_symbol(strdup_safe(sym->lexeme), sym->pos);
        } else if (check(p, ALDA_TOK_NUMBER)) {
            AldaToken* num = advance(p);
            elem = alda_node_lisp_number(num->literal.float_val, num->pos);
        } else if (check(p, ALDA_TOK_STRING)) {
            AldaToken* str = advance(p);
            /* Strip quotes from lexeme */
            size_t len = str->lexeme_len;
            char* value = NULL;
            if (len >= 2) {
                value = (char*)malloc(len - 1);
                if (value) {
                    memcpy(value, str->lexeme + 1, len - 2);
                    value[len - 2] = '\0';
                }
            }
            elem = alda_node_lisp_string(value, str->pos);
        } else if (check(p, ALDA_TOK_NEWLINE)) {
            advance(p);
            continue;
        } else {
            set_error_expected(p, "Unexpected token in S-expression",
                              "symbol, number, string, or '('");
            /* Skip the unexpected token and try to continue */
            advance(p);
            continue;
        }

        if (elem) {
            alda_node_append(&elements, elem);
        }
        skip_newlines(p);
    }

    if (!match(p, ALDA_TOK_RIGHT_PAREN)) {
        set_error_expected(p, "Unclosed S-expression", "')'");
    }

    /* Restore context */
    p->parse_context = prev_context;

    return alda_node_lisp_list(elements, pos);
}

static AldaNode* parse_cram(AldaParser* p) {
    AldaToken* tok = advance(p); /* consume { */
    AldaSourcePos pos = tok->pos;

    /* Save and set context */
    const char* prev_context = p->parse_context;
    p->parse_context = "in cram expression";

    AldaNode* events = parse_event_sequence(p, ALDA_TOK_CRAM_CLOSE);

    if (!match(p, ALDA_TOK_CRAM_CLOSE)) {
        set_error_expected(p, "Unclosed cram expression", "'}'");
    }

    AldaNode* duration = NULL;
    if (check(p, ALDA_TOK_NOTE_LENGTH) ||
        check(p, ALDA_TOK_NOTE_LENGTH_MS) ||
        check(p, ALDA_TOK_NOTE_LENGTH_S)) {
        duration = parse_duration(p);
    }

    /* Restore context */
    p->parse_context = prev_context;

    return alda_node_cram(events, duration, pos);
}

static AldaNode* parse_bracket_seq(AldaParser* p) {
    AldaToken* tok = advance(p); /* consume [ */
    AldaSourcePos pos = tok->pos;

    /* Save and set context */
    const char* prev_context = p->parse_context;
    p->parse_context = "in bracketed sequence";

    AldaNode* events = parse_event_sequence(p, ALDA_TOK_BRACKET_CLOSE);

    if (!match(p, ALDA_TOK_BRACKET_CLOSE)) {
        set_error_expected(p, "Unclosed bracketed sequence", "']'");
    }

    /* Restore context */
    p->parse_context = prev_context;

    return alda_node_bracket_seq(events, pos);
}

static AldaNode* parse_marker(AldaParser* p) {
    AldaToken* tok = advance(p);
    /* Skip the % prefix */
    char* name = strdup_safe(tok->lexeme + 1);
    return alda_node_marker(name, tok->pos);
}

static AldaNode* parse_at_marker(AldaParser* p) {
    AldaToken* tok = advance(p);
    /* Skip the @ prefix */
    char* name = strdup_safe(tok->lexeme + 1);
    return alda_node_at_marker(name, tok->pos);
}

static AldaNode* parse_voice(AldaParser* p) {
    AldaToken* tok = advance(p);
    /* Parse voice number from "V1:" format */
    int number = atoi(tok->lexeme + 1); /* Skip 'V' */
    AldaSourcePos pos = tok->pos;

    /* Parse events until the next voice marker, a new part declaration, or the
     * end of input. A part declaration implicitly closes the voice group - an
     * explicit "V0:" is optional, not required. */
    AldaNode* events = NULL;
    while (!is_at_end(p) && !check(p, ALDA_TOK_VOICE_MARKER)) {
        skip_newlines(p);
        if (is_at_end(p) || check(p, ALDA_TOK_VOICE_MARKER)) break;
        if (is_part_declaration(p)) break;

        AldaNode* event = parse_event(p);
        if (event) {
            alda_node_append(&events, event);
        } else {
            break;
        }
    }

    return alda_node_voice(number, events, pos);
}

static AldaNode* parse_voice_group(AldaParser* p) {
    AldaSourcePos pos = peek(p)->pos;
    AldaNode* voices = NULL;

    while (check(p, ALDA_TOK_VOICE_MARKER)) {
        AldaToken* tok = peek(p);
        int number = atoi(tok->lexeme + 1);

        /* V0: ends voice group */
        if (number == 0) {
            advance(p);
            break;
        }

        AldaNode* voice = parse_voice(p);
        if (voice) {
            alda_node_append(&voices, voice);
        }
    }

    return alda_node_voice_group(voices, pos);
}

static AldaNode* parse_primary_event(AldaParser* p) {
    skip_newlines(p);

    if (is_at_end(p)) return NULL;

    AldaToken* tok = peek(p);

    switch (tok->type) {
        case ALDA_TOK_NOTE_LETTER:
            return parse_note_or_chord(p);

        case ALDA_TOK_REST_LETTER:
            return parse_rest(p);

        case ALDA_TOK_OCTAVE_SET: {
            AldaToken* t = advance(p);
            return alda_node_octave_set(t->literal.int_val, t->pos);
        }

        case ALDA_TOK_OCTAVE_UP: {
            AldaToken* t = advance(p);
            return alda_node_octave_up(t->pos);
        }

        case ALDA_TOK_OCTAVE_DOWN: {
            AldaToken* t = advance(p);
            return alda_node_octave_down(t->pos);
        }

        case ALDA_TOK_BARLINE: {
            AldaToken* t = advance(p);
            return alda_node_barline(t->pos);
        }

        case ALDA_TOK_LEFT_PAREN:
            return parse_sexp(p);

        case ALDA_TOK_CRAM_OPEN:
            return parse_cram(p);

        case ALDA_TOK_BRACKET_OPEN:
            return parse_bracket_seq(p);

        case ALDA_TOK_MARKER:
            return parse_marker(p);

        case ALDA_TOK_AT_MARKER:
            return parse_at_marker(p);

        case ALDA_TOK_VOICE_MARKER:
            return parse_voice_group(p);

        case ALDA_TOK_NAME: {
            /* Could be variable reference or part of part declaration */
            AldaToken* t = advance(p);
            return alda_node_var_ref(strdup_safe(t->lexeme), t->pos);
        }

        default:
            return NULL;
    }
}

/* Parse repetition specification like "1", "1,3", "1-3", "1,3-5,7"
 * Returns array of repetition numbers and count via out parameters */
static int parse_rep_spec(const char* spec, int** out_reps, size_t* out_count) {
    if (!spec || !out_reps || !out_count) return -1;

    /* Allocate initial array */
    size_t capacity = 16;
    int* reps = (int*)malloc(capacity * sizeof(int));
    if (!reps) return -1;
    size_t count = 0;

    const char* p = spec;
    while (*p) {
        /* Skip any non-digit characters at start */
        while (*p && !isdigit((unsigned char)*p)) p++;
        if (!*p) break;

        /* Parse first number (capped at 10000 to prevent overflow) */
        int start = 0;
        while (*p && isdigit((unsigned char)*p)) {
            start = start * 10 + (*p - '0');
            if (start > 10000) { start = 10000; break; }
            p++;
        }
        while (*p && isdigit((unsigned char)*p)) p++;  /* skip remaining digits */

        int end = start;

        /* Check for range */
        if (*p == '-') {
            p++;
            end = 0;
            while (*p && isdigit((unsigned char)*p)) {
                end = end * 10 + (*p - '0');
                if (end > 10000) { end = 10000; break; }
                p++;
            }
            while (*p && isdigit((unsigned char)*p)) p++;  /* skip remaining digits */
        }

        /* Add all numbers in range */
        for (int i = start; i <= end; i++) {
            if (count >= capacity) {
                capacity *= 2;
                int* new_reps = (int*)realloc(reps, capacity * sizeof(int));
                if (!new_reps) {
                    free(reps);
                    return -1;
                }
                reps = new_reps;
            }
            reps[count++] = i;
        }

        /* Skip comma if present */
        if (*p == ',') p++;
    }

    *out_reps = reps;
    *out_count = count;
    return 0;
}

static AldaNode* parse_postfix(AldaParser* p, AldaNode* event) {
    /* Handle repeat (*N) */
    if (check(p, ALDA_TOK_REPEAT)) {
        AldaToken* tok = advance(p);
        int count = tok->literal.int_val;
        event = alda_node_repeat(event, count, tok->pos);
    }

    /* Handle on-repetitions ('1-3,5) */
    if (check(p, ALDA_TOK_REPETITIONS)) {
        AldaToken* tok = advance(p);
        /* Parse the repetition specification from the lexeme */
        int* reps = NULL;
        size_t rep_count = 0;
        if (parse_rep_spec(tok->lexeme, &reps, &rep_count) == 0 && rep_count > 0) {
            event = alda_node_on_reps(event, reps, rep_count, tok->pos);
        } else {
            /* Failed to parse or empty - free any allocated memory */
            free(reps);
            /* Create empty on_reps (will always play) */
            event = alda_node_on_reps(event, NULL, 0, tok->pos);
        }
    }

    return event;
}

static AldaNode* parse_event(AldaParser* p) {
    AldaNode* event = parse_primary_event(p);
    if (!event) return NULL;
    return parse_postfix(p, event);
}

static AldaNode* parse_event_sequence(AldaParser* p, AldaTokenType stop) {
    AldaNode* events = NULL;

    while (!is_at_end(p) && !p->error) {
        /* Check for stop token BEFORE skipping newlines
         * (important when stop=NEWLINE for variable definitions) */
        if (stop != ALDA_TOK_EOF && check(p, stop)) break;

        /* Skip newlines between events (but not when NEWLINE is our stop) */
        if (stop != ALDA_TOK_NEWLINE) {
            skip_newlines(p);
        }

        if (is_at_end(p)) break;
        if (stop != ALDA_TOK_EOF && check(p, stop)) break;

        /* Check for part declaration (NAME followed by COLON, ALIAS, or SEPARATOR) */
        if (check(p, ALDA_TOK_NAME)) {
            AldaToken* next = peek_next(p);
            if (next && (next->type == ALDA_TOK_COLON ||
                         next->type == ALDA_TOK_SEPARATOR ||
                         next->type == ALDA_TOK_ALIAS)) {
                break; /* Part declaration - let caller handle */
            }
            /* Also check for variable definition (NAME followed by EQUALS) */
            if (next && next->type == ALDA_TOK_EQUALS) {
                break; /* Variable definition - let caller handle */
            }
        }

        AldaNode* event = parse_event(p);
        if (event) {
            alda_node_append(&events, event);
        } else {
            break;
        }
    }

    return events;
}

static AldaNode* parse_part_declaration(AldaParser* p) {
    AldaSourcePos pos = peek(p)->pos;
    char** names = NULL;
    size_t name_count = 0;
    size_t name_capacity = 4;

    names = (char**)malloc(name_capacity * sizeof(char*));
    if (!names) return NULL;

    /* Parse instrument names separated by / */
    do {
        if (check(p, ALDA_TOK_NAME)) {
            AldaToken* tok = advance(p);
            if (name_count >= name_capacity) {
                name_capacity *= 2;
                char** new_names = (char**)realloc((void*)names, name_capacity * sizeof(char*));
                if (!new_names) {
                    for (size_t i = 0; i < name_count; i++) free(names[i]);
                    free((void*)names);
                    return NULL;
                }
                names = new_names;
            }
            names[name_count++] = strdup_safe(tok->lexeme);
        }
    } while (match(p, ALDA_TOK_SEPARATOR));

    /* Parse optional alias */
    char* alias = NULL;
    if (check(p, ALDA_TOK_ALIAS)) {
        AldaToken* tok = advance(p);
        /* Strip quotes */
        size_t len = tok->lexeme_len;
        if (len >= 2) {
            alias = (char*)malloc(len - 1);
            if (alias) {
                memcpy(alias, tok->lexeme + 1, len - 2);
                alias[len - 2] = '\0';
            }
        }
    }

    /* Expect colon */
    if (!match(p, ALDA_TOK_COLON)) {
        set_error_expected(p, "Expected ':' after part declaration", "':'");
    }

    return alda_node_part_decl(names, name_count, alias, pos);
}

static int is_part_declaration(AldaParser* p) {
    if (!check(p, ALDA_TOK_NAME)) return 0;

    /* Look ahead for colon or separator-colon */
    size_t save = p->current;
    int found = 0;

    while (p->current < p->token_count) {
        AldaToken* tok = &p->tokens[p->current];
        if (tok->type == ALDA_TOK_COLON) {
            found = 1;
            break;
        } else if (tok->type == ALDA_TOK_SEPARATOR ||
                   tok->type == ALDA_TOK_ALIAS ||
                   tok->type == ALDA_TOK_NAME) {
            p->current++;
        } else {
            break;
        }
    }

    p->current = save;
    return found;
}

static int is_var_definition(AldaParser* p) {
    /* Variable definition: NAME = ... */
    if (!check(p, ALDA_TOK_NAME)) return 0;

    /* Look ahead for EQUALS */
    AldaToken* next = peek_next(p);
    return next && next->type == ALDA_TOK_EQUALS;
}

static AldaNode* parse_var_definition(AldaParser* p) {
    /* Parse: NAME = events */
    const char* prev_context = p->parse_context;
    p->parse_context = "in variable definition";

    if (!check(p, ALDA_TOK_NAME)) {
        set_error_expected(p, "Expected variable name", "identifier");
        p->parse_context = prev_context;
        return NULL;
    }

    AldaToken* name_tok = advance(p);
    AldaSourcePos pos = name_tok->pos;
    char* name = strdup_safe(name_tok->lexeme);

    if (!match(p, ALDA_TOK_EQUALS)) {
        set_error_expected(p, "Expected '=' after variable name", "'='");
        free(name);
        p->parse_context = prev_context;
        return NULL;
    }

    /* Parse the events (RHS of the definition) */
    /* Events continue until newline or another top-level construct */
    AldaNode* events = NULL;

    /* Check if RHS is a bracket sequence */
    if (check(p, ALDA_TOK_BRACKET_OPEN)) {
        events = parse_event(p);
    } else {
        /* Parse events until newline */
        events = parse_event_sequence(p, ALDA_TOK_NEWLINE);
    }

    p->parse_context = prev_context;
    return alda_node_var_def(name, events, pos);
}

static AldaNode* parse_top_level(AldaParser* p) {
    AldaSourcePos pos = alda_pos_new(1, 1, p->filename);
    AldaNode* root = alda_node_root(pos);
    if (!root) return NULL;

    /* Continue parsing even after errors (collect multiple errors) */
    while (!is_at_end(p) && !alda_error_list_full(&p->error_list)) {
        skip_newlines(p);
        if (is_at_end(p)) break;

        if (is_var_definition(p)) {
            /* Parse variable definition: name = events */
            AldaNode* var_def = parse_var_definition(p);
            if (var_def) {
                alda_node_append(&root->data.root.children, var_def);
            } else {
                /* Recovery: skip to next statement */
                synchronize(p);
            }
        } else if (is_part_declaration(p)) {
            /* Parse part declaration */
            AldaNode* part_decl = parse_part_declaration(p);
            if (part_decl) {
                alda_node_append(&root->data.root.children, part_decl);
            }

            /* Parse events following part declaration */
            AldaNode* events = parse_event_sequence(p, ALDA_TOK_EOF);
            if (events) {
                AldaNode* event_seq = alda_node_event_seq(events, events->pos);
                if (event_seq) {
                    alda_node_append(&root->data.root.children, event_seq);
                }
            }
        } else {
            /* Parse events without part declaration (e.g., global attributes) */
            AldaNode* events = parse_event_sequence(p, ALDA_TOK_EOF);
            if (events) {
                AldaNode* event_seq = alda_node_event_seq(events, events->pos);
                if (event_seq) {
                    alda_node_append(&root->data.root.children, event_seq);
                }
            } else if (!is_at_end(p)) {
                /* No events could be parsed and we're not at EOF.
                 * Report error and use synchronize for recovery. */
                set_error(p, "Unexpected token");
                synchronize(p);
            }
            /* Continue loop - there may be part declarations following */
        }
    }

    return root;
}

/* Public API */

AldaParser* alda_parser_new(const char* source, const char* filename) {
    AldaParser* p = (AldaParser*)malloc(sizeof(AldaParser));
    if (!p) return NULL;

    p->source = source;
    p->filename = filename;
    p->tokens = NULL;
    p->token_count = 0;
    p->current = 0;
    p->error = NULL;
    p->parse_context = NULL;
    alda_error_list_init(&p->error_list, 10);  /* Collect up to 10 errors */

    return p;
}

void alda_parser_free(AldaParser* parser) {
    if (parser) {
        alda_tokens_free(parser->tokens, parser->token_count);
        alda_error_free(parser->error);
        alda_error_list_free(&parser->error_list);
        free(parser);
    }
}

AldaNode* alda_parser_parse(AldaParser* parser) {
    /* First, scan the source */
    AldaScanner* scanner = alda_scanner_new(parser->source, parser->filename);
    if (!scanner) return NULL;

    parser->tokens = alda_scanner_scan(scanner, &parser->token_count);

    if (alda_scanner_has_error(scanner)) {
        const AldaError* scan_err = alda_scanner_error(scanner);
        parser->error = alda_error_new(scan_err->type, scan_err->message,
                                       scan_err->pos, parser->source);
        alda_scanner_free(scanner);
        return NULL;
    }

    alda_scanner_free(scanner);

    if (!parser->tokens) {
        parser->error = alda_error_new(ALDA_ERR_MEMORY, "Failed to allocate tokens",
                                       alda_pos_new(0, 0, parser->filename), NULL);
        return NULL;
    }

    /* Parse the tokens */
    return parse_top_level(parser);
}

int alda_parser_has_error(AldaParser* parser) {
    return parser->error != NULL;
}

const AldaError* alda_parser_error(AldaParser* parser) {
    return parser->error;
}

char* alda_parser_error_string(AldaParser* parser) {
    if (!parser->error) return NULL;
    return alda_error_format(parser->error);
}

int alda_parser_error_count(AldaParser* parser) {
    return parser->error_list.count;
}

char* alda_parser_all_errors_string(AldaParser* parser) {
    if (parser->error_list.count == 0) {
        /* Fall back to single error if no list */
        if (parser->error) {
            return alda_error_format(parser->error);
        }
        return NULL;
    }
    return alda_error_list_format(&parser->error_list);
}

AldaNode* alda_parse(const char* source, const char* filename, char** error) {
    AldaParser* parser = alda_parser_new(source, filename);
    if (!parser) {
        if (error) *error = strdup_safe("Failed to allocate parser");
        return NULL;
    }

    AldaNode* ast = alda_parser_parse(parser);

    if (alda_parser_has_error(parser)) {
        if (error) {
            *error = alda_parser_error_string(parser);
        }
        alda_ast_free(ast);  /* Partial tree from error recovery */
        alda_parser_free(parser);
        return NULL;
    }

    alda_parser_free(parser);
    return ast;
}
