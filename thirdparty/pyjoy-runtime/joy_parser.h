/**
 * joy_parser.h - Simple Joy tokenizer and parser
 *
 * Provides parsing of Joy source code into JoyValue terms
 * and a simple REPL for interactive use.
 */

#ifndef JOY_PARSER_H
#define JOY_PARSER_H

#include "joy_runtime.h"

/* Parse a line of Joy source code and execute it */
void joy_eval_line(JoyContext* ctx, const char* line);

/* Parse Joy source and return a quotation of terms */
JoyQuotation* joy_parse(const char* source);

/* Run an interactive REPL */
void joy_repl(JoyContext* ctx);

/* Execute a Joy source file */
int joy_load_file(JoyContext* ctx, const char* filename);

#endif /* JOY_PARSER_H */
