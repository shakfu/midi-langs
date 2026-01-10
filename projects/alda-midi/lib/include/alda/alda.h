/**
 * @file alda.h
 * @brief Main header file for the libalda library.
 *
 * This header includes all public API headers for convenience.
 * Users can include just this header to access the complete API.
 *
 * Basic usage:
 *   #include <alda/alda.h>
 *
 *   AldaContext ctx;
 *   alda_context_init(&ctx);
 *   alda_midi_open_auto(&ctx, "MyApp");
 *   alda_interpret_string(&ctx, "piano: c d e f g", "<input>");
 *   alda_events_play(&ctx);
 *   alda_midi_cleanup(&ctx);
 *   alda_context_cleanup(&ctx);
 */

#ifndef ALDA_H
#define ALDA_H

/* Parser components */
#include "tokens.h"
#include "error.h"
#include "scanner.h"
#include "ast.h"
#include "parser.h"

/* Interpreter components */
#include "context.h"
#include "instruments.h"
#include "interpreter.h"

/* MIDI and scheduling */
#include "midi_backend.h"
#include "tsf_backend.h"
#include "scheduler.h"
#include "async.h"

#endif /* ALDA_H */
