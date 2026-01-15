/*
 * music_notation.h - Alda-like musical notation for Joy-MIDI
 *
 * Handles parsing of musical notation symbols like:
 * - Notes: c, d, e, f, g, a, b
 * - Duration: c4, c8, c16 (quarter, eighth, sixteenth)
 * - Dotted: c4., c4..
 * - Accidentals: c+, c-, c_ (sharp, flat, natural)
 * - Octaves: o4, >, <
 * - Rests: r, r4, r8
 * - Dynamics: ppp, pp, p, mp, mf, f, ff, fff
 * - Chords: c/e/g, c:maj, c:min7
 */

#ifndef MUSIC_NOTATION_H
#define MUSIC_NOTATION_H

#include "joy_runtime.h"
#include "music_context.h"

/* Initialize music notation system - sets up handler and creates MusicContext */
void music_notation_init(JoyContext* ctx);

/* Cleanup music notation system - frees MusicContext */
void music_notation_cleanup(JoyContext* ctx);

/* Parse and execute musical notation. Returns true if handled. */
bool music_handle_symbol(JoyContext* ctx, const char* name);

/* Get the music context from a Joy context */
MusicContext* music_get_context(JoyContext* ctx);

/* Play primitives - pop notes/lists and play them */
void music_play_(JoyContext* ctx);   /* sequential playback */
void music_chord_(JoyContext* ctx);  /* simultaneous playback */

/* notes - execute quotation and collect results into a list */
void music_notes_(JoyContext* ctx);

#endif /* MUSIC_NOTATION_H */
