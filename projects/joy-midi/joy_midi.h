/*
 * joy_midi.h - Joy MIDI extension API
 */

#ifndef JOY_MIDI_H
#define JOY_MIDI_H

#include "joy_runtime.h"

/*
 * Register all MIDI primitives with Joy's dictionary.
 * Call this after joy_context_new() and before joy_repl().
 */
void joy_midi_register_primitives(JoyContext* ctx);

/*
 * Cleanup MIDI resources.
 * Call this before program exit.
 */
void joy_midi_cleanup(void);

#endif /* JOY_MIDI_H */
