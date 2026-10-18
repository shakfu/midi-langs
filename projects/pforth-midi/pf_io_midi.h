/* pf_io_midi.h - output redirection for the pforth-midi CLAP plugin (pf_io_midi.c) */

#ifndef PF_IO_MIDI_H
#define PF_IO_MIDI_H

#ifdef __cplusplus
extern "C" {
#endif

/* Terminal output goes to write instead of stdout, and KEY reads an empty
 * line, until called with NULL. */
void pfm_set_output(void (*write)(char c, void* user), void* user);

#ifdef __cplusplus
}
#endif

#endif /* PF_IO_MIDI_H */
