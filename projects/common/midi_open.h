/* midi_open.h - MIDI output creation with the backend's error text preserved */

#ifndef MIDI_OPEN_H
#define MIDI_OPEN_H

#include <libremidi/libremidi-c.h>

#ifdef __cplusplus
extern "C" {
#endif

/* libremidi_midi_out_new with the backend error message captured.
 * libremidi collapses every backend failure to -EIO, which is not enough to
 * tell a missing port from a dead MIDI server connection. Arguments and
 * return value are otherwise those of libremidi_midi_out_new. */
int midi_out_open(const libremidi_midi_configuration* conf,
                  const libremidi_api_configuration* api,
                  libremidi_midi_out_handle** out);

/* Backend message for the last failed midi_out_open, or "".
 * Empty when the caller supplied its own on_error callback. */
const char* midi_out_last_error(void);

#ifdef __cplusplus
}
#endif

#endif /* MIDI_OPEN_H */
