/* midi_clock.h - script time, real or virtual
 *
 * Without a sink, midi_out_send sends to the port and midi_sleep sleeps.
 * With a sink installed (by a plugin host), script time is virtual:
 * midi_out_send stamps each message with it, midi_sleep advances it, and
 * the sink decides how long each advance waits. Script time is in ms.
 * The sink and the clock belong to the one thread that runs scripts. */

#ifndef MIDI_CLOCK_H
#define MIDI_CLOCK_H

#include <stddef.h>

#include <libremidi/libremidi-c.h>

#ifdef __cplusplus
extern "C" {
#endif

typedef struct midi_sink {
    void* user;
    /* A message sent at script time ms. */
    void (*send)(void* user, double ms, const unsigned char* msg, size_t len);
    /* Script time has advanced to ms; returns when the host is ready for it. */
    void (*wait)(void* user, double ms);
    /* Nonzero when the running script should stop; interpreters poll it. */
    int (*cancelled)(void* user);
} midi_sink;

/* NULL removes the sink. The sink is copied. */
void midi_sink_install(const midi_sink* sink);
int midi_sink_active(void);

double midi_clock_ms(void);
/* Sets script time without waiting, e.g. to a voice's wake time. */
void midi_clock_set_ms(double ms);
/* Sets script time to ms if later, then waits on the sink. */
void midi_clock_wait_until(double ms);
int midi_clock_cancelled(void);

/* libremidi_midi_out_send_message, or the sink's send when one is installed. */
int midi_out_send(libremidi_midi_out_handle* out, const unsigned char* msg, size_t len);

#ifdef __cplusplus
}
#endif

#endif /* MIDI_CLOCK_H */
