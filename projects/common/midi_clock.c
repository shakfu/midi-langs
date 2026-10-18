/* midi_clock.c - see midi_clock.h */

#include "midi_clock.h"

static midi_sink g_sink;
static int g_sink_active = 0;
static double g_now_ms = 0.0;

void midi_sink_install(const midi_sink* sink) {
    if (sink) {
        g_sink = *sink;
        g_sink_active = 1;
    } else {
        g_sink_active = 0;
    }
    g_now_ms = 0.0;
}

int midi_sink_active(void) {
    return g_sink_active;
}

double midi_clock_ms(void) {
    return g_now_ms;
}

void midi_clock_set_ms(double ms) {
    g_now_ms = ms;
}

void midi_clock_wait_until(double ms) {
    if (ms > g_now_ms) {
        g_now_ms = ms;
    }
    if (g_sink_active) {
        g_sink.wait(g_sink.user, g_now_ms);
    }
}

int midi_clock_cancelled(void) {
    return g_sink_active && g_sink.cancelled(g_sink.user);
}

int midi_out_send(libremidi_midi_out_handle* out, const unsigned char* msg, size_t len) {
    if (g_sink_active) {
        g_sink.send(g_sink.user, g_now_ms, msg, len);
        return 0;
    }
    return libremidi_midi_out_send_message(out, msg, len);
}
