/* midi_open.c - see midi_open.h */

#include "midi_open.h"

#include <string.h>

static char g_last_error[256];

static void capture_error(void* ctx, const char* err, size_t len, const void* loc) {
    (void)ctx;
    (void)loc;
    if (len >= sizeof(g_last_error)) {
        len = sizeof(g_last_error) - 1;
    }
    memcpy(g_last_error, err, len);
    g_last_error[len] = '\0';
}

int midi_out_open(const libremidi_midi_configuration* conf,
                  const libremidi_api_configuration* api,
                  libremidi_midi_out_handle** out) {
    libremidi_midi_configuration c = *conf;
    if (!c.on_error.callback) {
        c.on_error.context = NULL;
        c.on_error.callback = capture_error;
    }

    g_last_error[0] = '\0';
    return libremidi_midi_out_new(&c, api, out);
}

const char* midi_out_last_error(void) {
    return g_last_error;
}
