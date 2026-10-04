/* midi_open.c - see midi_open.h */

#include "midi_open.h"

#include <stdio.h>
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

void midi_port_label(const libremidi_midi_out_port* port, char* buf, size_t size) {
    const char* dev = NULL;
    const char* name = NULL;
    size_t dev_len = 0, name_len = 0;
    if (libremidi_midi_out_port_name(port, &name, &name_len) != 0) {
        name = "";
        name_len = 0;
    }
    if (libremidi_midi_out_port_device_name(port, &dev, &dev_len) == 0 && dev_len > 0) {
        snprintf(buf, size, "%.*s: %.*s", (int)dev_len, dev, (int)name_len, name);
    } else {
        snprintf(buf, size, "%.*s", (int)name_len, name);
    }
}
