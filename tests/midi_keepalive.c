/* midi_keepalive.c - hold a virtual MIDI port open until killed.
 *
 * macOS launchd stops MIDIServer once the last client disconnects. A process
 * that starts while the server is tearing down gets a connection that is
 * already dead: MIDIClientCreate then returns -304 for the life of that
 * process, so retrying in-process cannot recover. The test suites launch
 * dozens of short-lived MIDI processes, which makes that window frequent.
 * One long-lived client keeps the server up and removes it.
 *
 * Prints "ready" once the port is open. Exits on SIGTERM, or after ten
 * minutes so a killed harness cannot leave it behind.
 */

#include "midi_open.h"

#include <stdio.h>
#include <unistd.h>

#define KEEPALIVE_TIMEOUT_SEC 600

int main(void) {
    libremidi_midi_configuration conf;
    libremidi_api_configuration api;

    if (libremidi_midi_configuration_init(&conf) != 0
        || libremidi_midi_api_configuration_init(&api) != 0) {
        fprintf(stderr, "keepalive: cannot init libremidi configuration\n");
        return 1;
    }

    conf.virtual_port = true;
    conf.port_name = "midi-langs-keepalive";

    libremidi_midi_out_handle* handle;
    if (midi_out_open(&conf, &api, &handle) != 0) {
        fprintf(stderr, "keepalive: %s\n", midi_out_last_error());
        return 1;
    }

    printf("ready\n");
    fflush(stdout);

    alarm(KEEPALIVE_TIMEOUT_SEC);
    pause();

    libremidi_midi_out_free(handle);
    return 0;
}
