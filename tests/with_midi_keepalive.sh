#!/bin/bash
# Run a command with one MIDI client held open for its duration.
#
# See tests/midi_keepalive.c: without a live client, macOS stops MIDIServer
# between the short-lived processes these tests launch, and a process that
# starts during that teardown fails to open any MIDI port.
#
# Usage: with_midi_keepalive.sh <path-to-midi_keepalive> <command> [args...]

set -u

KEEPALIVE="$1"
shift

pid=""
for attempt in 1 2 3 4 5; do
    log=$(mktemp)
    "$KEEPALIVE" > "$log" 2>&1 &
    pid=$!

    for _ in $(seq 1 50); do
        grep -q ready "$log" 2>/dev/null && break
        kill -0 "$pid" 2>/dev/null || break
        sleep 0.1
    done

    if grep -q ready "$log" 2>/dev/null; then
        rm -f "$log"
        break
    fi

    # The keepalive can itself lose the race; a fresh process gets a fresh
    # connection, so try again before giving up and running unprotected.
    kill "$pid" 2>/dev/null
    pid=""
    rm -f "$log"
    sleep 0.5
done

cleanup() {
    [ -n "$pid" ] && kill "$pid" 2>/dev/null
}
trap cleanup EXIT

"$@"
