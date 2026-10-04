#!/bin/bash
# Tests for pforth-midi, following docs/spec.md section 10.
#
# Usage: test_pforth_midi.sh <path-to-pforth_midi>

set -u

PFORTH_MIDI="${1:-$(dirname "$0")/../build/pforth_midi}"
TMP=$(mktemp -d)
DUMP_PID=""
cleanup() {
    [ -n "$DUMP_PID" ] && kill "$DUMP_PID" 2>/dev/null
    rm -rf "$TMP"
}
trap cleanup EXIT

if [ ! -x "$PFORTH_MIDI" ]; then
    echo "Error: pforth_midi not found at $PFORTH_MIDI"
    exit 1
fi

FAIL=0
pass() { echo "  PASS: $1"; }
fail() { echo "  FAIL: $1"; FAIL=1; }

# run CODE: run Forth source from a file; prints stdout+stderr
run() {
    printf '%s\n' "$1" > "$TMP/t.fs"
    timeout 30 "$PFORTH_MIDI" "$TMP/t.fs" 2>&1
}

# check NAME CODE EXPECTED: output, with spaces squeezed and trimmed, equals EXPECTED
check() {
    local out
    out=$(run "$2" | tr -s ' \n' ' ' | sed 's/^ //; s/ $//')
    if [ "$out" = "$3" ]; then pass "$1"; else fail "$1: expected '$3', got '$out'"; fi
}

# contains NAME CODE PATTERN: output matches grep -E PATTERN
contains() {
    if run "$2" | grep -qE "$3"; then pass "$1"; else fail "$1: no match for /$3/"; fi
}

echo "=== pforth-midi ==="

echo "Interpreter"
check "standard Forth" ': sq dup * ; 7 sq . 3 0 do i . loop' "49 0 1 2"
check "recurse" ': fact dup 1 > if dup 1- recurse * then ; 6 fact .' "720"
check "floats" '1.5e0 2e0 f* f.' "3.000000"
contains "unknown word aborts" 'xyz' "unrecognized word"
out=$(printf '1 2 + .\n' | timeout 10 "$PFORTH_MIDI" -q 2>&1); rc=$?
if [ $rc -eq 0 ] && echo "$out" | grep -q "^3 " && ! echo "$out" | grep -q "1 2 +"; then
    pass "piped input: exits at EOF, no echo"
else
    fail "piped input: rc=$rc output='$out'"
fi
# timeout runs pforth in a background process group; on a tty that must not stop it.
# "; true" stops sh exec'ing timeout as session leader, which cannot change group.
if script -qec true /dev/null > /dev/null 2>&1; then
    printf '7 7 * .\n' > "$TMP/t.fs"
    out=$(script -qec "timeout 10 '$PFORTH_MIDI' '$TMP/t.fs'; true" /dev/null 2>&1)
    if echo "$out" | grep -q "^49 "; then
        pass "file run in background on a tty"
    else
        fail "file run in background on a tty: output='$out'"
    fi
else
    echo "  SKIP: background tty run (no util-linux script)"
fi

echo "Pitch parsing"
check "naturals" 'c4 . d4 . e4 . f4 . g4 . a4 . b4 .' "60 62 64 65 67 69 71"
check "accidentals" 'c#4 . cs4 . db4 . eb4 . bb3 .' "61 61 61 63 58"
check "case-insensitive" 'C4 . Db4 . E4 .' "60 61 64"
check "range" 'c-1 . e-1 . g9 .' "0 4 127"
check "HEX keeps numbers" 'hex c4 . e4 . decimal' "C4 E4"
check "floats still parse" '1e4 f.' "10000.00"
check "pitch in definitions" ': tonic c4 ; tonic .' "60"
check ".pitch" '60 .pitch 61 .pitch 127 .pitch' "C4 C#4 G9"
check "parse-pitch" 's" Db4" parse-pitch . s" H4" parse-pitch .' "61 -1"
check "cents>bend" '0 cents>bend . 100 cents>bend . -200 cents>bend .' "8192 12288 0"
check "transpose" 'c4 7 transpose . c4 octave-up . c4 octave-down .' "67 72 48"

echo "Chords"
check "major" 'c4 major .s' "Stack<10> 60 64 67 3"
check "triads" 'c4 minor . . . . c4 dim . . . . c4 aug . . . .' "3 67 63 60 3 66 63 60 3 68 64 60"
check "sevenths" 'c4 dom7 . . . . . c4 maj7 . . . . . c4 min7 . . . . .' \
    "4 70 67 64 60 4 71 67 64 60 4 70 67 63 60"
check "extended" 'c4 dim7 . . . . . c4 half-dim7 . . . . . c4 sus2 . . . . c4 sus4 . . . .' \
    "4 69 66 63 60 4 70 66 63 60 3 67 62 60 3 67 65 60"
check "user chord" 'chord: power 0 , 7 , 12 , end-table  a2 power .s' "Stack<10> 45 52 57 3"

echo "Scales"
check "build-scale" 'c4 scale-major build-scale .s' "Stack<10> 60 62 64 65 67 69 71 7"
check "scale sizes" 'scale-pentatonic nip . scale-blues nip . scale-chromatic nip .' "5 6 12"
check "scale-degree" 'c4 scale-major 5 scale-degree . c4 scale-major 9 scale-degree .' "67 74"
check "in-scale?" 'e4 c4 scale-major in-scale? . eb4 c4 scale-major in-scale? .' "-1 0"
check "quantize" 'c#4 c4 scale-pentatonic quantize .' "60"
check "user scale" 'scale: tri 0 , 4 , 8 , end-table  c4 tri 2 scale-degree .' "64"

echo "Timing"
check "durations at 120" 'whole . half . quarter . eighth . sixteenth .' "2000 1000 500 250 125"
check "set-tempo" '60 set-tempo tempo . quarter . whole .' "60 1000 4000"
check "tempo range" '5 set-tempo tempo . 999 set-tempo tempo .' "20 300"
check "dotted, bpm" 'quarter dotted . 90 bpm .' "750 666"
check "dynamics" 'ppp . pp . p . mp . mf . f . ff . fff .' "16 33 49 64 80 96 112 127"
check "defaults" 'vel . dur . chan . 100 to vel vel .' "80 500 1 100"

echo "Ports"
contains "midi-list" 'midi-list' "^  [0-9]+: |none"
check "virtual open/close" 'midi-open? . midi-open midi-open? . midi-close midi-open? .' "0 -1 0"
contains "note without port aborts" 'midi-close c4 note' "no port open"

# Messages need a receiver: aseqdump provides one on ALSA
if [ -c /dev/snd/seq ] && command -v aseqdump > /dev/null; then
    stdbuf -oL aseqdump > "$TMP/dump.log" 2>&1 &
    DUMP_PID=$!
    for _ in $(seq 1 50); do
        aconnect -o 2>/dev/null | grep -q "'aseqdump'" && break
        sleep 0.1
    done
    IDX=$(run 'midi-list' | grep "aseqdump: aseqdump" | sed 's/^ *\([0-9]*\):.*/\1/')
    run "$IDX midi-open-port
        20 to dur
        c4 note
        c4 major chord
        d4 e4 2 arpeggio
        1 7 100 cc
        2 5 program
        1 16383 pitch-bend
        record-count .
        120 record-start g4 note record-stop .
        s\" $TMP/rec.mid\" write-mid
        midi-close" > "$TMP/play.out"
    sleep 0.3
    kill "$DUMP_PID"; wait "$DUMP_PID" 2>/dev/null; DUMP_PID=""
    events=$(grep -E "Note on|Note off|Control change +[0-9]+, controller (7|123)|Program|Pitch" "$TMP/dump.log" \
        | grep -v "controller 123" | sed -E 's/^[0-9:]+ +//; s/ +/ /g')
    expected="Note on 0, note 60, velocity 80
Note off 0, note 60, velocity 0
Note on 0, note 60, velocity 80
Note on 0, note 64, velocity 80
Note on 0, note 67, velocity 80
Note off 0, note 67, velocity 0
Note off 0, note 64, velocity 0
Note off 0, note 60, velocity 0
Note on 0, note 62, velocity 80
Note off 0, note 62, velocity 0
Note on 0, note 64, velocity 80
Note off 0, note 64, velocity 0
Control change 0, controller 7, value 100
Program change 1, program 5
Pitch bend 0, value 8191
Note on 0, note 67, velocity 80
Note off 0, note 67, velocity 0"
    if [ "$events" = "$expected" ]; then
        pass "note, chord, arpeggio, cc, program, pitch-bend reach the port"
    else
        fail "events differ:"; diff <(echo "$expected") <(echo "$events") | sed 's/^/    /'
    fi
    if grep -c "controller 123" "$TMP/dump.log" | grep -qx 16; then
        pass "midi-close sends all-notes-off on 16 channels"
    else
        fail "midi-close: expected 16 CC 123 messages"
    fi
    if grep -q "recording stopped. 2 events" "$TMP/play.out"; then
        pass "record-start/stop counts events"
    else
        fail "recording: $(cat "$TMP/play.out")"
    fi
    contains "write-mid then read-mid" "s\" $TMP/rec.mid\" read-mid" "ch= 1 note-on +67 +80"
else
    echo "  SKIP: messages (no ALSA sequencer or aseqdump)"
fi

echo "Examples"
EXAMPLES="$(cd "$(dirname "$0")/.." && pwd)/projects/pforth-midi/examples"
for f in "$EXAMPLES"/*.fs; do
    out=$(timeout 60 "$PFORTH_MIDI" "$f" 2>&1); rc=$?
    if [ $rc -eq 0 ] && [ -z "$out" ]; then
        pass "$(basename "$f")"
    else
        fail "$(basename "$f"): rc=$rc output='$out'"
    fi
done

if [ $FAIL -eq 0 ]; then echo "=== All pforth_midi tests passed ==="; fi
exit $FAIL
