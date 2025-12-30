#!/bin/bash
# Test suite for pktpy_midi

set -e

PKTPY=${1:-./build/pktpy_midi}

echo "Testing pktpy_midi..."

# Test 1: Module import
echo "Test 1: Module import..."
echo 'import midi; print("ok")' | $PKTPY | grep -q "ok" || { echo "FAIL: module import"; exit 1; }
echo "  PASS"

# Test 2: Note parsing
echo "Test 2: Note parsing..."
echo 'import midi; print(midi.note("C4"))' | $PKTPY | grep -q "60" || { echo "FAIL: C4 != 60"; exit 1; }
echo 'import midi; print(midi.note("C#4"))' | $PKTPY | grep -q "61" || { echo "FAIL: C#4 != 61"; exit 1; }
echo 'import midi; print(midi.note("Db4"))' | $PKTPY | grep -q "61" || { echo "FAIL: Db4 != 61"; exit 1; }
echo 'import midi; print(midi.note("A4"))' | $PKTPY | grep -q "69" || { echo "FAIL: A4 != 69"; exit 1; }
echo 'import midi; print(midi.note("C-1"))' | $PKTPY | grep -q "0" || { echo "FAIL: C-1 != 0"; exit 1; }
echo 'import midi; print(midi.note("G9"))' | $PKTPY | grep -q "127" || { echo "FAIL: G9 != 127"; exit 1; }
echo "  PASS"

# Test 3: list_ports returns a list
echo "Test 3: list_ports..."
echo 'import midi; ports = midi.list_ports(); print(type(ports).__name__)' | $PKTPY | grep -q "list" || { echo "FAIL: list_ports not returning list"; exit 1; }
echo "  PASS"

# Test 4: open creates MidiOut
echo "Test 4: open..."
echo 'import midi; m = midi.open(); print(type(m).__name__)' | $PKTPY | grep -q "MidiOut" || { echo "FAIL: open not returning MidiOut"; exit 1; }
echo 'import midi; m = midi.open("Test"); print(type(m).__name__)' | $PKTPY | grep -q "MidiOut" || { echo "FAIL: open(name) not returning MidiOut"; exit 1; }
echo "  PASS"

# Test 5: MidiOut.is_open property
echo "Test 5: is_open property..."
echo 'import midi; m = midi.open(); print(m.is_open)' | $PKTPY | grep -q "True" || { echo "FAIL: is_open should be True"; exit 1; }
echo 'import midi; m = midi.open(); m.close(); print(m.is_open)' | $PKTPY | grep -q "False" || { echo "FAIL: is_open should be False after close"; exit 1; }
echo "  PASS"

# Test 6: Context manager
echo "Test 6: Context manager..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
with midi.open() as m:
    print("opened:", m.is_open)
print("closed:", m.is_open)
EOF
$PKTPY "$TMPFILE" | grep -q "opened: True" || { rm -f "$TMPFILE"; echo "FAIL: context manager open"; exit 1; }
$PKTPY "$TMPFILE" | grep -q "closed: False" || { rm -f "$TMPFILE"; echo "FAIL: context manager close"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 7: note() method (high-level)
echo "Test 7: note() method..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.note("C4")              # string pitch, defaults
m.note(62)                # int pitch, defaults
m.note("E4", 100)         # with velocity
m.note("G4", 80, 100)     # with velocity and duration
m.note(72, 80, 100, 2)    # with channel
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: note() method"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 8: chord() method
echo "Test 8: chord() method..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.chord(["C4", "E4", "G4"])           # string pitches
m.chord([60, 64, 67])                  # int pitches
m.chord(("C4", "E4", "G4"), 100)       # tuple, with velocity
m.chord(["C4", "E4"], 80, 100)         # with duration
m.chord(["C4", "E4"], 80, 100, 2)      # with channel
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: chord() method"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 9: Low-level note_on/note_off
echo "Test 9: note_on/note_off..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.note_on(60)
m.note_on(61, 100)
m.note_on(62, 80, 2)
m.note_off(60)
m.note_off(61, 64)
m.note_off(62, 0, 2)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: note_on/note_off"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 10: CC and program_change
echo "Test 10: CC and program_change..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.cc(1, 64)
m.cc(7, 100, 1)
m.program_change(0)
m.program_change(42, 2)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: cc/program_change"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 11: all_notes_off
echo "Test 11: all_notes_off..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.all_notes_off()
m.all_notes_off(1)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: all_notes_off"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 12: repr
echo "Test 12: repr..."
echo 'import midi; m = midi.open(); print(repr(m))' | $PKTPY | grep -q "MidiOut virtual" || { echo "FAIL: repr virtual"; exit 1; }
echo 'import midi; m = midi.open(); m.close(); print(repr(m))' | $PKTPY | grep -q "MidiOut closed" || { echo "FAIL: repr closed"; exit 1; }
echo "  PASS"

# Test 13: Error handling - invalid note
echo "Test 13: Error handling..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
try:
    midi.note("invalid")
    print("FAIL")
except ValueError:
    print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: invalid note should raise ValueError"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 14: Error handling - closed port
echo "Test 14: Error handling - closed port..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.close()
try:
    m.note_on(60)
    print("FAIL")
except RuntimeError:
    print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: closed port should raise RuntimeError"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 15: Dynamics constants
echo "Test 15: Dynamics constants..."
echo 'import midi; print(midi.ppp, midi.pp, midi.p, midi.mp, midi.mf, midi.f, midi.ff, midi.fff)' | $PKTPY | grep -q "16 33 49 64 80 96 112 127" || { echo "FAIL: dynamics constants"; exit 1; }
echo "  PASS"

# Test 16: Duration constants
echo "Test 16: Duration constants..."
echo 'import midi; print(midi.whole, midi.half, midi.quarter, midi.eighth, midi.sixteenth)' | $PKTPY | grep -q "2000 1000 500 250 125" || { echo "FAIL: duration constants"; exit 1; }
echo 'import midi; print(midi.dotted(midi.quarter))' | $PKTPY | grep -q "750" || { echo "FAIL: dotted duration"; exit 1; }
echo "  PASS"

# Test 17: Pitch constants
echo "Test 17: Pitch constants..."
echo 'import midi; print(midi.c4, midi.cs4, midi.d4, midi.a4)' | $PKTPY | grep -q "60 61 62 69" || { echo "FAIL: pitch constants"; exit 1; }
echo "  PASS"

# Test 18: Chord builders
echo "Test 18: Chord builders..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
assert midi.major("C4") == [60, 64, 67], "major"
assert midi.minor("C4") == [60, 63, 67], "minor"
assert midi.dim("C4") == [60, 63, 66], "dim"
assert midi.aug("C4") == [60, 64, 68], "aug"
assert midi.dom7("C4") == [60, 64, 67, 70], "dom7"
assert midi.maj7("C4") == [60, 64, 67, 71], "maj7"
assert midi.min7("C4") == [60, 63, 67, 70], "min7"
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: chord builders"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 19: Transpose helpers
echo "Test 19: Transpose helpers..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
assert midi.transpose("C4", 2) == 62, "transpose up"
assert midi.transpose(60, -2) == 58, "transpose down"
assert midi.octave_up("C4") == 72, "octave up"
assert midi.octave_down(60) == 48, "octave down"
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: transpose helpers"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 20: Tempo/BPM
echo "Test 20: Tempo/BPM..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
assert midi.get_tempo() == 120, "default tempo"
assert midi.bpm(120) == 500, "bpm(120)"
assert midi.bpm(60) == 1000, "bpm(60)"
midi.set_tempo(60)
assert midi.quarter == 1000, "quarter at 60 BPM"
assert midi.eighth == 500, "eighth at 60 BPM"
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: tempo/BPM"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 21: Arpeggio
echo "Test 21: Arpeggio..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.arpeggio(midi.major("C4"), velocity=80, note_duration=50)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: arpeggio"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 22: CC helpers
echo "Test 22: CC helpers..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
m = midi.open()
m.modulation(64)
m.volume(100)
m.pan(64)
m.sustain(True)
m.sustain(False)
m.close()
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: CC helpers"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# Test 23: Musical example (integration)
echo "Test 23: Musical example..."
TMPFILE=$(mktemp)
cat > "$TMPFILE" << 'EOF'
import midi
midi.set_tempo(240)  # Fast for testing
with midi.open() as m:
    m.chord(midi.major("C4"), midi.mf, midi.quarter)
    m.chord(midi.minor("A3"), midi.mp, midi.quarter)
    m.arpeggio(midi.dom7("G3"), midi.f, midi.sixteenth)
print("ok")
EOF
$PKTPY "$TMPFILE" | grep -q "ok" || { rm -f "$TMPFILE"; echo "FAIL: musical example"; exit 1; }
rm -f "$TMPFILE"
echo "  PASS"

# ============================================================================
# MIDI File I/O Tests
# ============================================================================

# Test 24: write_mid function exists
echo "Test 24: write_mid function exists..."
echo 'import midi; print(callable(midi.write_mid))' | $PKTPY | grep -q "True" || { echo "FAIL: write_mid not callable"; exit 1; }
echo "  PASS"

# Test 25: read_mid function exists
echo "Test 25: read_mid function exists..."
echo 'import midi; print(callable(midi.read_mid))' | $PKTPY | grep -q "True" || { echo "FAIL: read_mid not callable"; exit 1; }
echo "  PASS"

# Test 26: Write and read MIDI file round-trip
echo "Test 26: MIDI file round-trip..."
MIDFILE=$(mktemp /tmp/test_midi_XXXXXX.mid)
TMPFILE=$(mktemp)
cat > "$TMPFILE" << EOF
import midi
midi.record_midi(120)
m = midi.open()
m.note_on(60, 80)
import time
time.sleep(0.01)
m.note_off(60)
m.note_on(64, 90)
time.sleep(0.01)
m.note_off(64)
m.close()
midi.record_stop()
midi.write_mid("$MIDFILE")

data = midi.read_mid("$MIDFILE")
if not data:
    print("FAIL:no_data")
elif "events" not in data:
    print("FAIL:no_events")
elif len(data["events"]) < 4:
    print("FAIL:event_count:" + str(len(data["events"])))
else:
    print("ok")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE" "$MIDFILE"; echo "FAIL: MIDI file round-trip failed"; exit 1; }
rm -f "$TMPFILE" "$MIDFILE"
echo "  PASS"

# Test 27: read_mid returns correct structure
echo "Test 27: read_mid structure..."
MIDFILE=$(mktemp /tmp/test_midi_XXXXXX.mid)
TMPFILE=$(mktemp)
cat > "$TMPFILE" << EOF
import midi
midi.record_midi(120)
m = midi.open()
m.note_on(60, 80)
import time
time.sleep(0.01)
m.note_off(60)
m.close()
midi.record_stop()
midi.write_mid("$MIDFILE")

data = midi.read_mid("$MIDFILE")
ok = True
if "num_tracks" not in data:
    ok = False
    print("FAIL:num_tracks")
if "ppqn" not in data:
    ok = False
    print("FAIL:ppqn")
if "tempo" not in data:
    ok = False
    print("FAIL:tempo")
if "events" not in data:
    ok = False
    print("FAIL:events")
if ok:
    print("ok")
EOF
$PKTPY "$TMPFILE" 2>&1 | grep -q "ok" || { rm -f "$TMPFILE" "$MIDFILE"; echo "FAIL: read_mid structure check failed"; exit 1; }
rm -f "$TMPFILE" "$MIDFILE"
echo "  PASS"

echo ""
echo "All tests passed!"
