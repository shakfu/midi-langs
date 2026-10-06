# API Reference

## Setup

Examples import the module and use `m` for an open port:

```python setup
import midi
m = midi.open()
```

## Module Functions

### midi.open

```python norun
midi.open() -> MidiOut
midi.open(name: str) -> MidiOut
midi.open(index: int) -> MidiOut
```

Open a MIDI output port.

- `open()` - Create virtual port named "pktpyMIDI"

- `open("MyApp")` - Create virtual port with custom name

- `open(0)` - Open hardware port by index

```python norun
m = midi.open()                  # Virtual port
m = midi.open("MySynth")         # Named virtual port
m = midi.open(0)                 # First hardware port
```

### midi.list_ports

```python norun
midi.list_ports() -> list[tuple[int, str]]
```

List available MIDI output ports. Returns list of (index, name) tuples.

```python
for idx, name in midi.list_ports():
    print(f"{idx}: {name}")
```

### midi.note

```python norun
midi.note(name: str) -> int
```

Parse note name to MIDI number.

- Supports: C, D, E, F, G, A, B

- Accidentals: # or s (sharp), b (flat)

- Octaves: -1 to 9

```python
midi.note("C4")   # 60
midi.note("C#4")  # 61
midi.note("Db4")  # 61
midi.note("A4")   # 69
```

---

## MidiOut Class

The `MidiOut` class represents an open MIDI output port.

### Context Manager

```python
with midi.open() as m:
    m.note("C4")
# Automatically closes and sends all-notes-off
```

### MidiOut.note

```python norun
m.note(pitch, velocity=80, duration=500, channel=1)
```

Play a single note.

- `pitch` - MIDI number (0-127) or note name string

- `velocity` - Note velocity (0-127), default 80

- `duration` - Duration in milliseconds, default 500

- `channel` - MIDI channel (1-16), default 1

```python
m.note("C4")                    # Defaults
m.note(60)                      # By MIDI number
m.note("E4", 100)               # With velocity
m.note("G4", 80, 250)           # With duration
m.note("C5", 80, 500, 2)        # On channel 2
```

### MidiOut.chord

```python norun
m.chord(pitches, velocity=80, duration=500, channel=1)
```

Play multiple notes simultaneously.

- `pitches` - List or tuple of MIDI numbers or note names

- `velocity` - Note velocity (0-127)

- `duration` - Duration in milliseconds

- `channel` - MIDI channel (1-16)

```python
m.chord(["C4", "E4", "G4"])                # C major
m.chord([60, 64, 67])                       # By MIDI numbers
m.chord(midi.major("C4"), midi.mf, midi.half)  # With helpers
```

### MidiOut.arpeggio

```python norun
m.arpeggio(pitches, velocity=80, note_duration=None, spacing=None, channel=1)
```

Play notes sequentially (arpeggiated).

- `pitches` - List of pitches to arpeggiate

- `velocity` - Note velocity (0-127)

- `note_duration` - Duration of each note (default: eighth)

- `spacing` - Time between note starts (default: same as duration)

- `channel` - MIDI channel (1-16)

```python
m.arpeggio(midi.major("C4"))
m.arpeggio(midi.min7("A3"), velocity=80, note_duration=100)
```

### MidiOut.note_on / note_off

```python norun
m.note_on(pitch, velocity=80, channel=1)
m.note_off(pitch, velocity=0, channel=1)
```

Low-level note control. Use `note()` for most cases.

```python
m.note_on(60)
midi.sleep(500)
m.note_off(60)
```

### MidiOut.cc

```python norun
m.cc(control, value, channel=1)
```

Send Control Change message.

- `control` - CC number (0-127)

- `value` - CC value (0-127)

- `channel` - MIDI channel (1-16)

```python
m.cc(1, 64)      # Modulation wheel to middle
m.cc(7, 100)     # Volume
m.cc(64, 127)    # Sustain pedal on
```

### MidiOut.program_change

```python norun
m.program_change(program, channel=1)
```

Send Program Change message.

- `program` - Program number (0-127)

- `channel` - MIDI channel (1-16)

```python
m.program_change(0)     # Piano
m.program_change(25)    # Acoustic guitar
```

### MidiOut.all_notes_off

```python norun
m.all_notes_off(channel=None)
```

Send All Notes Off. If channel is None, sends on all channels.

```python
m.all_notes_off()     # All channels
m.all_notes_off(1)    # Channel 1 only
```

### MidiOut.close

```python
m.close()
```

Close the MIDI port. Sends All Notes Off on all channels before closing to prevent hanging notes.

Note: All Notes Off is also sent automatically when a MidiOut object is garbage collected, so notes won't hang even if you forget to call close().

### MidiOut.is_open

```python norun
m.is_open -> bool
```

Property indicating whether the port is open.

```python
m = midi.open()
print(m.is_open)  # True
m.close()
print(m.is_open)  # False
```

### CC Helpers

Convenience methods for common control changes:

```python norun
m.modulation(value, channel=1)   # CC 1 - Modulation wheel
m.volume(value, channel=1)       # CC 7 - Channel volume
m.pan(value, channel=1)          # CC 10 - Pan (0=left, 64=center, 127=right)
m.sustain(on=True, channel=1)    # CC 64 - Sustain pedal
```

---

## Pitch Constants

All pitches follow the pattern `<note><octave>`:

- Notes: c, d, e, f, g, a, b

- Sharps: cs, ds, fs, gs, as

- Flats: db, eb, gb, ab, bb

- Octaves: 0-8

Middle C (MIDI 60) is `c4`.

```python
midi.c4   # 60
midi.cs4  # 61 (C sharp)
midi.db4  # 61 (D flat)
midi.a4   # 69
midi.c5   # 72
```

| Pitch | MIDI | Pitch | MIDI | Pitch | MIDI |
| ------- | ------ | ------- | ------ | ------- | ------ |
| c4    | 60   | d4    | 62   | e4    | 64   |
| f4    | 65   | g4    | 67   | a4    | 69   |
| b4    | 71   | c5    | 72   | cs4   | 61   |

---

## Duration Constants

Based on 120 BPM by default. Use `midi.set_tempo()` to change.

| Constant          | Milliseconds | Musical Value |
| ------------------- | -------------- | --------------- |
| `midi.whole`      | 2000         | Whole note    |
| `midi.half`       | 1000         | Half note     |
| `midi.quarter`    | 500          | Quarter note  |
| `midi.eighth`     | 250          | Eighth note   |
| `midi.sixteenth`  | 125          | 16th note     |

### midi.dotted

```python norun
midi.dotted(duration) -> int
```

Returns 1.5x the given duration.

```python
midi.dotted(midi.quarter)  # 750
midi.dotted(midi.half)     # 1500
```

---

## Tempo

### midi.set_tempo

```python norun
midi.set_tempo(bpm: int)
```

Set tempo in BPM. This affects:

- Duration constants (`midi.quarter`, `midi.half`, etc.)

- All durations passed to `MidiOut.note()` and `MidiOut.chord()`

Durations are scaled relative to 120 BPM (the default). At 60 BPM, all durations double; at 240 BPM, they halve.

```python
midi.set_tempo(120)  # 120 BPM (default) - no scaling
midi.set_tempo(60)   # 60 BPM - all durations double
print(midi.quarter)  # 1000 at 60 BPM

# Even raw millisecond values are scaled:
m.note(60, 80, 500)    # At 60 BPM, plays for 1000ms (500 * 120/60)
```

### midi.get_tempo

```python norun
midi.get_tempo() -> int
```

Get current tempo in BPM.

```python
print(midi.get_tempo())  # 120
```

### midi.bpm

```python norun
midi.bpm(tempo: int) -> int
```

Calculate quarter note duration for a given tempo.

```python
midi.bpm(120)  # 500
midi.bpm(60)   # 1000
```

---

## Velocities (Dynamics)

| Constant   | Value | Dynamic        |
| ------------ | ------- | ---------------- |
| `midi.ppp` | 16    | Pianississimo  |
| `midi.pp`  | 33    | Pianissimo     |
| `midi.p`   | 49    | Piano          |
| `midi.mp`  | 64    | Mezzo-piano    |
| `midi.mf`  | 80    | Mezzo-forte    |
| `midi.f`   | 96    | Forte          |
| `midi.ff`  | 112   | Fortissimo     |
| `midi.fff` | 127   | Fortississimo  |

---

## Chord Builders

All chord builders accept a root pitch (int or str) and return a list of MIDI numbers.

### midi.major

```python norun
midi.major(root) -> list[int]
```

Build major triad: root, major 3rd, perfect 5th.

```python
midi.major("C4")  # [60, 64, 67]
midi.major(60)    # [60, 64, 67]
```

### midi.minor

```python norun
midi.minor(root) -> list[int]
```

Build minor triad: root, minor 3rd, perfect 5th.

```python
midi.minor("C4")  # [60, 63, 67]
```

### midi.dim

```python norun
midi.dim(root) -> list[int]
```

Build diminished triad: root, minor 3rd, diminished 5th.

```python
midi.dim("C4")  # [60, 63, 66]
```

### midi.aug

```python norun
midi.aug(root) -> list[int]
```

Build augmented triad: root, major 3rd, augmented 5th.

```python
midi.aug("C4")  # [60, 64, 68]
```

### midi.dom7

```python norun
midi.dom7(root) -> list[int]
```

Build dominant 7th chord.

```python
midi.dom7("C4")  # [60, 64, 67, 70]
```

### midi.maj7

```python norun
midi.maj7(root) -> list[int]
```

Build major 7th chord.

```python
midi.maj7("C4")  # [60, 64, 67, 71]
```

### midi.min7

```python norun
midi.min7(root) -> list[int]
```

Build minor 7th chord.

```python
midi.min7("C4")  # [60, 63, 67, 70]
```

---

## Scale Functions

### midi.scale

```python norun
midi.scale(root, name) -> list[int]
```

Build a scale from root pitch and scale name.

- `root` - Root pitch (int or str)

- `name` - Scale name (string)

```python
midi.scale(60, "major")      # [60, 62, 64, 65, 67, 69, 71]
midi.scale("C4", "minor")    # [60, 62, 63, 65, 67, 68, 70]
midi.scale("D4", "dorian")   # Dorian mode starting on D
```

### midi.degree

```python norun
midi.degree(root, name, n) -> int
```

Get the nth degree of a named scale (1-based).

- `root` - Root pitch (int or str)

- `name` - Scale name (string)

- `n` - Scale degree (1 = root, 2 = second, etc.)

Supports degrees beyond the octave (e.g., 9 = 2nd + octave).

```python
midi.degree(60, "major", 1)   # 60 (root)
midi.degree(60, "major", 3)   # 64 (major third)
midi.degree(60, "major", 5)   # 67 (perfect fifth)
midi.degree(60, "major", 8)   # 72 (octave)
midi.degree(60, "major", 9)   # 74 (ninth = octave + 2nd)
```

### midi.in_scale_named

```python norun
midi.in_scale_named(pitch, root, name) -> bool
```

Check if a pitch belongs to a named scale (in any octave).

```python
midi.in_scale_named(64, 60, "major")  # True  (E is in C major)
midi.in_scale_named(61, 60, "major")  # False (C# is not in C major)
```

### midi.quantize

```python norun
midi.quantize(pitch, root, name) -> int
```

Quantize a pitch to the nearest note in a named scale.

```python
midi.quantize(63, 60, "major")  # 62 (D#/Eb -> D)
midi.quantize(66, 60, "major")  # 67 (F#/Gb -> G)
```

### Low-Level Scale Functions

For direct control with interval tuples:

```python norun
midi.build_scale(root, intervals) -> list[int]
midi.scale_degree(root, intervals, degree) -> int
midi.in_scale(pitch, root, intervals) -> bool
midi.quantize_to_scale(pitch, root, intervals) -> int
```

```python
major = (0, 2, 4, 5, 7, 9, 11)
midi.build_scale(60, major)           # [60, 62, 64, 65, 67, 69, 71]
midi.scale_degree(60, major, 5)       # 67
midi.in_scale(64, 60, major)          # True
midi.quantize_to_scale(63, 60, major) # 62
```

---

## Scale Constants

All scales are available as tuples of semitone intervals from the root.

### Diatonic Modes

| Constant | Intervals |
| ---------- | ----------- |
| `midi.SCALE_MAJOR` | (0, 2, 4, 5, 7, 9, 11) |
| `midi.SCALE_MINOR` | (0, 2, 3, 5, 7, 8, 10) |
| `midi.SCALE_DORIAN` | (0, 2, 3, 5, 7, 9, 10) |
| `midi.SCALE_PHRYGIAN` | (0, 1, 3, 5, 7, 8, 10) |
| `midi.SCALE_LYDIAN` | (0, 2, 4, 6, 7, 9, 11) |
| `midi.SCALE_MIXOLYDIAN` | (0, 2, 4, 5, 7, 9, 10) |
| `midi.SCALE_LOCRIAN` | (0, 1, 3, 5, 6, 8, 10) |
| `midi.SCALE_IONIAN` | Same as major |
| `midi.SCALE_AEOLIAN` | Same as natural minor |

### Pentatonic & Blues

| Constant | Intervals |
| ---------- | ----------- |
| `midi.SCALE_PENTATONIC` | (0, 2, 4, 7, 9) |
| `midi.SCALE_PENTATONIC_MAJOR` | (0, 2, 4, 7, 9) |
| `midi.SCALE_PENTATONIC_MINOR` | (0, 3, 5, 7, 10) |
| `midi.SCALE_BLUES` | (0, 3, 5, 6, 7, 10) |
| `midi.SCALE_BLUES_MAJOR` | (0, 2, 3, 4, 7, 9) |

### Harmonic & Melodic

| Constant | Intervals |
| ---------- | ----------- |
| `midi.SCALE_HARMONIC_MINOR` | (0, 2, 3, 5, 7, 8, 11) |
| `midi.SCALE_MELODIC_MINOR` | (0, 2, 3, 5, 7, 9, 11) |
| `midi.SCALE_HARMONIC_MAJOR` | (0, 2, 4, 5, 7, 8, 11) |

### World Scales

| Constant | Description |
| ---------- | ------------- |
| `midi.SCALE_MAQAM_HIJAZ` | Arabic Hijaz (12-TET) |
| `midi.SCALE_MAQAM_NAHAWAND` | Arabic Nahawand |
| `midi.SCALE_RAGA_BHAIRAV` | Indian Bhairav |
| `midi.SCALE_JAPANESE` | Japanese In scale |
| `midi.SCALE_GYPSY` | Hungarian Gypsy |

### Other Scales

| Constant | Description |
| ---------- | ------------- |
| `midi.SCALE_CHROMATIC` | All 12 semitones |
| `midi.SCALE_WHOLE_TONE` | Whole-tone scale |
| `midi.SCALE_DIMINISHED` | Whole-half diminished |
| `midi.SCALE_AUGMENTED` | Augmented scale |
| `midi.SCALE_BEBOP_DOMINANT` | Bebop dominant |

### Scale Lookup Dictionary

```python
midi.scales["major"]      # Returns (0, 2, 4, 5, 7, 9, 11)
midi.scales["pentatonic"] # Returns (0, 2, 4, 7, 9)
```

---

## Microtonal

### MidiOut.pitch_bend

```python norun
m.pitch_bend(cents, channel=1)
```

Set pitch bend in cents (-200 to +200 for semitone range).

- `cents` - Pitch offset in cents

- `channel` - MIDI channel (1-16)

```python
m.pitch_bend(50)    # Bend up quarter-tone
m.pitch_bend(-50)   # Bend down quarter-tone
m.pitch_bend(0)     # Reset to center
```

### midi.cents_to_note

```python norun
midi.cents_to_note(root, cents) -> tuple[int, int]
```

Convert a cents interval to note and pitch bend values.

- `root` - Root pitch (MIDI number)

- `cents` - Interval in cents from root

Returns tuple of (midi_note, bend_cents).

```python
midi.cents_to_note(60, 150)   # (61, 50)  - 150 cents = C# minus 50 cents
midi.cents_to_note(60, 350)   # (63, 50)  - 350 cents = Eb plus 50 cents
```

### Microtonal Scale Constants

For scales with quarter-tones and other microtonal intervals:

| Constant | Description |
| ---------- | ------------- |
| `midi.SCALE_MAQAM_BAYATI_CENTS` | Authentic Bayati with 3/4 tones |
| `midi.SCALE_MAQAM_RAST_CENTS` | Authentic Rast with 3/4 tones |
| `midi.SCALE_MAQAM_SABA_CENTS` | Authentic Saba |
| `midi.SCALE_MAQAM_SIKAH_CENTS` | Authentic Sikah |
| `midi.SCALE_MAQAM_HIJAZ_CENTS` | Authentic Hijaz with microtones |
| `midi.SCALE_RAGA_TODI_CENTS` | Raga Todi with shruti |
| `midi.SCALE_SLENDRO_CENTS` | Javanese Slendro |
| `midi.SCALE_PELOG_CENTS` | Javanese Pelog |

```python
midi.SCALE_MAQAM_BAYATI_CENTS  # (0, 150, 300, 500, 700, 800, 1000)
midi.scales_cents["maqam_bayati"]  # Same via lookup
```

### Microtonal Playback Example

```python
import midi

with midi.open() as m:
    root = 60
    for cents in midi.SCALE_MAQAM_BAYATI_CENTS:
        note, bend = midi.cents_to_note(root, cents)
        m.pitch_bend(bend)
        m.note(note, midi.mf, midi.quarter)
    m.pitch_bend(0)  # Reset
```

---

## Pitch Helpers

### midi.transpose

```python norun
midi.transpose(pitch, semitones) -> int
```

Transpose pitch by semitones.

```python
midi.transpose("C4", 2)   # 62 (D4)
midi.transpose(60, -12)   # 48 (C3)
```

### midi.octave_up

```python norun
midi.octave_up(pitch) -> int
```

Transpose up one octave (+12 semitones).

```python
midi.octave_up("C4")  # 72 (C5)
```

### midi.octave_down

```python norun
midi.octave_down(pitch) -> int
```

Transpose down one octave (-12 semitones).

```python
midi.octave_down(60)  # 48 (C3)
```

---

## Timing

### midi.sleep

```python norun
midi.sleep(ms: int)
```

Sleep for given milliseconds.

```python
midi.sleep(500)  # Wait half second
```

### midi.rest

```python
midi.rest(duration=None)
```

Rest (silence) for given duration. Default is quarter note.

```python
midi.rest()              # Quarter note rest
midi.rest(midi.half)     # Half note rest
```

---

## CC Constants

Common Control Change numbers:

| Constant             | Value | Description      |
| --------------------- | ------- | ------------------ |
| `midi.CC_MODULATION` | 1     | Modulation wheel |
| `midi.CC_BREATH`     | 2     | Breath controller|
| `midi.CC_VOLUME`     | 7     | Channel volume   |
| `midi.CC_PAN`        | 10    | Pan position     |
| `midi.CC_EXPRESSION` | 11    | Expression       |
| `midi.CC_SUSTAIN`    | 64    | Sustain pedal    |
| `midi.CC_REVERB`     | 91    | Reverb send      |
| `midi.CC_CHORUS`     | 93    | Chorus send      |

---

## MIDI Recording

Record MIDI events for replay or export. Records note-on, note-off, and CC events with timestamps.

### midi.record_midi

```python norun
midi.record_midi(bpm=120)
```

Start recording MIDI events. Optional BPM parameter is stored with the recording for reference.

```python
midi.record_midi()       # Start at 120 BPM
midi.record_midi(140)    # Start at 140 BPM
```

### midi.record_stop

```python
midi.record_stop()
```

Stop recording MIDI events. Prints the number of events recorded.

```python
midi.record_stop()
# => MIDI recording stopped. 42 events recorded.
```

### midi.save_midi

```python norun
midi.save_midi(filename: str)
```

Save recorded events to a Python replay script. The generated file can be executed to replay the recorded performance.

```python
midi.save_midi("my_song.py")
# => Saved 42 events to my_song.py
```

The generated file contains:

- Event data as a Python list of tuples

- Replay code that recreates the timing

### midi.record_status

```python norun
midi.record_status() -> tuple[bool, int, int]
```

Get current recording status. Returns a tuple of (active, count, bpm).

```python
active, count, bpm = midi.record_status()
print(active)   # True/False
print(count)    # number of events
print(bpm)      # recording BPM
```

### Recording Example

```python
import midi

with midi.open() as m:
    midi.record_midi(120)

    m.note("C4", midi.mf, midi.quarter)
    m.note("E4", midi.mf, midi.quarter)
    m.chord(midi.major("G4"), midi.f, midi.half)

    midi.record_stop()
    midi.save_midi("melody.py")
```

---

## MIDI File I/O

Read and write standard MIDI files (.mid format).

### midi.write_mid

```python norun
midi.write_mid(filename)
```

Write recorded events to a standard MIDI file. The file can be opened in any DAW or MIDI player.

```python
import midi

with midi.open() as m:
    midi.record_midi(120)

    m.note("C4", midi.mf, midi.quarter)
    m.note("E4", midi.mf, midi.quarter)
    m.chord(midi.major("G4"), midi.f, midi.half)

    midi.record_stop()
    midi.write_mid("melody.mid")   # Standard MIDI file
```

### midi.read_mid

```python norun
midi.read_mid(filename) -> dict
```

Read a standard MIDI file and return its contents as a dictionary with metadata and events.

```python
midi.record_midi()
m.note(60)
midi.record_stop()
midi.write_mid("song.mid")

data = midi.read_mid("song.mid")

# Metadata
print(data["num_tracks"])  # Number of tracks
print(data["ppqn"])        # Pulses per quarter note
print(data["tempo"])       # Tempo in microseconds per beat
print(data["duration"])    # Duration in milliseconds
print(data["format"])      # MIDI format (0, 1, or 2)

# Events - list of tuples (track, tick, channel, type, data1, data2)
for event in data["events"]:
    track, tick, channel, event_type, data1, data2 = event
    print(f"t={tick} ch={channel} type={hex(event_type)} d1={data1} d2={data2}")
```

Event types (in the `type` field):

- `0x90` = Note On

- `0x80` = Note Off

- `0xB0` = Control Change

- `0xC0` = Program Change

- `0xE0` = Pitch Bend

---

## Generative Functions

Pure functions on pitch lists. Random functions take an explicit `seed`, so the same seed gives the same result. Functions that return `next_seed` let you chain calls.

| Function | Returns | Description |
|-|-|-|
| `midi.next_random(seed)` | `(value, next_seed)` | Linear congruential generator (glibc constants) |
| `midi.random_range(seed, lo, hi)` | `(value, next_seed)` | Integer in `[lo, hi]` |
| `midi.random_list(seed, n, lo, hi)` | `(list, next_seed)` | `n` integers in `[lo, hi]` |
| `midi.chance(seed, probability)` | `(bool, next_seed)` | `True` with `probability` percent |
| `midi.euclidean(hits, steps)` | `list` | Bjorklund rhythm: `True` = hit, `False` = rest |
| `midi.arp_up(lst)` | `list` | Copy in order |
| `midi.arp_down(lst)` | `list` | Reversed copy |
| `midi.arp_up_down(lst)` | `list` | Up, then down without repeating the ends |
| `midi.retrograde(lst)` | `list` | Reversed copy |
| `midi.invert(lst, axis)` | `list` | Mirror each pitch around `axis` |
| `midi.shuffle(seed, lst)` | `list` | Fisher-Yates shuffle |
| `midi.pick(seed, lst)` | `value` | One element, or `None` for an empty list |
| `midi.pick_n(seed, n, lst)` | `list` | `n` elements, with replacement |
| `midi.random_walk(seed, start, max_step, n)` | `list` | `n` pitches, each within `max_step` semitones of the last |
| `midi.drunk_walk(seed, start, scale_pitches, max_degrees, n)` | `list` | Random walk over `scale_pitches`, moving at most `max_degrees` |
| `midi.weighted_pick(seed, weights)` | `value` | `weights` is a list of `(value, weight)` tuples |

```python
rhythm = midi.euclidean(3, 8)          # [True, False, False, True, False, False, True, False]
melody = midi.shuffle(42, midi.scale(60, "pentatonic"))
walk = midi.random_walk(7, 60, 2, 8)
hit, seed = midi.chance(1, 75)
for i, on in enumerate(rhythm):
    if on:
        m.note(melody[i % len(melody)], midi.mf, midi.sixteenth)
```

---

## Async Scheduler

The async scheduler enables non-blocking multi-voice playback using Python generators and libuv timers. Multiple "voices" can play concurrently with independent timing.

### midi.spawn

```python norun
midi.spawn(func, name=None) -> int
```

Spawn a new voice from a generator function. Returns the voice ID.

- `func` - A generator function (must use `yield`)

- `name` - Optional name for debugging

The function is called immediately to create a generator. The generator should yield millisecond wait times.

```python
def my_voice():
    out = midi.open()
    out.note_on(60, 80, 1)
    yield 500  # Wait 500ms
    out.note_off(60, 0, 1)
    out.close()

voice_id = midi.spawn(my_voice, "Voice1")
```

### midi.run

```python
midi.run()
```

Run the scheduler until all voices complete. This is a blocking call.

```python
def voice_a():
    yield 100

def voice_b():
    yield 200

midi.spawn(voice_a, "A")
midi.spawn(voice_b, "B")
midi.run()  # Blocks until both A and B finish
print("All voices done")
```

### midi.poll

```python norun
midi.poll() -> bool
```

Run the voices that are ready, without blocking. Returns `True` while voices remain, so a loop can do other work between calls.

```python
def voice():
    yield 10

midi.spawn(voice)
while midi.poll():
    pass
```

### midi.stop

```python norun
midi.stop() -> None
midi.stop(voice_id) -> bool
```

Stop voices.

- `stop()` - Stop all voices

- `stop(voice_id)` - Stop specific voice, returns True if found

```python
midi.stop()      # Stop all
midi.stop(1)     # Stop voice with ID 1
```

### midi.voices

```python norun
midi.voices() -> int
```

Return count of active voices.

```python
def voice_a():
    yield 100

def voice_b():
    yield 200

midi.spawn(voice_a)
midi.spawn(voice_b)
print(midi.voices())  # 2
```

### midi.status

```python norun
midi.status() -> dict
```

Return scheduler status as a dictionary.

```python
s = midi.status()
print(s["running"])  # True/False - is run() active?
print(s["active"])   # int - number of active voices
```

---

## Async Playback Helpers

Generator functions for use inside spawned voices. These yield wait times automatically.

Use `yield from` to delegate to these generators:

### midi.play

```python norun
midi.play(out, pitch, velocity=None, duration=None, channel=1)
```

Play a note asynchronously (generator). Handles note_on, wait, note_off.

- `out` - MidiOut instance

- `pitch` - MIDI number or note name string

- `velocity` - Note velocity (default: mf)

- `duration` - Duration in ms (default: quarter)

- `channel` - MIDI channel (default: 1)

```python
def voice():
    out = midi.open()
    yield from midi.play(out, midi.c4, midi.mf, 500)  # Play C4 for 500ms
    yield from midi.play(out, midi.e4)                 # Play E4 for 250ms (default)
    out.close()
```

### midi.play_chord

```python norun
midi.play_chord(out, pitches, velocity=None, duration=None, channel=1)
```

Play a chord asynchronously (generator).

- `out` - MidiOut instance

- `pitches` - List of MIDI numbers or note names

- `velocity` - Note velocity (default: mf)

- `duration` - Duration in ms (default: quarter)

- `channel` - MIDI channel (default: 1)

```python
def voice():
    out = midi.open()
    yield from midi.play_chord(out, midi.major("C4"), midi.f, midi.half)
    out.close()
```

### midi.play_arp

```python norun
midi.play_arp(out, pitches, velocity=None, note_duration=None, spacing=None, channel=1)
```

Play notes as arpeggio asynchronously (generator).

- `out` - MidiOut instance

- `pitches` - List of pitches to arpeggiate

- `velocity` - Note velocity (default: mf)

- `note_duration` - Duration of each note (default: eighth)

- `spacing` - Time between note starts (default: same as duration)

- `channel` - MIDI channel (default: 1)

```python
def voice():
    out = midi.open()
    yield from midi.play_arp(out, midi.min7("A3"), midi.mp, midi.sixteenth)
    out.close()
```

### midi.wait

```python norun
midi.wait(ms)
```

Wait for milliseconds asynchronously (generator).

```python
def voice():
    out = midi.open()
    out.note_on(60, 80, 1)
    for ms in midi.wait(1000):  # Wait 1 second
        yield ms
    out.note_off(60, 0, 1)
    out.close()
```

### midi.ms / midi.yield_ms

```python norun
midi.ms(n) -> int
midi.yield_ms(n) -> int  # alias
```

Return milliseconds value for direct yielding in a voice. This provides cleaner syntax than `midi.wait()` when you just need a simple delay.

- `n` - Milliseconds to wait

```python
def voice():
    out = midi.open()
    out.note_on(midi.c4, midi.mf)
    yield midi.ms(500)              # Cleaner syntax!
    out.note_off(midi.c4)
    yield midi.yield_ms(250)        # Alias works too
    out.note_on(midi.e4, midi.mf)
    yield midi.ms(midi.quarter)     # Works with duration constants
    out.note_off(midi.e4)
    out.close()
```

**Comparison with midi.wait**:

```python
# Using midi.wait (requires for loop due to PocketPy limitation)
def with_wait():
    for ms in midi.wait(500):
        yield ms

# Using midi.ms (simpler, when you don't need the generator pattern)
def with_ms():
    yield midi.ms(500)
```

Use `midi.ms()` for simple delays. Use `midi.wait()` when you need the generator pattern for compatibility with other helper functions.

---

## Async Example

Complete example with multiple voices:

```python
import midi

def melody():
    out = midi.open()
    notes = [midi.c4, midi.e4, midi.g4, midi.c5]
    for note in notes:
        yield from midi.play(out, note, midi.mf, 200)
    out.close()

def bass():
    out = midi.open()
    yield from midi.play(out, midi.c2, midi.f, 400)
    yield from midi.play(out, midi.g2, midi.f, 400)
    out.close()

def drums():
    out = midi.open()
    for i in range(4):
        out.note_on(36, 100, 10)  # Kick on channel 10
        yield 100
        out.note_off(36, 0, 10)
        yield 100
    out.close()

# Spawn all voices
midi.spawn(melody, "Melody")
midi.spawn(bass, "Bass")
midi.spawn(drums, "Drums")

# Run until all complete
midi.run()
print("Done!")
```
