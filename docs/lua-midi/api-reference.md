# API Reference

## Port Management

### midi.list_ports

```lua
midi.list_ports() -> table
```

List available MIDI output ports. Returns table of `{index, name}` pairs.

```lua
midi.list_ports()
-- => {{0, "IAC Driver Bus 1"}, {1, "USB MIDI Device"}}
```

### midi.open

```lua
midi.open() -> MidiOut
midi.open(name) -> MidiOut
midi.open(index) -> MidiOut
```

Open a MIDI output port.

- `midi.open()` - Create virtual port named "luaMIDI"
- `midi.open("MyApp")` - Create virtual port with custom name
- `midi.open(0)` - Open hardware port by index

```lua
m = midi.open()                -- Virtual port
m = midi.open("MySynth")       -- Named virtual port
m = midi.open(0)               -- First hardware port
```

### m:close

```lua
m:close()
```

Close the MIDI port. Automatically sends all-notes-off on all channels.

```lua
m:close()
```

### m:is_open

```lua
m:is_open() -> boolean
```

Check if the MIDI port is open.

```lua
m = midi.open()
m:is_open()  -- => true
m:close()
m:is_open()  -- => false
```

---

## Note Playing

### m:note

```lua
m:note(pitch, [velocity], [duration], [channel])
```

Play a single note (blocking).

- `pitch` - MIDI number (0-127) or note name string
- `velocity` - Note velocity (0-127), default 80
- `duration` - Duration in milliseconds, default 500
- `channel` - MIDI channel (1-16), default 1

```lua
m:note(midi.c4)                     -- Defaults
m:note(60)                          -- By MIDI number
m:note("C4")                        -- By note name
m:note(midi.c4, midi.mf)            -- With velocity
m:note(midi.c4, midi.mf, midi.quarter)  -- With duration
m:note(midi.c4, 80, 500, 2)         -- On channel 2
```

### m:chord

```lua
m:chord(pitches, [velocity], [duration], [channel])
```

Play multiple notes simultaneously.

- `pitches` - Table of MIDI numbers or note names
- `velocity` - Note velocity (0-127), default 80
- `duration` - Duration in milliseconds, default 500
- `channel` - MIDI channel (1-16), default 1

```lua
m:chord({60, 64, 67})                         -- C major by numbers
m:chord({midi.c4, midi.e4, midi.g4})          -- C major by constants
m:chord(midi.major(midi.c4), midi.mf, midi.half)  -- Using chord builder
m:chord(midi.dom7(midi.g3), midi.f, midi.quarter, 2)  -- On channel 2
```

### m:arpeggio

```lua
m:arpeggio(pitches, [velocity], [duration], [channel])
```

Play notes sequentially (arpeggiated).

- `pitches` - Table of pitches to arpeggiate
- `velocity` - Note velocity (0-127), default 80
- `duration` - Duration of each note, default 250 (eighth)
- `channel` - MIDI channel (1-16), default 1

```lua
m:arpeggio(midi.major(midi.c4))                   -- Defaults
m:arpeggio(midi.min7(midi.a3), midi.mf, midi.sixteenth)  -- Fast arpeggio
```

### m:note_on

```lua
m:note_on(pitch, [velocity], [channel])
```

Send Note On message (non-blocking).

```lua
m:note_on(60)              -- Note on, default velocity
m:note_on(midi.c4, 100)    -- With velocity
m:note_on(midi.c4, 100, 2) -- On channel 2
```

### m:note_off

```lua
m:note_off(pitch, [velocity], [channel])
```

Send Note Off message.

```lua
m:note_off(60)
m:note_off(midi.c4, 64, 2)  -- With release velocity on channel 2
```

---

## Control Messages

### m:cc

```lua
m:cc(control, value, [channel])
```

Send Control Change message.

- `control` - CC number (0-127)
- `value` - CC value (0-127)
- `channel` - MIDI channel (1-16), default 1

```lua
m:cc(1, 64)         -- Modulation wheel to middle
m:cc(7, 100)        -- Volume
m:cc(64, 127)       -- Sustain pedal on
m:cc(64, 0)         -- Sustain pedal off
```

Common CC numbers:
- 1: Modulation wheel
- 7: Channel volume
- 10: Pan
- 11: Expression
- 64: Sustain pedal
- 91: Reverb
- 93: Chorus

### m:program

```lua
m:program(program, [channel])
```

Send Program Change message.

- `program` - Program number (0-127)
- `channel` - MIDI channel (1-16), default 1

```lua
m:program(0)       -- Piano
m:program(25)      -- Acoustic guitar
m:program(48, 2)   -- Strings on channel 2
```

### m:all_notes_off

```lua
m:all_notes_off([channel])
```

Send All Notes Off. If channel is omitted, sends on all channels (1-16).

```lua
m:all_notes_off()      -- All channels
m:all_notes_off(1)     -- Channel 1 only
```

---

## Pitch Helpers

### midi.note

```lua
midi.note(name) -> integer
```

Parse note name to MIDI number.

- Supports: C, D, E, F, G, A, B (case insensitive)
- Accidentals: # or s (sharp), b (flat)
- Octaves: -1 to 9

```lua
midi.note("C4")     -- => 60
midi.note("C#4")    -- => 61
midi.note("Db4")    -- => 61
midi.note("cs4")    -- => 61
```

### Pitch Constants

All pitches from C0 to B8 are available in the `midi` table:

```lua
midi.c0, midi.cs0, midi.d0, midi.ds0, midi.e0, midi.f0, midi.fs0, midi.g0, midi.gs0, midi.a0, midi.as0, midi.b0   -- Octave 0
midi.c1, midi.cs1, midi.d1, midi.ds1, midi.e1, midi.f1, midi.fs1, midi.g1, midi.gs1, midi.a1, midi.as1, midi.b1   -- Octave 1
-- ...
midi.c4, midi.cs4, midi.d4, midi.ds4, midi.e4, midi.f4, midi.fs4, midi.g4, midi.gs4, midi.a4, midi.as4, midi.b4   -- Octave 4 (middle C = c4 = 60)
-- ...
midi.c8, midi.cs8, midi.d8, midi.ds8, midi.e8, midi.f8, midi.fs8, midi.g8, midi.gs8, midi.a8, midi.as8, midi.b8   -- Octave 8
```

Flat aliases:

```lua
midi.db0, midi.eb0, midi.gb0, midi.ab0, midi.bb0   -- D-flat, E-flat, G-flat, A-flat, B-flat
-- ... through octave 8
```

### midi.transpose

```lua
midi.transpose(pitch, semitones) -> integer
```

Transpose pitch by semitones.

```lua
midi.transpose(midi.c4, 2)     -- => 62 (D4)
midi.transpose(60, -12)        -- => 48 (C3)
midi.transpose("C4", 7)        -- => 67 (G4)
```

### midi.octave_up

```lua
midi.octave_up(pitch) -> integer
```

Transpose up one octave (+12 semitones).

```lua
midi.octave_up(midi.c4)    -- => 72 (C5)
midi.octave_up(60)         -- => 72
```

### midi.octave_down

```lua
midi.octave_down(pitch) -> integer
```

Transpose down one octave (-12 semitones).

```lua
midi.octave_down(midi.c4)  -- => 48 (C3)
midi.octave_down(60)       -- => 48
```

---

## Chord Builders

All chord builders accept a root pitch (integer or string) and return a table of MIDI numbers.

### midi.major

```lua
midi.major(root) -> table
```

Build major triad: root, major 3rd, perfect 5th.

```lua
midi.major(midi.c4)    -- => {60, 64, 67}
midi.major(60)         -- => {60, 64, 67}
```

### midi.minor

```lua
midi.minor(root) -> table
```

Build minor triad: root, minor 3rd, perfect 5th.

```lua
midi.minor(midi.c4)    -- => {60, 63, 67}
```

### midi.dim

```lua
midi.dim(root) -> table
```

Build diminished triad: root, minor 3rd, diminished 5th.

```lua
midi.dim(midi.c4)      -- => {60, 63, 66}
```

### midi.aug

```lua
midi.aug(root) -> table
```

Build augmented triad: root, major 3rd, augmented 5th.

```lua
midi.aug(midi.c4)      -- => {60, 64, 68}
```

### midi.dom7

```lua
midi.dom7(root) -> table
```

Build dominant 7th chord.

```lua
midi.dom7(midi.c4)     -- => {60, 64, 67, 70}
```

### midi.maj7

```lua
midi.maj7(root) -> table
```

Build major 7th chord.

```lua
midi.maj7(midi.c4)     -- => {60, 64, 67, 71}
```

### midi.min7

```lua
midi.min7(root) -> table
```

Build minor 7th chord.

```lua
midi.min7(midi.c4)     -- => {60, 63, 67, 70}
```

---

## Duration Constants

Based on 120 BPM by default. Use `midi.set_tempo()` to change.

| Constant          | Milliseconds | Musical Value |
|-------------------|--------------|---------------|
| `midi.whole`      | 2000         | Whole note    |
| `midi.half`       | 1000         | Half note     |
| `midi.quarter`    | 500          | Quarter note  |
| `midi.eighth`     | 250          | Eighth note   |
| `midi.sixteenth`  | 125          | 16th note     |

### midi.dotted

```lua
midi.dotted(duration) -> integer
```

Returns 1.5x the given duration.

```lua
midi.dotted(midi.quarter)   -- => 750
midi.dotted(midi.half)      -- => 1500
```

---

## Tempo

### midi.set_tempo

```lua
midi.set_tempo(bpm)
```

Set tempo and update all duration constants.

```lua
midi.set_tempo(120)     -- 120 BPM (default)
midi.set_tempo(60)      -- 60 BPM - durations double
midi.quarter            -- => 1000 at 60 BPM
```

### midi.get_tempo

```lua
midi.get_tempo() -> integer
```

Get current tempo in BPM.

```lua
midi.get_tempo()    -- => 120
```

### midi.bpm

```lua
midi.bpm(tempo) -> integer
```

Calculate quarter note duration for a given tempo (without changing global tempo).

```lua
midi.bpm(120)    -- => 500
midi.bpm(60)     -- => 1000
midi.bpm(140)    -- => 428
```

---

## Velocity Constants (Dynamics)

| Constant    | Value | Dynamic        |
|-------------|-------|----------------|
| `midi.ppp`  | 16    | Pianississimo  |
| `midi.pp`   | 33    | Pianissimo     |
| `midi.p`    | 49    | Piano          |
| `midi.mp`   | 64    | Mezzo-piano    |
| `midi.mf`   | 80    | Mezzo-forte    |
| `midi.f`    | 96    | Forte          |
| `midi.ff`   | 112   | Fortissimo     |
| `midi.fff`  | 127   | Fortississimo  |

---

## Timing

### midi.sleep

```lua
midi.sleep(ms)
```

Sleep for given milliseconds.

```lua
midi.sleep(500)     -- Wait half second
midi.sleep(1000)    -- Wait one second
```

### midi.rest

```lua
midi.rest([duration])
```

Rest (silence) for given duration. Default is quarter note.

```lua
midi.rest()              -- Quarter note rest
midi.rest(midi.half)     -- Half note rest
midi.rest(1000)          -- 1 second rest
```

---

## Utilities

### help

```lua
help()
```

Display available functions and usage information.

```lua
> help()
lua_midi - Lua MIDI language
...
```

---

## REPL Convenience Functions

These functions use a global `midi._out` variable for simpler interactive sessions.

### open

```lua
open() -> MidiOut
open(name) -> MidiOut
open(index) -> MidiOut
```

Open a MIDI port and set it as the default. Closes any previously open port.

```lua
open()              -- Create virtual port
open("MyApp")       -- Named virtual port
open(0)             -- Hardware port by index
```

### close

```lua
close()
```

Close the default MIDI port.

```lua
close()
```

### n

```lua
n(pitch, [velocity], [duration], [channel])
```

Play a note on the default MIDI port.

```lua
n(midi.c4)                      -- Play C4 with defaults
n(midi.c4, midi.mf)             -- With velocity
n(midi.c4, midi.mf, midi.quarter)   -- With duration
n(midi.c4, 80, 500, 2)          -- On channel 2
```

### ch

```lua
ch(pitches, [velocity], [duration], [channel])
```

Play a chord on the default MIDI port.

```lua
ch(midi.major(midi.c4))              -- C major chord
ch(midi.minor(midi.a3), midi.mf, midi.half)  -- A minor, half note
```

### arp

```lua
arp(pitches, [velocity], [duration], [channel])
```

Arpeggiate notes on the default MIDI port.

```lua
arp(midi.major(midi.c4))                     -- Arpeggiate C major
arp(midi.min7(midi.a3), midi.mf, midi.sixteenth)  -- Fast A minor 7 arpeggio
```
