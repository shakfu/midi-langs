# lua-midi Examples

## Basic Examples

### Hello MIDI

Play a C major scale:

```lua
m = midi.open()

for _, p in ipairs({midi.c4, midi.d4, midi.e4, midi.f4, midi.g4, midi.a4, midi.b4, midi.c5}) do
    m:note(p, midi.mf, midi.quarter)
end

m:close()
```

### Using Note Names

Play notes using string notation:

```lua
m = midi.open()

m:note(midi.note("C4"))
m:note(midi.note("D4"))
m:note(midi.note("E4"))
m:note(midi.note("F4"))
m:note(midi.note("G4"), midi.f, midi.half)  -- Louder, longer

m:close()
```

### Chord Progression

Play a I-IV-V-I progression:

```lua
m = midi.open()

-- I - C major
m:chord(midi.major(midi.c4), midi.mf, midi.half)

-- IV - F major
m:chord(midi.major(midi.f3), midi.mf, midi.half)

-- V - G major
m:chord(midi.major(midi.g3), midi.mf, midi.half)

-- I - C major
m:chord(midi.major(midi.c4), midi.f, midi.whole)

m:close()
```

### Melody with Dynamics

```lua
m = midi.open()

-- Crescendo
m:note(midi.c4, midi.pp, midi.quarter)
m:note(midi.d4, midi.p, midi.quarter)
m:note(midi.e4, midi.mp, midi.quarter)
m:note(midi.f4, midi.mf, midi.quarter)
m:note(midi.g4, midi.f, midi.quarter)
m:note(midi.a4, midi.ff, midi.half)

-- Decrescendo
m:note(midi.g4, midi.f, midi.quarter)
m:note(midi.f4, midi.mf, midi.quarter)
m:note(midi.e4, midi.mp, midi.quarter)
m:note(midi.d4, midi.p, midi.quarter)
m:note(midi.c4, midi.pp, midi.whole)

m:close()
```

---

## Intermediate Examples

### Arpeggiated Chords

```lua
m = midi.open()

-- Arpeggiate C major up and down
m:arpeggio({midi.c4, midi.e4, midi.g4, midi.c5}, midi.mf, midi.eighth)
m:arpeggio({midi.c5, midi.g4, midi.e4, midi.c4}, midi.mf, midi.eighth)

midi.rest(midi.quarter)

-- Arpeggiate A minor
m:arpeggio(midi.minor(midi.a4), midi.ff, midi.sixteenth)

-- Reverse using table manipulation
local am = midi.minor(midi.a4)
local reversed = {}
for i = #am, 1, -1 do
    table.insert(reversed, am[i])
end
m:arpeggio(reversed, midi.ff, midi.sixteenth)

m:close()
```

### Tempo Changes

```lua
-- Start slow
midi.set_tempo(60)

m = midi.open()

m:note(midi.c4, midi.mf, midi.quarter)  -- 1000ms at 60 BPM
m:note(midi.e4, midi.mf, midi.quarter)
m:note(midi.g4, midi.mf, midi.quarter)

-- Speed up
midi.set_tempo(120)
m:note(midi.c5, midi.mf, midi.quarter)  -- 500ms at 120 BPM
m:note(midi.e5, midi.mf, midi.quarter)
m:note(midi.g5, midi.mf, midi.quarter)

-- Even faster
midi.set_tempo(180)
m:note(midi.c6, midi.mf, midi.quarter)  -- ~333ms at 180 BPM
m:note(midi.e6, midi.mf, midi.quarter)
m:note(midi.g6, midi.mf, midi.quarter)

m:close()
```

### Dotted Rhythms

```lua
m = midi.open()

-- Dotted quarter - eighth pattern
m:note(midi.c4, midi.mf, midi.dotted(midi.quarter))
m:note(midi.d4, midi.mf, midi.eighth)
m:note(midi.e4, midi.mf, midi.dotted(midi.quarter))
m:note(midi.f4, midi.mf, midi.eighth)
m:note(midi.g4, midi.f, midi.half)

m:close()
```

### All Chord Types

```lua
m = midi.open()
local root = midi.c4

-- Triads
m:chord(midi.major(root), midi.mf, midi.half)
m:chord(midi.minor(root), midi.mf, midi.half)
m:chord(midi.dim(root), midi.mf, midi.half)
m:chord(midi.aug(root), midi.mf, midi.half)

midi.rest(midi.quarter)

-- Seventh chords
m:chord(midi.dom7(root), midi.mf, midi.half)
m:chord(midi.maj7(root), midi.mf, midi.half)
m:chord(midi.min7(root), midi.mf, midi.half)

m:close()
```

---

## Advanced Examples

### Higher-Order Functions

Use Lua's functional features for musical patterns:

```lua
m = midi.open()

-- Create a note-playing function with fixed parameters
function make_player(velocity, duration)
    return function(pitch)
        m:note(pitch, velocity, duration)
    end
end

-- Define different "instruments"
local loud_short = make_player(midi.fff, midi.sixteenth)
local soft_long = make_player(midi.pp, midi.half)

-- Use them
for _, p in ipairs({midi.c4, midi.e4, midi.g4, midi.c5}) do
    loud_short(p)
end

for _, p in ipairs({midi.c5, midi.g4, midi.e4, midi.c4}) do
    soft_long(p)
end

m:close()
```

### Pattern Repetition

```lua
m = midi.open()

-- Define a pattern as a function
function pattern1()
    m:note(midi.c4, midi.mf, midi.eighth)
    m:note(midi.e4, midi.mf, midi.eighth)
    m:note(midi.g4, midi.mf, midi.eighth)
    m:note(midi.e4, midi.mf, midi.eighth)
end

-- Repeat it 4 times
for i = 1, 4 do
    pattern1()
end

-- Or use a helper
function times(n, func)
    for i = 1, n do
        func()
    end
end

times(4, pattern1)

m:close()
```

### Transpose a Melody

```lua
m = midi.open()

local melody = {midi.c4, midi.d4, midi.e4, midi.f4, midi.g4}

-- Original
for _, p in ipairs(melody) do
    m:note(p, midi.mf, midi.eighth)
end
midi.rest(midi.quarter)

-- Up a perfect fifth
for _, p in ipairs(melody) do
    m:note(midi.transpose(p, 7), midi.mf, midi.eighth)
end
midi.rest(midi.quarter)

-- Down an octave
for _, p in ipairs(melody) do
    m:note(midi.octave_down(p), midi.mf, midi.eighth)
end

m:close()
```

### Control Changes

```lua
m = midi.open()

-- Set volume
m:cc(7, 100)

-- Enable sustain pedal
m:cc(64, 127)

-- Play notes with sustain
m:note(midi.c4, midi.mf, midi.quarter)
m:note(midi.e4, midi.mf, midi.quarter)
m:note(midi.g4, midi.mf, midi.quarter)

-- Release sustain
m:cc(64, 0)

midi.rest(midi.quarter)

-- Modulation sweep
m:note_on(midi.c4, 80)
for i = 0, 127, 8 do
    m:cc(1, i)
    midi.sleep(50)
end
for i = 127, 0, -8 do
    m:cc(1, i)
    midi.sleep(50)
end
m:note_off(midi.c4)

m:close()
```

### Program Changes

```lua
m = midi.open()

-- Piano (program 0)
m:program(0)
m:note(midi.c4, midi.mf, midi.quarter)
m:note(midi.e4, midi.mf, midi.quarter)
m:note(midi.g4, midi.mf, midi.quarter)

-- Strings (program 48)
m:program(48)
m:chord(midi.major(midi.c4), midi.mp, midi.whole)

-- Brass (program 61)
m:program(61)
m:note(midi.c5, midi.ff, midi.half)

m:close()
```

### Generative Music

```lua
m = midi.open()
midi.set_tempo(140)

-- Random note from a table
function random_element(t)
    return t[math.random(#t)]
end

-- Random velocity in range
function random_velocity(min, max)
    return min + math.random(max - min)
end

-- Play 32 random notes from C major 7
local chord = midi.maj7(midi.c4)
for i = 1, 32 do
    m:note(
        random_element(chord),
        random_velocity(60, 100),
        midi.sixteenth
    )
end

m:close()
```

### Probability-Based Patterns

```lua
m = midi.open()

-- Play note with probability
function maybe_play(pitch, prob)
    if math.random(100) < prob then
        m:note(pitch, midi.mf, midi.sixteenth)
    else
        midi.rest(midi.sixteenth)
    end
end

-- Sparse texture
for i = 1, 32 do
    maybe_play(midi.c4, 30)   -- 30% chance
    maybe_play(midi.e4, 50)   -- 50% chance
    maybe_play(midi.g4, 70)   -- 70% chance
end

m:close()
```

### Multi-Channel Composition

```lua
m = midi.open()

-- Set up instruments
m:program(0, 1)    -- Piano on channel 1
m:program(32, 2)   -- Bass on channel 2

-- Helper for channel-specific playing
function play_on_channel(ch, pitch, vel, dur)
    m:note(pitch, vel, dur, ch)
end

-- Simple bass line and melody
function bar()
    -- Bass note (channel 2, plays first)
    play_on_channel(2, midi.c2, 100, midi.half)
    -- Melody notes (channel 1)
    play_on_channel(1, midi.c4, 80, midi.quarter)
    play_on_channel(1, midi.e4, 80, midi.quarter)
end

-- Play 4 bars
for i = 1, 4 do
    bar()
end

m:close()
```

---

## REPL Session Examples

Start the REPL:
```bash
./build/lua_midi
```

### Using Convenience Functions (Recommended)

The simplest way to use lua-midi interactively:

```lua
> open()
MidiOut(virtual, "luaMIDI")
> n(midi.c4)
> n(midi.e4)
> n(midi.g4)
> ch(midi.major(midi.c4))
> arp(midi.min7(midi.a3), midi.mf, midi.sixteenth)
> close()
```

### Quick Note Test (Explicit Port)

```lua
> m = midi.open()
MidiOut(virtual, "luaMIDI")
> m:note(midi.c4)
> m:note(midi.e4)
> m:note(midi.g4)
> m:close()
```

### Interactive Chord Exploration

```lua
> m = midi.open()
> m:chord(midi.major(midi.c4))
> m:chord(midi.minor(midi.a3))
> m:chord(midi.dom7(midi.g3))
> m:close()
```

### Testing Dynamics

```lua
> m = midi.open()
> m:note(midi.c4, midi.ppp)   -- Very soft
> m:note(midi.c4, midi.mf)    -- Medium
> m:note(midi.c4, midi.fff)   -- Very loud
> m:close()
```

### List MIDI Ports

```lua
> midi.list_ports()
{{0, "IAC Driver Bus 1"}, {1, "USB MIDI Device"}}
```

### Checking Values

```lua
> midi.c4
60
> midi.major(midi.c4)
{60, 64, 67}
> midi.mf
80
> midi.quarter
500
> midi.bpm(60)
1000
```
