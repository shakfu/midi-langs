# lua-midi

A Lua-based MIDI language using [Lua 5.5](https://www.lua.org/), providing a familiar scripting approach to MIDI programming.

## Features

- Full Lua 5.5 programming language
- Object-oriented MIDI port interface
- Musical abstractions: pitches, durations, velocities, chords
- Low-level MIDI control: note on/off, CC, program change
- Virtual and hardware MIDI port support
- Chord builders and transpose helpers
- Tempo-aware duration constants
- Readline support for command history

## Quick Start

### 1. Build

```bash
make
```

### 2. Run interactively

```bash
./build/lua_midi
```

### 3. Play some notes

Using convenience functions:
```lua
> open()
MidiOut(virtual, "luaMIDI")
> n(midi.c4)
> ch(midi.major(midi.c4))
> close()
```

Or using explicit port management:
```lua
> m = midi.open()
MidiOut(virtual, "luaMIDI")
> m:note(midi.c4, midi.mf, midi.quarter)
> m:chord(midi.major(midi.c4), midi.mf, midi.half)
> m:close()
```

## Usage Modes

### Interactive REPL

```bash
./build/lua_midi
```

Type `help()` for available functions, `quit()` or Ctrl-D to exit.

### Evaluate Expression

```bash
./build/lua_midi -e 'print(1 + 2)'
./build/lua_midi -e 'print(midi.major(midi.c4))'
```

### Run Lua File

```bash
./build/lua_midi script.lua
```

## Example Program

```lua
-- melody.lua - Simple melody with chords

m = midi.open()
midi.set_tempo(120)

-- Play a C major scale
for _, p in ipairs({midi.c4, midi.d4, midi.e4, midi.f4, midi.g4, midi.a4, midi.b4, midi.c5}) do
    m:note(p, midi.mf, midi.quarter)
end

-- Play chord progression
m:chord(midi.major(midi.c4), midi.mf, midi.half)    -- I
m:chord(midi.major(midi.f3), midi.mf, midi.half)    -- IV
m:chord(midi.major(midi.g3), midi.f, midi.half)     -- V
m:chord(midi.major(midi.c4), midi.mf, midi.whole)   -- I

m:close()
```

Run with:
```bash
./build/lua_midi melody.lua
```

## Why Lua for MIDI?

Lua's design makes it excellent for musical scripting:

- **Simple syntax**: Clean, readable code for musical patterns
- **Tables**: Flexible representation for chords and sequences
- **First-class functions**: Create reusable musical patterns
- **Metatables**: Object-oriented MIDI port interface
- **REPL**: Interactive exploration and live coding
- **Embeddable**: Easy integration with C/C++ audio applications

## REPL Tips

**Important**: In the Lua REPL, use global variables (not `local`) to persist values across lines:

```lua
-- This works:
> m = midi.open()
> m:note(midi.c4)

-- This does NOT work (m is not available on the next line):
> local m = midi.open()
> m:note(midi.c4)  -- Error: m is nil
```

For simpler interactive use, the convenience functions (`open()`, `n()`, `ch()`, `arp()`, `close()`) manage a global MIDI port automatically.

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the FFI works

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
