# mhs-midi

A Haskell MIDI library for MicroHs, providing a functional approach to MIDI programming with an interactive REPL.

## Features

- Interactive REPL with MIDI support
- Compile Haskell files to standalone executables
- Musical abstractions: pitches, durations, velocities, chords
- Low-level MIDI control: note on/off, CC, program change, pitch bend
- Virtual and hardware MIDI port support

## Quick Start

### 1. Build

```bash
make
```

### 2. Start the REPL

```bash
./scripts/mhs-midi-repl
```

### 3. Play some notes

```haskell
> import MidiRepl
> open
MIDI open
> n c4
> mapM_ n [c4, e4, g4]
> ch [c4, e4, g4]
> close
```

## Usage Modes

### Interactive REPL

```bash
./scripts/mhs-midi-repl
```

Note: Use `MidiRepl` for REPL-friendly functions (all return `IO ()`, no Show constraint errors).

### Run Haskell File (Interpreted)

```bash
./scripts/mhs-midi-repl -r MyProgram.hs
```

### Compile to Executable

```bash
./scripts/mhs-midi-compile MyProgram.hs -o my_program
./my_program
```

## Example Program

For compiled programs, use the `Music` module with pure data and transformations:

```haskell
module MyMelody(main) where
import Music

melody = line [c4, e4, g4, c5]
chords = times 2 (chord [c4, e4, g4])
piece = melody +:+ chords

main :: IO ()
main = do
    midiOpenVirtual "MyMelody"
    perform piece
    midiClose
```

### Pure Transformations

```haskell
module Expressive(main) where
import Music

melody = line [c4, d4, e4, f4, g4]
bass = withChan 2 (line [c2, g2, c2, g2])

-- Pure transformations
piece = transpose 5 (stretch 2 (melody ||| bass))

main :: IO ()
main = do
    midiOpenVirtual "Expressive"
    perform piece
    midiClose
```

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the FFI works

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
