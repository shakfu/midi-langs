# mhs-midi: A Haskell MIDI Library for MicroHs

Announcing `mhs-midi`, a new Haskell MIDI library designed for the MicroHs compiler. It offers two distinct approaches to music creation: a pure, functional DSL for composition and an immediate, REPL-friendly interface for interactive performance and generative music.

## Key Features

- **Dual-API Design**:
  - **`MusicPerform`**: A pure, declarative DSL for composing and transforming music as data. Use combinators like `(+:+)` (sequence) and `(|||)` (parallel) and transformations like `transpose`, `stretch`, and `invert`.
  - **`MidiPerform`**: An immediate, `IO`-based API for REPL-style interaction, allowing you to play notes, chords, and melodies directly.

- **Asynchronous Playback**: A built-in scheduler allows you to `spawn` multiple concurrent voices that run in parallel using native Haskell threads.

- **Generative Music**: Includes functions for creating algorithmic and random music, such as:
  - Random walks (`walk`, `drunk`)
  - Euclidean rhythms (`euclidean`)
  - Random selection (`pick`, `chance`, `oneOf`)

- **Rich Music Theory Support**:
  - Comes with 55 built-in scales, including diatonic modes, pentatonics, blues, world scales, and more.
  - Microtonal support with cents-based scales for quarter-tone music.

- **Interactive REPL**: The `mhs-midi-repl` provides a fast-starting environment for live coding and experimentation, with MIDI ports accessible for immediate feedback.

## Quick Example (Pure Composition)

```haskell
import MusicPerform

-- Define a melody and a bass line as pure data
melody = line [c4, e4, g4] mf quarter
bass = note c2 ff half

-- Combine them in parallel and sequence them
piece = (melody ||| bass) +:+ transpose 5 (melody ||| bass)

main = do
    midiOpenVirtual "MyComposition"
    perform piece
    midiClose
```

## Quick Example (Immediate IO)

This works great in a repl.

```haskell
import MidiPerform

main = do
    open
    -- Play a C major chord immediately
    chord [c4, e4, g4]
    -- Generate a 16-note random walk within the C major scale
    drunk 16 c4 (major c4) 2
    close
```

The project is built on `libremidi` for cross-platform MIDI support on macOS, Linux, and Windows.

For more information, check out the documentation in the `docs/mhs-midi` directory.
