# guile-midi

A Scheme-based MIDI language using [GNU Guile 3.0](https://www.gnu.org/software/guile/), providing a functional/Lisp approach to MIDI programming with access to Guile's full ecosystem.

## Features

- Full GNU Guile 3.0 (modules, GOOPS, macros, debugging)
- Musical abstractions: pitches, durations, velocities, chords
- **55 built-in scales** (modes, pentatonics, blues, exotic, Arabic Maqamat, Indian Ragas)
- **10 microtonal scales** with quarter-tone support via pitch bend
- Scale functions: build scales, get degrees, quantize pitches
- Low-level MIDI control: note on/off, CC, program change, pitch bend
- Virtual and hardware MIDI port support
- Chord builders and transpose helpers
- Tempo-aware duration constants
- **Async scheduler**: concurrent voice playback using thunk-based cooperative multitasking
- **Static linking on macOS**: self-contained binary with only system library dependencies

## Quick Start

### 1. Build

```bash
make
```

### 2. Run interactively

```bash
./build/guile_midi
```

### 3. Play some notes

Using convenience functions:

```scheme
> (open)
#<midi-out virtual "guileMIDI">
> (n c4)
> (ch (major c4))
> (close)
```

Or using explicit port management:

```scheme
> (define m (midi-open))
#<midi-out virtual "guileMIDI">
> (midi-note m c4 mf quarter)
> (midi-chord m (major c4) mf half)
> (midi-close m)
```

## Usage Modes

### Interactive REPL

```bash
./build/guile_midi
```

Type `(help)` for available functions, `(quit)` to exit.

### Evaluate Expression

```bash
./build/guile_midi -e '(+ 1 2)'
./build/guile_midi -e '(major c4)'
```

### Run Scheme File

```bash
./build/guile_midi script.scm
```

## Example Program

```scheme
;; melody.scm - Simple melody with chords

(define m (midi-open))
(set-tempo! 120)

;; Play a C major scale
(for-each (lambda (p) (midi-note m p mf quarter))
          (list c4 d4 e4 f4 g4 a4 b4 c5))

;; Play chord progression
(midi-chord m (major c4) mf half)    ; I
(midi-chord m (major f3) mf half)    ; IV
(midi-chord m (major g3) f half)     ; V
(midi-chord m (major c4) mf whole)   ; I

(midi-close m)
```

Run with:

```bash
./build/guile_midi melody.scm
```

## Async Playback

Play multiple voices concurrently using the thunk-based scheduler:

```scheme
(open)

;; Voice 1: Bass pattern (repeating)
(spawn (make-repeat-voice
         (lambda () (midi-note *midi* c2 f sixteenth))
         8
         eighth)
       "bass")

;; Voice 2: Chord progression
(spawn (make-sequence-voice
         (list
           (cons (lambda () (midi-chord *midi* (major c4) mf half)) half)
           (cons (lambda () (midi-chord *midi* (major f3) mf half)) half)
           (cons (lambda () (midi-chord *midi* (major g3) f half)) half)
           (cons (lambda () (midi-chord *midi* (major c4) mf whole)) 0)))
       "chords")

(run)   ; Both voices play simultaneously
(close)
```

Each voice is a closure that returns ms-to-wait or `#f` when done. See [API Reference](api-reference.md#async-scheduler) for details.

## Why Guile for MIDI?

Guile provides advantages over s7-midi for certain use cases:

- **Full GNU Guile**: Access to Guile's module system, GOOPS OOP, and debugging
- **Ecosystem**: Use existing Guile libraries and tools
- **Better errors**: Guile provides more informative error messages
- **Same API**: Shares the prelude with s7-midi - code is portable between them

Scheme's functional nature makes it ideal for musical programming:

- **First-class functions**: Pass note-playing functions as arguments
- **Closures**: Create parameterized musical patterns
- **Lists**: Natural representation for chords and sequences
- **Macros**: Define custom musical notation
- **REPL**: Interactive exploration and live coding

## guile-midi vs s7-midi

| Feature | guile-midi | s7-midi |
| --------- | ------------ | --------- |
| Interpreter | GNU Guile 3.0 | s7 (embedded) |
| Binary size | ~2.6MB (static) | ~350KB |
| Module system | Full (use-modules) | Limited |
| OOP | GOOPS | None |
| Debugging | Better | Basic |
| API | Same prelude | Same prelude |
| Startup | Instant | Instant |

Choose guile-midi if you need Guile's full features. Choose s7-midi for a smaller binary or when Guile isn't available.

## Documentation

- [API Reference](api-reference.md) - Complete function documentation
- [Examples](examples.md) - More code examples
- [Architecture](architecture.md) - How the Guile FFI works

## Requirements

- Built project (`make` must complete successfully)
- GNU Guile 3.0 (for building; statically linked on macOS)
- macOS (CoreMIDI), Linux (ALSA), or Windows (WinMM)
