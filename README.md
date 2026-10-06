# midi-langs - a polyglot toolkit for MIDI creation

A collection of minimalist yet powerful language implementations designed to generate, manipulate, and transform MIDI sequences in distinct stylistic and functional paradigms.

Each implementation leverages [libremidi](https://github.com/celtera/libremidi) for seamless cross-platform MIDI I/O:

| Implementation | Language| Style & Approach |
| ---------- | ---------- | ------- |
| **alda-midi** | [Alda](https://alda.io/) | Music-first notation--compose with elegant, human-readable syntax. |
| **stack-midi** | Forth-like stack language | Concise stack-based programming for minimalist control over MIDI events. |
| **pforth-midi** | [pForth](https://github.com/philburk/pforth) | Standard ANS-style Forth: defining words, deferred words and compiled definitions. |
| **guile-midi** | [GNU Guile](https://www.gnu.org/software/guile/) | Full-featured Scheme with macros, modules, and powerful FFI. |
| **joy-midi** | [Joy](https://hypercubed.github.io/joy/joy.html) | Concatenative functional programming with algebraic note composition. |
| **lua-midi** | [Lua 5.5](https://www.lua.org/) | Lightweight scripting with tables, closures, and event-driven workflows. |
| **mhs-midi** | [MicroHs](https://github.com/augustss/MicroHs) | Pure functional Haskell for compositions, algorithmic patterns, and type-safe transformations. |
| **pktpy-midi** | [PocketPy](https://pocketpy.dev) | Pythonic abstraction with context managers and expressive APIs. |
| **s7-midi** | [s7 Scheme](https://ccrma.stanford.edu/software/snd/snd/s7.html) | Macros + Lisp for meta-programming and dynamic MIDI patterns. |

**Note**

- This is an evolving project—-expect API iterations as implementations mature.

- There is a sibling project, [psnd](https://github.com/shakfu/psnd), which embeds some of the midi-langs into a polyglot editor & REPL for music programming languages.

## Building

```sh
make              # Build everything
make test         # Run all tests
make clean        # Remove build directory
make help         # Show all targets
```

**Individual language targets:**

```sh
make alda-midi    # Alda interpreter
make stack-midi   # Forth-like stack language
make pforth-midi  # pForth interpreter
make guile-midi   # GNU Guile interpreter (requires Guile 3.0)
make joy-midi     # Joy interpreter
make lua-midi     # Lua interpreter
make pktpy-midi   # PocketPy interpreter
make s7-midi      # s7 Scheme interpreter
make mhs-midi-pkg-zstd  # MicroHs (recommended variant)
```

Requires GCC/Clang and CMake 3.16+.

Set `MIDI_LANGS_BACKEND=null` to run any interpreter without a MIDI server: ports open, and all output is discarded.

**Linux (Ubuntu/Debian):** Install ALSA and readline development libraries:

```sh
sudo apt-get install libasound2-dev libreadline-dev
```

## Quick Examples

### alda-midi

```sh
% ./alda_midi --help
Usage: ./alda_midi [options] [file.alda]

Alda music language interpreter with MIDI output.
If no file is provided, starts an interactive REPL.

Options:
  -h, --help        Show this help message
  -v, --verbose     Enable verbose output
  -l, --list        List available MIDI ports
  -p, --port N      Use MIDI port N (0-based index)
  -o, --output NAME Use MIDI port matching NAME
  --virtual NAME    Create virtual MIDI port with NAME
  --no-sleep        Disable timing delays (for testing)
  -s, --sequential  Use sequential playback mode (wait for each input)

Built-in Synth Options:
  -sf, --soundfont PATH  Use built-in synth with soundfont (.sf2)

By default, connects to the first available MIDI port (or creates a virtual
port if none exist) and uses concurrent mode for polyphonic playback.

Examples:
  ./alda_midi                            Start interactive REPL
  ./alda_midi song.alda                  Play an Alda file
  ./alda_midi -l                         List MIDI ports
  ./alda_midi -p 0 song.alda             Play using port 0
  ./alda_midi --virtual iMIDI song.alda  Create virtual port + play song
  ./alda_midi -sf gm.sf2 song.alda       Use built-in synth
```

You can enter alda syntax in a repl or create `.alda` files in a text-editor:

```alda
piano:
  (tempo 120)
  o4 c4 d e f g a b > c2

violin:
  V1: c4 d e f
  V2: e4 f g a
  V0: c1
```

By default, alda-midi uses concurrent mode where multiple parts play simultaneously on different MIDI channels. Use `-s` for sequential mode.

[Full documentation](docs/alda-midi/README.md) | [Language Reference](docs/alda-midi/language-reference.md)

### stack-midi

```sh
% ./build/stack_midi --help
Usage: ./build/stack_midi [options] [file.stk ...]
Options:
  --script FILE   Run FILE in batch mode (no REPL, exit on error)
  --no-sleep      Disable all sleep/delay calls (for testing)
  --help          Show this help

Without --script, files are loaded then REPL starts.

stack-midi - a stack language for MIDI sequence generation

Concise Notation:
  c4,                     Play C4 with defaults
  c#4, db4, 60,           Sharps, flats, or MIDI numbers
  (c4 e4 g4),             Chord (concurrent)
  r,                      Rest (silence)
  ch! vel! dur!           Set default channel/velocity/duration

Word Definitions:
  : name ... ;            Define a new word
  name N times            Execute word N times total

File Operations:
  load filename           Load and execute a .stk file
  rec / stop / save       Record commands to file
  rec-midi / stop / save-midi  Record MIDI events (stack-midi source)
  write-mid filename      Write captured events to .mid file
  read-mid filename       Read and display .mid file info

Generative:
  c4|e4,                  Alternative (random selection)
  c4 75%,                 75% chance to play
  seed! seed@ next-random  PRNG functions
  [[ ... ]] list-len      List operations
  euclidean shuffle pick  Pattern operations

Dynamics: ppp pp p mp mf f ff fff
Articulation: c4. (staccato) c4> (accent) c4- (tenuto)

MIDI Output: midi-output-list midi-output-open midi-output-virtual midi-output-close
  (aliases: midi-list midi-open-port midi-open midi-close)
MIDI Input: midi-input-list midi-input-open midi-input-virtual midi-input-close
  midi-input? midi-input@ midi-input-flush
MIDI Control: cc panic
Sequences: seq-start seq-end seq-play seq-show
Scales: scale-major scale-minor scale-blues ... scales

Type 'words' to see all available words.
Type 'quit' to exit.
```

```forth
midi-open
\ Concise notation with probability and articulation
mf c4, e4., g4>, c5-,        \ staccato, accent, tenuto
c4|e4|g4 75%,                \ random selection, 75% chance
(c4 e4 g4),                  \ chord

\ Generative pattern with anonymous block
{ c4, d4, e4, } 4 *          \ repeat block 4 times

\ Async sequence playback
0 seq-start
  c4, e4, g4,
0 seq-end
seq-play&                    \ non-blocking playback
```

[Full documentation](docs/stack-midi/README.md) | [API](docs/stack-midi/api-reference.md) | [Tutorial](docs/stack-midi/tutorial.md)

### pforth-midi

```sh
% ./build/pforth_midi --help
Usage: ./build/pforth_midi [-q] [file.fs]

pForth with MIDI words. With no file, starts the interactive prompt.
Type MIDI-HELP at the prompt for the MIDI words, BYE to exit.

  -q    No banner
```

```forth
midi-open
c4 major chord  a3 minor chord               \ chords are pitches + count
c4 scale-major build-scale arpeggio

\ Standard Forth: a defining word for instruments
: instrument ( ch prog "name" -- )
    create , ,  does> dup cell+ @ to chan  chan swap @ program ;
2 33 instrument bass
bass c2 note
```

[Full documentation](docs/pforth-midi/README.md)

### guile-midi

```sh
% ./build/guile_midi --help
Usage: ./build/guile_midi [options] [file.scm|file.w]
Options:
  -e EXPR    Evaluate expression and print result
  --no-sleep Skip all waits; give it before the file (for testing)
  --version  Show version
  --help     Show this help

File types:
  .scm       Standard Scheme file
  .w         Wisp syntax file (indentation-based)

Without arguments, starts an interactive REPL.
```

GNU Guile Scheme with full MIDI support. Shares the same prelude as s7-midi (R5RS compatible):

```scheme
(open)
;; Functional transformations
(define melody (scale c4 'dorian))
(define bass (map (lambda (p) (- p 12)) melody))

;; Thunk-based concurrent voices
(spawn (make-melody-voice melody mf eighth) "melody")
(spawn (make-chord-voice (major c3) ff whole) "bass")

(run)
(close)
```

Note: guile-midi requires GNU Guile 3.0 to be installed. On macOS, Guile and its dependencies are statically linked (only system libraries required at runtime).

### joy-midi

```sh
% ./build/joy_midi --help
Usage: ./build/joy_midi [options] [file.joy]
Options:
  -h, --help     Show this help
  -v, --version  Show version
  --no-sleep     Skip all waits (for testing)

MIDI words:
  midi-list    - List MIDI output ports
  midi-virtual - Create virtual MIDI port 'JoyMIDI'
  midi-open    - Open port by index (n -- )
  midi-note    - Play note (pitch vel dur -- )
  midi-chord   - Play chord ([pitches] vel dur -- )
  pitch        - Parse pitch name ("C4" -- 60)
  major        - Build major triad (root -- [pitches])
  minor        - Build minor triad (root -- [pitches])
```

Joy-MIDI treats notes as integers at parse time, enabling algebraic composition:

```joy
\ Notes are integers - c=60, d=62, e=64, etc.
c d e                     \ Stack: 60 62 64
[c d e]                   \ List: [60 62 64]

\ All Joy combinators work naturally
[c d e] [7 +] map         \ Transpose: [67 69 71]
[c e g] reverse           \ Reverse: [67 64 60]

\ Play notes sequentially or as chord
[c e g] play              \ Arpeggio
[c e g] chord             \ C major chord

\ User-defined words
def pick == dup size rand swap rem at .
def arp == [play] step .

[c d e f g a b] pick play         \ Random note
[c e g] 12 [+] cons map arp       \ Arpeggiate octave up

\ Chord builders and dynamics
mf c major chord          \ C major at mezzo-forte
ff d minor chord          \ D minor at fortissimo
```

[Full documentation](docs/joy-midi/README.md) | [Next Steps](docs/joy-midi/next-steps.md)

### lua-midi

```sh
% ./build/lua_midi --help
Usage: ./build/lua_midi [options] [file.lua]
Options:
  -e EXPR    Execute Lua statement
  --no-sleep Skip all waits; give it before the file (for testing)
  --version  Show version
  --help     Show this help

Without arguments, starts an interactive REPL.
```

```lua
open()
-- Concurrent voices with coroutines
spawn(function()
    for _, p in ipairs(scale(c4, "pentatonic")) do
        play(p, mf, eighth)
    end
end, "melody")

spawn(function()
    ch(major(c3), ff, whole)  -- sustained chord
end, "harmony")

run()  -- wait for all voices
close()
```

[Full documentation](docs/lua-midi/README.md) | [API Reference](docs/lua-midi/api-reference.md)

### mhs-midi

```sh
% ./scripts/mhs-midi --help
usage: mhs-midi [-h] {repl,compile,run} ...

MicroHs MIDI development tool

positional arguments:
  {repl,compile,run}  Command to run
    repl              Start interactive REPL (default)
    compile           Compile Haskell file to executable
    run               Compile and run Haskell file

options:
  -h, --help          show this help message and exit

mhs-midi - MicroHs MIDI development tool

Commands:
    repl              Start interactive REPL with MIDI support (default)
    compile FILE.hs   Compile Haskell file to standalone executable
    run FILE.hs       Compile and immediately run

Examples:
    mhs-midi                          # Start REPL
    mhs-midi repl                     # Start REPL (explicit)
    mhs-midi compile demo.hs          # Compile to ./demo
    mhs-midi compile demo.hs -o out   # Compile to ./out
    mhs-midi run demo.hs              # Compile and run
```

**Interactive REPL:**

```sh
./scripts/mhs-midi              # Start REPL
./scripts/mhs-midi repl         # Same, explicit
```

**Standalone binaries** (no external files needed):

```sh
make mhs-midi-pkg-zstd  # Recommended (3MB, fast startup)
./build/mhs-midi-pkg-zstd -r MyFile.hs

# Other variants:
make mhs-midi-src       # 3.3MB, source embedding
make mhs-midi-src-zstd  # 1.3MB, smallest
make mhs-midi-pkg       # 4.8MB, fast startup
make mhs-midi-all       # Build all variants

# Or using cmake directly:
cmake --build build --target mhs-midi-pkg-zstd
```

**Write and compile a program:**

```haskell
-- DemoMhs.hs
module DemoMhs(main) where

import MusicPerform

main = do
    midiOpenVirtual "demo"
    -- Pure functional Music DSL with combinators
    let melody = line [c4, e4, g4, c5] mf eighth
        bass   = note c3 ff whole
        piece  = melody ||| bass  -- parallel composition

    perform piece

    -- Concurrent voices with native threads
    spawn "arp" $ perform (line (arpUp [c4, e4, g4]) mp sixteenth)
    spawn "pad" $ perform (chord [c3, g3, e4] pp whole)
    run
    midiClose
```

```sh
./scripts/mhs-midi compile demo.hs        # Creates ./demo
./scripts/mhs-midi compile demo.hs -o out # Creates ./out
./scripts/mhs-midi run demo.hs            # Compile and run
```

[Full documentation](docs/mhs-midi/README.md) | [API Reference](docs/mhs-midi/api-reference.md)

### pktpy-midi

```sh
% ./build/pktpy_midi --help
Usage: ./build/pktpy_midi [options] [file.py]

PocketPy interpreter with MIDI support.

Options:
  -e EXPR        Execute Python statement
  -l, --list     List available MIDI output ports
  --profile      Enable profiler (file mode only)
  --debug        Enable debugger (file mode only)
  --no-sleep     Skip all waits (for testing)
  -v, --version  Show version information
  -h, --help     Show this help message

Without arguments, starts an interactive REPL.

Examples:
  ./build/pktpy_midi                    # Start REPL
  ./build/pktpy_midi script.py          # Run a Python file
  ./build/pktpy_midi -e "print(1+2)"    # Execute expression
  ./build/pktpy_midi -l                 # List MIDI ports
```

A pktpy-midi file is a regular python3 file which can import the custom `midi` module (and other [pocketpy](https://pocketpy.dev) modules):

```python
import midi

# Generator-based async voices
def arpeggio():
    with midi.open() as m:
        for p in midi.scale(midi.c4, "dorian"):
            yield from midi.play(m, p, midi.mf, midi.eighth)

def drone():
    with midi.open() as m:
        yield from midi.play(m, midi.c2, midi.pp, midi.whole * 4)

# Concurrent playback
midi.spawn(arpeggio, "arp")
midi.spawn(drone, "drone")
midi.run()
```

[Full documentation](docs/pktpy-midi/README.md) | [API Reference](docs/pktpy-midi/api-reference.md)

### s7-midi

```sh
./build/s7_midi
```

```scheme
(open)
;; Functional transformations
(define melody (scale c4 'dorian))
(define bass (invert melody c4))  ; melodic inversion

;; Thunk-based concurrent voices
(spawn (make-melody-voice melody mf eighth) "melody")
(spawn (make-melody-voice bass ff quarter) "bass")

;; Euclidean rhythm: 3 hits over 8 steps, one (action . delay) per step
(define (drum-step hit)
  (cons (lambda ()
          (midi-note-off *midi* c2 1)
          (if hit (midi-note-on *midi* c2 ff 1)))
        sixteenth))
(spawn (make-sequence-voice (map drum-step (euclidean 3 8))) "rhythm")

(run)
(close)
```

[Full documentation](docs/s7-midi/README.md) | [API Reference](docs/s7-midi/api-reference.md)

## Common Features

Most implementations share functionality from a common C library (`projects/common/music_theory.c`):

| Implementation | Uses music_theory.c |
| ---------------- | --------------------- |
| alda-midi | No (uses own parser/interpreter) |
| stack-midi | Yes |
| pforth-midi | Yes |
| guile-midi | Yes |
| joy-midi | No (uses pyjoy-runtime) |
| lua-midi | Yes |
| mhs-midi | Yes |
| pktpy-midi | Yes |
| s7-midi | Yes |

The shared library provides:

- **Pitch parsing**: Note names like "C4", "C#4", "Db5" to MIDI numbers

- **55 scales**: Major, minor, modes, pentatonic, blues, world scales, maqamat, ragas

- **10 microtonal scales**: Quarter-tone maqamat, Turkish makamlar, Indian shruti

- **Scale operations**: Build scales, get degrees, check membership, quantize

- **Chord intervals**: Major, minor, diminished, augmented, 7th chords

- **Dynamics**: ppp to fff velocity constants

- **Durations**: Whole, half, quarter, eighth, sixteenth note values

## Prelude System

Interpreted languages have prelude files in their native syntax:

```sh
projects/guile-midi/prelude.scm -> Scheme constants and helpers (shared with s7)
projects/s7-midi/prelude.scm    -> Scheme constants and helpers
projects/lua-midi/prelude.lua   -> Lua constants and helpers
projects/pktpy-midi/prelude.py  -> Python constants and helpers
```

These are converted to C headers at build time via `scripts/prelude2c.py`.

pforth-midi's vocabulary, `projects/pforth-midi/midi.fth`, is compiled into pForth's dictionary at build time and embedded in the binary.

```sh
make preludes     # Regenerate all prelude headers
```

## Architecture

```sh
projects/
  alda-midi/        # Alda interpreter (~3000 lines C)
  common/           # Shared music theory library
  stack-midi/       # Forth-like stack language (~2700 lines C)
  pforth-midi/      # pForth + MIDI words
  guile-midi/       # GNU Guile + MIDI bindings
  joy-midi/         # Joy interpreter with MIDI extensions
  lua-midi/         # Lua 5.5 + MIDI bindings
  mhs-midi/         # MicroHs + MIDI FFI
  pktpy-midi/       # PocketPy + MIDI bindings
  s7-midi/          # s7 Scheme + MIDI bindings
thirdparty/
  libremidi/        # MIDI I/O library (auto-built)
  libuv/            # Event loop for async playback
  lua-5.5.0/        # Lua interpreter
  MicroHs/          # Haskell compiler
  miniaudio/        # Audio output for alda-midi's built-in synth
  pforth/           # pForth (Forth interpreter)
  pocketpy/         # Python interpreter
  readerwriterqueue/  # Lock-free queue for libremidi's PipeWire backend
  s7/               # Scheme interpreter
  TinySoundFont/    # SoundFont synth for alda-midi
  zstd-1.5.7/       # Compression for mhs-midi standalone variants
docs/               # Per-language documentation
tests/              # Test suite
```

## MIDI Playback

All languages can create a virtual MIDI port or open an existing one. To hear sound, connect to a General MIDI synthesizer.

### Opening a running synth

Port lists show `client: port`. Start the synth, list the ports, then open the synth's port by name or index:

```sh
fluidsynth                                    # or: timidity -iA -x "soundfont /usr/share/sounds/sf2/FluidR3_GM.sf2"
./build/alda_midi -l                          # 1: FLUID Synth (1234): Synth input port (1234:0)
./build/alda_midi -o FLUID song.alda          # -o matches a substring of the label, case-sensitive
```

The other languages list ports with `midi-list`, `midi.list_ports()` or `(midi-list-ports)`, and open one by index. stack-midi also opens by name: `midi-open-as FLUID`.

### FluidSynth (Recommended)

Use the included helper script:

```sh
# Set up SoundFont directory (one-time)
export ALDA_SF2_DIR=~/Music/sf2

# Start FluidSynth (in one terminal)
python scripts/fluidsynth-gm.py

# alternatively you can directly specify the soundfont
python scripts/fluidsynth-gm.py ~/Music/sf2/FluidR3_GM.sf2

# Run any midi-langs interpreter (in another terminal)
./build/alda_midi
./build/stack_midi
./build/pforth_midi
./build/lua_midi
```

Install [FluidSynth](https://www.fluidsynth.org) and a SoundFont:

| Platform | Install |
| ---------- | --------- |
| macOS | `brew install fluidsynth` |
| Ubuntu/Debian | `sudo apt install fluidsynth` |
| Fedora | `sudo dnf install fluidsynth` |

Download a GM SoundFont like [FluidR3_GM.sf2](https://musical-artifacts.com/artifacts/738) and place it in your `ALDA_SF2_DIR`.

### Other Options

- **DAW**: Route the virtual MIDI port to any DAW (Logic, Ableton, Reaper)

- **Hardware**: Connect to GM-compatible synthesizers or sound modules

- **Software synths**: Use any virtual instrument that accepts MIDI input

## Documentation

| Language | Docs |
| ---------- | ------ |
| alda-midi | [README](docs/alda-midi/README.md), [Language](docs/alda-midi/language-reference.md), [Examples](docs/alda-midi/examples.md) |
| stack-midi | [README](docs/stack-midi/README.md), [API](docs/stack-midi/api-reference.md), [Tutorial](docs/stack-midi/tutorial.md) |
| pforth-midi | [README](docs/pforth-midi/README.md) |
| guile-midi | See s7-midi (compatible API) |
| joy-midi | [README](docs/joy-midi/README.md), [Next Steps](docs/joy-midi/next-steps.md) |
| lua-midi | [README](docs/lua-midi/README.md), [API](docs/lua-midi/api-reference.md), [Examples](docs/lua-midi/examples.md) |
| mhs-midi | [README](docs/mhs-midi/README.md), [API](docs/mhs-midi/api-reference.md), [Examples](docs/mhs-midi/examples.md) |
| pktpy-midi | [README](docs/pktpy-midi/README.md), [API](docs/pktpy-midi/api-reference.md), [Examples](docs/pktpy-midi/examples.md) |
| s7-midi | [README](docs/s7-midi/README.md), [API](docs/s7-midi/api-reference.md), [Examples](docs/s7-midi/examples.md) |

See [CHANGELOG.md](CHANGELOG.md) for recent changes.

## Prior Art

This is not a comprehensive list:

- [abcmidi](https://abcmidi.sourceforge.io) - Abc2midi is a program that converts an abc music notation file to a MIDI file.

- [alda](https://alda.io) - a text-based programming language for music composition. Designed for musicians who don’t know how to program, as well as programmers who don’t know how to music.

- [chuck](https://chuck.cs.princeton.edu) - a strongly-timed programming language for real-time sound synthesis and music creation.

- [extempore](https://extemporelang.github.io) - an audiovisual live programming environment.

- [lilypond](https://lilypond.org) - a compiled system: it is run on a text file describing the music. The resulting output is viewed on-screen or printed. In some ways, LilyPond is more similar to a programming language than graphical score editing software.

- [melrose](https://github.com/emicklei/melrose) - a tool to create and play music by programming melodies. It uses a custom language to compose notes and create loops and tracks to play.

- [mezzo](https://github.com/DimaSamoz/mezzo) - A Haskell library for typesafe music composition

- [miti](https://github.com/schollz/miti) - a musical instrument textual interface. Basically, its MIDI, but with human-readable text.

- [mma](https://www.mellowood.ca/mma/) - an accompaniment generator. It creates MIDI tracks for a soloist to perform over from a user supplied file containing chords and MMA directives.

- [opusmodus](https://www.opusmodus.com) - a commercial music composition development environment. -- Uses common lisp. "currently the most advanced software for computer-assisted composition available".

- [scamp](https://github.com/MarcTheSpark/scamp) - Suite for Computer-Assisted Music in Python - a computer-assisted composition framework in Python designed to act as a hub, flexibly connecting the composer-programmer to a wide variety of resources for playback and notation.

- [textbeat](https://github.com/flipcoder/textbeat) - Compose music in a plaintext format or type music directly in the shell. The format is vertical and column-based, similar to early music trackers, but with syntax inspired by jazz/music theory.
