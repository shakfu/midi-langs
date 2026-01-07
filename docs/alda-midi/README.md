# alda-midi

A C implementation of the [Alda](https://alda.io/) music language with MIDI output via libremidi and a built-in synthesizer using TinySoundFont/miniaudio.

## Features

- Full Alda language parser (38 tokens, 28 AST node types)
- Music-first notation: notes, chords, rests, ties, dots
- Part declarations with 128 GM instruments
- Attributes: tempo, volume, dynamics, quantization, panning
- Voices for polyphony (V1:, V2:, V0:)
- Auto MIDI channel assignment (1-16)
- Non-blocking REPL with concurrent playback (default)
- Auto-connects to first available MIDI port
- File playback and interactive REPL modes
- Virtual and hardware MIDI port support
- **Built-in synthesizer** using TinySoundFont + miniaudio (no external synth required, just soundfonts)

## Quick Start

### 1. Build

```bash
make
```

### 2. Choose an MIDI output method

**Option A: Auto mode (virtual port or first available)**

```bash
./build/alda_midi                         # Creates "AldaMIDI" virtual port
```

Connect FluidSynth, a DAW, or hardware synth to the "AldaMIDI" port to hear sound. If a MIDI port is already available (e.g., FluidSynth running), alda-midi connects to it automatically.

**Option B: Specific MIDI port (by index or name)**

```bash
./build/alda_midi -l                      # List available MIDI ports
./build/alda_midi -p 0                    # Connect by port index
./build/alda_midi -o "FluidSynth"         # Connect by name (substring match)
```

**Option C: Built-in synth (no external software needed)**

```bash
./build/alda_midi -sf /path/to/gm.sf2     # Direct audio output
```

Requires a SoundFont file. See [Getting a SoundFont](#getting-a-soundfont) below.

### 3. Play some notes

```
alda> piano:
alda> c d e f g
alda> c/e/g
```

## Usage Modes

### Interactive REPL

```bash
./build/alda_midi           # Start REPL (concurrent mode, auto port)
./build/alda_midi -s        # Start in sequential mode
```

Type `help` for commands, `quit` or Ctrl-D to exit.

### Play a File

```bash
./build/alda_midi song.alda
./build/alda_midi -v song.alda    # Verbose output
```

### CLI Options

| Option | Description |
|--------|-------------|
| `-h, --help` | Show help message |
| `-v, --verbose` | Enable verbose output |
| `-l, --list` | List available MIDI ports |
| `-p, --port N` | Use hardware port by index |
| `-o, --output NAME` | Use port matching name |
| `--virtual NAME` | Create virtual port with name |
| `--no-sleep` | Disable timing (for testing) |
| `-s, --sequential` | Use sequential playback mode |
| `-sf, --soundfont PATH` | Use built-in synth with soundfont |

By default, alda-midi connects to the first available MIDI port (or creates a virtual port if none exist) and uses concurrent mode for polyphonic playback.

Use `-sf` with a SoundFont file to use the built-in synthesizer instead of MIDI output.

## REPL Commands

| Command | Description |
|---------|-------------|
| `help` | Show help |
| `quit`, `exit` | Exit the REPL |
| `list` | List MIDI ports |
| `stop` | Stop current playback |
| `panic` | All notes off |
| `sequential` | Enable sequential mode (wait for each input) |
| `concurrent` | Enable concurrent mode (default, polyphony) |
| `sf-load PATH` | Load soundfont and switch to built-in synth |
| `sf-list` | List soundfont presets |
| `midi` | Switch to MIDI output |
| `builtin` | Switch back to built-in synth |

## Example Programs

### Simple Scale

```alda
piano:
  c d e f g a b > c
```

### With Tempo and Dynamics

```alda
piano:
  (tempo 120)
  (mf)
  o4 c4 d e f | g2 g | a4 a a a | g2.
```

### Chord Progression

```alda
piano:
  c4/e/g c/e/g | f/a/c f/a/c | g/b/d g/b/d | c1/e/g
```

### Multiple Instruments

```alda
piano:
  o4 c8 d e f g a b > c

violin:
  o5 c2 g | e1

cello:
  o3 c1 | g1
```

### Voices (Polyphony)

```alda
piano:
  V1: c1 d e f
  V2: e1 f g a
  V0: c1/e/g
```

## Playback Modes

### Concurrent Mode (Default)

Each REPL input plays immediately without waiting for previous playback to finish. This allows polyphonic layering:

```
alda> piano: c1 d e f g a b > c
alda> violin: e2 f g a b > c d e    # Plays alongside piano
alda> cello: c1 g c g               # Adds bass layer
```

Each instrument is assigned a different MIDI channel automatically, enabling true polyphony when connected to a GM-compatible synthesizer.

### Sequential Mode

Use `-s` or the `sequential` command to wait for each input to finish before accepting the next:

```bash
./build/alda_midi -s
```

This is useful for debugging or when you want to hear each part in isolation.

## Alda Language Quick Reference

### Notes

```alda
c d e f g a b      # Natural notes
c# d# f# g# a#     # Sharps
db eb gb ab bb     # Flats
c4 d8 e16          # Durations: 4=quarter, 8=eighth, 16=sixteenth
c4. c4..           # Dotted notes
c4~4               # Tied notes
```

### Octaves

```alda
o4 c d e           # Set octave to 4
> c                # Octave up
< c                # Octave down
```

### Chords and Rests

```alda
c/e/g              # Chord (simultaneous notes)
r4                 # Quarter rest
r2.                # Dotted half rest
```

### Attributes

```alda
(tempo 140)        # Set tempo in BPM
(volume 80)        # Set volume 0-100
(quant 90)         # Set quantization 0-100
(panning 64)       # Set pan 0-127 (64=center)
(pp) (p) (mp) (mf) (f) (ff) (fff)   # Dynamics
```

### Parts

```alda
piano:             # Declare piano part
violin:            # Declare violin part
piano/violin:      # Apply events to both parts
```

### Voices

```alda
V1: c d e f        # Voice 1
V2: e f g a        # Voice 2 (independent timing)
V0: c1             # Merge voices (continue at max tick)
```

## Documentation

- [Language Reference](language-reference.md) - Complete syntax documentation
- [Examples](examples.md) - More code examples
- [Alda Language Guide](alda-language/README.md) - Comprehensive Alda documentation

## Requirements

- Built project (`make` must complete successfully)
- macOS (CoreAudio), Linux (ALSA), or Windows (WinMM)

For audio output, use one of:
- **Built-in synth**: Requires a SoundFont (.sf2) file
- **MIDI output**: Requires an external synth (FluidSynth, DAW, hardware)

## Built-in Synthesizer

alda-midi includes a built-in synthesizer powered by TinySoundFont and miniaudio. This allows audio playback without any external software.

### Using the Built-in Synth

```bash
# Play a file with built-in synth
./build/alda_midi -sf /path/to/soundfont.sf2 song.alda

# Interactive REPL with built-in synth
./build/alda_midi -sf /path/to/soundfont.sf2
```

### Getting a SoundFont

Download a General MIDI SoundFont:
- [FluidR3_GM.sf2](https://musical-artifacts.com/artifacts/738) (~140MB, high quality)
- [GeneralUser_GS.sf2](https://schristiancollins.com/generaluser.php) (~30MB, good quality)
- [Timbres of Heaven](https://midkar.com/soundfonts/) (~300MB, orchestral focus)

### Switching Between Modes in REPL

```
alda> sf-load /path/to/soundfont.sf2    # Load SF and switch to built-in synth
alda> piano: c d e f g                   # Plays through speakers
alda> midi                               # Switch to MIDI output
alda> builtin                            # Switch back to built-in synth
alda> sf-list                            # List available presets
```

## MIDI Playback with FluidSynth

alda-midi creates a virtual MIDI port. To hear sound, connect it to a synthesizer.

### Quick Setup

```bash
# Terminal 1: Start FluidSynth
export ALDA_SF2_DIR=~/Music/sf2    # Set your SoundFont directory
python scripts/fluidsynth-gm.py

# Terminal 2: Run alda-midi
./build/alda_midi
```

### Install FluidSynth

| Platform | Command |
|----------|---------|
| macOS | `brew install fluidsynth` |
| Ubuntu/Debian | `sudo apt install fluidsynth` |
| Fedora | `sudo dnf install fluidsynth` |

Download a GM SoundFont (e.g., [FluidR3_GM.sf2](https://musical-artifacts.com/artifacts/738)) and place it in your `ALDA_SF2_DIR`.

A convenient python3 script is available to launch fluidsynth with a soundfont:

```bash
python scripts/fluidsynth-gm.py --help
python scripts/fluidsynth-gm.py --list          # List available SoundFonts
python scripts/fluidsynth-gm.py -g 0.5          # Lower gain
python scripts/fluidsynth-gm.py MySoundFont.sf2 # Use specific SoundFont
```

### Other Playback Options

- **DAW**: Route the "AldaMIDI" virtual port to Logic, Ableton, Reaper, etc.
- **Hardware**: Connect to GM-compatible synthesizers
- **Software synths**: Any virtual instrument accepting MIDI input
