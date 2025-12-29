# API Reference

## MidiRepl (For REPL)

```haskell
import MidiRepl
```

REPL-friendly functions that all return `IO ()` (no Show constraint errors). Re-exports Midi and adds pitch-last functions for partial application.

### note

```haskell
note :: Channel -> Velocity -> Duration -> Pitch -> IO ()
```

Play a single note. Pitch is last for partial application.

```haskell
note 1 mf quarter c4

-- Partial application
loud = note 1 fff quarter
soft = note 1 pp half
loud c4
mapM_ soft [c4, e4, g4]
```

### notes

```haskell
notes :: Channel -> Velocity -> Duration -> [Pitch] -> IO ()
```

Play a chord. Pitches last for partial application.

```haskell
notes 1 mf quarter [c4, e4, g4]

-- Partial application
bigChord = notes 1 ff whole
bigChord [c3, g3, c4, e4, g4]
```

### n

```haskell
n :: Pitch -> IO ()
```

Default note: channel 1, mf velocity, quarter duration.

```haskell
n c4
mapM_ n [c4, e4, g4]
```

### ch

```haskell
ch :: [Pitch] -> IO ()
```

Default chord: channel 1, mf velocity, quarter duration.

```haskell
ch [c4, e4, g4]
```

### open / close / panic / ports

```haskell
open  :: IO ()  -- opens virtual port "MicroHs", prints status
close :: IO ()  -- closes MIDI port
panic :: IO ()  -- all notes off
ports :: IO ()  -- lists available MIDI ports
```

All return `IO ()` for REPL friendliness.

```haskell
> open
MIDI open
> n c4
> ports
2 MIDI port(s):
  0: IAC Driver Bus 1
  1: USB MIDI Device
> close
```

---

## Midi Module

```haskell
import Midi
```

---

## Initialization & Port Management

### midiInit

```haskell
midiInit :: IO Bool
```

Initialize the MIDI system. Returns `True` on success.

### midiCleanup

```haskell
midiCleanup :: IO ()
```

Cleanup the MIDI system and release resources.

### midiListPorts

```haskell
midiListPorts :: IO Int
```

List available MIDI output ports. Returns the count of available ports.

### midiPortName

```haskell
midiPortName :: Int -> IO String
```

Get the name of a MIDI port by index.

```haskell
n <- midiListPorts
names <- mapM midiPortName [0..n-1]
```

### midiOpen

```haskell
midiOpen :: Int -> IO Bool
```

Open a hardware MIDI port by index. Returns `True` on success.

### midiOpenVirtual

```haskell
midiOpenVirtual :: String -> IO Bool
```

Create and open a virtual MIDI port with the given name. Other applications (DAWs, synths) can connect to this port.

```haskell
midiOpenVirtual "MyApp"
```

### midiClose

```haskell
midiClose :: IO ()
```

Close the current MIDI port.

### midiIsOpen

```haskell
midiIsOpen :: IO Bool
```

Check if a MIDI port is currently open.

---

## Low-Level MIDI Operations

### midiNoteOn

```haskell
midiNoteOn :: Int -> Int -> Int -> IO ()
midiNoteOn channel pitch velocity
```

Send a Note On message.

- `channel`: 1-16
- `pitch`: 0-127 (60 = middle C)
- `velocity`: 0-127

### midiNoteOff

```haskell
midiNoteOff :: Int -> Int -> IO ()
midiNoteOff channel pitch
```

Send a Note Off message.

### midiCC

```haskell
midiCC :: Int -> Int -> Int -> IO ()
midiCC channel controller value
```

Send a Control Change message.

- `controller`: 0-127 (e.g., 1 = mod wheel, 7 = volume, 64 = sustain)
- `value`: 0-127

### midiProgram

```haskell
midiProgram :: Int -> Int -> IO ()
midiProgram channel program
```

Send a Program Change message.

- `program`: 0-127

### midiPitchBend

```haskell
midiPitchBend :: Int -> Int -> IO ()
midiPitchBend channel value
```

Send a Pitch Bend message.

- `value`: -8192 to 8191 (0 = center)

### midiSend

```haskell
midiSend :: Int -> Int -> Int -> IO ()
midiSend status data1 data2
```

Send a raw 3-byte MIDI message.

### midiSleep

```haskell
midiSleep :: Int -> IO ()
```

Sleep for the given number of milliseconds.

### midiPanic

```haskell
midiPanic :: IO ()
```

Send All Notes Off on all channels. Use to silence stuck notes.

---

## Types

```haskell
type Pitch    = Int   -- MIDI note number (0-127)
type Duration = Int   -- milliseconds
type Velocity = Int   -- 0-127
type Channel  = Int   -- 1-16
```

---

## Pitch Names

### Pitch Constants

All pitches follow the pattern `<note><octave>` where:

- Note: `c`, `d`, `e`, `f`, `g`, `a`, `b`
- Sharps: `cs`, `ds`, `fs`, `gs`, `as`
- Octave: 0-8

Middle C (MIDI 60) is `c4`.

| Pitch | MIDI | Pitch | MIDI | Pitch | MIDI |
|-------|------|-------|------|-------|------|
| c4    | 60   | d4    | 62   | e4    | 64   |
| f4    | 65   | g4    | 67   | a4    | 69   |
| b4    | 71   | c5    | 72   | cs4   | 61   |

### Flat Aliases

```haskell
db = cs   -- D flat = C sharp
eb = ds   -- E flat = D sharp
gb = fs   -- G flat = F sharp
ab = gs   -- A flat = G sharp
bb = as   -- B flat = A sharp
```

### Pitch Class (Semitone Offset)

For building chords and scales:

```haskell
c, cs, d, ds, e, f, fs, g, gs, a, as, b :: Int
-- c = 0, cs = 1, d = 2, ... b = 11
```

---

## Durations

Based on 120 BPM by default.

| Constant    | Milliseconds | Musical Value |
|-------------|--------------|---------------|
| `whole`     | 2000         | Whole note    |
| `half`      | 1000         | Half note     |
| `quarter`   | 500          | Quarter note  |
| `eighth`    | 250          | Eighth note   |
| `sixteenth` | 125          | 16th note     |

### dotted

```haskell
dotted :: Duration -> Duration
```

Returns 1.5x the given duration.

```haskell
dotted quarter  -- 750ms
dotted half     -- 1500ms
```

### bpm

```haskell
bpm :: Int -> Duration
```

Calculate quarter note duration for a given tempo.

```haskell
bpm 120  -- 500ms
bpm 60   -- 1000ms
bpm 140  -- ~428ms
```

---

## Velocities (Dynamics)

| Constant | Value | Dynamic        |
|----------|-------|----------------|
| `ppp`    | 16    | Pianississimo  |
| `pp`     | 33    | Pianissimo     |
| `p`      | 49    | Piano          |
| `mp`     | 64    | Mezzo-piano    |
| `mf`     | 80    | Mezzo-forte    |
| `ff`     | 96    | Fortissimo     |
| `fff`    | 112   | Fortississimo  |

---

## High-Level Note Playing

### play

```haskell
play :: Pitch -> Velocity -> Duration -> IO ()
```

Play a single note with specified velocity and duration on the default channel.

```haskell
play c4 mf quarter
play g4 ff half
```

### playNote

```haskell
playNote :: Pitch -> Duration -> IO ()
```

Play a note with default velocity (`mf`).

```haskell
playNote c4 quarter
playNote e4 eighth
```

### playChord

```haskell
playChord :: [Pitch] -> Velocity -> Duration -> IO ()
```

Play multiple notes simultaneously.

```haskell
playChord [c4, e4, g4] mf half      -- C major chord
playChord [a4, c5, e5] ff quarter   -- A minor chord
```

### rest

```haskell
rest :: Duration -> IO ()
```

Silent pause for the given duration.

```haskell
rest quarter
rest (dotted half)
```

---

## Sequences

### melody

```haskell
melody :: [(Pitch, Duration)] -> Velocity -> IO ()
```

Play a sequence of notes.

```haskell
melody [(c4, quarter), (d4, quarter), (e4, half)] mf
```

### chord

```haskell
chord :: [Pitch] -> Duration -> IO ()
```

Play a chord with default velocity (`mf`).

```haskell
chord [c4, e4, g4] half
```

### arpeggio

```haskell
arpeggio :: [Pitch] -> Duration -> Velocity -> IO ()
```

Play notes one after another (arpeggiated).

```haskell
arpeggio [c4, e4, g4, c5] eighth mf
```

### times

```haskell
times :: Int -> IO () -> IO ()
```

Repeat an action n times.

```haskell
times 4 (playNote c4 quarter)
times 2 (chord [c4, e4, g4] half)
```

---

## Channel Management

### defaultChannel

```haskell
defaultChannel :: Channel  -- = 1
```

The default MIDI channel used by high-level functions.

### withChannel

```haskell
withChannel :: Channel -> (Channel -> IO ()) -> IO ()
```

Execute an action with a specific channel.

```haskell
withChannel 2 (\ch -> midiNoteOn ch 60 100)
```

---

## Scales

### Scale Type

```haskell
type Scale = [Int]  -- List of semitone intervals from root
```

### buildScale

```haskell
buildScale :: Pitch -> Scale -> [Pitch]
```

Build a scale from root pitch and intervals.

```haskell
buildScale c4 scaleMajor    -- [60, 62, 64, 65, 67, 69, 71]
buildScale d4 scaleDorian   -- D dorian scale
```

### scaleDegree

```haskell
scaleDegree :: Pitch -> Scale -> Int -> Pitch
```

Get the nth degree of a scale (1-based). Supports extended degrees.

```haskell
scaleDegree c4 scaleMajor 1   -- 60 (root)
scaleDegree c4 scaleMajor 3   -- 64 (major third)
scaleDegree c4 scaleMajor 5   -- 67 (perfect fifth)
scaleDegree c4 scaleMajor 9   -- 74 (ninth = octave + 2nd)
```

### inScale

```haskell
inScale :: Pitch -> Pitch -> Scale -> Bool
```

Check if a pitch belongs to a scale (in any octave).

```haskell
inScale e4 c4 scaleMajor   -- True (E is in C major)
inScale cs4 c4 scaleMajor  -- False (C# is not in C major)
```

### quantize

```haskell
quantize :: Pitch -> Pitch -> Scale -> Pitch
```

Quantize a pitch to the nearest note in a scale.

```haskell
quantize 63 c4 scaleMajor  -- 62 (D# quantizes to D)
quantize 66 c4 scaleMajor  -- 67 (F# quantizes to G)
```

---

## Scale Constants

All scales are lists of semitone intervals from the root.

### Diatonic Modes

| Constant | Intervals |
|----------|-----------|
| `scaleMajor` | [0, 2, 4, 5, 7, 9, 11] |
| `scaleMinor` | [0, 2, 3, 5, 7, 8, 10] |
| `scaleDorian` | [0, 2, 3, 5, 7, 9, 10] |
| `scalePhrygian` | [0, 1, 3, 5, 7, 8, 10] |
| `scaleLydian` | [0, 2, 4, 6, 7, 9, 11] |
| `scaleMixolydian` | [0, 2, 4, 5, 7, 9, 10] |
| `scaleLocrian` | [0, 1, 3, 5, 6, 8, 10] |
| `scaleIonian` | Same as major |
| `scaleAeolian` | Same as minor |

### Minor Variants

| Constant | Description |
|----------|-------------|
| `scaleHarmonicMinor` | Raised 7th |
| `scaleMelodicMinor` | Raised 6th and 7th |
| `scaleHarmonicMajor` | Lowered 6th |

### Pentatonic & Blues

| Constant | Description |
|----------|-------------|
| `scalePentatonic` | Major pentatonic |
| `scalePentatonicMajor` | Major pentatonic |
| `scalePentatonicMinor` | Minor pentatonic |
| `scaleBlues` | Blues scale |
| `scaleBluesMajor` | Major blues |

### Symmetric

| Constant | Description |
|----------|-------------|
| `scaleWholeTone` | Whole-tone scale |
| `scaleChromatic` | All 12 semitones |
| `scaleDiminished` | Whole-half diminished |
| `scaleAugmented` | Augmented scale |

### Bebop

| Constant | Description |
|----------|-------------|
| `scaleBebopDominant` | Dominant with passing tone |
| `scaleBebopMajor` | Major with passing tone |
| `scaleBebopMinor` | Minor with passing tone |

### World Scales

| Constant | Description |
|----------|-------------|
| `scaleHungarianMinor` | Hungarian minor |
| `scaleDoubleHarmonic` | Byzantine/Arabic |
| `scaleGypsy` | Hungarian Gypsy |
| `scaleHirajoshi` | Japanese |
| `scaleInSen` | Japanese In-Sen |
| `scalePersian` | Persian scale |

### Arabic Maqamat (12-TET)

| Constant | Description |
|----------|-------------|
| `scaleMaqamHijaz` | Maqam Hijaz |
| `scaleMaqamNahawand` | Maqam Nahawand |
| `scaleMaqamNikriz` | Maqam Nikriz |

### Indian Ragas (12-TET)

| Constant | Description |
|----------|-------------|
| `scaleRagaBhairav` | Raga Bhairav |
| `scaleRagaTodi` | Raga Todi |
| `scaleRagaMarwa` | Raga Marwa |

---

## Microtonal

### centsToBend

```haskell
centsToBend :: Int -> IO Int
```

Convert cents offset to MIDI pitch bend value (-8192 to 8191).

```haskell
bend <- centsToBend 50   -- Quarter-tone up
midiPitchBend 1 bend
```

### centsToNote

```haskell
centsToNote :: Pitch -> Int -> (Pitch, Int)
```

Convert a cents interval to (MIDI note, bend cents).

```haskell
centsToNote c4 150   -- (61, 50) - C# minus 50 cents
centsToNote c4 350   -- (63, 50) - Eb plus 50 cents
```

### pitchBendCents

```haskell
pitchBendCents :: Channel -> Int -> IO ()
```

Send pitch bend in cents.

```haskell
pitchBendCents 1 50     -- Bend up quarter-tone
pitchBendCents 1 (-50)  -- Bend down quarter-tone
pitchBendCents 1 0      -- Reset to center
```

---

## Microtonal Scale Constants

Cents-based scales for quarter-tones and other microtonal intervals.

### Arabic Maqamat (Authentic)

| Constant | Description |
|----------|-------------|
| `scaleMaqamBayatiCents` | Bayati with 3/4 tones |
| `scaleMaqamRastCents` | Rast with 3/4 tones |
| `scaleMaqamSabaCents` | Saba |
| `scaleMaqamSikahCents` | Sikah |
| `scaleMaqamHuzamCents` | Huzam |

### Turkish Makamlar

| Constant | Description |
|----------|-------------|
| `scaleMakamUssakCents` | Makam Ussak |
| `scaleMakamHuseyniCents` | Makam Huseyni |

### Indian

| Constant | Description |
|----------|-------------|
| `scaleShrutiCents` | 22-shruti scale |

### Microtonal Playback Example

```haskell
playMaqam :: IO ()
playMaqam = do
    midiOpenVirtual "Maqam"
    mapM_ playMicroNote scaleMaqamBayatiCents
    midiClose
  where
    root = c4
    playMicroNote cents = do
        let (note, bend) = centsToNote root cents
        pitchBendCents 1 bend
        play note mf quarter
```
