# mhs-midi Examples

## MidiPerform Examples (Immediate IO)

### Basic Playback

```haskell
module Basic(main) where
import MidiPerform

main :: IO ()
main = do
    open
    note c4
    note e4
    note g4
    chord [c4, e4, g4]
    close
```

### Melody and Chords

```haskell
module MelodyChords(main) where
import MidiPerform

main :: IO ()
main = do
    open
    melody [c4, d4, e4, f4, g4]
    rest quarter
    chord [c4, e4, g4]
    chord [f4, a4, c5]
    chord [g4, b4, d5]
    chord [c4, e4, g4]
    close
```

### Using noteWith for Control

```haskell
module NoteWith(main) where
import MidiPerform

-- Pitch-last allows partial application
loud = noteWith 1 fff quarter
soft = noteWith 1 pp half
fast = noteWith 1 mf sixteenth

main :: IO ()
main = do
    open
    mapM_ loud [c4, e4, g4]
    mapM_ soft [c5, g4, e4, c4]
    mapM_ fast [c4, d4, e4, f4, g4, a4, b4, c5]
    close
```

---

## Generative Examples

### Random Selection

```haskell
module RandomPick(main) where
import MidiPerform

main :: IO ()
main = do
    open
    seed 42  -- reproducible randomness

    -- Pick random notes from pentatonic
    times 16 (pick (pentatonic c4))

    rest quarter

    -- 75% chance to play each note
    times 8 (chance 75 (note c4) >> rest eighth)

    close
```

### Drunk Walk

```haskell
module DrunkWalk(main) where
import MidiPerform

main :: IO ()
main = do
    open

    -- Drunk walk on C major, max 2 scale degrees
    drunk 16 c4 (major c4) 2

    rest quarter

    -- Drunk walk on A minor pentatonic
    drunk 16 a4 (pentatonic a4) 1

    close
```

### Random Walk

```haskell
module RandomWalk(main) where
import MidiPerform

main :: IO ()
main = do
    open

    -- Random walk, max 3 semitone steps
    walk 16 c4 3

    rest quarter

    -- Bigger steps, starting from g4
    walk 16 g4 5

    close
```

### Euclidean Rhythms

```haskell
module Euclidean(main) where
import MidiPerform

main :: IO ()
main = do
    open

    -- 3 hits over 8 steps (tresillo)
    times 2 (euclidean 3 8 (note c4))

    rest quarter

    -- 5 over 8 (cinquillo)
    times 2 (euclidean 5 8 (note c4))

    rest quarter

    -- 7 over 16 with chord
    euclidean 7 16 (chord [c4, e4, g4])

    close
```

### Scramble

```haskell
module Scramble(main) where
import MidiPerform

main :: IO ()
main = do
    open

    -- Play C major chord tones in random order
    times 4 (scramble [c4, e4, g4, c5])

    rest quarter

    -- Scramble a pentatonic scale
    scramble (pentatonic c4)

    close
```

### Combined Generative

```haskell
module GenerativeCombo(main) where
import MidiPerform

main :: IO ()
main = do
    open
    seed 123

    -- Alternate between two patterns randomly
    times 8 $ oneOf
        [ drunk 4 c4 (major c4) 2
        , arpeggio [c4, e4, g4, c5]
        ]

    rest quarter

    -- Maybe play a chord between melody notes
    times 8 $ do
        pick (pentatonic c4)
        maybeDo (chord [c3, g3])

    close
```

---

## MusicPerform Examples (Pure Composition)

### Basic Composition

```haskell
module BasicComposition(main) where
import MusicPerform

-- Pure music values
melody1 = line [c4, e4, g4] mf quarter
melody2 = line [g4, e4, c4] mf quarter
piece = melody1 +:+ melody2

main :: IO ()
main = do
    midiOpenVirtual "BasicComposition"
    perform piece
    midiClose
```

### Parallel Composition

```haskell
module Parallel(main) where
import MusicPerform

melody = line [c4, d4, e4, f4, g4] mf quarter
bass = line [c3, g3, c3, g3, c3] ff half

-- Play melody and bass together
piece = melody ||| bass

main :: IO ()
main = do
    midiOpenVirtual "Parallel"
    perform piece
    midiClose
```

### Transformations

```haskell
module Transformations(main) where
import MusicPerform

theme = line [c4, e4, g4, e4] mf quarter

-- Apply transformations
variation1 = transpose 7 theme           -- up a fifth
variation2 = louder 20 theme             -- louder
variation3 = stretch 2 theme             -- twice as slow
variation4 = compress 2 (softer 30 theme) -- fast and quiet

piece = theme +:+ variation1 +:+ variation2 +:+ variation3 +:+ variation4

main :: IO ()
main = do
    midiOpenVirtual "Transformations"
    perform piece
    midiClose
```

### Chords and Lines

```haskell
module ChordsLines(main) where
import MusicPerform

-- I-IV-V-I progression
cMaj = chord [c4, e4, g4] mf half
fMaj = chord [f4, a4, c5] mf half
gMaj = chord [g4, b4, d5] mf half

progression = cMaj +:+ fMaj +:+ gMaj +:+ chord [c4, e4, g4] mf whole

-- Add a melody on top
melody = line [c5, b4, a4, g4, f4, e4, d4, c4] mp eighth

piece = progression ||| (rest whole +:+ melody)

main :: IO ()
main = do
    midiOpenVirtual "ChordsLines"
    perform piece
    midiClose
```

### Using timesM

```haskell
module TimesM(main) where
import MusicPerform

pattern = line [c4, e4, g4, e4] mf sixteenth
bassNote = note c3 ff quarter

-- Repeat pattern 4 times over a bass note
piece = timesM 4 pattern ||| timesM 4 bassNote

main :: IO ()
main = do
    midiOpenVirtual "TimesM"
    perform piece
    midiClose
```

### Building from Scale Degrees

```haskell
module ScaleDegrees(main) where
import MusicPerform

-- Build I, IV, V chords from scale degrees
root = c4
scale = scaleMajor

i  = chord [scaleDegree root scale 1, scaleDegree root scale 3, scaleDegree root scale 5] mf half
iv = chord [scaleDegree root scale 4, scaleDegree root scale 6, scaleDegree root scale 8] mf half
v  = chord [scaleDegree root scale 5, scaleDegree root scale 7, scaleDegree root scale 9] mf half

progression = i +:+ iv +:+ v +:+ i

main :: IO ()
main = do
    midiOpenVirtual "ScaleDegrees"
    perform progression
    perform (transpose 5 progression)  -- transpose to F
    midiClose
```

---

## REPL Session Examples

Start the REPL:

```bash
./scripts/mhs-midi-repl
```

### Quick Test with MidiPerform

```haskell
> import MidiPerform
> open
MIDI open
> note c4
> chord [c4, e4, g4]
> melody [c4, d4, e4, f4, g4]
> close
```

### Generative Session

```haskell
> import MidiPerform
> open
MIDI open
> seed 42
> times 8 (pick (pentatonic c4))
> drunk 16 c4 (major c4) 2
> euclidean 5 8 (note c4)
> close
```

### Pure Composition in REPL

```haskell
> import MusicPerform
> midiOpenVirtual "repl"
> let m = line [c4, e4, g4] mf quarter
> perform m
> perform (transpose 7 m)
> perform (stretch 2 m)
> midiClose
```

### List MIDI Ports

```haskell
> import MidiPerform
> ports
2 MIDI port(s):
  0: IAC Driver Bus 1
  1: USB MIDI Device
```

---

## Pure Generative Examples (Music.hs)

These functions are pure and use explicit seeds for reproducibility.

### Euclidean Rhythms (Pure)

```haskell
module EuclideanPure(main) where
import MusicPerform

-- Generate rhythm pattern: 3 hits over 8 steps
pattern = euclideanRhythm 3 8  -- [True,False,False,True,False,False,True,False]

-- Convert to music
rhythmToMusic :: [Bool] -> Pitch -> Velocity -> Duration -> Music
rhythmToMusic bs p v d = MSeq [if b then note p v d else rest d | b <- bs]

main :: IO ()
main = do
    midiOpenVirtual "EuclideanPure"
    perform $ rhythmToMusic (euclideanRhythm 3 8) c4 mf eighth
    perform $ rhythmToMusic (euclideanRhythm 5 8) e4 mf eighth
    midiClose
```

### Arpeggio Patterns

```haskell
module ArpeggioPatterns(main) where
import MusicPerform

cMaj = [c4, e4, g4, c5]

main :: IO ()
main = do
    midiOpenVirtual "ArpeggioPatterns"

    -- Ascending
    perform $ line (arpUp cMaj) mf sixteenth

    -- Descending
    perform $ line (arpDown cMaj) mf sixteenth

    -- Up-down (no repeated top)
    perform $ line (arpUpDown cMaj) mf sixteenth

    midiClose
```

### Retrograde and Inversion

```haskell
module Transformations(main) where
import MusicPerform

theme = line [c4, d4, e4, g4] mf quarter

main :: IO ()
main = do
    midiOpenVirtual "Transformations"

    -- Original
    perform theme

    -- Retrograde (reversed)
    perform (retrograde theme)

    -- Inversion around E4 (64)
    perform (invert e4 theme)

    -- Combined: retrograde inversion
    perform (retrograde (invert e4 theme))

    midiClose
```

### Seed-based Random Walk

```haskell
module PureWalk(main) where
import MusicPerform

-- Same seed always produces same result
walkPitches = randomWalk 42 c4 3 16  -- seed, start, maxStep, count

main :: IO ()
main = do
    midiOpenVirtual "PureWalk"
    perform $ line walkPitches mf eighth
    -- Play again - same melody
    perform $ line walkPitches mf eighth
    midiClose
```

### Seed-based Drunk Walk

```haskell
module PureDrunk(main) where
import MusicPerform

scale = buildScale c4 scalePentatonic

-- Drunk walk constrained to scale
drunkPitches = drunkWalk 123 c4 scale 2 16  -- seed, start, scale, maxDegrees, count

main :: IO ()
main = do
    midiOpenVirtual "PureDrunk"
    perform $ line drunkPitches mf eighth
    midiClose
```

### Shuffle

```haskell
module ShufflePure(main) where
import MusicPerform

pitches = [c4, e4, g4, b4]

main :: IO ()
main = do
    midiOpenVirtual "ShufflePure"

    -- Original order
    perform $ line pitches mf quarter

    -- Shuffled (seed 1)
    perform $ line (shuffle 1 pitches) mf quarter

    -- Shuffled (seed 2) - different order
    perform $ line (shuffle 2 pitches) mf quarter

    -- Same seed = same order
    perform $ line (shuffle 1 pitches) mf quarter

    midiClose
```

---

## Advanced Examples

### Multi-Channel

```haskell
module MultiChannel(main) where
import MusicPerform

-- Perform on different channels
main :: IO ()
main = do
    midiOpenVirtual "MultiChannel"

    -- Set instruments
    midiProgram 1 0   -- Piano
    midiProgram 2 32  -- Bass

    let melody = line [c4, e4, g4, c5] mf quarter
    let bass = line [c2, g2] ff half

    -- Perform on different channels sequentially
    performOn 1 melody
    performOn 2 bass

    midiClose
```

### Microtonal Maqam

```haskell
module Maqam(main) where
import MusicPerform

playMicroNote :: Pitch -> Int -> IO ()
playMicroNote root cents = do
    let (n, bend) = centsToNote root cents
    pitchBendCents 1 bend
    midiNoteOn 1 n mf
    midiSleep quarter
    midiNoteOff 1 n

main :: IO ()
main = do
    midiOpenVirtual "Maqam"

    -- Play Maqam Bayati with quarter-tones
    mapM_ (playMicroNote d4) scaleMaqamBayatiCents

    -- Reset pitch bend
    pitchBendCents 1 0

    midiClose
```

### Control Changes

```haskell
module ControlChanges(main) where
import MidiPerform

main :: IO ()
main = do
    open

    -- Set volume
    cc 1 7 100

    -- Enable sustain
    cc 1 64 127

    -- Play with sustain
    note c4
    note e4
    note g4

    -- Release sustain
    cc 1 64 0

    rest quarter
    close
```
