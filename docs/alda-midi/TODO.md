# alda-midi TODO

## Shared Test Suite Status

**20/20 tests passing** - All shared test suite tests now pass.

---

## Implemented Features

### Core Features
- Notes, rests, chords (`c/e/g`)
- Durations (`c4`, `c8.`, ties `c4~4`)
- Octaves (`o4`, `>`, `<`)
- Dynamics (`(ppp)` through `(fff)`)
- Tempo (`(tempo 120)`)
- Volume/Quantization (`(volume 80)`, `(quant 90)`)
- Panning (`(panning 64)`)
- Parts/Instruments (128 GM instruments with aliases)
- Voices (`V1:`, `V2:`, `V0:`)
- Simple repeats (`[c d e]*4`)
- Barlines (`|`) - visual only
- Comments (`# ...`)
- Program changes (instrument switching)
- Part aliases (`piano "melody":`)
- Variables (`motif = c d e f`, `piano: motif`)
- Cram expressions (`{c d e f g}2` - fit notes into duration)
- Multi-instrument parts (`violin/viola/cello:`)

### Recently Implemented
- **Key Signatures**: `(key-sig '(g major))`, `(key-sig "f+ c+")`
  - Supports major, minor, and modal scales
  - Natural sign `_` overrides key signature
- **Transposition**: `(transpose 5)`, `(transpose -7)`
  - Applied after key signature calculation
- **Markers**: `%marker-name` and `@marker-name`
  - Synchronize parts at specific positions
- **On-Repetitions**: `'1`, `'1,3`, `'1-3`, `'1,3-5,7`
  - Conditional playback within repeat blocks
  - Enables alternate endings

---

## Potential Future Enhancements

### Barline Timing (Low Priority)
**Why:** Currently barlines are visual-only. Could optionally enforce timing alignment.

**Current behavior:** `|` is parsed and ignored.

**Potential enhancement:** Add strict mode that validates measure boundaries.

---

### MIDI File Export (Low Priority)
**Why:** Convenience for sharing compositions.

**Current:** Can output to MIDI ports.

**Enhancement:** Add `--midi-out file.mid` option to write Standard MIDI File.

---

## Testing Notes

All tests in the shared test suite pass:
- `01_notes_basic.alda` - Basic note playback
- `02_notes_accidentals.alda` - Sharps and flats
- `03_notes_durations.alda` - Note lengths
- `04_octaves.alda` - Octave changes
- `05_rests.alda` - Rest handling
- `06_chords.alda` - Chord notation
- `07_ties.alda` - Tied notes and slurs
- `08_tempo.alda` - Tempo changes
- `09_volume.alda` - Volume control
- `10_dynamics.alda` - Dynamic markings
- `11_parts.alda` - Part declarations and multi-instrument groups
- `12_variables.alda` - Variable definitions
- `13_markers.alda` - Marker synchronization
- `14_voices.alda` - Voice polyphony
- `15_repeats.alda` - Repeats with on-repetitions
- `16_cram.alda` - Cram expressions (tuplets)
- `17_key_signature.alda` - Key signatures
- `18_transpose.alda` - Transposition
- `19_quantization.alda` - Quantization/articulation
- `20_panning.alda` - Pan control
