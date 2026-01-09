# TODO

## Sequencer / Timing

- [ ] Timeline-based sequencing (more intuitive for complex compositions)
- [ ] Non-blocking sequences with absolute timing
- [ ] Tempo changes mid-sequence (`bpm` word)
- [ ] Swing/shuffle timing modifier
- [ ] Polyrhythms (independent time divisions)

## Patterns

- [ ] Named pattern storage (`pattern! pattern@`)
- [ ] Pattern transformations (reverse, invert, retrograde)
- [x] Euclidean rhythms (`euclidean` word)
- [ ] Markov chains for generative sequences

## Scales / Harmony

- [x] Scale definitions (major, minor, modes) - 49 scales via `scale-*` words
- [x] Scale-constrained random (`quantize` word snaps to scale)
- [ ] Chord symbols (`Cmaj7`, `Dm`, `G7`)
- [ ] Automatic voice leading

## MIDI Input

- [x] MIDI input handling - polling-based input with message queue
  - `midi-input-list`, `midi-input-open`, `midi-input-virtual`, `midi-input-close`
  - `midi-input?` (check pending), `midi-input@` (read message), `midi-input-flush`
  - Lock-free ring buffer for callback thread safety
- [ ] MIDI input handlers (note triggers) - event-driven callbacks
- [ ] Live looping / overdub
- [ ] MIDI learn for CC mapping

## Control Flow

- [x] `do ... loop` counted loops - with `i`, `j`, `+loop`, `leave`
- [x] `begin ... until` / `begin ... while ... repeat`
- [ ] Variables (`variable`, `!`, `@`)
- [ ] Local variables in word definitions

## File I/O

- [ ] Load/save word definitions
- [ ] MIDI file export
- [ ] Import patterns from files
