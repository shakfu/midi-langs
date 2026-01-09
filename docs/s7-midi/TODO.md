# s7-midi TODO

## Async Scheduler

- [x] Thunk-based concurrent voices with `spawn`/`run`
- [x] Voice builders: `make-note-voice`, `make-melody-voice`, `make-chord-voice`
- [x] Repeat/loop helpers: `make-repeat-voice`, `make-loop-voice`
- [x] Voice control: `stop`, `voices`, `scheduler-status`
- [x] Async convenience: `async-note`, `async-chord`, `async-melody`
- [x] `poll` for REPL responsiveness during playback
- [ ] Voice naming/listing in status output
- [ ] Priority scheduling for time-critical voices
- [ ] Voice groups (start/stop multiple voices together)
- [ ] Synchronized voice start (all begin at same time)

## MIDI Input

- [ ] MIDI input port support
- [ ] Callback-based note/CC handlers (`on-note-on`, `on-cc`)
- [ ] Live looping / overdub
- [ ] MIDI learn for CC mapping
- [ ] MIDI thru (echo input to output)

## Generative / Algorithmic

- [ ] Deterministic PRNG with seed control
- [ ] `(pick seed list)` - seeded random selection
- [ ] `(shuffle seed list)` - seeded shuffle
- [ ] `(random-walk seed start step-range n)` - random walk
- [ ] `(drunk-walk seed scale-pitches start n)` - scale-constrained walk
- [ ] `(euclidean pulses steps)` - Euclidean rhythm generator
- [ ] `(markov seed transitions state)` - Markov chain
- [ ] Cellular automata patterns

## Patterns / Sequencing

- [ ] Pattern storage with `define-pattern` / `pattern`
- [ ] Pattern transformations: `reverse-pattern`, `invert-pattern`, `retrograde`
- [ ] Timeline-based sequencing with absolute timing
- [ ] Tempo changes mid-sequence
- [ ] Swing/shuffle timing modifier
- [ ] Polyrhythm helpers
- [ ] Canon/fugue generators

## Scales / Harmony

- [x] 55 12-TET scales via `scale-*` variables
- [x] 10 microtonal scales via `scale-*-cents` variables
- [x] Scale functions: `scale`, `degree`, `in-scale?`, `quantize`
- [x] Microtonal pitch bend support
- [ ] Chord symbol parsing (`(parse-chord "Cmaj7")`)
- [ ] Automatic voice leading
- [ ] Chord progression generators
- [ ] Roman numeral analysis (`(roman-numeral root 'I)`)

## Recording / File I/O

- [x] MIDI event recording with timestamps
- [x] Save to Scheme replay scripts
- [x] Read/write standard MIDI files
- [ ] Import patterns from MIDI files
- [ ] Export to LilyPond format
- [ ] Export to MusicXML

## Language Integration

- [ ] `(define-instrument name program)` macro
- [ ] `(with-instrument name body ...)` context
- [ ] Music notation DSL (like Alda but in s-expressions)
- [ ] Integration with s7's `define-macro` for custom syntax

## REPL Enhancements

- [x] Readline with tab completion
- [x] Pitch, duration, dynamics completion
- [x] Scale and chord function completion
- [ ] Command history persistence across sessions
- [ ] Multi-line input with proper paren matching
- [ ] REPL commands (`:help`, `:ports`, `:voices`)
