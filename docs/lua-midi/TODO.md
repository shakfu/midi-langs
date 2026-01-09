# lua-midi TODO

## Async Scheduler

- [x] Coroutine-based concurrent voices with `spawn`/`run`
- [x] Non-blocking `yield_ms()` for timing
- [x] Voice control: `stop`, `voices`, `scheduler.status`
- [x] Async note helpers: `play`, `play_chord`, `play_arp`
- [x] `poll()` for REPL responsiveness during playback
- [ ] Voice naming/listing in status output
- [ ] Priority scheduling for time-critical voices
- [ ] Voice groups (start/stop multiple voices together)

## MIDI Input

- [ ] MIDI input port support
- [ ] Callback-based note/CC handlers
- [ ] Live looping / overdub
- [ ] MIDI learn for CC mapping
- [ ] MIDI thru (echo input to output)

## Generative / Algorithmic

- [ ] Deterministic PRNG with seed control
- [ ] `pick(seed, list)` - seeded random selection
- [ ] `shuffle(seed, list)` - seeded shuffle
- [ ] `random_walk(seed, start, step_range, n)` - random walk
- [ ] `drunk_walk(seed, scale, start, n)` - scale-constrained walk
- [ ] `euclidean(pulses, steps)` - Euclidean rhythm generator
- [ ] `markov(seed, transition_table, state)` - Markov chain

## Patterns / Sequencing

- [ ] Pattern storage and recall
- [ ] Pattern transformations (reverse, invert, retrograde)
- [ ] Timeline-based sequencing with absolute timing
- [ ] Tempo changes mid-sequence
- [ ] Swing/shuffle timing modifier
- [ ] Polyrhythm helpers

## Scales / Harmony

- [x] 55 12-TET scales via `midi.scales`
- [x] 10 microtonal scales via `midi.scales_cents`
- [x] Scale functions: `scale`, `degree`, `in_scale`, `quantize`
- [x] Microtonal pitch bend support
- [ ] Chord symbol parsing (`"Cmaj7"`, `"Dm"`, `"G7"`)
- [ ] Automatic voice leading
- [ ] Chord progression generators

## Recording / File I/O

- [x] MIDI event recording with timestamps
- [x] Save to Lua replay scripts
- [x] Read/write standard MIDI files
- [ ] Import patterns from MIDI files
- [ ] Export to other formats (MusicXML, ABC notation)

## REPL Enhancements

- [x] Readline with tab completion
- [x] Pitch, duration, dynamics completion
- [ ] Command history persistence across sessions
- [ ] Multi-line input support
- [ ] Syntax highlighting (if terminal supports)
