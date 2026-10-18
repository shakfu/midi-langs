# CLAP plugins: design

User docs: [docs/clap/README.md](../clap/README.md).

## Script time

The interpreters send MIDI and sleep in their own C code. `projects/common/midi_clock.h` puts a `midi_sink` between them and the OS:

- `midi_out_send` stamps each message with the script time and hands it to the sink.
- `midi_sleep` advances the script time and calls the sink's `wait`.
- Schedulers keep voices in script time and expose next-wake, resume-at and stop-all.

Without a sink, nothing changes: the CLIs send to libremidi and sleep on the wall clock.

## Why the frame clock, and why clips

Two options for timing were considered.

1. **Wall clock plus a fixed latency.** Each message would be stamped when sent and placed one block later. It needed one shared send function. Messages would jitter by up to one block (2.7-11.6 ms at 128-512 frames, 44.1 kHz). Offline render would bunch notes, because the host renders faster than real time.
2. **Host frame clock.** Script time waits on rendered frames. Every sleep site and scheduler needs a script-time path.

The frame clock was chosen: a DAW bounces offline, and sample offsets are what CLAP events carry.

Imperative code cannot follow a host seek or loop: a running voice cannot rewind. So the default mode evaluates without waiting and stores the messages as a clip in beats. The audio thread then looks up each block by song position. Live mode keeps the run-as-you-go behaviour for open-ended code.

## Threads

| Thread | Owns | Talks to |
|-|-|-|
| main | lifecycle, editor, state | evaluator (job queue), audio (atomics) |
| evaluator | the interpreter, the `midi_sink`, script time | audio (clip handoff, live queue, horizon/frontier) |
| audio | clip and live players, held notes | evaluator (as above) |
| udp | the socket | evaluator (job queue, reply promise) |

guile-midi adds a watcher thread in Guile mode; see Stop.

Handoffs, all in `projects/clap/plugin.cpp`:

- Clip: `nextClip` (evaluator to audio) and `retiredClip` (audio to evaluator, which frees it). The audio thread takes a new clip only when `retiredClip` is empty, so it never frees memory.
- Live: an SPSC ring of 16384 events. Each event carries the stop generation of its job. Stop bumps the generation; the audio thread then drops older events and releases held notes.
- Live pacing: the audio thread publishes `horizon`, the beat the evaluator may run ahead to (block end plus 50 ms). The evaluator publishes `frontier`, the beat before which every event is queued. Offline, `process()` waits until `frontier` passes the block end, for at most 10 s. A queued job lowers `frontier` too, so offline rendering waits for code that has not started yet.
- Stop drops queued jobs. A job records the generation when it is dequeued; if a stop came after that, it does not run. Without this, a stop that arrived between dequeue and start was lost, and the job ran unstoppable.

## Stop

Each interpreter needs a way for Stop to end running code. Each polls `midi_clock_cancelled()`:

| Language | Mechanism |
|-|-|
| lua | count hook, every 10000 instructions |
| pktpy | `py_interrupt_hook`, checked before each instruction; a 6-line patch to `thirdparty/pocketpy/pocketpy.c`, marked `midi-langs patch` |
| s7 | begin hook; s7 compiles tight loops without `begin`, so the MIDI bindings also raise `'stopped` |
| guile | a watcher thread marks an async on the evaluator thread; Guile runs asyncs in every loop. The async throws only while Stop is pending, so a late one does nothing |
| joy | `joy_interrupt_hook`, before each value executes |
| stack | `stack_interrupt_hook`, before each token and in `times`, `*` and do-loops |
| pforth | MIDI words return an ior that `?midi` turns into an abort; pForth is vendored unmodified, so code without MIDI words cannot be stopped |
| alda | the playback loop checks between events |

pocketpy got a patch where s7 and pForth did not. Its only polled check is a CPU-time watchdog, and `clock()` counts every thread in a DAW. A trace hook does not fire for one-line loops.

## Output

pocketpy, s7, Guile and Lua take output callbacks or ports. Joy and stack-midi printed with `printf` directly; their prints now go through `joy_printf` and `stack_printf`, which write to stdout unless a host redirects them. pForth's `sdTerminalOut`, `sdSleepMillis` and `sdTerminalIn` are renamed at compile time and wrapped in `projects/pforth-midi/pf_io_midi.c`, as `sdTerminalIn` already was.

## Tests

- `clap_units` (`test_clap_units.cpp`): eval-code selection, clip length and serialisation, clip looping, release on stop, seek and new clip, live ordering.
- `<lang>_midi_clap` (`test_clap_host.cpp` with each language's `clap_test_cases.txt`): loads the built plugin and checks exact frames for clips and voices, in Live offline and real time, plus Stop, state and the one-instance rule. Only note-on and note-off events are checked, so a language may add program and control changes. `[busy]` is optional, for languages whose Stop interrupts any code.

Sanitizers, on Linux:

- ThreadSanitizer: all eight tests, no reports. TSan needs `setarch -R` on kernels with high ASLR entropy, and so does building pforth-midi under it, since the build runs an instrumented `pforth_midi_builder`. It found a join race in stack-midi's async player, which the CLI shared; fixed.
- ASan and UBSan (`build-asan`): all pass except `joy_midi_clap`, which leaks on error paths: joy's `longjmp` skips the frees. `joy_midi_timing` fails the same way. `tests/lsan.supp` suppresses Guile objects that only its GC frees.
