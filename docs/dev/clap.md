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

guile-midi adds a watcher thread in Guile mode; see Stop. mhs-midi runs its interpreter on a thread of its own; see mhs-midi.

Handoffs, all in `projects/clap/plugin.cpp`:

- Clip: `nextClip` (evaluator to audio) and `retiredClip` (audio to evaluator, which frees it). The audio thread takes a new clip only when `retiredClip` is empty, so it never frees memory.
- Live: an SPSC ring of 16384 events. Each event carries the stop generation of its job. Stop bumps the generation; the audio thread then drops older events and releases held notes.
- Live pacing: the audio thread publishes `horizon`, the beat the evaluator may run ahead to (block end plus 50 ms). The evaluator publishes `frontier`, the beat before which every event is queued. Offline, `process()` waits until `frontier` passes the block end, for at most 10 s. A queued job lowers `frontier` too, so offline rendering waits for code that has not started yet.
- Live start: a job starts at `horizon`. Its first send or wait moves it to the current `horizon` if that has passed the start, so code that runs past the lookahead before its first event starts late as a whole instead of clamping its first notes. mhs-midi needs this: it compiles each evaluation first.
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
| mhs | the adapter sets the runtime's `has_sigint`, which raises `UserInterrupt` in the REPL thread |

pocketpy got a patch where s7 and pForth did not. Its only polled check is a CPU-time watchdog, and `clock()` counts every thread in a DAW. A trace hook does not fire for one-line loops.

## Output

pocketpy, s7, Guile and Lua take output callbacks or ports. Joy and stack-midi printed with `printf` directly; their prints now go through `joy_printf` and `stack_printf`, which write to stdout unless a host redirects them. pForth's `sdTerminalOut`, `sdSleepMillis` and `sdTerminalIn` are renamed at compile time and wrapped in `projects/pforth-midi/pf_io_midi.c`, as `sdTerminalIn` already was.

## mhs-midi

The MicroHs runtime can neither run on the plugin's evaluator thread nor return to it between evaluations: its REPL owns the thread it runs on. So it runs on a thread that lives for the process, and the adapter (`projects/mhs-midi/plugin/mhs_clap.cpp`) sends it requests and waits. The REPL is `PluginRepl.hs`, built from MicroHs's own REPL functions and compiled to C like `generated/mhs.c`. It needs a two-line export patch to `thirdparty/MicroHs/src/MicroHs/{Interactive,Main}.hs`, marked `midi-langs patch`.

`mhs_plugin_runtime.c` compiles the runtime with three changes, made with macros and include order rather than in the vendored source:

- Haskell's `stdout` and `stderr` write to a private stream, read per request; the host's are untouched.
- No SIGINT handler. Stop sets `has_sigint` directly.
- The runtime clock is script time (`plugin/runtime/extra.c`), and IO polling is off (`plugin/runtime/config.h`), so a runtime whose threads all sleep advances script time instead of sleeping. `midiSleep` is a `threadDelay` under a sink, so voices are ordinary `forkIO` threads, run in script-time order by MicroHs's timer queue. `nextWake` reads that queue; `resume` sleeps the REPL thread until the given time.

The alternative was running `mhs-midi` as a child process over a pipe. It was not chosen: script time would have to cross the pipe, and some hosts restrict child processes.

## Editor

`editor_imgui.cpp` draws the editor with ImGuiColorTextEdit (`thirdparty/imgui-color-text-edit`), not `ImGui::InputTextMultiline`, which draws all text in one colour. That widget colours text by its own line-by-line rules, which have no Scheme, Haskell, Forth or Lua 5.5 definitions. So the vendored copy has a small `midi-langs patch`: a callback that runs after its colorizer whenever the text changed, and `SetColor`, through which `highlight.cpp` applies tree-sitter's colours.

`highlight.cpp` parses the whole text on each edit. Buffers are a few KB, so incremental parsing was not worth tracking edits for. It evaluates the queries' `#eq?`, `#match?` and `#any-of?` predicates, which tree-sitter's C API reports but leaves to the caller. Where captures overlap, the inner node wins, then the later pattern; the vendored `highlights.scm` files are written for that order. A plugin picks its grammar with `midi_langs_add_clap(... GRAMMAR <name>)`; the build turns each grammar's query into `highlight_<name>.h`.

The Haskell grammar adds 3.8 MB to mhs-midi's plugin and 19 MB of generated C to `thirdparty/`.

## VST3

`thirdparty/clap-wrapper` builds each VST3 from the same plugin code. Two layouts were possible:

1. **Nested:** a generic wrapper that loads the `.clap` from its bundle at run time.
2. **Linked:** the plugin code linked into the VST3, which exports `clap_entry` for the wrapper to find in its own binary.

Linked was chosen: one binary, and no search path that differs by platform (on macOS the wrapper ignores `CLAP_PATH`). Both modules link one OBJECT library. A static archive would drop `plugin.cpp`, since only the exported `clap_entry` refers to it.

- Each module links its own pugl with its own Objective-C class prefix, so a CLAP and a VST3 can load in one host.
- clap-wrapper's VST3 output dropped `CLAP_EVENT_MIDI`, the only event the plugins send. The vendored copy carries that fix and three others as open upstream PRs; see `thirdparty/clap-wrapper/VERSION.txt`.
- Live refuses a VST3 instrument without a note input and an audio output ([clap-wrapper#635](https://github.com/free-audio/clap-wrapper/issues/635)). In the VST3 only, found through clap-wrapper's host-information extension, the plugin declares both: the input takes CLAP note events and is ignored, and the output is silent. A MIDI input would make clap-wrapper add about 2100 MIDI-CC mapping parameters.

## Tests

- `clap_units` (`test_clap_units.cpp`): eval-code selection, clip length and serialisation, clip looping, release on stop, seek and new clip, live ordering.
- `<lang>_midi_clap` (`test_clap_host.cpp` with each language's `clap_test_cases.txt`): loads the built plugin and checks exact frames for clips and voices, in Live offline and real time, plus Stop, state and the one-instance rule. Only note-on and note-off events are checked, so a language may add program and control changes. `[busy]` is optional, for languages whose Stop interrupts any code.
- `<lang>_midi_vst3` (`test_vst3_host.cpp`, same cases): loads the VST3 and checks what the wrapper adds: class and buses, notes as VST3 note events on exact frames, and state. `[cc]` is optional; it checks a control change as a legacy MIDI CC event.

Sanitizers, on Linux:

- ThreadSanitizer: all eight tests, no reports. TSan needs `setarch -R` on kernels with high ASLR entropy, and so does building pforth-midi under it, since the build runs an instrumented `pforth_midi_builder`. It found a join race in stack-midi's async player, which the CLI shared; fixed.
- ASan and UBSan (`build-asan`): all pass except `joy_midi_clap`, which leaks on error paths: joy's `longjmp` skips the frees. `joy_midi_timing` fails the same way. `tests/lsan.supp` suppresses Guile objects that only its GC frees.
