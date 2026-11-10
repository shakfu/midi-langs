# CLAP plugins

Each midi-lang is also a CLAP note-effect plugin. Code comes from an editor in the plugin window, or over UDP. Its MIDI goes to the host on one note output port; route that port to any instrument.

## Build and install

```sh
make clap                     # every plugin, in build/clap/
make clap-dev CLAP=s7-midi    # open one in the dev host
```

`BUILD_CLAP` is on by default except on Windows. On Linux it needs X11 and OpenGL headers (`sudo apt-get install libx11-dev libgl-dev`); without them CMake warns and skips the plugins. guile-midi's plugin needs Guile, as its CLI does, and links the system libguile.

Copy a plugin to the CLAP folder:

| Platform | Folder |
|-|-|
| Linux | `~/.clap/` |
| macOS | `~/Library/Audio/Plug-Ins/CLAP/` (copy the `.clap` bundle) |

The dev host (`build/clap_dev_host PLUGIN.clap`) plays the plugin at 120 BPM and sends its MIDI to the first MIDI port. With no ports, it creates a virtual port `midi-langs-dev`. `-l` lists ports, `-p N` picks one, and `--virtual NAME` creates one.

## VST3

Each plugin is also built as a VST3, for hosts without CLAP such as Ableton Live:

```sh
make vst3                     # every plugin, in build/vst3/
```

`BUILD_VST3` is on wherever `BUILD_CLAP` is. Copy the `.vst3` bundle to `~/Library/Audio/Plug-Ins/VST3/` on macOS, or `~/.vst3/` on Linux. It is self-contained and needs no `.clap`.

The VST3 behaves as the CLAP does. VST3 has no MIDI-effect category, so hosts list it as an instrument. Live refuses an instrument without a note input and an audio output, so the VST3 adds both: the input is ignored and the output is silent. Notes leave as VST3 note events. Control change, program change, pitch bend, channel and poly pressure leave as legacy MIDI CC events. Whether a host passes these to other tracks depends on the host.

## Modes

The **Live** checkbox selects the mode. Both modes play the same code.

| | Clip (default) | Live |
|-|-|-|
| Evaluation | runs at once, without waiting | waits as the code sleeps |
| Plays | the result, looped | each message as it is sent |
| Timeline | host song position, from beat 0 | the plugin's own, from load |
| Transport stopped | silent, held notes released | plays |
| Seek, loop, tempo change | follows | follows tempo only |
| Offline render | exact | exact; the host waits for the code |
| Saved with the project | the clip | no |
| Code that never ends | cut off at 1024 beats or 200000 events | plays until Stop |

A clip is one evaluation's messages. An evaluation that sends nothing, such as a definition, keeps the current clip playing.

## Time

Script time is milliseconds. 500 ms is one beat, so a script at its default 120 BPM follows the host tempo exactly. A script tempo other than 120 scales durations against the host's.

- Clip: the length is whole bars past the last message. A bar follows the host time signature, or 4/4. Without a host timeline, the clip loops at 120 BPM from activation.
- Live: new code starts one block plus 50 ms ahead of the host. Every message lands on its exact frame, in real time and offline.

## Editor

| Button | Key | Action |
|-|-|-|
| Eval Block | Ctrl+Return (Cmd on macOS) | Evaluate the selection, else the block around the caret |
| Eval All | Ctrl+Shift+Return | Evaluate the whole editor |
| Stop | Ctrl+. | Stop the running code, its voices and queued evaluations; release every held note; silence the clip |

A block is the paragraph around the caret (the lines between blank lines), or for s7 and guile, the top-level bracket group. Output, results and errors appear in the log pane. **Clear** empties the editor and the log. **Wrap** wraps long lines. On Linux the editor needs the host's CLAP timer support to redraw.

The editor colours code with [tree-sitter](https://tree-sitter.github.io/) grammars: lua-midi (Lua), pktpy-midi (Python), s7-midi and guile-midi (Scheme), mhs-midi (Haskell), pforth-midi (Forth) and joy-midi (Joy, with joy-midi's `\` comments). alda-midi has no grammar, and stack-midi's chords would read as Forth comments, so those two show plain text. The editor also shows line numbers, matches brackets and closes them as you type, and has undo, multiple cursors and find (Ctrl+F, Cmd on macOS).

## UDP

Each datagram is evaluated. The reply is the output, or `ok`:

```sh
$ nc -u 127.0.0.1 57161
2 + 3
5
```

UDP is off by default; toggle it in the editor. `<NAME>_MIDI_CLAP_UDP_PORT`, such as `LUA_MIDI_CLAP_UDP_PORT`, turns it on at load and sets the port. It binds to loopback only, but any local process can send it code.

## State

The project saves the editor text, the Wrap, Live and UDP settings, and the clip. A reopened project plays its clip without evaluating anything.

## Languages

| Plugin | UDP port | Voices | Stop interrupts |
|-|-|-|-|
| lua-midi | 57161 | `spawn` | any code |
| pktpy-midi | 57162 | `midi.spawn` | any code |
| s7-midi | 57163 | `spawn` | code with a `begin`, or a MIDI call |
| guile-midi | 57164 | `spawn` | any code |
| joy-midi | 57165 | none | any code |
| stack-midi | 57166 | `seq-play&`, `seq-loop&` | any code |
| pforth-midi | 57167 | none | a MIDI word |
| alda-midi | 57168 | parts play together | playback, between events |
| mhs-midi | 57169 | `spawn` (Async) | any code |

In every plugin:

- A port is open at load where the language has a default port: `open()`, `(open)`, `midi-open`. Every port sends to the host; port lists are empty.
- Voices run after each evaluation; `run` is not needed. In Live mode, `run` holds the evaluator until its voices end.
- In Live mode, blocking calls, such as a note with a duration or a sleep, hold the evaluator for their duration, and later evaluations wait. Use voices for parts that play together.
- Exiting the host is disabled: `os.exit`, `exit`, `quit`, `bye` and the like raise an error or are removed.
- Messages printed by the C layer outside the interpreter's own output, such as lua's `help()` and pforth's `midi-list`, go to the host's standard output.

Per language:

- **pktpy-midi**: `midi` is imported. `time.sleep` and `midi.sleep` wait in script time.
- **s7-midi**: a tight loop with neither a `begin` nor a MIDI call cannot be stopped; s7 checks for Stop only at `begin`.
- **joy-midi**: the stack is printed after each evaluation, then cleared. `prelude.joy` is compiled in.
- **stack-midi**: output with a counted error starts with `error:`. Stopped code also resets the stack, compile mode and blocks; definitions stay.
- **pforth-midi**: code runs line by line, 256 characters at most per line. Only a MIDI word sees Stop, so `begin again` without one cannot be stopped. `key` and `accept` read an empty line.
- **alda-midi**: parts, instruments and attributes carry over between evaluations; each evaluation plays only its own notes. Parse errors appear in full; other errors go to the host's standard error. At `(tempo 120)` a quarter note is one beat.
- **mhs-midi**: each paragraph is one REPL entry: a group of definitions, or one expression, which may span lines. `MidiPerform` and `Async` are imported. Every evaluation is compiled first, which takes 0.1 to 0.3 s; in Live mode the code then starts at the next point the host can still play on time. The first instance in a process starts the interpreter, in about 2 s; it then outlives instances, and each new instance starts with no definitions. `midiSleep` and `threadDelay` wait in script time, so `spawn` voices play together. The REPL cannot show the thread id `spawn` returns, so end a `spawn` entry with `>> return ()`.

Example, in lua-midi:

```lua
open()
spawn(function()
  for i = 1, 8 do play(c3, f, eighth) end
end)
spawn(function()
  play_arp({c4, e4, g4, c5}, mf, eighth)
end)
run()
```

## Limits

- One instance per language per process: the interpreters keep process-wide state. Different languages can share a process.
- No MIDI input. Sysex is not sent.
- Live mode ignores the host's transport position and play state.
- Code that Stop cannot interrupt blocks removing the plugin.
- The CLAPs are tested on Linux and macOS. The VST3s are tested on macOS in `test_vst3_host` and in Ableton Live 12.4. Windows is not built.
- A CLAP and a VST3 of the same language share a UDP port. If both turn UDP on, the second fails to bind and its UDP stays off.

## Adding a language

1. Send through `midi_out_send` and sleep through `midi_sleep` (`projects/common/midi_clock.h`).
2. Give the scheduler, if any, a script-time path: next wake, resume due voices, stop all. See `projects/lua-midi/scheduler.c`.
3. Give the interpreter a way for Stop to end running code, polling `midi_clock_cancelled()`.
4. Move the interpreter into a library, and write an adapter that defines `kLanguage` (`projects/clap/language.hpp`).
5. Call `midi_langs_add_clap` in the language's `CMakeLists.txt`; it adds the VST3 too. Add the language to the CLAP and VST3 loops in `tests/CMakeLists.txt` and the plugin loops in the top-level `CMakeLists.txt`, and write its `clap_test_cases.txt`.

Design and threads: [docs/dev/clap.md](../dev/clap.md).
