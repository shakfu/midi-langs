# guile-midi Architecture

This document explains how guile-midi integrates the GNU Guile interpreter with MIDI functionality.

## Overview

```text
+------------------+     +------------------+     +------------------+
|   Scheme Code    | --> |  midi_module.c   | --> |   libremidi      |
| (user scripts)   |     | (Guile FFI)      |     | (MIDI backend)   |
+------------------+     +------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|   GNU Guile      |     | Scheme Prelude   |
|  (interpreter)   |     | (helper funcs)   |
+------------------+     +------------------+
        |                        |
        v                        v
+------------------+     +------------------+
|     main.c       | <-- | midi-out type    |
|  (entry point)   |     | (foreign object) |
+------------------+     +------------------+
        |
        v
+------------------+
|   scheduler.c    |
| (libuv async)    |
+------------------+
```

## Components

### 1. GNU Guile - Interpreter

GNU Guile 3.0 is the official extension language for the GNU project. It provides a full Scheme implementation with:

- Full R5RS/R6RS/R7RS Scheme
- Module system (`use-modules`)
- GOOPS object system
- Powerful macro system
- Excellent debugging and error messages
- Foreign function interface via `scm_c_define_gsubr`
- Foreign object types via `scm_make_foreign_object_type`

On macOS, guile-midi statically links Guile and its dependencies (gmp, gc, unistring), with only system libraries (libffi, libiconv) remaining dynamic.

### 2. midi_module.c - FFI Bindings

Location: `projects/guile-midi/midi_module.c`

The core C module providing MIDI functionality to Scheme:

- Module initialization (`guile_midi_init`)
- C functions exposed to Scheme via `scm_c_define_gsubr`
- Custom `midi-out` foreign object type
- Scheme prelude (embedded Scheme code)

#### midi-out Type

```c
static SCM midi_out_type;

typedef struct {
    libremidi_midi_out_handle* handle;
    int is_virtual;
    char *port_name;
} MidiOutData;
```

The `midi-out` type wraps a libremidi output handle with:

- Automatic cleanup via finalizer
- String representation via custom print function
- Type predicate via `midi-out?`

### 3. scheduler.c - Async Scheduler

Location: `projects/guile-midi/scheduler.c`

The scheduler provides non-blocking concurrent playback:

- libuv-based event loop
- Voice management with GC protection
- Timer callbacks invoking Scheme thunks
- Cooperative multitasking model

```c
typedef struct Voice {
    int id;
    SCM thunk;           // Guile procedure (GC-protected)
    char *name;
    uv_timer_t timer;
    struct Voice *next;
} Voice;

typedef struct {
    uv_loop_t *loop;
    Voice *voices;
    int next_id;
    int running;
} SchedulerState;
```

### 4. libremidi - MIDI Backend

Location: `thirdparty/libremidi/`

Cross-platform MIDI library supporting:

- macOS: CoreMIDI
- Linux: ALSA
- Windows: WinMM

### 5. Scheme Prelude

The module includes a Scheme prelude shared with s7-midi that defines:

- Pitch constants (c0-c8, cs0-cs8, etc.)
- Duration constants (whole, half, quarter, etc.)
- Velocity constants (ppp through fff)
- Chord builders (major, minor, dim, aug, dom7, maj7, min7)
- Scale definitions (55 scales including exotic and microtonal)
- Tempo functions (set-tempo!, get-tempo, bpm)
- Helper functions (transpose, octave-up, octave-down)
- Voice builders for async playback

The prelude is maintained as native Scheme code in `prelude.scm` and converted to a C header at build time:

```text
projects/guile-midi/prelude.scm  -->  scm_prelude.h  -->  midi_module.c
                                 (prelude2c.py)       (#include)
```

To regenerate after editing `prelude.scm`:

```bash
make preludes
# or
./scripts/prelude2c.py projects/guile-midi/prelude.scm
```

## Guile FFI Patterns

### Defining Functions

All C functions exposed to Scheme follow this pattern:

```c
static SCM g_my_function(SCM arg1, SCM arg2) {
    // Type checking
    SCM_ASSERT(scm_is_integer(arg1), arg1, SCM_ARG1, "my-function");

    // Convert to C types
    int value = scm_to_int(arg1);

    // Do work...

    // Return result
    return scm_from_int(result);
    // Or for void functions:
    return SCM_UNSPECIFIED;
}

// Registration:
scm_c_define_gsubr("my-function", 2, 1, 0, g_my_function);
//                 name          req opt rest fn
```

### Creating Foreign Object Types

```c
// 1. Define type and finalizer
static SCM midi_out_type;

static void finalize_midi_out(SCM obj) {
    MidiOutData *data = scm_foreign_object_ref(obj, 0);
    if (data) {
        if (data->handle) {
            libremidi_midi_out_free(data->handle);
        }
        free(data->port_name);
        free(data);
    }
}

// 2. Create type in init
SCM name = scm_from_utf8_symbol("midi-out");
SCM slots = scm_list_1(scm_from_utf8_symbol("data"));
midi_out_type = scm_make_foreign_object_type(name, slots, finalize_midi_out);

// 3. Create instances
MidiOutData *data = malloc(sizeof(MidiOutData));
data->handle = handle;
data->port_name = strdup(name);
return scm_make_foreign_object_1(midi_out_type, data);

// 4. Extract from Scheme
MidiOutData *data = scm_foreign_object_ref(obj, 0);
```

### GC Protection

Guile uses a precise garbage collector. When storing Scheme objects in C structures (like voice thunks), they must be protected:

```c
// Protect from GC
SCM thunk = ...;
scm_gc_protect_object(thunk);

// Later, when done:
scm_gc_unprotect_object(thunk);
```

### Error Handling

```c
// Type assertion (throws on failure)
SCM_ASSERT(scm_is_integer(arg), arg, SCM_ARG1, "func-name");

// General error
scm_error(scm_from_utf8_symbol("midi-error"),
          "func-name",
          "Error message: ~A",
          scm_list_1(details),
          SCM_BOOL_F);
```

### Type Conversions

| C Type | To Scheme | From Scheme |
| -------- | ----------- | ------------- |
| int | `scm_from_int(n)` | `scm_to_int(obj)` |
| double | `scm_from_double(d)` | `scm_to_double(obj)` |
| char* | `scm_from_utf8_string(s)` | `scm_to_utf8_string(obj)` |
| bool | `scm_from_bool(b)` | `scm_is_true(obj)` |
| list | `scm_list_n(...)` | `scm_car/scm_cdr` |
| pair | `scm_cons(a, b)` | `scm_car/scm_cdr` |

## Comparison: Guile vs s7 FFI

| Aspect | Guile | s7 |
| -------- | ------- | ---- |
| Function registration | `scm_c_define_gsubr(name, req, opt, rest, fn)` | `s7_define_function(sc, name, fn, req, opt, rst, doc)` |
| Custom types | `scm_make_foreign_object_type(name, slots, fin)` | `s7_make_c_type(sc, name)` |
| Create instance | `scm_make_foreign_object_1(type, data)` | `s7_make_c_object(sc, tag, data)` |
| Extract data | `scm_foreign_object_ref(obj, 0)` | `s7_c_object_value(obj)` |
| Integer conversion | `scm_to_int(obj)` / `scm_from_int(n)` | `s7_integer(obj)` / `s7_make_integer(sc, n)` |
| String conversion | `scm_to_utf8_string(obj)` | `s7_string(obj)` |
| GC protection | `scm_gc_protect_object(obj)` | `s7_gc_protect(sc, obj)` |
| Call procedure | `scm_call_0(proc)` | `s7_call(sc, proc, args)` |
| Error handling | `scm_error(...)` | `s7_error(sc, ...)` |

## MIDI Message Format

MIDI messages are sent as byte arrays:

```c
// Note On: 0x90 + channel, pitch, velocity
uint8_t msg[3] = {
    0x90 | ((channel - 1) & 0x0F),
    pitch & 0x7F,
    velocity & 0x7F
};
libremidi_midi_out_send_message(data->handle, msg, 3);

// Note Off: 0x80 + channel, pitch, velocity
msg[0] = 0x80 | ((channel - 1) & 0x0F);

// Control Change: 0xB0 + channel, control, value
msg[0] = 0xB0 | ((channel - 1) & 0x0F);

// Program Change: 0xC0 + channel, program (2 bytes only)
uint8_t pc[2] = {
    0xC0 | ((channel - 1) & 0x0F),
    program & 0x7F
};
```

## Module Initialization

The initialization sequence in `guile_midi_init()`:

1. Create `midi-out` foreign object type with finalizer
2. Register all C functions with Guile
3. Initialize scheduler
4. Load Scheme prelude

```c
void guile_midi_init(void) {
    // 1. Create midi-out type
    SCM name = scm_from_utf8_symbol("midi-out");
    SCM slots = scm_list_1(scm_from_utf8_symbol("data"));
    midi_out_type = scm_make_foreign_object_type(name, slots, finalize_midi_out);

    // 2. Register functions
    scm_c_define_gsubr("midi-open", 0, 1, 0, g_midi_open);
    scm_c_define_gsubr("midi-close", 1, 0, 0, g_midi_close);
    scm_c_define_gsubr("midi-note", 2, 3, 0, g_midi_note);
    // ... more functions

    // 3. Initialize scheduler
    scheduler_init();

    // 4. Load prelude
    scm_c_eval_string(SCHEME_PRELUDE);
}
```

## File Structure

```text
projects/guile-midi/
  main.c              # Entry point, REPL with readline
  midi_module.c       # Guile FFI bindings
  midi_module.h       # Module header
  scheduler.c         # libuv async scheduler
  scheduler.h         # Scheduler types and API
  prelude.scm         # Scheme prelude source (shared with s7-midi)
  scm_prelude.h       # Generated C header (do not edit)
  CMakeLists.txt      # Build configuration

thirdparty/libremidi/
  # MIDI library

tests/
  test_guile_midi.sh  # Test suite

docs/guile-midi/
  README.md           # Overview
  api-reference.md    # Complete API docs
  examples.md         # Code examples
  architecture.md     # This file
```

## Build Process

```text
main.c + midi_module.c + scheduler.c
           +
    libguile-3.0.a (static on macOS)
           +
      libremidi.a
           |
           v
     guile_midi (executable)
```

CMake configuration highlights:

```cmake
find_package(PkgConfig REQUIRED)
pkg_check_modules(GUILE guile-3.0)

add_executable(guile_midi
    main.c
    midi_module.c
    scheduler.c
)

target_include_directories(guile_midi PRIVATE
    ${GUILE_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/thirdparty/libuv/include
)

# Static linking on macOS
if(APPLE AND GUILE_STATIC)
    target_link_libraries(guile_midi PRIVATE
        ${GUILE_STATIC_LIB}
        ${GMP_STATIC_LIB}
        ${GC_STATIC_LIB}
        ${UNISTRING_STATIC_LIB}
        ffi iconv       # System libs (dynamic)
        music_theory midi_file uv_a
    )
endif()
```

## Extending the Module

### Adding a New C Function

1. Write the C function:

```c
static SCM g_new_func(SCM arg) {
    SCM_ASSERT(scm_is_integer(arg), arg, SCM_ARG1, "new-func");
    int value = scm_to_int(arg);
    // Implementation...
    return scm_from_int(result);
}
```

2. Register it in `guile_midi_init`:

```c
scm_c_define_gsubr("new-func", 1, 0, 0, g_new_func);
```

### Adding via Scheme Prelude

For simpler additions, edit `prelude.scm` directly:

```scheme
;; In projects/guile-midi/prelude.scm
(define (new-helper x)
  (* x 2))
```

Then regenerate the header:

```bash
make preludes
```

This is preferred for:

- Pure Scheme logic
- Helper functions
- Constants
- Higher-order functions

## Static Linking on macOS

guile-midi uses a hybrid static/dynamic linking approach on macOS:

**Statically linked:**
- libguile-3.0.a (Guile interpreter)
- libgmp.a (GNU Multiple Precision)
- libgc.a (Boehm garbage collector)
- libunistring.a (Unicode string library)

**Dynamically linked (system libraries):**
- libffi (no static library available on macOS)
- libiconv (system library)
- CoreMIDI, CoreFoundation, CoreAudio frameworks

This produces a ~2.6MB binary that only depends on system libraries, making distribution easier while avoiding the complexity of fully static linking.
