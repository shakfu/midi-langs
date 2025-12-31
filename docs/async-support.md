# Async MIDI Support Investigation

This document analyzes approaches for adding non-blocking, asynchronous MIDI playback to the midi-langs implementations.

## Current State

All implementations currently use blocking sleep calls for timing:
- **forth-midi**: `seq-play` blocks via `midi_sleep_ms()` / `usleep()`
- **lua-midi/pktpy-midi/s7-midi**: Direct note methods block for duration
- **mhs-midi**: Relies on MicroHs host for timing control

This prevents:
- Playing multiple sequences simultaneously
- REPL interaction during playback
- Complex multi-voice compositions with independent timing

## libremidi Recommendations

The underlying libremidi library recommends (see [queue.md](https://celtera.github.io/libremidi/queue.html)):

1. **Callback-based queue**: Build event processing on callbacks, integrating with the application's event loop
2. **Async runtime with coroutines**: Use C++20 coroutines for imperative-style non-blocking code

---

## Approach 1: Event Scheduler (Timer-Based)

**Concept:** A timestamp-based event queue processed by OS timer callbacks, enabling non-blocking playback.

### Candidates

| Library | Platform | Type | Notes |
|---------|----------|------|-------|
| [libxev](https://github.com/mitchellh/libxev) | Linux/macOS/WASI | C API | Proactor model, zero allocs, production-ready |
| `dispatch_source` (GCD) | macOS native | System | Built-in, excellent but macOS-only |
| [libdispatch (Linux port)](https://github.com/RIFTIO/libdispatch) | Linux | System-like | Ports GCD to Linux |
| `kqueue` (direct) | macOS/BSD | System | Native but low-level |
| `timerfd` + `epoll` | Linux | System | Native but Linux-only |

### Architecture

```
+------------------+      +--------------+      +-------------+
| Sequence Events  |  ->  | Event Queue  |  ->  | Timer Loop  |
| (sorted by time) |      | (SPSC queue) |      | (libxev/GCD)|
+------------------+      +--------------+      +-------------+
                                                      |
                                                      v
                                               +-------------+
                                               | MIDI Output |
                                               +-------------+
```

### Trade-offs

| Pros | Cons |
|------|------|
| True non-blocking, single-threaded | Adds dependency (libxev ~100KB) |
| Deterministic timing | Event loop must integrate with REPL input |
| libxev production-ready (used by Ghostty) | Platform abstraction complexity |

### Best for

**forth-midi** - Already has sequence system to build on.

---

## Approach 2: Thread-Per-Voice

**Concept:** Each voice/sequence runs in its own pthread, sharing MIDI output.

### Current State

- PocketPy already has pthread wrappers in `pocketpy.c:378-9177`
- No threading in forth-midi, lua-midi, s7-midi, mhs-midi
- libremidi uses internal mutexes (output should be thread-safe)

### Implementation Sketch

```c
typedef struct {
    pthread_t thread;
    Sequence* seq;
    int playing;
    int stop_requested;
} VoiceThread;

void* voice_thread_fn(void* arg) {
    VoiceThread* vt = (VoiceThread*)arg;
    // Play sequence with midi_sleep_ms between events
    // Check stop_requested periodically
    // Each thread independently manages its timing
}

// Launch async playback
void seq_play_async(int seq_id) {
    VoiceThread* vt = &voice_threads[seq_id];
    vt->seq = &sequences[seq_id];
    vt->playing = 1;
    pthread_create(&vt->thread, NULL, voice_thread_fn, vt);
}
```

### Trade-offs

| Pros | Cons |
|------|------|
| Simplest mental model | Thread overhead for many voices |
| pthread available everywhere | Timing affected by OS scheduling |
| Each voice independent | Need to verify libremidi thread-safety |

### Best for

**pktpy-midi** - Already has thread infrastructure.

---

## Approach 3: Coroutine Integration

**Concept:** Leverage host language coroutines for cooperative multitasking.

### Lua 5.5 Coroutine API

```c
lua_State* lua_newthread(lua_State *L);
int lua_resume(lua_State *L, lua_State *from, int narg, int *nres);
int lua_yieldk(lua_State *L, int nresults, lua_KContext ctx, lua_KFunction k);
#define lua_yield(L,n) lua_yieldk(L, (n), 0, NULL)
```

### User-Facing Pattern

```lua
-- User code defines voices as coroutines
function voice1()
    while true do
        m:note_on(60, 100)
        yield_ms(500)  -- yields control back to scheduler
        m:note_off(60)
        yield_ms(500)
    end
end

function voice2()
    while true do
        m:note_on(64, 80)
        yield_ms(333)
        m:note_off(64)
        yield_ms(333)
    end
end

-- Launch both voices
scheduler.spawn(voice1)
scheduler.spawn(voice2)
scheduler.run()  -- runs until all voices complete or stopped
```

### Implementation Requirements

1. Create coroutine per voice via `lua_newthread`
2. When `yield_ms(n)` called, record resume time
3. Main loop polls and resumes coroutines whose time has arrived
4. Use non-blocking `select()` for REPL input during wait

### Trade-offs

| Pros | Cons |
|------|------|
| Natural fit for Lua users | Requires scheduler loop in C |
| No threading complexity | Timing granularity depends on poll frequency |
| Works with existing stdlib | More complex than simple threading |

### Best for

**lua-midi** - Cooperative multitasking is idiomatic Lua.

---

## Approach 4: Lock-Free Queue

**Concept:** Decouple event production from playback using a lock-free SPSC queue.

### Top Candidates

| Library | Language | Type | License |
|---------|----------|------|---------|
| [cameron314/readerwriterqueue](https://github.com/cameron314/readerwriterqueue) | C++11 | Header-only | BSD |
| [MengRao/SPSC_Queue](https://github.com/MengRao/SPSC_Queue) | C++ | Header-only | MIT |
| [Boost.Lockfree spsc_queue](https://theboostcpplibraries.com/boost.lockfree) | C++ | Library | Boost |
| [Zephyr SPSC](https://docs.zephyrproject.org/latest/kernel/data_structures/spsc_lockfree.html) | C | Embedded | Apache 2.0 |

### Pattern

```c
// Producer (user code / REPL)
MidiEvent event = {.time = now + 500, .type = NOTE_ON, .pitch = 60};
spsc_queue_push(&event_queue, event);

// Consumer thread (MIDI output loop)
while (running) {
    MidiEvent event;
    while (spsc_queue_pop(&event_queue, &event)) {
        if (current_time >= event.time) {
            send_midi_event(&event);
        } else {
            // Re-queue or use priority queue
        }
    }
    sleep_until_next_event();
}
```

### Trade-offs

| Pros | Cons |
|------|------|
| Decouples production from playback | Still need consumer thread/loop |
| Battle-tested implementations | C++ dependency for best options |
| Works with threading or async | Adds complexity |

### Best for

**Any implementation** - Foundation layer, combine with threading or event loop.

---

## Recommendations by Implementation

| Implementation | Recommended Approach | Rationale |
|----------------|---------------------|-----------|
| **forth-midi** | Event Scheduler + libxev | Already has sequences; scheduler adds non-blocking |
| **lua-midi** | Coroutine Integration | Natural Lua pattern, single-threaded |
| **pktpy-midi** | Thread-per-voice | Already has pthread infrastructure |
| **s7-midi** | Event Scheduler | Scheme's functional style suits event queues |
| **mhs-midi** | Host language (MicroHs) | Defer to Haskell's concurrency model |

---

## Simplest Path Forward

For a **proof of concept**, two approaches stand out:

### Option A: lua-midi with Coroutines

Most natural fit for the language, no new dependencies, single-threaded.

### Option B: forth-midi with pthread

Simple thread playing one sequence while REPL continues:

```c
// In sequences.c
static pthread_t player_thread;
static volatile int player_running = 0;

void* seq_play_thread(void* arg) {
    int seq_id = *(int*)arg;
    // Existing seq_play logic here
    player_running = 0;
    return NULL;
}

void op_seq_play_async(Stack* s) {
    static int seq_id;
    seq_id = current_seq;
    player_running = 1;
    pthread_create(&player_thread, NULL, seq_play_thread, &seq_id);
    // Returns immediately, REPL continues
}
```

---

## References

- [libxev - Cross-platform event loop](https://github.com/mitchellh/libxev)
- [cameron314/readerwriterqueue - SPSC queue](https://github.com/cameron314/readerwriterqueue)
- [libremidi queue documentation](https://celtera.github.io/libremidi/queue.html)
- [Lua coroutine tutorial](http://lua-users.org/wiki/CoroutinesTutorial)
- [itch.io coroutines for non-blocking IO](https://leafo.net/posts/itchio-and-coroutines.html)
- [Cross-platform timer challenges](https://gaultier.github.io/blog/the_missing_cross_platform_os_api_for_timers.html)
- [libdispatch Linux port](https://github.com/RIFTIO/libdispatch)
