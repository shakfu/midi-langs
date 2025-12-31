/* scheduler.h - Coroutine-based async scheduler for lua-midi
 *
 * Enables non-blocking MIDI playback using Lua coroutines and libuv timers.
 * Multiple "voices" can run concurrently with independent timing.
 */

#ifndef LUA_MIDI_SCHEDULER_H
#define LUA_MIDI_SCHEDULER_H

#include "lua.h"
#include "lauxlib.h"
#include <uv.h>

/* ============================================================================
 * Constants
 * ============================================================================ */

#define MAX_VOICES 16

/* ============================================================================
 * Types
 * ============================================================================ */

typedef struct {
    int active;                    /* Is this voice slot in use? */
    int voice_id;                  /* Unique ID for this voice */
    lua_State *co;                 /* Coroutine thread */
    int registry_ref;              /* LUA_REGISTRYINDEX reference (prevents GC) */

    /* Timing state */
    uint64_t wake_time_ms;         /* Absolute time to resume */
    int waiting;                   /* Currently waiting on yield_ms? */

    /* libuv timer handle */
    uv_timer_t timer;

    /* Control flags */
    int stop_requested;

    /* Name for debugging */
    char name[32];
} Voice;

typedef struct {
    /* libuv event loop (runs in separate thread) */
    uv_loop_t *loop;
    uv_thread_t thread;
    uv_async_t wake_async;         /* Wake loop for new work */
    uv_mutex_t mutex;              /* Protect voice list */

    /* Voice management */
    Voice voices[MAX_VOICES];
    int next_voice_id;
    int active_count;

    /* Scheduler state */
    int running;                   /* Is run() active? */
    int shutdown_requested;

    /* Reference to main Lua state */
    lua_State *main_L;

    /* Pending resume queue (timer callbacks add here) */
    int pending_resumes[MAX_VOICES];
    int pending_count;
    uv_mutex_t pending_mutex;
    uv_async_t resume_async;       /* Signal main thread to process resumes */
} SchedulerSystem;

/* ============================================================================
 * Public API
 * ============================================================================ */

/* Initialize the scheduler system (call once at startup) */
int scheduler_init(lua_State *L);

/* Cleanup the scheduler system (call at shutdown) */
void scheduler_cleanup(void);

/* Register scheduler functions as Lua module */
int luaopen_scheduler(lua_State *L);

/* Check if scheduler has pending work (for REPL integration) */
int scheduler_has_pending(void);

/* Process one batch of pending resumes (for REPL integration) */
int scheduler_process_pending(lua_State *L);

#endif /* LUA_MIDI_SCHEDULER_H */
