/* scheduler.h - Thunk-based async scheduler for guile-midi
 *
 * Uses a simple cooperative model where each "voice" is a Scheme procedure
 * that returns either:
 *   - A number (milliseconds to wait before next call)
 *   - #f (voice is complete)
 *
 * This avoids Guile's continuation complexities by not trying to suspend/resume
 * Scheme evaluation - instead each voice maintains its own state in a closure.
 *
 * Single-threaded: runs libuv event loop on main thread during (run).
 */

#ifndef GUILE_MIDI_SCHEDULER_H
#define GUILE_MIDI_SCHEDULER_H

#include <libguile.h>
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

    /* Scheme thunk - stored as gc-protected SCM */
    SCM thunk;                     /* The voice procedure */

    /* Timing state */
    uint64_t wake_time_ms;         /* Absolute time to resume */
    int waiting;                   /* Currently waiting on timer? */

    /* libuv timer handle */
    uv_timer_t timer;

    /* Control flags */
    int stop_requested;

    /* Name for debugging */
    char name[32];
} Voice;

typedef struct {
    /* libuv event loop (single-threaded) */
    uv_loop_t *loop;

    /* Voice management */
    Voice voices[MAX_VOICES];
    int next_voice_id;
    int active_count;

    /* Scheduler state */
    int running;                   /* Is run active? */
    int shutdown_requested;
} SchedulerState;

/* ============================================================================
 * Public API
 * ============================================================================ */

/* Initialize the scheduler system (call once at startup) */
int guile_scheduler_init(void);

/* Cleanup the scheduler system (call at shutdown) */
void guile_scheduler_cleanup(void);

/* Register scheduler functions in Guile */
void guile_scheduler_register(void);

/* Check if scheduler has active voices */
int guile_scheduler_active_count(void);

#endif /* GUILE_MIDI_SCHEDULER_H */
