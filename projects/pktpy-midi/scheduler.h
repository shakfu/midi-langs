/* scheduler.h - Generator-based async scheduler for pktpy-midi
 *
 * Enables non-blocking MIDI playback using Python generators and libuv timers.
 * Multiple "voices" can run concurrently with independent timing.
 */

#ifndef PKTPY_MIDI_SCHEDULER_H
#define PKTPY_MIDI_SCHEDULER_H

#include "pocketpy.h"
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
    int slot_index;                /* Index in the generators list */

    /* Timing state */
    double wake_time_ms;           /* Absolute time to resume; script time under a midi_sink */
    int waiting;                   /* Currently waiting on yield? */

    /* libuv timer handle */
    uv_timer_t timer;

    /* Control flags */
    int stop_requested;

    /* Name for debugging */
    char name[32];
} Voice;

typedef struct {
    /* libuv event loop */
    uv_loop_t *loop;
    uv_mutex_t mutex;              /* Protect voice list */

    /* Voice management */
    Voice voices[MAX_VOICES];
    int next_voice_id;
    int active_count;

    /* Scheduler state */
    int running;                   /* Is run() active? */

    /* Pending resume queue (timer callbacks add here) */
    int pending_resumes[MAX_VOICES];
    int pending_count;
    uv_mutex_t pending_mutex;
} SchedulerState;

/* ============================================================================
 * Public API
 * ============================================================================ */

/* Initialize the scheduler system (call once at startup) */
int pk_scheduler_init(void);

/* Cleanup the scheduler system (call at shutdown) */
void pk_scheduler_cleanup(void);

/* Register scheduler functions in the midi module */
void pk_scheduler_register(py_GlobalRef mod);

/* Check if scheduler has active voices */
int pk_scheduler_active_count(void);

/* Under a midi_sink (midi_clock.h), voices wait in script time, not on timers,
 * and a host drives them with these. */

/* Earliest wake time of a waiting voice; returns 0 if no voice waits. */
int pk_scheduler_next_wake(double *ms);

/* Resume, in slot order, each voice due at or before ms, at its own wake time. */
void pk_scheduler_resume_at(double ms);

/* Stop every voice. */
void pk_scheduler_stop_all(void);

/* Voice errors go to fn instead of stderr; NULL restores stderr. */
void pk_scheduler_set_error_handler(void (*fn)(void *user, const char *msg), void *user);

#endif /* PKTPY_MIDI_SCHEDULER_H */
