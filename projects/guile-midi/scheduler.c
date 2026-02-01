/* scheduler.c - Thunk-based async scheduler for guile-midi
 *
 * Each voice is a Scheme procedure that returns ms-to-wait or #f when done.
 * Uses single-threaded libuv for timing - no thread safety issues.
 */

#include "scheduler.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

/* Cross-platform timing */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
static LARGE_INTEGER sched_perf_freq;
static int sched_perf_freq_init = 0;
static void sched_init_perf_freq(void) {
    if (!sched_perf_freq_init) {
        QueryPerformanceFrequency(&sched_perf_freq);
        sched_perf_freq_init = 1;
    }
}
#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 0
#endif
static int clock_gettime(int clk_id, struct timespec *tp) {
    (void)clk_id;
    sched_init_perf_freq();
    LARGE_INTEGER count;
    QueryPerformanceCounter(&count);
    tp->tv_sec = (long)(count.QuadPart / sched_perf_freq.QuadPart);
    tp->tv_nsec = (long)((count.QuadPart % sched_perf_freq.QuadPart) * 1000000000 / sched_perf_freq.QuadPart);
    return 0;
}
#endif

/* ============================================================================
 * Static State
 * ============================================================================ */

static SchedulerState sched = {0};

/* ============================================================================
 * Time Helpers
 * ============================================================================ */

static uint64_t now_ms(void) {
    struct timespec ts;
    clock_gettime(CLOCK_MONOTONIC, &ts);
    return (uint64_t)ts.tv_sec * 1000 + (uint64_t)ts.tv_nsec / 1000000;
}

/* ============================================================================
 * Voice Management
 * ============================================================================ */

static Voice* find_free_voice(void) {
    for (int i = 0; i < MAX_VOICES; i++) {
        if (!sched.voices[i].active) {
            return &sched.voices[i];
        }
    }
    return NULL;
}

static Voice* find_voice_by_id(int voice_id) {
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active && sched.voices[i].voice_id == voice_id) {
            return &sched.voices[i];
        }
    }
    return NULL;
}

static void cleanup_voice(Voice* v) {
    if (!v->active) return;

    /* Stop timer if running */
    uv_timer_stop(&v->timer);

    /* Unprotect from GC */
    if (!scm_is_false(v->thunk)) {
        scm_gc_unprotect_object(v->thunk);
    }

    v->thunk = SCM_BOOL_F;
    v->active = 0;
    v->waiting = 0;
    v->stop_requested = 0;
    v->name[0] = '\0';

    sched.active_count--;
}

/* ============================================================================
 * Timer Callback (runs on main thread via libuv)
 * ============================================================================ */

static void on_timer(uv_timer_t* handle) {
    Voice* v = (Voice*)handle->data;

    if (!v->active || v->stop_requested) {
        return;
    }

    v->waiting = 0;

    /* Call the thunk - it should return ms to wait or #f */
    SCM result = scm_call_0(v->thunk);

    if (scm_is_false(result)) {
        /* Voice completed */
        cleanup_voice(v);
    } else if (scm_is_integer(result) || scm_is_real(result)) {
        /* Voice wants to wait */
        int64_t wait_ms = scm_is_integer(result)
            ? scm_to_int64(result)
            : (int64_t)scm_to_double(result);
        if (wait_ms < 0) wait_ms = 0;

        /* Set wake time and start timer */
        v->wake_time_ms = now_ms() + (uint64_t)wait_ms;
        v->waiting = 1;
        uv_timer_start(&v->timer, on_timer, (uint64_t)wait_ms, 0);
    } else {
        /* Invalid return - treat as done */
        fprintf(stderr, "Voice '%s': invalid return (expected number or #f)\n",
                v->name[0] ? v->name : "(unnamed)");
        cleanup_voice(v);
    }
}

/* ============================================================================
 * Public API
 * ============================================================================ */

int guile_scheduler_init(void) {
    if (sched.loop != NULL) {
        return 0;  /* Already initialized */
    }

    sched.next_voice_id = 1;
    sched.active_count = 0;
    sched.running = 0;
    sched.shutdown_requested = 0;

    /* Create event loop */
    sched.loop = malloc(sizeof(uv_loop_t));
    if (!sched.loop) {
        return -1;
    }

    if (uv_loop_init(sched.loop) != 0) {
        free(sched.loop);
        sched.loop = NULL;
        return -1;
    }

    /* Initialize voice timer handles */
    for (int i = 0; i < MAX_VOICES; i++) {
        Voice* v = &sched.voices[i];
        v->active = 0;
        v->voice_id = 0;
        v->thunk = SCM_BOOL_F;

        uv_timer_init(sched.loop, &v->timer);
        v->timer.data = v;
    }

    return 0;
}

void guile_scheduler_cleanup(void) {
    if (!sched.loop) return;

    /* Stop all voices */
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active) {
            sched.voices[i].stop_requested = 1;
            uv_timer_stop(&sched.voices[i].timer);
            if (!scm_is_false(sched.voices[i].thunk)) {
                scm_gc_unprotect_object(sched.voices[i].thunk);
            }
        }
    }

    /* Close handles */
    for (int i = 0; i < MAX_VOICES; i++) {
        uv_close((uv_handle_t*)&sched.voices[i].timer, NULL);
    }

    /* Process remaining close callbacks */
    uv_run(sched.loop, UV_RUN_DEFAULT);

    /* Cleanup */
    uv_loop_close(sched.loop);
    free(sched.loop);
    sched.loop = NULL;
}

int guile_scheduler_active_count(void) {
    return sched.active_count;
}

/* ============================================================================
 * Scheme Functions
 * ============================================================================ */

/* (spawn thunk [name]) -> voice-id
 * Creates a new voice from a procedure.
 * The procedure takes no arguments and returns either:
 *   - A number (ms to wait before being called again)
 *   - #f (voice is done)
 */
static SCM g_spawn(SCM thunk, SCM rest) {
    if (!scm_is_true(scm_procedure_p(thunk))) {
        scm_error(scm_from_utf8_symbol("wrong-type-arg"),
                  "spawn", "expected a procedure",
                  SCM_EOL, SCM_EOL);
    }

    const char* name = "";
    if (!scm_is_null(rest)) {
        SCM name_arg = scm_car(rest);
        if (scm_is_string(name_arg)) {
            char *tmp = scm_to_utf8_string(name_arg);
            /* We'll copy this into the voice struct */
            name = tmp;
            free(tmp);
        }
    }

    /* Initialize scheduler if needed */
    if (!sched.loop) {
        if (guile_scheduler_init() != 0) {
            scm_error(scm_from_utf8_symbol("scheduler-error"),
                      "spawn", "Failed to initialize scheduler",
                      SCM_EOL, SCM_EOL);
        }
    }

    Voice* v = find_free_voice();
    if (!v) {
        scm_error(scm_from_utf8_symbol("scheduler-error"),
                  "spawn", "No free voice slots",
                  SCM_EOL, SCM_EOL);
    }

    /* Protect thunk from GC */
    scm_gc_protect_object(thunk);
    v->thunk = thunk;

    /* Initialize voice */
    v->active = 1;
    v->voice_id = sched.next_voice_id++;
    v->waiting = 0;
    v->stop_requested = 0;
    v->wake_time_ms = 0;

    /* Get name again since we freed it */
    if (!scm_is_null(rest)) {
        SCM name_arg = scm_car(rest);
        if (scm_is_string(name_arg)) {
            char *tmp = scm_to_utf8_string(name_arg);
            strncpy(v->name, tmp, sizeof(v->name) - 1);
            v->name[sizeof(v->name) - 1] = '\0';
            free(tmp);
        } else {
            v->name[0] = '\0';
        }
    } else {
        v->name[0] = '\0';
    }

    sched.active_count++;
    int voice_id = v->voice_id;

    /* Schedule immediate call (0ms timer) */
    uv_timer_start(&v->timer, on_timer, 0, 0);

    return scm_from_int(voice_id);
}

/* (run) - Run the scheduler until all voices complete */
static SCM g_run(void) {
    if (!sched.loop) {
        /* No voices spawned yet */
        return SCM_UNSPECIFIED;
    }

    sched.running = 1;

    /* Run the event loop until all voices complete */
    while (sched.active_count > 0 && sched.running) {
        /* Run one iteration of the event loop */
        int result = uv_run(sched.loop, UV_RUN_ONCE);
        if (result == 0 && sched.active_count > 0) {
            /* No pending operations but voices still active - brief sleep */
            uv_sleep(1);
        }
    }

    sched.running = 0;
    return SCM_UNSPECIFIED;
}

/* (poll) - Process any ready voice timers without blocking
 * Returns #t if there are still active voices, #f otherwise
 * Use this for non-blocking playback while keeping REPL responsive
 */
static SCM g_poll(void) {
    if (!sched.loop || sched.active_count == 0) {
        return SCM_BOOL_F;
    }

    /* Process any ready timers without blocking */
    uv_run(sched.loop, UV_RUN_NOWAIT);

    /* Return #t if voices still active */
    return sched.active_count > 0 ? SCM_BOOL_T : SCM_BOOL_F;
}

/* (stop [voice-id]) - Stop a specific voice or all voices */
static SCM g_stop(SCM rest) {
    if (!scm_is_null(rest)) {
        /* Stop specific voice */
        SCM id_arg = scm_car(rest);
        if (!scm_is_integer(id_arg)) {
            scm_error(scm_from_utf8_symbol("wrong-type-arg"),
                      "stop", "expected an integer",
                      SCM_EOL, SCM_EOL);
        }
        int id = scm_to_int(id_arg);
        Voice* v = find_voice_by_id(id);
        if (v) {
            v->stop_requested = 1;
            uv_timer_stop(&v->timer);
            cleanup_voice(v);
            return SCM_BOOL_T;
        }
        return SCM_BOOL_F;
    }

    /* Stop all voices */
    sched.running = 0;
    for (int i = 0; i < MAX_VOICES; i++) {
        if (sched.voices[i].active) {
            sched.voices[i].stop_requested = 1;
            uv_timer_stop(&sched.voices[i].timer);
            cleanup_voice(&sched.voices[i]);
        }
    }
    return SCM_UNSPECIFIED;
}

/* (voices) -> count of active voices */
static SCM g_voices(void) {
    return scm_from_int(sched.active_count);
}

/* (scheduler-status) -> alist with scheduler info */
static SCM g_scheduler_status(void) {
    SCM result = SCM_EOL;

    /* running */
    result = scm_cons(
        scm_cons(scm_from_utf8_symbol("running"),
                 sched.running ? SCM_BOOL_T : SCM_BOOL_F),
        result);

    /* active */
    result = scm_cons(
        scm_cons(scm_from_utf8_symbol("active"),
                 scm_from_int(sched.active_count)),
        result);

    /* voices list */
    SCM voices_list = SCM_EOL;
    for (int i = MAX_VOICES - 1; i >= 0; i--) {
        if (sched.voices[i].active) {
            SCM voice_info = scm_list_3(
                scm_from_int(sched.voices[i].voice_id),
                scm_from_utf8_string(sched.voices[i].name),
                sched.voices[i].waiting ? SCM_BOOL_T : SCM_BOOL_F);
            voices_list = scm_cons(voice_info, voices_list);
        }
    }
    result = scm_cons(
        scm_cons(scm_from_utf8_symbol("voices"), voices_list),
        result);

    return result;
}

/* ============================================================================
 * Module Registration
 * ============================================================================ */

void guile_scheduler_register(void) {
    scm_c_define_gsubr("spawn", 1, 0, 1, g_spawn);
    scm_c_define_gsubr("run", 0, 0, 0, g_run);
    scm_c_define_gsubr("poll", 0, 0, 0, g_poll);
    scm_c_define_gsubr("stop", 0, 0, 1, g_stop);
    scm_c_define_gsubr("voices", 0, 0, 0, g_voices);
    scm_c_define_gsubr("scheduler-status", 0, 0, 0, g_scheduler_status);
}
