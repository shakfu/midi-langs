/**
 * @file async.c
 * @brief Asynchronous event playback for Alda interpreter.
 *
 * This module enables non-blocking playback using libuv, allowing the REPL
 * to remain responsive while music plays.
 */

#include "alda/async.h"
#include "alda/scheduler.h"
#include <uv.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============================================================================
 * Async Playback State
 * ============================================================================ */

typedef struct {
    /* libuv event loop and thread */
    uv_loop_t* loop;
    uv_thread_t thread;
    uv_async_t wake_async;
    uv_mutex_t mutex;

    /* Playback state */
    int running;
    int shutdown_requested;
    int playing;

    /* Event timer */
    uv_timer_t timer;

    /* Event data (copy of scheduled events) */
    AldaScheduledEvent* events;
    int event_count;
    int event_index;
    int tempo;

    /* MIDI output handle (borrowed from context) */
    libremidi_midi_out_handle* midi_out;

    /* Stop request */
    uv_async_t stop_async;
    int stop_requested;

} AsyncPlaybackSystem;

static AsyncPlaybackSystem async_sys = {0};

/* ============================================================================
 * Forward Declarations
 * ============================================================================ */

static void on_timer(uv_timer_t* handle);
static void schedule_next_event(void);

/* ============================================================================
 * Timer Callback - Fires for each MIDI event
 * ============================================================================ */

static void send_event(AldaScheduledEvent* evt) {
    if (!async_sys.midi_out) return;

    unsigned char msg[3];
    int channel = evt->channel;  /* Already 0-based */

    switch (evt->type) {
        case ALDA_EVT_NOTE_ON:
            msg[0] = 0x90 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            msg[2] = evt->data2 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;

        case ALDA_EVT_NOTE_OFF:
            msg[0] = 0x80 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            msg[2] = 0;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;

        case ALDA_EVT_PROGRAM:
            msg[0] = 0xC0 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 2);
            break;

        case ALDA_EVT_CC:
            msg[0] = 0xB0 | (channel & 0x0F);
            msg[1] = evt->data1 & 0x7F;
            msg[2] = evt->data2 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;

        case ALDA_EVT_PAN:
            msg[0] = 0xB0 | (channel & 0x0F);
            msg[1] = 10;  /* CC 10 = Pan */
            msg[2] = evt->data1 & 0x7F;
            libremidi_midi_out_send_message(async_sys.midi_out, msg, 3);
            break;
    }
}

static void on_timer(uv_timer_t* handle) {
    (void)handle;

    if (async_sys.stop_requested || async_sys.event_index >= async_sys.event_count) {
        /* Done playing */
        uv_mutex_lock(&async_sys.mutex);
        async_sys.playing = 0;
        uv_mutex_unlock(&async_sys.mutex);
        return;
    }

    /* Send current event */
    AldaScheduledEvent* evt = &async_sys.events[async_sys.event_index];
    send_event(evt);

    async_sys.event_index++;
    schedule_next_event();
}

static void schedule_next_event(void) {
    if (async_sys.stop_requested) {
        uv_mutex_lock(&async_sys.mutex);
        async_sys.playing = 0;
        uv_mutex_unlock(&async_sys.mutex);
        return;
    }

    if (async_sys.event_index >= async_sys.event_count) {
        /* Done playing */
        uv_mutex_lock(&async_sys.mutex);
        async_sys.playing = 0;
        uv_mutex_unlock(&async_sys.mutex);
        return;
    }

    AldaScheduledEvent* curr = &async_sys.events[async_sys.event_index];
    int prev_tick = (async_sys.event_index > 0) ?
                    async_sys.events[async_sys.event_index - 1].tick : 0;

    int delta_ticks = curr->tick - prev_tick;
    int ms = alda_ticks_to_ms(delta_ticks, async_sys.tempo);
    if (ms < 0) ms = 0;

    uv_timer_start(&async_sys.timer, on_timer, ms, 0);
}

/* ============================================================================
 * Stop Signal Handler
 * ============================================================================ */

static void on_stop_signal(uv_async_t* handle) {
    (void)handle;
    async_sys.stop_requested = 1;
    uv_timer_stop(&async_sys.timer);
    uv_mutex_lock(&async_sys.mutex);
    async_sys.playing = 0;
    uv_mutex_unlock(&async_sys.mutex);
}

/* ============================================================================
 * Wake Handler - Check for shutdown
 * ============================================================================ */

static void on_wake(uv_async_t* handle) {
    (void)handle;
    if (async_sys.shutdown_requested) {
        uv_stop(async_sys.loop);
    }
}

/* ============================================================================
 * Event Loop Thread
 * ============================================================================ */

static void async_thread_fn(void* arg) {
    (void)arg;

    while (!async_sys.shutdown_requested) {
        uv_run(async_sys.loop, UV_RUN_DEFAULT);

        if (!async_sys.shutdown_requested) {
            /* Brief sleep to avoid busy-waiting */
            uv_sleep(10);
        }
    }

    async_sys.running = 0;
}

/* ============================================================================
 * Public API
 * ============================================================================ */

int alda_async_init(void) {
    if (async_sys.loop != NULL) {
        return 0;  /* Already initialized */
    }

    async_sys.loop = malloc(sizeof(uv_loop_t));
    if (!async_sys.loop) {
        return -1;
    }

    if (uv_loop_init(async_sys.loop) != 0) {
        free(async_sys.loop);
        async_sys.loop = NULL;
        return -1;
    }

    uv_mutex_init(&async_sys.mutex);

    /* Initialize wake async handle */
    uv_async_init(async_sys.loop, &async_sys.wake_async, on_wake);

    /* Initialize timer */
    uv_timer_init(async_sys.loop, &async_sys.timer);

    /* Initialize stop async handle */
    uv_async_init(async_sys.loop, &async_sys.stop_async, on_stop_signal);

    /* Start the event loop thread */
    async_sys.running = 1;
    async_sys.shutdown_requested = 0;
    async_sys.playing = 0;

    if (uv_thread_create(&async_sys.thread, async_thread_fn, NULL) != 0) {
        async_sys.running = 0;
        uv_loop_close(async_sys.loop);
        free(async_sys.loop);
        async_sys.loop = NULL;
        return -1;
    }

    return 0;
}

void alda_async_cleanup(void) {
    if (!async_sys.loop) return;

    /* Stop any playback */
    alda_async_stop();

    /* Signal shutdown */
    async_sys.shutdown_requested = 1;
    uv_async_send(&async_sys.wake_async);

    /* Wait for thread to finish */
    if (async_sys.running) {
        uv_thread_join(&async_sys.thread);
    }

    /* Close handles */
    uv_close((uv_handle_t*)&async_sys.timer, NULL);
    uv_close((uv_handle_t*)&async_sys.stop_async, NULL);
    uv_close((uv_handle_t*)&async_sys.wake_async, NULL);

    /* Run loop to process close callbacks */
    uv_run(async_sys.loop, UV_RUN_DEFAULT);

    /* Free event copy */
    if (async_sys.events) {
        free(async_sys.events);
        async_sys.events = NULL;
    }

    uv_mutex_destroy(&async_sys.mutex);
    uv_loop_close(async_sys.loop);
    free(async_sys.loop);
    async_sys.loop = NULL;
}

int alda_events_play_async(AldaContext* ctx) {
    if (!ctx) return -1;

    if (ctx->event_count == 0) {
        return 0;  /* Nothing to play */
    }

    if (!ctx->midi_out) {
        fprintf(stderr, "No MIDI output open\n");
        return -1;
    }

    /* Initialize async system if needed */
    if (!async_sys.loop) {
        if (alda_async_init() != 0) {
            fprintf(stderr, "Failed to initialize async playback\n");
            return -1;
        }
    }

    /* Wait for any previous playback to complete */
    while (alda_async_is_playing()) {
        uv_sleep(10);
    }

    /* Sort events */
    alda_events_sort(ctx);

    /* Copy events */
    uv_mutex_lock(&async_sys.mutex);

    if (async_sys.events) {
        free(async_sys.events);
    }

    async_sys.events = malloc(ctx->event_count * sizeof(AldaScheduledEvent));
    if (!async_sys.events) {
        uv_mutex_unlock(&async_sys.mutex);
        fprintf(stderr, "Failed to allocate event buffer\n");
        return -1;
    }

    memcpy(async_sys.events, ctx->events, ctx->event_count * sizeof(AldaScheduledEvent));
    async_sys.event_count = ctx->event_count;
    async_sys.event_index = 0;
    async_sys.tempo = ctx->global_tempo > 0 ? ctx->global_tempo : ALDA_DEFAULT_TEMPO;
    async_sys.midi_out = ctx->midi_out;
    async_sys.stop_requested = 0;
    async_sys.playing = 1;

    uv_mutex_unlock(&async_sys.mutex);

    /* Schedule first event */
    int first_ms = 0;
    if (async_sys.event_count > 0 && async_sys.events[0].tick > 0) {
        first_ms = alda_ticks_to_ms(async_sys.events[0].tick, async_sys.tempo);
    }

    uv_timer_start(&async_sys.timer, on_timer, first_ms, 0);

    /* Wake the event loop thread */
    uv_async_send(&async_sys.wake_async);

    return 0;
}

void alda_async_stop(void) {
    if (!async_sys.loop) return;

    async_sys.stop_requested = 1;
    uv_async_send(&async_sys.stop_async);
}

int alda_async_is_playing(void) {
    if (!async_sys.loop) return 0;

    uv_mutex_lock(&async_sys.mutex);
    int playing = async_sys.playing;
    uv_mutex_unlock(&async_sys.mutex);

    return playing;
}

int alda_async_wait(int timeout_ms) {
    if (!async_sys.loop) return 0;

    int waited = 0;
    int interval = 10;

    while (alda_async_is_playing()) {
        uv_sleep(interval);
        waited += interval;

        if (timeout_ms > 0 && waited >= timeout_ms) {
            return -1;  /* Timeout */
        }
    }

    return 0;
}
