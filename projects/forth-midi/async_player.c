/* async_player.c - Asynchronous sequence playback using libuv
 *
 * This module enables non-blocking sequence playback, allowing the REPL
 * to remain responsive while multiple sequences play simultaneously.
 */

#include "forth_midi.h"
#include <uv.h>

/* ============================================================================
 * Constants
 * ============================================================================ */

#define MAX_ASYNC_PLAYERS 8

/* ============================================================================
 * Async Player State
 * ============================================================================ */

typedef struct {
    int active;             /* Is this slot in use? */
    int seq_id;             /* Which sequence is playing */

    /* Sequence playback state */
    Sequence seq_copy;      /* Copy of sequence data */
    int event_index;
    int bpm;
    int stop_requested;
    int looping;            /* Should sequence loop? */

    /* libuv handles */
    uv_timer_t timer;
    uv_async_t stop_async;
} AsyncPlayerSlot;

typedef struct {
    uv_loop_t* loop;
    uv_thread_t thread;
    uv_async_t wake_async;  /* Wake up loop to check for new work */

    AsyncPlayerSlot slots[MAX_ASYNC_PLAYERS];
    int active_count;
    int running;
    int shutdown_requested;

    /* Mutex for thread-safe access */
    uv_mutex_t mutex;
} AsyncPlayerSystem;

static AsyncPlayerSystem async_system = {0};

/* ============================================================================
 * Forward Declarations
 * ============================================================================ */

static void on_timer(uv_timer_t* handle);
static void schedule_next_event(AsyncPlayerSlot* slot);

/* ============================================================================
 * Timer Callback - Fires for each MIDI event
 * ============================================================================ */

static void on_timer(uv_timer_t* handle) {
    AsyncPlayerSlot* slot = (AsyncPlayerSlot*)handle->data;

    if (slot->stop_requested || slot->event_index >= slot->seq_copy.length) {
        slot->active = 0;
        uv_mutex_lock(&async_system.mutex);
        async_system.active_count--;
        uv_mutex_unlock(&async_system.mutex);
        return;
    }

    /* Send current event */
    MidiEvent* e = &slot->seq_copy.events[slot->event_index];

    unsigned char msg[3];
    switch (e->type) {
        case EVT_NOTE_ON:
            msg[0] = 0x90 | (e->channel & 0x0F);
            msg[1] = e->data1 & 0x7F;
            msg[2] = e->data2 & 0x7F;
            break;
        case EVT_NOTE_OFF:
            msg[0] = 0x80 | (e->channel & 0x0F);
            msg[1] = e->data1 & 0x7F;
            msg[2] = 0;
            break;
        case EVT_CC:
            msg[0] = 0xB0 | (e->channel & 0x0F);
            msg[1] = e->data1 & 0x7F;
            msg[2] = e->data2 & 0x7F;
            break;
        default:
            slot->event_index++;
            schedule_next_event(slot);
            return;
    }

    if (midi_out != NULL) {
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }

    slot->event_index++;
    schedule_next_event(slot);
}

static void schedule_next_event(AsyncPlayerSlot* slot) {
    if (slot->stop_requested) {
        slot->active = 0;
        uv_mutex_lock(&async_system.mutex);
        async_system.active_count--;
        uv_mutex_unlock(&async_system.mutex);
        return;
    }

    /* Check if we've reached the end */
    if (slot->event_index >= slot->seq_copy.length) {
        if (slot->looping) {
            /* Restart from beginning */
            slot->event_index = 0;
        } else {
            /* Done playing */
            slot->active = 0;
            uv_mutex_lock(&async_system.mutex);
            async_system.active_count--;
            uv_mutex_unlock(&async_system.mutex);
            return;
        }
    }

    MidiEvent* curr = &slot->seq_copy.events[slot->event_index];
    MidiEvent* prev = slot->event_index > 0 ?
                      &slot->seq_copy.events[slot->event_index - 1] : NULL;

    int prev_time = prev ? prev->time : 0;
    int ticks = curr->time - prev_time;
    int ms = (ticks * 60000) / (TICKS_PER_QUARTER * slot->bpm);
    if (ms < 0) ms = 0;

    uv_timer_start(&slot->timer, on_timer, ms, 0);
}

/* ============================================================================
 * Stop Signal Handler
 * ============================================================================ */

static void on_stop_signal(uv_async_t* handle) {
    AsyncPlayerSlot* slot = (AsyncPlayerSlot*)handle->data;
    slot->stop_requested = 1;
    uv_timer_stop(&slot->timer);
    if (slot->active) {
        slot->active = 0;
        uv_mutex_lock(&async_system.mutex);
        async_system.active_count--;
        uv_mutex_unlock(&async_system.mutex);
    }
}

/* ============================================================================
 * Wake Handler - Check for shutdown
 * ============================================================================ */

static void on_wake(uv_async_t* handle) {
    (void)handle;
    if (async_system.shutdown_requested) {
        uv_stop(async_system.loop);
    }
}

/* ============================================================================
 * Event Loop Thread
 * ============================================================================ */

static void async_thread_fn(void* arg) {
    (void)arg;

    /* Run the event loop until shutdown */
    while (!async_system.shutdown_requested) {
        uv_run(async_system.loop, UV_RUN_DEFAULT);

        /* If loop stopped but not shutting down, there might be more work */
        if (!async_system.shutdown_requested) {
            /* Brief sleep to avoid busy-waiting */
            uv_sleep(10);
        }
    }

    async_system.running = 0;
}

/* ============================================================================
 * Helper: Sort events by time
 * ============================================================================ */

static void seq_sort_for_async(Sequence* seq) {
    for (int i = 1; i < seq->length; i++) {
        MidiEvent tmp = seq->events[i];
        int j = i - 1;
        while (j >= 0 && seq->events[j].time > tmp.time) {
            seq->events[j + 1] = seq->events[j];
            j--;
        }
        seq->events[j + 1] = tmp;
    }
}

/* ============================================================================
 * Public API
 * ============================================================================ */

/* Initialize the async player system (call once at startup) */
int async_player_init(void) {
    if (async_system.loop != NULL) {
        return 0;  /* Already initialized */
    }

    async_system.loop = malloc(sizeof(uv_loop_t));
    if (async_system.loop == NULL) {
        return -1;
    }

    if (uv_loop_init(async_system.loop) != 0) {
        free(async_system.loop);
        async_system.loop = NULL;
        return -1;
    }

    uv_mutex_init(&async_system.mutex);

    /* Initialize wake async handle */
    uv_async_init(async_system.loop, &async_system.wake_async, on_wake);

    /* Initialize all slots */
    for (int i = 0; i < MAX_ASYNC_PLAYERS; i++) {
        AsyncPlayerSlot* slot = &async_system.slots[i];
        slot->active = 0;
        slot->seq_id = -1;

        uv_timer_init(async_system.loop, &slot->timer);
        slot->timer.data = slot;

        uv_async_init(async_system.loop, &slot->stop_async, on_stop_signal);
        slot->stop_async.data = slot;
    }

    /* Start the event loop thread */
    async_system.running = 1;
    async_system.shutdown_requested = 0;
    if (uv_thread_create(&async_system.thread, async_thread_fn, NULL) != 0) {
        async_system.running = 0;
        uv_loop_close(async_system.loop);
        free(async_system.loop);
        async_system.loop = NULL;
        return -1;
    }

    return 0;
}

/* Cleanup the async player system (call at shutdown) */
void async_player_cleanup(void) {
    if (async_system.loop == NULL) return;

    /* Stop all playing sequences */
    async_player_stop_all();

    /* Signal shutdown */
    async_system.shutdown_requested = 1;
    uv_async_send(&async_system.wake_async);

    /* Wait for thread to finish */
    if (async_system.running) {
        uv_thread_join(&async_system.thread);
    }

    /* Close all handles */
    for (int i = 0; i < MAX_ASYNC_PLAYERS; i++) {
        uv_close((uv_handle_t*)&async_system.slots[i].timer, NULL);
        uv_close((uv_handle_t*)&async_system.slots[i].stop_async, NULL);
    }
    uv_close((uv_handle_t*)&async_system.wake_async, NULL);

    /* Run loop to process close callbacks */
    uv_run(async_system.loop, UV_RUN_DEFAULT);

    uv_mutex_destroy(&async_system.mutex);
    uv_loop_close(async_system.loop);
    free(async_system.loop);
    async_system.loop = NULL;
}

/* Find a free slot */
static AsyncPlayerSlot* find_free_slot(void) {
    for (int i = 0; i < MAX_ASYNC_PLAYERS; i++) {
        if (!async_system.slots[i].active) {
            return &async_system.slots[i];
        }
    }
    return NULL;
}

/* Find slot playing a specific sequence */
static AsyncPlayerSlot* find_slot_for_seq(int seq_id) {
    for (int i = 0; i < MAX_ASYNC_PLAYERS; i++) {
        if (async_system.slots[i].active &&
            async_system.slots[i].seq_id == seq_id) {
            return &async_system.slots[i];
        }
    }
    return NULL;
}

/* Internal: Start async playback of a sequence with options */
static int async_player_start_internal(int seq_id, int looping) {
    if (async_system.loop == NULL) {
        if (async_player_init() != 0) {
            printf("Failed to initialize async player\n");
            return -1;
        }
    }

    if (seq_id < 0 || seq_id >= sequence_count) {
        printf("Invalid sequence id: %d\n", seq_id);
        return -1;
    }

    Sequence* seq = &sequences[seq_id];
    if (seq->length == 0) {
        printf("Sequence %d is empty\n", seq_id);
        return -1;
    }

    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return -1;
    }

    uv_mutex_lock(&async_system.mutex);

    /* Check if this sequence is already playing */
    AsyncPlayerSlot* existing = find_slot_for_seq(seq_id);
    if (existing) {
        uv_mutex_unlock(&async_system.mutex);
        printf("Sequence %d is already playing\n", seq_id);
        return -1;
    }

    /* Find a free slot */
    AsyncPlayerSlot* slot = find_free_slot();
    if (slot == NULL) {
        uv_mutex_unlock(&async_system.mutex);
        printf("No free async player slots (max %d)\n", MAX_ASYNC_PLAYERS);
        return -1;
    }

    /* Copy sequence data (so original can be modified) */
    memcpy(&slot->seq_copy, seq, sizeof(Sequence));
    seq_sort_for_async(&slot->seq_copy);

    /* Setup playback state */
    slot->seq_id = seq_id;
    slot->event_index = 0;
    slot->bpm = seq->bpm > 0 ? seq->bpm : global_bpm;
    slot->stop_requested = 0;
    slot->looping = looping;
    slot->active = 1;
    async_system.active_count++;

    uv_mutex_unlock(&async_system.mutex);

    /* Calculate time to first event */
    int first_ms = 0;
    if (slot->seq_copy.length > 0 && slot->seq_copy.events[0].time > 0) {
        first_ms = (slot->seq_copy.events[0].time * 60000) /
                   (TICKS_PER_QUARTER * slot->bpm);
    }

    /* Start timer for first event */
    uv_timer_start(&slot->timer, on_timer, first_ms, 0);

    printf("Started %s playback of sequence %d\n",
           looping ? "looping" : "async", seq_id);
    return 0;
}

/* Start async playback of a sequence (one-shot) */
int async_player_start(int seq_id) {
    return async_player_start_internal(seq_id, 0);
}

/* Start looping playback of a sequence */
int async_player_loop(int seq_id) {
    return async_player_start_internal(seq_id, 1);
}

/* Stop async playback of a specific sequence */
int async_player_stop_seq(int seq_id) {
    uv_mutex_lock(&async_system.mutex);
    AsyncPlayerSlot* slot = find_slot_for_seq(seq_id);
    if (slot == NULL) {
        uv_mutex_unlock(&async_system.mutex);
        return -1;
    }
    uv_mutex_unlock(&async_system.mutex);

    slot->stop_requested = 1;
    uv_async_send(&slot->stop_async);

    printf("Stopped sequence %d\n", seq_id);
    return 0;
}

/* Stop all async playback */
void async_player_stop_all(void) {
    for (int i = 0; i < MAX_ASYNC_PLAYERS; i++) {
        if (async_system.slots[i].active) {
            async_system.slots[i].stop_requested = 1;
            uv_async_send(&async_system.slots[i].stop_async);
        }
    }
    printf("Stopped all async playback\n");
}

/* Legacy single-stop (stops all) */
void async_player_stop(void) {
    async_player_stop_all();
}

/* Check if any async playback is active */
int async_player_is_playing(void) {
    uv_mutex_lock(&async_system.mutex);
    int count = async_system.active_count;
    uv_mutex_unlock(&async_system.mutex);
    return count > 0;
}

/* Get count of active players */
int async_player_active_count(void) {
    uv_mutex_lock(&async_system.mutex);
    int count = async_system.active_count;
    uv_mutex_unlock(&async_system.mutex);
    return count;
}

/* ============================================================================
 * Forth Words
 * ============================================================================ */

/* seq-play& ( -- ) Play current sequence asynchronously */
void op_seq_play_async(Stack* s) {
    (void)s;
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    async_player_start(current_seq);
}

/* seq-loop& ( -- ) Play current sequence in a loop */
void op_seq_loop_async(Stack* s) {
    (void)s;
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    async_player_loop(current_seq);
}

/* seq-stop ( -- ) Stop current sequence's async playback */
void op_seq_stop(Stack* s) {
    (void)s;
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    if (async_player_stop_seq(current_seq) != 0) {
        printf("Sequence %d is not playing\n", current_seq);
    }
}

/* seq-stop-all ( -- ) Stop all async playback */
void op_seq_stop_all(Stack* s) {
    (void)s;
    async_player_stop_all();
}

/* seq-playing? ( -- flag ) Check if any async playback is active */
void op_seq_playing(Stack* s) {
    push(&stack, async_player_is_playing() ? -1 : 0);
}

/* seq-active ( -- n ) Get count of active async players */
void op_seq_active(Stack* s) {
    push(&stack, async_player_active_count());
}
