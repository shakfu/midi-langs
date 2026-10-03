/**
 * @file finalize.c
 * @brief Turn the interpreter's notes and tempo changes into MIDI events.
 *
 * What happens here follows Alda 2.4.7, checked against `alda export`
 * (docs/dev/conformance.md):
 *
 * - The tempo map is the first declared part's tempo changes, overridden by
 *   (tempo! N) at the same time. Other parts' tempos only place their own
 *   notes, which are already in seconds.
 * - A channel is sent its program, pan (CC 10) and track volume (CC 11) with
 *   the first note that needs a value different from the one it holds, so a
 *   channel never keeps a previous part's settings.
 * - Parts that fit get one channel each in declaration order. Beyond 15
 *   melodic parts, a channel passes to another part once its part stops
 *   sounding. Channel numbers themselves may differ from Alda's; the program
 *   and controller values each note plays with may not.
 */

#include "finalize.h"
#include "alda/scheduler.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define DRUM_CHANNEL 9
#define EPSILON 1e-9

/* ============================================================================
 * Recording
 * ============================================================================ */

int alda_record_note(AldaContext* ctx, AldaPartState* part, double start,
                     double duration, int pitch, int velocity) {
    if (ctx->note_count == ctx->note_capacity) {
        int capacity = ctx->note_capacity ? ctx->note_capacity * 2 : 1024;
        AldaNoteRecord* notes = realloc(ctx->notes, sizeof(AldaNoteRecord) * (size_t)capacity);
        if (!notes) {
            fprintf(stderr, "Error: Out of memory\n");
            return -1;
        }
        ctx->notes = notes;
        ctx->note_capacity = capacity;
    }

    AldaNoteRecord* n = &ctx->notes[ctx->note_count++];
    n->start = start;
    n->duration = duration > 0.0 ? duration : 0.0;
    n->pitch = pitch;
    n->velocity = velocity;
    n->part_index = (int)(part - ctx->parts);
    n->program = part->program;
    n->pan = part->pan;
    n->track_volume = part->track_volume;
    n->percussion = part->percussion;
    n->pinned_channel = part->pinned_channel;
    n->owner = n->part_index;
#ifdef ALDA_SOURCE_TRACKING
    n->source_line = ctx->source_tracking_line;
#else
    n->source_line = 0;
#endif
    return 0;
}

int alda_record_tempo(AldaContext* ctx, double time, int tempo, int global) {
    if (ctx->tempo_change_count == ctx->tempo_change_capacity) {
        int capacity = ctx->tempo_change_capacity ? ctx->tempo_change_capacity * 2 : 16;
        AldaTempoChange* changes = realloc(ctx->tempo_changes,
                                           sizeof(AldaTempoChange) * (size_t)capacity);
        if (!changes) {
            fprintf(stderr, "Error: Out of memory\n");
            return -1;
        }
        ctx->tempo_changes = changes;
        ctx->tempo_change_capacity = capacity;
    }

    AldaTempoChange* c = &ctx->tempo_changes[ctx->tempo_change_count++];
    c->time = time;
    c->tempo = tempo;
    c->global = global;
    return 0;
}

int alda_record_voice_group_end(AldaContext* ctx, int part_index) {
    if (ctx->voice_group_end_count == ctx->voice_group_end_capacity) {
        int capacity = ctx->voice_group_end_capacity ? ctx->voice_group_end_capacity * 2 : 16;
        AldaVoiceGroupEnd* ends = realloc(ctx->voice_group_ends,
                                          sizeof(AldaVoiceGroupEnd) * (size_t)capacity);
        if (!ends) {
            fprintf(stderr, "Error: Out of memory\n");
            return -1;
        }
        ctx->voice_group_ends = ends;
        ctx->voice_group_end_capacity = capacity;
    }
    ctx->voice_group_ends[ctx->voice_group_end_count++] =
        (AldaVoiceGroupEnd){part_index, ctx->note_count};
    return 0;
}

/* ============================================================================
 * Tempo map
 * ============================================================================ */

typedef struct {
    double time;
    int tempo;
    int explicit_change;  /* 0 for the tempo the score starts from */
    double tick;          /* Ticks elapsed at this point */
} TempoPoint;

static int compare_points(const void* a, const void* b) {
    const TempoPoint* pa = a;
    const TempoPoint* pb = b;
    return (pa->time > pb->time) - (pa->time < pb->time);
}

/* One tempo per moment; a global change wins over the first part's at the
 * same time. Returns the number of points written to `points`. */
static int build_tempo_map(AldaContext* ctx, TempoPoint* points) {
    int count = 0;
    points[count++] = (TempoPoint){0.0, ctx->base_tempo, 0, 0.0};

    for (int pass = 0; pass < 2; pass++) {
        for (int i = 0; i < ctx->tempo_change_count; i++) {
            AldaTempoChange* c = &ctx->tempo_changes[i];
            if (c->global != pass) continue;  /* Part changes first, globals win */
            int j = 0;
            while (j < count && (points[j].time > c->time + EPSILON ||
                                 points[j].time < c->time - EPSILON)) {
                j++;
            }
            if (j == count) count++;
            points[j] = (TempoPoint){c->time, c->tempo, 1, 0.0};
        }
    }

    qsort(points, (size_t)count, sizeof(TempoPoint), compare_points);
    for (int i = 1; i < count; i++) {
        points[i].tick = points[i - 1].tick + (points[i].time - points[i - 1].time)
                       * points[i - 1].tempo / 60.0 * ALDA_TICKS_PER_QUARTER;
    }
    return count;
}

static int time_to_tick(const TempoPoint* points, int count, double time) {
    int i = 0;
    while (i + 1 < count && points[i + 1].time <= time + EPSILON) i++;
    double tick = points[i].tick
                + (time - points[i].time) * points[i].tempo / 60.0 * ALDA_TICKS_PER_QUARTER;
    return (int)(tick + 0.5);
}

/* ============================================================================
 * Channels
 * ============================================================================ */

typedef struct {
    int owner;     /* AldaNoteRecord.owner */
    double start;
    double end;
    int channel;
} Run;

static const AldaNoteRecord* g_sort_notes;

/* Order note indices by owner, then start, then position in the score. */
static int compare_by_owner(const void* a, const void* b) {
    int ia = *(const int*)a;
    int ib = *(const int*)b;
    const AldaNoteRecord* na = &g_sort_notes[ia];
    const AldaNoteRecord* nb = &g_sort_notes[ib];
    if (na->owner != nb->owner) return na->owner - nb->owner;
    if (na->start != nb->start) return (na->start > nb->start) - (na->start < nb->start);
    return ia - ib;
}

static int compare_by_start(const void* a, const void* b) {
    int ia = *(const int*)a;
    int ib = *(const int*)b;
    const AldaNoteRecord* na = &g_sort_notes[ia];
    const AldaNoteRecord* nb = &g_sort_notes[ib];
    if (na->start != nb->start) return (na->start > nb->start) - (na->start < nb->start);
    return ia - ib;
}

static int compare_runs(const void* a, const void* b) {
    const Run* ra = a;
    const Run* rb = b;
    if (ra->start != rb->start) return (ra->start > rb->start) - (ra->start < rb->start);
    if (ra->owner != rb->owner) return ra->owner - rb->owner;
    return (ra->end > rb->end) - (ra->end < rb->end);
}

static int movable(const AldaNoteRecord* note, int owner) {
    return note->owner == owner && !note->percussion && note->pinned_channel < 0;
}

/* Move a part to a channel of its own after a voice group, when it must.
 *
 * Alda moves a part to a new channel after every voice group. psnd keeps the
 * channel unless a note from before the group's end is still sounding when the
 * part plays the same pitch: on one channel the new note would cut the old one
 * off, where Alda sounds both (docs/dev/conformance.md). The rest of the part
 * gets a new owner, which assign_channels() treats as a part of its own.
 * Returns the number of owners. */
static int separate_voice_group_overlaps(AldaContext* ctx) {
    int owners = ctx->part_count;
    int current[ALDA_MAX_PARTS];
    for (int p = 0; p < ALDA_MAX_PARTS; p++) current[p] = p;

    for (int g = 0; g < ctx->voice_group_end_count; g++) {
        AldaVoiceGroupEnd* end = &ctx->voice_group_ends[g];
        int owner = current[end->part_index];

        double first = -1.0;
        for (int i = end->note_index; i < ctx->note_count; i++) {
            AldaNoteRecord* n = &ctx->notes[i];
            if (movable(n, owner) && (first < 0.0 || n->start < first)) first = n->start;
        }
        if (first < 0.0) continue;

        int clash = 0;
        for (int i = 0; i < end->note_index && !clash; i++) {
            AldaNoteRecord* e = &ctx->notes[i];
            if (!movable(e, owner) || e->start + e->duration <= first + EPSILON) continue;
            for (int j = end->note_index; j < ctx->note_count && !clash; j++) {
                AldaNoteRecord* n = &ctx->notes[j];
                clash = movable(n, owner) && n->pitch == e->pitch &&
                        e->start <= n->start && n->start < e->start + e->duration - EPSILON;
            }
        }
        if (!clash) continue;

        for (int i = end->note_index; i < ctx->note_count; i++) {
            if (movable(&ctx->notes[i], owner)) ctx->notes[i].owner = owners;
        }
        current[end->part_index] = owners++;
    }
    return owners;
}

/* Fill channels[i] for every note. Returns 0, or -1 when out of memory. */
static int assign_channels(AldaContext* ctx, int* channels) {
    int n = ctx->note_count;
    int pinned[16] = {0};
    int owner_limit = separate_voice_group_overlaps(ctx);
    int* owner_channel = malloc(sizeof(int) * (size_t)owner_limit);
    int* last_used = malloc(sizeof(int) * (size_t)owner_limit);
    int owners = 0;

    if (!owner_channel || !last_used) {
        free(owner_channel);
        free(last_used);
        fprintf(stderr, "Error: Out of memory\n");
        return -1;
    }
    for (int i = 0; i < owner_limit; i++) {
        owner_channel[i] = -2;  /* -2: silent */
        last_used[i] = -1;
    }
    for (int i = 0; i < n; i++) {
        AldaNoteRecord* note = &ctx->notes[i];
        if (note->percussion) continue;
        if (note->pinned_channel >= 0) {
            pinned[note->pinned_channel] = 1;
        } else if (owner_channel[note->owner] == -2) {
            owner_channel[note->owner] = -1;
            owners++;
        }
    }

    int pool[16];
    int pool_size = 0;
    for (int c = 0; c < 16; c++) {
        if (c != DRUM_CHANNEL && !pinned[c]) pool[pool_size++] = c;
    }
    if (pool_size == 0) {
        /* Every melodic channel is pinned; share them rather than fail */
        for (int c = 0; c < 16; c++) if (c != DRUM_CHANNEL) pool[pool_size++] = c;
    }

    int* run_of = NULL;  /* Run index for each note, when channels are reused */
    Run* runs = NULL;

    if (owners <= pool_size) {
        /* One channel per part, in declaration order */
        int next = 0;
        for (int o = 0; o < owner_limit; o++) {
            if (owner_channel[o] == -1) owner_channel[o] = pool[next++];
        }
    } else {
        /* A part holds a channel only while it sounds. Group each part's notes
         * into stretches that touch or overlap, then hand out channels
         * stretch by stretch, preferring the channel the part had last and
         * otherwise the one free the longest. */
        int* order = malloc(sizeof(int) * (size_t)n);
        run_of = malloc(sizeof(int) * (size_t)n);
        runs = malloc(sizeof(Run) * (size_t)n);
        if (!order || !run_of || !runs) {
            free(order);
            free(run_of);
            free(runs);
            free(owner_channel);
            free(last_used);
            fprintf(stderr, "Error: Out of memory\n");
            return -1;
        }

        int count = 0;
        for (int i = 0; i < n; i++) order[count++] = i;
        g_sort_notes = ctx->notes;
        qsort(order, (size_t)count, sizeof(int), compare_by_owner);

        int run_count = 0;
        for (int k = 0; k < count; k++) {
            AldaNoteRecord* note = &ctx->notes[order[k]];
            run_of[order[k]] = -1;
            if (note->percussion || note->pinned_channel >= 0) continue;
            double end = note->start + note->duration;
            Run* last = run_count ? &runs[run_count - 1] : NULL;
            if (last && last->owner == note->owner && note->start <= last->end + EPSILON) {
                if (end > last->end) last->end = end;
            } else {
                runs[run_count++] = (Run){note->owner, note->start, end, -1};
            }
            run_of[order[k]] = run_count - 1;
        }
        free(order);

        /* Sort a copy so run_of keeps pointing at the right entries */
        Run* by_time = malloc(sizeof(Run) * (size_t)(run_count ? run_count : 1));
        if (!by_time) {
            free(run_of);
            free(runs);
            free(owner_channel);
            free(last_used);
            fprintf(stderr, "Error: Out of memory\n");
            return -1;
        }
        for (int r = 0; r < run_count; r++) {
            by_time[r] = runs[r];
            by_time[r].channel = r;  /* Remember the original index */
        }
        qsort(by_time, (size_t)run_count, sizeof(Run), compare_runs);

        double free_from[16];
        for (int c = 0; c < 16; c++) free_from[c] = -1e300;

        for (int r = 0; r < run_count; r++) {
            Run* run = &by_time[r];
            int preferred = last_used[run->owner];
            int choice = -1;
            if (preferred >= 0 && free_from[preferred] <= run->start + EPSILON) {
                choice = preferred;
            } else {
                for (int k = 0; k < pool_size; k++) {
                    int c = pool[k];
                    if (free_from[c] > run->start + EPSILON) continue;
                    if (choice < 0 || free_from[c] < free_from[choice]) choice = c;
                }
                if (choice < 0) {
                    /* More parts sound at once than there are channels */
                    for (int k = 0; k < pool_size; k++) {
                        int c = pool[k];
                        if (choice < 0 || free_from[c] < free_from[choice]) choice = c;
                    }
                }
            }
            if (run->end > free_from[choice]) free_from[choice] = run->end;
            last_used[run->owner] = choice;
            runs[run->channel].channel = choice;
        }
        free(by_time);
    }

    for (int i = 0; i < n; i++) {
        AldaNoteRecord* note = &ctx->notes[i];
        if (note->percussion) {
            channels[i] = DRUM_CHANNEL;
        } else if (note->pinned_channel >= 0) {
            channels[i] = note->pinned_channel;
        } else if (runs) {
            channels[i] = runs[run_of[i]].channel;
        } else {
            channels[i] = owner_channel[note->owner];
        }
    }

    free(run_of);
    free(runs);
    free(owner_channel);
    free(last_used);
    return 0;
}

/* ============================================================================
 * Finalize
 * ============================================================================ */

static int schedule(AldaContext* ctx, int tick, AldaEventType type, int channel,
                    int data1, int data2, int part_index, int source_line) {
#ifdef ALDA_SOURCE_TRACKING
    ctx->source_tracking_line = source_line;
#else
    (void)source_line;
#endif
    return alda_schedule_event(ctx, tick, type, channel, data1, data2, part_index);
}

int alda_events_finalize(AldaContext* ctx) {
    int n = ctx->note_count;
    int result = -1;

    TempoPoint* points = malloc(sizeof(TempoPoint) * (size_t)(ctx->tempo_change_count + 1));
    int* channels = malloc(sizeof(int) * (size_t)(n ? n : 1));
    int* order = malloc(sizeof(int) * (size_t)(n ? n : 1));
    if (!points || !channels || !order) {
        fprintf(stderr, "Error: Out of memory\n");
        goto done;
    }

    int point_count = build_tempo_map(ctx, points);
    if (assign_channels(ctx, channels) < 0) goto done;

    /* Tempo events. The players start at ctx->global_tempo, so the starting
     * tempo needs an event only when it differs from that. */
    for (int i = 0; i < point_count; i++) {
        TempoPoint* p = &points[i];
        int needed = p->explicit_change ||
                     (p->time <= EPSILON && p->tempo != ctx->global_tempo);
        if (needed && schedule(ctx, time_to_tick(points, point_count, p->time),
                               ALDA_EVT_TEMPO, -1, p->tempo, 0, -1, 0) < 0) {
            goto done;
        }
    }

    /* Notes in time order, each preceded by the channel settings it needs */
    for (int i = 0; i < n; i++) order[i] = i;
    g_sort_notes = ctx->notes;
    qsort(order, (size_t)n, sizeof(int), compare_by_start);

    int sent_program[16], sent_pan[16], sent_volume[16];
    int part_seen[ALDA_MAX_PARTS] = {0};
    for (int c = 0; c < 16; c++) {
        sent_program[c] = -1;
        sent_pan[c] = -1;
        sent_volume[c] = -1;
    }

    for (int k = 0; k < n; k++) {
        AldaNoteRecord* note = &ctx->notes[order[k]];
        int ch = channels[order[k]];
        int on = time_to_tick(points, point_count, note->start);
        int off = time_to_tick(points, point_count, note->start + note->duration);
        int part = note->part_index;
        int line = note->source_line;

        if (!part_seen[part]) {
            part_seen[part] = 1;
            ctx->parts[part].channel = ch + 1;  /* For display; 1-based */
        }
        if (!note->percussion && sent_program[ch] != note->program) {
            if (schedule(ctx, on, ALDA_EVT_PROGRAM, ch, note->program, 0, part, line) < 0) goto done;
            sent_program[ch] = note->program;
        }
        if (sent_pan[ch] != note->pan) {
            if (schedule(ctx, on, ALDA_EVT_PAN, ch, note->pan, 0, part, line) < 0) goto done;
            sent_pan[ch] = note->pan;
        }
        if (sent_volume[ch] != note->track_volume) {
            if (schedule(ctx, on, ALDA_EVT_CC, ch, 11, note->track_volume, part, line) < 0) goto done;
            sent_volume[ch] = note->track_volume;
        }
        if (schedule(ctx, on, ALDA_EVT_NOTE_ON, ch, note->pitch, note->velocity, part, line) < 0 ||
            schedule(ctx, off, ALDA_EVT_NOTE_OFF, ch, note->pitch, 0, part, line) < 0) {
            goto done;
        }
    }

    alda_events_sort(ctx);
    result = 0;

done:
    free(points);
    free(channels);
    free(order);
    return result;
}
