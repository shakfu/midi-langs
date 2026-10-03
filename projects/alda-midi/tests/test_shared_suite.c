/**
 * @file test_shared_suite.c
 * @brief Check alda-midi's output against Alda's own, score by score.
 *
 * Usage: test_shared_suite DIR [EXPECTED_DIR]
 *
 * Interprets every DIR/<name>.alda and compares it with
 * EXPECTED_DIR/<name>.expected (EXPECTED_DIR defaults to DIR). The .expected
 * files are written from `alda export` output by aldakit's
 * scripts/gen_shared_suite.py; shared_suite/README.md gives the format and
 * docs/alda-midi/conformance.md the method.
 *
 * Each note is compared on pitch, start, duration and velocity, plus the
 * program and controllers 10 (pan) and 11 (track volume) in effect on its
 * channel when it starts. Channel numbers are not compared: they are an
 * implementation detail, and alda-midi numbers them differently from Alda. Notes
 * are read from alda-midi's events the way a MIDI file reader reads them, because
 * that is how the expected notes were read.
 */

#include <ctype.h>
#include <math.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <dirent.h>

#include <alda/alda.h>
#include <alda/interpreter.h>
#include <alda/context.h>
#include <alda/scheduler.h>

/* The expected values come from a 128 ticks-per-beat file, within one tick
 * of Alda's millisecond values; 10ms covers that at any tempo in the corpus. */
#define TOLERANCE 0.01
#define SHOW 5  /* Mismatches listed per category */

/* ============================================================================
 * Notes and their channel state
 * ============================================================================ */

typedef struct {
    int pitch;
    double start;
    double duration;
    int velocity;
    int channel;
    int program;   /* -1 when none was sent */
    int pan;       /* -1 when none was sent */
    int volume;    /* CC 11, -1 when none was sent */
    int matched;
} Note;

typedef struct {
    double time;
    int channel;
    int control;   /* -1 for a program change */
    int value;
} Setting;

typedef struct {
    double time;
    double bpm;
} Tempo;

typedef struct {
    Note* notes;
    int note_count;
    Setting* settings;
    int setting_count;
    Tempo* tempos;
    int tempo_count;
} Score;

static void* grow(void* items, int count, size_t size) {
    /* Double at powers of two */
    if (count == 0 || (count & (count - 1)) == 0) {
        void* bigger = realloc(items, size * (size_t)(count ? count * 2 : 16));
        if (!bigger) {
            fprintf(stderr, "Out of memory\n");
            exit(2);
        }
        return bigger;
    }
    return items;
}

static void add_note(Score* s, Note n) {
    s->notes = grow(s->notes, s->note_count, sizeof(Note));
    s->notes[s->note_count++] = n;
}

static void add_setting(Score* s, Setting x) {
    s->settings = grow(s->settings, s->setting_count, sizeof(Setting));
    s->settings[s->setting_count++] = x;
}

static void add_tempo(Score* s, Tempo t) {
    s->tempos = grow(s->tempos, s->tempo_count, sizeof(Tempo));
    s->tempos[s->tempo_count++] = t;
}

static void free_score(Score* s) {
    free(s->notes);
    free(s->settings);
    free(s->tempos);
}

/* The value of a program (control -1) or controller in effect on a channel
 * at a time: the last one sent at or before it. Settings are in time order. */
static int state_at(const Score* s, int channel, int control, double time) {
    int value = -1;
    for (int i = 0; i < s->setting_count; i++) {
        const Setting* x = &s->settings[i];
        if (x->time > time + 1e-6) break;
        if (x->channel == channel && x->control == control) value = x->value;
    }
    return value;
}

static int compare_settings(const void* a, const void* b) {
    const Setting* x = a;
    const Setting* y = b;
    return (x->time > y->time) - (x->time < y->time);
}

static void resolve_state(Score* s) {
    /* Stable enough: settings at one time on one channel do not conflict */
    qsort(s->settings, (size_t)s->setting_count, sizeof(Setting), compare_settings);
    for (int i = 0; i < s->note_count; i++) {
        Note* n = &s->notes[i];
        n->program = state_at(s, n->channel, -1, n->start);
        n->pan = state_at(s, n->channel, 10, n->start);
        n->volume = state_at(s, n->channel, 11, n->start);
    }
}

/* ============================================================================
 * Reading .expected files
 * ============================================================================ */

static int read_expected(const char* path, Score* s) {
    FILE* f = fopen(path, "r");
    if (!f) return -1;

    char line[256];
    while (fgets(line, sizeof(line), f)) {
        double a, b, c;
        int i, j, k;
        if (sscanf(line, "NOTE %d %lf %lf %d %d", &i, &a, &b, &j, &k) == 5) {
            add_note(s, (Note){i, a, b, j, k, -1, -1, -1, 0});
        } else if (sscanf(line, "PROGRAM %d %d %lf", &i, &j, &a) == 3) {
            add_setting(s, (Setting){a, j, -1, i});
        } else if (sscanf(line, "CC %d %d %d %lf", &i, &j, &k, &a) == 4) {
            add_setting(s, (Setting){a, k, i, j});
        } else if (sscanf(line, "TEMPO %lf %lf", &a, &c) == 2) {
            add_tempo(s, (Tempo){c, a});
        }
    }
    fclose(f);
    resolve_state(s);
    return 0;
}

/* ============================================================================
 * Reading alda-midi's events
 * ============================================================================ */

typedef struct {
    int tick;
    double time;
    int tempo;
} TempoPoint;

static double tick_seconds(const TempoPoint* map, int count, int tick) {
    int i = 0;
    while (i + 1 < count && map[i + 1].tick <= tick) i++;
    return map[i].time + (double)(tick - map[i].tick) / ALDA_TICKS_PER_QUARTER
                         * 60.0 / map[i].tempo;
}

static int read_ours(const char* path, Score* s) {
    AldaContext* ctx = malloc(sizeof(AldaContext));
    if (!ctx) return -1;
    alda_context_init(ctx);
    alda_set_no_sleep(ctx, 1);

    if (alda_interpret_file(ctx, path) != 0) {
        alda_context_cleanup(ctx);
        free(ctx);
        return -1;
    }
    alda_events_sort(ctx);

    /* Tempo map as the export writes it: the context tempo unless the score
     * sets its own at tick 0, then every tempo event. */
    int count = 0;
    TempoPoint* map = malloc(sizeof(TempoPoint) * (size_t)(ctx->event_count + 1));
    map[count++] = (TempoPoint){0, 0.0, ctx->global_tempo};
    for (int i = 0; i < ctx->event_count; i++) {
        AldaScheduledEvent* e = &ctx->events[i];
        if (e->type != ALDA_EVT_TEMPO) continue;
        if (map[count - 1].tick == e->tick) {
            map[count - 1].tempo = e->data1;
        } else {
            double t = tick_seconds(map, count, e->tick);
            map[count++] = (TempoPoint){e->tick, t, e->data1};
        }
    }
    for (int i = 0; i < count; i++) add_tempo(s, (Tempo){map[i].time, map[i].tempo});

    /* Notes as a MIDI file reader pairs them: one sounding note per channel
     * and pitch; a new note-on replaces it, and a velocity-0 note-on ends it. */
    int pending_tick[16][128], pending_velocity[16][128];
    for (int c = 0; c < 16; c++) {
        for (int p = 0; p < 128; p++) pending_tick[c][p] = -1;
    }

    for (int i = 0; i < ctx->event_count; i++) {
        AldaScheduledEvent* e = &ctx->events[i];
        double t = tick_seconds(map, count, e->tick);
        int ch = e->channel;
        switch (e->type) {
            case ALDA_EVT_NOTE_ON:
                if (e->data2 > 0) {
                    pending_tick[ch][e->data1] = e->tick;
                    pending_velocity[ch][e->data1] = e->data2;
                    break;
                }
                /* fall through: velocity 0 ends the note */
            case ALDA_EVT_NOTE_OFF: {
                int start = pending_tick[ch][e->data1];
                if (start < 0) break;
                pending_tick[ch][e->data1] = -1;
                double begin = tick_seconds(map, count, start);
                double length = t - begin;
                add_note(s, (Note){e->data1, begin, length > 0.001 ? length : 0.001,
                                   pending_velocity[ch][e->data1], ch, -1, -1, -1, 0});
                break;
            }
            case ALDA_EVT_PROGRAM:
                add_setting(s, (Setting){t, ch, -1, e->data1});
                break;
            case ALDA_EVT_PAN:
                add_setting(s, (Setting){t, ch, 10, e->data1});
                break;
            case ALDA_EVT_CC:
                add_setting(s, (Setting){t, ch, e->data1, e->data2});
                break;
            default:
                break;
        }
    }

    free(map);
    alda_context_cleanup(ctx);
    free(ctx);
    resolve_state(s);
    return 0;
}

/* ============================================================================
 * Comparison
 * ============================================================================ */

enum { C_MISSING, C_EXTRA, C_DURATION, C_VELOCITY, C_PROGRAM, C_PAN, C_VOLUME,
       C_TEMPO, C_COUNT };
static const char* CATEGORY[C_COUNT] = {
    "note missing", "note extra", "note duration", "note velocity",
    "note program", "note pan (CC 10)", "note track volume (CC 11)", "tempo"
};

typedef struct {
    int count[C_COUNT];
    char lines[C_COUNT][SHOW][160];
} Report;

static void report(Report* r, int category, const char* fmt, ...) {
    if (r->count[category] < SHOW) {
        va_list ap;
        va_start(ap, fmt);
        vsnprintf(r->lines[category][r->count[category]], 160, fmt, ap);
        va_end(ap);
    }
    r->count[category]++;
}

static int state_differences(const Note* a, const Note* b) {
    return (a->program != b->program) + (a->pan != b->pan) + (a->volume != b->volume);
}

static void compare(Score* want, Score* got, Report* r) {
    for (int i = 0; i < want->note_count; i++) {
        Note* w = &want->notes[i];
        Note* best = NULL;
        /* Same pitch, start within tolerance; prefer the same instrument state,
         * then the closest duration, so unison notes in different parts pair
         * by part. */
        for (int j = 0; j < got->note_count; j++) {
            Note* g = &got->notes[j];
            if (g->matched || g->pitch != w->pitch) continue;
            if (fabs(g->start - w->start) > TOLERANCE) continue;
            if (!best ||
                state_differences(g, w) < state_differences(best, w) ||
                (state_differences(g, w) == state_differences(best, w) &&
                 fabs(g->duration - w->duration) < fabs(best->duration - w->duration))) {
                best = g;
            }
        }
        if (!best) {
            report(r, C_MISSING, "%.4f p%d", w->start, w->pitch);
            continue;
        }
        best->matched = 1;
        if (fabs(best->duration - w->duration) > 2 * TOLERANCE) {
            report(r, C_DURATION, "%.4f p%d: alda %.4f, ours %.4f",
                   w->start, w->pitch, w->duration, best->duration);
        }
        if (best->velocity != w->velocity) {
            report(r, C_VELOCITY, "%.4f p%d: alda %d, ours %d",
                   w->start, w->pitch, w->velocity, best->velocity);
        }
        if (best->program != w->program) {
            report(r, C_PROGRAM, "%.4f p%d: alda %d, ours %d",
                   w->start, w->pitch, w->program, best->program);
        }
        if (best->pan != w->pan) {
            report(r, C_PAN, "%.4f p%d: alda %d, ours %d",
                   w->start, w->pitch, w->pan, best->pan);
        }
        if (best->volume != w->volume) {
            report(r, C_VOLUME, "%.4f p%d: alda %d, ours %d",
                   w->start, w->pitch, w->volume, best->volume);
        }
    }
    for (int j = 0; j < got->note_count; j++) {
        if (!got->notes[j].matched) {
            report(r, C_EXTRA, "%.4f p%d", got->notes[j].start, got->notes[j].pitch);
        }
    }

    /* Tempo maps: each expected change must be in effect in ours at its time */
    for (int i = 0; i < want->tempo_count; i++) {
        Tempo* w = &want->tempos[i];
        double bpm = 0.0;
        for (int j = 0; j < got->tempo_count; j++) {
            if (got->tempos[j].time <= w->time + TOLERANCE) bpm = got->tempos[j].bpm;
        }
        if (fabs(bpm - w->bpm) > 0.01) {
            report(r, C_TEMPO, "%.4f: alda %.2f, ours %.2f", w->time, w->bpm, bpm);
        }
    }
}

/* ============================================================================
 * Main
 * ============================================================================ */

static int run(const char* alda_path, const char* expected_path, const char* name) {
    Score want = {0}, got = {0};
    if (read_expected(expected_path, &want) != 0) {
        printf("FAIL: %s - no .expected file; write one with aldakit's "
               "scripts/gen_shared_suite.py\n", name);
        return -1;
    }
    if (read_ours(alda_path, &got) != 0) {
        printf("FAIL: %s - parse/interpret error\n", name);
        free_score(&want);
        return -1;
    }

    Report r;
    memset(&r, 0, sizeof(r));
    compare(&want, &got, &r);

    int failed = 0;
    for (int c = 0; c < C_COUNT; c++) failed |= r.count[c] > 0;
    if (!failed) {
        printf("PASS: %s (%d notes)\n", name, want.note_count);
    } else {
        printf("FAIL: %s\n", name);
        for (int c = 0; c < C_COUNT; c++) {
            if (!r.count[c]) continue;
            printf("      %s: %d\n", CATEGORY[c], r.count[c]);
            for (int k = 0; k < r.count[c] && k < SHOW; k++) {
                printf("        %s\n", r.lines[c][k]);
            }
        }
    }

    free_score(&want);
    free_score(&got);
    return failed ? -1 : 0;
}

int main(int argc, char** argv) {
    if (argc < 2) {
        fprintf(stderr, "usage: %s DIR [EXPECTED_DIR]\n", argv[0]);
        return 2;
    }
    const char* dir_path = argv[1];
    const char* expected_dir = argc >= 3 ? argv[2] : argv[1];
    printf("Comparing %s with %s\n\n", dir_path, expected_dir);

    DIR* dir = opendir(dir_path);
    if (!dir) {
        fprintf(stderr, "Cannot open directory: %s\n", dir_path);
        return 2;
    }

    int passed = 0, failed = 0;
    struct dirent* entry;
    while ((entry = readdir(dir)) != NULL) {
        const char* name = entry->d_name;
        size_t len = strlen(name);
        if (len < 5 || strcmp(name + len - 5, ".alda") != 0) continue;

        char alda_path[1024], expected_path[1024];
        snprintf(alda_path, sizeof(alda_path), "%s/%s", dir_path, name);
        snprintf(expected_path, sizeof(expected_path), "%s/%.*s.expected",
                 expected_dir, (int)(len - 5), name);

        if (run(alda_path, expected_path, name) == 0) passed++;
        else failed++;
    }
    closedir(dir);

    printf("\n========================================\n");
    printf("Results: %d passed, %d failed\n", passed, failed);
    printf("========================================\n");
    return failed > 0 ? 1 : 0;
}
