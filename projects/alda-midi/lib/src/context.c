/**
 * @file context.c
 * @brief Alda interpreter context implementation.
 */

#include "alda/context.h"
#include "alda/instruments.h"
#include <math.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>

/* ============================================================================
 * Context Management
 * ============================================================================ */

void alda_context_init(AldaContext* ctx) {
    if (!ctx) return;

    /* MIDI output handles */
    ctx->midi_observer = NULL;
    ctx->midi_out = NULL;
    for (int i = 0; i < ALDA_MAX_PORTS; i++) {
        ctx->out_ports[i] = NULL;
    }
    ctx->out_port_count = 0;

    /* Parts management */
    ctx->part_count = 0;
    ctx->current_part_count = 0;
    ctx->next_channel = 1;  /* Start assigning from channel 1 */

    for (int i = 0; i < ALDA_MAX_PARTS; i++) {
        ctx->current_part_indices[i] = -1;
        memset(&ctx->parts[i], 0, sizeof(AldaPartState));
    }

    /* Global defaults */
    ctx->global_tempo = ALDA_DEFAULT_TEMPO;
    ctx->global_volume = ALDA_DEFAULT_VOLUME;
    ctx->global_quant = ALDA_DEFAULT_QUANT;
    ctx->global_pan = ALDA_DEFAULT_PAN;
    ctx->global_track_volume = ALDA_DEFAULT_TRACK_VOLUME;

    /* Markers (deferred) */
    ctx->marker_count = 0;

    /* Variables (deferred) */
    ctx->variable_count = 0;

    /* Event queue */
    ctx->events = NULL;
    ctx->event_count = 0;
    ctx->event_capacity = 0;

    ctx->notes = NULL;
    ctx->note_count = 0;
    ctx->note_capacity = 0;
    ctx->tempo_changes = NULL;
    ctx->tempo_change_count = 0;
    ctx->tempo_change_capacity = 0;
    ctx->voice_group_ends = NULL;
    ctx->voice_group_end_count = 0;
    ctx->voice_group_end_capacity = 0;
    ctx->base_tempo = ALDA_DEFAULT_TEMPO;

    for (int i = 0; i < 7; i++) {
        ctx->global_key_signature[i] = 0;
    }
    ctx->has_global_key_signature = 0;

    /* Runtime flags */
    ctx->no_sleep_mode = 0;
    ctx->verbose_mode = 0;

    /* Repeat context */
    ctx->current_repetition = 0;

    /* File context */
    ctx->current_file = NULL;
    ctx->current_line = 0;
}

void alda_context_cleanup(AldaContext* ctx) {
    if (!ctx) return;

    /* Free event queue */
    if (ctx->events) {
        free(ctx->events);
        ctx->events = NULL;
    }
    ctx->event_count = 0;
    ctx->event_capacity = 0;

    free(ctx->notes);
    ctx->notes = NULL;
    ctx->note_count = 0;
    ctx->note_capacity = 0;
    free(ctx->tempo_changes);
    ctx->tempo_changes = NULL;
    ctx->tempo_change_count = 0;
    ctx->tempo_change_capacity = 0;
    free(ctx->voice_group_ends);
    ctx->voice_group_ends = NULL;
    ctx->voice_group_end_count = 0;
    ctx->voice_group_end_capacity = 0;

    /* Note: MIDI cleanup is handled separately by midi_backend */
}

void alda_context_reset(AldaContext* ctx) {
    if (!ctx) return;

    /* Reset parts but keep them defined */
    for (int i = 0; i < ctx->part_count; i++) {
        AldaPartState* part = &ctx->parts[i];
        part->current_time = 0.0;
        part->time_scale = 1.0;
    }

    /* Clear current part selection */
    ctx->current_part_count = 0;
    for (int i = 0; i < ALDA_MAX_PARTS; i++) {
        ctx->current_part_indices[i] = -1;
    }

    /* Clear events */
    ctx->event_count = 0;
    ctx->note_count = 0;
    ctx->tempo_change_count = 0;
    ctx->voice_group_end_count = 0;

    /* Clear markers and variables */
    ctx->marker_count = 0;
    ctx->variable_count = 0;
}

/* ============================================================================
 * Part Management
 * ============================================================================ */

void alda_part_init(AldaPartState* part, const char* name, int channel, int program) {
    if (!part) return;

    memset(part, 0, sizeof(AldaPartState));

    if (name) {
        strncpy(part->name, name, sizeof(part->name) - 1);
        part->name[sizeof(part->name) - 1] = '\0';
    }

    part->program = program;
    part->channel = channel;
    part->percussion = 0;
    part->pinned_channel = -1;

    /* Musical state defaults */
    part->octave = ALDA_DEFAULT_OCTAVE;
    part->volume = -1;  /* -1 = use global */
    part->velocity_override = 69;  /* mf: Alda's volume 54 as velocity */
    part->tempo = 0;   /* 0 = use global */
    part->quant = -1;  /* -1 = use global; 0 is a valid quantization */
    part->pan = ALDA_DEFAULT_PAN;
    part->track_volume = ALDA_DEFAULT_TRACK_VOLUME;

    /* Duration defaults: a quarter note */
    part->default_beats = 4.0 / ALDA_DEFAULT_DURATION;
    part->default_ms = 0.0;

    /* Timing */
    part->current_time = 0.0;
    part->time_scale = 1.0;

    /* Key signature (all natural) */
    for (int i = 0; i < 7; i++) {
        part->key_signature[i] = 0;
    }

    /* Transposition (none) */
    part->transpose = 0;
}

AldaPartState* alda_find_part(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return NULL;

    for (int i = 0; i < ctx->part_count; i++) {
        /* Check name */
        if (strcmp(ctx->parts[i].name, name) == 0) {
            return &ctx->parts[i];
        }
        /* Check alias */
        if (ctx->parts[i].alias[0] != '\0' &&
            strcmp(ctx->parts[i].alias, name) == 0) {
            return &ctx->parts[i];
        }
    }

    /* Dot accessor: "<group-alias>.<instrument>" selects one member of a
     * previously declared group, e.g. "strings.cello" after
     * violin/viola/cello "strings". Every member of an aliased group carries
     * the group alias, so match on alias plus instrument name. */
    const char* dot = strchr(name, '.');
    if (dot && dot != name && dot[1] != '\0') {
        size_t alias_len = (size_t)(dot - name);
        const char* member = dot + 1;

        for (int i = 0; i < ctx->part_count; i++) {
            AldaPartState* part = &ctx->parts[i];
            if (part->alias[0] == '\0') continue;
            if (strlen(part->alias) != alias_len) continue;
            if (strncmp(part->alias, name, alias_len) != 0) continue;
            if (strcmp(part->name, member) == 0) {
                return part;
            }
        }
    }

    return NULL;
}

AldaPartState* alda_get_or_create_part(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return NULL;

    /* First, try to find existing part */
    AldaPartState* existing = alda_find_part(ctx, name);
    if (existing) {
        return existing;
    }

    /* Check capacity */
    if (ctx->part_count >= ALDA_MAX_PARTS) {
        fprintf(stderr, "Error: Maximum number of parts (%d) exceeded\n",
                ALDA_MAX_PARTS);
        return NULL;
    }

    /* Look up instrument program number */
    int program = alda_instrument_program(name);
    if (program < 0) {
        /* Unknown instrument - default to piano */
        if (ctx->verbose_mode) {
            fprintf(stderr, "Warning: Unknown instrument '%s', using piano\n", name);
        }
        program = 0;
    }

    /* Allocate channel (skip 10 for non-percussion) */
    int channel = ctx->next_channel;

    /* Check if this is a percussion instrument */
    int is_percussion = alda_instrument_is_percussion(name);

    if (is_percussion) {
        channel = 10;  /* Drums always on channel 10 */
    } else {
        /* Skip channel 10 for non-percussion */
        if (channel == 10) {
            channel = 11;
        }
        ctx->next_channel = channel + 1;
        if (ctx->next_channel > 16) {
            ctx->next_channel = 1;  /* Wrap around */
        }
        if (ctx->next_channel == 10) {
            ctx->next_channel = 11;  /* Skip 10 again */
        }
    }

    /* Create new part */
    AldaPartState* part = &ctx->parts[ctx->part_count];
    alda_part_init(part, name, channel, program);
    part->percussion = is_percussion;
    part->pan = ctx->global_pan;
    part->track_volume = ctx->global_track_volume;

    /* A part declared after (key-sig! ...) still inherits it. */
    if (ctx->has_global_key_signature) {
        for (int i = 0; i < 7; i++) {
            part->key_signature[i] = ctx->global_key_signature[i];
        }
    }

    ctx->part_count++;

    if (ctx->verbose_mode) {
        printf("Created part: %s (program=%d, channel=%d)\n",
               name, program, channel);
    }

    return part;
}

/* Helper to create a new part (always creates, never reuses) */
static AldaPartState* alda_create_new_part(AldaContext* ctx, const char* name) {
    if (!ctx || !name) return NULL;

    /* Check capacity */
    if (ctx->part_count >= ALDA_MAX_PARTS) {
        fprintf(stderr, "Error: Maximum number of parts (%d) exceeded\n",
                ALDA_MAX_PARTS);
        return NULL;
    }

    /* Look up instrument program number */
    int program = alda_instrument_program(name);
    if (program < 0) {
        if (ctx->verbose_mode) {
            fprintf(stderr, "Warning: Unknown instrument '%s', using piano\n", name);
        }
        program = 0;
    }

    /* Allocate channel (skip 10 for non-percussion) */
    int channel = ctx->next_channel;
    int is_percussion = alda_instrument_is_percussion(name);

    if (is_percussion) {
        channel = 10;
    } else {
        if (channel == 10) {
            channel = 11;
        }
        ctx->next_channel = channel + 1;
        if (ctx->next_channel > 16) {
            ctx->next_channel = 1;
        }
        if (ctx->next_channel == 10) {
            ctx->next_channel = 11;
        }
    }

    /* Create new part */
    AldaPartState* part = &ctx->parts[ctx->part_count];
    alda_part_init(part, name, channel, program);
    part->percussion = is_percussion;
    part->pan = ctx->global_pan;
    part->track_volume = ctx->global_track_volume;

    /* A part declared after (key-sig! ...) still inherits it. */
    if (ctx->has_global_key_signature) {
        for (int i = 0; i < 7; i++) {
            part->key_signature[i] = ctx->global_key_signature[i];
        }
    }

    ctx->part_count++;

    if (ctx->verbose_mode) {
        printf("Created part: %s (program=%d, channel=%d)\n",
               name, program, channel);
    }

    return part;
}

/* Resolve one name from a part declaration to an existing part, or NULL when a
 * new one should be created.
 *
 * An alias names a specific instance, so an aliased declaration matches only a
 * part already carrying that alias: "violin/viola/cello \"strings\"" is a
 * different set of parts from a plain "violin:" earlier in the score. An
 * un-aliased declaration prefers the alias-less part of that instrument, and
 * otherwise treats the name as a reference to an existing alias - which is what
 * lets "guitar/sax:" address parts declared as
 *
 *     electric-guitar-distorted "guitar": o2
 *     tenor-saxophone "sax": o3
 *
 * and keep their accumulated octaves rather than resetting them. */
static AldaPartState* resolve_declared_part(AldaContext* ctx, const char* name,
                                            const char* alias) {
    if (!ctx || !name) return NULL;

    if (alias && alias[0] != '\0') {
        for (int i = 0; i < ctx->part_count; i++) {
            if (strcmp(ctx->parts[i].name, name) == 0 &&
                strcmp(ctx->parts[i].alias, alias) == 0) {
                return &ctx->parts[i];
            }
        }
        return NULL;  /* Distinct instance - caller creates it */
    }

    /* Un-aliased: the instrument's own part, if it has no alias of its own. */
    for (int i = 0; i < ctx->part_count; i++) {
        if (ctx->parts[i].alias[0] == '\0' &&
            strcmp(ctx->parts[i].name, name) == 0) {
            return &ctx->parts[i];
        }
    }

    /* Otherwise the name may itself be an alias. Match on alias only: falling
     * back to the instrument name would make a bare "violin:" reuse a part
     * declared as violin "a", when Alda treats those as separate instances. */
    for (int i = 0; i < ctx->part_count; i++) {
        if (ctx->parts[i].alias[0] != '\0' &&
            strcmp(ctx->parts[i].alias, name) == 0) {
            return &ctx->parts[i];
        }
    }

    /* The "<group-alias>.<instrument>" dot-accessor form. */
    if (strchr(name, '.')) {
        return alda_find_part(ctx, name);
    }

    return NULL;
}

int alda_set_current_parts_aliased(AldaContext* ctx, char** names, int count,
                                   const char* alias) {
    if (!ctx) return -1;

    /* Clear current selection */
    ctx->current_part_count = 0;

    if (!names || count <= 0) {
        return 0;
    }

    for (int i = 0; i < count && i < ALDA_MAX_PARTS; i++) {
        AldaPartState* part = resolve_declared_part(ctx, names[i], alias);
        if (!part) {
            part = alda_create_new_part(ctx, names[i]);
        }

        if (!part) {
            return -1;
        }

        /* Find part index */
        int idx = (int)(part - ctx->parts);
        ctx->current_part_indices[ctx->current_part_count] = idx;
        ctx->current_part_count++;
    }

    return 0;
}

int alda_set_current_parts(AldaContext* ctx, char** names, int count) {
    return alda_set_current_parts_aliased(ctx, names, count, NULL);
}

AldaPartState* alda_current_part(AldaContext* ctx) {
    if (!ctx || ctx->current_part_count == 0) {
        return NULL;
    }
    int idx = ctx->current_part_indices[0];
    if (idx < 0 || idx >= ctx->part_count) {
        return NULL;
    }
    return &ctx->parts[idx];
}

/* ============================================================================
 * Utility Functions
 * ============================================================================ */

int alda_effective_tempo(AldaContext* ctx, AldaPartState* part) {
    if (!ctx) return ALDA_DEFAULT_TEMPO;
    if (part && part->tempo > 0) {
        return part->tempo;
    }
    return ctx->global_tempo;
}

int alda_effective_velocity(AldaContext* ctx, AldaPartState* part) {
    /* Check for direct velocity override (set by dynamics) */
    if (part && part->velocity_override >= 0) {
        return part->velocity_override;
    }

    double volume = ALDA_DEFAULT_VOLUME;

    if (part && part->volume >= 0) {
        volume = part->volume;
    } else if (ctx) {
        volume = ctx->global_volume;
    }

    return alda_percent_to_midi(volume);
}

int alda_percent_to_midi(double percent) {
    /* As Alda: scale to a fraction, multiply by 127 and round half away from
     * zero, so 50 is 64. Truncating gives 63. */
    double scaled = floor(percent / 100.0 * 127.0 + 0.5);
    if (scaled < 0) scaled = 0;
    if (scaled > 127) scaled = 127;
    return (int)scaled;
}

int alda_effective_quant(AldaContext* ctx, AldaPartState* part) {
    if (part && part->quant >= 0) {
        return part->quant;
    }
    if (ctx) {
        return ctx->global_quant;
    }
    return ALDA_DEFAULT_QUANT;
}

int alda_no_sleep(AldaContext* ctx) {
    return ctx ? ctx->no_sleep_mode : 0;
}

void alda_set_no_sleep(AldaContext* ctx, int value) {
    if (ctx) {
        ctx->no_sleep_mode = value ? 1 : 0;
    }
}
