/**
 * @file finalize.h
 * @brief Recording notes and tempo changes, and turning them into events.
 *
 * The interpreter places notes in seconds, part by part. Channels, the
 * controller state each channel needs and tick positions depend on the whole
 * score, so they are worked out once it has been read.
 */

#ifndef ALDA_FINALIZE_H
#define ALDA_FINALIZE_H

#include "alda/context.h"

/** Record a note at a time in seconds, with the part's current settings. */
int alda_record_note(AldaContext* ctx, AldaPartState* part, double start,
                     double duration, int pitch, int velocity);

/**
 * Record a tempo change for the MIDI tempo map.
 * @param global Non-zero for (tempo! N), which wins over the first part's own
 *               tempo at the same time.
 */
int alda_record_tempo(AldaContext* ctx, double time, int tempo, int global);

/** Record that a voice group has just ended for a part. */
int alda_record_voice_group_end(AldaContext* ctx, int part_index);

/** Assign channels and ticks and fill ctx->events from the recorded score. */
int alda_events_finalize(AldaContext* ctx);

#endif /* ALDA_FINALIZE_H */
