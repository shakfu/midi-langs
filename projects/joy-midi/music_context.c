/*
 * music_context.c - Musical state implementation for Joy-MIDI
 */

#include "music_context.h"
#include <stdlib.h>

MusicContext* music_context_new(void) {
    MusicContext* mctx = malloc(sizeof(MusicContext));
    if (mctx) {
        music_context_reset(mctx);
    }
    return mctx;
}

void music_context_free(MusicContext* mctx) {
    if (mctx) {
        free(mctx);
    }
}

void music_context_reset(MusicContext* mctx) {
    mctx->octave = 4;           /* Default octave 4 (middle C = C4 = MIDI 60) */
    mctx->duration_value = 4;   /* Default quarter note */
    mctx->tempo = 120;          /* Default tempo */
    mctx->duration_ms = music_duration_to_ms(4, 120);  /* 500ms at 120 BPM */
    mctx->velocity = 80;        /* Default mezzo-forte */
    mctx->quantization = 90;    /* Default 90% gate time (legato) */
    mctx->last_pitch = -1;      /* No last pitch */
    mctx->in_chord = false;
    mctx->chord_count = 0;
}

int music_duration_to_ms(int duration, int tempo) {
    /* At any tempo, quarter note = 60000/tempo ms */
    /* duration: 1=whole, 2=half, 4=quarter, 8=eighth, etc. */
    if (duration <= 0) duration = 4;  /* Default to quarter */
    int quarter_ms = 60000 / tempo;
    return quarter_ms * 4 / duration;
}
