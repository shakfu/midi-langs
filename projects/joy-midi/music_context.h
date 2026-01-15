/*
 * music_context.h - Musical state for Joy-MIDI Alda-like notation
 */

#ifndef MUSIC_CONTEXT_H
#define MUSIC_CONTEXT_H

#include <stdbool.h>

#define MUSIC_MAX_CHORD_NOTES 16

typedef struct {
    int octave;           /* Current octave (0-9, default 4) */
    int duration_ms;      /* Current duration in ms (default 500) */
    int velocity;         /* Current velocity (0-127, default 80) */
    int tempo;            /* BPM (default 120) */
    int quantization;     /* Gate percentage (0-100, default 90) */
    int last_pitch;       /* Last played pitch for ties */
    bool in_chord;        /* Currently building a chord */
    int chord_pitches[MUSIC_MAX_CHORD_NOTES];  /* Pitches for current chord */
    int chord_count;      /* Number of pitches in chord */
} MusicContext;

/* Create a new music context with default values */
MusicContext* music_context_new(void);

/* Free a music context */
void music_context_free(MusicContext* mctx);

/* Reset music context to default values */
void music_context_reset(MusicContext* mctx);

/* Convert duration value (1=whole, 2=half, 4=quarter, etc.) to ms based on tempo */
int music_duration_to_ms(int duration, int tempo);

#endif /* MUSIC_CONTEXT_H */
