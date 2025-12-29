/*
 * music_theory.c - Common music theory utilities implementation
 */

#include "music_theory.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <strings.h>

/* ============================================================================
 * Chord Interval Definitions
 * ============================================================================ */

const int CHORD_MAJOR[]     = {0, 4, 7};
const int CHORD_MINOR[]     = {0, 3, 7};
const int CHORD_DIM[]       = {0, 3, 6};
const int CHORD_AUG[]       = {0, 4, 8};
const int CHORD_DOM7[]      = {0, 4, 7, 10};
const int CHORD_MAJ7[]      = {0, 4, 7, 11};
const int CHORD_MIN7[]      = {0, 3, 7, 10};
const int CHORD_DIM7[]      = {0, 3, 6, 9};
const int CHORD_HALF_DIM7[] = {0, 3, 6, 10};
const int CHORD_SUS2[]      = {0, 2, 7};
const int CHORD_SUS4[]      = {0, 5, 7};

/* ============================================================================
 * Pitch Parsing
 * ============================================================================ */

int music_parse_pitch(const char* name) {
    if (name == NULL || name[0] == '\0') return -1;

    /* Parse note letter (case insensitive) */
    int note;
    char c = toupper((unsigned char)name[0]);
    switch (c) {
        case 'C': note = 0;  break;
        case 'D': note = 2;  break;
        case 'E': note = 4;  break;
        case 'F': note = 5;  break;
        case 'G': note = 7;  break;
        case 'A': note = 9;  break;
        case 'B': note = 11; break;
        default: return -1;
    }

    const char* p = name + 1;

    /* Parse accidental (optional) */
    if (*p == '#' || *p == 's' || *p == 'S') {
        note++;
        p++;
    } else if (*p == 'b' || *p == 'B') {
        note--;
        p++;
    }

    /* Parse octave number (required) */
    if (*p == '\0') return -1;

    /* Handle negative octave (-1) */
    int negative = 0;
    if (*p == '-') {
        negative = 1;
        p++;
    }

    if (*p == '\0' || !isdigit((unsigned char)*p)) return -1;

    int octave = atoi(p);
    if (negative) octave = -octave;

    /* Validate octave range: -1 to 9 */
    if (octave < -1 || octave > 9) return -1;

    /* Calculate MIDI note number */
    int midi_note = (octave + 1) * 12 + note;

    /* Validate final MIDI range */
    if (midi_note < 0 || midi_note > 127) return -1;

    return midi_note;
}

char* music_pitch_to_name(int pitch, char* buf, int buflen, int use_sharps) {
    if (buf == NULL || buflen < 5) return NULL;
    if (pitch < 0 || pitch > 127) return NULL;

    static const char* sharp_names[] = {
        "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#", "A", "A#", "B"
    };
    static const char* flat_names[] = {
        "C", "Db", "D", "Eb", "E", "F", "Gb", "G", "Ab", "A", "Bb", "B"
    };

    int octave = (pitch / 12) - 1;
    int note_index = pitch % 12;

    const char* note_name = use_sharps ? sharp_names[note_index] : flat_names[note_index];

    snprintf(buf, buflen, "%s%d", note_name, octave);
    return buf;
}

/* ============================================================================
 * Chord Building
 * ============================================================================ */

int music_build_chord(int root, const int* intervals, int num_notes, int* out_pitches) {
    if (intervals == NULL || out_pitches == NULL || num_notes <= 0) return 0;
    if (root < 0 || root > 127) return 0;

    int valid_count = 0;
    for (int i = 0; i < num_notes; i++) {
        int pitch = root + intervals[i];
        if (pitch >= 0 && pitch <= 127) {
            out_pitches[valid_count++] = pitch;
        }
    }

    return valid_count;
}

/* ============================================================================
 * Dynamics Parsing
 * ============================================================================ */

int music_parse_dynamics(const char* name) {
    if (name == NULL) return -1;

    /* Compare case-insensitively */
    if (strcasecmp(name, "ppp") == 0) return DYN_PPP;
    if (strcasecmp(name, "pp")  == 0) return DYN_PP;
    if (strcasecmp(name, "p")   == 0) return DYN_P;
    if (strcasecmp(name, "mp")  == 0) return DYN_MP;
    if (strcasecmp(name, "mf")  == 0) return DYN_MF;
    if (strcasecmp(name, "f")   == 0) return DYN_F;
    if (strcasecmp(name, "ff")  == 0) return DYN_FF;
    if (strcasecmp(name, "fff") == 0) return DYN_FFF;

    return -1;
}

/* ============================================================================
 * Duration Calculation
 * ============================================================================ */

int music_duration_ms(double beats, int bpm) {
    if (bpm <= 0) return 0;

    /* Duration = (beats / bpm) * 60000 ms */
    return (int)((beats * 60000.0) / bpm);
}
