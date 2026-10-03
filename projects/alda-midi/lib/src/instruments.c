/**
 * @file instruments.c
 * @brief General MIDI instrument name to program number mapping.
 */

#include "alda/instruments.h"
#include <string.h>
#include <ctype.h>

/* ============================================================================
 * GM Instrument Table
 * ============================================================================ */

typedef struct {
    const char* name;
    int program;
} InstrumentEntry;

/* All names are lowercase for case-insensitive lookup */

/* Alda's own names, generated from its instrument list */
static const InstrumentEntry ALDA_INSTRUMENTS[] = {
#include "instruments_alda.inc"
    {NULL, -1}
};

/* Further names psnd accepts that Alda's list does not have, such as the
 * unprefixed General MIDI names. */
static const InstrumentEntry GM_INSTRUMENTS[] = {
    /* Piano (0-7) */
    {"acoustic-grand-piano", 0},
    {"bright-acoustic-piano", 1},
    {"electric-grand-piano", 2},
    {"honky-tonk-piano", 3},
    {"electric-piano-1", 4},
    {"electric-piano-2", 5},

    /* Organ (16-23) */
    {"drawbar-organ", 16},
    {"percussive-organ", 17},
    {"rock-organ", 18},
    {"church-organ", 19},
    {"reed-organ", 20},
    {"tango-accordion", 23},

    /* Guitar (24-31) */
    {"acoustic-guitar-nylon", 24},
    {"nylon-guitar", 24},
    {"acoustic-guitar-steel", 25},
    {"steel-guitar", 25},
    {"electric-guitar-jazz", 26},
    {"jazz-guitar", 26},
    {"clean-guitar", 27},
    {"electric-guitar-muted", 28},
    {"midi-electric-guitar-muted", 28},
    {"muted-guitar", 28},
    {"overdriven-guitar", 29},
    {"midi-overdriven-guitar", 29},
    {"distorted-guitar", 30},
    {"distortion-guitar", 30},
    {"guitar-harmonics", 31},

    /* Bass (32-39) */
    {"finger-bass", 33},
    {"pick-bass", 34},
    {"slap-bass-1", 36},
    {"midi-slap-bass-1", 36},
    {"slap-bass", 36},
    {"slap-bass-2", 37},
    {"midi-slap-bass-2", 37},
    {"synth-bass-1", 38},
    {"synth-bass", 38},
    {"synth-bass-2", 39},

    /* Strings (40-47) */
    {"tremolo-strings", 44},
    {"pizzicato-strings", 45},

    /* Ensemble (48-55) */
    {"string-ensemble-1", 48},
    {"strings", 48},
    {"string-ensemble", 48},
    {"string-ensemble-2", 49},
    {"synth-strings-1", 50},
    {"synth-strings", 50},
    {"synth-strings-2", 51},
    {"choir-aahs", 52},
    {"choir", 52},
    {"voice-oohs", 53},
    {"synth-voice", 54},
    {"orchestra-hit", 55},

    /* Brass (56-63) */
    {"muted-trumpet", 59},
    {"horn", 60},
    {"brass-section", 61},
    {"brass", 61},
    {"synth-brass-1", 62},
    {"synth-brass", 62},
    {"synth-brass-2", 63},

    /* Pipe (72-79) */
    {"blown-bottle", 76},
    {"midi-blown-bottle", 76},

    /* Synth Lead (80-87) */
    {"lead-1-square", 80},
    {"midi-lead-1-square", 80},
    {"lead-2-sawtooth", 81},
    {"midi-lead-2-sawtooth", 81},
    {"sawtooth-lead", 81},
    {"lead-3-calliope", 82},
    {"midi-lead-3-calliope", 82},
    {"lead-4-chiff", 83},
    {"midi-lead-4-chiff", 83},
    {"chiff-lead", 83},
    {"lead-5-charang", 84},
    {"midi-lead-5-charang", 84},
    {"charang-lead", 84},
    {"lead-6-voice", 85},
    {"midi-lead-6-voice", 85},
    {"voice-lead", 85},
    {"lead-7-fifths", 86},
    {"midi-lead-7-fifths", 86},
    {"fifths-lead", 86},
    {"lead-8-bass-lead", 87},
    {"midi-lead-8-bass-lead", 87},
    {"bass-lead", 87},

    /* Synth Pad (88-95) */
    {"pad-1-new-age", 88},
    {"midi-pad-1-new-age", 88},
    {"new-age-pad", 88},
    {"pad-2-warm", 89},
    {"midi-pad-2-warm", 89},
    {"warm-pad", 89},
    {"pad-3-polysynth", 90},
    {"midi-pad-3-polysynth", 90},
    {"polysynth-pad", 90},
    {"pad-4-choir", 91},
    {"midi-pad-4-choir", 91},
    {"choir-pad", 91},
    {"pad-5-bowed", 92},
    {"midi-pad-5-bowed", 92},
    {"bowed-pad", 92},
    {"pad-6-metallic", 93},
    {"midi-pad-6-metallic", 93},
    {"metallic-pad", 93},
    {"pad-7-halo", 94},
    {"midi-pad-7-halo", 94},
    {"halo-pad", 94},
    {"pad-8-sweep", 95},
    {"midi-pad-8-sweep", 95},
    {"sweep-pad", 95},

    /* Synth Effects (96-103) */
    {"fx-1-rain", 96},
    {"midi-fx-1-rain", 96},
    {"rain", 96},
    {"fx-2-soundtrack", 97},
    {"midi-fx-2-soundtrack", 97},
    {"soundtrack", 97},
    {"fx-3-crystal", 98},
    {"midi-fx-3-crystal", 98},
    {"crystal", 98},
    {"fx-4-atmosphere", 99},
    {"midi-fx-4-atmosphere", 99},
    {"atmosphere", 99},
    {"fx-5-brightness", 100},
    {"midi-fx-5-brightness", 100},
    {"brightness", 100},
    {"fx-6-goblins", 101},
    {"midi-fx-6-goblins", 101},
    {"goblins", 101},
    {"fx-7-echoes", 102},
    {"midi-fx-7-echoes", 102},
    {"echoes", 102},
    {"fx-8-sci-fi", 103},
    {"midi-fx-8-sci-fi", 103},
    {"sci-fi", 103},

    /* Ethnic (104-111) */
    {"bagpipe", 109},
    {"midi-bagpipe", 109},
    {"fiddle", 110},

    /* Percussive (112-119) */
    {"tinkle-bell", 112},
    {"agogo", 113},
    {"woodblock", 115},
    {"taiko-drum", 116},
    {"taiko", 116},
    {"melodic-tom", 117},
    {"synth-drum", 118},
    {"reverse-cymbal", 119},

    /* Sound Effects (120-127) */
    {"guitar-fret-noise", 120},
    {"breath-noise", 121},
    {"seashore", 122},
    {"bird-tweet", 123},
    {"telephone-ring", 124},
    {"helicopter", 125},
    {"applause", 126},
    {"gunshot", 127},

    /* Sentinel */
    {NULL, -1}
};
/* Canonical names for each program number */
static const char* CANONICAL_NAMES[128] = {
    "acoustic-grand-piano",    /* 0 */
    "bright-acoustic-piano",   /* 1 */
    "electric-grand-piano",    /* 2 */
    "honky-tonk-piano",        /* 3 */
    "electric-piano-1",        /* 4 */
    "electric-piano-2",        /* 5 */
    "harpsichord",             /* 6 */
    "clavinet",                /* 7 */
    "celesta",                 /* 8 */
    "glockenspiel",            /* 9 */
    "music-box",               /* 10 */
    "vibraphone",              /* 11 */
    "marimba",                 /* 12 */
    "xylophone",               /* 13 */
    "tubular-bells",           /* 14 */
    "dulcimer",                /* 15 */
    "drawbar-organ",           /* 16 */
    "percussive-organ",        /* 17 */
    "rock-organ",              /* 18 */
    "church-organ",            /* 19 */
    "reed-organ",              /* 20 */
    "accordion",               /* 21 */
    "harmonica",               /* 22 */
    "tango-accordion",         /* 23 */
    "acoustic-guitar-nylon",   /* 24 */
    "acoustic-guitar-steel",   /* 25 */
    "electric-guitar-jazz",    /* 26 */
    "electric-guitar-clean",   /* 27 */
    "electric-guitar-muted",   /* 28 */
    "overdriven-guitar",       /* 29 */
    "distortion-guitar",       /* 30 */
    "guitar-harmonics",        /* 31 */
    "acoustic-bass",           /* 32 */
    "electric-bass-finger",    /* 33 */
    "electric-bass-pick",      /* 34 */
    "fretless-bass",           /* 35 */
    "slap-bass-1",             /* 36 */
    "slap-bass-2",             /* 37 */
    "synth-bass-1",            /* 38 */
    "synth-bass-2",            /* 39 */
    "violin",                  /* 40 */
    "viola",                   /* 41 */
    "cello",                   /* 42 */
    "contrabass",              /* 43 */
    "tremolo-strings",         /* 44 */
    "pizzicato-strings",       /* 45 */
    "orchestral-harp",         /* 46 */
    "timpani",                 /* 47 */
    "string-ensemble-1",       /* 48 */
    "string-ensemble-2",       /* 49 */
    "synth-strings-1",         /* 50 */
    "synth-strings-2",         /* 51 */
    "choir-aahs",              /* 52 */
    "voice-oohs",              /* 53 */
    "synth-voice",             /* 54 */
    "orchestra-hit",           /* 55 */
    "trumpet",                 /* 56 */
    "trombone",                /* 57 */
    "tuba",                    /* 58 */
    "muted-trumpet",           /* 59 */
    "french-horn",             /* 60 */
    "brass-section",           /* 61 */
    "synth-brass-1",           /* 62 */
    "synth-brass-2",           /* 63 */
    "soprano-sax",             /* 64 */
    "alto-sax",                /* 65 */
    "tenor-sax",               /* 66 */
    "baritone-sax",            /* 67 */
    "oboe",                    /* 68 */
    "english-horn",            /* 69 */
    "bassoon",                 /* 70 */
    "clarinet",                /* 71 */
    "piccolo",                 /* 72 */
    "flute",                   /* 73 */
    "recorder",                /* 74 */
    "pan-flute",               /* 75 */
    "blown-bottle",            /* 76 */
    "shakuhachi",              /* 77 */
    "whistle",                 /* 78 */
    "ocarina",                 /* 79 */
    "lead-1-square",           /* 80 */
    "lead-2-sawtooth",         /* 81 */
    "lead-3-calliope",         /* 82 */
    "lead-4-chiff",            /* 83 */
    "lead-5-charang",          /* 84 */
    "lead-6-voice",            /* 85 */
    "lead-7-fifths",           /* 86 */
    "lead-8-bass-lead",        /* 87 */
    "pad-1-new-age",           /* 88 */
    "pad-2-warm",              /* 89 */
    "pad-3-polysynth",         /* 90 */
    "pad-4-choir",             /* 91 */
    "pad-5-bowed",             /* 92 */
    "pad-6-metallic",          /* 93 */
    "pad-7-halo",              /* 94 */
    "pad-8-sweep",             /* 95 */
    "fx-1-rain",               /* 96 */
    "fx-2-soundtrack",         /* 97 */
    "fx-3-crystal",            /* 98 */
    "fx-4-atmosphere",         /* 99 */
    "fx-5-brightness",         /* 100 */
    "fx-6-goblins",            /* 101 */
    "fx-7-echoes",             /* 102 */
    "fx-8-sci-fi",             /* 103 */
    "sitar",                   /* 104 */
    "banjo",                   /* 105 */
    "shamisen",                /* 106 */
    "koto",                    /* 107 */
    "kalimba",                 /* 108 */
    "bagpipe",                 /* 109 */
    "fiddle",                  /* 110 */
    "shanai",                  /* 111 */
    "tinkle-bell",             /* 112 */
    "agogo",                   /* 113 */
    "steel-drums",             /* 114 */
    "woodblock",               /* 115 */
    "taiko-drum",              /* 116 */
    "melodic-tom",             /* 117 */
    "synth-drum",              /* 118 */
    "reverse-cymbal",          /* 119 */
    "guitar-fret-noise",       /* 120 */
    "breath-noise",            /* 121 */
    "seashore",                /* 122 */
    "bird-tweet",              /* 123 */
    "telephone-ring",          /* 124 */
    "helicopter",              /* 125 */
    "applause",                /* 126 */
    "gunshot"                  /* 127 */
};

/* Percussion instruments */
static const char* PERCUSSION_INSTRUMENTS[] = {
    "percussion",
    "midi-percussion",
    "drum",
    "drums",
    "drum-kit",
    "drumkit",
    NULL
};

/* ============================================================================
 * Helper: Case-insensitive string compare
 * ============================================================================ */

static int strcasecmp_local(const char* s1, const char* s2) {
    while (*s1 && *s2) {
        int c1 = tolower((unsigned char)*s1);
        int c2 = tolower((unsigned char)*s2);
        if (c1 != c2) return c1 - c2;
        s1++;
        s2++;
    }
    return tolower((unsigned char)*s1) - tolower((unsigned char)*s2);
}

/* ============================================================================
 * Public Functions
 * ============================================================================ */

int alda_instrument_program(const char* name) {
    if (!name) return -1;

    /* Search Alda's names, then psnd's further names (case-insensitive) */
    for (int i = 0; ALDA_INSTRUMENTS[i].name != NULL; i++) {
        if (strcasecmp_local(ALDA_INSTRUMENTS[i].name, name) == 0) {
            return ALDA_INSTRUMENTS[i].program;
        }
    }
    for (int i = 0; GM_INSTRUMENTS[i].name != NULL; i++) {
        if (strcasecmp_local(GM_INSTRUMENTS[i].name, name) == 0) {
            return GM_INSTRUMENTS[i].program;
        }
    }

    return -1;  /* Not found */
}

const char* alda_instrument_name(int program) {
    if (program < 0 || program > 127) return NULL;
    return CANONICAL_NAMES[program];
}

int alda_instrument_is_percussion(const char* name) {
    if (!name) return 0;

    for (int i = 0; PERCUSSION_INSTRUMENTS[i] != NULL; i++) {
        if (strcasecmp_local(PERCUSSION_INSTRUMENTS[i], name) == 0) {
            return 1;
        }
    }

    return 0;
}

#include <stdlib.h>

char** alda_instrument_get_completions(const char* prefix, int* count, int max) {
    if (!count) return NULL;
    *count = 0;

    size_t prefix_len = prefix ? strlen(prefix) : 0;

    /* First pass: count matches using canonical names only (avoids duplicates) */
    int match_count = 0;
    for (int i = 0; i < 128 && match_count < max; i++) {
        const char* name = CANONICAL_NAMES[i];
        if (prefix_len == 0 || strncmp(name, prefix, prefix_len) == 0) {
            match_count++;
        }
    }
    /* Also check percussion instruments */
    for (int i = 0; PERCUSSION_INSTRUMENTS[i] != NULL && match_count < max; i++) {
        const char* name = PERCUSSION_INSTRUMENTS[i];
        if (prefix_len == 0 || strncmp(name, prefix, prefix_len) == 0) {
            match_count++;
        }
    }

    if (match_count == 0) return NULL;

    /* Allocate result array */
    char** result = malloc((match_count + 1) * sizeof(char*));
    if (!result) return NULL;

    /* Second pass: collect matches */
    int idx = 0;
    for (int i = 0; i < 128 && idx < match_count; i++) {
        const char* name = CANONICAL_NAMES[i];
        if (prefix_len == 0 || strncmp(name, prefix, prefix_len) == 0) {
            result[idx] = strdup(name);
            if (!result[idx]) {
                for (int j = 0; j < idx; j++) free(result[j]);
                free(result);
                return NULL;
            }
            idx++;
        }
    }
    for (int i = 0; PERCUSSION_INSTRUMENTS[i] != NULL && idx < match_count; i++) {
        const char* name = PERCUSSION_INSTRUMENTS[i];
        if (prefix_len == 0 || strncmp(name, prefix, prefix_len) == 0) {
            result[idx] = strdup(name);
            if (!result[idx]) {
                for (int j = 0; j < idx; j++) free(result[j]);
                free(result);
                return NULL;
            }
            idx++;
        }
    }
    result[idx] = NULL;

    /* Sort completions alphabetically */
    for (int i = 0; i < idx - 1; i++) {
        for (int j = i + 1; j < idx; j++) {
            if (strcmp(result[i], result[j]) > 0) {
                char* tmp = result[i];
                result[i] = result[j];
                result[j] = tmp;
            }
        }
    }

    *count = idx;
    return result;
}
