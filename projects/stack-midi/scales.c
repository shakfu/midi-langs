/* scales.c - Scale operations for stack-midi interpreter */

#include "stack_midi.h"

/* Scale lookup table entry */
typedef struct {
    const char* name;
    const int* intervals;
    int size;
} ScaleInfo;

/* Scale IDs - must match order in scale_table */
enum {
    SCALE_ID_MAJOR = 0,
    SCALE_ID_DORIAN,
    SCALE_ID_PHRYGIAN,
    SCALE_ID_LYDIAN,
    SCALE_ID_MIXOLYDIAN,
    SCALE_ID_MINOR,
    SCALE_ID_LOCRIAN,
    SCALE_ID_HARMONIC_MINOR,
    SCALE_ID_MELODIC_MINOR,
    SCALE_ID_PENTATONIC_MAJOR,
    SCALE_ID_PENTATONIC_MINOR,
    SCALE_ID_BLUES,
    SCALE_ID_WHOLE_TONE,
    SCALE_ID_CHROMATIC,
    SCALE_ID_DIMINISHED_HW,
    SCALE_ID_DIMINISHED_WH,
    SCALE_ID_AUGMENTED,
    SCALE_ID_BEBOP_DOMINANT,
    SCALE_ID_BEBOP_MAJOR,
    SCALE_ID_BEBOP_MINOR,
    SCALE_ID_HUNGARIAN_MINOR,
    SCALE_ID_DOUBLE_HARMONIC,
    SCALE_ID_NEAPOLITAN_MAJOR,
    SCALE_ID_NEAPOLITAN_MINOR,
    SCALE_ID_PHRYGIAN_DOMINANT,
    SCALE_ID_PERSIAN,
    SCALE_ID_ALTERED,
    SCALE_ID_ENIGMATIC,
    SCALE_ID_EGYPTIAN,
    SCALE_ID_ROMANIAN_MINOR,
    SCALE_ID_SPANISH_8_TONE,
    SCALE_ID_HIRAJOSHI,
    SCALE_ID_IN_SEN,
    SCALE_ID_IWATO,
    SCALE_ID_KUMOI,
    SCALE_ID_MAQAM_HIJAZ,
    SCALE_ID_MAQAM_NAHAWAND,
    SCALE_ID_MAQAM_NIKRIZ,
    SCALE_ID_MAQAM_ATHAR_KURD,
    SCALE_ID_MAQAM_SHAWQ_AFZA,
    SCALE_ID_MAQAM_JIHARKAH,
    SCALE_ID_RAGA_BHAIRAV,
    SCALE_ID_RAGA_TODI,
    SCALE_ID_RAGA_MARWA,
    SCALE_ID_RAGA_PURVI,
    SCALE_ID_RAGA_CHARUKESHI,
    SCALE_ID_RAGA_DARBARI,
    SCALE_ID_RAGA_KHAMAJ,
    SCALE_ID_RAGA_BHIMPALASI,
    SCALE_ID_COUNT
};

/* Scale lookup table */
static const ScaleInfo scale_table[] = {
    { "major",            SCALE_MAJOR,            SCALE_DIATONIC_SIZE },
    { "dorian",           SCALE_DORIAN,           SCALE_DIATONIC_SIZE },
    { "phrygian",         SCALE_PHRYGIAN,         SCALE_DIATONIC_SIZE },
    { "lydian",           SCALE_LYDIAN,           SCALE_DIATONIC_SIZE },
    { "mixolydian",       SCALE_MIXOLYDIAN,       SCALE_DIATONIC_SIZE },
    { "minor",            SCALE_MINOR,            SCALE_DIATONIC_SIZE },
    { "locrian",          SCALE_LOCRIAN,          SCALE_DIATONIC_SIZE },
    { "harmonic-minor",   SCALE_HARMONIC_MINOR,   SCALE_DIATONIC_SIZE },
    { "melodic-minor",    SCALE_MELODIC_MINOR,    SCALE_DIATONIC_SIZE },
    { "pentatonic",       SCALE_PENTATONIC_MAJOR, SCALE_PENTATONIC_SIZE },
    { "pentatonic-minor", SCALE_PENTATONIC_MINOR, SCALE_PENTATONIC_SIZE },
    { "blues",            SCALE_BLUES,            SCALE_BLUES_SIZE },
    { "whole-tone",       SCALE_WHOLE_TONE,       SCALE_WHOLE_TONE_SIZE },
    { "chromatic",        SCALE_CHROMATIC,        SCALE_CHROMATIC_SIZE },
    { "diminished-hw",    SCALE_DIMINISHED_HW,    SCALE_DIMINISHED_SIZE },
    { "diminished-wh",    SCALE_DIMINISHED_WH,    SCALE_DIMINISHED_SIZE },
    { "augmented",        SCALE_AUGMENTED,        SCALE_AUGMENTED_SIZE },
    { "bebop-dominant",   SCALE_BEBOP_DOMINANT,   SCALE_BEBOP_SIZE },
    { "bebop-major",      SCALE_BEBOP_MAJOR,      SCALE_BEBOP_SIZE },
    { "bebop-minor",      SCALE_BEBOP_MINOR,      SCALE_BEBOP_SIZE },
    { "hungarian-minor",  SCALE_HUNGARIAN_MINOR,  SCALE_DIATONIC_SIZE },
    { "double-harmonic",  SCALE_DOUBLE_HARMONIC,  SCALE_DIATONIC_SIZE },
    { "neapolitan-major", SCALE_NEAPOLITAN_MAJOR, SCALE_DIATONIC_SIZE },
    { "neapolitan-minor", SCALE_NEAPOLITAN_MINOR, SCALE_DIATONIC_SIZE },
    { "phrygian-dominant", SCALE_PHRYGIAN_DOMINANT, SCALE_DIATONIC_SIZE },
    { "persian",          SCALE_PERSIAN,           SCALE_DIATONIC_SIZE },
    { "altered",          SCALE_ALTERED,           SCALE_DIATONIC_SIZE },
    { "enigmatic",        SCALE_ENIGMATIC,         SCALE_DIATONIC_SIZE },
    { "egyptian",         SCALE_EGYPTIAN,          SCALE_PENTATONIC_SIZE },
    { "romanian-minor",   SCALE_ROMANIAN_MINOR,    SCALE_DIATONIC_SIZE },
    { "spanish-8-tone",   SCALE_SPANISH_8_TONE,    8 },
    { "hirajoshi",        SCALE_HIRAJOSHI,         SCALE_PENTATONIC_SIZE },
    { "in-sen",           SCALE_IN_SEN,            SCALE_PENTATONIC_SIZE },
    { "iwato",            SCALE_IWATO,             SCALE_PENTATONIC_SIZE },
    { "kumoi",            SCALE_KUMOI,             SCALE_PENTATONIC_SIZE },
    { "maqam-hijaz",      SCALE_MAQAM_HIJAZ,       SCALE_DIATONIC_SIZE },
    { "maqam-nahawand",   SCALE_MAQAM_NAHAWAND,    SCALE_DIATONIC_SIZE },
    { "maqam-nikriz",     SCALE_MAQAM_NIKRIZ,      SCALE_DIATONIC_SIZE },
    { "maqam-athar-kurd", SCALE_MAQAM_ATHAR_KURD,  SCALE_DIATONIC_SIZE },
    { "maqam-shawq-afza", SCALE_MAQAM_SHAWQ_AFZA,  SCALE_DIATONIC_SIZE },
    { "maqam-jiharkah",   SCALE_MAQAM_JIHARKAH,    SCALE_DIATONIC_SIZE },
    { "raga-bhairav",     SCALE_RAGA_BHAIRAV,      SCALE_DIATONIC_SIZE },
    { "raga-todi",        SCALE_RAGA_TODI,         SCALE_DIATONIC_SIZE },
    { "raga-marwa",       SCALE_RAGA_MARWA,        SCALE_DIATONIC_SIZE },
    { "raga-purvi",       SCALE_RAGA_PURVI,        SCALE_DIATONIC_SIZE },
    { "raga-charukeshi",  SCALE_RAGA_CHARUKESHI,   SCALE_DIATONIC_SIZE },
    { "raga-darbari",     SCALE_RAGA_DARBARI,      SCALE_DIATONIC_SIZE },
    { "raga-khamaj",      SCALE_RAGA_KHAMAJ,       SCALE_DIATONIC_SIZE },
    { "raga-bhimpalasi",  SCALE_RAGA_BHIMPALASI,   SCALE_PENTATONIC_SIZE },
};

/* Scale constant words - push scale ID */
void op_scale_major(Stack* s) { push(&stack, SCALE_ID_MAJOR); }
void op_scale_dorian(Stack* s) { push(&stack, SCALE_ID_DORIAN); }
void op_scale_phrygian(Stack* s) { push(&stack, SCALE_ID_PHRYGIAN); }
void op_scale_lydian(Stack* s) { push(&stack, SCALE_ID_LYDIAN); }
void op_scale_mixolydian(Stack* s) { push(&stack, SCALE_ID_MIXOLYDIAN); }
void op_scale_minor(Stack* s) { push(&stack, SCALE_ID_MINOR); }
void op_scale_locrian(Stack* s) { push(&stack, SCALE_ID_LOCRIAN); }
void op_scale_harmonic_minor(Stack* s) { push(&stack, SCALE_ID_HARMONIC_MINOR); }
void op_scale_melodic_minor(Stack* s) { push(&stack, SCALE_ID_MELODIC_MINOR); }
void op_scale_pentatonic(Stack* s) { push(&stack, SCALE_ID_PENTATONIC_MAJOR); }
void op_scale_pentatonic_minor(Stack* s) { push(&stack, SCALE_ID_PENTATONIC_MINOR); }
void op_scale_blues(Stack* s) { push(&stack, SCALE_ID_BLUES); }
void op_scale_whole_tone(Stack* s) { push(&stack, SCALE_ID_WHOLE_TONE); }
void op_scale_chromatic(Stack* s) { push(&stack, SCALE_ID_CHROMATIC); }
void op_scale_diminished_hw(Stack* s) { push(&stack, SCALE_ID_DIMINISHED_HW); }
void op_scale_diminished_wh(Stack* s) { push(&stack, SCALE_ID_DIMINISHED_WH); }
void op_scale_augmented_scale(Stack* s) { push(&stack, SCALE_ID_AUGMENTED); }
void op_scale_bebop_dominant(Stack* s) { push(&stack, SCALE_ID_BEBOP_DOMINANT); }
void op_scale_bebop_major(Stack* s) { push(&stack, SCALE_ID_BEBOP_MAJOR); }
void op_scale_bebop_minor(Stack* s) { push(&stack, SCALE_ID_BEBOP_MINOR); }
void op_scale_hungarian_minor(Stack* s) { push(&stack, SCALE_ID_HUNGARIAN_MINOR); }
void op_scale_double_harmonic(Stack* s) { push(&stack, SCALE_ID_DOUBLE_HARMONIC); }
void op_scale_neapolitan_major(Stack* s) { push(&stack, SCALE_ID_NEAPOLITAN_MAJOR); }
void op_scale_neapolitan_minor(Stack* s) { push(&stack, SCALE_ID_NEAPOLITAN_MINOR); }
static void op_scale_phrygian_dominant(Stack* s) { (void)s; push(&stack, SCALE_ID_PHRYGIAN_DOMINANT); }
static void op_scale_persian(Stack* s) { (void)s; push(&stack, SCALE_ID_PERSIAN); }
static void op_scale_altered(Stack* s) { (void)s; push(&stack, SCALE_ID_ALTERED); }
static void op_scale_enigmatic(Stack* s) { (void)s; push(&stack, SCALE_ID_ENIGMATIC); }
static void op_scale_egyptian(Stack* s) { (void)s; push(&stack, SCALE_ID_EGYPTIAN); }
static void op_scale_romanian_minor(Stack* s) { (void)s; push(&stack, SCALE_ID_ROMANIAN_MINOR); }
static void op_scale_spanish_8_tone(Stack* s) { (void)s; push(&stack, SCALE_ID_SPANISH_8_TONE); }
static void op_scale_hirajoshi(Stack* s) { (void)s; push(&stack, SCALE_ID_HIRAJOSHI); }
static void op_scale_in_sen(Stack* s) { (void)s; push(&stack, SCALE_ID_IN_SEN); }
static void op_scale_iwato(Stack* s) { (void)s; push(&stack, SCALE_ID_IWATO); }
static void op_scale_kumoi(Stack* s) { (void)s; push(&stack, SCALE_ID_KUMOI); }
static void op_scale_maqam_hijaz(Stack* s) { (void)s; push(&stack, SCALE_ID_MAQAM_HIJAZ); }
static void op_scale_maqam_nahawand(Stack* s) { (void)s; push(&stack, SCALE_ID_MAQAM_NAHAWAND); }
static void op_scale_maqam_nikriz(Stack* s) { (void)s; push(&stack, SCALE_ID_MAQAM_NIKRIZ); }
static void op_scale_maqam_athar_kurd(Stack* s) { (void)s; push(&stack, SCALE_ID_MAQAM_ATHAR_KURD); }
static void op_scale_maqam_shawq_afza(Stack* s) { (void)s; push(&stack, SCALE_ID_MAQAM_SHAWQ_AFZA); }
static void op_scale_maqam_jiharkah(Stack* s) { (void)s; push(&stack, SCALE_ID_MAQAM_JIHARKAH); }
static void op_scale_raga_bhairav(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_BHAIRAV); }
static void op_scale_raga_todi(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_TODI); }
static void op_scale_raga_marwa(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_MARWA); }
static void op_scale_raga_purvi(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_PURVI); }
static void op_scale_raga_charukeshi(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_CHARUKESHI); }
static void op_scale_raga_darbari(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_DARBARI); }
static void op_scale_raga_khamaj(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_KHAMAJ); }
static void op_scale_raga_bhimpalasi(Stack* s) { (void)s; push(&stack, SCALE_ID_RAGA_BHIMPALASI); }

/* scale ( root scale-id -- p1 p2 ... pN N ) build scale and push all pitches + count */
static void op_scale(Stack* s) {
    int32_t scale_id = pop(&stack);
    int32_t root = pop(&stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        stack_error("Invalid scale ID: %d", scale_id);
        push(&stack, 0);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int pitches[16];
    int count = music_build_scale(root, info->intervals, info->size, pitches);

    for (int i = 0; i < count; i++) {
        push(&stack, pitches[i]);
    }
    push(&stack, count);
}

/* play-scale ( root scale-id -- ) play the scale ascending with the current defaults */
static void op_play_scale(Stack* s) {
    (void)s;
    int32_t scale_id = pop(&stack);
    int32_t root = pop(&stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        stack_error("Invalid scale ID: %d", scale_id);
        return;
    }
    if (midi_out == NULL) {
        stack_error("No MIDI output open");
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int pitches[16];
    int count = music_build_scale(root, info->intervals, info->size, pitches);
    for (int i = 0; i < count; i++) {
        play_single_note(&stack, pitches[i]);
    }
}

/* degree ( root scale-id degree -- pitch ) get nth degree of scale (1-based) */
static void op_degree(Stack* s) {
    int32_t deg = pop(&stack);
    int32_t scale_id = pop(&stack);
    int32_t root = pop(&stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        stack_error("Invalid scale ID: %d", scale_id);
        push(&stack, root);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int pitch = music_scale_degree(root, info->intervals, info->size, deg);
    push(&stack, pitch >= 0 ? pitch : root);
}

/* in-scale? ( pitch root scale-id -- flag ) check if pitch is in scale */
static void op_in_scale(Stack* s) {
    int32_t scale_id = pop(&stack);
    int32_t root = pop(&stack);
    int32_t pitch = pop(&stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        stack_error("Invalid scale ID: %d", scale_id);
        push(&stack, 0);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int result = music_in_scale(pitch, root, info->intervals, info->size);
    push(&stack, result ? -1 : 0);  /* Forth true = -1 */
}

/* quantize ( pitch root scale-id -- quantized-pitch ) */
static void op_quantize(Stack* s) {
    int32_t scale_id = pop(&stack);
    int32_t root = pop(&stack);
    int32_t pitch = pop(&stack);

    if (scale_id < 0 || scale_id >= SCALE_ID_COUNT) {
        stack_error("Invalid scale ID: %d", scale_id);
        push(&stack, pitch);
        return;
    }

    const ScaleInfo* info = &scale_table[scale_id];
    int result = music_quantize_to_scale(pitch, root, info->intervals, info->size);
    push(&stack, result);
}

/* scales ( -- ) list all available scales */
static void op_scales(Stack* s) {
    (void)stack;
    printf("Available scales (%d total):\n", SCALE_ID_COUNT);
    for (int i = 0; i < SCALE_ID_COUNT; i++) {
        printf("  %2d: scale-%s\n", i, scale_table[i].name);
    }
}

/* cents>bend ( cents -- bend ) pitch bend value for a cents offset (+/-2 semitone range) */
static void op_cents_to_bend(Stack* s) {
    (void)s;
    push(&stack, music_cents_to_bend(pop(&stack)));
}

/* pb-cents ( cents ch -- ) send a pitch bend in cents */
static void op_pb_cents(Stack* s) {
    (void)s;
    if (stack.top < 1) {
        stack_error("pb-cents needs cents and channel");
        return;
    }
    int32_t channel = pop(&stack);
    int32_t cents = pop(&stack);
    if (channel < 1 || channel > 16) {
        stack_error("Channel must be 1-16");
        return;
    }
    if (midi_out == NULL) {
        stack_error("No MIDI output open");
        return;
    }
    midi_send_pitch_bend(music_cents_to_bend(cents), channel);
}

/* Register all scale words */
void register_scale_words(void) {
    /* Scale constants */
    add_word("scale-major", op_scale_major, 1);
    add_word("scale-dorian", op_scale_dorian, 1);
    add_word("scale-phrygian", op_scale_phrygian, 1);
    add_word("scale-lydian", op_scale_lydian, 1);
    add_word("scale-mixolydian", op_scale_mixolydian, 1);
    add_word("scale-minor", op_scale_minor, 1);
    add_word("scale-locrian", op_scale_locrian, 1);
    add_word("scale-harmonic-minor", op_scale_harmonic_minor, 1);
    add_word("scale-melodic-minor", op_scale_melodic_minor, 1);
    add_word("scale-pentatonic", op_scale_pentatonic, 1);
    add_word("scale-pentatonic-minor", op_scale_pentatonic_minor, 1);
    add_word("scale-blues", op_scale_blues, 1);
    add_word("scale-whole-tone", op_scale_whole_tone, 1);
    add_word("scale-chromatic", op_scale_chromatic, 1);
    add_word("scale-diminished-hw", op_scale_diminished_hw, 1);
    add_word("scale-diminished-wh", op_scale_diminished_wh, 1);
    add_word("scale-augmented", op_scale_augmented_scale, 1);
    add_word("scale-bebop-dominant", op_scale_bebop_dominant, 1);
    add_word("scale-bebop-major", op_scale_bebop_major, 1);
    add_word("scale-bebop-minor", op_scale_bebop_minor, 1);
    add_word("scale-hungarian-minor", op_scale_hungarian_minor, 1);
    add_word("scale-double-harmonic", op_scale_double_harmonic, 1);
    add_word("scale-neapolitan-major", op_scale_neapolitan_major, 1);
    add_word("scale-neapolitan-minor", op_scale_neapolitan_minor, 1);
    add_word("scale-phrygian-dominant", op_scale_phrygian_dominant, 1);
    add_word("scale-persian", op_scale_persian, 1);
    add_word("scale-altered", op_scale_altered, 1);
    add_word("scale-enigmatic", op_scale_enigmatic, 1);
    add_word("scale-egyptian", op_scale_egyptian, 1);
    add_word("scale-romanian-minor", op_scale_romanian_minor, 1);
    add_word("scale-spanish-8-tone", op_scale_spanish_8_tone, 1);
    add_word("scale-hirajoshi", op_scale_hirajoshi, 1);
    add_word("scale-in-sen", op_scale_in_sen, 1);
    add_word("scale-iwato", op_scale_iwato, 1);
    add_word("scale-kumoi", op_scale_kumoi, 1);
    add_word("scale-maqam-hijaz", op_scale_maqam_hijaz, 1);
    add_word("scale-maqam-nahawand", op_scale_maqam_nahawand, 1);
    add_word("scale-maqam-nikriz", op_scale_maqam_nikriz, 1);
    add_word("scale-maqam-athar-kurd", op_scale_maqam_athar_kurd, 1);
    add_word("scale-maqam-shawq-afza", op_scale_maqam_shawq_afza, 1);
    add_word("scale-maqam-jiharkah", op_scale_maqam_jiharkah, 1);
    add_word("scale-raga-bhairav", op_scale_raga_bhairav, 1);
    add_word("scale-raga-todi", op_scale_raga_todi, 1);
    add_word("scale-raga-marwa", op_scale_raga_marwa, 1);
    add_word("scale-raga-purvi", op_scale_raga_purvi, 1);
    add_word("scale-raga-charukeshi", op_scale_raga_charukeshi, 1);
    add_word("scale-raga-darbari", op_scale_raga_darbari, 1);
    add_word("scale-raga-khamaj", op_scale_raga_khamaj, 1);
    add_word("scale-raga-bhimpalasi", op_scale_raga_bhimpalasi, 1);

    /* Scale operations */
    add_word("scale", op_scale, 1);
    add_word("play-scale", op_play_scale, 1);
    add_word("degree", op_degree, 1);
    add_word("in-scale?", op_in_scale, 1);
    add_word("quantize", op_quantize, 1);
    add_word("scales", op_scales, 1);

    /* Microtonal pitch bend */
    add_word("cents>bend", op_cents_to_bend, 1);
    add_word("pb-cents", op_pb_cents, 1);
}
