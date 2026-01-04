/* sequences.c - Sequence operations for MIDI Forth interpreter */

#include "forth_midi.h"

/* All globals now accessed via g_ctx macros defined in forth_midi.h */

/* Helper to sort events by time (simple insertion sort) */
static void seq_sort(Sequence* seq) {
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

/* Helper to add event to current sequence */
static void add_event(int time, int type, int ch, int d1, int d2) {
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    Sequence* seq = &sequences[current_seq];
    if (seq->length >= MAX_SEQ_EVENTS) {
        printf("Sequence full\n");
        return;
    }
    MidiEvent* e = &seq->events[seq->length++];
    e->time = time;
    e->type = type;
    e->channel = ch;
    e->data1 = d1;
    e->data2 = d2;
}

/* seq-new ( -- seq-id ) Create new sequence */
void op_seq_new(Stack* s) {
    (void)s;
    if (sequence_count >= MAX_SEQUENCES) {
        printf("Max sequences reached\n");
        push(&stack, -1);
        return;
    }
    int id = sequence_count++;
    sequences[id].length = 0;
    sequences[id].bpm = global_bpm;
    current_seq = id;
    push(&stack, id);
}

/* seq-new! ( -- ) Create new sequence without returning ID */
void op_seq_new_store(Stack* s) {
    (void)s;
    if (sequence_count >= MAX_SEQUENCES) {
        printf("Max sequences reached\n");
        return;
    }
    int id = sequence_count++;
    sequences[id].length = 0;
    sequences[id].bpm = global_bpm;
    current_seq = id;
}

/* seq ( id -- ) Select sequence as current */
void op_seq_select(Stack* s) {
    int32_t id = pop(&stack);
    if (id < 0 || id >= sequence_count) {
        printf("Invalid sequence id: %d\n", id);
        return;
    }
    current_seq = id;
}

/* seq@ ( -- id ) Get current sequence id */
void op_seq_current(Stack* s) {
    push(&stack, current_seq);
}

/* seq-start ( id -- ) Start recording to sequence */
void op_seq_start(Stack* s) {
    int32_t id = pop(&stack);

    /* Auto-create sequence if it doesn't exist */
    while (sequence_count <= id && sequence_count < MAX_SEQUENCES) {
        sequences[sequence_count].length = 0;
        sequences[sequence_count].bpm = global_bpm;
        sequence_count++;
    }

    if (id < 0 || id >= MAX_SEQUENCES) {
        printf("Invalid sequence id: %d\n", id);
        return;
    }

    /* Clear the sequence to start fresh */
    sequences[id].length = 0;
    sequences[id].bpm = global_bpm;

    /* Enter recording mode */
    seq_recording_mode = 1;
    seq_recording_id = id;
    seq_recording_time = 0;
    current_seq = id;
}

/* seq-end ( id -- ) Stop recording to sequence */
void op_seq_end(Stack* s) {
    int32_t id = pop(&stack);

    if (!seq_recording_mode) {
        printf("Not currently recording\n");
        return;
    }

    if (id != seq_recording_id) {
        printf("Warning: seq-end id (%d) doesn't match seq-start id (%d)\n",
               id, seq_recording_id);
    }

    seq_recording_mode = 0;
    printf("Recorded %d events to sequence %d\n",
           sequences[seq_recording_id].length, seq_recording_id);
}

/* seq-length ( -- n ) Get length of current sequence */
void op_seq_length(Stack* s) {
    if (current_seq < 0) {
        push(&stack, 0);
        return;
    }
    push(&stack, sequences[current_seq].length);
}

/* seq-clear ( -- ) Clear current sequence */
void op_seq_clear(Stack* s) {
    (void)stack;
    if (current_seq < 0) return;
    sequences[current_seq].length = 0;
}

/* seq-note ( time pitch vel dur -- ) Add note at time using default channel */
void op_seq_note(Stack* s) {
    (void)s;
    int32_t dur = pop(&stack);
    int32_t vel = pop(&stack);
    int32_t pitch = pop(&stack);
    int32_t time = pop(&stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length >= MAX_SEQ_EVENTS - 2) {
        printf("Sequence full\n");
        return;
    }

    int ch = default_channel - 1;  /* 1-16 -> 0-15 */

    /* Add note-on */
    add_event(time, EVT_NOTE_ON, ch, pitch, vel);

    /* Add note-off */
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

/* seq-note-ch ( time ch pitch vel dur -- ) Add note with specific channel */
void op_seq_note_ch(Stack* s) {
    (void)s;
    int32_t dur = pop(&stack);
    int32_t vel = pop(&stack);
    int32_t pitch = pop(&stack);
    int32_t ch = pop(&stack);
    int32_t time = pop(&stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length >= MAX_SEQ_EVENTS - 2) {
        printf("Sequence full\n");
        return;
    }

    int channel = ch - 1;  /* 1-16 -> 0-15 */
    if (channel < 0 || channel > 15) channel = 0;

    /* Add note-on */
    add_event(time, EVT_NOTE_ON, channel, pitch, vel);

    /* Add note-off */
    add_event(time + dur, EVT_NOTE_OFF, channel, pitch, 0);
}

/* seq-add ( packed-note time -- ) Add packed note at time */
void op_seq_add(Stack* s) {
    (void)s;
    int32_t time = pop(&stack);
    int32_t packed = pop(&stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length >= MAX_SEQ_EVENTS - 2) {
        printf("Sequence full\n");
        return;
    }

    /* Unpack note: pitch=bits 0-6, vel=bits 7-13, ch=bits 14-17, dur=bits 18-31 */
    int pitch = packed & 0x7F;
    int vel = (packed >> 7) & 0x7F;
    int ch = (packed >> 14) & 0x0F;
    int dur = (packed >> 18) & 0x3FFF;

    /* Add note-on */
    add_event(time, EVT_NOTE_ON, ch, pitch, vel);

    /* Add note-off */
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

/* seq-play ( -- ) Play current sequence */
void op_seq_play(Stack* s) {
    (void)stack;
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    if (midi_out == NULL) {
        printf("No MIDI output open\n");
        return;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) {
        printf("Sequence empty\n");
        return;
    }

    /* Sort events by time */
    seq_sort(seq);

    int bpm = seq->bpm > 0 ? seq->bpm : global_bpm;
    int last_time = 0;

    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];

        /* Wait until this event's time */
        if (e->time > last_time) {
            int ticks = e->time - last_time;
            int ms = (ticks * 60000) / (TICKS_PER_QUARTER * bpm);
            midi_sleep_ms(ms);
            last_time = e->time;
        }

        /* Send the event */
        unsigned char msg[3];
        switch (e->type) {
            case EVT_NOTE_ON:
                msg[0] = 0x90 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = e->data2 & 0x7F;
                capture_add_event(0, e->channel, e->data1, e->data2);
                break;
            case EVT_NOTE_OFF:
                msg[0] = 0x80 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = 0;
                capture_add_event(1, e->channel, e->data1, 0);
                break;
            case EVT_CC:
                msg[0] = 0xB0 | (e->channel & 0x0F);
                msg[1] = e->data1 & 0x7F;
                msg[2] = e->data2 & 0x7F;
                capture_add_event(2, e->channel, e->data1, e->data2);
                break;
            default:
                continue;
        }
        libremidi_midi_out_send_message(midi_out, msg, 3);
    }
}

/* seq-transpose ( semitones -- ) Transpose all notes in current sequence */
void op_seq_transpose(Stack* s) {
    int32_t semi = pop(&stack);
    if (current_seq < 0) return;

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < seq->length; i++) {
        if (seq->events[i].type == EVT_NOTE_ON || seq->events[i].type == EVT_NOTE_OFF) {
            int p = seq->events[i].data1 + semi;
            if (p < 0) p = 0;
            if (p > 127) p = 127;
            seq->events[i].data1 = p;
        }
    }
}

/* seq-show ( -- ) Show current sequence events */
void op_seq_show(Stack* s) {
    (void)stack;
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }
    Sequence* seq = &sequences[current_seq];
    printf("Sequence %d: %d events, bpm=%d\n", current_seq, seq->length, seq->bpm);
    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];
        const char* type_str = e->type == EVT_NOTE_ON ? "ON " :
                               e->type == EVT_NOTE_OFF ? "OFF" : "CC ";
        printf("  t=%4d %s ch=%d d1=%3d d2=%3d\n",
               e->time, type_str, e->channel + 1, e->data1, e->data2);
    }
}

/* seq-reverse ( -- ) Reverse timing of current sequence */
void op_seq_reverse(Stack* s) {
    (void)stack;
    if (current_seq < 0) return;
    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) return;

    /* Find max time */
    int max_time = 0;
    for (int i = 0; i < seq->length; i++) {
        if (seq->events[i].time > max_time)
            max_time = seq->events[i].time;
    }

    /* Reverse times and swap note-on/off */
    for (int i = 0; i < seq->length; i++) {
        seq->events[i].time = max_time - seq->events[i].time;
        if (seq->events[i].type == EVT_NOTE_ON)
            seq->events[i].type = EVT_NOTE_OFF;
        else if (seq->events[i].type == EVT_NOTE_OFF)
            seq->events[i].type = EVT_NOTE_ON;
    }
    seq_sort(seq);
}

/* seq-stretch ( factor -- ) Multiply all times by factor/100 */
void op_seq_stretch(Stack* s) {
    int32_t factor = pop(&stack);
    if (current_seq < 0) return;

    Sequence* seq = &sequences[current_seq];
    for (int i = 0; i < seq->length; i++) {
        seq->events[i].time = (seq->events[i].time * factor) / 100;
    }
}

/* bpm! ( n -- ) Set tempo */
void op_bpm_store(Stack* s) {
    int32_t bpm = pop(&stack);
    if (bpm < 20) bpm = 20;
    if (bpm > 300) bpm = 300;
    global_bpm = bpm;
}

/* bpm@ ( -- n ) Get tempo */
void op_bpm_fetch(Stack* s) {
    push(&stack, global_bpm);
}

/* ============================================================================
 * Sequence Export Functions
 * ============================================================================ */

#include "midi_file.h"

/* seq-write-mid ( filename -- ) Write current sequence to MIDI file */
int seq_write_mid(const char* filename) {
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return -1;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) {
        printf("Sequence is empty\n");
        return -1;
    }

    /* Sort events by time */
    seq_sort(seq);

    int ppqn = TICKS_PER_QUARTER;
    int bpm = seq->bpm > 0 ? seq->bpm : global_bpm;

    /* Create MIDI file writer */
    midi_file_writer* writer = NULL;
    if (midi_file_writer_new(&writer) != 0) {
        printf("Error: cannot create MIDI file writer\n");
        return -1;
    }

    /* Set PPQN and add a track for events */
    midi_file_writer_set_ppqn(writer, ppqn);
    midi_file_writer_add_track(writer);

    /* Add tempo meta event */
    int us_per_beat = 60000000 / bpm;
    midi_file_writer_tempo(writer, 0, 0, us_per_beat);

    /* Convert sequence events to MIDI file events */
    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];
        int tick = e->time;  /* Already in ticks */

        if (e->type == EVT_NOTE_ON) {
            midi_file_writer_note_on(writer, tick, 0, e->channel + 1, e->data1, e->data2);
        } else if (e->type == EVT_NOTE_OFF) {
            midi_file_writer_note_off(writer, tick, 0, e->channel + 1, e->data1, 0);
        } else if (e->type == EVT_CC) {
            midi_file_writer_cc(writer, tick, 0, e->channel + 1, e->data1, e->data2);
        }
    }

    /* Write to file */
    int result = midi_file_writer_save(writer, filename);
    midi_file_writer_free(writer);

    if (result != 0) {
        printf("Error: failed to write MIDI file '%s'\n", filename);
        return -1;
    }

    printf("Saved sequence %d (%d events) to '%s'\n", current_seq, seq->length, filename);
    return 0;
}

/* Helper: convert ticks to ms based on BPM */
static int ticks_to_ms(int ticks, int bpm) {
    return (ticks * 60000) / (TICKS_PER_QUARTER * bpm);
}

/* Helper: pitch number to note name */
static const char* pitch_to_name(int pitch) {
    static char buf[8];
    static const char* names[] = {"c", "c#", "d", "d#", "e", "f", "f#", "g", "g#", "a", "a#", "b"};
    int octave = (pitch / 12) - 1;
    int note = pitch % 12;
    snprintf(buf, sizeof(buf), "%s%d", names[note], octave);
    return buf;
}

/* seq-save ( filename -- ) Save current sequence as Forth script */
int seq_save(const char* filename) {
    if (current_seq < 0) {
        printf("No sequence selected\n");
        return -1;
    }

    Sequence* seq = &sequences[current_seq];
    if (seq->length == 0) {
        printf("Sequence is empty\n");
        return -1;
    }

    FILE* f = fopen(filename, "w");
    if (f == NULL) {
        printf("Error: cannot create file '%s'\n", filename);
        return -1;
    }

    /* Sort events by time */
    seq_sort(seq);

    int bpm = seq->bpm > 0 ? seq->bpm : global_bpm;

    /* Write header */
    fprintf(f, "\\ Sequence export\n");
    fprintf(f, "\\ Generated by midi_forth\n\n");
    fprintf(f, "%d bpm!\n", bpm);
    fprintf(f, "0 seq-start\n\n");

    /* Track note-on events to pair with note-offs */
    int note_on_time[16][128];
    int note_on_vel[16][128];
    for (int ch = 0; ch < 16; ch++) {
        for (int p = 0; p < 128; p++) {
            note_on_time[ch][p] = -1;
            note_on_vel[ch][p] = 0;
        }
    }

    /* Build list of complete notes */
    typedef struct { int time; int pitch; int vel; int dur; int ch; } Note;
    Note notes[MAX_SEQ_EVENTS];
    int note_count = 0;

    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];

        if (e->type == EVT_NOTE_ON && e->data2 > 0) {
            note_on_time[e->channel][e->data1] = e->time;
            note_on_vel[e->channel][e->data1] = e->data2;
        } else if (e->type == EVT_NOTE_OFF ||
                   (e->type == EVT_NOTE_ON && e->data2 == 0)) {
            int start = note_on_time[e->channel][e->data1];
            if (start >= 0 && note_count < MAX_SEQ_EVENTS) {
                notes[note_count].time = start;
                notes[note_count].pitch = e->data1;
                notes[note_count].vel = note_on_vel[e->channel][e->data1];
                notes[note_count].dur = e->time - start;
                notes[note_count].ch = e->channel + 1;
                note_count++;
                note_on_time[e->channel][e->data1] = -1;
            }
        }
    }

    /* Sort notes by time (simple insertion sort) */
    for (int i = 1; i < note_count; i++) {
        Note tmp = notes[i];
        int j = i - 1;
        while (j >= 0 && notes[j].time > tmp.time) {
            notes[j + 1] = notes[j];
            j--;
        }
        notes[j + 1] = tmp;
    }

    /* Output notes using new syntax */
    int last_vel = -1;
    int last_dur = -1;
    int last_ch = -1;

    for (int i = 0; i < note_count; i++) {
        Note* n = &notes[i];
        int dur_ms = ticks_to_ms(n->dur, bpm);
        if (dur_ms < 1) dur_ms = 1;

        /* Set parameters if changed */
        if (n->ch != last_ch) {
            fprintf(f, "%d ch! ", n->ch);
            last_ch = n->ch;
        }
        if (n->vel != last_vel) {
            fprintf(f, "%d vel! ", n->vel);
            last_vel = n->vel;
        }
        if (dur_ms != last_dur) {
            fprintf(f, "%d dur! ", dur_ms);
            last_dur = dur_ms;
        }

        /* Output note */
        fprintf(f, "%s,\n", pitch_to_name(n->pitch));
    }

    fprintf(f, "\n0 seq-end\n");
    fprintf(f, "\n\\ %d notes exported\n", note_count);
    fprintf(f, "\\ To play: midi-open 0 seq seq-play midi-close\n");

    fclose(f);
    printf("Saved sequence %d (%d notes) to '%s'\n", current_seq, note_count, filename);
    return 0;
}

/* Stack operations for sequence export */
void op_seq_write_mid(Stack* s) {
    (void)s;
    /* Handled specially in interpreter */
}

void op_seq_save(Stack* s) {
    (void)s;
    /* Handled specially in interpreter */
}
