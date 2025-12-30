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

/* seq-note ( time pitch vel dur -- ) Add note to current sequence */
void op_seq_note(Stack* s) {
    int32_t dur = pop(&stack);
    int32_t vel = pop(&stack);
    int32_t pitch = pop(&stack);
    int32_t time = pop(&stack);

    if (current_seq < 0) {
        printf("No sequence selected (use seq-new first)\n");
        return;
    }

    int ch = default_channel;  /* use current default channel */
    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

/* seq-note-ch ( time ch pitch vel dur -- ) Add note with channel */
void op_seq_note_ch(Stack* s) {
    int32_t dur = pop(&stack);
    int32_t vel = pop(&stack);
    int32_t pitch = pop(&stack);
    int32_t ch = pop(&stack) - 1;  /* 1-16 -> 0-15 */
    int32_t time = pop(&stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
}

/* seq-add ( packed-note time -- ) Add packed note at time */
void op_seq_add(Stack* s) {
    int32_t time = pop(&stack);
    int32_t n = pop(&stack);

    if (current_seq < 0) {
        printf("No sequence selected\n");
        return;
    }

    int ch = note_ch(n);
    int pitch = note_pitch(n);
    int vel = note_vel(n);
    int dur = note_dur(n);

    add_event(time, EVT_NOTE_ON, ch, pitch, vel);
    add_event(time + dur, EVT_NOTE_OFF, ch, pitch, 0);
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
    fprintf(f, "seq-new drop\n\n");

    /* Track note-on events to pair with note-offs */
    int note_on_time[16][128];
    int note_on_vel[16][128];
    for (int ch = 0; ch < 16; ch++) {
        for (int p = 0; p < 128; p++) {
            note_on_time[ch][p] = -1;
            note_on_vel[ch][p] = 0;
        }
    }

    int notes_written = 0;

    for (int i = 0; i < seq->length; i++) {
        MidiEvent* e = &seq->events[i];

        if (e->type == EVT_NOTE_ON && e->data2 > 0) {
            note_on_time[e->channel][e->data1] = e->time;
            note_on_vel[e->channel][e->data1] = e->data2;
        } else if (e->type == EVT_NOTE_OFF ||
                   (e->type == EVT_NOTE_ON && e->data2 == 0)) {
            int start = note_on_time[e->channel][e->data1];
            if (start >= 0) {
                int dur = e->time - start;
                if (dur < 1) dur = 1;
                int vel = note_on_vel[e->channel][e->data1];

                fprintf(f, "%d %d %d %d %d seq-note-ch\n",
                        start, e->channel + 1, e->data1, vel, dur);
                notes_written++;

                note_on_time[e->channel][e->data1] = -1;
            }
        } else if (e->type == EVT_CC) {
            fprintf(f, "\\ t=%d CC ch=%d cc=%d val=%d\n",
                    e->time, e->channel + 1, e->data1, e->data2);
        }
    }

    fprintf(f, "\n\\ %d notes written\n", notes_written);
    fprintf(f, "\\ To play: midi-open seq-play midi-close\n");

    fclose(f);
    printf("Saved sequence %d (%d notes) to '%s'\n", current_seq, notes_written, filename);
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
