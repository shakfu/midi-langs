/*
 * midi_file.h - C wrapper for libremidi MIDI file I/O
 *
 * Provides C API for reading and writing standard MIDI files (.mid)
 * using libremidi's reader and writer classes.
 */

#ifndef MIDI_FILE_H
#define MIDI_FILE_H

#include <stdint.h>
#include <stddef.h>

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * MIDI File Writer
 * ============================================================================ */

typedef struct midi_file_writer midi_file_writer;

/* Create a new MIDI file writer
 * Returns 0 on success, non-zero on error */
int midi_file_writer_new(midi_file_writer** writer);

/* Free a MIDI file writer */
void midi_file_writer_free(midi_file_writer* writer);

/* Set ticks per quarter note (default: 480) */
void midi_file_writer_set_ppqn(midi_file_writer* writer, uint16_t ppqn);

/* Add a new track to the MIDI file */
void midi_file_writer_add_track(midi_file_writer* writer);

/* Add a note-on event
 * tick: time in ticks
 * track: track number (0-based)
 * channel: MIDI channel (1-16)
 * pitch: MIDI note number (0-127)
 * velocity: note velocity (0-127) */
void midi_file_writer_note_on(midi_file_writer* writer, int tick, int track,
                               int channel, int pitch, int velocity);

/* Add a note-off event */
void midi_file_writer_note_off(midi_file_writer* writer, int tick, int track,
                                int channel, int pitch, int velocity);

/* Add a control change event */
void midi_file_writer_cc(midi_file_writer* writer, int tick, int track,
                          int channel, int control, int value);

/* Add a program change event */
void midi_file_writer_program(midi_file_writer* writer, int tick, int track,
                               int channel, int program);

/* Add a pitch bend event (value: 0-16383, center=8192) */
void midi_file_writer_pitch_bend(midi_file_writer* writer, int tick, int track,
                                  int channel, int value);

/* Add a tempo change event (microseconds per quarter note) */
void midi_file_writer_tempo(midi_file_writer* writer, int tick, int track,
                             int tempo_uspqn);

/* Add a tempo change event by BPM */
void midi_file_writer_tempo_bpm(midi_file_writer* writer, int tick, int track,
                                 int bpm);

/* Add a raw MIDI message */
void midi_file_writer_raw(midi_file_writer* writer, int tick, int track,
                           const uint8_t* msg, size_t len);

/* Write the MIDI file to disk
 * Returns 0 on success, non-zero on error */
int midi_file_writer_save(midi_file_writer* writer, const char* filename);

/* ============================================================================
 * MIDI File Reader
 * ============================================================================ */

typedef struct midi_file_reader midi_file_reader;

/* MIDI event structure for iteration */
typedef struct {
    int track;           /* Track number (0-based) */
    int tick;            /* Time in ticks */
    uint8_t type;        /* Message type (0x80=note-off, 0x90=note-on, etc.) */
    uint8_t channel;     /* MIDI channel (1-16) */
    uint8_t data1;       /* First data byte (pitch, control number, etc.) */
    uint8_t data2;       /* Second data byte (velocity, value, etc.) */
    const uint8_t* raw;  /* Pointer to raw message bytes */
    size_t raw_len;      /* Length of raw message */
} midi_file_event;

/* Callback function type for iterating events */
typedef void (*midi_file_event_callback)(void* ctx, const midi_file_event* event);

/* Create a new MIDI file reader
 * Returns 0 on success, non-zero on error */
int midi_file_reader_new(midi_file_reader** reader);

/* Free a MIDI file reader */
void midi_file_reader_free(midi_file_reader* reader);

/* Parse a MIDI file from disk
 * Returns: 0=invalid, 1=incomplete, 2=complete, 3=validated */
int midi_file_reader_load(midi_file_reader* reader, const char* filename);

/* Get the number of tracks */
int midi_file_reader_num_tracks(midi_file_reader* reader);

/* Get ticks per quarter note (PPQN) */
float midi_file_reader_ppqn(midi_file_reader* reader);

/* Get starting tempo in BPM */
float midi_file_reader_tempo(midi_file_reader* reader);

/* Get the end time in seconds */
double midi_file_reader_duration(midi_file_reader* reader);

/* Get the MIDI file format (0, 1, or 2) */
int midi_file_reader_format(midi_file_reader* reader);

/* Iterate through all events, calling the callback for each
 * Events are delivered in order by track, then by tick */
void midi_file_reader_for_each(midi_file_reader* reader, void* ctx,
                                midi_file_event_callback callback);

/* Get total number of events across all tracks */
int midi_file_reader_num_events(midi_file_reader* reader);

#ifdef __cplusplus
}
#endif

#endif /* MIDI_FILE_H */
