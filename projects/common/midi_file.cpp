/*
 * midi_file.cpp - C wrapper implementation for libremidi MIDI file I/O
 */

#include "midi_file.h"

#include <libremidi/reader.hpp>
#include <libremidi/writer.hpp>
#include <libremidi/message.hpp>

#include <fstream>
#include <vector>
#include <cstdio>

/* ============================================================================
 * Writer Implementation
 * ============================================================================ */

struct midi_file_writer {
    libremidi::writer writer;
};

int midi_file_writer_new(midi_file_writer** out) {
    if (!out) return -1;
    try {
        *out = new midi_file_writer();
        (*out)->writer.ticksPerQuarterNote = 480;
        return 0;
    } catch (...) {
        *out = nullptr;
        return -1;
    }
}

void midi_file_writer_free(midi_file_writer* w) {
    delete w;
}

void midi_file_writer_set_ppqn(midi_file_writer* w, uint16_t ppqn) {
    if (w) w->writer.ticksPerQuarterNote = ppqn;
}

void midi_file_writer_add_track(midi_file_writer* w) {
    if (w) w->writer.add_track();
}

void midi_file_writer_note_on(midi_file_writer* w, int tick, int track,
                               int channel, int pitch, int velocity) {
    if (!w) return;
    auto msg = libremidi::channel_events::note_on(
        static_cast<uint8_t>(channel),
        static_cast<uint8_t>(pitch),
        static_cast<uint8_t>(velocity));
    w->writer.add_event(tick, track, msg);
}

void midi_file_writer_note_off(midi_file_writer* w, int tick, int track,
                                int channel, int pitch, int velocity) {
    if (!w) return;
    auto msg = libremidi::channel_events::note_off(
        static_cast<uint8_t>(channel),
        static_cast<uint8_t>(pitch),
        static_cast<uint8_t>(velocity));
    w->writer.add_event(tick, track, msg);
}

void midi_file_writer_cc(midi_file_writer* w, int tick, int track,
                          int channel, int control, int value) {
    if (!w) return;
    auto msg = libremidi::channel_events::control_change(
        static_cast<uint8_t>(channel),
        static_cast<uint8_t>(control),
        static_cast<uint8_t>(value));
    w->writer.add_event(tick, track, msg);
}

void midi_file_writer_program(midi_file_writer* w, int tick, int track,
                               int channel, int program) {
    if (!w) return;
    auto msg = libremidi::channel_events::program_change(
        static_cast<uint8_t>(channel),
        static_cast<uint8_t>(program));
    w->writer.add_event(tick, track, msg);
}

void midi_file_writer_pitch_bend(midi_file_writer* w, int tick, int track,
                                  int channel, int value) {
    if (!w) return;
    auto msg = libremidi::channel_events::pitch_bend(
        static_cast<uint8_t>(channel),
        value);
    w->writer.add_event(tick, track, msg);
}

void midi_file_writer_tempo(midi_file_writer* w, int tick, int track,
                             int tempo_uspqn) {
    if (!w) return;
    auto msg = libremidi::meta_events::tempo(tempo_uspqn);
    w->writer.add_event(tick, track, msg);
}

void midi_file_writer_tempo_bpm(midi_file_writer* w, int tick, int track,
                                 int bpm) {
    if (!w || bpm <= 0) return;
    int uspqn = 60000000 / bpm;  /* microseconds per quarter note */
    midi_file_writer_tempo(w, tick, track, uspqn);
}

void midi_file_writer_raw(midi_file_writer* w, int tick, int track,
                           const uint8_t* msg, size_t len) {
    if (!w || !msg || len == 0) return;
    libremidi::message m;
    m.bytes.assign(msg, msg + len);
    w->writer.add_event(tick, track, m);
}

int midi_file_writer_save(midi_file_writer* w, const char* filename) {
    if (!w || !filename) return -1;
    try {
        std::ofstream file(filename, std::ios::binary);
        if (!file.is_open()) return -1;
        w->writer.write(file);
        return 0;
    } catch (...) {
        return -1;
    }
}

/* ============================================================================
 * Reader Implementation
 * ============================================================================ */

struct midi_file_reader {
    libremidi::reader reader;
    std::vector<uint8_t> file_data;
    int parse_result;

    midi_file_reader() : reader(true), parse_result(0) {}  /* useAbsolute=true */
};

int midi_file_reader_new(midi_file_reader** out) {
    if (!out) return -1;
    try {
        *out = new midi_file_reader();
        return 0;
    } catch (...) {
        *out = nullptr;
        return -1;
    }
}

void midi_file_reader_free(midi_file_reader* r) {
    delete r;
}

int midi_file_reader_load(midi_file_reader* r, const char* filename) {
    if (!r || !filename) return 0;

    try {
        std::ifstream file(filename, std::ios::binary);
        if (!file.is_open()) return 0;

        r->file_data.assign(
            std::istreambuf_iterator<char>(file),
            std::istreambuf_iterator<char>());

        auto result = r->reader.parse(r->file_data);
        r->parse_result = static_cast<int>(result);
        return r->parse_result;
    } catch (...) {
        return 0;
    }
}

int midi_file_reader_num_tracks(midi_file_reader* r) {
    if (!r) return 0;
    return static_cast<int>(r->reader.tracks.size());
}

float midi_file_reader_ppqn(midi_file_reader* r) {
    if (!r) return 0;
    return r->reader.ticksPerBeat;
}

float midi_file_reader_tempo(midi_file_reader* r) {
    if (!r) return 120.0f;
    /* Convert starting tempo from internal format to BPM */
    if (r->reader.startingTempo > 0) {
        return 60000000.0f / r->reader.startingTempo;
    }
    return 120.0f;
}

double midi_file_reader_duration(midi_file_reader* r) {
    if (!r) return 0;
    return r->reader.get_end_time();
}

int midi_file_reader_format(midi_file_reader* r) {
    if (!r) return 0;
    return r->reader.format;
}

void midi_file_reader_for_each(midi_file_reader* r, void* ctx,
                                midi_file_event_callback callback) {
    if (!r || !callback) return;

    int track_num = 0;
    for (const auto& track : r->reader.tracks) {
        for (const auto& event : track) {
            midi_file_event e;
            e.track = track_num;
            e.tick = event.m.timestamp;  /* Using absolute ticks */
            e.raw = event.m.bytes.data();
            e.raw_len = event.m.bytes.size();

            if (e.raw_len > 0) {
                uint8_t status = e.raw[0];
                e.type = status & 0xF0;
                e.channel = (status & 0x0F) + 1;
                e.data1 = e.raw_len > 1 ? e.raw[1] : 0;
                e.data2 = e.raw_len > 2 ? e.raw[2] : 0;

                /* Handle system messages */
                if (status >= 0xF0) {
                    e.type = status;
                    e.channel = 0;
                }
            } else {
                e.type = 0;
                e.channel = 0;
                e.data1 = 0;
                e.data2 = 0;
            }

            callback(ctx, &e);
        }
        track_num++;
    }
}

int midi_file_reader_num_events(midi_file_reader* r) {
    if (!r) return 0;
    int count = 0;
    for (const auto& track : r->reader.tracks) {
        count += static_cast<int>(track.size());
    }
    return count;
}
