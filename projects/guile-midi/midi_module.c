/*
 * MIDI module for GNU Guile
 * Provides Scheme bindings for MIDI output using libremidi
 */

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>
#include <time.h>

/* Cross-platform compatibility */
#ifdef _WIN32
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#define usleep(us) Sleep((us) / 1000)
/* Windows high-resolution timing */
static LARGE_INTEGER perf_freq;
static int perf_freq_init = 0;
static void init_perf_freq(void) {
    if (!perf_freq_init) {
        QueryPerformanceFrequency(&perf_freq);
        perf_freq_init = 1;
    }
}
#ifndef CLOCK_MONOTONIC
#define CLOCK_MONOTONIC 0
#endif
static int clock_gettime(int clk_id, struct timespec *tp) {
    (void)clk_id;
    init_perf_freq();
    LARGE_INTEGER count;
    QueryPerformanceCounter(&count);
    tp->tv_sec = (long)(count.QuadPart / perf_freq.QuadPart);
    tp->tv_nsec = (long)((count.QuadPart % perf_freq.QuadPart) * 1000000000 / perf_freq.QuadPart);
    return 0;
}
#else
#include <unistd.h>
#endif

#include <libguile.h>
#include <libremidi/libremidi-c.h>
#include "scm_prelude.h"
#include "music_theory.h"
#include "midi_file.h"
#include "scheduler.h"

#define MAX_PORTS 64
#define MAX_CAPTURE_EVENTS 4096
#define TICKS_PER_QUARTER 480
#define BASE_TEMPO 120

/* Global state */
static int global_tempo_bpm = 120;

/* Scale duration for current tempo (120 BPM = 1x, 60 BPM = 2x, 240 BPM = 0.5x) */
static int scale_duration_for_tempo(int duration_ms) {
    if (global_tempo_bpm <= 0 || global_tempo_bpm == BASE_TEMPO) {
        return duration_ms;
    }
    return (int)((double)duration_ms * BASE_TEMPO / global_tempo_bpm);
}

static libremidi_midi_observer_handle* midi_observer = NULL;
static libremidi_midi_out_port* out_ports[MAX_PORTS];
static int out_port_count = 0;

/* MidiOut foreign object type */
static SCM midi_out_type;

/* MIDI capture system */
typedef struct {
    uint32_t time_ms;
    uint8_t type;      /* 0=note-on, 1=note-off, 2=cc */
    uint8_t channel;
    uint8_t data1;
    uint8_t data2;
} CapturedEvent;

static CapturedEvent capture_buffer[MAX_CAPTURE_EVENTS];
static int capture_count = 0;
static int capture_active = 0;
static struct timespec capture_start_time;
static int capture_bpm = 120;

/* Get current time in milliseconds since capture start */
static uint32_t capture_get_time_ms(void) {
    struct timespec now;
    clock_gettime(CLOCK_MONOTONIC, &now);
    uint32_t elapsed = (now.tv_sec - capture_start_time.tv_sec) * 1000 +
                       (now.tv_nsec - capture_start_time.tv_nsec) / 1000000;
    return elapsed;
}

/* Add an event to the capture buffer */
static void capture_add_event(int type, int channel, int data1, int data2) {
    if (!capture_active) return;
    if (capture_count >= MAX_CAPTURE_EVENTS) {
        printf("Capture buffer full!\n");
        capture_active = 0;
        return;
    }
    CapturedEvent* e = &capture_buffer[capture_count++];
    e->time_ms = capture_get_time_ms();
    e->type = type;
    e->channel = channel;
    e->data1 = data1;
    e->data2 = data2;
}

/* MidiOut data structure */
typedef struct {
    libremidi_midi_out_handle* handle;
    int is_virtual;
    char *port_name;
} MidiOutData;

/* Forward declarations */
static void midi_cleanup_observer(void);
static int midi_init_observer(void);

/* ============================================================================
 * Observer and port management
 * ============================================================================ */

static void on_output_port_found(void* ctx, const libremidi_midi_out_port* port) {
    (void)ctx;
    if (out_port_count >= MAX_PORTS) return;
    libremidi_midi_out_port_clone(port, &out_ports[out_port_count]);
    out_port_count++;
}

static int midi_init_observer(void) {
    if (midi_observer != NULL) return 0;

    libremidi_observer_configuration observer_conf;
    int ret = libremidi_midi_observer_configuration_init(&observer_conf);
    if (ret != 0) return ret;

    observer_conf.track_any = true;

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) return ret;

    ret = libremidi_midi_observer_new(&observer_conf, &api_conf, &midi_observer);
    if (ret != 0) return ret;

    out_port_count = 0;
    ret = libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);
    return ret;
}

static void midi_cleanup_observer(void) {
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;

    if (midi_observer != NULL) {
        libremidi_midi_observer_free(midi_observer);
        midi_observer = NULL;
    }
}

/* ============================================================================
 * Pitch parsing - uses common music_theory library
 * ============================================================================ */

/* Wrapper for backward compatibility */
static int parse_pitch(const char* name) {
    return music_parse_pitch(name);
}

/* Get pitch from Guile value (integer, string, or symbol) */
static int get_pitch(SCM arg) {
    if (scm_is_integer(arg)) {
        return scm_to_int(arg);
    } else if (scm_is_string(arg)) {
        char *str = scm_to_utf8_string(arg);
        int pitch = parse_pitch(str);
        free(str);
        return pitch;
    } else if (scm_is_symbol(arg)) {
        SCM str = scm_symbol_to_string(arg);
        char *cstr = scm_to_utf8_string(str);
        int pitch = parse_pitch(cstr);
        free(cstr);
        return pitch;
    }
    return -1;
}

/* ============================================================================
 * MidiOut type functions
 * ============================================================================ */

static void finalize_midi_out(SCM obj) {
    MidiOutData *data = scm_foreign_object_ref(obj, 0);
    if (data) {
        if (data->handle) {
            /* Send all notes off before closing */
            for (int ch = 0; ch < 16; ch++) {
                uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
                libremidi_midi_out_send_message(data->handle, msg, 3);
            }
            libremidi_midi_out_free(data->handle);
        }
        if (data->port_name) free(data->port_name);
        free(data);
    }
}

static void init_midi_out_type(void) {
    SCM name = scm_from_utf8_symbol("midi-out");
    SCM slots = scm_list_1(scm_from_utf8_symbol("data"));
    midi_out_type = scm_make_foreign_object_type(name, slots, finalize_midi_out);
}

static SCM make_midi_out(MidiOutData *data) {
    return scm_make_foreign_object_1(midi_out_type, data);
}

static MidiOutData* get_midi_out_data(SCM obj) {
    scm_assert_foreign_object_type(midi_out_type, obj);
    return scm_foreign_object_ref(obj, 0);
}

/* ============================================================================
 * Module functions
 * ============================================================================ */

/* (midi-list-ports) -> list of (index name) */
static SCM g_midi_list_ports(void) {
    if (midi_init_observer() != 0) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-list-ports", "Failed to initialize MIDI observer",
                  SCM_EOL, SCM_EOL);
    }

    /* Re-enumerate ports */
    for (int i = 0; i < out_port_count; i++) {
        libremidi_midi_out_port_free(out_ports[i]);
    }
    out_port_count = 0;
    libremidi_midi_observer_enumerate_output_ports(midi_observer, NULL, on_output_port_found);

    /* Build list */
    SCM result = SCM_EOL;
    for (int i = out_port_count - 1; i >= 0; i--) {
        const char* name = NULL;
        size_t len = 0;
        if (libremidi_midi_out_port_name(out_ports[i], &name, &len) == 0) {
            SCM entry = scm_list_2(scm_from_int(i), scm_from_utf8_string(name));
            result = scm_cons(entry, result);
        }
    }
    return result;
}

/* (midi-open) or (midi-open "name") or (midi-open index) */
static SCM g_midi_open(SCM rest) {
    if (midi_init_observer() != 0) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-open", "Failed to initialize MIDI observer",
                  SCM_EOL, SCM_EOL);
    }

    libremidi_midi_configuration midi_conf;
    int ret = libremidi_midi_configuration_init(&midi_conf);
    if (ret != 0) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-open", "Failed to init MIDI config",
                  SCM_EOL, SCM_EOL);
    }

    libremidi_api_configuration api_conf;
    ret = libremidi_midi_api_configuration_init(&api_conf);
    if (ret != 0) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-open", "Failed to init API config",
                  SCM_EOL, SCM_EOL);
    }

    int is_virtual = 0;
    const char *port_name = "guileMIDI";

    if (scm_is_null(rest)) {
        /* (midi-open) -> virtual port with default name */
        midi_conf.virtual_port = true;
        midi_conf.port_name = port_name;
        is_virtual = 1;
    } else {
        SCM arg = scm_car(rest);
        if (scm_is_integer(arg)) {
            /* (midi-open index) -> hardware port */
            int port_idx = scm_to_int(arg);
            if (port_idx < 0 || port_idx >= out_port_count) {
                scm_error(scm_from_utf8_symbol("out-of-range"),
                          "midi-open", "Invalid port index",
                          SCM_EOL, SCM_EOL);
            }
            midi_conf.out_port = out_ports[port_idx];
        } else if (scm_is_string(arg)) {
            /* (midi-open "name") -> virtual port with name */
            char *name_str = scm_to_utf8_string(arg);
            port_name = name_str;
            midi_conf.virtual_port = true;
            midi_conf.port_name = port_name;
            is_virtual = 1;
            /* Note: port_name is used temporarily, will be strdup'd below */
        } else {
            scm_error(scm_from_utf8_symbol("wrong-type-arg"),
                      "midi-open", "expected integer or string",
                      SCM_EOL, SCM_EOL);
        }
    }

    libremidi_midi_out_handle* handle;
    ret = libremidi_midi_out_new(&midi_conf, &api_conf, &handle);
    if (ret != 0) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-open", "Failed to open MIDI output",
                  SCM_EOL, SCM_EOL);
    }

    MidiOutData *data = (MidiOutData *)malloc(sizeof(MidiOutData));
    data->handle = handle;
    data->is_virtual = is_virtual;
    data->port_name = is_virtual ? strdup(port_name) : NULL;

    return make_midi_out(data);
}

/* (note "C4") or (note 'c4) -> MIDI number */
static SCM g_note(SCM arg) {
    int pitch = get_pitch(arg);
    if (pitch < 0) {
        scm_error(scm_from_utf8_symbol("invalid-note"),
                  "note", "Invalid note name",
                  SCM_EOL, SCM_EOL);
    }
    return scm_from_int(pitch);
}

/* ============================================================================
 * MidiOut methods
 * ============================================================================ */

/* (midi-close m) */
static SCM g_midi_close(SCM obj) {
    MidiOutData *data = get_midi_out_data(obj);
    if (data->handle) {
        /* Send all notes off */
        for (int ch = 0; ch < 16; ch++) {
            uint8_t msg[3] = { 0xB0 | ch, 123, 0 };
            libremidi_midi_out_send_message(data->handle, msg, 3);
        }
        libremidi_midi_out_free(data->handle);
        data->handle = NULL;
    }
    return SCM_UNSPECIFIED;
}

/* (midi-open? m) */
static SCM g_midi_open_p(SCM obj) {
    if (!SCM_IS_A_P(obj, midi_out_type)) {
        return SCM_BOOL_F;
    }
    MidiOutData *data = get_midi_out_data(obj);
    return scm_from_bool(data->handle != NULL);
}

/* (midi-out? x) */
static SCM g_midi_out_p(SCM obj) {
    return scm_from_bool(SCM_IS_A_P(obj, midi_out_type));
}

/* (midi-note-on m pitch [velocity] [channel]) */
static SCM g_midi_note_on(SCM obj, SCM pitch_arg, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-note-on", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    int pitch = get_pitch(pitch_arg);
    if (pitch < 0 || pitch > 127) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "midi-note-on", "pitch must be 0-127",
                  SCM_EOL, SCM_EOL);
    }

    int velocity = 80;
    int channel = 1;

    if (!scm_is_null(rest)) {
        velocity = scm_to_int(scm_car(rest));
        rest = scm_cdr(rest);
        if (!scm_is_null(rest)) {
            channel = scm_to_int(scm_car(rest));
        }
    }

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(0, channel - 1, pitch, velocity);

    return SCM_UNSPECIFIED;
}

/* (midi-note-off m pitch [velocity] [channel]) */
static SCM g_midi_note_off(SCM obj, SCM pitch_arg, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-note-off", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    int pitch = get_pitch(pitch_arg);
    if (pitch < 0 || pitch > 127) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "midi-note-off", "pitch must be 0-127",
                  SCM_EOL, SCM_EOL);
    }

    int velocity = 0;
    int channel = 1;

    if (!scm_is_null(rest)) {
        velocity = scm_to_int(scm_car(rest));
        rest = scm_cdr(rest);
        if (!scm_is_null(rest)) {
            channel = scm_to_int(scm_car(rest));
        }
    }

    if (velocity < 0 || velocity > 127) velocity = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0x80 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(1, channel - 1, pitch, 0);

    return SCM_UNSPECIFIED;
}

/* (midi-note m pitch [velocity] [duration] [channel]) - high level */
static SCM g_midi_note(SCM obj, SCM pitch_arg, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-note", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    int pitch = get_pitch(pitch_arg);
    if (pitch < 0 || pitch > 127) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "midi-note", "pitch must be 0-127",
                  SCM_EOL, SCM_EOL);
    }

    int velocity = 80;
    int duration = 500;
    int channel = 1;

    if (!scm_is_null(rest)) {
        velocity = scm_to_int(scm_car(rest));
        rest = scm_cdr(rest);
        if (!scm_is_null(rest)) {
            duration = scm_to_int(scm_car(rest));
            rest = scm_cdr(rest);
            if (!scm_is_null(rest)) {
                channel = scm_to_int(scm_car(rest));
            }
        }
    }

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (duration < 0) duration = 500;
    if (channel < 1 || channel > 16) channel = 1;

    /* Note on */
    uint8_t msg[3] = {
        0x90 | ((channel - 1) & 0x0F),
        pitch & 0x7F,
        velocity & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(0, channel - 1, pitch, velocity);

    /* Wait (scaled for tempo) */
    int scaled_dur = scale_duration_for_tempo(duration);
    if (scaled_dur > 0) {
        usleep(scaled_dur * 1000);
    }

    /* Note off */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(1, channel - 1, pitch, 0);

    return SCM_UNSPECIFIED;
}

/* (midi-chord m pitches [velocity] [duration] [channel]) */
static SCM g_midi_chord(SCM obj, SCM pitches_list, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-chord", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    if (!scm_is_pair(pitches_list) && !scm_is_null(pitches_list)) {
        scm_error(scm_from_utf8_symbol("wrong-type-arg"),
                  "midi-chord", "expected list for pitches",
                  SCM_EOL, SCM_EOL);
    }

    int velocity = 80;
    int duration = 500;
    int channel = 1;

    if (!scm_is_null(rest)) {
        velocity = scm_to_int(scm_car(rest));
        rest = scm_cdr(rest);
        if (!scm_is_null(rest)) {
            duration = scm_to_int(scm_car(rest));
            rest = scm_cdr(rest);
            if (!scm_is_null(rest)) {
                channel = scm_to_int(scm_car(rest));
            }
        }
    }

    if (velocity < 0 || velocity > 127) velocity = 80;
    if (duration < 0) duration = 500;
    if (channel < 1 || channel > 16) channel = 1;

    /* Collect pitches */
    int pitches[128];
    int count = 0;
    SCM p = pitches_list;
    while (scm_is_pair(p) && count < 128) {
        int pitch = get_pitch(scm_car(p));
        if (pitch >= 0 && pitch <= 127) {
            pitches[count++] = pitch;
        }
        p = scm_cdr(p);
    }

    if (count == 0) {
        return SCM_UNSPECIFIED;
    }

    /* Send note-ons */
    uint8_t msg[3];
    msg[0] = 0x90 | ((channel - 1) & 0x0F);
    msg[2] = velocity & 0x7F;
    for (int i = 0; i < count; i++) {
        msg[1] = pitches[i] & 0x7F;
        libremidi_midi_out_send_message(data->handle, msg, 3);
        capture_add_event(0, channel - 1, pitches[i], velocity);
    }

    /* Wait (scaled for tempo) */
    int scaled_dur = scale_duration_for_tempo(duration);
    if (scaled_dur > 0) {
        usleep(scaled_dur * 1000);
    }

    /* Send note-offs */
    msg[0] = 0x80 | ((channel - 1) & 0x0F);
    msg[2] = 0;
    for (int i = 0; i < count; i++) {
        msg[1] = pitches[i] & 0x7F;
        libremidi_midi_out_send_message(data->handle, msg, 3);
        capture_add_event(1, channel - 1, pitches[i], 0);
    }

    return SCM_UNSPECIFIED;
}

/* (midi-cc m control value [channel]) */
static SCM g_midi_cc(SCM obj, SCM control_arg, SCM value_arg, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-cc", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    int control = scm_to_int(control_arg);
    int value = scm_to_int(value_arg);
    int channel = 1;

    if (!scm_is_null(rest)) {
        channel = scm_to_int(scm_car(rest));
    }

    if (control < 0 || control > 127) control = 0;
    if (value < 0 || value > 127) value = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[3] = {
        0xB0 | ((channel - 1) & 0x0F),
        control & 0x7F,
        value & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);
    capture_add_event(2, channel - 1, control, value);

    return SCM_UNSPECIFIED;
}

/* (midi-program m program [channel]) */
static SCM g_midi_program(SCM obj, SCM program_arg, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-program", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    int program = scm_to_int(program_arg);
    int channel = 1;

    if (!scm_is_null(rest)) {
        channel = scm_to_int(scm_car(rest));
    }

    if (program < 0 || program > 127) program = 0;
    if (channel < 1 || channel > 16) channel = 1;

    uint8_t msg[2] = {
        0xC0 | ((channel - 1) & 0x0F),
        program & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 2);

    return SCM_UNSPECIFIED;
}

/* (midi-all-notes-off m [channel]) */
static SCM g_midi_all_notes_off(SCM obj, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-all-notes-off", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    int start_ch = 1, end_ch = 16;
    if (!scm_is_null(rest)) {
        int ch = scm_to_int(scm_car(rest));
        if (ch >= 1 && ch <= 16) {
            start_ch = end_ch = ch;
        }
    }

    for (int ch = start_ch; ch <= end_ch; ch++) {
        uint8_t msg[3] = {
            0xB0 | ((ch - 1) & 0x0F),
            123,
            0
        };
        libremidi_midi_out_send_message(data->handle, msg, 3);
    }

    return SCM_UNSPECIFIED;
}

/* (midi-sleep ms) */
static SCM g_midi_sleep(SCM ms_arg) {
    int ms = scm_to_int(ms_arg);
    int scaled_ms = scale_duration_for_tempo(ms);
    if (scaled_ms > 0) {
        usleep(scaled_ms * 1000);
    }
    return SCM_UNSPECIFIED;
}

/* (set-tempo-c! bpm) - Set C layer tempo for duration scaling */
static SCM g_set_tempo_c(SCM bpm_arg) {
    int bpm = scm_to_int(bpm_arg);
    if (bpm > 0) {
        global_tempo_bpm = bpm;
    }
    return SCM_UNSPECIFIED;
}

/* (get-tempo-c) - Get C layer tempo */
static SCM g_get_tempo_c(void) {
    return scm_from_int(global_tempo_bpm);
}

/* ============================================================================
 * Scale functions - using common music_theory library
 * ============================================================================ */

/* Helper to read interval list from Guile list */
static int get_intervals_from_list(SCM lst, int* intervals, int max_size) {
    int count = 0;
    SCM p = lst;
    while (scm_is_pair(p) && count < max_size) {
        if (scm_is_integer(scm_car(p))) {
            intervals[count++] = scm_to_int(scm_car(p));
        }
        p = scm_cdr(p);
    }
    return count;
}

/* (build-scale root intervals) -> list of pitches */
static SCM g_build_scale(SCM root_arg, SCM intervals_list) {
    int root = get_pitch(root_arg);
    if (root < 0 || root > 127) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "build-scale", "Invalid root pitch",
                  SCM_EOL, SCM_EOL);
    }

    if (!scm_is_pair(intervals_list) && !scm_is_null(intervals_list)) {
        scm_error(scm_from_utf8_symbol("wrong-type-arg"),
                  "build-scale", "expected list for intervals",
                  SCM_EOL, SCM_EOL);
    }

    int intervals[16];
    int num_intervals = get_intervals_from_list(intervals_list, intervals, 16);
    if (num_intervals == 0) {
        return SCM_EOL;
    }

    int pitches[16];
    int count = music_build_scale(root, intervals, num_intervals, pitches);

    /* Build result list */
    SCM result = SCM_EOL;
    for (int i = count - 1; i >= 0; i--) {
        result = scm_cons(scm_from_int(pitches[i]), result);
    }
    return result;
}

/* (scale-degree root intervals degree) -> pitch */
static SCM g_scale_degree(SCM root_arg, SCM intervals_list, SCM degree_arg) {
    int root = get_pitch(root_arg);
    if (root < 0 || root > 127) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "scale-degree", "Invalid root pitch",
                  SCM_EOL, SCM_EOL);
    }

    int intervals[16];
    int num_intervals = get_intervals_from_list(intervals_list, intervals, 16);
    if (num_intervals == 0) {
        scm_error(scm_from_utf8_symbol("invalid-scale"),
                  "scale-degree", "Empty intervals list",
                  SCM_EOL, SCM_EOL);
    }

    int degree = scm_to_int(degree_arg);
    int pitch = music_scale_degree(root, intervals, num_intervals, degree);
    if (pitch < 0) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "scale-degree", "Scale degree out of range",
                  SCM_EOL, SCM_EOL);
    }
    return scm_from_int(pitch);
}

/* (in-scale? pitch root intervals) -> boolean */
static SCM g_in_scale(SCM pitch_arg, SCM root_arg, SCM intervals_list) {
    int pitch = get_pitch(pitch_arg);
    if (pitch < 0 || pitch > 127) {
        return SCM_BOOL_F;
    }

    int root = get_pitch(root_arg);
    if (root < 0 || root > 127) {
        return SCM_BOOL_F;
    }

    int intervals[16];
    int num_intervals = get_intervals_from_list(intervals_list, intervals, 16);
    if (num_intervals == 0) {
        return SCM_BOOL_F;
    }

    int result = music_in_scale(pitch, root, intervals, num_intervals);
    return scm_from_bool(result);
}

/* (quantize-to-scale pitch root intervals) -> pitch */
static SCM g_quantize_to_scale(SCM pitch_arg, SCM root_arg, SCM intervals_list) {
    int pitch = get_pitch(pitch_arg);
    if (pitch < 0 || pitch > 127) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "quantize-to-scale", "Invalid pitch",
                  SCM_EOL, SCM_EOL);
    }

    int root = get_pitch(root_arg);
    if (root < 0 || root > 127) {
        scm_error(scm_from_utf8_symbol("out-of-range"),
                  "quantize-to-scale", "Invalid root pitch",
                  SCM_EOL, SCM_EOL);
    }

    int intervals[16];
    int num_intervals = get_intervals_from_list(intervals_list, intervals, 16);
    if (num_intervals == 0) {
        return scm_from_int(pitch);
    }

    int result = music_quantize_to_scale(pitch, root, intervals, num_intervals);
    return scm_from_int(result);
}

/* (midi-pitch-bend m cents [channel]) - send pitch bend for microtonal */
static SCM g_midi_pitch_bend(SCM obj, SCM cents_arg, SCM rest) {
    MidiOutData *data = get_midi_out_data(obj);
    if (!data->handle) {
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "midi-pitch-bend", "MIDI output is closed",
                  SCM_EOL, SCM_EOL);
    }

    int cents = scm_to_int(cents_arg);
    int channel = 1;

    if (!scm_is_null(rest)) {
        channel = scm_to_int(scm_car(rest));
    }

    if (channel < 1 || channel > 16) channel = 1;

    int bend = music_cents_to_bend(cents);
    uint8_t msg[3] = {
        0xE0 | ((channel - 1) & 0x0F),
        bend & 0x7F,
        (bend >> 7) & 0x7F
    };
    libremidi_midi_out_send_message(data->handle, msg, 3);

    return SCM_UNSPECIFIED;
}

/* (help) */
static SCM g_help(void) {
    const char *help_text =
        "guile_midi - Scheme MIDI language (GNU Guile)\n"
        "\n"
        "Port management:\n"
        "  (midi-list-ports)           List available MIDI ports\n"
        "  (midi-open)                 Open virtual port \"guileMIDI\"\n"
        "  (midi-open \"name\")          Open virtual port with name\n"
        "  (midi-open index)           Open hardware port by index\n"
        "  (midi-close m)              Close MIDI port\n"
        "  (midi-open? m)              Check if port is open\n"
        "  (midi-out? x)               Check if x is a midi-out\n"
        "\n"
        "Note playing:\n"
        "  (midi-note m pitch [vel] [dur] [ch])     Play note\n"
        "  (midi-chord m pitches [vel] [dur] [ch])  Play chord\n"
        "  (midi-note-on m pitch [vel] [ch])        Note on\n"
        "  (midi-note-off m pitch [vel] [ch])       Note off\n"
        "\n"
        "Control:\n"
        "  (midi-cc m ctrl val [ch])   Control change\n"
        "  (midi-program m prog [ch])  Program change\n"
        "  (midi-all-notes-off m [ch]) All notes off\n"
        "\n"
        "Recording and File I/O:\n"
        "  (record-midi [bpm])         Start recording\n"
        "  (record-stop)               Stop recording\n"
        "  (save-midi filename)        Save as Scheme file\n"
        "  (write-mid filename [ppqn]) Save as standard MIDI file\n"
        "  (read-mid filename)         Read MIDI file\n"
        "\n"
        "Utilities:\n"
        "  (note \"C4\") or (note 'c4)  Parse note name to MIDI number\n"
        "  (midi-sleep ms)             Sleep for milliseconds\n"
        "\n"
        "Pitch constants: c0-c8, cs0-cs8 (sharps), etc.\n"
        "Dynamics: ppp pp p mp mf f ff fff\n"
        "Durations: whole half quarter eighth sixteenth\n"
        "\n"
        "Chord builders:\n"
        "  (major root) (minor root) (dim root) (aug root)\n"
        "  (dom7 root) (maj7 root) (min7 root)\n"
        "\n"
        "Scale functions:\n"
        "  (build-scale root intervals)      Build scale pitches\n"
        "  (scale-degree root intervals n)   Get nth scale degree\n"
        "  (in-scale? pitch root intervals)  Check if pitch in scale\n"
        "  (quantize-to-scale pitch root intervals) Snap to scale\n"
        "  (midi-pitch-bend m cents [ch])    Pitch bend (microtonal)\n"
        "\n"
        "Scale helpers (via prelude):\n"
        "  (scale root name)        Build scale, e.g. (scale c4 'major)\n"
        "  (degree root name n)     Get scale degree\n"
        "  scale-major, scale-minor, scale-dorian, scale-blues, etc.\n"
        "\n"
        "Example:\n"
        "  (define m (midi-open))\n"
        "  (midi-note m c4 mf quarter)\n"
        "  (midi-chord m (major c4) mf half)\n"
        "  (midi-close m)\n";

    printf("%s", help_text);
    return SCM_UNSPECIFIED;
}

/* ============================================================================
 * MIDI Recording functions
 * ============================================================================ */

/* (record-midi [bpm]) - Start recording MIDI events */
static SCM g_record_midi(SCM rest) {
    if (capture_active) {
        printf("Already recording (use (record-stop) first)\n");
        return SCM_UNSPECIFIED;
    }
    capture_bpm = 120;
    if (!scm_is_null(rest)) {
        capture_bpm = scm_to_int(scm_car(rest));
    }
    capture_count = 0;
    capture_active = 1;
    clock_gettime(CLOCK_MONOTONIC, &capture_start_time);
    printf("MIDI recording started at %d BPM. Play notes, then (record-stop) and (save-midi filename).\n", capture_bpm);
    return SCM_UNSPECIFIED;
}

/* (record-stop) - Stop recording MIDI events */
static SCM g_record_stop(void) {
    if (!capture_active) {
        printf("Not recording\n");
        return SCM_UNSPECIFIED;
    }
    capture_active = 0;
    printf("MIDI recording stopped. %d events recorded. Use (save-midi filename) to save.\n", capture_count);
    return SCM_UNSPECIFIED;
}

/* (save-midi filename) - Save recorded MIDI to a Scheme file */
static SCM g_save_midi(SCM filename_arg) {
    char *filename = scm_to_utf8_string(filename_arg);

    if (capture_count == 0) {
        printf("Nothing to save (recording is empty)\n");
        free(filename);
        return SCM_UNSPECIFIED;
    }

    FILE* f = fopen(filename, "w");
    if (f == NULL) {
        free(filename);
        scm_error(scm_from_utf8_symbol("file-error"),
                  "save-midi", "Cannot create file",
                  SCM_EOL, SCM_EOL);
    }

    /* Write header */
    fprintf(f, ";; MIDI recording\n");
    fprintf(f, ";; %d events recorded at %d BPM\n\n", capture_count, capture_bpm);
    fprintf(f, "(define m (midi-open))\n\n");
    fprintf(f, ";; Notes: (start-ms pitch velocity duration-ms channel)\n");
    fprintf(f, "(define notes '(\n");

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

    for (int i = 0; i < capture_count; i++) {
        CapturedEvent* e = &capture_buffer[i];

        if (e->type == 0) {  /* Note-on */
            note_on_time[e->channel][e->data1] = e->time_ms;
            note_on_vel[e->channel][e->data1] = e->data2;
        } else if (e->type == 1) {  /* Note-off */
            int start_ms = note_on_time[e->channel][e->data1];
            if (start_ms >= 0) {
                int dur_ms = e->time_ms - start_ms;
                if (dur_ms < 1) dur_ms = 1;
                int vel = note_on_vel[e->channel][e->data1];

                fprintf(f, "  (%d %d %d %d %d)\n",
                        start_ms, e->data1, vel, dur_ms, e->channel + 1);
                notes_written++;

                note_on_time[e->channel][e->data1] = -1;
            }
        }
    }

    fprintf(f, "))\n\n");

    /* Write playback code */
    fprintf(f, ";; Playback with original timing (uses delta delays between notes)\n");
    fprintf(f, "(define (play-timed)\n");
    fprintf(f, "  (let loop ((ns notes) (last-time 0))\n");
    fprintf(f, "    (if (not (null? ns))\n");
    fprintf(f, "        (let* ((n (car ns))\n");
    fprintf(f, "               (start-ms (car n))\n");
    fprintf(f, "               (pitch (cadr n))\n");
    fprintf(f, "               (vel (caddr n))\n");
    fprintf(f, "               (dur (cadddr n))\n");
    fprintf(f, "               (ch (car (cddddr n)))\n");
    fprintf(f, "               (delay (- start-ms last-time)))\n");
    fprintf(f, "          (if (> delay 0) (midi-sleep delay))\n");
    fprintf(f, "          (midi-note-on m pitch vel ch)\n");
    fprintf(f, "          (midi-sleep dur)\n");
    fprintf(f, "          (midi-note-off m pitch ch)\n");
    fprintf(f, "          (loop (cdr ns) (+ start-ms dur))))))\n\n");

    fprintf(f, ";; Run: (play-timed)\n");
    fprintf(f, ";; Close when done: (midi-close m)\n");
    fprintf(f, ";; %d notes written\n", notes_written);

    fclose(f);
    printf("Saved %d notes to '%s'\n", notes_written, filename);
    free(filename);
    return SCM_UNSPECIFIED;
}

/* (record-status) - Get recording status */
static SCM g_record_status(void) {
    return scm_list_3(
        scm_cons(scm_from_utf8_symbol("active"), scm_from_bool(capture_active)),
        scm_cons(scm_from_utf8_symbol("events"), scm_from_int(capture_count)),
        scm_cons(scm_from_utf8_symbol("bpm"), scm_from_int(capture_bpm)));
}

/* ============================================================================
 * MIDI File I/O (using libremidi reader/writer)
 * ============================================================================ */

/* (write-mid filename [ppqn]) - Write captured events to standard MIDI file */
static SCM g_write_mid(SCM filename_arg, SCM rest) {
    char *filename = scm_to_utf8_string(filename_arg);

    int ppqn = TICKS_PER_QUARTER;
    if (!scm_is_null(rest)) {
        ppqn = scm_to_int(scm_car(rest));
    }

    if (capture_count == 0) {
        printf("Nothing to save (no events recorded)\n");
        free(filename);
        return SCM_UNSPECIFIED;
    }

    midi_file_writer* writer = NULL;
    if (midi_file_writer_new(&writer) != 0) {
        free(filename);
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "write-mid", "Failed to create MIDI file writer",
                  SCM_EOL, SCM_EOL);
    }

    midi_file_writer_set_ppqn(writer, (uint16_t)ppqn);
    midi_file_writer_add_track(writer);

    /* Add tempo event at the beginning */
    midi_file_writer_tempo_bpm(writer, 0, 0, capture_bpm);

    /* Convert milliseconds to ticks: tick = ms * ppqn * bpm / 60000 */
    double ms_to_tick = (double)ppqn * capture_bpm / 60000.0;

    for (int i = 0; i < capture_count; i++) {
        CapturedEvent* e = &capture_buffer[i];
        int tick = (int)(e->time_ms * ms_to_tick);
        int channel = e->channel + 1;  /* 1-based for midi_file API */

        switch (e->type) {
            case 0:  /* note-on */
                midi_file_writer_note_on(writer, tick, 0, channel, e->data1, e->data2);
                break;
            case 1:  /* note-off */
                midi_file_writer_note_off(writer, tick, 0, channel, e->data1, 0);
                break;
            case 2:  /* cc */
                midi_file_writer_cc(writer, tick, 0, channel, e->data1, e->data2);
                break;
            default:
                break;
        }
    }

    int result = midi_file_writer_save(writer, filename);
    midi_file_writer_free(writer);

    if (result != 0) {
        free(filename);
        scm_error(scm_from_utf8_symbol("file-error"),
                  "write-mid", "Failed to write MIDI file",
                  SCM_EOL, SCM_EOL);
    }

    printf("Saved %d events to '%s'\n", capture_count, filename);
    free(filename);
    return SCM_UNSPECIFIED;
}

/* Callback context for reading MIDI file */
typedef struct {
    SCM events_list;
} ReadMidContext;

static void read_mid_callback(void* ctx, const midi_file_event* event) {
    ReadMidContext* rctx = (ReadMidContext*)ctx;

    /* Skip meta events and system messages */
    if (event->type < 0x80 || event->type >= 0xF0) return;

    /* Decode message type */
    const char* type_name = "unknown";
    switch (event->type) {
        case 0x80: type_name = "note-off"; break;
        case 0x90: type_name = (event->data2 > 0) ? "note-on" : "note-off"; break;
        case 0xA0: type_name = "poly-pressure"; break;
        case 0xB0: type_name = "cc"; break;
        case 0xC0: type_name = "program"; break;
        case 0xD0: type_name = "aftertouch"; break;
        case 0xE0: type_name = "pitch-bend"; break;
        default: break;
    }

    /* Create event as alist */
    SCM event_list = SCM_EOL;

    /* Add fields in reverse order since cons prepends */
    if (event->type == 0x80 || event->type == 0x90) {
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("velocity"),
                                       scm_from_int(event->data2)), event_list);
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("pitch"),
                                       scm_from_int(event->data1)), event_list);
    } else if (event->type == 0xB0) {
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("value"),
                                       scm_from_int(event->data2)), event_list);
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("control"),
                                       scm_from_int(event->data1)), event_list);
    } else if (event->type == 0xC0) {
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("program"),
                                       scm_from_int(event->data1)), event_list);
    } else if (event->type == 0xE0) {
        int bend = event->data1 | (event->data2 << 7);
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("value"),
                                       scm_from_int(bend)), event_list);
    } else {
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("data2"),
                                       scm_from_int(event->data2)), event_list);
        event_list = scm_cons(scm_cons(scm_from_utf8_symbol("data1"),
                                       scm_from_int(event->data1)), event_list);
    }

    event_list = scm_cons(scm_cons(scm_from_utf8_symbol("type"),
                                   scm_from_utf8_symbol(type_name)), event_list);
    event_list = scm_cons(scm_cons(scm_from_utf8_symbol("channel"),
                                   scm_from_int(event->channel)), event_list);
    event_list = scm_cons(scm_cons(scm_from_utf8_symbol("tick"),
                                   scm_from_int(event->tick)), event_list);
    event_list = scm_cons(scm_cons(scm_from_utf8_symbol("track"),
                                   scm_from_int(event->track)), event_list);

    /* Add event to list */
    rctx->events_list = scm_cons(event_list, rctx->events_list);
}

/* (read-mid filename) - Read MIDI file and return alist with events */
static SCM g_read_mid(SCM filename_arg) {
    char *filename = scm_to_utf8_string(filename_arg);

    midi_file_reader* reader = NULL;
    if (midi_file_reader_new(&reader) != 0) {
        free(filename);
        scm_error(scm_from_utf8_symbol("midi-error"),
                  "read-mid", "Failed to create MIDI file reader",
                  SCM_EOL, SCM_EOL);
    }

    int result = midi_file_reader_load(reader, filename);
    if (result == 0) {
        midi_file_reader_free(reader);
        free(filename);
        scm_error(scm_from_utf8_symbol("file-error"),
                  "read-mid", "Failed to parse MIDI file",
                  SCM_EOL, SCM_EOL);
    }

    /* Gather events */
    ReadMidContext ctx = { SCM_EOL };
    midi_file_reader_for_each(reader, &ctx, read_mid_callback);

    /* Reverse list to get chronological order */
    SCM events = scm_reverse(ctx.events_list);

    /* Build result alist */
    SCM result_list = SCM_EOL;
    result_list = scm_cons(scm_cons(scm_from_utf8_symbol("events"), events), result_list);
    result_list = scm_cons(scm_cons(scm_from_utf8_symbol("format"),
                                    scm_from_int(midi_file_reader_format(reader))), result_list);
    result_list = scm_cons(scm_cons(scm_from_utf8_symbol("duration"),
                                    scm_from_double(midi_file_reader_duration(reader))), result_list);
    result_list = scm_cons(scm_cons(scm_from_utf8_symbol("tempo"),
                                    scm_from_double(midi_file_reader_tempo(reader))), result_list);
    result_list = scm_cons(scm_cons(scm_from_utf8_symbol("ppqn"),
                                    scm_from_double(midi_file_reader_ppqn(reader))), result_list);
    result_list = scm_cons(scm_cons(scm_from_utf8_symbol("num-tracks"),
                                    scm_from_int(midi_file_reader_num_tracks(reader))), result_list);

    midi_file_reader_free(reader);
    free(filename);
    return result_list;
}

/* ============================================================================
 * Module initialization
 * ============================================================================ */

void guile_midi_init(void) {
    /* Create midi-out type */
    init_midi_out_type();

    /* Module functions */
    scm_c_define_gsubr("midi-list-ports", 0, 0, 0, g_midi_list_ports);
    scm_c_define_gsubr("midi-open", 0, 0, 1, g_midi_open);
    scm_c_define_gsubr("midi-close", 1, 0, 0, g_midi_close);
    scm_c_define_gsubr("midi-open?", 1, 0, 0, g_midi_open_p);
    scm_c_define_gsubr("midi-out?", 1, 0, 0, g_midi_out_p);
    scm_c_define_gsubr("note", 1, 0, 0, g_note);

    /* MidiOut methods */
    scm_c_define_gsubr("midi-note-on", 2, 0, 1, g_midi_note_on);
    scm_c_define_gsubr("midi-note-off", 2, 0, 1, g_midi_note_off);
    scm_c_define_gsubr("midi-note", 2, 0, 1, g_midi_note);
    scm_c_define_gsubr("midi-chord", 2, 0, 1, g_midi_chord);
    scm_c_define_gsubr("midi-cc", 3, 0, 1, g_midi_cc);
    scm_c_define_gsubr("midi-program", 2, 0, 1, g_midi_program);
    scm_c_define_gsubr("midi-all-notes-off", 1, 0, 1, g_midi_all_notes_off);
    scm_c_define_gsubr("midi-sleep", 1, 0, 0, g_midi_sleep);
    scm_c_define_gsubr("set-tempo-c!", 1, 0, 0, g_set_tempo_c);
    scm_c_define_gsubr("get-tempo-c", 0, 0, 0, g_get_tempo_c);

    /* Scale functions */
    scm_c_define_gsubr("build-scale", 2, 0, 0, g_build_scale);
    scm_c_define_gsubr("scale-degree", 3, 0, 0, g_scale_degree);
    scm_c_define_gsubr("in-scale?", 3, 0, 0, g_in_scale);
    scm_c_define_gsubr("quantize-to-scale", 3, 0, 0, g_quantize_to_scale);
    scm_c_define_gsubr("midi-pitch-bend", 2, 0, 1, g_midi_pitch_bend);

    scm_c_define_gsubr("help", 0, 0, 0, g_help);

    /* Recording functions */
    scm_c_define_gsubr("record-midi", 0, 0, 1, g_record_midi);
    scm_c_define_gsubr("record-stop", 0, 0, 0, g_record_stop);
    scm_c_define_gsubr("save-midi", 1, 0, 0, g_save_midi);
    scm_c_define_gsubr("record-status", 0, 0, 0, g_record_status);

    /* MIDI File I/O functions */
    scm_c_define_gsubr("write-mid", 1, 0, 1, g_write_mid);
    scm_c_define_gsubr("read-mid", 1, 0, 0, g_read_mid);

    /* Register scheduler functions (spawn, run, stop, voices, scheduler-status) */
    guile_scheduler_register();

    /* Load Scheme prelude */
    scm_c_eval_string(SCHEME_PRELUDE_MODULE);
}

void guile_midi_cleanup(void) {
    guile_scheduler_cleanup();
    midi_cleanup_observer();
}
