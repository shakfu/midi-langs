/**
 * @file midi_backend.h
 * @brief MIDI I/O backend using libremidi.
 */

#ifndef ALDA_MIDI_BACKEND_H
#define ALDA_MIDI_BACKEND_H

#include "context.h"

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Initialization and Cleanup
 * ============================================================================ */

/**
 * @brief Initialize the MIDI observer and enumerate ports.
 * @param ctx Alda context.
 */
void alda_midi_init_observer(AldaContext* ctx);

/**
 * @brief Cleanup MIDI resources.
 * @param ctx Alda context.
 */
void alda_midi_cleanup(AldaContext* ctx);

/* ============================================================================
 * Port Management
 * ============================================================================ */

/**
 * @brief List available MIDI output ports.
 * @param ctx Alda context.
 */
void alda_midi_list_ports(AldaContext* ctx);

/**
 * @brief Open a MIDI output port by index.
 * @param ctx Alda context.
 * @param port_idx Port index (0-based).
 * @return 0 on success, -1 on error.
 */
int alda_midi_open_port(AldaContext* ctx, int port_idx);

/**
 * @brief Create a virtual MIDI output port.
 * @param ctx Alda context.
 * @param name Port name.
 * @return 0 on success, -1 on error.
 */
int alda_midi_open_virtual(AldaContext* ctx, const char* name);

/**
 * @brief Open a port by name (searches hardware, falls back to virtual).
 * @param ctx Alda context.
 * @param name Port name or substring to match.
 * @return 0 on success, -1 on error.
 */
int alda_midi_open_by_name(AldaContext* ctx, const char* name);

/**
 * @brief Auto-select MIDI output: first available port, or virtual if none.
 * @param ctx Alda context.
 * @param virtual_name Name for virtual port if no hardware ports found.
 * @return 0 on success, -1 on error.
 */
int alda_midi_open_auto(AldaContext* ctx, const char* virtual_name);

/**
 * @brief Close the current MIDI output.
 * @param ctx Alda context.
 */
void alda_midi_close(AldaContext* ctx);

/**
 * @brief Check if MIDI output is open.
 * @param ctx Alda context.
 * @return Non-zero if open, 0 if closed.
 */
int alda_midi_is_open(AldaContext* ctx);

/* ============================================================================
 * MIDI Message Sending
 * ============================================================================ */

/**
 * @brief Send a note-on message.
 * @param ctx Alda context.
 * @param channel MIDI channel (1-16).
 * @param pitch Note pitch (0-127).
 * @param velocity Note velocity (0-127).
 */
void alda_midi_send_note_on(AldaContext* ctx, int channel, int pitch, int velocity);

/**
 * @brief Send a note-off message.
 * @param ctx Alda context.
 * @param channel MIDI channel (1-16).
 * @param pitch Note pitch (0-127).
 */
void alda_midi_send_note_off(AldaContext* ctx, int channel, int pitch);

/**
 * @brief Send a program change message.
 * @param ctx Alda context.
 * @param channel MIDI channel (1-16).
 * @param program GM program number (0-127).
 */
void alda_midi_send_program(AldaContext* ctx, int channel, int program);

/**
 * @brief Send a control change message.
 * @param ctx Alda context.
 * @param channel MIDI channel (1-16).
 * @param cc Controller number (0-127).
 * @param value Controller value (0-127).
 */
void alda_midi_send_cc(AldaContext* ctx, int channel, int cc, int value);

/**
 * @brief Send all notes off on all channels.
 * @param ctx Alda context.
 */
void alda_midi_all_notes_off(AldaContext* ctx);

/* ============================================================================
 * Timing
 * ============================================================================ */

/**
 * @brief Sleep for a specified number of milliseconds.
 * @param ctx Alda context (respects no_sleep_mode).
 * @param ms Milliseconds to sleep.
 */
void alda_midi_sleep_ms(AldaContext* ctx, int ms);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_MIDI_BACKEND_H */
