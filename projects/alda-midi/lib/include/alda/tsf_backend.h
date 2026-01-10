/**
 * @file tsf_backend.h
 * @brief Built-in synthesizer using TinySoundFont + miniaudio.
 *
 * Provides an optional audio output backend that can run alongside
 * the libremidi MIDI backend. Useful as a fallback when external
 * synths (FluidSynth, hardware) aren't available.
 */

#ifndef ALDA_TSF_BACKEND_H
#define ALDA_TSF_BACKEND_H

#ifdef __cplusplus
extern "C" {
#endif

/* ============================================================================
 * Initialization and Cleanup
 * ============================================================================ */

/**
 * @brief Initialize the TSF backend (must be called before other functions).
 * @return 0 on success, -1 on error.
 */
int alda_tsf_init(void);

/**
 * @brief Cleanup TSF resources and stop audio.
 */
void alda_tsf_cleanup(void);

/* ============================================================================
 * Soundfont Management
 * ============================================================================ */

/**
 * @brief Load a SoundFont file (.sf2).
 * @param path Path to the .sf2 file.
 * @return 0 on success, -1 on error.
 */
int alda_tsf_load_soundfont(const char* path);

/**
 * @brief Check if a soundfont is loaded.
 * @return Non-zero if loaded, 0 if not.
 */
int alda_tsf_has_soundfont(void);

/**
 * @brief Get the number of presets in the loaded soundfont.
 * @return Number of presets, or 0 if no soundfont loaded.
 */
int alda_tsf_get_preset_count(void);

/**
 * @brief Get the name of a preset by index.
 * @param index Preset index (0 to preset_count-1).
 * @return Preset name, or NULL if invalid index.
 */
const char* alda_tsf_get_preset_name(int index);

/* ============================================================================
 * Enable/Disable
 * ============================================================================ */

/**
 * @brief Enable the built-in synth (starts audio output).
 * @return 0 on success, -1 on error (e.g., no soundfont loaded).
 */
int alda_tsf_enable(void);

/**
 * @brief Disable the built-in synth (stops audio output).
 */
void alda_tsf_disable(void);

/**
 * @brief Check if the built-in synth is enabled.
 * @return Non-zero if enabled, 0 if disabled.
 */
int alda_tsf_is_enabled(void);

/* ============================================================================
 * MIDI Message Sending (real-time)
 * ============================================================================ */

/**
 * @brief Send a note-on message.
 * @param channel MIDI channel (1-16).
 * @param pitch Note pitch (0-127).
 * @param velocity Note velocity (0-127).
 */
void alda_tsf_send_note_on(int channel, int pitch, int velocity);

/**
 * @brief Send a note-off message.
 * @param channel MIDI channel (1-16).
 * @param pitch Note pitch (0-127).
 */
void alda_tsf_send_note_off(int channel, int pitch);

/**
 * @brief Send a program change message.
 * @param channel MIDI channel (1-16).
 * @param program GM program number (0-127).
 */
void alda_tsf_send_program(int channel, int program);

/**
 * @brief Send a control change message.
 * @param channel MIDI channel (1-16).
 * @param cc Controller number (0-127).
 * @param value Controller value (0-127).
 */
void alda_tsf_send_cc(int channel, int cc, int value);

/**
 * @brief Send all notes off on all channels.
 */
void alda_tsf_all_notes_off(void);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_TSF_BACKEND_H */
