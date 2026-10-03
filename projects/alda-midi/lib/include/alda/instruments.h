/**
 * @file instruments.h
 * @brief General MIDI instrument name to program number mapping.
 */

#ifndef ALDA_INSTRUMENTS_H
#define ALDA_INSTRUMENTS_H

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Look up a GM program number by instrument name.
 *
 * Accepts various Alda instrument names and aliases:
 *   "piano", "acoustic-grand-piano" -> 0
 *   "violin" -> 40
 *   "trumpet" -> 56
 *   etc.
 *
 * @param name Instrument name.
 * @return GM program number (0-127), or -1 if not found.
 */
int alda_instrument_program(const char* name);

/**
 * @brief Get the canonical name for a GM program number.
 * @param program GM program number (0-127).
 * @return Instrument name, or NULL if invalid.
 */
const char* alda_instrument_name(int program);

/**
 * @brief Check if an instrument is a percussion instrument.
 * @param name Instrument name.
 * @return 1 if percussion, 0 otherwise.
 */
int alda_instrument_is_percussion(const char* name);

/**
 * @brief Get instrument names matching a prefix for tab completion.
 *
 * @param prefix  Prefix to match (empty string matches all)
 * @param count   Output: number of completions returned
 * @param max     Maximum number of completions to return
 * @return Array of strdup'd strings (caller frees array and strings), or NULL
 */
char** alda_instrument_get_completions(const char* prefix, int* count, int max);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_INSTRUMENTS_H */
