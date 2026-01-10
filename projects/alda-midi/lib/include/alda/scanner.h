/**
 * @file scanner.h
 * @brief Lexer/scanner for the Alda parser.
 */

#ifndef ALDA_SCANNER_H
#define ALDA_SCANNER_H

#include "tokens.h"
#include "error.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Opaque scanner structure.
 */
typedef struct AldaScanner AldaScanner;

/**
 * @brief Create a new scanner.
 * @param source Source text to scan (will not be copied, must remain valid).
 * @param filename Source filename for error reporting (may be NULL).
 * @return Newly allocated scanner. Caller must free with alda_scanner_free().
 */
AldaScanner* alda_scanner_new(const char* source, const char* filename);

/**
 * @brief Free a scanner.
 * @param scanner Scanner to free.
 */
void alda_scanner_free(AldaScanner* scanner);

/**
 * @brief Scan source text and produce tokens.
 * @param scanner Scanner to use.
 * @param count Output: number of tokens produced.
 * @return Array of tokens. Caller must free with alda_tokens_free().
 *         Returns NULL on memory error.
 */
AldaToken* alda_scanner_scan(AldaScanner* scanner, size_t* count);

/**
 * @brief Check if scanner has an error.
 * @param scanner Scanner to check.
 * @return Non-zero if an error occurred.
 */
int alda_scanner_has_error(AldaScanner* scanner);

/**
 * @brief Get the scanner error.
 * @param scanner Scanner to check.
 * @return Error structure, or NULL if no error. Do not free.
 */
const AldaError* alda_scanner_error(AldaScanner* scanner);

/**
 * @brief Get formatted error message.
 * @param scanner Scanner to check.
 * @return Formatted error string. Caller must free().
 */
char* alda_scanner_error_string(AldaScanner* scanner);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_SCANNER_H */
