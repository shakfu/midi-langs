/**
 * @file error.h
 * @brief Error handling for the Alda parser.
 */

#ifndef ALDA_ERROR_H
#define ALDA_ERROR_H

#include "tokens.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Error types.
 */
typedef enum {
    ALDA_ERR_NONE = 0,
    ALDA_ERR_SCAN,              /* Lexical error */
    ALDA_ERR_SYNTAX,            /* Syntax/parse error */
    ALDA_ERR_MEMORY,            /* Memory allocation error */
} AldaErrorType;

/**
 * @brief Error structure.
 */
typedef struct {
    AldaErrorType type;         /* Error type */
    char* message;              /* Error message (owned) */
    AldaSourcePos pos;          /* Source position */
    char* source_line;          /* Source line containing error (owned, may be NULL) */
} AldaError;

/**
 * @brief Create a new error.
 * @param type Error type.
 * @param message Error message (will be copied).
 * @param pos Source position.
 * @param source Full source text (for extracting line, may be NULL).
 * @return Newly allocated error. Caller must free with alda_error_free().
 */
AldaError* alda_error_new(AldaErrorType type, const char* message,
                          AldaSourcePos pos, const char* source);

/**
 * @brief Free an error.
 * @param err Error to free.
 */
void alda_error_free(AldaError* err);

/**
 * @brief Format an error message with context.
 * @param err Error to format.
 * @return Formatted error string. Caller must free().
 */
char* alda_error_format(AldaError* err);

/**
 * @brief Extract a line from source text.
 * @param source Full source text.
 * @param line Line number (1-based).
 * @return Newly allocated line text. Caller must free().
 */
char* alda_extract_line(const char* source, int line);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_ERROR_H */
