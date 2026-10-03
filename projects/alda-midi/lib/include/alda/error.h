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
    char* context;              /* Parsing context hint (owned, may be NULL) */
    char* expected;             /* What was expected (owned, may be NULL) */
    char* found;                /* What was found (owned, may be NULL) */
} AldaError;

/**
 * @brief Error list for collecting multiple errors.
 */
typedef struct {
    AldaError** errors;         /* Array of error pointers */
    int count;                  /* Number of errors */
    int capacity;               /* Array capacity */
    int max_errors;             /* Maximum errors to collect (0 = unlimited) */
} AldaErrorList;

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

/**
 * @brief Create an error with extended context.
 * @param type Error type.
 * @param message Error message (will be copied).
 * @param pos Source position.
 * @param source Full source text (for extracting line, may be NULL).
 * @param context Parsing context (e.g., "in S-expression", may be NULL).
 * @param expected What token/construct was expected (may be NULL).
 * @param found What was actually found (may be NULL).
 * @return Newly allocated error. Caller must free with alda_error_free().
 */
AldaError* alda_error_new_detailed(AldaErrorType type, const char* message,
                                    AldaSourcePos pos, const char* source,
                                    const char* context, const char* expected,
                                    const char* found);

/**
 * @brief Initialize an error list.
 * @param list Error list to initialize.
 * @param max_errors Maximum errors to collect (0 = unlimited, default 10).
 */
void alda_error_list_init(AldaErrorList* list, int max_errors);

/**
 * @brief Free an error list and all contained errors.
 * @param list Error list to free.
 */
void alda_error_list_free(AldaErrorList* list);

/**
 * @brief Add an error to the list.
 * @param list Error list.
 * @param error Error to add (ownership transferred).
 * @return 1 if added, 0 if list is full or allocation failed.
 */
int alda_error_list_add(AldaErrorList* list, AldaError* error);

/**
 * @brief Check if error list has reached its limit.
 * @param list Error list.
 * @return Non-zero if at limit.
 */
int alda_error_list_full(AldaErrorList* list);

/**
 * @brief Format all errors in the list.
 * @param list Error list.
 * @return Formatted string with all errors. Caller must free().
 */
char* alda_error_list_format(AldaErrorList* list);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_ERROR_H */
