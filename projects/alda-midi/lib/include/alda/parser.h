/**
 * @file parser.h
 * @brief Parser for the Alda language.
 */

#ifndef ALDA_PARSER_H
#define ALDA_PARSER_H

#include "ast.h"
#include "error.h"
#include "tokens.h"

#ifdef __cplusplus
extern "C" {
#endif

/**
 * @brief Opaque parser structure.
 */
typedef struct AldaParser AldaParser;

/**
 * @brief Create a new parser.
 * @param source Source text to parse (will not be copied, must remain valid).
 * @param filename Source filename for error reporting (may be NULL).
 * @return Newly allocated parser. Caller must free with alda_parser_free().
 */
AldaParser* alda_parser_new(const char* source, const char* filename);

/**
 * @brief Free a parser.
 * @param parser Parser to free.
 */
void alda_parser_free(AldaParser* parser);

/**
 * @brief Parse source text and produce an AST.
 * @param parser Parser to use.
 * @return Root AST node. Caller must free with alda_ast_free().
 *         Returns NULL on error.
 */
AldaNode* alda_parser_parse(AldaParser* parser);

/**
 * @brief Check if parser has an error.
 * @param parser Parser to check.
 * @return Non-zero if an error occurred.
 */
int alda_parser_has_error(AldaParser* parser);

/**
 * @brief Get the parser error.
 * @param parser Parser to check.
 * @return Error structure, or NULL if no error. Do not free.
 */
const AldaError* alda_parser_error(AldaParser* parser);

/**
 * @brief Get formatted error message.
 * @param parser Parser to check.
 * @return Formatted error string. Caller must free().
 */
char* alda_parser_error_string(AldaParser* parser);

/**
 * @brief Convenience function to parse a string.
 * @param source Source text to parse.
 * @param filename Source filename for error reporting (may be NULL).
 * @param error Output: error message if parsing fails (caller must free).
 * @return Root AST node. Caller must free with alda_ast_free().
 *         Returns NULL on error.
 */
AldaNode* alda_parse(const char* source, const char* filename, char** error);

#ifdef __cplusplus
}
#endif

#endif /* ALDA_PARSER_H */
