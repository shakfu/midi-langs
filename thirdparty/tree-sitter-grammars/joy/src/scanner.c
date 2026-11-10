/**
 * External scanner for Joy language
 * Handles:
 * - Nested block comments (* ... *)
 * - String interpolation $"...${...}..."
 */

#include "tree_sitter/parser.h"
#include <wctype.h>

enum TokenType {
  BLOCK_COMMENT,
  INTERPOLATED_STRING,
};

void *tree_sitter_joy_external_scanner_create(void) {
  return NULL;
}

void tree_sitter_joy_external_scanner_destroy(void *payload) {
}

unsigned tree_sitter_joy_external_scanner_serialize(void *payload, char *buffer) {
  return 0;
}

void tree_sitter_joy_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
}

static void advance(TSLexer *lexer) {
  lexer->advance(lexer, false);
}

static void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

static bool scan_block_comment(TSLexer *lexer) {
  // We've already seen '(' - check for '*'
  if (lexer->lookahead != '(') {
    return false;
  }
  advance(lexer);

  if (lexer->lookahead != '*') {
    return false;
  }
  advance(lexer);

  int depth = 1;

  while (depth > 0 && !lexer->eof(lexer)) {
    if (lexer->lookahead == '(') {
      advance(lexer);
      if (lexer->lookahead == '*') {
        advance(lexer);
        depth++;
      }
    } else if (lexer->lookahead == '*') {
      advance(lexer);
      if (lexer->lookahead == ')') {
        advance(lexer);
        depth--;
      }
    } else {
      advance(lexer);
    }
  }

  return depth == 0;
}

static bool scan_interpolated_string(TSLexer *lexer) {
  // We expect '$"' at the start
  if (lexer->lookahead != '$') {
    return false;
  }
  advance(lexer);

  if (lexer->lookahead != '"') {
    return false;
  }
  advance(lexer);

  while (!lexer->eof(lexer)) {
    if (lexer->lookahead == '"') {
      advance(lexer);
      return true;
    } else if (lexer->lookahead == '\\') {
      // Escape sequence
      advance(lexer);
      if (!lexer->eof(lexer)) {
        advance(lexer);
      }
    } else if (lexer->lookahead == '$') {
      advance(lexer);
      if (lexer->lookahead == '{') {
        advance(lexer);
        // Scan until matching '}'
        int brace_depth = 1;
        int bracket_depth = 0;
        while (brace_depth > 0 && !lexer->eof(lexer)) {
          if (lexer->lookahead == '{') {
            brace_depth++;
            advance(lexer);
          } else if (lexer->lookahead == '}') {
            brace_depth--;
            if (brace_depth > 0) {
              advance(lexer);
            } else {
              advance(lexer);  // consume closing }
            }
          } else if (lexer->lookahead == '[') {
            bracket_depth++;
            advance(lexer);
          } else if (lexer->lookahead == ']') {
            bracket_depth--;
            advance(lexer);
          } else if (lexer->lookahead == '"') {
            // Nested string in interpolation
            advance(lexer);
            while (!lexer->eof(lexer) && lexer->lookahead != '"') {
              if (lexer->lookahead == '\\') {
                advance(lexer);
                if (!lexer->eof(lexer)) advance(lexer);
              } else {
                advance(lexer);
              }
            }
            if (lexer->lookahead == '"') advance(lexer);
          } else {
            advance(lexer);
          }
        }
      }
      // else: literal $ not followed by {, continue
    } else {
      advance(lexer);
    }
  }

  return false;  // Unterminated string
}

bool tree_sitter_joy_external_scanner_scan(
  void *payload,
  TSLexer *lexer,
  const bool *valid_symbols
) {
  // Skip whitespace
  while (iswspace(lexer->lookahead)) {
    skip(lexer);
  }

  if (valid_symbols[BLOCK_COMMENT] && lexer->lookahead == '(') {
    lexer->result_symbol = BLOCK_COMMENT;
    return scan_block_comment(lexer);
  }

  if (valid_symbols[INTERPOLATED_STRING] && lexer->lookahead == '$') {
    lexer->result_symbol = INTERPOLATED_STRING;
    return scan_interpolated_string(lexer);
  }

  return false;
}
