/**
 * @file Joy language grammar for tree-sitter
 * @author Joy Language Authors
 * @license MIT
 */

/// <reference types="tree-sitter-cli/dsl" />
// @ts-check

module.exports = grammar({
  name: 'joy',

  externals: $ => [
    $.block_comment,
    $.interpolated_string,
  ],

  extras: $ => [
    /\s/,
    $.line_comment,
    $.block_comment,
  ],

  word: $ => $.symbol,

  rules: {
    source_file: $ => repeat($._item),

    _item: $ => choice(
      $.definition,
      $.statement,
      $.library_keyword,
      $.shell_escape,
      $.semicolon,
      // Standalone period (end of program)
      '.',
    ),

    // Shell escape: $ command (at start of line)
    shell_escape: $ => token(seq('$', /[^\n]*/)),

    // Library/Module keywords (LIBRA, DEFINE, HIDE, IN, END, MODULE, etc.)
    library_keyword: $ => choice(
      'LIBRA',
      'DEFINE',
      'HIDE',
      'IN',
      'END',
      'MODULE',
      'PRIVATE',
      'PUBLIC',
      'CONST',
      'INLINE',
    ),

    // Definitions: name == body (. or ;)
    definition: $ => seq(
      field('name', $.symbol),
      '==',
      optional(field('body', $._body)),
      choice('.', ';'),
    ),

    _body: $ => repeat1($._expression),

    // A statement is one or more expressions followed by a period
    statement: $ => seq(
      repeat1($._expression),
      '.',
    ),

    // Expressions
    _expression: $ => choice(
      $._literal,
      $.quotation,
      $.set_literal,
      $.native_vector,
      $.native_matrix,
      $.symbol,
      $.operator,
    ),

    // Literals
    _literal: $ => choice(
      $.integer,
      $.float,
      $.character,
      $.string,
      $.interpolated_string,
      $.boolean,
      $.null,
    ),

    integer: $ => token(seq(
      optional('-'),
      /[0-9]+/,
    )),

    float: $ => token(seq(
      optional('-'),
      /[0-9]+/,
      '.',
      /[0-9]+/,
    )),

    character: $ => token(seq(
      "'",
      choice(
        /[^\\']/,
        /\\[btnvfr"'\\]/,
        /\\[0-7]{1,3}/,
      ),
    )),

    string: $ => token(seq(
      '"',
      repeat(choice(
        /[^"\\]/,
        /\\[btnvfr"'\\]/,
        /\\[0-7]{1,3}/,
      )),
      '"',
    )),

    boolean: $ => choice('true', 'false'),

    null: $ => 'null',

    // Quotations/lists: [...]
    quotation: $ => seq(
      '[',
      repeat($._quotation_item),
      ']',
    ),

    _quotation_item: $ => choice(
      $._expression,
      $.cons_operator,
    ),

    cons_operator: $ => ':',

    // Set literal: {...}
    set_literal: $ => seq(
      '{',
      repeat($._expression),
      '}',
    ),

    // Native vector: v[1 2 3]
    native_vector: $ => seq(
      'v[',
      repeat($._number),
      ']',
    ),

    // Native matrix: m[[1 2] [3 4]]
    native_matrix: $ => seq(
      'm[',
      repeat($.matrix_row),
      ']',
    ),

    matrix_row: $ => seq(
      '[',
      repeat($._number),
      ']',
    ),

    _number: $ => choice($.integer, $.float),

    // Symbols (identifiers)
    symbol: $ => /[a-zA-Z_][a-zA-Z0-9_\-?!]*/,

    // Symbolic operators (non-alphabetic)
    operator: $ => choice(
      // Arithmetic
      '+', '-', '*', '/',
      // Comparison
      '=', '<', '>', '<=', '>=', '!=',
      // Conversion operators starting with > or ending with >
      '>set', '>dict', '>vec', '>mat', '>list', '>json', 'json>',
      // Vector operators
      'v+', 'v-', 'v*', 'v/',
      // Matrix operators
      'm+', 'm-', 'm*', 'm/',
    ),

    // Semicolon separator
    semicolon: $ => ';',

    // Line comment: # ..., or \\ ... as in joy-midi (midi-langs change)
    line_comment: $ => token(seq(choice('#', '\\'), /.*/)),
  },
});
