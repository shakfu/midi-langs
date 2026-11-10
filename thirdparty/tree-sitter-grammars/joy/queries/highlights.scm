; Keywords
(library_keyword) @keyword

; Definition operator
"==" @keyword.operator

; Boolean literals
(boolean) @constant.builtin

; Null literal
(null) @constant.builtin

; Numeric literals
(integer) @number
(float) @number.float

; Character literal
(character) @character

; String literals
(string) @string
(interpolated_string) @string

; Comments
(line_comment) @comment
(block_comment) @comment

; Shell escape
(shell_escape) @comment.note

; Operators
(operator) @operator

; Cons operator in patterns
(cons_operator) @operator

; Definition name
(definition
  name: (symbol) @function)

; Symbols (identifiers)
(symbol) @variable

; Quotation brackets
[
  "["
  "]"
] @punctuation.bracket

; Set literal brackets
[
  "{"
  "}"
] @punctuation.bracket

; Native vector/matrix brackets
(native_vector
  "v[" @punctuation.special)
(native_matrix
  "m[" @punctuation.special)

; Statement/definition terminator
"." @punctuation.delimiter
(semicolon) @punctuation.delimiter

; Built-in stack operations (highlighted via symbol)
((symbol) @function.builtin
  (#any-of? @function.builtin
    "dup" "drop" "swap" "rot" "over" "pop"
    "dip" "dipd" "dipdd"
    "nullary" "unary" "binary" "ternary"
    "i" "x"))

; Built-in control flow
((symbol) @keyword.control
  (#any-of? @keyword.control
    "branch" "ifte" "cond" "while"
    "genrec" "binrec" "linrec" "primrec" "tailrec"))

; Built-in type predicates
((symbol) @type.builtin
  (#match? @type.builtin "\\?$"))

; Pattern matching keywords
((symbol) @keyword.control
  (#any-of? @keyword.control
    "match" "cases" "let"))

; I/O operations
((symbol) @function.builtin
  (#any-of? @function.builtin
    "put" "putch" "putchars" "putstring"
    "get" "getch"))

; List operations
((symbol) @function.builtin
  (#any-of? @function.builtin
    "map" "filter" "fold" "step"
    "first" "rest" "cons" "concat"
    "reverse" "size" "at"
    "app" "app1" "app2" "compose"))

; Math functions
((symbol) @function.builtin
  (#any-of? @function.builtin
    "abs" "neg" "sign" "sqrt" "exp" "log" "log10"
    "sin" "cos" "tan" "asin" "acos" "atan" "atan2"
    "floor" "ceil" "round" "truncate"
    "max" "min" "pow" "rem" "mod"
    "succ" "pred"))

; Boolean operators
((symbol) @function.builtin
  (#any-of? @function.builtin
    "and" "or" "xor" "not"))

; Dictionary operations
((symbol) @function.builtin
  (#any-of? @function.builtin
    "dempty" "dput" "dget" "dgetd" "dhas" "ddel"
    "dkeys" "dvals" "dsize" "dmerge"))
