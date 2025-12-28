#!/usr/bin/env python3

"""prelude2c -- Convert prelude source files to C headers

Converts source files in various languages (Scheme, Lua, Python) to C header
files containing the source as string constants.

Usage:
    ./scripts/prelude2c.py [options] <input_file>

Examples:
    ./scripts/prelude2c.py projects/s7-midi/prelude.scm
    ./scripts/prelude2c.py --strip-comments projects/lua-midi/prelude.lua
    ./scripts/prelude2c.py -o custom.h -v MY_PRELUDE input.py
"""

import argparse
import io
import os
import re
import sys
import tokenize
from pathlib import Path

# Language configurations
LANG_CONFIG = {
    '.scm': {
        'comment_pattern': r';.*$',
        'varname': 'SCHEME_PRELUDE_MODULE',
        'header_comment': 'Scheme prelude - constants and helper functions',
        'strip_comments': False,  # Keep Scheme comments by default (informative)
    },
    '.lua': {
        'comment_pattern': r'--.*$',
        'varname': 'LUA_PRELUDE_MODULE',
        'header_comment': 'Lua prelude - constants and helper functions',
        'strip_comments': False,
    },
    '.py': {
        'comment_pattern': None,  # Python uses tokenizer for proper handling
        'varname': 'PY_PRELUDE_MODULE',
        'header_comment': 'Python prelude - constants and helper functions',
        'strip_comments': True,  # Strip Python comments/docstrings by default
    },
    '.hs': {
        'comment_pattern': r'--.*$',
        'varname': 'HS_PRELUDE_MODULE',
        'header_comment': 'Haskell prelude - constants and helper functions',
        'strip_comments': False,
    },
}


def remove_python_comments_and_docstrings(source):
    """Remove comments and docstrings from Python source.

    Based on tokenize module for accurate parsing.
    """
    io_obj = io.StringIO(source)
    out = ""
    prev_toktype = tokenize.INDENT
    last_lineno = -1
    last_col = 0

    for tok in tokenize.generate_tokens(io_obj.readline):
        token_type = tok[0]
        token_string = tok[1]
        start_line, start_col = tok[2]
        end_line, end_col = tok[3]

        if start_line > last_lineno:
            last_col = 0
        if start_col > last_col:
            out += " " * (start_col - last_col)

        # Remove comments
        if token_type == tokenize.COMMENT:
            pass
        # Remove docstrings
        elif token_type == tokenize.STRING:
            if prev_toktype != tokenize.INDENT:
                if prev_toktype != tokenize.NEWLINE:
                    if start_col > 0:
                        out += token_string
        else:
            out += token_string

        prev_toktype = token_type
        last_col = end_col
        last_lineno = end_line

    return out


def remove_regex_comments(source, pattern):
    """Remove comments matching a regex pattern (line by line)."""
    lines = source.split('\n')
    result = []
    for line in lines:
        cleaned = re.sub(pattern, '', line, flags=re.MULTILINE)
        result.append(cleaned.rstrip())
    return '\n'.join(result)


def remove_multiple_empty_lines(contents):
    """Reduce multiple consecutive blank lines to single blank lines."""
    return re.sub(r'\n\s*\n', '\n\n', contents)


def remove_main_block(source, ext):
    """Remove if __name__ == '__main__' blocks for Python."""
    if ext != '.py':
        return source

    lines = source.split('\n')
    result = []
    include = True
    for line in lines:
        if line.startswith("if __name__ == "):
            include = False
        if include:
            result.append(line)
    return '\n'.join(result)


def to_c_string(source, varname):
    """Convert source code to a C string constant."""
    # Escape backslashes first, then quotes
    source = source.replace('\\', '\\\\')
    source = source.replace('"', '\\"')

    lines = source.split('\n')
    c_lines = []
    for line in lines:
        c_lines.append(f'"{line}\\n"')

    return '\n'.join(c_lines)


def process_file(input_path, output_path=None, varname=None, strip_comments=None,
                 strip_empty_lines=True):
    """Process a source file and generate a C header."""
    input_path = Path(input_path)
    ext = input_path.suffix.lower()

    if ext not in LANG_CONFIG:
        raise ValueError(f"Unsupported file extension: {ext}. "
                        f"Supported: {', '.join(LANG_CONFIG.keys())}")

    config = LANG_CONFIG[ext]

    # Determine output path
    if output_path is None:
        stem = input_path.stem
        # prelude.scm -> scm_prelude.h
        output_path = input_path.parent / f"{ext[1:]}_prelude.h"
    else:
        output_path = Path(output_path)

    # Determine variable name
    if varname is None:
        varname = config['varname']

    # Determine whether to strip comments
    if strip_comments is None:
        strip_comments = config.get('strip_comments', False)

    # Read source
    with open(input_path) as f:
        source = f.read()

    # Process source
    source = remove_main_block(source, ext)

    if strip_comments:
        if ext == '.py':
            source = remove_python_comments_and_docstrings(source)
        elif config['comment_pattern']:
            source = remove_regex_comments(source, config['comment_pattern'])

    if strip_empty_lines:
        source = remove_multiple_empty_lines(source)

    # Strip trailing whitespace from each line and file
    lines = [line.rstrip() for line in source.split('\n')]
    # Remove trailing empty lines
    while lines and not lines[-1]:
        lines.pop()
    source = '\n'.join(lines)

    # Generate C header
    c_string = to_c_string(source, varname)

    header_comment = config['header_comment']
    header_content = f"""/* ============================================================================
 * {header_comment}
 * Generated from {input_path.name} by scripts/prelude2c.py
 * ============================================================================ */

static const char *{varname} =
{c_string};
"""

    # Write output
    with open(output_path, 'w') as f:
        f.write(header_content)

    return output_path


def main():
    parser = argparse.ArgumentParser(
        prog='prelude2c',
        description='Convert prelude source files to C headers',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    %(prog)s projects/s7-midi/prelude.scm
    %(prog)s --strip-comments projects/lua-midi/prelude.lua
    %(prog)s -o custom.h -v MY_PRELUDE input.py

Supported extensions: .scm, .lua, .py, .hs
        """
    )

    parser.add_argument('input', help='Input source file')
    parser.add_argument('-o', '--output', help='Output header file (default: <lang>_prelude.h)')
    parser.add_argument('-v', '--varname', help='C variable name (default: based on language)')
    parser.add_argument('--strip-comments', action='store_true', default=None,
                        help='Strip comments from source')
    parser.add_argument('--keep-comments', action='store_true',
                        help='Keep comments in source')
    parser.add_argument('--no-strip-empty', action='store_true',
                        help='Keep multiple consecutive empty lines')

    args = parser.parse_args()

    # Handle comment stripping flags
    strip_comments = args.strip_comments
    if args.keep_comments:
        strip_comments = False

    try:
        output = process_file(
            args.input,
            output_path=args.output,
            varname=args.varname,
            strip_comments=strip_comments,
            strip_empty_lines=not args.no_strip_empty
        )
        print(f"Generated: {output}")
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == '__main__':
    main()
