#!/usr/bin/env python3
"""Fix markdown table formatting issues (MD060/table-column-style).

Fixes:
1. Leading double pipes (||) to single pipe (|)
2. Separator rows: adds spaces around dashes (|------| -> | ------ |)

Usage:
    python scripts/fix-markdown-tables.py [--dry-run] [--path PATH]
"""

import argparse
import re
import sys
from pathlib import Path


def is_separator_row(line: str) -> bool:
    """Check if a line is a markdown table separator row."""
    line = line.strip()
    if not line.startswith('|') or not line.endswith('|'):
        return False
    
    # Remove leading/trailing pipes
    content = line[1:-1].strip()
    if not content:
        return False
    
    # Split by pipes and check if all parts are dashes
    parts = [p.strip() for p in content.split('|')]
    return all(part and all(c in '-|' for c in part) for part in parts)


def fix_separator_row(line: str) -> str:
    """Fix separator row by adding spaces around dashes."""
    line = line.rstrip('\n')
    if not line.startswith('|') or not line.endswith('|'):
        return line + '\n'
    
    # Split into parts
    parts = [p.strip() for p in line[1:-1].split('|')]
    fixed_parts = []
    
    for part in parts:
        if part and all(c in '-|' for c in part):
            # Count dashes (ignore pipes)
            dashes = len([c for c in part if c == '-'])
            if dashes > 0:
                fixed_parts.append(f' {"-" * dashes} ')
            else:
                fixed_parts.append(part)
        else:
            fixed_parts.append(part)
    
    return '|' + '|'.join(fixed_parts) + '|\n'


def fix_table_line(line: str) -> str:
    """Fix a single table line."""
    # Fix leading double pipe
    if line.startswith('||'):
        line = '|' + line[2:]
    
    # Fix separator rows
    if is_separator_row(line):
        line = fix_separator_row(line)
    
    return line


def fix_markdown_file(filepath: Path, dry_run: bool = False) -> tuple[int, list[str]]:
    """Fix table formatting issues in a markdown file.
    
    Returns:
        (number of changes, list of change descriptions)
    """
    try:
        with open(filepath, 'r', encoding='utf-8') as f:
            lines = f.readlines()
    except Exception as e:
        return 0, [f"Error reading file: {e}"]
    
    changes = []
    fixed_lines = []
    changed = False
    
    for i, line in enumerate(lines, 1):
        original = line
        fixed = fix_table_line(line)
        
        if original != fixed:
            changed = True
            # Describe the change
            if original.startswith('||') and fixed.startswith('|'):
                changes.append(f"  Line {i}: Fixed leading || to |")
            elif is_separator_row(original):
                changes.append(f"  Line {i}: Fixed separator row spacing")
            fixed_lines.append(fixed)
        else:
            fixed_lines.append(original)
    
    if changed and not dry_run:
        try:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.writelines(fixed_lines)
        except Exception as e:
            return 0, [f"Error writing file: {e}"]
    
    return len(changes), changes


def find_markdown_files(root: Path) -> list[Path]:
    """Recursively find all .md files."""
    md_files = []
    for path in root.rglob('*.md'):
        if path.is_file():
            md_files.append(path)
    return sorted(md_files)


def main():
    parser = argparse.ArgumentParser(
        description='Fix markdown table formatting issues (MD060)',
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
  # Dry run to see what would be fixed
  python scripts/fix-markdown-tables.py --dry-run

  # Fix all markdown files
  python scripts/fix-markdown-tables.py

  # Fix files in a specific directory
  python scripts/fix-markdown-tables.py --path docs/
        """
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Show what would be fixed without making changes'
    )
    parser.add_argument(
        '--path',
        type=str,
        default='.',
        help='Path to search for markdown files (default: current directory)'
    )
    parser.add_argument(
        '--verbose',
        action='store_true',
        help='Show detailed changes for each file'
    )
    
    args = parser.parse_args()
    
    root = Path(args.path).resolve()
    if not root.exists():
        print(f"Error: Path does not exist: {root}", file=sys.stderr)
        return 1
    
    md_files = find_markdown_files(root)
    
    if not md_files:
        print(f"No markdown files found in {root}")
        return 0
    
    print(f"Found {len(md_files)} markdown file(s)")
    if args.dry_run:
        print("DRY RUN MODE - No files will be modified\n")
    else:
        print()
    
    total_changes = 0
    files_changed = 0
    
    for md_file in md_files:
        rel_path = md_file.relative_to(root)
        num_changes, change_list = fix_markdown_file(md_file, dry_run=args.dry_run)
        
        if num_changes > 0:
            files_changed += 1
            total_changes += num_changes
            print(f"{rel_path}: {num_changes} change(s)")
            if args.verbose:
                for change in change_list:
                    print(change)
    
    print()
    if args.dry_run:
        print(f"Would fix {total_changes} issue(s) in {files_changed} file(s)")
    else:
        print(f"Fixed {total_changes} issue(s) in {files_changed} file(s)")
    
    return 0


if __name__ == '__main__':
    sys.exit(main())
