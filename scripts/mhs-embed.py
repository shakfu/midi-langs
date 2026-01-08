#!/usr/bin/env python3
"""
mhs-embed.py - Embed files into C headers for MicroHs standalone binaries

Unified Python tool that supports:
- Source mode: Embed .hs/.c/.h files (default: uncompressed)
- Package mode: Embed .pkg files with module mappings (--pkg-mode)
- Zstd compression: Optional with --zstd flag (requires: pip install zstandard)

This is an optional development tool. The C version (mhs-embed.c) is the
primary tool used by the build system.

Usage:
    mhs-embed.py <output.h> [libdir ...] [options]

Examples:
    # Source mode (uncompressed)
    mhs-embed.py build/mhs_embedded.h lib/ --runtime src/runtime/

    # Source mode with zstd compression
    mhs-embed.py build/mhs_embedded_zstd.h lib/ --zstd

    # Package mode
    mhs-embed.py build/mhs_embedded_pkgs.h --pkg-mode \\
        --base-pkg ~/.mcabal/mhs-0.15.2.0/packages/base.pkg \\
        --music-pkg build/music.pkg \\
        --base-dir ~/.mcabal/mhs-0.15.2.0
"""

import argparse
import os
import sys
from pathlib import Path

# Optional zstd support
try:
    import zstandard as zstd
    HAS_ZSTD = True
except ImportError:
    HAS_ZSTD = False

# Compression settings
COMPRESSION_LEVEL = 19
DICT_SIZE = 112 * 1024
CHUNK_SIZE = 4000


def escape_c_string(content: bytes) -> str:
    """Escape raw bytes for C string literal."""
    result = []
    for byte in content:
        if byte == ord('\\'):
            result.append('\\\\')
        elif byte == ord('"'):
            result.append('\\"')
        elif byte == ord('\n'):
            result.append('\\n')
        elif byte == ord('\r'):
            result.append('\\r')
        elif byte == ord('\t'):
            result.append('\\t')
        elif byte < 32 or byte > 126:
            result.append(f'\\{byte:03o}')
        else:
            result.append(chr(byte))
    return ''.join(result)


def split_string_literal(escaped: str, max_len: int = CHUNK_SIZE) -> list[str]:
    """Split a long string into multiple literals for C89 compatibility."""
    if len(escaped) <= max_len:
        return [escaped]

    chunks = []
    current = []
    current_len = 0

    i = 0
    while i < len(escaped):
        if escaped[i] == '\\' and i + 1 < len(escaped):
            if escaped[i + 1] in '\\"\'\n\r\t':
                seq = escaped[i:i + 2]
                i += 2
            elif escaped[i + 1].isdigit():
                end = i + 2
                while end < len(escaped) and end < i + 4 and escaped[end].isdigit():
                    end += 1
                seq = escaped[i:end]
                i = end
            else:
                seq = escaped[i:i + 2]
                i += 2
        else:
            seq = escaped[i]
            i += 1

        if current_len + len(seq) > max_len:
            chunks.append(''.join(current))
            current = [seq]
            current_len = len(seq)
        else:
            current.append(seq)
            current_len += len(seq)

    if current:
        chunks.append(''.join(current))

    return chunks


def format_byte_array(data: bytes, bytes_per_line: int = 16) -> str:
    """Format binary data as C byte array initializer."""
    lines = []
    for i in range(0, len(data), bytes_per_line):
        chunk = data[i:i + bytes_per_line]
        hex_bytes = ', '.join(f'0x{b:02x}' for b in chunk)
        lines.append(f'    {hex_bytes}')
    return ',\n'.join(lines)


def sanitize_name(name: str) -> str:
    """Convert path/filename to valid C identifier."""
    return ''.join(c if c.isalnum() else '_' for c in name)


def is_text_file(path: str) -> bool:
    """Check if file is a text file that benefits from dictionary compression."""
    text_extensions = {'.hs', '.hs-boot', '.c', '.h', '.txt'}
    return any(path.endswith(ext) for ext in text_extensions)


def collect_hs_files(lib_dirs: list[str]) -> list[tuple[str, str, bytes]]:
    """Collect all .hs files from library directories."""
    files = []
    seen_paths = set()

    for lib_dir in lib_dirs:
        lib_path = Path(lib_dir).resolve()
        if not lib_path.exists():
            print(f"Warning: Library directory not found: {lib_dir}", file=sys.stderr)
            continue

        base_name = lib_path.name

        for pattern in ["*.hs", "*.hs-boot"]:
            for hs_file in lib_path.rglob(pattern):
                rel_path = hs_file.relative_to(lib_path)
                vfs_path = f"{base_name}/{rel_path}"

                if vfs_path in seen_paths:
                    print(f"Note: Skipping duplicate {vfs_path}", file=sys.stderr)
                    continue
                seen_paths.add(vfs_path)

                try:
                    content = hs_file.read_bytes()
                    files.append((vfs_path, str(hs_file), content))
                except Exception as e:
                    print(f"Warning: Could not read {hs_file}: {e}", file=sys.stderr)

    return files


def collect_runtime_files(runtime_dir: str) -> list[tuple[str, str, bytes]]:
    """Collect runtime C/H files for embedding."""
    files = []
    runtime_path = Path(runtime_dir).resolve()
    if not runtime_path.exists():
        print(f"Warning: Runtime directory not found: {runtime_dir}", file=sys.stderr)
        return files

    src_parent = runtime_path.parent

    for pattern in ["*.c", "*.h"]:
        for c_file in runtime_path.rglob(pattern):
            rel_path = c_file.relative_to(src_parent.parent)
            vfs_path = str(rel_path)

            try:
                content = c_file.read_bytes()
                files.append((vfs_path, str(c_file), content))
            except Exception as e:
                print(f"Warning: Could not read {c_file}: {e}", file=sys.stderr)

    return files


def collect_extra_files(file_paths: list[str], vfs_prefix: str) -> list[tuple[str, str, bytes]]:
    """Collect extra files (libraries, headers) for embedding."""
    files = []

    for file_path in file_paths:
        path = Path(file_path).resolve()
        if not path.exists():
            print(f"Warning: File not found: {file_path}", file=sys.stderr)
            continue

        vfs_path = f"{vfs_prefix}/{path.name}"

        try:
            content = path.read_bytes()
            files.append((vfs_path, str(path), content))
            print(f"  Embedding: {path.name} ({len(content):,} bytes)")
        except Exception as e:
            print(f"Warning: Could not read {path}: {e}", file=sys.stderr)

    return files


def collect_txt_files(base_dir: Path) -> list[tuple[str, Path]]:
    """Collect .txt module mapping files from base directory."""
    txt_files = []
    for txt_path in base_dir.rglob("*.txt"):
        if "packages" in txt_path.parts:
            continue
        rel_path = txt_path.relative_to(base_dir)
        vfs_path = str(rel_path)
        txt_files.append((vfs_path, txt_path))
    return sorted(txt_files, key=lambda x: x[0])


def collect_runtime_files_pkg(base_dir: Path) -> list[tuple[str, Path]]:
    """Collect runtime source files for package mode."""
    runtime_files = []
    packages_dir = base_dir / "packages"

    if not packages_dir.exists():
        return []

    for mhs_dir in packages_dir.iterdir():
        if mhs_dir.is_dir() and mhs_dir.name.startswith("mhs-"):
            runtime_dir = mhs_dir / "data" / "src" / "runtime"
            if runtime_dir.exists():
                for file_path in runtime_dir.rglob("*"):
                    if file_path.is_file():
                        rel_path = file_path.relative_to(runtime_dir)
                        vfs_path = f"src/runtime/{rel_path}"
                        runtime_files.append((vfs_path, file_path))
                break

    return sorted(runtime_files, key=lambda x: x[0])


# ============================================================
# Source mode - Uncompressed
# ============================================================

def generate_header_uncompressed(output_path: str, files: list[tuple[str, str, bytes]]) -> None:
    """Generate uncompressed C header."""
    total_size = sum(len(content) for _, _, content in files)
    hs_count = sum(1 for p, _, _ in files if p.endswith('.hs') or p.endswith('.hs-boot'))
    runtime_count = len(files) - hs_count

    print(f"Embedding {len(files)} files ({hs_count} .hs, {runtime_count} runtime, {total_size:,} bytes)")

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("/* Auto-generated by mhs-embed.py - DO NOT EDIT */\n")
        f.write(f"/* {len(files)} embedded files ({total_size:,} bytes total) */\n\n")
        f.write("#ifndef MHS_EMBEDDED_LIBS_H\n")
        f.write("#define MHS_EMBEDDED_LIBS_H\n\n")
        f.write("#include <stddef.h>\n\n")

        f.write("typedef struct {\n")
        f.write("    const char* path;\n")
        f.write("    const char* content;\n")
        f.write("    size_t length;\n")
        f.write("} EmbeddedFile;\n\n")

        f.write("static const EmbeddedFile embedded_files[] = {\n")

        for vfs_path, full_path, content in sorted(files):
            escaped = escape_c_string(content)
            chunks = split_string_literal(escaped)

            f.write(f"    /* {full_path} */\n")
            f.write(f'    {{ "{vfs_path}",\n')

            if len(chunks) == 1:
                f.write(f'      "{chunks[0]}",\n')
            else:
                f.write('      ')
                for i, chunk in enumerate(chunks):
                    if i > 0:
                        f.write('\n      ')
                    f.write(f'"{chunk}"')
                f.write(',\n')

            f.write(f'      {len(content)} }},\n\n')

        f.write("    { NULL, NULL, 0 }\n")
        f.write("};\n\n")

        f.write(f"#define EMBEDDED_FILE_COUNT {len(files)}\n\n")
        f.write("#endif /* MHS_EMBEDDED_LIBS_H */\n")

    print(f"Generated: {output_path}")


# ============================================================
# Source mode - Zstd compressed
# ============================================================

def generate_header_zstd(output_path: str, files: list[tuple[str, str, bytes]]) -> None:
    """Generate zstd-compressed C header."""
    if not HAS_ZSTD:
        print("Error: --zstd requires zstandard. Install with: pip install zstandard",
              file=sys.stderr)
        sys.exit(1)

    # Separate text files for dictionary training
    text_files = [(p, fp, c) for p, fp, c in files if is_text_file(p)]
    total_uncompressed = sum(len(c) for _, _, c in files)

    print(f"\nCollected {len(files)} files ({total_uncompressed:,} bytes)")
    print(f"  Text files: {len(text_files)} (dictionary compression)")

    # Train dictionary
    dict_data = b""
    if text_files:
        samples = [content for _, _, content in text_files]
        total_sample = sum(len(s) for s in samples)
        print(f"Training dictionary from {len(samples)} samples ({total_sample:,} bytes)...")
        dict_data = zstd.train_dictionary(DICT_SIZE, samples).as_bytes()
        print(f"Dictionary trained: {len(dict_data):,} bytes")

    # Compress files
    compressed_files = []
    total_compressed = 0

    cdict = zstd.ZstdCompressionDict(dict_data) if dict_data else None
    compressor_dict = zstd.ZstdCompressor(level=COMPRESSION_LEVEL, dict_data=cdict) if cdict else None
    compressor_no_dict = zstd.ZstdCompressor(level=COMPRESSION_LEVEL)

    print("Compressing files...")
    for vfs_path, full_path, content in sorted(files):
        if is_text_file(vfs_path) and compressor_dict:
            compressed = compressor_dict.compress(content)
            use_dict = True
        else:
            compressed = compressor_no_dict.compress(content)
            use_dict = False

        compressed_files.append((vfs_path, full_path, content, compressed, use_dict))
        total_compressed += len(compressed)

    dict_size = len(dict_data)
    total_embedded = total_compressed + dict_size
    ratio = total_uncompressed / total_embedded if total_embedded else 0

    print(f"\nCompression results:")
    print(f"  Original:   {total_uncompressed:,} bytes")
    print(f"  Compressed: {total_compressed:,} bytes")
    print(f"  Dictionary: {dict_size:,} bytes")
    print(f"  Total:      {total_embedded:,} bytes ({ratio:.2f}x)")

    # Generate header
    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("/* Auto-generated by mhs-embed.py --zstd - DO NOT EDIT */\n")
        f.write(f"/* {len(files)} files, zstd compressed */\n")
        f.write(f"/* Original: {total_uncompressed:,} -> Embedded: {total_embedded:,} ({ratio:.2f}x) */\n\n")

        f.write("#ifndef MHS_EMBEDDED_ZSTD_H\n")
        f.write("#define MHS_EMBEDDED_ZSTD_H\n\n")
        f.write("#include <stddef.h>\n")
        f.write("#include <stdint.h>\n\n")

        # Dictionary
        if dict_data:
            f.write("/* Zstd dictionary */\n")
            f.write("static const unsigned char embedded_zstd_dict[] = {\n")
            f.write(format_byte_array(dict_data))
            f.write('\n};\n')
            f.write(f"#define EMBEDDED_DICT_SIZE {len(dict_data)}\n\n")
        else:
            f.write("static const unsigned char embedded_zstd_dict[] = {};\n")
            f.write("#define EMBEDDED_DICT_SIZE 0\n\n")

        # Struct
        f.write("typedef struct {\n")
        f.write("    const char* path;\n")
        f.write("    const unsigned char* data;\n")
        f.write("    uint32_t compressed_size;\n")
        f.write("    uint32_t original_size;\n")
        f.write("    uint8_t use_dict;\n")
        f.write("} EmbeddedFileZstd;\n\n")

        # Compressed data arrays
        f.write("/* Compressed file data */\n")
        for i, (vfs_path, _, original, compressed, _) in enumerate(compressed_files):
            f.write(f"/* {vfs_path} ({len(original)} -> {len(compressed)}) */\n")
            f.write(f"static const unsigned char file_data_{i}[] = {{\n")
            f.write(format_byte_array(compressed))
            f.write('\n};\n\n')

        # File table
        f.write("static const EmbeddedFileZstd embedded_files_zstd[] = {\n")
        for i, (vfs_path, _, original, compressed, use_dict) in enumerate(compressed_files):
            f.write(f'    {{ "{vfs_path}", file_data_{i}, {len(compressed)}, {len(original)}, {1 if use_dict else 0} }},\n')
        f.write("    { NULL, NULL, 0, 0, 0 }\n")
        f.write("};\n\n")

        f.write(f"#define EMBEDDED_FILE_COUNT {len(files)}\n")
        f.write(f"#define EMBEDDED_TOTAL_COMPRESSED {total_compressed}\n")
        f.write(f"#define EMBEDDED_TOTAL_ORIGINAL {total_uncompressed}\n\n")
        f.write("#endif /* MHS_EMBEDDED_ZSTD_H */\n")

    print(f"Generated: {output_path}")


# ============================================================
# Package mode
# ============================================================

def generate_header_pkg(output_path: str, packages: list[tuple[str, str]],
                        txt_files: list[tuple[str, Path | bytes]],
                        runtime_files: list[tuple[str, Path]],
                        use_zstd: bool = False) -> None:
    """Generate package-mode C header."""

    if use_zstd and not HAS_ZSTD:
        print("Error: --zstd requires zstandard. Install with: pip install zstandard",
              file=sys.stderr)
        sys.exit(1)

    # Collect all data
    entries = []  # (vfs_path, content, file_type)

    # Packages
    total_pkg = 0
    for vfs_path, file_path in packages:
        path = Path(file_path).expanduser().resolve()
        if not path.exists():
            print(f"Error: Package not found: {file_path}", file=sys.stderr)
            sys.exit(1)
        content = path.read_bytes()
        total_pkg += len(content)
        entries.append((vfs_path, content, 'pkg'))
        print(f"  {path.name}: {len(content):,} bytes")

    print(f"Packages: {len(packages)} files, {total_pkg:,} bytes")

    # Txt files
    total_txt = 0
    for vfs_path, source in txt_files:
        if isinstance(source, Path):
            content = source.read_bytes()
        else:
            content = source
        total_txt += len(content)
        entries.append((vfs_path, content, 'txt'))

    print(f"Module mappings: {len(txt_files)} files, {total_txt:,} bytes")

    # Runtime files
    total_rt = 0
    for vfs_path, file_path in runtime_files:
        content = file_path.read_bytes()
        total_rt += len(content)
        entries.append((vfs_path, content, 'runtime'))

    print(f"Runtime files: {len(runtime_files)} files, {total_rt:,} bytes")

    total_uncompressed = total_pkg + total_txt + total_rt

    if use_zstd:
        _generate_header_pkg_zstd(output_path, entries, total_uncompressed)
    else:
        _generate_header_pkg_plain(output_path, entries)


def _generate_header_pkg_plain(output_path: str, entries: list[tuple[str, bytes, str]]) -> None:
    """Generate uncompressed package header."""
    pkg_count = sum(1 for _, _, t in entries if t == 'pkg')
    txt_count = sum(1 for _, _, t in entries if t == 'txt')
    rt_count = sum(1 for _, _, t in entries if t == 'runtime')
    total_size = sum(len(c) for _, c, _ in entries)

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("/* Auto-generated by mhs-embed.py --pkg-mode - DO NOT EDIT */\n")
        f.write(f"/* {pkg_count} packages, {txt_count} txt, {rt_count} runtime */\n\n")

        f.write("#ifndef MHS_EMBEDDED_PKGS_H\n")
        f.write("#define MHS_EMBEDDED_PKGS_H\n\n")
        f.write("#include <stddef.h>\n\n")

        # Data arrays
        for i, (vfs_path, content, _) in enumerate(sorted(entries)):
            f.write(f"/* {vfs_path} */\n")
            f.write(f"static const unsigned char pkg_data_{i}[] = {{\n")
            f.write(format_byte_array(content))
            f.write('\n};\n\n')

        # Struct
        f.write("typedef struct {\n")
        f.write("    const char* path;\n")
        f.write("    const unsigned char* content;\n")
        f.write("    size_t length;\n")
        f.write("    int file_type;\n")
        f.write("} EmbeddedPackage;\n\n")

        f.write("#define PKG_FILE_TYPE_PKG     0\n")
        f.write("#define PKG_FILE_TYPE_TXT     1\n")
        f.write("#define PKG_FILE_TYPE_RUNTIME 2\n\n")

        # Table
        type_map = {'pkg': 0, 'txt': 1, 'runtime': 2}
        f.write("static const EmbeddedPackage embedded_packages[] = {\n")
        for i, (vfs_path, content, ftype) in enumerate(sorted(entries)):
            f.write(f'    {{ "{vfs_path}", pkg_data_{i}, {len(content)}, {type_map[ftype]} }},\n')
        f.write("    { NULL, NULL, 0, 0 }\n")
        f.write("};\n\n")

        f.write(f"#define EMBEDDED_PACKAGE_COUNT {pkg_count}\n")
        f.write(f"#define EMBEDDED_TXT_COUNT {txt_count}\n")
        f.write(f"#define EMBEDDED_RUNTIME_COUNT {rt_count}\n\n")
        f.write("#endif /* MHS_EMBEDDED_PKGS_H */\n")

    print(f"Generated: {output_path} ({total_size:,} bytes)")


def _generate_header_pkg_zstd(output_path: str, entries: list[tuple[str, bytes, str]],
                               total_uncompressed: int) -> None:
    """Generate zstd-compressed package header."""
    # Train dictionary
    samples = [c for _, c, _ in entries]
    total_sample = sum(len(s) for s in samples)
    print(f"Training dictionary from {len(samples)} samples ({total_sample:,} bytes)...")

    dict_data = zstd.train_dictionary(DICT_SIZE, samples).as_bytes()
    print(f"Dictionary trained: {len(dict_data):,} bytes")

    # Compress
    cdict = zstd.ZstdCompressionDict(dict_data)
    compressor = zstd.ZstdCompressor(level=COMPRESSION_LEVEL, dict_data=cdict)

    compressed_entries = []
    total_compressed = 0

    for vfs_path, content, ftype in sorted(entries):
        compressed = compressor.compress(content)
        total_compressed += len(compressed)
        compressed_entries.append((vfs_path, content, compressed, ftype))

    dict_size = len(dict_data)
    total_embedded = total_compressed + dict_size
    ratio = total_uncompressed / total_embedded if total_embedded else 0

    print(f"\nCompression: {total_uncompressed:,} -> {total_embedded:,} ({ratio:.2f}x)")

    pkg_count = sum(1 for _, _, _, t in compressed_entries if t == 'pkg')
    txt_count = sum(1 for _, _, _, t in compressed_entries if t == 'txt')
    rt_count = sum(1 for _, _, _, t in compressed_entries if t == 'runtime')

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("/* Auto-generated by mhs-embed.py --pkg-mode --zstd - DO NOT EDIT */\n")
        f.write(f"/* {pkg_count} packages, {txt_count} txt, {rt_count} runtime */\n")
        f.write(f"/* Original: {total_uncompressed:,} -> Embedded: {total_embedded:,} ({ratio:.2f}x) */\n\n")

        f.write("#ifndef MHS_EMBEDDED_PKGS_ZSTD_H\n")
        f.write("#define MHS_EMBEDDED_PKGS_ZSTD_H\n\n")
        f.write("#include <stddef.h>\n")
        f.write("#include <stdint.h>\n\n")

        # Dictionary
        f.write("/* Zstd dictionary */\n")
        f.write("static const unsigned char embedded_pkg_zstd_dict[] = {\n")
        f.write(format_byte_array(dict_data))
        f.write('\n};\n')
        f.write(f"#define EMBEDDED_PKG_DICT_SIZE {len(dict_data)}\n\n")

        # Compressed data
        for i, (vfs_path, original, compressed, _) in enumerate(compressed_entries):
            f.write(f"/* {vfs_path} ({len(original)} -> {len(compressed)}) */\n")
            f.write(f"static const unsigned char pkgzstd_data_{i}[] = {{\n")
            f.write(format_byte_array(compressed))
            f.write('\n};\n\n')

        # Struct
        f.write("typedef struct {\n")
        f.write("    const char* path;\n")
        f.write("    const unsigned char* data;\n")
        f.write("    uint32_t compressed_size;\n")
        f.write("    uint32_t original_size;\n")
        f.write("    uint8_t file_type;\n")
        f.write("} EmbeddedFilePkgZstd;\n\n")

        f.write("#define PKG_FILE_TYPE_PKG     0\n")
        f.write("#define PKG_FILE_TYPE_TXT     1\n")
        f.write("#define PKG_FILE_TYPE_RUNTIME 2\n\n")

        # Table
        type_map = {'pkg': 0, 'txt': 1, 'runtime': 2}
        f.write("static const EmbeddedFilePkgZstd embedded_files_pkg_zstd[] = {\n")
        for i, (vfs_path, original, compressed, ftype) in enumerate(compressed_entries):
            f.write(f'    {{ "{vfs_path}", pkgzstd_data_{i}, {len(compressed)}, {len(original)}, {type_map[ftype]} }},\n')
        f.write("    { NULL, NULL, 0, 0, 0 }\n")
        f.write("};\n\n")

        f.write(f"#define EMBEDDED_PKG_ZSTD_PACKAGE_COUNT {pkg_count}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TXT_COUNT {txt_count}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_RUNTIME_COUNT {rt_count}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TOTAL_COUNT {len(compressed_entries)}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TOTAL_COMPRESSED {total_compressed}\n")
        f.write(f"#define EMBEDDED_PKG_ZSTD_TOTAL_ORIGINAL {total_uncompressed}\n\n")
        f.write("#endif /* MHS_EMBEDDED_PKGS_ZSTD_H */\n")

    print(f"Generated: {output_path}")


# ============================================================
# Main
# ============================================================

def main():
    parser = argparse.ArgumentParser(
        description="Embed files into C headers for MicroHs standalone binaries"
    )
    parser.add_argument("output", help="Output header file path")
    parser.add_argument("libdirs", nargs="*", help="Library directories to embed")
    parser.add_argument("--runtime", help="Runtime directory to embed")
    parser.add_argument("--header", action="append", dest="headers",
                        help="Header file to embed (repeatable)")
    parser.add_argument("--lib", action="append", dest="libs",
                        help="Library file (.a) to embed (repeatable)")
    parser.add_argument("--zstd", action="store_true",
                        help="Enable zstd compression (requires: pip install zstandard)")

    # Package mode options
    parser.add_argument("--pkg-mode", action="store_true",
                        help="Package embedding mode")
    parser.add_argument("--base-pkg", help="Path to base.pkg")
    parser.add_argument("--music-pkg", help="Path to music.pkg")
    parser.add_argument("--base-dir", help="Base directory containing .txt files")
    parser.add_argument("--music-modules", default="Async,Midi,MidiPerform,Music,MusicPerform",
                        help="Comma-separated music modules")

    args = parser.parse_args()

    output_path = Path(args.output)
    output_path.parent.mkdir(parents=True, exist_ok=True)

    if args.pkg_mode:
        # Package mode
        if not args.base_pkg or not args.music_pkg or not args.base_dir:
            print("Error: --pkg-mode requires --base-pkg, --music-pkg, and --base-dir",
                  file=sys.stderr)
            sys.exit(1)

        base_dir = Path(args.base_dir).expanduser().resolve()
        if not base_dir.exists():
            print(f"Error: Base directory not found: {args.base_dir}", file=sys.stderr)
            sys.exit(1)

        # Collect files
        txt_files = collect_txt_files(base_dir)
        runtime_files = collect_runtime_files_pkg(base_dir)

        # Build package list
        base_name = Path(args.base_pkg).name
        music_name = Path(args.music_pkg).name
        packages = [
            (f"packages/{base_name}", args.base_pkg),
            (f"packages/{music_name}", args.music_pkg),
        ]

        # Add synthetic txt for music modules
        music_modules = [m.strip() for m in args.music_modules.split(',') if m.strip()]
        for mod in music_modules:
            txt_files.append((f"{mod}.txt", music_name.encode()))

        # Add extra libs/headers to runtime files
        if args.libs:
            for lib in args.libs:
                path = Path(lib).resolve()
                if path.exists():
                    runtime_files.append((f"lib/{path.name}", path))

        if args.headers:
            for hdr in args.headers:
                path = Path(hdr).resolve()
                if path.exists():
                    runtime_files.append((f"include/{path.name}", path))

        print("Embedding packages, module mappings, and runtime files:")
        generate_header_pkg(args.output, packages, txt_files, runtime_files, args.zstd)

    else:
        # Source mode
        if not args.libdirs:
            print("Error: At least one library directory required", file=sys.stderr)
            sys.exit(1)

        files = collect_hs_files(args.libdirs)

        if args.runtime:
            runtime_files = collect_runtime_files(args.runtime)
            files.extend(runtime_files)

        if args.headers:
            header_files = collect_extra_files(args.headers, "src/runtime")
            files.extend(header_files)

        if args.libs:
            print("Embedding libraries:")
            lib_files = collect_extra_files(args.libs, "lib")
            files.extend(lib_files)

        if not files:
            print("Error: No files found", file=sys.stderr)
            sys.exit(1)

        if args.zstd:
            generate_header_zstd(args.output, files)
        else:
            generate_header_uncompressed(args.output, files)


if __name__ == "__main__":
    main()
