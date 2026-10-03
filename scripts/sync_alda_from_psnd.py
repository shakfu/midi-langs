#!/usr/bin/env python3
"""Vendor psnd's Alda interpreter, tests and conformance data into alda-midi.

psnd (`source/langs/alda/`) is the upstream. Files fall into four groups:

- verbatim: copied byte for byte.
- adapted: copied, then edited to drop psnd-only fields (SharedContext,
  Scala, Csound). Every edit must find its anchor; a missing anchor is an
  error, since it means upstream changed the code the edit expects.
- watched: midi-langs keeps its own version. A change upstream is reported,
  for a manual merge, by comparing against the hash recorded at the last port.
- ignored: psnd-only files.

An upstream file in none of these groups is reported, so new files are not
missed. Files present only in midi-langs are reported, never deleted.

Usage:
    scripts/sync_alda_from_psnd.py [--psnd PATH] [--check]

--check writes nothing and exits 1 if any file would change.
See docs/alda-midi/conformance.md.
"""

import argparse
import hashlib
import os
import re
import subprocess
import sys
from dataclasses import dataclass, field
from pathlib import Path
from typing import Callable, Dict, List, Optional, Tuple

REPO_ROOT = Path(__file__).resolve().parent.parent


class SyncError(Exception):
    """An upstream file is missing or no longer matches an adaptation."""


# ============================================================================
# Edit helpers
# ============================================================================

def replace_once(text: str, old: str, new: str, name: str) -> str:
    """Replace the single occurrence of old; raise if it occurs 0 or 2+ times."""
    count = text.count(old)
    if count != 1:
        raise SyncError(f"{name}: expected 1 occurrence of {old[:60]!r}, found {count}")
    return text.replace(old, new)


def replace_all(text: str, old: str, new: str, name: str) -> str:
    """Replace every occurrence of old; raise if there is none."""
    if old not in text:
        raise SyncError(f"{name}: anchor not found: {old[:60]!r}")
    return text.replace(old, new)


def sub_once(text: str, pattern: str, new: str, name: str) -> str:
    """Regex-substitute exactly one match of pattern."""
    result, count = re.subn(pattern, new, text)
    if count != 1:
        raise SyncError(f"{name}: expected 1 match of /{pattern[:60]}/, found {count}")
    return result


def replace_between(text: str, start: str, end: str, new: str, name: str) -> str:
    """Replace text from start up to, not including, end."""
    i = text.find(start)
    if i < 0:
        raise SyncError(f"{name}: start anchor not found: {start[:60]!r}")
    j = text.find(end, i)
    if j < 0:
        raise SyncError(f"{name}: end anchor not found after start: {end[:60]!r}")
    return text[:i] + new + text[j:]


# ============================================================================
# Adaptations
# ============================================================================

MIDI_OUTPUT_FIELDS = (
    "    /* MIDI output */\n"
    "    libremidi_midi_observer_handle* midi_observer;\n"
    "    libremidi_midi_out_handle* midi_out;\n"
    "    libremidi_midi_out_port* out_ports[ALDA_MAX_PORTS];\n"
)

MIDI_OUTPUT_INIT = (
    "    /* MIDI output handles */\n"
    "    ctx->midi_observer = NULL;\n"
    "    ctx->midi_out = NULL;\n"
    "    for (int i = 0; i < ALDA_MAX_PORTS; i++) {\n"
    "        ctx->out_ports[i] = NULL;\n"
    "    }\n"
)


def adapt_context_h(text: str) -> str:
    n = "context.h"
    text = replace_once(text, "/* Forward declaration for shared audio/MIDI context */\n"
                              "struct SharedContext;\n\n", "", n)
    text = sub_once(text, r"\n    /\* Microtuning \(NULL = standard 12-TET\) \*/\n(    .*\n){3}",
                    "\n", n)
    text = replace_once(text, "struct ScalaScale;  /* Forward declaration for microtuning */\n",
                        "", n)
    text = replace_between(text, "    /* Shared audio/MIDI/Link context */",
                           "    int out_port_count;", MIDI_OUTPUT_FIELDS, n)
    text = sub_once(text, r"    int out_port_count; +/\* Unused - always 0 \*/",
                    "    int out_port_count;", n)
    text = replace_once(text,
                        "    int builtin_synth_enabled;  /* Built-in synth enabled */\n"
                        "    int csound_enabled;  /* Csound synth enabled */\n",
                        "    int tsf_enabled;     /* Built-in synth enabled */\n", n)
    return text


def adapt_context_c(text: str) -> str:
    n = "context.c"
    text = replace_once(text, '#include "context.h"  /* SharedContext */\n', "", n)
    text = replace_between(text, "    /* SharedContext is NOT allocated here",
                           "    ctx->out_port_count = 0;", MIDI_OUTPUT_INIT, n)
    text = replace_between(text, "    /* SharedContext is NOT cleaned up here",
                           "    /* Note: Legacy MIDI cleanup", "", n)
    text = replace_once(text,
                        "    /* Note: Legacy MIDI cleanup is handled separately by midi_backend */",
                        "    /* Note: MIDI cleanup is handled separately by midi_backend */", n)
    text = sub_once(text, r"\n    /\* Microtuning \(12-TET by default\) \*/\n(    part->scale.*\n){3}",
                    "", n)
    return text


def adapt_test_shared_suite(text: str) -> str:
    n = "test_shared_suite.c"
    edits = [
        ("Check psnd's Alda output", "Check alda-midi's output"),
        ("test_alda_shared_suite DIR", "test_shared_suite DIR"),
        ("and psnd numbers them", "and alda-midi numbers them"),
        ("are read from psnd's events", "are read from alda-midi's events"),
        ("read_psnd", "read_ours"),
        ("Reading psnd's events", "Reading alda-midi's events"),
        (", psnd %", ", ours %"),
        ("must be in effect in psnd's", "must be in effect in ours"),
        ("docs/dev/conformance.md the method", "docs/alda-midi/conformance.md the method"),
    ]
    for old, new in edits:
        text = replace_all(text, old, new, n)
    leftover = [line for line in text.splitlines() if "psnd" in line.lower()]
    if leftover:
        raise SyncError(f"{n}: new psnd references to adapt: {leftover[:3]}")
    return text


# ============================================================================
# Manifest
# ============================================================================

IMPL = "source/langs/alda/impl"
INC = "source/langs/alda/include/alda"
TESTS = "source/langs/alda/tests"
LIB = "projects/alda-midi/lib"
DOCS = "docs/alda-midi"


@dataclass
class Manifest:
    """Upstream (psnd-relative) to destination (midi-langs-relative) paths."""
    verbatim: Dict[str, str] = field(default_factory=dict)
    adapted: Dict[str, Tuple[str, Callable[[str], str]]] = field(default_factory=dict)
    # Upstream path to the sha256 it had when midi-langs last merged it
    watched: Dict[str, str] = field(default_factory=dict)
    ignored: List[str] = field(default_factory=list)
    # (upstream dir, destination dir, glob, names excluded from the copy)
    data_dirs: List[Tuple[str, str, str, Tuple[str, ...]]] = field(default_factory=list)
    # Upstream dirs whose files must all be listed in one of the groups above
    audited_dirs: List[Tuple[str, str]] = field(default_factory=list)


def default_manifest() -> Manifest:
    m = Manifest()
    for name in ["ast.c", "attributes.c", "error.c", "finalize.c", "finalize.h",
                 "instruments.c", "instruments_alda.inc", "interpreter.c",
                 "parser.c", "scanner.c", "tokens.c"]:
        m.verbatim[f"{IMPL}/{name}"] = f"{LIB}/src/{name}"
    for name in ["alda.h", "ast.h", "error.h", "instruments.h", "interpreter.h",
                 "midi_backend.h", "parser.h", "scanner.h", "scheduler.h",
                 "tokens.h", "tsf_backend.h"]:
        m.verbatim[f"{INC}/{name}"] = f"{LIB}/include/alda/{name}"
    for name in ["scanner", "parser", "parser_fuzz", "interpreter"]:
        m.verbatim[f"{TESTS}/test_{name}.c"] = f"projects/alda-midi/tests/test_{name}.c"
    for name in ["test_framework.h", "test_framework.c"]:
        m.verbatim[f"source/testing/{name}"] = f"projects/alda-midi/tests/{name}"

    m.adapted[f"{IMPL}/context.c"] = (f"{LIB}/src/context.c", adapt_context_c)
    m.adapted[f"{INC}/context.h"] = (f"{LIB}/include/alda/context.h", adapt_context_h)
    m.adapted[f"{TESTS}/test_shared_suite.c"] = (
        "projects/alda-midi/tests/test_shared_suite.c", adapt_test_shared_suite)

    # midi-langs fixed overflow and drift in its playback loop (CHANGELOG)
    m.watched["source/langs/alda/scheduler.c"] = (
        "f9e31c19827df6c0dab2da0bb6415e6669669871a772c69575e79d9705035d9a")

    m.ignored = [f"{IMPL}/scala.c", f"{INC}/scala.h", f"{INC}/csound_backend.h",
                 f"{INC}/async.h"]
    m.ignored += [f"{TESTS}/{name}" for name in [
        "CMakeLists.txt", "test_backends.c", "test_csound_microtuning.c",
        "test_examples.c", "test_integration.c", "test_microtuning.c"]]

    m.data_dirs = [
        ("source/langs/alda/examples", f"{DOCS}/examples", "*.alda", ()),
        (f"{TESTS}/shared_suite", f"{DOCS}/shared_suite", "*", ()),
        # alda_reference/README.md describes psnd's paths; midi-langs has its own
        (f"{TESTS}/alda_reference", f"{DOCS}/alda_reference", "*", ("README.md",)),
    ]
    m.audited_dirs = [(IMPL, "*"), (INC, "*"), (TESTS, "*.c")]
    return m


# ============================================================================
# Sync
# ============================================================================

@dataclass
class Report:
    changed: List[str] = field(default_factory=list)
    unchanged: int = 0
    warnings: List[str] = field(default_factory=list)


def sha256(path: Path) -> str:
    return hashlib.sha256(path.read_bytes()).hexdigest()


def plan(psnd: Path, dest: Path, m: Manifest) -> Tuple[Dict[Path, bytes], List[str]]:
    """Return the desired content of every destination file, and warnings."""
    want: Dict[Path, bytes] = {}
    warnings: List[str] = []

    def upstream(rel: str) -> Path:
        path = psnd / rel
        if not path.is_file():
            raise SyncError(f"upstream file missing: {rel}")
        return path

    for src, dst in m.verbatim.items():
        want[dest / dst] = upstream(src).read_bytes()

    for src, (dst, adapt) in m.adapted.items():
        text = upstream(src).read_bytes().decode("utf-8")
        want[dest / dst] = adapt(text).encode("utf-8")

    for src, recorded in m.watched.items():
        if sha256(upstream(src)) != recorded:
            warnings.append(f"{src} changed upstream; merge it by hand, then update "
                            f"its hash in {Path(__file__).name}")

    for src_dir, dst_dir, pattern, excluded in m.data_dirs:
        names = set()
        for path in sorted((psnd / src_dir).glob(pattern)):
            if path.is_file() and path.name not in excluded:
                names.add(path.name)
                want[dest / dst_dir / path.name] = path.read_bytes()
        for path in sorted((dest / dst_dir).glob(pattern)):
            if path.is_file() and path.name not in names and path.name not in excluded:
                warnings.append(f"{dst_dir}/{path.name} is not in psnd; delete it if "
                                f"upstream removed it")

    listed = set(m.verbatim) | set(m.adapted) | set(m.watched) | set(m.ignored)
    for src_dir, pattern in m.audited_dirs:
        for path in sorted((psnd / src_dir).glob(pattern)):
            rel = f"{src_dir}/{path.name}"
            if path.is_file() and rel not in listed:
                warnings.append(f"{rel} is new upstream; add it to the manifest")

    return want, warnings


def sync(psnd: Path, dest: Path, m: Manifest, write: bool) -> Report:
    want, warnings = plan(psnd, dest, m)
    report = Report(warnings=warnings)
    for path, content in sorted(want.items()):
        if path.is_file() and path.read_bytes() == content:
            report.unchanged += 1
            continue
        report.changed.append(str(path.relative_to(dest)))
        if write:
            path.parent.mkdir(parents=True, exist_ok=True)
            path.write_bytes(content)
    return report


def psnd_revision(psnd: Path) -> str:
    try:
        rev = subprocess.run(["git", "-C", str(psnd), "describe", "--always", "--dirty"],
                             capture_output=True, text=True, check=True).stdout.strip()
        return rev or "unknown"
    except (OSError, subprocess.CalledProcessError):
        return "unknown"


def default_psnd_dir() -> Path:
    return Path(os.environ.get("PSND_DIR", REPO_ROOT.parent / "psnd"))


def main(argv: Optional[List[str]] = None) -> int:
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0])
    parser.add_argument("--psnd", type=Path, default=default_psnd_dir(),
                        help="psnd checkout (default: $PSND_DIR or ../psnd)")
    parser.add_argument("--dest", type=Path, default=REPO_ROOT,
                        help=argparse.SUPPRESS)
    parser.add_argument("--check", action="store_true",
                        help="write nothing; exit 1 if any file would change")
    args = parser.parse_args(argv)

    if not (args.psnd / IMPL).is_dir():
        print(f"error: no psnd Alda sources under {args.psnd}", file=sys.stderr)
        return 2

    try:
        report = sync(args.psnd, args.dest, default_manifest(), write=not args.check)
    except SyncError as e:
        print(f"error: {e}", file=sys.stderr)
        return 2

    verb = "would update" if args.check else "updated"
    print(f"psnd {psnd_revision(args.psnd)}: {len(report.changed)} {verb}, "
          f"{report.unchanged} unchanged")
    for path in report.changed:
        print(f"  {verb}: {path}")
    for warning in report.warnings:
        print(f"  warning: {warning}")
    if report.changed and not args.check:
        print("Run `make test` and review `git diff`.")
    return 1 if args.check and report.changed else 0


if __name__ == "__main__":
    sys.exit(main())
