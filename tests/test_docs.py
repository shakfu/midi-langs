"""Run the code blocks in README.md and docs/; each must exit 0.

Fence conventions and the baseline workflow: docs/dev/doc-tests.md.
Regenerate the baseline with: python3 tests/test_docs.py --write-baseline
Regenerate README --help blocks with: python3 tests/test_docs.py --write-help
"""

import functools
import hashlib
import os
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from dataclasses import dataclass
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BUILD = Path(os.environ.get("MIDI_LANGS_BUILD", ROOT / "build")).resolve()
BASELINE = ROOT / "tests" / "docs_baseline.txt"
TIMEOUT = 30

# interpreter -> (command, {fence language: file extension})
# mhs-midi is left out until the MicroHs upgrade lands.
INTERPRETERS = {
    "alda-midi": (["alda_midi", "--no-sleep"], {"alda": ".alda"}),
    "stack-midi": (["stack_midi", "--no-sleep", "--script"], {"forth": ".stk"}),
    "pforth-midi": (["pforth_midi", "-q"], {"forth": ".fs"}),
    "joy-midi": (["joy_midi", "--no-sleep"], {"joy": ".joy"}),
    "lua-midi": (["lua_midi", "--no-sleep"], {"lua": ".lua"}),
    "pktpy-midi": (["pktpy_midi", "--no-sleep"], {"python": ".py"}),
    "s7-midi": (["s7_midi", "--no-sleep"], {"scheme": ".scm"}),
    "guile-midi": (["guile_midi", "--no-sleep"], {"scheme": ".scm", "wisp": ".w"}),
}

# Interpreter for a fence outside docs/<interpreter>/ and README sections
DEFAULT_FOR_FENCE = {
    "alda": "alda-midi",
    "forth": "stack-midi",
    "joy": "joy-midi",
    "lua": "lua-midi",
    "python": "pktpy-midi",
    "scheme": "s7-midi",
}

EXCLUDED = [
    "docs/dev/",
    "docs/mhs-midi/",
    # Upstream Alda documentation, vendored
    "docs/alda-midi/alda-language/",
    "docs/alda-midi/alda_reference/",
    "docs/alda-midi/shared_suite/",
]

FENCE = re.compile(r"^```(\S*)[ \t]*([^\n]*)\n(.*?)^```[ \t]*$", re.M | re.S)
SECTION = re.compile(r"^(#{1,6}) +(\S+)", re.M)
HELP = re.compile(r"^```sh\n% (\S+) --help\n(.*?)^```", re.M | re.S)
README = ROOT / "README.md"


@dataclass(frozen=True)
class Block:
    path: str  # relative to ROOT
    line: int
    interpreter: str
    ext: str
    code: str
    setup: str  # prepended before running; "" if none
    is_setup: bool

    @property
    def key(self) -> str:
        digest = hashlib.sha1(self.code.encode()).hexdigest()[:12]
        return f"{self.path} {digest}"

    @property
    def id(self) -> str:
        return f"{self.path}:{self.line}"


def doc_files() -> list[Path]:
    files = [ROOT / "README.md", *sorted((ROOT / "docs").rglob("*.md"))]
    return [
        f
        for f in files
        if f.name != "TODO.md"
        and not any(f.relative_to(ROOT).as_posix().startswith(e) for e in EXCLUDED)
    ]


def interpreter_sections(text: str, fences: list[re.Match]) -> list[tuple[int, str | None]]:
    """(offset, interpreter) for each README heading outside code fences."""
    inside = [(m.start(), m.end()) for m in fences]
    sections = []
    for h in SECTION.finditer(text):
        if any(a <= h.start() < b for a, b in inside):
            continue
        name = h.group(2)
        if name in INTERPRETERS or len(h.group(1)) <= 2:
            sections.append((h.start(), name if name in INTERPRETERS else None))
    return sections


def blocks_in(path: Path) -> list[Block]:
    rel = path.relative_to(ROOT).as_posix()
    text = path.read_text()
    fences = list(FENCE.finditer(text))
    parts = rel.split("/")
    dir_interp = parts[1] if parts[0] == "docs" and len(parts) > 2 else None
    sections = interpreter_sections(text, fences) if dir_interp is None else []

    found = []
    for m in fences:
        lang, flags, code = m.group(1), m.group(2).split(), m.group(3)
        if "norun" in flags:
            continue
        interp = dir_interp
        if interp is None:
            current = [i for off, i in sections if off < m.start()]
            interp = current[-1] if current and current[-1] else DEFAULT_FOR_FENCE.get(lang)
        if interp not in INTERPRETERS or lang not in INTERPRETERS[interp][1]:
            continue
        line = text.count("\n", 0, m.start()) + 1
        found.append((interp, lang, line, code, "setup" in flags))

    setups = {(i, lang): code for i, lang, _, code, is_setup in found if is_setup}
    return [
        Block(
            path=rel,
            line=line,
            interpreter=interp,
            ext=INTERPRETERS[interp][1][lang],
            code=code,
            setup="" if is_setup else setups.get((interp, lang), ""),
            is_setup=is_setup,
        )
        for interp, lang, line, code, is_setup in found
    ]


def all_blocks() -> list[Block]:
    return [b for f in doc_files() for b in blocks_in(f)]


def binary(block: Block) -> Path:
    return BUILD / INTERPRETERS[block.interpreter][0][0]


@functools.cache
def wisp_available() -> bool:
    """guile_midi runs .w files only with Guile 3.0.10+, which ships (language wisp)."""
    probe = Block("probe", 0, "guile-midi", ".w", "display 1\n", "", False)
    return binary(probe).exists() and run_block(probe)[0]


def skip_reason(block: Block) -> str | None:
    if not binary(block).exists():
        return f"{binary(block).name} not built"
    if (block.ext == ".w" or "(language wisp)" in block.code) and not wisp_available():
        return "this Guile has no (language wisp); needs 3.0.10+"
    return None


def run_block(block: Block) -> tuple[bool, str]:
    """Run block on the null backend in its own directory; (exit 0, output)."""
    cmd, _ = INTERPRETERS[block.interpreter]
    env = dict(os.environ, MIDI_LANGS_BACKEND="null")
    with tempfile.TemporaryDirectory() as tmp:
        script = Path(tmp) / f"block{block.ext}"
        script.write_text(block.setup + block.code)
        try:
            r = subprocess.run(
                [str(binary(block)), *cmd[1:], str(script)],
                cwd=tmp,
                env=env,
                stdin=subprocess.DEVNULL,
                capture_output=True,
                text=True,
                timeout=TIMEOUT,
            )
        except subprocess.TimeoutExpired:
            return False, f"timed out after {TIMEOUT}s"
    return r.returncode == 0, (r.stdout + r.stderr)[-2000:]


def run_all(blocks: list[Block]) -> dict[str, tuple[bool, str]]:
    with ThreadPoolExecutor(os.cpu_count()) as pool:
        return dict(zip((b.key for b in blocks), pool.map(run_block, blocks)))


def read_baseline() -> set[str]:
    if not BASELINE.exists():
        return set()
    keys = set()
    for line in BASELINE.read_text().splitlines():
        fields = line.split("#", 1)[0].split()
        if len(fields) >= 2:
            keys.add(f"{fields[0]} {fields[1]}")
    return keys


def write_baseline() -> None:
    blocks = [b for b in all_blocks() if skip_reason(b) is None]
    results = run_all(blocks)
    failing = [b for b in blocks if not results[b.key][0]]
    lines = [
        "# Doc blocks that fail today: <path> <sha1 of block, 12 hex>  # first line",
        "# Fix a block, then delete its line. See docs/dev/doc-tests.md.",
    ]
    for b in sorted(failing, key=lambda b: (b.path, b.line)):
        first = next((l.strip() for l in b.code.splitlines() if l.strip()), "")
        lines.append(f"{b.key}  # {first[:60]}")
    BASELINE.write_text("\n".join(lines) + "\n")
    print(f"{len(failing)} of {len(blocks)} blocks fail; wrote {BASELINE.relative_to(ROOT)}")


def help_binary(shown: str) -> Path:
    """Binary for a README '% ./path --help' line."""
    return ROOT / shown if shown.startswith("./scripts/") else BUILD / Path(shown).name


def help_output(shown: str) -> str:
    """--help output, run with argv[0] as README shows it."""
    r = subprocess.run(
        [shown, "--help"],
        executable=str(help_binary(shown)),
        cwd=ROOT,
        stdin=subprocess.DEVNULL,
        capture_output=True,
        text=True,
        timeout=TIMEOUT,
    )
    return (r.stdout + r.stderr).rstrip("\n") + "\n"


def write_help() -> None:
    def replace(m: re.Match) -> str:
        if not help_binary(m.group(1)).exists():
            print(f"skipped {m.group(1)}: not built")
            return m.group(0)
        return f"```sh\n% {m.group(1)} --help\n{help_output(m.group(1))}```"

    README.write_text(HELP.sub(replace, README.read_text()))


if __name__ == "__main__":
    if sys.argv[1:] == ["--write-baseline"]:
        write_baseline()
    elif sys.argv[1:] == ["--write-help"]:
        write_help()
    else:
        sys.exit(__doc__)
else:
    import pytest

    BLOCKS = all_blocks()

    @pytest.fixture(scope="session")
    def baseline() -> set[str]:
        return read_baseline()

    @pytest.fixture(scope="session")
    def results(request) -> dict[str, tuple[bool, str]]:
        """Run every selected block once, in parallel."""
        selected = [
            item.callspec.params["block"]
            for item in request.session.items
            if "block" in getattr(getattr(item, "callspec", None), "params", {})
        ]
        return run_all([b for b in selected if skip_reason(b) is None])

    @pytest.mark.parametrize("block", BLOCKS, ids=[b.id for b in BLOCKS])
    def test_block(block, results, baseline):
        if reason := skip_reason(block):
            pytest.skip(reason)
        ok, output = results[block.key]
        if block.key in baseline:
            if ok:
                pytest.fail(f"passes now: delete '{block.key}' from {BASELINE.name}")
            pytest.xfail("in baseline")
        assert ok, output

    def test_baseline_entries_match_blocks(baseline):
        stale = baseline - {b.key for b in BLOCKS}
        assert not stale, f"no such block (edited or removed); delete: {sorted(stale)}"

    def test_setup_blocks_only_in_references():
        misplaced = [b.id for b in BLOCKS if b.is_setup and not b.path.endswith("reference.md")]
        assert not misplaced, f"setup blocks are for *reference.md only: {misplaced}"

    HELP_BLOCKS = [(m.group(1), m.group(2)) for m in HELP.finditer(README.read_text())]

    @pytest.mark.parametrize("shown,text", HELP_BLOCKS, ids=[h[0] for h in HELP_BLOCKS])
    def test_readme_help_matches_binary(shown, text):
        if not help_binary(shown).exists():
            pytest.skip(f"{shown} not built")
        assert text == help_output(shown), "README is stale: python3 tests/test_docs.py --write-help"
