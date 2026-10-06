"""Check each docs/<lang>/api-reference.md against the names its interpreter defines.

Documented names must exist. Defined names must be documented, or listed in
tests/api_undocumented.txt. See docs/dev/doc-tests.md.
Regenerate the list with: python3 tests/test_api_docs.py --write-undocumented
"""

import os
import re
import subprocess
import sys
import tempfile
from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

ROOT = Path(__file__).resolve().parent.parent
BUILD = Path(os.environ.get("MIDI_LANGS_BUILD", ROOT / "build")).resolve()
UNDOCUMENTED = ROOT / "tests" / "api_undocumented.txt"
ENV = dict(os.environ, MIDI_LANGS_BACKEND="null")

LANGS = {
    "stack-midi": ["stack_midi", "--no-sleep", "--script"],
    "lua-midi": ["lua_midi"],
    "pktpy-midi": ["pktpy_midi"],
    "s7-midi": ["s7_midi"],
    "guile-midi": ["guile_midi"],
}
EXT = {"stack-midi": ".stk", "lua-midi": ".lua", "pktpy-midi": ".py", "s7-midi": ".scm", "guile-midi": ".scm"}

# Commands the stack-midi REPL handles itself; scripts do not have them
REPL_ONLY = {"stack-midi": {"quit"}}

# Pitches, dynamics, durations and scale constants are documented as groups
CONSTANT = re.compile(
    r"^([a-g](s|b|#)?-?\d|ppp|pp|p|mp|mf|f|ff|fff|whole|half|quarter|eighth|sixteenth"
    r"|scale-.*|SCALE_.*|scales.*)$"
)


def run(lang: str, code: str) -> str:
    with tempfile.TemporaryDirectory() as tmp:
        script = Path(tmp) / f"probe{EXT[lang]}"
        script.write_text(code)
        cmd = LANGS[lang]
        r = subprocess.run(
            [str(BUILD / cmd[0]), *cmd[1:], str(script)],
            cwd=tmp, env=ENV, stdin=subprocess.DEVNULL,
            capture_output=True, text=True, timeout=30,
        )
    return r.stdout + r.stderr


def reference(lang: str) -> str:
    # `\|` is a literal | inside a Markdown table cell
    return (ROOT / "docs" / lang / "api-reference.md").read_text().replace("\\|", "|")


def documented(lang: str) -> list[str]:
    """Names the reference documents: table rows for stack-midi, ### headings otherwise."""
    text = reference(lang)
    names = []
    if lang == "stack-midi":
        for row in re.findall(r"^\| `([^`]+)`", text, re.M):
            word = row.split()[0]
            if "=" not in word:  # vel=100 and the like are notation
                names.append(word)
        return names
    for heading in re.findall(r"^### (.+)$", text, re.M):
        parts = [p.strip() for p in heading.replace(" (helper)", "").split(" / ")]
        if any(" " in p for p in parts):
            continue  # prose heading
        prefix = re.match(r"^(\w+[.:])", parts[0])
        for p in parts:
            names.append(p if prefix is None or re.match(r"^\w+[.:]", p) else prefix.group(1) + p)
    return names


def missing(lang: str, names: list[str]) -> list[str]:
    """Documented names the interpreter does not define."""
    names = [n for n in names if n not in REPL_ONLY.get(lang, set())]
    if lang == "stack-midi":
        def unknown(word):
            return f"Unknown word: {word}" in run(lang, word + "\n")
        with ThreadPoolExecutor(os.cpu_count()) as pool:
            return [w for w, u in zip(names, pool.map(unknown, names)) if u]
    if lang == "lua-midi":
        probe = "m = midi.open()\n" + "".join(
            f'do local o = _G; for k in ("{n}"):gsub(":", "."):gmatch("[^.]+") do '
            f'if k == "m" and o == _G then o = m else o = o[k] end; if o == nil then break end end; '
            f'if o == nil then print("MISSING {n}") end end\n'
            for n in names
        )
    elif lang == "pktpy-midi":
        probe = "import midi\nm = midi.open()\n" + "".join(
            f'if not hasattr({"m" if n.startswith("MidiOut.") else "midi"}, "{n.split(".", 1)[1]}"): print("MISSING {n}")\n'
            for n in names
        )
    else:
        probe = "".join(f'(if (not (defined? (quote {n}))) (begin (display "MISSING {n}") (newline)))\n' for n in names)
    return re.findall(r"^MISSING (\S+)$", run(lang, probe), re.M)


def defined(lang: str) -> set[str]:
    """Public names the interpreter defines."""
    if lang == "stack-midi":
        return set(run(lang, "words\n").split()[2:])  # after "Words (N):"
    if lang == "lua-midi":
        out = run(lang, 'm = midi.open()\nfor k in pairs(midi) do print("midi." .. k) end\n'
                        'for k in pairs(getmetatable(m).__index) do print("m:" .. k) end\n')
    elif lang == "pktpy-midi":
        out = run(lang, 'import midi\nm = midi.open()\nfor k in dir(midi): print("midi." + k)\n'
                        'for k in dir(m): print("MidiOut." + k)\n')
    else:
        # The Scheme global environment also holds the whole language, so take
        # the names this project registers: C bindings and top-level prelude defines.
        src = "".join((ROOT / "projects" / lang / f).read_text() for f in ("midi_module.c", "scheduler.c", "prelude.scm"))
        return set(re.findall(r'(?:s7_define_function\(sc,|scm_c_define_gsubr\()\s*"([^"]+)"', src)) | set(
            re.findall(r"^\(define \(([^\s)]+)", src, re.M)
        )
    return {n for n in out.split() if not re.split(r"[.:]", n)[-1].startswith("_")}


def mentioned(lang: str) -> set[str]:
    """Every token in the reference's code spans, code blocks and headings."""
    text = reference(lang)
    chunks = re.findall(r"`([^`\n]+)`", text) + re.findall(r"^```[^\n]*\n(.*?)^```", text, re.M | re.S)
    chunks += re.findall(r"^#+ (.+)$", text, re.M)
    tokens = set()
    for chunk in chunks:
        for tok in chunk.split():
            tokens.add(tok)
            tokens.update(re.findall(r"[\w?!*<>=\-]+", tok))  # midi.open() -> midi, open
    return tokens


def undocumented(lang: str) -> set[str]:
    men = mentioned(lang)
    return {
        n for n in defined(lang)
        if not CONSTANT.match(re.split(r"[.:]", n)[-1])
        and n not in men and re.split(r"[.:]", n)[-1] not in men
    }


def read_allowlist() -> set[tuple[str, str]]:
    if not UNDOCUMENTED.exists():
        return set()
    entries = set()
    for line in UNDOCUMENTED.read_text().splitlines():
        fields = line.split("#", 1)[0].split()
        if len(fields) == 2:
            entries.add((fields[0], fields[1]))
    return entries


def built(lang: str) -> bool:
    return (BUILD / LANGS[lang][0]).exists()


def write_allowlist() -> None:
    lines = [
        "# Defined names the API reference does not mention: <lang> <name>",
        "# Document a name, then delete its line. See docs/dev/doc-tests.md.",
    ]
    for lang in LANGS:
        if built(lang):
            lines += [f"{lang} {n}" for n in sorted(undocumented(lang))]
    UNDOCUMENTED.write_text("\n".join(lines) + "\n")
    print(f"{len(lines) - 2} names; wrote {UNDOCUMENTED.relative_to(ROOT)}")


if __name__ == "__main__":
    if sys.argv[1:] == ["--write-undocumented"]:
        write_allowlist()
    else:
        sys.exit(__doc__)
else:
    import pytest

    @pytest.fixture(params=list(LANGS))
    def lang(request):
        if not built(request.param):
            pytest.skip(f"{LANGS[request.param][0]} not built")
        return request.param

    def test_documented_names_exist(lang):
        assert not missing(lang, documented(lang))

    def test_defined_names_documented(lang):
        listed = {n for l, n in read_allowlist() if l == lang}
        found = undocumented(lang)
        assert not found - listed, f"document these, or list them in {UNDOCUMENTED.name}"
        assert not listed - found, f"documented or removed; delete from {UNDOCUMENTED.name}"
