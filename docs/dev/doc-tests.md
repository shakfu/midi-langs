# Doc tests

`tests/test_docs.py` runs every fenced code block in `README.md` and `docs/`. A block passes when its interpreter exits 0. It also checks that each `% ./binary --help` block in `README.md` matches the binary. `tests/test_api_docs.py` checks each `api-reference.md` against its interpreter. CTest runs both as `docs_examples`.

Run one doc:

```sh
uv run --no-project --with pytest pytest -q tests/test_docs.py -k 'lua-midi and api-reference'
```

## Interpreter

The first rule that applies picks the interpreter:

1. Under `docs/<interpreter>/`: that interpreter.
2. In `README.md`: the enclosing `### <interpreter>` section.
3. Otherwise, the fence language: `alda`, `forth` (stack-midi), `joy`, `lua`, `python` (pktpy-midi), `scheme` (s7-midi).

Other fence languages (`sh`, `text`, `c`, `haskell`) are not run. These paths are not collected:

- `docs/dev/`
- `docs/mhs-midi/`, until the MicroHs upgrade
- the vendored Alda docs under `docs/alda-midi/`
- `TODO.md` files

## Fence flags

| Fence | Meaning |
|-|-|
| ` ```lua ` | Complete program. Runs alone. |
| ` ```lua setup ` | `*reference.md` only, such as `api-reference.md` and alda's `language-reference.md`. Prepended to every other `lua` block in the file, and also run alone. |
| ` ```lua norun ` | Not run: signatures, listings, REPL transcripts, examples that need a hardware port, proposed features. |

Tutorials, examples and READMEs hold complete programs. Reference entries may be fragments, with the context they need in the file's setup block. The setup block stays visible, so a reader can rebuild the same context.

## Environment

Each block runs with `MIDI_LANGS_BACKEND=null` and `--no-sleep` (except pforth-midi, which has no such flag), in its own temporary directory, with stdin closed, for at most 30 s.

## Baseline

`tests/docs_baseline.txt` lists the blocks that failed when the test was added. An entry is a path and the SHA-1 of the block's content.

- A baselined block that fails is reported as xfail.
- A baselined block that passes fails the run. Delete its line.
- Editing a block changes its hash. The old entry fails the run as stale, and the edited block must pass.
- A new block must pass.

`python3 tests/test_docs.py --write-baseline` rewrites the file from a fresh run. It adds failures as well as removing fixed ones, so review the diff. It skips interpreters that are not built, and wisp blocks when Guile lacks `(language wisp)`, which ships with Guile 3.0.10.

## README help blocks

`python3 tests/test_docs.py --write-help` replaces each `--help` block in `README.md` with the binary's output.

## API reference coverage

`tests/test_api_docs.py` reads the documented names from `api-reference.md`: `###` headings, or the first column of stack-midi's tables.

- Every documented name must exist in the interpreter. The test asks the running interpreter, so prelude definitions and aliases count.
- Every name the interpreter defines must appear in the reference: in a heading, a code span or a code block. Pitch, dynamic, duration and scale constants are exempt.

`tests/api_undocumented.txt` lists the defined names that the reference did not mention when the test was added. It works like the baseline: a new undocumented name fails the run, and so does a listed name that is now documented or removed. `python3 tests/test_api_docs.py --write-undocumented` rewrites it.

