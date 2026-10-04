# Alda Conformance

alda-midi is correct when it produces the MIDI that Alda 2.4.7 produces (`alda export`). The interpreter is ported from psnd (`~/projects/psnd/source/langs/alda/impl/`, psnd 0.4.0). psnd's `docs/dev/conformance.md` lists each cause found and its fix (P1-P19).

## Status

All 60 scores match. Two CTest tests check this:

| Test | Scores | Expected values |
|-|-|-|
| `alda_midi_test_suite` | `shared_suite/*.alda` | `shared_suite/*.expected` |
| `alda_conformance_examples` | `examples/*.alda` | `alda_reference/*.expected` |

Both run `projects/alda-midi/tests/test_shared_suite.c`. It compares each note's pitch, start, duration and velocity, plus the program, pan (CC 10) and track volume (CC 11) on its channel when it starts. Timing tolerance is 10ms. Channel numbers are not compared.

The `.expected` files are copied from psnd and must not be edited by hand (`alda_reference/README.md`).

## Deliberate deviations

- **Channel numbers.** Alda picks a channel per note. alda-midi keeps one channel per part while parts fit. psnd's `docs/dev/conformance.md` gives the reasoning.

- **Errors.** Alda refuses a score with, for example, `(vol 120)` or an unknown attribute. alda-midi clamps or ignores such values and continues.

- **Suffix accidentals.** `cs` and `db` spell C sharp and D flat. Alda has no such syntax; there, `cs` could name a variable.

## Keeping it current

`scripts/sync_alda_from_psnd.py` vendors psnd's interpreter, unit tests and conformance data, and removes psnd's `SharedContext`, Scala and Csound fields. psnd is found at `$PSND_DIR` or `../psnd`.

```sh
scripts/sync_alda_from_psnd.py --check   # report drift, write nothing
scripts/sync_alda_from_psnd.py           # sync, then run make test
```

It stops with an error if an adapted file no longer matches its edits. It warns about files new upstream, files only in midi-langs, and changes to psnd's `scheduler.c`, whose playback loop midi-langs fixed separately.
