# Alda Reference for the Examples

`<example>.expected` records what Alda 2.4.7 produces for each score in
`../examples/`, in the format described in `../shared_suite/README.md`.
`alda_conformance_examples` (CTest) compares alda-midi's output with it; see
`../conformance.md`.

The files are copied from psnd (`source/langs/alda/tests/alda_reference/`),
which writes them from `alda export` output committed in aldakit
(`tests/alda_reference/`). Do not edit them by hand. To regenerate, from an
aldakit checkout:

```sh
python scripts/gen_shared_suite.py --examples PATH/TO/midi-langs/docs/alda-midi/alda_reference
```

A new example needs an Alda export first, which needs Alda installed:
`make alda-diff` in aldakit, after copying the score into its `examples/`.
