# TODO

Ideas for common features and enhancements to all implementations.

## Critical

## High

## Medium

## Low

### Testing

- [ ] Property-based testing for Music transformations
- [ ] MIDI output verification (mock port)
- [ ] Performance benchmarks

### Documentation

- [ ] Video tutorials
- [ ] Troubleshooting guide (common issues and solutions)
- [ ] Document performance characteristics (startup time, memory usage)

### Integration

- [ ] OSC support for external control
- [ ] Ableton Link for tempo sync
- [ ] Jack audio connection kit support (Linux)
- [ ] MusicXML import/export (maybe [libmusicxml])(<https://github.com/grame-cncm/libmusicxml>) and [partitura](https://github.com/CPJKU/partitura))

### Enhance existing async features

- [ ] Inter-voice communication/sync primitives
- [ ] Async MIDI input handling

## Done

### Documentation

- [x] Example compositions - see `examples/` directory
- [x] Comparison guide (when to use which)

### Enhance existing async features

- [x] REPL responsiveness during run - added `poll()` to lua/pktpy/s7

### Unify/document async patterns

- [x] Create cross-language async examples - see `docs/async-models.md`
- [x] Document the different async models - see `docs/async-models.md`

## Polyphony and Sequencing cocurrent patterns
