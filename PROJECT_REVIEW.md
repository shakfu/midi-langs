# midi-langs Project Review

**Date:** 2025-12-30
**Scope:** Comprehensive review of architecture, implementations, testing, and documentation

---

## Executive Summary

**midi-langs** is a sophisticated polyglot MIDI programming project providing five distinct language implementations built around a common music theory library. The project enables composing and generating MIDI sequences using different programming paradigms: stack-based (Forth), imperative (Lua), functional (Haskell/MicroHs), object-oriented (Python/PocketPy), and Lisp-style (Scheme/s7).

**Key Metrics:**
- 5 language implementations
- 55 scales + 10 microtonal scales
- ~60,000 lines of C code
- 22 automated tests across implementations
- Cross-platform support (macOS, Linux, Windows)

---

## 1. Architecture Overview

### 1.1 Project Structure

```
midi-langs/
  projects/
    common/           # Shared music theory library (463 lines)
    forth-midi/       # Custom Forth interpreter (14 modules)
    lua-midi/         # Lua 5.5 FFI bindings
    mhs-midi/         # MicroHs/Haskell integration
    pktpy-midi/       # PocketPy Python bindings
    s7-midi/          # s7 Scheme bindings
  thirdparty/         # libremidi, MicroHs, Lua, s7, PocketPy
  tests/              # Shell-based test suites
  docs/               # Per-language documentation
  scripts/            # Build utilities (prelude2c.py)
```

### 1.2 Layered Architecture

```
+------------------+------------------+------------------+------------------+------------------+
|    forth-midi    |    lua-midi      |    mhs-midi      |   pktpy-midi     |    s7-midi       |
|   (Stack-based)  |   (Imperative)   |   (Functional)   |    (OOP)         |    (Lisp)        |
+------------------+------------------+------------------+------------------+------------------+
|                              FFI / Language Bindings Layer                                  |
+---------------------------------------------------------------------------------------------+
|                              music_theory.h/c (Common Library)                              |
|                       Pitch parsing, scales, chords, dynamics, durations                    |
+---------------------------------------------------------------------------------------------+
|                              libremidi (MIDI Backend)                                       |
|                       CoreMIDI (macOS) / ALSA (Linux) / WinMM (Windows)                     |
+---------------------------------------------------------------------------------------------+
```

### 1.3 Common Library (music_theory)

The 463-line shared library provides:

| Category | Contents |
|----------|----------|
| **Pitch Parsing** | `music_parse_pitch()` - "C4", "C#4", "Db5" to MIDI 0-127 |
| **Pitch Formatting** | `music_pitch_to_name()` - MIDI to note names |
| **Chord Types** | 11 types: major, minor, dim, aug, 7ths, sus2, sus4 |
| **Scales** | 55 twelve-tone equal temperament scales |
| **Microtonal** | 10 quarter-tone scales (cents-based) |
| **Dynamics** | DYN_PPP (16) through DYN_FFF (127) |
| **Durations** | Whole, half, quarter, eighth, sixteenth |
| **Constants** | NOTE_C0 through NOTE_G9 |

### 1.4 Prelude System

A distinctive design pattern used by lua-midi, s7-midi, and pktpy-midi:

1. Native language source files (`prelude.lua`, `prelude.scm`, `prelude.py`)
2. Converted to C headers at build time via `scripts/prelude2c.py`
3. Embedded in executables and loaded during initialization
4. Provides constants, helpers, and convenience functions

**Benefits:**
- Maintains native syntax highlighting in editors
- Easy to edit without C string escaping
- Separates music helpers from FFI code
- Build-time verification

---

## 2. Language Implementations

### 2.1 Comparison Matrix

| Attribute | forth-midi | lua-midi | mhs-midi | pktpy-midi | s7-midi |
|-----------|-----------|----------|----------|-----------|---------|
| **Paradigm** | Stack-based | Imperative | Functional | Object-oriented | Lisp/Functional |
| **Type System** | Dynamic | Dynamic | Static | Dynamic | Dynamic |
| **Binary Size** | 495 KB | 674 KB | 745 KB | 1.0 MB | 1.8 MB |
| **Startup** | ~50ms | ~100ms | 1-2s | ~100ms | ~100ms |
| **Lines of Code** | 3,664 | 1,407 | 1,200+ | 1,306 | 1,241 |
| **Test Count** | 78 | 26 | 8 | 16 | 16 |

### 2.2 forth-midi (Custom Forth)

**Architecture:** 14 modular C files implementing a complete Forth interpreter

| Module | Purpose |
|--------|---------|
| `main.c` | Entry point, REPL loop |
| `interpreter.c` | Tokenizer, interpret/compile modes |
| `dictionary.c` | Word lookup and registration |
| `stack.c` | Stack operations |
| `primitives.c` | Arithmetic, bitwise, comparison |
| `notation.c` | Concise notation system |
| `generative.c` | PRNG, euclidean, shuffle, pick |
| `sequences.c` | Non-blocking sequence playback |
| `scales.c` | Scale operations (49 built-in) |
| `patterns.c` | Chord builders, duration helpers |
| `midi_core.c` | MIDI I/O, context defaults |
| `packed_notes.c` | 32-bit encoded notes |
| `recording.c` | Command/MIDI capture |
| `readline_comp.c` | Tab completion |

**Unique Features:**
- Concise notation: `c4,` plays middle C, `(c4 e4 g4),` plays chord
- Word definitions: `: melody c4, e4, g4, ;`
- Anonymous blocks: `{ c4, e4, } 4 *`
- Probability: `c4 75%,` (75% chance)
- Alternatives: `c4|e4|g4,` (random selection)
- Recording/capture to files

### 2.3 lua-midi (Lua 5.5)

**Architecture:** FFI bindings with MidiOut userdata type

```lua
-- Example usage
local m = midi.open()
m:note("C4", 500)
m:chord({"C4", "E4", "G4"}, 1000)
m:close()

-- Or with convenience functions
open()
n("C4")
ch("C4", "E4", "G4")
```

**Unique Features:**
- Full Lua 5.5 language (tables, closures, metatables)
- `__gc` metamethod for automatic cleanup
- Comprehensive readline autocomplete
- Microtonal support via pitch bend

### 2.4 mhs-midi (MicroHs/Haskell)

**Architecture:** Pure functional with strict IO separation

| Module | Purpose |
|--------|---------|
| `Music.hs` | Pure music DSL (no IO) |
| `Midi.hs` | FFI bindings only |
| `MusicPerform.hs` | Bridge from Music to MIDI |
| `MidiPerform.hs` | Immediate IO for REPL |

**Unique Features:**
- Pure composition: `melody = c4 +:+ e4 +:+ g4` then `perform melody`
- Parallel playback: `melody ||| bass`
- Type-safe transformations: `transpose 12`, `louder 20`
- Seed-based reproducible randomness

### 2.5 pktpy-midi (PocketPy)

**Architecture:** Embedded Python interpreter with context manager support

```python
# Context manager pattern
with midi.open() as m:
    m.note("C4", 500)
    m.chord(["C4", "E4", "G4"], 1000)

# Convenience functions
open()
n("C4")
```

**Unique Features:**
- Python 3.x compatibility via PocketPy (941 KB embedded)
- Context manager support (`with` statement)
- Pythonic API design

### 2.6 s7-midi (s7 Scheme)

**Architecture:** Lisp dialect with macros and closures

```scheme
(define m (midi-open))
(note m "C4" 500)
(chord m '("C4" "E4" "G4") 1000)
(midi-close m)
```

**Unique Features:**
- Full s7 Scheme dialect
- Macro system for custom notation
- First-class functions for musical abstractions

---

## 3. Feature Matrix

| Feature | forth | lua | mhs | pktpy | s7 |
|---------|:-----:|:---:|:---:|:-----:|:--:|
| **Core MIDI** |||||
| Note on/off | Y | Y | Y | Y | Y |
| Chords | Y | Y | Y | Y | Y |
| Pitch bend | Y | Y | Y | Y | Y |
| CC messages | Y | Y | Y | Y | Y |
| Program change | Y | Y | Y | Y | Y |
| **Music Theory** |||||
| 55 scales | Y | Y | Y | Y | Y |
| 10 microtonal | - | Y | Y | Y | Y |
| Chord builders | Y | Y | Y | Y | Y |
| **Generative** |||||
| Probability | Y | via Lua | Y | via Python | via Scheme |
| Random selection | Y | via Lua | Y | via Python | via Scheme |
| Random walks | Y | via Lua | Y | via Python | via Scheme |
| Euclidean rhythms | Y | via Lua | Y | via Python | via Scheme |
| **Interaction** |||||
| REPL | Y | Y | Y | Y | Y |
| Readline | Y | Y | built-in | Y | Y |
| Tab completion | basic | comprehensive | built-in | comprehensive | comprehensive |
| File execution | Y | Y | Y | Y | Y |
| **Advanced** |||||
| Word/function defs | Y | Y | Y | Y | Y |
| Sequences | explicit | via language | via Haskell | via language | via language |
| Command recording | Y | - | - | - | - |
| MIDI capture/export | Y | - | - | - | - |
| Virtual ports | Y | Y | Y | Y | Y |

---

## 4. Build System

### 4.1 CMake Structure

```cmake
cmake_minimum_required(VERSION 3.16)
project(midi_langs_project VERSION 1.0.0 LANGUAGES C CXX)
```

**Targets:**
- `libremidi` - MIDI backend (auto-built from thirdparty)
- `music_theory` - Shared library
- `forth_midi`, `lua_midi`, `mhs-midi`, `pktpy_midi`, `s7_midi` - Executables

### 4.2 Make Commands

| Command | Description |
|---------|-------------|
| `make` | Build everything |
| `make test` | Run all tests |
| `make test-quick` | Run quick smoke tests |
| `make clean` | Remove build directory |
| `make preludes` | Regenerate prelude headers |
| `make help` | Show all targets |

### 4.3 Dependencies

| Dependency | Version | Size | Purpose |
|------------|---------|------|---------|
| libremidi | v5.3.1 | ~4 MB | MIDI I/O backend |
| Lua | 5.5.0 | ~1.2 MB | Lua interpreter |
| s7 | latest | ~1.5 MB | Scheme interpreter |
| PocketPy | v2.1.6 | 941 KB | Python interpreter |
| MicroHs | latest | ~50 MB | Haskell compiler |

---

## 5. Testing

### 5.1 Test Coverage

| Implementation | Tests | Lines | Type |
|----------------|-------|-------|------|
| forth-midi | 78 | 488 | Shell script, comprehensive |
| lua-midi | 26 | 215 | Shell script, functional |
| s7-midi | 16 | 222 | Shell script, expression-based |
| pktpy-midi | 16 | 296 | Shell script, context manager |
| mhs-midi | 8 | 38 | Shell script, minimal |

### 5.2 Test Categories

**Quick Tests (labeled "quick"):**
- Smoke tests (basic functionality)
- Pitch parsing
- Arithmetic operations
- Note constants
- Port management

**Integration Tests (labeled "integration"):**
- Full test suites
- MIDI command sequences
- Complex expressions

### 5.3 Running Tests

```bash
make test                                    # All tests
ctest --test-dir build -L quick              # Quick only
ctest --test-dir build -R forth_midi         # Specific implementation
ctest --test-dir build --output-on-failure   # Verbose on failure
```

### 5.4 Test Quality Assessment

**Strengths:**
- Good coverage for forth-midi (78 tests)
- Integration-level testing with actual MIDI commands
- Language-specific edge cases covered

**Gaps:**
- No property-based testing
- No MIDI output verification (black box only)
- No performance benchmarks
- mhs-midi has minimal coverage (8 tests)

---

## 6. Documentation

### 6.1 Structure

```
docs/
  forth-midi/
    README.md, tutorial.md, syntax.md, scales.md,
    sequences.md, data_structures.md, new_features.md
  lua-midi/
    README.md, api-reference.md, examples.md
  mhs-midi/
    README.md, api-reference.md, examples.md
  pktpy-midi/
    README.md, api-reference.md, examples.md
  s7-midi/
    README.md, api-reference.md, examples.md, architecture.md
```

### 6.2 Documentation Quality

**Strengths:**
- Comprehensive coverage for each language
- Consistent structure across implementations
- Good examples (quick start, basic, advanced)
- Architecture documentation for s7-midi

**Gaps:**
- No cross-language comparison guide
- No video tutorials
- Limited advanced composition patterns
- No troubleshooting guide
- No performance profiling documentation

---

## 7. Code Quality Assessment

### 7.1 Strengths

1. **Consistent Patterns:** All FFI implementations follow similar structure
2. **Modular Architecture:** Clear separation between language runtime and MIDI
3. **Feature Parity:** All languages support core MIDI + scales
4. **Well-Commented:** FFI code has good documentation
5. **Prelude System:** Elegant solution for language-native code
6. **Error Handling:** Proper bounds checking, null safety
7. **Platform Abstraction:** libremidi handles OS differences cleanly
8. **Active Maintenance:** Recent commits show ongoing development

### 7.2 Areas for Improvement

1. **Testing:**
   - Add property-based tests
   - Implement MIDI output verification
   - Create performance benchmarks
   - Increase mhs-midi test coverage

2. **Documentation:**
   - Create language comparison guide
   - Add troubleshooting section
   - Document performance characteristics
   - Add advanced composition tutorials

3. **Features:**
   - MIDI capture/recording only in forth-midi - other implementations lack `save-midi` functionality
   - MIDI input support (currently output only)
   - Timeline-based sequencing

4. **Code:**
   - Consistent naming conventions across FFI layers
   - Prelude validation during build

---

## 8. Recommendations

### 8.2 Medium Priority

1. **Implement MIDI output verification tests** - Currently no output verification
2. **Add property-based testing** - Especially for pitch parsing and scale generation
3. **Create performance benchmarks** - Startup time, throughput, memory usage
4. **Add troubleshooting documentation** - Common issues and solutions

### 8.3 Low Priority

1. **MIDI input support** - Would enable recording from external controllers
2. **Timeline-based sequencing** - More intuitive for complex compositions

---

## 9. Conclusion

**midi-langs** is a well-engineered, comprehensive MIDI programming project that successfully demonstrates how five different programming paradigms can integrate with the same MIDI backend. The project excels in:

- **Breadth:** 55 scales, 10 microtonal scales, all core MIDI features
- **Quality:** Professional code organization, good documentation, active maintenance
- **Pragmatism:** All implementations are functional and usable
- **Innovation:** Unique design patterns (prelude system, concise notation, pure/IO separation)
- **Educational Value:** Demonstrates FFI patterns for 5 different language families

The main opportunities for enhancement are in testing (property-based tests, output verification), documentation (comparison guide, troubleshooting), and features (MIDI input, file export).

---

## Appendix A: Scale Categories

### A.1 Twelve-Tone Equal Temperament (55 scales)

**Diatonic Modes (9):**
Ionian/Major, Dorian, Phrygian, Lydian, Mixolydian, Aeolian/Minor, Locrian

**Minor Variants (3):**
Harmonic Minor, Melodic Minor, Harmonic Major

**Pentatonic (5):**
Major Pentatonic, Minor Pentatonic, Blues Pentatonic, Neutral Pentatonic, Rock Pentatonic

**Symmetric (5):**
Whole Tone, Chromatic, Diminished (H-W), Diminished (W-H), Augmented

**Bebop (3):**
Bebop Dominant, Bebop Major, Bebop Minor

**Blues (2):**
Blues, Blues Major

**World Scales (8):**
Hungarian Minor, Byzantine/Double Harmonic, Spanish Phrygian, Persian, Enigmatic, Japanese (In), Egyptian, Romanian Minor

**Arabic Maqamat - 12-TET (6):**
Hijaz, Nahawand, Nikriz, Athar Kurd, Shawq Afza, Jiharkah

**Indian Ragas - 12-TET (11):**
Bhairav, Todi, Marwa, Purvi, Charukeshi, Asavari, Bilawal, Khamaj, Kalyan, Bhimpalasi, Darbari Kanada

### A.2 Microtonal Scales (10)

**Arabic Maqamat - Quarter-tone (7):**
Bayati, Rast, Saba, Sikah, Huzam, Iraq, Bastanikar

**Turkish Makamlar (2):**
Ussak, Huseyni

**Indian (1):**
Shruti (22-shruti scale)

---

## Appendix B: Binary Sizes

| Executable | Size | Notes |
|------------|------|-------|
| forth_midi | 495 KB | Custom interpreter |
| lua_midi | 674 KB | Embedded Lua 5.5 |
| mhs-midi | 745 KB | MicroHs REPL |
| pktpy_midi | 1.0 MB | Embedded PocketPy |
| s7_midi | 1.8 MB | Embedded s7 |

---

*Generated by Claude Code review on 2025-12-30*
