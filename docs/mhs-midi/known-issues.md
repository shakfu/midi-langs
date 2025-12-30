# Known Issues - mhs-midi

## MicroHs Parallel Build Cache Corruption (Fixed)

**Status:** Fixed in CMakeLists.txt

### Problem

When building with parallel jobs (`make -j4` or similar), multiple MicroHs compilations could run simultaneously. All MicroHs compilations share a single `.mhscache` file in the working directory, and concurrent access corrupts this file, causing build failures:

```
/Users/.../thirdparty/MicroHs/bin/mhs: uncaught exception: Handle(.mhscache): end of file (hGetChar)
```

### Solution

The CMakeLists.txt now includes explicit dependencies between MicroHs example targets to ensure they build sequentially:

```cmake
# Serialize MicroHs compilations to avoid .mhscache corruption
add_dependencies(chords hello_midi)
add_dependencies(melody_example chords)
add_dependencies(arpeggio melody_example)
add_dependencies(list_ports arpeggio)
```

This creates a build chain: `hello_midi -> chords -> melody_example -> arpeggio -> list_ports`

### Recovery

If you encounter cache corruption (e.g., after interrupting a build), clear the cache:

```bash
make reset   # Removes .mhscache files recursively
make         # Rebuild
```

Or manually:

```bash
rm -f projects/mhs-midi/.mhscache
```
