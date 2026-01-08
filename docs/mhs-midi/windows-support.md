# Windows Support - mhs-midi

## Current Status

The `mhs-midi` and `mhs-midi-standalone` binaries are **not available on Windows** due to the use of `fmemopen()`, a POSIX function for creating in-memory file streams that has no direct Windows equivalent.

The Virtual Filesystem (VFS) used by these binaries relies on `fmemopen()` to serve embedded files to the MicroHs runtime.

## What Works on Windows

- **MicroHs compiler**: The MicroHs compiler itself works on Windows
- **Compiling to executables**: You can compile Haskell MIDI programs to standalone executables
- **libremidi**: The MIDI library supports Windows via WinMM

## Using MicroHs with MIDI on Windows

### Option 1: Build music.pkg and use standard mhs

Build the music package:

```powershell
cd thirdparty\MicroHs
nmake -f Makefile.windows
nmake -f Makefile.windows install
```

Create the music package:

```powershell
set MHSDIR=path\to\MicroHs
mhs -Pmusic-0.1.0 -iprojects\mhs-midi\lib Midi Music MusicPerform Async -o music-0.1.0.pkg
mhs -Q music-0.1.0.pkg %USERPROFILE%\.mcabal\mhs-X.Y.Z
```

Then use the installed `mhs` with the music package:

```powershell
mhs -pmusic
```

### Option 2: Compile programs to executables

Compile your Haskell MIDI program to a standalone executable:

```powershell
set MHSDIR=path\to\MicroHs
mhs -iprojects\mhs-midi\lib MyProgram -oMyProgram.c
cc MyProgram.c path\to\runtime\*.c -o MyProgram.exe
```

You'll need to link against:
- `libremidi` (build from thirdparty/libremidi)
- `winmm.lib` (Windows Multimedia API)

### Linking libremidi on Windows

Build libremidi as a static library:

```powershell
cd thirdparty\libremidi
cmake -B build -G "Visual Studio 17 2022"
cmake --build build --config Release
```

Link against `build\Release\libremidi.lib` and `winmm.lib`.

## Future Improvements

Potential approaches to enable Windows support:

1. **Implement fmemopen alternative**: Create a Windows-compatible implementation using `CreateFileMapping`/`MapViewOfFile` or temporary files
2. **Use a different VFS approach**: Modify the VFS to use a different mechanism that works cross-platform
3. **Compile-time only**: Focus on compiling Haskell to executables rather than providing a REPL

Contributions welcome.
