# Alda-MIDI
# C implementation of Alda music language with MIDI output
#
# Structure:
#   lib/  - libalda static library (parser, interpreter, MIDI backend)
#   cli/  - alda_midi executable (REPL and file player)
#   tests/ - test suite

cmake_minimum_required(VERSION 3.15)
project(alda-midi C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Build subdirectories
add_subdirectory(lib)
add_subdirectory(cli)

# Tests
enable_testing()
add_subdirectory(tests)
