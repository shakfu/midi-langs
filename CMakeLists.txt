cmake_minimum_required(VERSION 3.16)
project(midi_forth_suite VERSION 1.0.0 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

# Output executables to build directory root
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Build libremidi as a subdirectory
set(LIBREMIDI_EXAMPLES OFF CACHE BOOL "" FORCE)
set(LIBREMIDI_TESTS OFF CACHE BOOL "" FORCE)
add_subdirectory(thirdparty/libremidi)

# Add project subdirectories
add_subdirectory(projects/forth)
add_subdirectory(projects/midi_forth)
add_subdirectory(projects/mhs_midi)
add_subdirectory(projects/pktpy_midi)
add_subdirectory(projects/s7_midi)

# Enable testing
enable_testing()
add_subdirectory(tests)
