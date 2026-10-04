cmake_minimum_required(VERSION 3.16)
project(midi_langs_project VERSION 0.2.0 LANGUAGES C CXX)

set(CMAKE_C_STANDARD 11)
set(CMAKE_CXX_STANDARD 17)

# Default to Release build if not specified
if(NOT CMAKE_BUILD_TYPE)
    set(CMAKE_BUILD_TYPE Release CACHE STRING "Build type" FORCE)
endif()

option(BUILD_EXAMPLES "Build language standalone examples" OFF)
option(BUILD_MHS_MIDI "Build mhs-midi targets (requires MicroHs)" ON)
# Option to enable address and undefined behavior sanitizers
option(ENABLE_SANITIZERS "Enable AddressSanitizer and UndefinedBehaviorSanitizer" OFF)

if(ENABLE_SANITIZERS)
    message(STATUS "Sanitizers enabled")
    if(MSVC)
        # MSVC sanitizer flags
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} /fsanitize=address")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} /fsanitize=address")
    else()
        set(SANITIZER_FLAGS "-fsanitize=address,undefined -fno-omit-frame-pointer")
        set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${SANITIZER_FLAGS}")
        set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${SANITIZER_FLAGS}")
        set(CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} ${SANITIZER_FLAGS}")
    endif()
endif()

# Optimization flags for Release mode
if(MSVC)
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} /O2")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} /O2")
else()
    set(CMAKE_C_FLAGS_RELEASE "${CMAKE_C_FLAGS_RELEASE} -O2")
    set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O2")
endif()

if(UNIX AND NOT APPLE)
    # Linux: strip debug symbols via linker
    add_link_options($<$<CONFIG:Release>:-s>)
endif()

# Macro to add strip post-build step (for macOS)
macro(add_strip_command target)
    if(APPLE AND CMAKE_BUILD_TYPE STREQUAL "Release")
        add_custom_command(TARGET ${target} POST_BUILD
            COMMAND strip $<TARGET_FILE:${target}>
            COMMENT "Stripping ${target}"
        )
    endif()
endmacro()

# Output executables to build directory root
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR})

# Build libremidi as a subdirectory
set(LIBREMIDI_EXAMPLES OFF CACHE BOOL "" FORCE)
set(LIBREMIDI_TESTS OFF CACHE BOOL "" FORCE)
option(MIDI_LANGS_PIPEWIRE "Build libremidi's PipeWire backend" OFF)
if(MIDI_LANGS_PIPEWIRE)
    # Defining this target first stops libremidi from fetching readerwriterqueue at configure time
    add_library(readerwriterqueue INTERFACE)
    target_include_directories(readerwriterqueue INTERFACE ${CMAKE_SOURCE_DIR}/thirdparty/readerwriterqueue)
    set(LIBREMIDI_NO_PIPEWIRE OFF CACHE BOOL "" FORCE)
else()
    set(LIBREMIDI_NO_PIPEWIRE ON CACHE BOOL "" FORCE)
endif()
add_subdirectory(thirdparty/libremidi)
if(MIDI_LANGS_PIPEWIRE)
    get_target_property(_libremidi_defs libremidi INTERFACE_COMPILE_DEFINITIONS)
    if(NOT "LIBREMIDI_PIPEWIRE" IN_LIST _libremidi_defs)
        message(FATAL_ERROR "MIDI_LANGS_PIPEWIRE is ON but libremidi did not enable PipeWire (install libpipewire-0.3-dev)")
    endif()
endif()

# Build libuv as a subdirectory (for async event loop)
set(LIBUV_BUILD_SHARED OFF CACHE BOOL "" FORCE)
set(LIBUV_BUILD_TESTS OFF CACHE BOOL "" FORCE)
add_subdirectory(thirdparty/libuv)

# Build common music theory library
add_subdirectory(projects/common)

# Add project subdirectories
add_subdirectory(projects/stack-midi)
# pforth-midi has not been built on Windows yet
if(NOT WIN32)
    add_subdirectory(projects/pforth-midi)
endif()
if(BUILD_MHS_MIDI)
    add_subdirectory(projects/mhs-midi)
endif()
add_subdirectory(projects/pktpy-midi)
add_subdirectory(projects/s7-midi)
add_subdirectory(projects/lua-midi)
add_subdirectory(projects/alda-midi)
add_subdirectory(projects/joy-midi)
add_subdirectory(projects/guile-midi)

# ============================================
# Alias targets for consistent naming
# ============================================
# Provides hyphenated target names (e.g., stack-midi instead of stack_midi)

add_custom_target(stack-midi DEPENDS stack_midi)
if(TARGET pforth_midi)
    add_custom_target(pforth-midi DEPENDS pforth_midi)
endif()
add_custom_target(lua-midi DEPENDS lua_midi)
add_custom_target(pktpy-midi DEPENDS pktpy_midi)
add_custom_target(s7-midi DEPENDS s7_midi)
add_custom_target(alda-midi DEPENDS alda_midi)
add_custom_target(joy-midi DEPENDS joy_midi)

# guile-midi alias (conditional on Guile being found)
if(TARGET guile_midi)
    add_custom_target(guile-midi DEPENDS guile_midi)
endif()

# Convenience target to build all midi-langs
add_custom_target(all-midi-langs
    DEPENDS stack_midi lua_midi pktpy_midi s7_midi alda_midi joy_midi
)
# Add guile_midi to all-midi-langs if available
if(TARGET guile_midi)
    add_dependencies(all-midi-langs guile_midi)
endif()
if(TARGET pforth_midi)
    add_dependencies(all-midi-langs pforth_midi)
endif()
if(BUILD_MHS_MIDI)
    add_dependencies(all-midi-langs mhs-midi)
    if(NOT WIN32)
        # Standalone variants require fmemopen (not available on Windows)
        add_dependencies(all-midi-langs mhs-midi-src mhs-midi-src-zstd mhs-midi-pkg mhs-midi-pkg-zstd)
    endif()
endif()

# Enable testing
enable_testing()
add_subdirectory(tests)
