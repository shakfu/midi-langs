# Core stack-midi source files (without main.c for testing)
set(STACK_MIDI_CORE_SOURCES
    context.c
    stack.c
    primitives.c
    midi_core.c
    notation.c
    generative.c
    sequences.c
    packed_notes.c
    patterns.c
    scales.c
    recording.c
    dictionary.c
    interpreter.c
    readline_comp.c
    async_player.c
)

# Create a static library for unit testing
add_library(stack_midi_lib STATIC ${STACK_MIDI_CORE_SOURCES})
target_link_libraries(stack_midi_lib PUBLIC music_theory midi_file midi_open uv_a)
target_include_directories(stack_midi_lib PUBLIC ${CMAKE_SOURCE_DIR}/thirdparty/libuv/include)

# Main executable
add_executable(stack_midi main.c)
target_link_libraries(stack_midi PRIVATE stack_midi_lib)

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(stack_midi_lib PRIVATE -Wall -Wextra)
    target_compile_options(stack_midi PRIVATE -Wall -Wextra)
endif()

# Link with readline (optional on Windows)
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_link_libraries(stack_midi PRIVATE ${READLINE_LIB})
    target_link_libraries(stack_midi_lib PUBLIC ${READLINE_LIB})
    target_compile_definitions(stack_midi PRIVATE USE_READLINE)
    target_compile_definitions(stack_midi_lib PUBLIC USE_READLINE)
else()
    if(NOT WIN32)
        message(FATAL_ERROR "readline library not found (install libreadline-dev on Linux)")
    endif()
endif()

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(stack_midi_lib PUBLIC
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(stack_midi_lib PUBLIC ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(stack_midi)

install(TARGETS stack_midi RUNTIME DESTINATION bin)
