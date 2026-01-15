# Joy-MIDI - Joy interpreter with MIDI extensions

# Main executable
add_executable(joy_midi
    main.c
    joy_midi.c
    midi_primitives.c
    music_context.c
    music_notation.c
)

# Link against pyjoy_runtime
target_link_libraries(joy_midi PRIVATE pyjoy_runtime music_theory libremidi)

# Include paths
target_include_directories(joy_midi PRIVATE
    ${CMAKE_SOURCE_DIR}/thirdparty/pyjoy-runtime
    ${CMAKE_SOURCE_DIR}/projects/common
    ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
)

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(joy_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Platform-specific dependencies for MIDI
if(APPLE)
    target_link_libraries(joy_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(joy_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(joy_midi)

install(TARGETS joy_midi RUNTIME DESTINATION bin)
