# Joy-MIDI - Joy interpreter with MIDI extensions

# Include the core Joy interpreter library
add_subdirectory(joy)

# MIDI primitives and music notation without the REPL, shared by joy_midi and the CLAP plugin
add_library(joy_midi_core STATIC
    joy_midi.c
    midi_primitives.c
    music_context.c
    music_notation.c
)

# Link against joy_core (the standalone Joy interpreter)
target_link_libraries(joy_midi_core PUBLIC joy_core music_theory midi_open libremidi)

# Include paths
target_include_directories(joy_midi_core PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}/joy
    ${CMAKE_SOURCE_DIR}/projects/common
    ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
)

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(joy_midi_core PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Platform-specific dependencies for MIDI
if(APPLE)
    target_link_libraries(joy_midi_core PUBLIC
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(joy_midi_core PUBLIC ${ALSA_LIBRARIES})
    endif()
endif()

# Main executable
add_executable(joy_midi main.c)
target_link_libraries(joy_midi PRIVATE joy_midi_core)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(joy_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

add_strip_command(joy_midi)

# Copy prelude.joy to build directory next to executable
add_custom_command(TARGET joy_midi POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_CURRENT_SOURCE_DIR}/prelude.joy
        $<TARGET_FILE_DIR:joy_midi>/prelude.joy
    COMMENT "Copying prelude.joy to build directory"
)

install(TARGETS joy_midi RUNTIME DESTINATION bin)

if(COMMAND midi_langs_add_clap)
    # Embed prelude.joy: a DAW loads the plugin with no prelude.joy beside it.
    # A byte array, since MSVC limits string literals to 64 KB.
    set(JOY_PRELUDE ${CMAKE_CURRENT_SOURCE_DIR}/prelude.joy)
    set_property(DIRECTORY APPEND PROPERTY CMAKE_CONFIGURE_DEPENDS ${JOY_PRELUDE})
    file(READ ${JOY_PRELUDE} JOY_PRELUDE_HEX HEX)
    string(REGEX REPLACE "([0-9a-f][0-9a-f])" "0x\\1," JOY_PRELUDE_BYTES "${JOY_PRELUDE_HEX}")
    file(CONFIGURE OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/joy_prelude_data.cpp
        CONTENT "// Generated from prelude.joy by projects/joy-midi/CMakeLists.txt\nextern const char kJoyPrelude[] = {${JOY_PRELUDE_BYTES}0x00};\n")

    midi_langs_add_clap(joy_midi_clap
        NAME joy-midi
        CLASS_PREFIX MidiLangsJoyMidi
        SOURCES joy_clap.cpp ${CMAKE_CURRENT_BINARY_DIR}/joy_prelude_data.cpp
        LIBRARIES joy_midi_core
    )
endif()
install(FILES prelude.joy DESTINATION bin)
