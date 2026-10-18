set(S7_DIR ${CMAKE_SOURCE_DIR}/thirdparty/s7)

# Interpreter and MIDI module without the REPL, shared by s7_midi and the CLAP plugin
add_library(s7_midi_core STATIC
    midi_module.c
    scheduler.c
    ${S7_DIR}/s7.c
)

target_include_directories(s7_midi_core PUBLIC
    ${S7_DIR}
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Compiler warnings and size optimization (GCC/Clang only)
# s7.c is very large, so -Os helps reduce binary size
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(s7_midi_core PRIVATE -Wall -Wextra -Wno-unused-parameter -Os)
endif()

# Disable some s7 features to reduce size
target_compile_definitions(s7_midi_core PUBLIC
    WITH_SYSTEM_EXTRAS=0
    WITH_C_LOADER=0
    DISABLE_AUTOLOAD=1
)

# s7 requires libuv for async scheduler
target_link_libraries(s7_midi_core PUBLIC music_theory midi_file midi_open uv_a)

add_executable(s7_midi main.c)
target_link_libraries(s7_midi PRIVATE s7_midi_core)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(s7_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Check for readline (optional on Windows)
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(s7_midi PRIVATE USE_READLINE)
    target_link_libraries(s7_midi PRIVATE ${READLINE_LIB})
else()
    if(NOT WIN32)
        message(FATAL_ERROR "readline library not found (install libreadline-dev on Linux)")
    endif()
endif()

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(s7_midi_core PUBLIC
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    # Math library and dl needed on Unix
    target_link_libraries(s7_midi_core PUBLIC dl m)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(s7_midi_core PUBLIC ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(s7_midi)

if(COMMAND midi_langs_add_clap)
    midi_langs_add_clap(s7_midi_clap
        NAME s7-midi
        CLASS_PREFIX MidiLangsS7Midi
        SOURCES s7_clap.cpp
        LIBRARIES s7_midi_core
    )
endif()

install(TARGETS s7_midi RUNTIME DESTINATION bin)
