# Interpreter and MIDI module without the REPL, shared by pktpy_midi and the CLAP plugin
add_library(pktpy_midi_core STATIC
    midi_module.c
    scheduler.c
    ${CMAKE_SOURCE_DIR}/thirdparty/pocketpy/pocketpy.c
)

target_include_directories(pktpy_midi_core PUBLIC
    ${CMAKE_SOURCE_DIR}/thirdparty/pocketpy
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# py_interrupt_hook (a midi-langs patch in pocketpy.c) lets the CLAP plugin's Stop end a script
target_compile_definitions(pktpy_midi_core PUBLIC PK_ENABLE_WATCHDOG=1)

# Disable pocketpy threading on Windows to avoid C11 atomics issues with MSVC
if(WIN32)
    target_compile_definitions(pktpy_midi_core PUBLIC PK_ENABLE_THREADS=0)
endif()

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(pktpy_midi_core PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Link with music_theory, midi_file, and libuv for async scheduler
target_link_libraries(pktpy_midi_core PUBLIC music_theory midi_file midi_open uv_a)

add_executable(pktpy_midi main.c)
target_link_libraries(pktpy_midi PRIVATE pktpy_midi_core)

if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(pktpy_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Check for readline (optional on Windows)
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(pktpy_midi PRIVATE USE_READLINE)
    target_link_libraries(pktpy_midi PRIVATE ${READLINE_LIB})
else()
    if(NOT WIN32)
        message(FATAL_ERROR "readline library not found (install libreadline-dev on Linux)")
    endif()
endif()

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(pktpy_midi_core PUBLIC
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(pktpy_midi_core PUBLIC ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(pktpy_midi)

if(COMMAND midi_langs_add_clap)
    midi_langs_add_clap(pktpy_midi_clap
        NAME pktpy-midi
        CLASS_PREFIX MidiLangsPktpyMidi
        SOURCES pktpy_clap.cpp
        LIBRARIES pktpy_midi_core
    )
endif()

install(TARGETS pktpy_midi RUNTIME DESTINATION bin)
