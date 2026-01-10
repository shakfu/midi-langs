# alda_midi CLI - Alda music language REPL and file player

add_executable(alda_midi src/main.c)

# Link against libalda
target_link_libraries(alda_midi PRIVATE alda)

# Check for readline (optional on Windows)
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(alda_midi PRIVATE USE_READLINE)
    target_link_libraries(alda_midi PRIVATE ${READLINE_LIB})
else()
    if(NOT WIN32)
        message(FATAL_ERROR "readline library not found (install libreadline-dev on Linux)")
    endif()
endif()

# Strip release builds
if(COMMAND add_strip_command)
    add_strip_command(alda_midi)
endif()

# Installation rules
install(TARGETS alda_midi
    RUNTIME DESTINATION bin
)
