# Alda-MIDI
# C implementation of Alda music language with MIDI output

cmake_minimum_required(VERSION 3.15)
project(alda-midi C)

set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)

# Parser source files
set(ALDA_PARSER_SOURCES
    src/tokens.c
    src/error.c
    src/scanner.c
    src/ast.c
    src/parser.c
)

# Interpreter source files
set(ALDA_INTERPRETER_SOURCES
    src/context.c
    src/instruments.c
    src/midi_backend.c
    src/scheduler.c
    src/interpreter.c
    src/attributes.c
)

# Header files
set(ALDA_HEADERS
    include/alda/alda.h
    include/alda/tokens.h
    include/alda/error.h
    include/alda/scanner.h
    include/alda/ast.h
    include/alda/parser.h
    include/alda/context.h
    include/alda/instruments.h
    include/alda/midi_backend.h
    include/alda/scheduler.h
    include/alda/interpreter.h
)

# Create the core library (parser + interpreter)
add_library(alda-core STATIC
    ${ALDA_PARSER_SOURCES}
    ${ALDA_INTERPRETER_SOURCES}
    ${ALDA_HEADERS}
)

# Set include directories
target_include_directories(alda-core
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
)

# Link libremidi
target_link_libraries(alda-core PUBLIC libremidi)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(alda-core PUBLIC
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX AND NOT APPLE)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(alda-core PUBLIC ${ALSA_LIBRARIES})
        target_include_directories(alda-core PRIVATE ${ALSA_INCLUDE_DIRS})
    endif()
endif()

# Compiler warnings
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(alda-core PRIVATE
        -Wall -Wextra -Wpedantic
    )
endif()

# Main executable
add_executable(alda_midi src/main.c)
target_link_libraries(alda_midi PRIVATE alda-core)

# Installation rules
install(TARGETS alda-core alda_midi
    RUNTIME DESTINATION bin
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

install(DIRECTORY include/alda
    DESTINATION include
)
