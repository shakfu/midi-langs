# libalda - Alda music language library
# Provides parsing, interpretation, and MIDI output

set(LIBALDA_SOURCES
    src/tokens.c
    src/error.c
    src/scanner.c
    src/ast.c
    src/parser.c
    src/context.c
    src/instruments.c
    src/midi_backend.c
    src/tsf_backend.c
    src/scheduler.c
    src/interpreter.c
    src/attributes.c
    src/async.c
)

set(LIBALDA_HEADERS
    include/alda/alda.h
    include/alda/tokens.h
    include/alda/error.h
    include/alda/scanner.h
    include/alda/ast.h
    include/alda/parser.h
    include/alda/context.h
    include/alda/instruments.h
    include/alda/midi_backend.h
    include/alda/tsf_backend.h
    include/alda/scheduler.h
    include/alda/interpreter.h
    include/alda/async.h
)

# Create the library
add_library(alda STATIC
    ${LIBALDA_SOURCES}
    ${LIBALDA_HEADERS}
)

# Alias for use in parent projects
add_library(alda::alda ALIAS alda)

# Set include directories
target_include_directories(alda
    PUBLIC
        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
        $<INSTALL_INTERFACE:include>
    PRIVATE
        ${CMAKE_SOURCE_DIR}/thirdparty/TinySoundFont
        ${CMAKE_SOURCE_DIR}/thirdparty/miniaudio
)

# Link dependencies
target_link_libraries(alda PUBLIC libremidi uv_a)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(alda PUBLIC
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
        "-framework AudioToolbox"
    )
elseif(UNIX AND NOT APPLE)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(alda PUBLIC ${ALSA_LIBRARIES})
        target_include_directories(alda PRIVATE ${ALSA_INCLUDE_DIRS})
    endif()
    # miniaudio needs pthread, dl, m on Linux
    target_link_libraries(alda PUBLIC pthread dl m)
endif()

# set_target_properties(
#     alda
#     PROPERTIES 
#     OUTPUT_NAME "alda"
#     ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
#     LIBRARY_OUTPUT_DIRECTORY "${CMAKE_BINARY_DIR}"
# )

# Compiler warnings
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(alda PRIVATE
        -Wall -Wextra -Wpedantic
    )
endif()

# Installation rules
install(TARGETS alda
    EXPORT aldaTargets
    ARCHIVE DESTINATION lib
    LIBRARY DESTINATION lib
)

install(DIRECTORY include/alda
    DESTINATION include
)

install(EXPORT aldaTargets
    FILE aldaTargets.cmake
    NAMESPACE alda::
    DESTINATION lib/cmake/alda
)
