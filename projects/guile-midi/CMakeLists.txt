# Guile MIDI - Scheme MIDI language using GNU Guile

find_package(PkgConfig QUIET)

if(PkgConfig_FOUND)
    pkg_check_modules(GUILE guile-3.0)
endif()

if(NOT GUILE_FOUND)
    message(STATUS "Guile not found - skipping guile_midi")
    return()
endif()

message(STATUS "Found Guile: ${GUILE_VERSION}")

add_executable(guile_midi
    main.c
    midi_module.c
    scheduler.c
)

target_include_directories(guile_midi PRIVATE
    ${GUILE_INCLUDE_DIRS}
    ${CMAKE_SOURCE_DIR}/thirdparty/libuv/include
)

# Compiler warnings (GCC/Clang only)
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(guile_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()

# Check for readline (optional on Windows)
find_library(READLINE_LIB readline)
if(READLINE_LIB)
    target_compile_definitions(guile_midi PRIVATE USE_READLINE)
    target_link_libraries(guile_midi PRIVATE ${READLINE_LIB})
else()
    if(NOT WIN32)
        message(WARNING "readline library not found for guile_midi - REPL will have limited editing")
    endif()
endif()

# Add Guile library directories (needed for linker to find guile-3.0)
target_link_directories(guile_midi PRIVATE ${GUILE_LIBRARY_DIRS})

# Option to link Guile statically (default ON for macOS)
if(APPLE)
    option(GUILE_STATIC "Link Guile statically" ON)
else()
    option(GUILE_STATIC "Link Guile statically" OFF)
endif()

if(GUILE_STATIC)
    # Static linking of Guile and its dependencies where possible
    # Note: Some system libraries (libffi, iconv) remain dynamic on macOS

    # Find static libraries
    find_library(GUILE_STATIC_LIB libguile-3.0.a PATHS ${GUILE_LIBRARY_DIRS} NO_DEFAULT_PATH)
    find_library(GMP_STATIC_LIB libgmp.a PATHS /opt/homebrew/opt/gmp/lib NO_DEFAULT_PATH)
    find_library(GC_STATIC_LIB libgc.a PATHS /opt/homebrew/opt/bdw-gc/lib NO_DEFAULT_PATH)
    find_library(UNISTRING_STATIC_LIB libunistring.a PATHS /opt/homebrew/opt/libunistring/lib NO_DEFAULT_PATH)

    if(NOT GUILE_STATIC_LIB)
        message(FATAL_ERROR "Static libguile-3.0.a not found")
    endif()

    message(STATUS "Static Guile: ${GUILE_STATIC_LIB}")
    message(STATUS "Static GMP: ${GMP_STATIC_LIB}")
    message(STATUS "Static GC: ${GC_STATIC_LIB}")
    message(STATUS "Static unistring: ${UNISTRING_STATIC_LIB}")

    target_link_libraries(guile_midi PRIVATE
        ${GUILE_STATIC_LIB}
        ${GMP_STATIC_LIB}
        ${GC_STATIC_LIB}
        ${UNISTRING_STATIC_LIB}
        ffi          # Dynamic - no static available on macOS
        iconv        # Dynamic - system library
        music_theory
        midi_file
        uv_a
    )
else()
    # Dynamic linking (default)
    target_link_libraries(guile_midi PRIVATE
        ${GUILE_LIBRARIES}
        music_theory
        midi_file
        uv_a
    )
endif()

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(guile_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    # Math library needed on Unix
    target_link_libraries(guile_midi PRIVATE m)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(guile_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

add_strip_command(guile_midi)

install(TARGETS guile_midi RUNTIME DESTINATION bin)
