# pforth-midi: pForth (thirdparty/pforth, unmodified) with MIDI words.
#
# pForth's own CMake writes its dictionary into its source tree, so this
# builds it here instead, entirely inside the build tree:
# 1. pforth_midi_builder: pForth plus the MIDI C words, loading a .dic file
# 2. builder -i system.fth       -> pforth.dic (pForth + C glue words)
# 3. builder mkdic.fth           -> pfdicdat.h (adds midi.fth)
# 4. pforth_midi: same sources with PF_STATIC_DIC, so no .dic file at runtime

set(PFORTH_DIR ${CMAKE_SOURCE_DIR}/thirdparty/pforth)
set(PFORTH_CSRC ${PFORTH_DIR}/csrc)

# Upstream's source list, minus its sample custom words (midi_words.c replaces them)
file(STRINGS ${PFORTH_CSRC}/sources.cmake PFORTH_LIST)
set(PFORTH_SOURCES)
foreach(f ${PFORTH_LIST})
    if(f MATCHES "\\.c$" AND NOT f STREQUAL "pfcustom.c")
        list(APPEND PFORTH_SOURCES ${PFORTH_CSRC}/${f})
    endif()
endforeach()

if(WIN32)
    list(APPEND PFORTH_SOURCES
        ${PFORTH_CSRC}/stdio/pf_fileio_stdio.c
        ${PFORTH_CSRC}/win32_console/pf_io_win32_console.c)
else()
    list(APPEND PFORTH_SOURCES
        ${PFORTH_CSRC}/stdio/pf_fileio_stdio.c
        ${PFORTH_CSRC}/posix/pf_io_posix.c
        pf_io_midi.c)
    # pf_io_midi.c wraps these two; see that file for why
    set_source_files_properties(${PFORTH_CSRC}/posix/pf_io_posix.c PROPERTIES
        COMPILE_DEFINITIONS "sdTerminalIn=pf_posix_sdTerminalIn;sdTerminalEcho=pf_posix_sdTerminalEcho")
endif()

set(PFORTH_MIDI_SOURCES
    ${PFORTH_SOURCES}
    midi_words.c
    # midi_ffi is mhs-midi's C MIDI layer; compiled in so pforth-midi does
    # not depend on BUILD_MHS_MIDI
    ${CMAKE_SOURCE_DIR}/projects/mhs-midi/midi_ffi.c
)

function(pforth_midi_core name)
    add_library(${name} STATIC ${PFORTH_MIDI_SOURCES})
    target_include_directories(${name} PUBLIC
        ${PFORTH_CSRC}
        ${CMAKE_SOURCE_DIR}/projects/mhs-midi
        ${CMAKE_SOURCE_DIR}/thirdparty/libremidi/include
    )
    target_compile_definitions(${name} PUBLIC PF_SUPPORT_FP)
    # pForth assumes signed char, as upstream builds it
    target_compile_options(${name} PRIVATE $<$<NOT:$<C_COMPILER_ID:MSVC>>:-fsigned-char>)
    target_link_libraries(${name} PUBLIC music_theory midi_file midi_open)
    if(APPLE)
        target_link_libraries(${name} PUBLIC
            "-framework CoreMIDI"
            "-framework CoreFoundation"
            "-framework CoreAudio")
    elseif(UNIX)
        target_link_libraries(${name} PUBLIC m)
        find_package(ALSA)
        if(ALSA_FOUND)
            target_link_libraries(${name} PUBLIC ${ALSA_LIBRARIES})
        endif()
    endif()
endfunction()

# 1. Builder: loads a .dic file
pforth_midi_core(pforth_midi_core)
add_executable(pforth_midi_builder ${PFORTH_CSRC}/pf_main.c)
target_link_libraries(pforth_midi_builder PRIVATE pforth_midi_core)
set_target_properties(pforth_midi_builder PROPERTIES
    RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR})

# 2. pforth.dic, built where the copied .fth files are
set(FTH_DIR ${CMAKE_CURRENT_BINARY_DIR}/fth)
file(MAKE_DIRECTORY ${FTH_DIR})
file(GLOB PFORTH_FTH ${PFORTH_DIR}/fth/*.fth)
add_custom_command(
    OUTPUT ${FTH_DIR}/pforth.dic
    COMMAND ${CMAKE_COMMAND} -E copy ${PFORTH_FTH} ${FTH_DIR}
    COMMAND $<TARGET_FILE:pforth_midi_builder> -q -i system.fth
    WORKING_DIRECTORY ${FTH_DIR}
    DEPENDS pforth_midi_builder ${PFORTH_FTH}
    COMMENT "Building pForth dictionary"
    VERBATIM
)

# 3. pfdicdat.h with midi.fth added
set(MIDI_FTH ${CMAKE_CURRENT_SOURCE_DIR}/midi.fth ${CMAKE_CURRENT_SOURCE_DIR}/mkdic.fth)
add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/pfdicdat.h
    COMMAND ${CMAKE_COMMAND} -E copy ${MIDI_FTH} ${FTH_DIR}
    COMMAND $<TARGET_FILE:pforth_midi_builder> -q -d pforth.dic mkdic.fth
    COMMAND ${CMAKE_COMMAND} -E rename pfdicdat.h ${CMAKE_CURRENT_BINARY_DIR}/pfdicdat.h
    WORKING_DIRECTORY ${FTH_DIR}
    DEPENDS ${FTH_DIR}/pforth.dic ${MIDI_FTH}
    COMMENT "Building pforth-midi static dictionary"
    VERBATIM
)
add_custom_target(pforth_midi_dic DEPENDS ${CMAKE_CURRENT_BINARY_DIR}/pfdicdat.h)

# 4. pforth_midi: dictionary compiled in
pforth_midi_core(pforth_midi_core_sd)
target_compile_definitions(pforth_midi_core_sd PUBLIC PF_STATIC_DIC)
target_include_directories(pforth_midi_core_sd PRIVATE ${CMAKE_CURRENT_BINARY_DIR})
add_dependencies(pforth_midi_core_sd pforth_midi_dic)

add_executable(pforth_midi main.c)
target_link_libraries(pforth_midi PRIVATE pforth_midi_core_sd)

add_strip_command(pforth_midi)

install(TARGETS pforth_midi RUNTIME DESTINATION bin)
