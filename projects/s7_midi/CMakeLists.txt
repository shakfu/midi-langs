set(S7_DIR ${CMAKE_SOURCE_DIR}/thirdparty/s7)

add_executable(s7_midi
    main.c
    midi_module.c
    ${S7_DIR}/s7.c
)

target_include_directories(s7_midi PRIVATE ${S7_DIR})

target_compile_options(s7_midi PRIVATE -Wall -Wextra -Wno-unused-parameter)

# s7 requires math and dl libraries
target_link_libraries(s7_midi PRIVATE libremidi m)

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(s7_midi PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    target_link_libraries(s7_midi PRIVATE dl)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(s7_midi PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

install(TARGETS s7_midi RUNTIME DESTINATION bin)
