add_executable(midi_forth midi_forth.c)

target_compile_options(midi_forth PRIVATE -Wall -Wextra)

# Link with libremidi
target_link_libraries(midi_forth PRIVATE libremidi)

# Link with readline
find_library(READLINE_LIBRARY readline)
if(READLINE_LIBRARY)
    target_link_libraries(midi_forth PRIVATE ${READLINE_LIBRARY})
else()
    message(FATAL_ERROR "readline library not found")
endif()

# Platform-specific dependencies
if(APPLE)
    target_link_libraries(midi_forth PRIVATE
        "-framework CoreMIDI"
        "-framework CoreFoundation"
        "-framework CoreAudio"
    )
elseif(UNIX)
    find_package(ALSA)
    if(ALSA_FOUND)
        target_link_libraries(midi_forth PRIVATE ${ALSA_LIBRARIES})
    endif()
endif()

install(TARGETS midi_forth RUNTIME DESTINATION bin)
