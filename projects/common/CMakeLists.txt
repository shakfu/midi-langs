# Common music theory library

add_library(music_theory STATIC
    music_theory.c
    music_theory.h
)

target_include_directories(music_theory PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

# Set standard
set_target_properties(music_theory PROPERTIES
    C_STANDARD 11
    C_STANDARD_REQUIRED ON
)
