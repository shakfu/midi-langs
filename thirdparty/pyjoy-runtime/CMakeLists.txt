# pyjoy-runtime - Joy language runtime library

add_library(pyjoy_runtime STATIC
    joy_runtime.c
    joy_primitives.c
    joy_parser.c
)

target_include_directories(pyjoy_runtime PUBLIC ${CMAKE_CURRENT_SOURCE_DIR})

# Link math library on Unix
if(UNIX)
    target_link_libraries(pyjoy_runtime PUBLIC m)
endif()

# Compiler warnings
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(pyjoy_runtime PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()
