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

# Readline support (enabled by default)
if(NOT WIN32)
    find_library(READLINE_LIB readline)
    if(READLINE_LIB)
        target_compile_definitions(pyjoy_runtime PUBLIC HAVE_READLINE)
        target_link_libraries(pyjoy_runtime PUBLIC ${READLINE_LIB})
        message(STATUS "pyjoy-runtime: readline support enabled")
    else()
        message(STATUS "pyjoy-runtime: readline not found, using basic input")
    endif()
endif()

# Compiler warnings
if(CMAKE_C_COMPILER_ID MATCHES "GNU|Clang")
    target_compile_options(pyjoy_runtime PRIVATE -Wall -Wextra -Wno-unused-parameter)
endif()
