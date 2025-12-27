add_executable(forth forth.c)

target_compile_options(forth PRIVATE -Wall -Wextra)

add_strip_command(forth)

install(TARGETS forth RUNTIME DESTINATION bin)
