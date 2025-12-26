add_executable(forth forth.c)

target_compile_options(forth PRIVATE -Wall -Wextra)

install(TARGETS forth RUNTIME DESTINATION bin)
