# alda-midi tests

# Unit tests (ported from psnd's source/langs/alda/tests)
foreach(name scanner parser parser_fuzz interpreter playback)
    add_executable(test_alda_${name} test_${name}.c test_framework.c)
    target_link_libraries(test_alda_${name} PRIVATE alda $<$<NOT:$<PLATFORM_ID:Windows>>:m>)
    add_test(NAME alda_${name}_tests COMMAND $<TARGET_FILE:test_alda_${name}>)
    set_tests_properties(alda_${name}_tests PROPERTIES LABELS "unit")
endforeach()

# Shared suite test runner (Unix only - uses POSIX dirent.h)
if(NOT WIN32)
    add_executable(test_shared_suite test_shared_suite.c)
    target_link_libraries(test_shared_suite PRIVATE alda)
    target_link_libraries(test_shared_suite PRIVATE m)  # for math functions

    # CTest integration
    add_test(
        NAME alda_midi_test_suite
        COMMAND $<TARGET_FILE:test_shared_suite> "${CMAKE_SOURCE_DIR}/docs/alda-midi/shared_suite"
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    )
    set_tests_properties(alda_midi_test_suite PROPERTIES LABELS "integration")

    # Every example against Alda's own output (docs/alda-midi/conformance.md)
    add_test(
        NAME alda_conformance_examples
        COMMAND $<TARGET_FILE:test_shared_suite>
                "${CMAKE_SOURCE_DIR}/docs/alda-midi/examples"
                "${CMAKE_SOURCE_DIR}/docs/alda-midi/alda_reference"
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    )
    set_tests_properties(alda_conformance_examples PROPERTIES LABELS "integration")
endif()
