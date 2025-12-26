# Test using the comprehensive shell script
add_test(
    NAME midi_forth_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_midi_forth.sh $<TARGET_FILE:midi_forth>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# Set test properties
set_tests_properties(midi_forth_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Individual quick tests for faster feedback during development
add_test(
    NAME midi_forth_smoke
    COMMAND sh -c "echo 'midi-list' | $<TARGET_FILE:midi_forth>"
)

add_test(
    NAME midi_forth_arithmetic
    COMMAND sh -c "echo '3 4 + .' | $<TARGET_FILE:midi_forth> | grep -q '7'"
)

add_test(
    NAME midi_forth_pitch_parsing
    COMMAND sh -c "echo 'c4 .' | $<TARGET_FILE:midi_forth> | grep -q '60'"
)

add_test(
    NAME midi_forth_conditionals
    COMMAND sh -c "echo '1 if 42 . then' | $<TARGET_FILE:midi_forth> | grep -q '42'"
)

add_test(
    NAME midi_forth_word_definitions
    COMMAND sh -c "echo ': double 2 * ; 7 double .' | $<TARGET_FILE:midi_forth> | grep -q '14'"
)

add_test(
    NAME midi_forth_blocks
    COMMAND sh -c "echo '{ 1 . } 3 *' | $<TARGET_FILE:midi_forth> | grep -q '1 1 1'"
)

# Label quick tests
set_tests_properties(
    midi_forth_smoke
    midi_forth_arithmetic
    midi_forth_pitch_parsing
    midi_forth_conditionals
    midi_forth_word_definitions
    midi_forth_blocks
    PROPERTIES LABELS "quick"
)
