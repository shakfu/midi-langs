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

# pktpy_midi tests
add_test(
    NAME pktpy_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_pktpy_midi.sh $<TARGET_FILE:pktpy_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(pktpy_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Quick pktpy_midi tests
add_test(
    NAME pktpy_midi_smoke
    COMMAND sh -c "echo 'import midi; print(\"ok\")' | $<TARGET_FILE:pktpy_midi> | grep -q 'ok'"
)

add_test(
    NAME pktpy_midi_note_parsing
    COMMAND sh -c "echo 'import midi; print(midi.note(\"C4\"))' | $<TARGET_FILE:pktpy_midi> | grep -q '60'"
)

add_test(
    NAME pktpy_midi_virtual_port
    COMMAND sh -c "echo 'import midi; m = midi.open(); print(m.is_open)' | $<TARGET_FILE:pktpy_midi> | grep -q 'True'"
)

set_tests_properties(
    pktpy_midi_smoke
    pktpy_midi_note_parsing
    pktpy_midi_virtual_port
    PROPERTIES LABELS "quick"
)

# s7_midi tests
add_test(
    NAME s7_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_s7_midi.sh $<TARGET_FILE:s7_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(s7_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

# Quick s7_midi tests
add_test(
    NAME s7_midi_smoke
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e '(+ 1 2)' | grep -q '3'"
)

add_test(
    NAME s7_midi_note_parsing
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e '(note \"C4\")' | grep -q '60'"
)

add_test(
    NAME s7_midi_pitch_constants
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e 'c4' | grep -q '60'"
)

add_test(
    NAME s7_midi_virtual_port
    COMMAND sh -c "$<TARGET_FILE:s7_midi> -e '(begin (define m (midi-open)) (midi-out? m))' | grep -q '#t'"
)

set_tests_properties(
    s7_midi_smoke
    s7_midi_note_parsing
    s7_midi_pitch_constants
    s7_midi_virtual_port
    PROPERTIES LABELS "quick"
)
