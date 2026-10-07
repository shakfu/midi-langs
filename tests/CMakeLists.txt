# Tests that open a MIDI port run under this wrapper, which holds one MIDI
# client open for their duration; tests/midi_keepalive.c explains why.
if(NOT WIN32)
    add_executable(midi_keepalive midi_keepalive.c)
    target_link_libraries(midi_keepalive PRIVATE midi_open)

    set(MIDI_KEEPALIVE
        ${CMAKE_CURRENT_SOURCE_DIR}/with_midi_keepalive.sh
        $<TARGET_FILE:midi_keepalive>
    )
endif()

# C unit tests for stack_midi
add_executable(test_stack_midi_unit test_stack_midi_unit.c)
target_link_libraries(test_stack_midi_unit PRIVATE stack_midi_lib)
target_include_directories(test_stack_midi_unit PRIVATE ${CMAKE_SOURCE_DIR}/projects/stack-midi)

add_test(
    NAME stack_midi_unit_tests
    COMMAND test_stack_midi_unit
)
set_tests_properties(stack_midi_unit_tests PROPERTIES LABELS "quick")

# Shell-based tests (Unix only)
if(NOT WIN32)

# Test using the comprehensive shell script
add_test(
    NAME stack_midi_test_suite
    COMMAND ${MIDI_KEEPALIVE} ${CMAKE_CURRENT_SOURCE_DIR}/test_stack_midi.sh $<TARGET_FILE:stack_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

# Set test properties (includes midi label because it tests MIDI functionality)
set_tests_properties(stack_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration;midi"
)

# Individual quick tests for faster feedback during development
add_test(
    NAME stack_midi_smoke
    COMMAND sh -c "echo 'midi-list' | $<TARGET_FILE:stack_midi>"
)

add_test(
    NAME stack_midi_arithmetic
    COMMAND sh -c "echo '3 4 + .' | $<TARGET_FILE:stack_midi> | grep -q '7'"
)

add_test(
    NAME stack_midi_pitch_parsing
    COMMAND sh -c "echo 'c4 .' | $<TARGET_FILE:stack_midi> | grep -q '60'"
)

add_test(
    NAME stack_midi_conditionals
    COMMAND sh -c "echo '1 if 42 . then' | $<TARGET_FILE:stack_midi> | grep -q '42'"
)

add_test(
    NAME stack_midi_word_definitions
    COMMAND sh -c "echo ': double 2 * ; 7 double .' | $<TARGET_FILE:stack_midi> | grep -q '14'"
)

add_test(
    NAME stack_midi_blocks
    COMMAND sh -c "echo '{ 1 . } 3 *' | $<TARGET_FILE:stack_midi> | grep -q '1 1 1'"
)

# Label quick tests
set_tests_properties(
    stack_midi_smoke
    stack_midi_arithmetic
    stack_midi_pitch_parsing
    stack_midi_conditionals
    stack_midi_word_definitions
    stack_midi_blocks
    PROPERTIES LABELS "quick"
)

# pktpy_midi tests
add_test(
    NAME pktpy_midi_test_suite
    COMMAND ${MIDI_KEEPALIVE} ${CMAKE_CURRENT_SOURCE_DIR}/test_pktpy_midi.sh $<TARGET_FILE:pktpy_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(pktpy_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration;midi"
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
    COMMAND ${MIDI_KEEPALIVE} sh -c "echo 'import midi; m = midi.open(); print(m.is_open)' | $<TARGET_FILE:pktpy_midi> | grep -q 'True'"
)

set_tests_properties(
    pktpy_midi_smoke
    pktpy_midi_note_parsing
    PROPERTIES LABELS "quick"
)

set_tests_properties(
    pktpy_midi_virtual_port
    PROPERTIES LABELS "quick;midi"
)

# s7_midi tests
add_test(
    NAME s7_midi_test_suite
    COMMAND ${MIDI_KEEPALIVE} ${CMAKE_CURRENT_SOURCE_DIR}/test_s7_midi.sh $<TARGET_FILE:s7_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(s7_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration;midi"
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
    COMMAND ${MIDI_KEEPALIVE} sh -c "$<TARGET_FILE:s7_midi> -e '(begin (define m (midi-open)) (midi-out? m))' | grep -q '#t'"
)

set_tests_properties(
    s7_midi_smoke
    s7_midi_note_parsing
    s7_midi_pitch_constants
    PROPERTIES LABELS "quick"
)

set_tests_properties(
    s7_midi_virtual_port
    PROPERTIES LABELS "quick;midi"
)

# lua_midi tests
add_test(
    NAME lua_midi_test_suite
    COMMAND ${MIDI_KEEPALIVE} ${CMAKE_CURRENT_SOURCE_DIR}/test_lua_midi.sh $<TARGET_FILE:lua_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(lua_midi_test_suite PROPERTIES
    TIMEOUT 60
    LABELS "integration;midi"
)

# Quick lua_midi tests
add_test(
    NAME lua_midi_smoke
    COMMAND sh -c "$<TARGET_FILE:lua_midi> -e 'print(1+2)' | grep -q '3'"
)

add_test(
    NAME lua_midi_note_parsing
    COMMAND sh -c "$<TARGET_FILE:lua_midi> -e 'print(midi.note(\"C4\"))' | grep -q '60'"
)

add_test(
    NAME lua_midi_pitch_constants
    COMMAND sh -c "$<TARGET_FILE:lua_midi> -e 'print(midi.c4)' | grep -q '60'"
)

add_test(
    NAME lua_midi_virtual_port
    COMMAND ${MIDI_KEEPALIVE} sh -c "$<TARGET_FILE:lua_midi> -e 'local m = midi.open(); print(m:is_open()); m:close()' | grep -q 'true'"
)

set_tests_properties(
    lua_midi_smoke
    lua_midi_note_parsing
    lua_midi_pitch_constants
    PROPERTIES LABELS "quick"
)

set_tests_properties(
    lua_midi_virtual_port
    PROPERTIES LABELS "quick;midi"
)

# mhs-midi tests
add_test(
    NAME mhs_midi_test_suite
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_mhs_midi.sh $<TARGET_FILE:mhs-midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(mhs_midi_test_suite PROPERTIES
    TIMEOUT 120
    LABELS "integration"
)

# pforth-midi tests
add_test(
    NAME pforth_midi_test_suite
    COMMAND ${MIDI_KEEPALIVE} ${CMAKE_CURRENT_SOURCE_DIR}/test_pforth_midi.sh $<TARGET_FILE:pforth_midi>
    WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
)

set_tests_properties(pforth_midi_test_suite PROPERTIES
    TIMEOUT 120
    LABELS "integration;midi"
)

# Every binding lists ports as "client: port"; skips without an ALSA sequencer
add_test(
    NAME port_labels
    COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_port_labels.sh ${CMAKE_BINARY_DIR}
)

set_tests_properties(port_labels PROPERTIES
    TIMEOUT 180
    LABELS "integration;midi"
)

# Golden .stk test programs for stack_midi
add_test(
    NAME stack_midi_golden_basic
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_basic.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_basic.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_notation
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_notation.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_notation.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_blocks
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_blocks.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_blocks.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_generative
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_generative.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_generative.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_sequences
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_sequences.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_sequences.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_async
    COMMAND ${MIDI_KEEPALIVE} sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_async.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_async.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_seq_record
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_seq_record.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_seq_record.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_comments
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_comments.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_comments.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_dictionary
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_dictionary.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_dictionary.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_intervals
    COMMAND sh -c "$<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_intervals.stk | grep -q '999' && ! $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_intervals.stk | grep -q ' 0 '"
)

add_test(
    NAME stack_midi_golden_async_churn
    # 200 cycles and one final check each print -1; sequence messages also contain " 0 "
    COMMAND sh -c "out=$($<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/golden/test_async_churn.stk); [ $(echo \"$out\" | tr ' ' '\\n' | grep -cx -- -1) -eq 201 ] && ! echo \"$out\" | grep -q 'not playing'"
)

set_tests_properties(
    stack_midi_golden_async_churn
    stack_midi_golden_intervals
    stack_midi_golden_dictionary
    stack_midi_golden_comments
    stack_midi_golden_basic
    stack_midi_golden_notation
    stack_midi_golden_blocks
    stack_midi_golden_generative
    stack_midi_golden_sequences
    stack_midi_golden_seq_record
    # Check stdout only; the null backend opens ports without a MIDI system
    PROPERTIES LABELS "golden" ENVIRONMENT "MIDI_LANGS_BACKEND=null"
)

set_tests_properties(
    stack_midi_golden_async
    PROPERTIES LABELS "golden;midi"
)

# alda_midi integration tests (shell script, Unix only)
if(NOT WIN32)
    add_test(
        NAME alda_midi_integration
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/test_alda_midi.sh $<TARGET_FILE:alda_midi>
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )

    set_tests_properties(alda_midi_integration PROPERTIES
        TIMEOUT 60
        LABELS "integration"
    )
endif()

# Tests for scripts/sync_alda_from_psnd.py (pytest, run through uv)
find_program(UV_EXECUTABLE uv)
# CI sets this so a missing uv fails configure instead of skipping docs_examples
option(MIDI_LANGS_REQUIRE_DOC_TESTS "Fail configure if uv is missing" OFF)
if(MIDI_LANGS_REQUIRE_DOC_TESTS AND NOT UV_EXECUTABLE)
    message(FATAL_ERROR "MIDI_LANGS_REQUIRE_DOC_TESTS is ON but uv was not found; docs_examples needs it")
endif()
if(UV_EXECUTABLE)
    add_test(
        NAME alda_sync_script
        COMMAND ${UV_EXECUTABLE} run --no-project --with pytest
                pytest -q ${CMAKE_CURRENT_SOURCE_DIR}/test_sync_alda_from_psnd.py
        WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
    )
    set_tests_properties(alda_sync_script PROPERTIES LABELS "unit")

    # Code blocks in README.md and docs/ (docs/dev/doc-tests.md)
    if(NOT WIN32)
        add_test(
            NAME docs_examples
            COMMAND ${UV_EXECUTABLE} run --no-project --with pytest
                    pytest -q ${CMAKE_CURRENT_SOURCE_DIR}/test_docs.py
                              ${CMAKE_CURRENT_SOURCE_DIR}/test_api_docs.py
            WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}
        )
        set_tests_properties(docs_examples PROPERTIES
            ENVIRONMENT "MIDI_LANGS_BUILD=${CMAKE_BINARY_DIR}"
            LABELS "docs"
            TIMEOUT 600
        )
    endif()
endif()

# Quick alda_midi tests (shell-based, Unix only)
if(NOT WIN32)
    add_test(
        NAME alda_midi_smoke
        COMMAND sh -c "$<TARGET_FILE:alda_midi> --help | grep -q 'Alda'"
    )

    set_tests_properties(
        alda_midi_smoke
        PROPERTIES LABELS "quick"
    )
endif()

# joy_midi tests (shell-based, Unix only)
if(NOT WIN32)
    add_test(
        NAME joy_midi_smoke
        COMMAND sh -c "echo 'midi-list' | $<TARGET_FILE:joy_midi>"
    )

    add_test(
        NAME joy_midi_arithmetic
        COMMAND sh -c "echo '3 4 + .' | $<TARGET_FILE:joy_midi> | grep -q '7'"
    )

    add_test(
        NAME joy_midi_pitch_parsing
        COMMAND sh -c "echo '\"C4\" pitch .' | $<TARGET_FILE:joy_midi> | grep -q '60'"
    )

    add_test(
        NAME joy_midi_major_chord
        COMMAND sh -c "echo '60 major .' | $<TARGET_FILE:joy_midi> | grep -q '\\[60 64 67\\]'"
    )

    add_test(
        NAME joy_midi_minor_chord
        COMMAND sh -c "echo '60 minor .' | $<TARGET_FILE:joy_midi> | grep -q '\\[60 63 67\\]'"
    )

    add_test(
        NAME joy_midi_transpose
        COMMAND sh -c "echo '60 12 transpose .' | $<TARGET_FILE:joy_midi> | grep -q '72'"
    )

    add_test(
        NAME joy_midi_virtual_port
        COMMAND ${MIDI_KEEPALIVE} sh -c "echo 'midi-virtual' | $<TARGET_FILE:joy_midi> | grep -q 'JoyMIDI'"
    )

    set_tests_properties(
        joy_midi_smoke
        joy_midi_arithmetic
        joy_midi_pitch_parsing
        joy_midi_major_chord
        joy_midi_minor_chord
        joy_midi_transpose
        PROPERTIES LABELS "quick"
    )

    set_tests_properties(
        joy_midi_virtual_port
        PROPERTIES LABELS "quick;midi"
    )

    # Include Joy unit tests from pyjoy-lang
    add_subdirectory(joy-midi)
endif()

# demo_example_mhs (shell script)
add_test(
    NAME demo_example_mhs
    COMMAND ${CMAKE_SOURCE_DIR}/scripts/mhs-midi run ${CMAKE_CURRENT_SOURCE_DIR}/examples/DemoMhs.hs
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
)

set_tests_properties(
    demo_example_mhs
    PROPERTIES
        LABELS "integration"
        TIMEOUT 120
)

# mhs-midi-src compilation test (tests the default standalone variant)
# Compiles a simple Haskell file to executable and runs it
add_test(
    NAME mhs_standalone_compile
    COMMAND sh -c "TMPEXE=$(mktemp) && $<TARGET_FILE:mhs-midi-src> -o\$TMPEXE ${CMAKE_CURRENT_SOURCE_DIR}/mhs/TestCompile.hs && \$TMPEXE; STATUS=\$?; rm -f \$TMPEXE; exit \$STATUS"
)

set_tests_properties(mhs_standalone_compile PROPERTIES
    TIMEOUT 60
    LABELS "integration"
)

endif() # NOT WIN32

# Cross-platform tests (work on both Unix and Windows)

# alda_midi basic test - direct executable, no shell
add_test(
    NAME alda_midi_basic
    COMMAND $<TARGET_FILE:alda_midi> --no-sleep ${CMAKE_CURRENT_SOURCE_DIR}/alda/basic.alda
)

set_tests_properties(
    alda_midi_basic
    PROPERTIES LABELS "quick"
)

# README example tests - verify examples from README.md work
# These use --no-sleep to run quickly without actual audio delays

add_test(
    NAME demo_example_alda
    COMMAND $<TARGET_FILE:alda_midi> --no-sleep ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_alda.alda
)

add_test(
    NAME demo_example_stack
    COMMAND $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_stack.stk
)

add_test(
    NAME demo_example_lua
    COMMAND ${MIDI_KEEPALIVE} $<TARGET_FILE:lua_midi> ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_lua.lua
)

add_test(
    NAME demo_example_pktpy
    COMMAND $<TARGET_FILE:pktpy_midi> ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_pktpy.py
)

add_test(
    NAME demo_example_s7
    COMMAND $<TARGET_FILE:s7_midi> ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_s7.scm
)

add_test(
    NAME demo_example_joy
    COMMAND $<TARGET_FILE:joy_midi> ${CMAKE_CURRENT_SOURCE_DIR}/examples/demo_joy.joy
)

set_tests_properties(
    demo_example_alda
    demo_example_stack
    demo_example_pktpy
    demo_example_s7
    demo_example_joy
    PROPERTIES LABELS "quick" ENVIRONMENT "MIDI_LANGS_BACKEND=null"
)

set_tests_properties(
    demo_example_lua
    PROPERTIES LABELS "quick;midi"
)

# A script error must give a non-zero exit status (WILL_FAIL inverts the result)
add_test(
    NAME error_exit_stack
    COMMAND $<TARGET_FILE:stack_midi> --no-sleep --script ${CMAKE_CURRENT_SOURCE_DIR}/errors/unknown_word.stk
)

add_test(
    NAME error_exit_s7
    COMMAND $<TARGET_FILE:s7_midi> ${CMAKE_CURRENT_SOURCE_DIR}/errors/unbound.scm
)

set_tests_properties(
    error_exit_stack
    error_exit_s7
    PROPERTIES WILL_FAIL TRUE LABELS "quick"
)

if(TARGET guile_midi)
    add_test(
        NAME error_exit_guile
        COMMAND $<TARGET_FILE:guile_midi> ${CMAKE_CURRENT_SOURCE_DIR}/errors/unbound.scm
    )
    set_tests_properties(error_exit_guile PROPERTIES WILL_FAIL TRUE LABELS "quick")
endif()

# README examples on the null backend (MIDI_LANGS_BACKEND=null): no MIDI
# server or keepalive client needed
function(add_null_backend_test lang)
    add_test(NAME null_backend_${lang} COMMAND ${ARGN})
    set_tests_properties(null_backend_${lang} PROPERTIES
        ENVIRONMENT "MIDI_LANGS_BACKEND=null"
        LABELS "quick"
    )
endfunction()

set(EX ${CMAKE_CURRENT_SOURCE_DIR}/examples)
add_null_backend_test(alda $<TARGET_FILE:alda_midi> --no-sleep ${EX}/demo_alda.alda)
add_null_backend_test(stack $<TARGET_FILE:stack_midi> --no-sleep --script ${EX}/demo_stack.stk)
add_null_backend_test(lua $<TARGET_FILE:lua_midi> ${EX}/demo_lua.lua)
add_null_backend_test(pktpy $<TARGET_FILE:pktpy_midi> ${EX}/demo_pktpy.py)
add_null_backend_test(s7 $<TARGET_FILE:s7_midi> ${EX}/demo_s7.scm)
add_null_backend_test(joy $<TARGET_FILE:joy_midi> ${EX}/demo_joy.joy)
