\ test_blocks.4th - Anonymous blocks and repetition
\ Each test prints -1 for pass, 0 for fail

\ Test block execution with *
\ Block repeats push values to stack

\ Test single execution
clear { 42 } 1 * 42 = .

\ Test that blocks work with definitions
: test-block { 1 } 3 * ;
clear test-block depth 3 = .

\ Test block with * operator - creates 4 values on stack
clear { 5 } 4 * depth 4 = .

\ Sum all values from block repetition
clear { 10 } 5 * + + + + 50 = .

\ Test zero repetitions (should not execute)
clear { 99 } 0 * depth 0 = .

\ Print marker
999 .
