\ test_basic.4th - Basic arithmetic and stack operations
\ Each test prints -1 for pass (Forth true), 0 for fail

\ Arithmetic tests
3 4 + 7 = .
10 3 - 7 = .
6 7 * 42 = .
20 4 / 5 = .
17 5 mod 2 = .

\ Comparison tests
5 3 > .
3 5 < .
7 7 = .
4 5 <> .

\ Stack operations
42 dup + 84 = .
1 2 swap drop 2 = .

\ Negative numbers via subtraction (since -5 is parsed as rest + 5)
0 5 - abs 5 = .
7 negate 0 7 - = .

\ Min/max
3 7 min 3 = .
3 7 max 7 = .

\ Conditionals (print 1 if true branch taken)
1 if 1 else 0 then .
0 if 0 else 1 then .

\ Word definitions
: double 2 * ;
7 double 14 = .

\ Nested definitions
: triple 3 * ;
: times6 double triple ;
2 times6 12 = .

\ Print marker for test completion
999 .
