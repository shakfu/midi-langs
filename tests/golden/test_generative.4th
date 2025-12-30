\ test_generative.4th - Generative and random features
\ Each test prints -1 for pass, 0 for fail

\ Set a known seed for reproducibility
12345 seed!

\ Test seed store/fetch
seed@ 12345 = .

\ Different seed
42 seed! seed@ 42 = .

\ Reset seed
12345 seed!

\ Test random - should return some value
random 0 >= .

\ Test srand-range (random in range) - should be 0-99
0 100 srand-range dup 0 >= swap 100 < and .

\ Print marker
999 .
