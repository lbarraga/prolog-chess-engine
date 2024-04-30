:- module(test1, [addition/3]).

addition(X, Y, Z) :- Z is X + Y.

:- begin_tests(addition).

test(addition_1) :- addition(2, 3, 5).
test(addition_2) :- addition(0, 0, 0).
test(addition_3) :- addition(-1, 1, 0).

:- end_tests(addition).