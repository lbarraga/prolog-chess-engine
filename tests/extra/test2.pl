:- module(test2, [subtraction/3]).

subtraction(X, Y, Z) :- Z is X - Y.

:- begin_tests(subtraction).

% Test cases for subtraction
test(subtraction_1) :- subtraction(5, 3, 2).
test(subtraction_2) :- subtraction(10, 7, 3).
test(subtraction_3) :- subtraction(0, 0, 0).
test(subtraction_4) :- subtraction(-1, 1, -2).

:- end_tests(subtraction).