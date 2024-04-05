:- use_module(san_test).
:- use_module(san_test2).

:- begin_tests(san_and_all_moves).

test(san_test) :-
    test.

%test(all_moves_present) :-
%    test_all_moves_present.

:- end_tests(san_and_all_moves).

run :-
    run_tests.
