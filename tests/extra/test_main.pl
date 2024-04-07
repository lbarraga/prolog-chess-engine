:- use_module(test_grading_pgns, [parse_pgn_test/1]).
:- use_module(san_move_test, [test_file/1]).

:- begin_tests(san_and_all_moves).

test(grading_pgns) :-
    parse_pgn_test("tests/grading/pgns"),
    parse_pgn_test("tests/extra/test_pgns"),
    true.

test(san_move_test) :-
    test_file('tests/extra/all_SAN.txt').

:- end_tests(san_and_all_moves).

