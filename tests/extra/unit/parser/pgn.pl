:- module(pgn, []).
:- use_module('../../../../src/parsing/parse_pgn.pl').

parse_file(FileName) :-
    open(FileName, read, Stream),
    phrase_from_stream(pgn(_, _), Stream), !.

:- begin_tests(pgn).

test(pgn1) :- parse_file("tests/grading/pgns/random.01.0.pgn").
test(pgn1) :- parse_file("tests/grading/pgns/random.10.1.pgn").
test(pgn1) :- parse_file("tests/grading/pgns/random.31.0.pgn").
test(pgn1) :- parse_file("tests/grading/pgns/tags.1.pgn").
test(pgn1) :- parse_file("tests/extra/test_pgns/fisher_game_123.pgn").

:- end_tests(pgn).