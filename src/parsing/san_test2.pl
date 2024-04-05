:- module(san_test2, [test_all_moves_present/0]).
:- use_module(parse_move).


test_moves(Moves) :-
    findall(Move, (phrase(top_move, Codes), string_codes(Move, Codes)), Moves).

read_all_lines(FileName, Lines) :-
    open(FileName, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

read_lines(Stream, [Line|Lines]) :-
    read_line_to_string(Stream, Line),
    Line \= end_of_file, !,
    read_lines(Stream, Lines).
read_lines(_, []).

check_move(Move, AllLines) :-
    (   memberchk(Move, AllLines)
    ->  true
    ;   format('[FAIL] ~w~n', [Move]), fail
    ).

check_moves_in_file([], _).
check_moves_in_file([Move|Moves], AllLines) :-
    check_move(Move, AllLines),
    check_moves_in_file(Moves, AllLines).

test_all_moves_present :-
    test_moves(TestMoves),
    read_all_lines('src/parsing/all_SAN.txt', AllMoves),
    check_moves_in_file(TestMoves, AllMoves).
