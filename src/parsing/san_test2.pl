% Step 1: Define a list of test moves

test_moves(Moves) :-
    findall(Move, (phrase(top_move, Codes), string_codes(Move, Codes)), Moves).

% Utility predicate to read all lines from a file into a list
read_all_lines(FileName, Lines) :-
    open(FileName, read, Stream),
    read_lines(Stream, Lines),
    close(Stream).

read_lines(Stream, [Line|Lines]) :-
    read_line_to_string(Stream, Line),
    Line \= end_of_file, !,
    read_lines(Stream, Lines).
read_lines(_, []).

% Predicate to check if a single move is in the file and print the result
check_move(Move, AllLines) :-
    (   memberchk(Move, AllLines)
    ->  format('OK ~w~n', [Move])
    ;   format('[FAIL] ~w~n', [Move]), fail
    ).

% Predicate to iterate through all test moves and check each one
check_moves_in_file([], _).
check_moves_in_file([Move|Moves], AllLines) :-
    check_move(Move, AllLines),
    check_moves_in_file(Moves, AllLines).

% Entry point for the test
test_all_moves_present :-
    test_moves(TestMoves),
    read_all_lines('src/parsing/all_SAN.txt', AllMoves),
    check_moves_in_file(TestMoves, AllMoves).

% Running the test
:- begin_tests(all_moves).

test(all_moves_present) :-
    test_all_moves_present.

:- end_tests(all_moves).

% To run the test, use ?- run_tests.
