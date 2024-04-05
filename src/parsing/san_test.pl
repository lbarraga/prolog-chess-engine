:- module(san_test, [test/0]).
:- use_module(parse_move).

read_line(Stream, Line) :-
    read_line_to_codes(Stream, LineCodes),
    LineCodes \= end_of_file,
    atom_codes(Line, LineCodes).

test_parse(Line) :-
    string_codes(Line, Codes),
    phrase(top_move, Codes), !.

test_file(FileName) :-
    open(FileName, read, Stream),
    test_lines(Stream),
    close(Stream).

test_lines(Stream) :-
    read_line(Stream, Line),
    !,
    (   test_parse(Line)
    ->  true
    ;   writeln('Failed to parse: '), writeln(Line), fail
    ),
    test_lines(Stream).
test_lines(_).

test :-
    test_file('src/parsing/all_SAN.txt').
