:- module(san_move_test, [test_file/1]).
:- use_module('../../src/parsing/parse_san', [san/2]).

read_line(Stream, Line) :-
    read_line_to_codes(Stream, LineCodes),
    LineCodes \= end_of_file,
    atom_codes(Line, LineCodes).

test_parse(Line) :-
    string_codes(Line, Codes),
    phrase(san, Codes), !.

test_file(FileName) :-
    open(FileName, read, Stream),
    test_lines(Stream),
    close(Stream).

test_lines(Stream) :-
    read_line(Stream, Line),
    !,
    (   test_parse(Line)
    ->  true
    ;   writeln('[FAIL] '), writeln(Line), fail
    ),
    test_lines(Stream).
test_lines(_).
