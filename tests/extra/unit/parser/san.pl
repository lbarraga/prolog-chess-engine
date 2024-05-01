:- module(san, []).
:- use_module('../../../../src/board.pl').
:- use_module('../../../../src/moves/moves_main.pl').
:- use_module('../../../../src/parsing/parse_san.pl').

read_file_to_lines(FilePath, Lines) :-
    read_file_to_codes(FilePath, Codes, []),
    atom_codes(Atom, Codes),
    atomic_list_concat(Atoms, '\n', Atom),
    maplist(atom_string, Atoms, Lines).

parse_list([]).
parse_list([SanString|T]) :-
    string_codes(SanString, SanCodes),
    phrase(san(_, _), SanCodes),
    parse_list(T), !.


:- begin_tests(san).

% Test if all 29274 SAN moves can be parsed.
test(san1) :- read_file_to_lines("tests/extra/all_SAN.txt", List), parse_list(List).

:- end_tests(san).