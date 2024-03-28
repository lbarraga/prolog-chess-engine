% Your parser definitions go here...
% ...

% Utility predicate to read lines from a file
read_line(Stream, Line) :-
    read_line_to_codes(Stream, LineCodes),
    LineCodes \= end_of_file,
    atom_codes(Line, LineCodes).

% Predicate to test parsing of a single line
test_parse(Line) :-
    string_codes(Line, Codes),
    phrase(top_move, Codes), !.

% Predicate to read and test each line in the file
test_file(FileName) :-
    open(FileName, read, Stream),
    test_lines(Stream),
    close(Stream).

% Helper predicate to go through each line and test it
test_lines(Stream) :-
    read_line(Stream, Line),
    !,
    (   test_parse(Line)
    ->  write('OK for '), writeln(Line)  % Line parsed successfully, proceed to next line
    ;   writeln('Failed to parse: '), writeln(Line), fail  % Report failure and stop
    ),
    test_lines(Stream).
test_lines(_).  % End of file reached

% Entry point to start the test
:- begin_tests(san).

start_test :-
    test_file('all_SAN.txt').

:- end_tests(san).
