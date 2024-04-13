:- initialization(main).
:- use_module('parsing/parse_pgn', [pgn/3]).
:- use_module('parsing/parse_to_board', [parse_to_board/4]).

parse_file(FileName, Parsed) :-
    % Open the file
    open(FileName, read, Stream),

    % Use the pgn predicate from the parse_pgn module to parse the file
    ( phrase_from_stream(pgn(Moves), Stream)
    -> Parsed = Moves
    ;  Parsed = []
    ),
    close(Stream).

% Define a predicate to pretty print a list
pretty_print([]).
pretty_print([turn(From, To)|T]) :-
    write(From), write(" ---- "), write(To), nl,
    pretty_print(T).


main :-
    % Extract the filename from the command line arguments
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, FileName),
    write('Processing file: '), writeln(FileName),
    parse_file(FileName),
    halt.