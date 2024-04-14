:- initialization(main).
:- use_module('parsing/parse_pgn', [pgn/3]).
:- use_module('parsing/parse_to_board', [parse_to_board/4]).
:- use_module(prettyprint, [print_board/1]).
:- use_module('board.pl').

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
pretty_print(_, []).
pretty_print(N, [turn(From, To)|T]) :-
    write(N), write(". "),
    write(From), write(" - "), write(To), nl,
    N1 is N + 1,
    pretty_print(N1, T).


main :-
    % Extract the filename from the command line arguments
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, FileName),

    % Parse the file into a list of moves.
    write('Processing file: '), writeln(FileName),
    parse_file(FileName, Parsed),
    pretty_print(1, Parsed),

    % Parse the moves into a board
    writeln('Initial State:'),
    init_state(InitialState),
    writeln('Initial State:'),
    parse_to_board(Parsed, InitialState, FinalState, _),
    writeln('Initial State:'),
    FinalState = state(Board, _, _),
    writeln('Initial State:'),
    print_board(Board),
    halt.