:- initialization(main).
:- use_module('parsing/parse_pgn', [pgn/3]).
:- use_module('parsing/parse_to_board', [parse_to_board/4]).
:- use_module(prettyprint, [print_board/1]).
:- use_module(alpha_beta).
:- use_module('board.pl').
:- use_module(library(prolog_stack)).
:- use_module(test).

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
    play,
    halt,
    % Extract the filename from the command line arguments
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, FileName),

    % Parse the file into a list of moves.
    write('Processing file: '), writeln(FileName), nl,
    parse_file(FileName, Parsed),
    pretty_print(1, Parsed), nl, nl,

    % Parse the moves into a board
    init_state(InitialState),
    parse_to_board(Parsed, InitialState, FinalState, _),
    FinalState = state(Board, Info, Color),
    write('Info: '), writeln(Info),
    write('Color: '), writeln(Color),
    writeln('Board:'),
    print_board(Board), nl,

    % Calculate the best move and profile it
    time(best_move(FinalState, 1, BestMove)),

    write('Best move: '), writeln(BestMove),
    halt.