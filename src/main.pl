:- initialization(main).
:- use_module('parsing/parse_pgn', [pgn/3]).
:- use_module('parsing/parse_san', [san/4]).
:- use_module('parsing/san_move', [san_move/3]).
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

san_to_move_string(no_move, "") :- !.
san_to_move_string(San, MoveString) :- phrase(San, List), atom_chars(MoveString, List).

% Define a predicate to pretty print a list
pretty_print(Moves) :- pretty_print_(1, no_move, Moves).

pretty_print_(_, no_move, []) :- !.
pretty_print_(N, _, []) :- write(N), write(". ").
pretty_print_(N, _, [turn(WhiteSan, BlackSan)|T]) :-
    write(N), write(". "),
    san_to_move_string(WhiteSan, WhiteMoveString),
    san_to_move_string(BlackSan, BlackMoveString),
    write(WhiteMoveString), write(" "),
    % Dont write anything if blacks an is no_move
    (BlackSan = no_move -> true ; write(BlackMoveString), write(" ")),

    N1 is N + 1,
    pretty_print_(N1, BlackSan, T).


main :-
    % Extract the filename from the command line arguments
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, FileName),

    % Parse the file into a list of moves.
    parse_file(FileName, Parsed),
    pretty_print(Parsed),

    % Parse the moves into a board
    init_state(InitialState),
    parse_to_board(Parsed, InitialState, FinalState, _),

    % Calculate the best move and profile it
    best_move(FinalState, 1, vm(_, BestMove)),

    san_move(San, BestMove, FinalState),
    san_to_move_string(San, MoveString),
    writeln(MoveString),
    halt.