:- initialization(main).
:- use_module('parsing/parse_pgn', [pgn/4]).
:- use_module('parsing/parse_san', [san/4]).
:- use_module('parsing/san_move', [san_move/3]).
:- use_module('parsing/parse_to_board', [parse_to_board/4]).
:- use_module('moves/moves_main', [legal_move/3, is_checkmate/1, is_stalemate/1]).
:- use_module(prettyprint, [print_board/1, print_state/1]).
:- use_module(alpha_beta).
:- use_module('board.pl').
:- use_module(library(prolog_stack)).
:- use_module(test).

print_rules :- rules(Rules), writeln(Rules).

parse_file(FileName, Parsed) :-
    % Open the file
    open(FileName, read, Stream),

    % Use the pgn predicate from the parse_pgn module to parse the file
    ( phrase_from_stream(pgn(Moves, Rules), Stream), !
    -> Parsed = Moves, assert(rules(Rules))
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

result_string(state(Board, Info, white), "0-1") :- is_checkmate(state(Board, Info, white)), !.
result_string(state(Board, Info, black), "1-0") :- is_checkmate(state(Board, Info, black)), !.
result_string(State, "1/2-1/2") :- is_stalemate(State), !.
result_string(_, "*").

output_line(Parsed, FinalState, Move) :-
    pretty_print(Parsed),
    legal_move(Move, FinalState, MovedState),
    result_string(MovedState, S),
    san_move(San, Move, FinalState),
    san_to_move_string(San, MoveString),
    write(MoveString),
    write(' '), writeln(S).

output(no_test, Parsed, FinalState) :-
    best_move(FinalState, 3, vm(_, BestMove)),
    output_line(Parsed, FinalState, BestMove).


output(test, Parsed, FinalState) :-
    findall(Move, legal_move(Move, FinalState, _), Moves),
    maplist(output_line(Parsed, FinalState), Moves).


main :-
    % Extract the filename from the command line arguments
    current_prolog_flag(argv, Argv),
    nth0(0, Argv, FileName),
    (nth0(1, Argv, 'TEST') -> Test = test; Test = no_test),

    % Parse the file into a list of moves.
    parse_file(FileName, Parsed),
    % Parse the moves into a board
    init_state(InitialState),
    parse_to_board(Parsed, InitialState, FinalState, _),
    output(Test, Parsed, FinalState),
    halt.