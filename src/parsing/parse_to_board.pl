:- module(parse_to_board, [parse_to_board/3]).
:- use_module('../pieces.pl').
:- use_module('../moves/moves_main.pl').
:- use_module('../parsing/san_move.pl', [san_move/3]).
:- use_module(library(ansi_term)).

% parse_to_board(+turns, +State, -EndState)
% Converts the parsed moves into a board state.
% The turns are a list of turns, where each turn is a pair of plies.
% A ply is either a move or no_move.
% State is the initial state of the board.
% EndState is the final state of the board, after all the moves have been applied.
parse_to_board([], State, State).
parse_to_board([turn(WhitePly, BlackPly) | Rest], StartState, EndState) :-
    parse_ply(WhitePly, StartState, WhiteTempState),
    parse_ply(BlackPly, WhiteTempState, BlackTempState),
    parse_to_board(Rest, BlackTempState, EndState).

% parse_ply(+Ply, +State, -EndState)
% Parses a ply into a move and applies it to the board.
parse_ply(no_move, State, State) :- !.
parse_ply(San, State, EndState) :-
    (san_move(San, Move, State) % Try to convert the San to an internal representation
    -> true
    ; san_string(San, SanString), ansi_format([bold,fg(yellow)], 'Invalid move: ~w~n', [SanString]), fail
    ),
    legal_move(Move, State, EndState).


san_string(San, SanString) :-
    phrase(San, SanCodes),
    string_codes(SanString, SanCodes).