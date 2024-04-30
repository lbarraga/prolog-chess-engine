:- module(parse_to_board, [parse_to_board/3]).
:- use_module('../pieces.pl').
:- use_module('../moves/moves_main.pl').
:- use_module('../parsing/san_move.pl', [san_move/3]).

parse_to_board([], State, State).
parse_to_board([turn(WhitePlie, BlackPlie) | Rest], StartState, EndState) :-
    parse_plie(WhitePlie, StartState, WhiteTempState),
    parse_plie(BlackPlie, WhiteTempState, BlackTempState),
    parse_to_board(Rest, BlackTempState, EndState).

parse_plie(no_move, State, State) :- !.
parse_plie(San, State, EndState) :-
    san_move(San, Move, State),
    legal_move(Move, State, EndState).