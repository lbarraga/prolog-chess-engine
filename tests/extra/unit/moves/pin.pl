:- module(pin, []).
:- use_module('../../../../src/board.pl').
:- use_module('../../../../src/moves/moves_main.pl').

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white) ) :- test_board(Board).

test_board([
    [' ', ' ', ' ', 'K', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', 'B', ' ', ' ', ' ', ' '], % Bishop is pinned by the rook and cannot move
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', 'r', ' ', ' ', ' ', 'k']
]).

piece_position((4, 3)).
test_can_piece_move(To) :- test_state(State), piece_position(From), legal_move(move(From, To), State, _).

:- begin_tests(pin).

test(test_pin) :- findall(To, test_can_piece_move(To), DestCoords), length(DestCoords, 0).

:- end_tests(pin).
