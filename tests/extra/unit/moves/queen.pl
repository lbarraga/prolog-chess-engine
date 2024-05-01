:- module(queen, []).
:- use_module('../../../../src/board.pl').
:- use_module('../../../../src/moves/moves_main.pl').

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white) ) :- test_board(Board).

test_board([
    ['k', ' ', 'K', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', 'p', ' ', ' ', ' ', 'P', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', 'Q', ' ', ' ', ' ', ' ', ' ', ' ']
]).

piece_position((7, 1)).
test_can_piece_move(To) :- test_state(State), piece_position(From), legal_move(move(From, To), State, _).

:- begin_tests(moves_queen).

test(queen1) :- test_can_piece_move((6, 0)), !.
test(queen2) :- test_can_piece_move((6, 2)), !.
test(queen3) :- \+ test_can_piece_move((2, 5)), !.    % Can't capture own piece
test(queen4) :- \+ test_can_piece_move((5, 0)), !.    % Can't capture own piece
test(queen5) :- test_can_piece_move((7, 0)), !.
test(queen6) :- test_can_piece_move((7, 2)), !.
test(queen_all) :- findall(To, test_can_piece_move(To), DestCoords), length(DestCoords, 15).

:- end_tests(moves_queen).
