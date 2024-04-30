:- module(moves_knight, []).
:- use_module('../../../src/board.pl').
:- use_module('../../../src/moves/moves_main.pl').

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white) ) :- test_board(Board).

test_board([
    ['k', ' ', 'K', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    ['p', ' ', 'P', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', 'N', ' ', ' ', ' ', ' ', ' ', ' ']
]).

piece_position((7, 1)).
test_can_piece_move(To) :- test_state(State), piece_position(From), legal_move(move(From, To), State, _).

:- begin_tests(moves_knight).

test(knight1) :- test_can_piece_move((5, 0)), !.    % Can capture enemy piece
test(knight2) :- \+ test_can_piece_move((5, 2)), !. % Can't capture own piece
test(knight3) :- test_can_piece_move((6, 3)), !.
test(knight4) :- \+ test_can_piece_move((6, 0)), !.
test(knight_all) :- findall(To, test_can_piece_move(To), DestCoords), length(DestCoords, 2).
:- end_tests(moves_knight).
