:- module(moves_rook, []).
:- use_module('../../../src/board.pl').
:- use_module('../../../src/moves/moves_main.pl').

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white) ) :- test_board(Board).

test_board([
    ['k', ' ', 'K', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', 'R', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', 'P', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

piece_position((3, 3)).
test_can_piece_move(To) :- test_state(State), piece_position(From), legal_move(move(From, To), State, _).

:- begin_tests(moves_rook).

test(rook1) :- test_can_piece_move((3, 0)), !.
test(rook2) :- test_can_piece_move((3, 2)), !.
test(rook3) :- \+ test_can_piece_move((4, 3)), !.  % Can't move forward because of the pawn
test(rook4) :- test_can_piece_move((0, 3)), !.
test(rook5) :- test_can_piece_move((2, 3)), !.
test(rook_all) :- findall(To, test_can_piece_move(To), DestCoords), length(DestCoords, 10).

:- end_tests(moves_rook).
