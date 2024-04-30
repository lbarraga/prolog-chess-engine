:- module(moves_pawn, []).
:- use_module('../../../src/board.pl').
:- use_module('../../../src/moves/moves_main.pl').

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white) ) :- test_board(Board).

test_board([
    ['k', ' ', ' ', ' ', ' ', ' ', ' ', 'K'],
    [' ', ' ', ' ', 'P', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', 'P', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', 'P', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

piece_position((3, 3)).
test_can_piece_move(Move) :- test_state(State), legal_move(Move, State, _).

:- begin_tests(moves_pawn).

test(pawn1_one_forward) :- test_can_piece_move(move((6, 2), (5, 2))), !.  % Can move one forward
test(pawn1_two_forward) :- test_can_piece_move(move((6, 2), (4, 2))), !.  % Can move two forward
test(pawn2_not_two_forward) :- \+ test_can_piece_move(move((4, 5), (2, 5))), !.  % Can't move backwards
test(pawn3_promotion) :- test_can_piece_move(promotion((1, 3), (0, 3), bishop)), !.  % pawn can promote
test(pawn3_promotion) :- test_can_piece_move(promotion((1, 3), (0, 3), queen)), !.  % pawn can promote
test(pawn3_promotion) :- test_can_piece_move(promotion((1, 3), (0, 3), knight)), !.  % pawn can promote
test(pawn3_promotion) :- test_can_piece_move(promotion((1, 3), (0, 3), rook)), !.  % pawn can promote
test(pawn4) :- \+ test_can_piece_move((3, 4)), !.
test(pawn_all) :- findall(M, test_can_piece_move(M), DestCoords), length(DestCoords, 10).

:- end_tests(moves_pawn).
