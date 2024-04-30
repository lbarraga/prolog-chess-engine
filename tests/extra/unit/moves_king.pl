:- module(moves_king, []).
:- use_module('../../../src/board.pl').
:- use_module('../../../src/moves/moves_main.pl').

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white) ) :- test_board(Board).

test_board([
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', 'k', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', 'K', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

piece_position((3, 3)).
test_can_piece_move(To) :- test_state(State), piece_position(From), legal_move(move(From, To), State, _).

:- begin_tests(moves_king).

test(king1) :- \+ test_can_piece_move((2, 2)), !. % Other king is there
test(king2) :- test_can_piece_move((2, 3)), !.
test(king3) :- test_can_piece_move((2, 4)), !.
test(king4) :- test_can_piece_move((3, 2)), !.
test(king5) :- test_can_piece_move((3, 4)), !.
test(king6) :- test_can_piece_move((4, 2)), !.
test(king7) :- test_can_piece_move((4, 3)), !.
test(king8) :- test_can_piece_move((4, 4)), !.
test(king_all) :- findall(To, test_can_piece_move(To), DestCoords), length(DestCoords, 7).

:- end_tests(moves_king).
