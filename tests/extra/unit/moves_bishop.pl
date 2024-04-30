:- module(moves_bishop, []).
:- use_module('../../../src/board.pl').
:- use_module('../../../src/moves/moves_main.pl').

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white) ) :- test_board(Board).

test_board([
    ['k', ' ', 'K', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', 'P', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    ['p', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', 'B', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

piece_position((6, 1)).
test_can_piece_move(To) :- test_state(State), piece_position(From), legal_move(move(From, To), State, _).

:- begin_tests(moves_bishop).

test(bishop1) :- test_can_piece_move((5, 2)), !.
test(bishop2) :- test_can_piece_move((4, 3)), !.
test(bishop3) :- test_can_piece_move((3, 4)), !.
test(bishop4) :- \+ test_can_piece_move((2, 5)), !.    % Can't capture own piece
test(bishop5) :- test_can_piece_move((5, 0)), !.       % Can capture enemy piece
test(bishop6) :- test_can_piece_move((7, 2)), !.
test(bishop_all) :- findall(To, test_can_piece_move(To), DestCoords), length(DestCoords, 6).

:- end_tests(moves_bishop).
