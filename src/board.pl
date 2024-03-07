:- use_module(list_utils, [replace2D/4]).

initialize_board([
    ['r', 'n', 'b', 'q', 'k', 'b', 'n', 'r'],
    ['p', 'p', 'p', 'p', 'p', 'p', 'p', 'p'],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    ['P', 'P', 'P', 'P', 'P', 'P', 'P', 'P'],
    ['R', 'N', 'B', 'Q', 'K', 'B', 'N', 'R']
]).


valid_position((R, C)) :-
    between(0, 7, R),
    between(0, 7, C).


row(Index, Board, Row) :- nth0(Index, Board, Row).


column(Index, Board, Column) :-
    transpose(Board, Columns),
    nth0(Index, Columns, Column).


% Get a position on the board. Positions are coordinates with the center of origin in the top-left corner.
get((R, C), Board, Piece) :-
    row(R, Board, Row),
    nth0(C, Row, Piece).


remove(Coordinate, Board, NewBoard) :- replace2D(Coordinate, ' ', Board, NewBoard).

place(Coordinate, Piece, Board, NewBoard) :- replace2D(Coordinate, Piece, Board, NewBoard).

move(FromPos, ToPos, Board, NewBoard) :-
    get(FromPos, Board, Piece),
    remove(FromPos, Board, RemovedBoard),
    place(ToPos, Piece, RemovedBoard, NewBoard).


diagonal(StartCoordinate, Board, Direction, [StartCoordinate | Rest]) :-
    valid_position(StartCoordinate),
    next_position_diagonal(Direction, StartCoordinate, NextCoordinate),
    diagonal(NextCoordinate, Board, Direction, Rest), !.
diagonal(_, _, _, []).

next_position_diagonal(left, (R, C), (NextR, NextC))  :- NextR is R + 1, NextC is C + 1.
next_position_diagonal(right, (R, C), (NextR, NextC)) :- NextR is R - 1, NextC is C + 1.
