:- use_module(list_utils, [replace2D/4, split/4]).
:- use_module(library(clpfd), [transpose/2]).

% The initial setup of a chess board.
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


% -------------------------------------------------------------------------------------------------------
%                                          Board properties
% -------------------------------------------------------------------------------------------------------

% If a coordinate is on the board.
valid_position((R, C)) :-
    between(0, 7, R),
    between(0, 7, C).


% Get a row on the board.
row(Index, Board, Row) :- nth0(Index, Board, Row).


% Get a column on the board
column(Index, Board, Column) :-
    transpose(Board, Columns),
    nth0(Index, Columns, Column).


% Get a diagonal on the board, starting from a coordinate and moving in `Direction`.
% If the direction is `left`, the diagonal goes from top-left to bottom-right. 
% If the `Direction` is right, it goes from the bottom-left to th top right.
diagonal(StartCoordinate, Board, Direction, [StartCoordinate | Rest]) :-
    valid_position(StartCoordinate),
    next_position_diagonal(Direction, StartCoordinate, NextCoordinate),
    diagonal(NextCoordinate, Board, Direction, Rest), !.
diagonal(_, _, _, []).

next_position_diagonal(left, (R, C), (NextR, NextC))  :- NextR is R + 1, NextC is C + 1.
next_position_diagonal(right, (R, C), (NextR, NextC)) :- NextR is R - 1, NextC is C + 1.


% -------------------------------------------------------------------------------------------------------
%                                          Board actions
% -------------------------------------------------------------------------------------------------------

% Get a position on the board. Positions are coordinates with the center of origin in the top-left corner.
get((R, C), Board, Piece) :-
    row(R, Board, Row),
    nth0(C, Row, Piece).


% Remove a certain piece from the board.
remove(Coordinate, Board, NewBoard) :- replace2D(Coordinate, ' ', Board, NewBoard).

% Place a piece on the board.
place(Coordinate, Piece, Board, NewBoard) :- replace2D(Coordinate, Piece, Board, NewBoard).

% Move a piece from one position to another position 
move(FromPos, ToPos, Board, NewBoard) :-
    get(FromPos, Board, Piece),
    remove(FromPos, Board, RemovedBoard),
    place(ToPos, Piece, RemovedBoard, NewBoard).


take_until_not_empty(Board, [Co|RestInput], [Co|RestOutput]) :-
    get(Co, Board, Square),
    empty(Square),
    take_until_not_empty(Board, RestInput, RestOutput), !.
take_until_not_empty(_, [Co|_], [Co]).


open_path_moves(Board, Axis, PieceCo, OpenPathMoves) :- 
    nth0(Index, Axis, PieceCo),
    split(Index, Axis, LeftOfPiece, RightOfPiece),
    reverse(LeftOfPiece, ReversedLeftOfPiece),
    take_until_not_empty(Board, ReversedLeftOfPiece, OpenPathLeft),
    take_until_not_empty(Board, RightOfPiece, OpenPathRight),
    append(OpenPathLeft, OpenPathRight, OpenPathMoves).

