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
on_board((R, C)) :-
    between(0, 7, R),
    between(0, 7, C).


co_empty(Co, Board) :-
    get(Co, Board, Square),
    empty(Square).


% column_through_co((_, C), Coords)        :- findall(Co, on_column(C, Co), Coords).
% row_through_co((R, _), Coords)           :- findall(Co, on_row(R, Co), Coords).

% diagonal_through_co((R, C), Coords)      :- 
%     N is R - C,
%     findall(Co, on_diagonal(N, Co), Coords).

% anti_diagonal_through_co((R, C), Coords) :- 
%     N is R + C,
%     findall(Co, on_anti_diagonal(N, Co), Coords).

on_same_row((R, _), (R, C)) :- on_board((R, C)).

on_same_column((_, C), (R, C)) :- on_board((R, C)).

on_same_anti_diagonal((R1, C1), (R2, C2)) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    R1 + C1 =:= R2 + C2.

on_same_diagonal((R1, C1), (R2, C2)) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    R1 - C1 =:= R2 - C2.


% -------------------------------------------------------------------------------------------------------
%                                          Board actions
% -------------------------------------------------------------------------------------------------------

% Get a position on the board. Positions are coordinates with the center of origin in the top-left corner.
get((R, C), Board, Piece) :-
    nth0(R, Board, Row),
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


split_at_Co(Axis, Co, Coords) :- 
    append(_, [Co|Coords], Axis).

split_at_Co(Axis, Co, ReversedCoords) :- 
    append(Coords, [Co|_], Axis),
    reverse(Coords, ReversedCoords).


take_until_not_empty(Board, [Co|RestInput], [Co|RestOutput]) :-
    co_empty(Co, Board),
    take_until_not_empty(Board, RestInput, RestOutput), !.
take_until_not_empty(_, [Co|_], [Co]).


open_path_moves(Board, Axis, PieceCo, OpenPathMoves) :- 
    split_at_Co(Axis, PieceCo, Range),
    take_until_not_empty(Board, Range, OpenPathMoves).

