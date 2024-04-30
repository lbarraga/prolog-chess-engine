:- module(board, [
    init_state/1,
    on_board/1,
    co_empty/2,
    co_taken/2,
    color_of_co/3,
    value_of_piece_on_board/3,
    on_same/3,
    in_between_on/4,
    in_sight/4,
    get/3,
    remove/3,
    place/4,
    move_unsafe/3
]).
:- use_module(pieces).
:- use_module(list_utils, [replace2D/4, split/4]).
:- use_module(library(clpfd), [transpose/2]).

% init_state(-State) is det.
% State is the initial state of the game. white to move and both players can castle. no en passant possible.
init_state(state(
    Board,
    info(
        castling_info(
            (long, short),
            (long, short)
        ),
        no_ep
    ),
    white)
) :- initialize_board(Board).

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

% on_board(+Coordinate) is det.
% If a coordinate is on the board.
on_board((R, C)) :-
    between(0, 7, R),
    between(0, 7, C).

% co_empty(+Coordinate, +Board) is det.
% If a coordinate is empty on the board.
co_empty(Co, Board) :-
    get(Co, Board, Square),
    empty(Square).

% co_taken(+Coordinate, +Board) is det.
% If there is a piece on the coordinate.
co_taken(Co, Board) :-
    get(Co, Board, Square),
    piece(Square).

% color_of_co(+Coordinate, +Board, -Color) is det.
% Get the color of the piece on the coordinate.
color_of_co(Co, Board, Color) :- 
    get(Co, Board, Square),
    color(Square, Color).

% value_of_piece_on_board(+Color, +Board, -Value) is det.
% Get the value of the piece on the board.
value_of_piece_on_board(Color, Board, Value) :- 
    get(_, Board, Piece),
    color(Piece, Color),
    name(Piece, Name),
    value(Name, Value).


% on_same(+Axis, +From, +To) is det.
% If two coordinates are on the same Axis.
% Axis could be row, column, anti_diagonal or diagonal.
on_same(Axis, From, To) :-
    on_board(From), on_board(To),
    on_same_help(Axis, From, To).

% Helper for on_same/3.
on_same_help(row,           (R, _), (R, _)).
on_same_help(column,        (_, C), (_, C)).
on_same_help(anti_diagonal, (R1, C1), (R2, C2)) :- R1 + C1 =:= R2 + C2.
on_same_help(diagonal,      (R1, C1), (R2, C2)) :- R1 - C1 =:= R2 - C2.


% in_between_on(+Axis, +Co, +Co1, +Co2) is det.
% In between: If Coordinate `Co` is between `Co1` and `Co2` on axis `Axis`.
in_between_on(Axis, Co, Co1, Co2) :-
    on_same(Axis, Co, Co1),
    on_same(Axis, Co1, Co2),
    in_between_on_help(Axis, Co, Co1, Co2).

% Helper for in_between_on/4.
in_between_on_help(row, (_, C), (_, C1), (_, C2)) :- between_non_inclusive(C, C1, C2), !.
in_between_on_help(_,   (R, _), (R1, _), (R2, _)) :- between_non_inclusive(R, R1, R2).

between_non_inclusive(Z, X, Y) :- X < Z, Z < Y.
between_non_inclusive(Z, X, Y) :- Y < Z, Z < X.

% `Co1` and `Co2` can "see" each-other on `Axis`.
in_sight(Axis, Co1, Co2, Board) :- 
    on_board(Co1), on_board(Co2),
    on_same(Axis, Co1, Co2),
    forall(
        in_between_on(Axis, Co, Co1, Co2), % For all coordinates `Co` between `Co1` and `Co2`,
        co_empty(Co, Board)                % `Co` must be empty.
    ).

% -------------------------------------------------------------------------------------------------------
%                                          Board actions
% -------------------------------------------------------------------------------------------------------

% get(+Coordinate, +Board, -Piece) is det.
% Get a position on the board. Positions are coordinates with the center of origin in the top-left corner.
get((R, C), Board, Piece) :-
    nth0(R, Board, Row),
    nth0(C, Row, Piece).


% remove(+Coordinate, +Board, -NewBoard) is det.
% Remove a certain piece from the board.
remove(Coordinate, Board, NewBoard) :- replace2D(Coordinate, ' ', Board, NewBoard).

% place(+Coordinate, +Piece, +Board, -NewBoard) is det.
% Place a piece on the board.
place(Coordinate, Piece, Board, NewBoard) :- replace2D(Coordinate, Piece, Board, NewBoard).

% move_unsafe(+Move, +Board, -NewBoard) is det.
% Move a piece from one position to another position, absolutely no checks are done.
move_unsafe(move(FromPos, ToPos), Board, NewBoard) :-
    get(FromPos, Board, Piece),
    remove(FromPos, Board, RemovedBoard),
    place(ToPos, Piece, RemovedBoard, NewBoard).