:- use_module(list_utils, [replace2D/4, split/4]).
:- use_module(library(clpfd), [transpose/2]).

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

test_state(state(Board, info(castling_info((long, short), (long, short)), no_ep), white)) :- test_board(Board).

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

test_board([
    ['r', 'n', 'b', 'q', 'k', 'b', 'n', ' '],
    [' ', 'p', 'p', 'p', 'p', 'p', 'p', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', 'r'],
    ['p', ' ', ' ', ' ', ' ', ' ', ' ', 'p'],
    ['P', ' ', ' ', ' ', ' ', ' ', ' ', 'P'],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', 'R'],
    [' ', 'P', 'P', 'P', 'P', 'P', 'P', ' '],
    ['R', 'N', 'B', 'Q', 'K', 'B', 'N', ' ']
]).

test_board3([
    [' ', ' ', ' ', ' ', ' ', 'k', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', 'r', 'q', ' '],
    ['p', ' ', 'p', ' ', 'Q', ' ', ' ', 'R'],
    [' ', 'p', ' ', ' ', 'P', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', 'p', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    ['P', 'P', 'P', ' ', ' ', ' ', ' ', 'P'],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', 'K']
]).

test_board4([
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', 'k'],
    [' ', ' ', ' ', ' ', ' ', 'K', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', 'N', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', 'N'],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
]).

test_board5([
    ['r', ' ', ' ', 'q', 'k', ' ', ' ', 'r'],
    ['p', 'b', ' ', ' ', ' ', ' ', 'p', 'p'],
    [' ', 'n', ' ', ' ', 'P', 'b', ' ', ' '],
    [' ', ' ', 'B', ' ', ' ', 'Q', ' ', ' '],
    ['p', ' ', 'p', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', 'P', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', 'B', ' ', ' ', 'P', 'P', 'P'],
    ['R', 'N', ' ', ' ', 'R', ' ', 'K', ' ']
]).

test_board6([
    ['r', ' ', ' ', 'q', ' ', 'r', ' ', ' '],
    ['p', 'p', 'p', 'n', 'n', ' ', 'b', 'k'],
    [' ', ' ', ' ', 'p', ' ', ' ', 'p', 'p'],
    [' ', ' ', ' ', 'P', 'p', 'p', ' ', ' '],
    [' ', ' ', 'P', ' ', 'P', 'P', ' ', ' '],
    [' ', ' ', 'N', 'B', 'B', ' ', ' ', ' '],
    ['P', 'P', ' ', ' ', 'Q', ' ', 'P', 'P'],
    ['R', ' ', ' ', ' ', ' ', 'R', 'K', ' ']
]).

test_board7([
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' '],
    [' ', ' ', ' ', ' ', ' ', ' ', ' ', ' ']
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

co_taken(Co, Board) :-
    get(Co, Board, Square),
    piece(Square).

color_of_co(Co, Board, Color) :- 
    get(Co, Board, Square),
    color(Square, Color).

value_of_piece_on_board(Color, Board, Value) :- 
    get(_, Board, Piece),
    color(Piece, Color),
    name(Piece, Name),
    value(Name, Value).


% If two coordinates are on the same Axis.
on_same(Axis, From, To) :- 
    on_board(From), on_board(To),
    on_same_help(Axis, From, To).

on_same_help(row,           (R, _), (R, _)).
on_same_help(column,        (_, C), (_, C)).
on_same_help(anti_diagonal, (R1, C1), (R2, C2)) :- R1 + C1 =:= R2 + C2.
on_same_help(diagonal,      (R1, C1), (R2, C2)) :- R1 - C1 =:= R2 - C2.


% In between: If Coordinate `Co` is between `Co1` and `Co2` on axis `Axis`.
in_between_on(Axis, Co, Co1, Co2) :-
    on_same(Axis, Co, Co1),
    on_same(Axis, Co1, Co2),
    in_between_on_help(Axis, Co, Co1, Co2).

in_between_on_help(row, (_, C), (_, C1), (_, C2)) :- between_non_inclusive(C, C1, C2), !.
in_between_on_help(_,   (R, _), (R1, _), (R2, _)) :- between_non_inclusive(R, R1, R2).

between_non_inclusive(Z, X, Y) :- X < Z, Z < Y.
between_non_inclusive(Z, X, Y) :- Y < Z, Z < X.

% `Co1` and `Co2` can see eachother on `Axis`.
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

% Get a position on the board. Positions are coordinates with the center of origin in the top-left corner.
get((R, C), Board, Piece) :-
    nth0(R, Board, Row),
    nth0(C, Row, Piece).


% Remove a certain piece from the board.
remove(Coordinate, Board, NewBoard) :- replace2D(Coordinate, ' ', Board, NewBoard).

% Place a piece on the board.
place(Coordinate, Piece, Board, NewBoard) :- replace2D(Coordinate, Piece, Board, NewBoard).

% Move a piece from one position to another position 
move_unsafe(move(FromPos, ToPos), Board, NewBoard) :-
    get(FromPos, Board, Piece),
    remove(FromPos, Board, RemovedBoard),
    place(ToPos, Piece, RemovedBoard, NewBoard).