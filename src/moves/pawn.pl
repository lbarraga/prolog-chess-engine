basic_piece_movement(pawn, From, To, Board) :-
    on_board(From), on_board(To),
    color_of_co(From, Board, Color),
    pawn_direction(Color, Direction),
    can_pawn_move(Color, Direction, From, To, Board).

% Basic forward move (1 step)
can_pawn_move(_, Dir, (R1, C), (R2, C), Board) :-
    R2 is R1 + Dir,
    co_empty((R2, C), Board).

% Initial two-step move
can_pawn_move(Color, Dir, (R1, C), (R3, C), Board) :-
    pawn_start_row(Color, R1),
    R2 is R1 + Dir,                 % Intermediate square
    R3 is R1 + Dir * 2,             % Final square
    co_empty((R2, C), Board),
    co_empty((R3, C), Board).

% Capture move
can_pawn_move(Color, Dir, (R1, C1), (R2, C2), Board) :-
    R2 is R1 + Dir,                 % Pawn must go one square in its direction.
    abs(C2 - C1) =:= 1,             % Taking is done one square diagonally
    opponent(Color, OpponentColor),
    get((R2, C2), Board, Piece),
    color(Piece, OpponentColor).    % Piece on attacking square must be of opposite color.
