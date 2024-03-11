
% ---------------------------------------------------------------------------------------------------------
%                                                   Moves 
% ---------------------------------------------------------------------------------------------------------
% In this module, the possible move rules of every piece are defined. This is not with respect to a board,
%           where the set of possible moves is reduced if other pieces are in the way. 
% ---------------------------------------------------------------------------------------------------------

% ------------------------------------------------ General ------------------------------------------------


% koning mag niet schaak staan, 
% To mag niet van het eigen kleur zijn
% Er moet een stuk staan op de from plaats

can_move(From, To, Board) :-
    get(From, Board, FromPiece),
    get(To, Board, ToSquare),
    could_capture(FromPiece, ToSquare),
    name(FromPiece, FromName),
    can_move(FromName, From, To, Board).





% -------------------------------------------------- Rook -------------------------------------------------

can_move(rook, From, To, Board) :- in_sight(row, From, To, Board).
can_move(rook, From, To, Board) :- in_sight(column, From, To, Board).

% ------------------------------------------------- Bishop -----------------------------------------------

can_move(bishop, From, To, Board) :- in_sight(diagonal, From, To, Board).
can_move(bishop, From, To, Board) :- in_sight(anti_diagonal, From, To, Board).

% -------------------------------------------------  Queen  -----------------------------------------------

can_move(queen, From, To, Board) :- can_move(bishop, From, To, Board).
can_move(queen, From, To, Board) :- can_move(rook, From, To, Board).

% -------------------------------------------------  Knight  -----------------------------------------------

can_move(knight, (R1, C1), (R2, C2), _) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R1 - R2) =:= 1, 
    abs(C2 - C1) =:= 2.

can_move(knight, (R1, C1), (R2, C2), _) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =:= 2, 
    abs(C2 - C1) =:= 1.

% -------------------------------------------------  King  -----------------------------------------------

can_move(king, (R1, C1), (R2, C2), _) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =< 1, 
    abs(C2 - C1) =< 1.

% -------------------------------------------------  Pawn  -----------------------------------------------

% Een pion kan twee plaatsen naar voor gaan als hij nog op zijn rank staat. normaal kan hij eentje naar voor gaan. 


can_move(pawn, From, To, Board) :-
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
    color(Piece, OpponentColor).   % Piece on attacking square must be of opposite color.
