
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
    % Color of the piece.
    get(From, Board, FromPiece),
    get(To, Board, ToPiece),
    piece(FromPiece),
    color(FromPiece, FromColor),
    % To must be same color or empty
    (empty(ToPiece); (color(ToPiece, ToColor), FromColor =\= ToColor)),




% -------------------------------------------------- Rook -------------------------------------------------

can_move(rook, _, From, To, Board) :- in_sight(row, From, To, Board).
can_move(rook, _, From, To, Board) :- in_sight(column, From, To, Board).

% ------------------------------------------------- Bischop -----------------------------------------------

can_move(bischop, _, From, To, Board) :- in_sight(diagonal, From, To, Board).
can_move(bischop, _, From, To, Board) :- in_sight(anti_diagonal, From, To, Board).

% -------------------------------------------------  Queen  -----------------------------------------------

can_move(queen, _, From, To, Board) :- can_move(bischop, From, To, Board).
can_move(queen, _, From, To, Board) :- can_move(rook, From, To, Board).

% -------------------------------------------------  Knight  -----------------------------------------------

can_move(knight, _, (R1, C1), (R2, C2), _) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R1 - R2) =:= 1, 
    abs(C2 - C1) =:= 2.

can_move(knight, _, (R1, C1), (R2, C2), _) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =:= 2, 
    abs(C2 - C1) =:= 1.

% -------------------------------------------------  King  -----------------------------------------------

can_move(king, _, (R1, C1), (R2, C2), _) :- 
    on_board((R1, C1)), on_board((R2, C2)),
    abs(R2 - R1) =< 1, 
    abs(C2 - C1) =< 1.

% -------------------------------------------------  Pawn  -----------------------------------------------

% Een pion kan twee plaatsen naar voor gaan als hij nog op zijn rank staat. normaal kan hij eentje naar voor gaan. 

can_move(pawn, white, (6, C), (4, C), Board) :- 
    co_empty((5, C), Board), 
    co_empty((4, C), Board).

can_move(pawn, white, (R1, C), (R2, C), Board) :- 
    R2 =:= R1 - 1,
    co_empty((R2, C), Board).

can_move(pawn, white, (R1, C1), (R2, C2), Board) :-
    R2 =:= R1 - 1,
    abs(C2 - C1) =:= 1,
    get(R2, C2, Board, Piece),
    black(Piece).

% Black
can_move(pawn, black, (1, C), (3, C), Board) :- 
    co_empty((2, C), Board), 
    co_empty((3, C), Board).

can_move(pawn, black, (R1, C), (R2, C), Board) :- 
    R2 =:= R1 + 1,
    co_empty((R2, C), Board).

can_move(pawn, black, (R1, C1), (R2, C2), Board) :-
    R2 =:= R1 + 1,
    abs(C2 - C1) =:= 1,
    get(R2, C2, Board, Piece),
    white(Piece).
