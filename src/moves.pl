
% ---------------------------------------------------------------------------------------------------------
%                                                   Moves 
% ---------------------------------------------------------------------------------------------------------
% In this module, the possible move rules of every piece are defined. This is not with respect to a board,
%           where the set of possible moves is reduced if other pieces are in the way. 
% ---------------------------------------------------------------------------------------------------------

% ------------------------------------------------ General ------------------------------------------------

hyper_move(move(From, To), Color, Board, BoardInfo, NewBoard, BoardInfo) :-
    uber_move(From, To, Color, Board, NewBoard).

hyper_move(castle(Type), Color, Board, BoardInfo, NewBoard, _) :-
    uber_can_move_castling(Type, Color, Board, BoardInfo),
    uber_move_castling(Type, Color, Board, BoardInfo, NewBoard, _).

uber_move_castling(Type, Color, Board, Info, NewBoard, _) :-  % TODO info moet veranderen.
    uber_can_move_castling(Type, Color, Board, Info),
    between_squares(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos),
    move(KingPos, TwoNextToKingPos, Board, TempBoard),
    move(RookPos, OneNextToKingPos, TempBoard, NewBoard).


uber_can_move_castling(Type, Color, Board, info(CanCastleShort, CanCastleLong)) :-
    \+ king_in_check(Board, Color),
    info_ok_for_castling(Type, CanCastleShort, CanCastleLong),
    between_squares(Color, Type, KingPos, OneNextToKingPos, TwoNextToKingPos, RookPos),
    uber_can_move(RookPos, OneNextToKingPos, Color, Board),             % Rook could move next to king.
    uber_move(KingPos, OneNextToKingPos, Color, Board, TempBoard),      % King could move one square closer to rook.
    uber_move(OneNextToKingPos, TwoNextToKingPos, Color, TempBoard, _). % King could move two squares closer to rook.

%                                     King   King+1  King+2   Rook
between_squares(white, long_castle,  (7, 4), (7, 3), (7, 2), (7, 0)).
between_squares(white, short_castle, (7, 4), (7, 5), (7, 6), (7, 7)).
between_squares(black, long_castle,  (0, 4), (0, 3), (0, 2), (0, 0)).
between_squares(black, short_castle, (0, 4), (0, 5), (0, 6), (0, 7)).

info_ok_for_castling(short_castle, can_castle_short, _).
info_ok_for_castling(long_castle, _, can_castle_long).

uber_move_promotion(From, To, Color, Promotion, Board, NewBoard) :-
    uber_move(From, To, Color, Board, TempBoard),
    do_promotion(To, Color, Promotion, TempBoard, NewBoard).

do_promotion(_, _, no_promotion, Board, Board).
do_promotion(To, Color, Promotion, Board, NewBoard) :-
    name(PromotionPiece, Promotion),
    color(PromotionPiece, Color),
    place(To, PromotionPiece, Board, NewBoard).

uber_move(From, To, Color, Board, NewBoard) :-
    uber_can_move(From, To, Color, Board),
    move(From, To, Board, NewBoard).


uber_can_move(From, To, Color, Board) :-
    super_can_move(From, To, Color, Board),
    move(From, To, Board, NewBoard),    % Get the board on which the desired move was made.
    color_of_co(From, Board, Color),    % Only pieces of the moving color can move.
    \+ king_in_check(NewBoard, Color).  % The own king cannot be in check in this position.
    

super_can_move(From, To, Color, Board) :-
    get(From, Board, FromPiece),
    color(FromPiece, Color),
    get(To, Board, ToSquare),
    name(FromPiece, FromName),               
    could_capture(FromPiece, ToSquare),     % Attacking square must be empty or of opposite color.
    can_move(FromName, From, To, Board).    %


king_in_check(Board, Color) :- 
    king(Color, King),
    get(KingCo, Board, King),
    opponent(Color, OpponentColor),
    super_can_move(_, KingCo, OpponentColor, Board).

is_checkmate(Board, Color) :- 
    king_in_check(Board, Color),
    \+ uber_can_move(_, _, Color, Board).


is_stalemate(Board, ToMoveColor) :-
    \+ king_in_check(Board, ToMoveColor),
    \+ uber_can_move(_, _, ToMoveColor, Board).


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
    color(Piece, OpponentColor).    % Piece on attacking square must be of opposite color.
